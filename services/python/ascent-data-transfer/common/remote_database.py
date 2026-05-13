'''
contains code for interacting with the remote database
'''

import json
from psycopg2.extras import RealDictCursor
from common.database import DatabaseConnectionPg, DatabaseConnectionParameters

class RemoteDatabase(DatabaseConnectionPg):
    ''' class for interacting with the remote database '''
    # table names
    SMPS_SETTINGS_TABLE = 'smps.instrument_settings'
    SMPS_MEASUREMENTS_TABLE = 'smps.sample_analysis'
    XACT_SAMPLE_ANALYSIS_TABLE = 'xact.sample_analysis'
    XACT_SETTINGS_TABLE = 'xact.instrument_settings'
    XACT_RAW_MEASUREMENTS_TABLE = 'xact.raw_measurements'
    ACSM_DATA_TABLE = 'acsm.sample_analysis'
    ACSM_MASS_LOADINGS_TABLE = 'acsm.mass_loadings'
    ACSM_DIAG_CALIB_TABLE = 'acsm.diag_calib'
    ACSM_TPS_TABLE = 'acsm.tps'
    DRYERSTATS_TABLE = 'acsm.dryer_stats'
    SITES_TABLE = 'common.sites'
    APP_LOG_TABLE = 'common.site_app_logs'
    # column names
    LOCAL_ID_COLUMN = 'site_record_id'

    ''' for dealing with the cloud database and data synching '''
    def __init__(self, connection_parameters: DatabaseConnectionParameters):
        DatabaseConnectionPg.__init__(self, connection_parameters)
        self.site_validated = False
        self._site_number = None
        self._site_code = None
        self._conn = None  # persistent connection

    def __del__(self):
        """Ensure connection is closed when object is destroyed"""
        if self._conn is not None and not self._conn.closed:
            self._conn.close()

    @property
    def site_number(self) -> int:
        ''' remote database site number '''
        return self._site_number
    
    def get_connection(self):
        ''' Get or create a connection '''
        if self._conn is None or self._conn.closed:
            self._conn = self.connect()
        return self._conn

    def _save_record(self, table_name: str, record: dict) -> int:
        ''' save record to database '''
        conn = self.get_connection()
        cursor = conn.cursor()
        db_id = self._save_partial_record(table_name, record, cursor)
        print(f'new {table_name} record cloud id: {db_id}')
        cursor.close()
        conn.commit()
        return db_id

    def _save_partial_record(self, table_name: str, record: dict, cursor) -> str:
        ''' save record to database without committing it'''
        column_names = []
        column_values = []
        for (column_name, value) in record.items():
            column_names.append(column_name)
            column_values.append(value)
        placeholders = ['%s'] * len(column_names)
        sql_query = f'INSERT INTO {table_name} ({",".join(column_names)})'
        sql_query += f' VALUES ({",".join(placeholders)})'
        sql_query += ' RETURNING id'
        cursor.execute(sql_query, tuple(column_values))
        result = cursor.fetchone()
        db_id = result[0]
        return db_id

    def _save_record_with_child(self, parent_table_name: str, parent_record: dict,
                                child_table_name: str, child_records: list) -> int:
        ''' save (parent) record that has child records to database, return (parent) id '''
        conn = self.get_connection()
        cursor = conn.cursor()
        parent_db_id = self._save_partial_record(parent_table_name, parent_record, cursor)
        for child_record in child_records:
            child_record['sample_analysis_id'] = parent_db_id
            child_db_id = self._save_partial_record(child_table_name, child_record, cursor)
        cursor.close()
        conn.commit()
        return parent_db_id

    def _update_record(self, table_name: str, local_record: dict, key_column: str) -> bool:
        ''' updates the record in the database '''
        # assumes we have already checked the existance of the record
        column_names = []
        column_values = []
        for (column_name, value) in local_record.items():
            if key_column == column_name:
                continue # skip key column
            column_names.append(column_name)
            column_values.append(value)
        set_items = [f'{col} = %s' for col in column_names]
        sql_query = f'UPDATE {table_name}'
        sql_query += ' SET ' + ','.join(set_items)
        sql_query += f' WHERE {self.LOCAL_ID_COLUMN} = {local_record[key_column]} AND site_number = {self.site_number}'
        conn = self.get_connection()
        cursor = conn.cursor()
        cursor.execute(sql_query, tuple(column_values))
        # need to check that it went through
        cursor.close()
        conn.commit()
        return True

    def set_site(self, site_number: int, site_code: str):
        self.site_validated = False
        self._site_code = site_code
        self._site_number = site_number

    def validate_site(self) -> bool:
        ''' checks the local settings against cloud sites table. '''
        self.site_validated = False
        conn = self.get_connection()
        cursor = conn.cursor(cursor_factory=RealDictCursor)
        sql_query = f'SELECT * FROM {self.SITES_TABLE} WHERE site_number = %s'
        cursor.execute(sql_query, (self._site_number,))
        results = cursor.fetchall()
        if len(results) == 0:
            msg = f'Failed to find site record for site_number "{self._site_number}"'
            raise IndexError(msg)
        record = results[0]
        db_site_code = record['site_code']
        if db_site_code != self._site_code:
            msg = f'Cloud site_code "{db_site_code}"'
            msg += f' did not match local site_code "{self._site_code}"'
            msg += f' for site_number "{self._site_number}"'
            raise AssertionError(msg)
        # matched
        print('Validated site_number ', self._site_number)
        self.site_validated = True
        cursor.close()
        return True

    def _check_record(self, table_name: str, local_id: int) -> bool:
        ''' checks if record already in database using the local db id'''
        sql_query = f'SELECT EXISTS (SELECT 1 FROM {table_name} WHERE site_number = %s AND {self.LOCAL_ID_COLUMN} = %s)'
        conn = self.get_connection()
        cursor = conn.cursor()
        cursor.execute(sql_query, (self.site_number, local_id))
        exists = cursor.fetchone()[0]
        cursor.close()
        return exists

    def _update_local_fields(self, record: dict, key_field: str, is_child=False) -> dict:
        ''' need to update the record for the remote db id fields (external_id, site_number) '''
        local_id = record[key_field]
        record.pop(key_field)
        record[self.LOCAL_ID_COLUMN] = local_id
        if not self.site_validated:
            self.validate_site()
        record['site_number'] = self.site_number
        if is_child:
            record.pop('sample_analysis_id')
        return dict(record)

    #### common tables ####
    def save_app_log(self, local_record: dict) -> int:
        ''' saves log record to db, returns id '''
        record = self._update_local_fields(local_record, 'id')
        db_id = self._save_record(self.APP_LOG_TABLE, record)
        return db_id

    def check_app_log(self, local_record_id: int) -> bool:
        ''' checks if the record is already in the remote db '''
        return self._check_record(self.APP_LOG_TABLE, local_record_id)

    #### smps tables ####
    def save_smps_measurement(self, local_record: dict) -> int:
        ''' saves record to db, returns id '''
        #print('Adding measurement ', local_record['id'])
        record = self._update_local_fields(local_record, 'id')
        # need to convert json fields back to text
        json_columns = ['concentration_json', 'raw_concentration_json']
        for column_name in json_columns:
            if local_record[column_name] is None:
                continue
            record[column_name] = json.dumps(local_record[column_name])
        db_id = self._save_record(self.SMPS_MEASUREMENTS_TABLE, record)
        return db_id

    def save_smps_setting(self, local_record: dict) -> int:
        ''' saves record to db, returns id '''
        print('Adding setting ', local_record['id'])
        record = self._update_local_fields(local_record, 'id')
        db_id = self._save_record(self.SMPS_SETTINGS_TABLE, record)
        return db_id

    def update_smps_setting(self, local_record: dict) -> bool:
        ''' updated remote db record '''
        print('Updating setting ', local_record['id'])
        return self._update_record(self.SMPS_SETTINGS_TABLE, local_record, 'id')

    def check_smps_measurement(self, local_record_id: int) -> bool:
        ''' checks that the record is already in the remote database '''
        return self._check_record(self.SMPS_MEASUREMENTS_TABLE, local_record_id)

    def check_smps_setting(self, local_record_id: int) -> bool:
        ''' checks that the record is already in the remote db '''
        return self._check_record(self.SMPS_SETTINGS_TABLE, local_record_id)

    #### xact tables ####
    def save_xact_measurement(self, parent_record: dict, child_records: list) -> int:
        ''' saves parent record and child record to db, returns parent id '''
        #print('Adding measurement ', local_record['id'])
        parent_record = self._update_local_fields(parent_record, 'id')
        for child_record in child_records:
            child_record = self._update_local_fields(child_record, 'id', is_child=True)
        db_id = self._save_record_with_child(self.XACT_SAMPLE_ANALYSIS_TABLE, parent_record,
                                             self.XACT_RAW_MEASUREMENTS_TABLE, child_records)
        return db_id

    def check_xact_measurement(self, local_record_id: int) -> bool:
        ''' checks that the record is already in the remote database '''
        return self._check_record(self.XACT_SAMPLE_ANALYSIS_TABLE, local_record_id)
    
    def save_xact_setting(self, local_record: dict) -> int:
        ''' saves record to db, returns id '''
        print('Adding setting ', local_record['id'])
        record = self._update_local_fields(local_record, 'id')
        db_id = self._save_record(self.XACT_SETTINGS_TABLE, record)
        return db_id

    def update_xact_setting(self, local_record: dict) -> bool:
        ''' updated remote db record '''
        print('Updating setting ', local_record['id'])
        return self._update_record(self.XACT_SETTINGS_TABLE, local_record, 'id')

    def check_xact_setting(self, local_record_id: int) -> bool:
        ''' checks that the record is already in the remote db '''
        return self._check_record(self.XACT_SETTINGS_TABLE, local_record_id)
    
    #### acsm tables ####
    def check_acsm_measurement(self, local_record_id: int) -> bool:
        ''' checks that the record is already in the remote database '''
        return self._check_record(self.ACSM_DATA_TABLE, local_record_id)
    
    def save_acsm_measurement(self, parent_record: dict, children_records: tuple) -> int:
        ''' saves record to db, returns id '''
        print('Adding measurement ', parent_record['id'])
        conn = self.get_connection()
        cursor = conn.cursor()
        #change 'id' column to 'site_record_id', add 'site_number'
        parent_record['site_record_id'] = parent_record.pop('id')
        parent_record['site_number'] = self.site_number

        #upload parent record, get parent_record_remote_db_id to use for children's 'sample_analysis_id' fields
        parent_record_remote_db_id = self._save_partial_record(self.ACSM_DATA_TABLE, parent_record, cursor)
        for child_record in children_records:
            child_record['record']['site_record_id'] = child_record['record'].pop('id')
            child_record['record']['sample_analysis_id'] = parent_record_remote_db_id
            self._save_partial_record(child_record['table_name'], child_record['record'], cursor)
        cursor.close()
        conn.commit()
        #todo: check if actually uploaded, done in calling function already but only for parent table
        return parent_record_remote_db_id
    
    def check_dryerstats_measurement(self, local_record_id: int) -> bool:
        ''' checks that the record is already in the remote database '''
        return self._check_record(self.DRYERSTATS_TABLE, local_record_id)
    
    def save_dryerstats_measurement(self, local_record: dict) -> int:
        ''' saves record to db, returns id '''
        #print('Adding measurement ', local_record['id'])
        record = self._update_local_fields(local_record, 'id')
        db_id = self._save_record(self.DRYERSTATS_TABLE, record)
        return db_id