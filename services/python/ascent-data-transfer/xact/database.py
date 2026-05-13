'''
All database transactions should be in this file
'''
from datetime import datetime
from psycopg2.extensions import connection
from psycopg2.extras import RealDictCursor
from pgcopy import CopyManager
from pandas import DataFrame
from common.local_database import LocalDatabase
from common.instrument_database import InstrumentDatabase

class XactDatabase(InstrumentDatabase):
    ''' has all database interactions '''
    # table names
    XACT_SETTINGS_TABLE = 'xact.instrument_settings'
    XACT_DATA_TABLE = 'xact.sample_analysis'
    XACT_RAW_MEASUREMENTS_TABLE = 'xact.raw_measurements'
    XACT_XC_VER_COLUMN = 'xc_ver'

    def __init__(self, database_connection: LocalDatabase):
        InstrumentDatabase.__init__(self, database_connection)

    def get_settings(self) -> list[dict]:
        ''' gets instrument settings for Xact '''
        cursor = self.active_connection.cursor(cursor_factory=RealDictCursor)
        sql_query = f"SELECT * FROM {self.XACT_SETTINGS_TABLE} WHERE end_date IS NULL"
        cursor.execute(sql_query)
        results = cursor.fetchall()
        records = [dict(record) for record in results]
        cursor.close()
        return records
    
    def end_settings_record(self, record_id: int, end_date: datetime):
        ''' updates the record with an end date '''
        cursor = self.active_connection.cursor()
        sql = f'UPDATE {self.XACT_SETTINGS_TABLE} SET end_date = %s WHERE id = %s'
        cursor.execute(sql, (end_date, record_id))
        print(cursor.query)
        rows_updated = cursor.rowcount
        print('rows updated:', rows_updated)
        self.add_synch_record(self.XACT_SETTINGS_TABLE, record_id, True)
        cursor.close()

    def add_settings_record(self, field_name: str, field_value: str, start_date: datetime) -> bool:
        ''' adds a new settings record '''
        record = {
            'name': field_name,
            'value': field_value,
            'start_date': start_date
        }
        return self._save_record(self.XACT_SETTINGS_TABLE, record)

    def is_record_new(self, db_key_column: str, key_value) -> bool:
        ''' checks if record already added '''
        return self._is_record_new(self.XACT_DATA_TABLE, db_key_column, key_value)

    def save_partial_record(self, record: DataFrame, table_name: str, columns: list, conn: connection):
        ''' Save partial record to database without committing it '''
        # todo: this should return int db_id
        # convert df into a tuple for use with CopyManager
        record = list(record.itertuples(index=False))
        mgr = CopyManager(conn, table_name, columns)
        mgr.copy(record)

    def save_record_with_child(self, parent_record: DataFrame, parent_table_name: str,
                               child_records: DataFrame, child_table_name: str):
        ''' Save record atomically, inserting both parent and child records. 
        Adds synch record at the end for parent record id'''
        self.start_transaction()
        conn = self.active_connection
        self.save_partial_record(parent_record, parent_table_name, parent_record.columns, conn)
        cursor = conn.cursor()
        date_imported = str(parent_record.date_imported[0])
        # todo: make more specific:
        # sql_get_sample_id = f"""SELECT id FROM {sa_table}
        #             WHERE date_imported = '{date_imported}'
        #             AND site_number = {site_number}
        #             AND sample_datetime = '{sample_datetime}'"""
        query = f"SELECT id FROM {parent_table_name} WHERE date_imported = '{date_imported}'"
        cursor.execute(query)
        parent_db_id = cursor.fetchone()[0]
        child_records.insert(0, 'sample_analysis_id', parent_db_id)
        # to do: check if parent_db_id is valid/not null
        # to do: change insert/copy method to return the db_id and use it to add synch record below
        # Add synch record
        # Synch record is not to be added for a (child) xact.raw_measurements table.
        # Its sync will be handled by its parent, xact.sample_analysis table.
        self.save_partial_record(child_records, child_table_name, child_records.columns, conn)
        conn.commit()
        cursor.close()
        self.add_synch_record(parent_table_name, parent_db_id)

    def save_record(self, record, table, columns):
        ''' Posts record to a provided table, and adds to syncing queue'''
        # post to DB
        self.start_transaction()
        conn = self.active_connection
        self.save_partial_record(record, table, columns, conn)

    def get_measurement(self, record_id: int) -> dict:
        ''' gets measurement from database '''
        return self._get_record(self.XACT_DATA_TABLE, record_id)

    def get_child_records(self, parent_record_id: int) -> list:
        ''' gets child records for a givent parent record id from database'''
        return self._get_child_records(self.XACT_RAW_MEASUREMENTS_TABLE, 'sample_analysis_id', parent_record_id)
    
    def get_setting(self, record_id: int) -> dict:
        ''' gets instrument setting from database '''
        return self._get_record(self.XACT_SETTINGS_TABLE, record_id)
