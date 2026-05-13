'''
All database transactions should be in this file
'''

from datetime import datetime
from psycopg2.extras import RealDictCursor
from common.local_database import LocalDatabase
from common.instrument_database import InstrumentDatabase
import logging
logger = logging.getLogger(__name__)

class AcsmDatabase(InstrumentDatabase):
    ''' has all database interactions '''
    # table names
    ACSM_DATA_TABLE = 'acsm.sample_analysis'
    ACSM_CHILDREN_TABLES = ['acsm.mass_loadings', 'acsm.diag_calib', 'acsm.tps']
    PARENT_COLUMN_NAME = 'sample_analysis_id'

    def __init__(self, database_connection: LocalDatabase):
        InstrumentDatabase.__init__(self, database_connection)

    def get_children_records(self, parent_id: str) -> tuple:
        children_records = ()  # Initialize an empty tuple to store child records
        for child_table in self.ACSM_CHILDREN_TABLES:
            sql_query = f'SELECT * FROM {child_table} WHERE {self.PARENT_COLUMN_NAME} = {parent_id}'
            # todo: connection is not active at this point, shouldn't it be?
            if self.active_connection is None:
                self.active_connection = self.dbc.connect()
            # cursor = self.active_connection.cursor(cursor_factory=RealDictCursor)
            with self.active_connection.cursor(cursor_factory=RealDictCursor) as cursor:
                cursor.execute(sql_query)
                child_record = cursor.fetchone()
                if child_record:
                    child_data = {
                        'table_name': child_table,
                        'record': child_record
                    }
                    children_records += (child_data,)  # Add the dictionary to the tuple
                    
        return children_records
    
    def is_record_new(self, table_name: str, db_column_name_1: str, db_column_name_2: str, key_value_1, key_value_2) -> bool:
        ''' check if record is already in db based on two columns'''
        sql_query = f'SELECT EXISTS (SELECT 1 FROM {table_name} WHERE {db_column_name_1} = %s AND {db_column_name_2} = %s)'
        with self.active_connection.cursor() as cursor:
            cursor.execute(sql_query, (key_value_1, key_value_2 ))
            exists = cursor.fetchone()[0]
            return not exists

    def save_record(self, table_name: str, record: dict) -> int:
        ''' uses active connection '''
        column_names = []
        column_values = []
        for (column_name, value) in record.items():
            column_names.append(column_name)
            column_values.append(value)
        placeholders = ['%s'] * len(column_names)
        sql_query = f'INSERT INTO {table_name} ({",".join(column_names)})'
        sql_query += f' VALUES ({",".join(placeholders)})'
        sql_query += ' RETURNING id'
        cursor = self.active_connection.cursor()
        cursor.execute(sql_query, tuple(column_values))
        db_id = cursor.fetchone()[0]
        logger.debug(f"New record id: {db_id}")
        if table_name is self.ACSM_DATA_TABLE:
            self.add_synch_record(table_name, db_id)
        cursor.close()
        return db_id

    def save_measurement(self, record_list):
        for group_name, group_data in record_list:
            if group_name == 'sample_analysis':
                parent_id = self.save_record('acsm.sample_analysis', group_data)
            else:
                # meaning its a child group
                # add into the group the parent id 
                group_data['sample_analysis_id'] = parent_id
                self.save_record('acsm.' + group_name, group_data)
                # save_table (child table)
        self.active_connection.commit()

    def get_measurement(self, record_id: int) -> dict:
        ''' gets measurement from datbase '''
        return self._get_record(self.ACSM_DATA_TABLE, record_id)