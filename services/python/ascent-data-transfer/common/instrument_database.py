'''
Shared functions for the different instrument sections
'''

from psycopg2.extras import RealDictCursor
from common.column_mapping import ColumnMapping
from common.local_database import LocalDatabase


class InstrumentDatabase():
    ''' Base class for instrument specific db interfaces '''
    def __init__(self, db: LocalDatabase):
        self.dbc = db
        self.active_connection = None

    def start_transaction(self):
        ''' creates the connection '''
        self.active_connection = self.dbc.connect()

    def end_transaction(self):
        ''' ends the connection '''
        self.active_connection.commit()
        self.active_connection.close()

    def get_column_mapping(self, schema: str, table_name: str) -> list[ColumnMapping]:
        ''' gets the data file to db column mapping '''
        conn = self.dbc.connect()
        cursor = conn.cursor(cursor_factory=RealDictCursor)
        sql_query = "SELECT * FROM common.column_mappings WHERE table_schema = %s AND table_name = %s"
        cursor.execute(sql_query, (schema, table_name))
        results = cursor.fetchall()
        mappings = []
        for record in results:
            mapping = ColumnMapping(dict(record))
            mappings.append(mapping)
        cursor.close()
        conn.close()
        return mappings

    def _get_record(self, table_name: str, record_id: int) -> dict:
        ''' gets record from table '''
        conn = self.dbc.connect()
        cursor = conn.cursor(cursor_factory=RealDictCursor)
        sql_query = f"SELECT * FROM {table_name} WHERE id = %s"
        cursor.execute(sql_query, (record_id,))
        results = cursor.fetchall()
        if len(results) == 0:
            msg = f'Failed to find {table_name} record with id "{record_id}"'
            raise RuntimeError(msg)
        record = dict(results[0])
        cursor.close()
        conn.close()
        return record

    def _is_record_new(self, table_name: str, db_column_name: str, key_value) -> bool:
        ''' check if record is already in db '''
        sql_query = f'SELECT EXISTS (SELECT 1 FROM {table_name} WHERE {db_column_name} = %s)'
        with self.active_connection.cursor() as cursor:
            cursor.execute(sql_query, (key_value, ))
            exists = cursor.fetchone()[0]
            return not exists

    def _save_record(self, table_name: str, record: dict) -> bool:
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
        print('new record id:', db_id)
        cursor.close()
        self.add_synch_record(table_name, db_id)

    def add_synch_record(self, table_name: str, record_id: int, is_update: bool = False):
        ''' Add entry to synch queue '''
        # set the update type
        update_type = "insert"
        if is_update:
            update_type = "update"
        # check if record is already added
        #  (this could happen for updates if there was a delay in synchronizing)
        sql_query = 'SELECT EXISTS (SELECT 1 FROM common.synchronize WHERE table_name = %s AND record_id = %s)'
        with self.active_connection.cursor() as cursor:
            cursor.execute(sql_query, (table_name, record_id))
            exists = cursor.fetchone()[0]
            if exists:
                print('Sync record already added for', table_name, 'record ', record_id)
                return
        # insert record
        cursor = self.active_connection.cursor()
        sql_query = "INSERT INTO common.synchronize "
        sql_query += "(table_name, record_id, update_type) "
        sql_query += "VALUES (%s, %s, %s) RETURNING record_id"
        cursor.execute(sql_query, (table_name, record_id, update_type))
        db_id = cursor.fetchone()[0]
        print('added synch record for:', table_name, db_id)
        cursor.close()

    def _get_child_records(self, table_name: str, parent_record_id_column: str, parent_record_id: int) -> list:
        ''' gets child records from a table given its parent record id, returns a list of child records '''
        conn = self.dbc.connect()
        cursor = conn.cursor(cursor_factory=RealDictCursor)
        sql_query = f"SELECT * FROM {table_name} WHERE {parent_record_id_column} = %s"
        cursor.execute(sql_query, (parent_record_id,))
        results = cursor.fetchall()
        if len(results) == 0:
            msg = f'Failed to find {table_name} record with id "{parent_record_id}"'
            raise RuntimeError(msg)
        cursor.close()
        conn.close()
        return results
