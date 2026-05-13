'''
Collection of main loop db functions
'''


from psycopg2.extras import RealDictCursor
from common.database import DatabaseConnectionParameters, DatabaseConnectionPg
from common.synch_record import SynchRecord


class LocalDatabase(DatabaseConnectionPg):
    ''' Connection for database '''

    # table names
    APP_LOGS_TABLE = 'common.app_logs'

    def __init__(self, connection_parameters: DatabaseConnectionParameters):
        DatabaseConnectionPg.__init__(self, connection_parameters)

    def get_data_tables(self) -> dict[str, str]:
        ''' get the data table records that map table name to instrument type '''
        conn = self.connect()
        cursor = conn.cursor(cursor_factory=RealDictCursor)
        sql_query = "SELECT * FROM common.data_tables"
        cursor.execute(sql_query)
        results = cursor.fetchall()
        mappings = {}
        for record in results:
            key = record['table_name']
            value = record['instrument']
            mappings[key] = value
        cursor.close()
        conn.close()
        return mappings

    def is_pending_synch(self) -> int:
        ''' checks if there are any pending synch record '''
        conn = self.connect()
        cursor = conn.cursor()
        sql_query = 'SELECT EXISTS (SELECT 1 FROM common.synchronize)'
        cursor.execute(sql_query)
        exists = cursor.fetchone()[0]
            #print(cursor.query)
            #print('Record already exists.')
        return exists

    def get_next_synch(self) -> list[SynchRecord]:
        ''' gets pending synch records '''
        instrument_table = self.get_data_tables()
        synch_records = []
        for table_name in instrument_table.keys():
            conn = self.connect()
            cursor = conn.cursor(cursor_factory=RealDictCursor)
            sql_query = "SELECT * FROM common.synchronize WHERE table_name = %s ORDER BY record_id DESC LIMIT 100"
            cursor.execute(sql_query, (table_name,))
            results = cursor.fetchall()
            for record in results:
                synch_records.append(SynchRecord(dict(record), instrument_table ))
            cursor.close()
            conn.close()
        return synch_records

    def clear_synch_record(self, synch_record: SynchRecord):
        ''' removes entry from queue '''
        #print('Removing synch record, ', synch_record)
        conn = self.connect()
        cursor = conn.cursor()
        sql_query = 'DELETE FROM common.synchronize WHERE table_name = %s AND record_id = %s'
        cursor.execute(sql_query, (synch_record.table_name, synch_record.record_id))
        cursor.close()
        conn.commit()
        conn.close()

    def get_log_record(self, record_id: int) -> dict:
        ''' gets log record '''
        conn = self.connect()
        cursor = conn.cursor(cursor_factory=RealDictCursor)
        sql_query = f"SELECT * FROM {self.APP_LOGS_TABLE} WHERE id = %s"
        cursor.execute(sql_query, (record_id,))
        results = cursor.fetchall()
        if len(results) == 0:
            msg = f'Failed to find {self.APP_LOGS_TABLE} record with id "{record_id}"'
            raise RuntimeError(msg)
        record = dict(results[0])
        cursor.close()
        conn.close()
        return record