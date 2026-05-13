'''
All database transactions should be in this file
'''

from datetime import datetime
from psycopg2.extras import RealDictCursor
from common.local_database import LocalDatabase
from common.instrument_database import InstrumentDatabase


class SmpsDatabase(InstrumentDatabase):
    ''' has all database interactions '''
    # table names
    SMPS_SETTINGS_TABLE = 'smps.instrument_settings'
    SMPS_DATA_TABLE = 'smps.sample_analysis'

    def __init__(self, database_connection: LocalDatabase):
        InstrumentDatabase.__init__(self, database_connection)

    def get_settings(self) -> list[dict]:
        ''' gets instrument settings for smps '''
        cursor = self.active_connection.cursor(cursor_factory=RealDictCursor)
        sql_query = f"SELECT * FROM {self.SMPS_SETTINGS_TABLE} WHERE end_date IS NULL"
        cursor.execute(sql_query)
        results = cursor.fetchall()
        records = [dict(record) for record in results]
        cursor.close()
        return records

    def end_settings_record(self, record_id: int, end_date: datetime):
        ''' updates the record with an end date '''
        cursor = self.active_connection.cursor()
        sql = f'UPDATE {self.SMPS_SETTINGS_TABLE} SET end_date = %s WHERE id = %s'
        cursor.execute(sql, (end_date, record_id))
        print(cursor.query)
        rows_updated = cursor.rowcount
        print('rows updated:', rows_updated)
        self.add_synch_record(self.SMPS_SETTINGS_TABLE, record_id, True)
        cursor.close()

    def add_settings_record(self, field_name: str, field_value: str, start_date: datetime) -> bool:
        ''' adds a new settings record '''
        record = {
            'name': field_name,
            'value': field_value,
            'start_date': start_date
        }
        return self._save_record(self.SMPS_SETTINGS_TABLE, record)

    def is_record_new(self, db_key_column: str, key_value) -> bool:
        ''' checks if record already added '''
        return self._is_record_new(self.SMPS_DATA_TABLE, db_key_column, key_value)

    def save_measurement(self, record: dict) -> bool:
        ''' saves a measurement to the database, dict keys should already match column names '''
        return self._save_record(self.SMPS_DATA_TABLE, record)

    def get_setting(self, record_id: int) -> dict:
        ''' gets instrument setting from database '''
        return self._get_record(self.SMPS_SETTINGS_TABLE, record_id)

    def get_measurement(self, record_id: int) -> dict:
        ''' gets measurement from database '''
        return self._get_record(self.SMPS_DATA_TABLE, record_id)