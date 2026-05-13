'''
All database transactions should be in this file
'''

from datetime import datetime
from psycopg2.extras import RealDictCursor
from common.local_database import LocalDatabase
from common.instrument_database import InstrumentDatabase


class DryerstatsDatabase(InstrumentDatabase):
    ''' has all database interactions '''
    # table names
    DRYERSTATS_DATA_TABLE = 'acsm.dryer_stats'

    def __init__(self, database_connection: LocalDatabase):
        InstrumentDatabase.__init__(self, database_connection)

    def is_record_new(self, db_key_column: str, key_value) -> bool:
        ''' checks if record already added '''
        return self._is_record_new(self.DRYERSTATS_DATA_TABLE, db_key_column, key_value)

    def save_measurement(self, record: dict) -> bool:
        ''' saves a measurement to the database, dict keys should already match column names '''
        return self._save_record(self.DRYERSTATS_DATA_TABLE, record)

    def get_measurement(self, record_id: int) -> dict:
        ''' gets measurement from database '''
        return self._get_record(self.DRYERSTATS_DATA_TABLE, record_id)