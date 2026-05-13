'''
Used to make dealing with the synch records easier
'''



class SynchRecord():
    ''' used to make dealing with the synch records easier '''
    def __init__(self, db_record: dict, instrument_table: dict):
        self._db_record = db_record
        self.instrument = self._set_instrument(self.table_name, instrument_table)

    def _set_instrument(self, table_name: str, instrument_table: dict) -> str:
        ''' converts table_name to instrument using lookup '''
        if table_name in instrument_table:
            return instrument_table[table_name]
        msg = f'Missing table entry in table "data_tables" for table "{table_name}"'
        raise IndexError(msg)
    
    def __str__(self):
        ''' for print() '''
        text = f'"{self.update_type} {self.table_name} id {self.record_id}"'
        return text

    @property
    def table_name(self) -> str:
        ''' updated table name '''
        return self._db_record['table_name']

    @property
    def record_id(self) -> int:
        ''' updated record id '''
        return int(self._db_record['record_id'])

    @property
    def update_type(self) -> str:
        ''' should be either insert, updated, or delete '''
        return self._db_record['update_type']
    