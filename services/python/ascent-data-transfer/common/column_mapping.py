'''
wrapper with some validation for the common.column_mappings table
'''



class ColumnMapping():
    ''' wrapper for common.column_mappings '''
    def __init__(self, db_record):
        self._db_record = db_record

    @property
    def file_column_name(self) -> str:
        ''' file_column_name '''
        return self._db_record['file_column_name']

    @property
    def db_column_name(self) -> str:
        ''' db_column_name '''
        return self._db_record['db_column_name']
    
    @property
    def column_type(self) -> str:
        ''' column_type '''
        return self._db_record['column_type']
    
    @property
    def pd_column_type(self) -> str:
        ''' returns the pandas column type '''
        # some of the types don't match the pandas types
        dtype = self.column_type
        if 'TIMESTAMP' in dtype:
            dtype = 'datetime64[ns]'
        if 'VARCHAR' in dtype:
            dtype = 'string'
        if 'SMALLINT' in dtype:
            dtype = 'int64'
        if 'NUMERIC' in dtype:
            dtype = 'float64'
        if 'INTEGER' in dtype:
            dtype = 'int64'
        valid_types = ['datetime64[ns]', 'float64', 'int64', 'string']
        if dtype not in valid_types:
            entry = str(self._db_record)
            raise ValueError('Invalid column mapping type: ' + dtype + ' (record: ' + entry + ')')
        return dtype

    @property
    def column_required(self) -> bool:
        ''' required '''
        return self._db_record['column_required']

    @property
    def value_required(self) -> bool:
        ''' column needs a value '''
        return self._db_record['value_required']

    @property
    def grouping(self) -> str:
        ''' grouped columns '''
        return self._db_record['grouping']