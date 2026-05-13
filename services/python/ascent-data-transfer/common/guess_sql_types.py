'''

'''


import pandas as pd
from decimal import Decimal 
from difflib import ndiff


class ColumnSqlType:
    def __init__(self, column: pd.Series):
        ''' '''
        self.column = column
        self.column_name = column.name
        self.sql_type = ''
        self.dtype = ''
        self.notes = ''
        self.missing_count = 0
        self.min = None
        self.max = None
        self.process()

    def get_number_dimensions(self, number):
        # because of floating point approximation, need to convert back to string to get proper number of digits
        n = str(number) 
        if n == 'nan':
            return pd.Series([None, None])
        d = Decimal(n)
        t = d.as_tuple()
        precision = len(t.digits)
        scale = -(t.exponent)
        #print(f'{number} => t:{t} => d:{digits}, l:{len(digits)}, e:{exp}')
        return pd.Series([precision, scale])

    def process(self):
        ''' '''
        self.dtype = self.column.dtype
        type_map = {
            'int64':self.process_int64,
            'float64':self.process_float64,
            'string':self.process_string,
            'datetime64[ns]':self.process_datetime
            }
        if not str(self.dtype) in type_map:
            print(f'type {self.dtype} not found')
            return
        # check column values according to type
        type_map[str(self.dtype)]()
        if self.missing_count == 0:
            self.sql_type += ' NOT NULL'

    def process_datetime(self):
        (self.min, self.max) = self.column.agg(['min', 'max'])
        self.missing_count = self.column.isnull().sum()
        self.notes = f'min: {self.min},\t max: {self.max},\t missing: {self.missing_count}'
        self.sql_type = 'TIMESTAMP(0)' # (p) with time zone?

    def process_int64(self):
        (self.min, self.max) = self.column.agg(['min', 'max'])
        self.missing_count = self.column.isnull().sum()
        self.notes = f'min: {self.min},\t max: {self.max},\t missing: {self.missing_count}'
        self.sql_type = 'INTEGER' # int4 or bigint/int8

    def process_float64(self):
        (self.min, self.max) = self.column.agg(['min', 'max'])
        self.missing_count = self.column.isnull().sum()
        dimensions = self.column.apply(self.get_number_dimensions)
        precision = dimensions[0].max()
        scale = dimensions[1].max()
        self.sql_type = f'NUMERIC({precision}, {scale})' # float8 or numeric(p,s)
        self.notes = f'min: {min},\t max: {max},\t missing: {self.missing_count}'

    def process_string(self):
        lengths = self.column.astype('bytes').str.len()
        self.min = lengths.min()
        self.max = lengths.max()
        self.missing_count = self.column.isnull().sum()
        self.notes = f'min: {self.min},\t max len: {self.max},\t missing: {self.missing_count}'
        self.sql_type = 'VARCHAR(' + str(self.max) + ')'

    @staticmethod
    def diff_files(file1, file2):
        left = ''
        with open(file1, 'r') as handle:
            left = handle.readlines()
        right = ''
        with open(file2, 'r') as handle:
            right = handle.readlines()
        mixed = ndiff(left, right)
        print(''.join(mixed), end='')
