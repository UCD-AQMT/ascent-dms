'''
parses the dryerstats data file
'''

import csv
import json
import pathlib
import re
import pandas as pd
from common.column_mapping import ColumnMapping
from dryerstats.database import DryerstatsDatabase
from common.helpers import add_start_end_timestamps
import logging

logger = logging.getLogger(__name__) 

class DryerstatsDataFile():
    ''' data file '''

    KEY_COLUMN = "DateTime"

    def __init__(self, file: pathlib.Path, measurement_mapping: list[ColumnMapping]):
        self.filename = file
        self.column_mappings = measurement_mapping
        self.records = []
        self.missing_columns = []
        self.extra_columns = []
        self.first_date = None
        self.import_notes = ''
        self.new_record_count = 0

    def _add_note(self, note: str, has_issue: bool = False):
        """Collects all notes and logs them instead of printing."""
        self.import_notes += f'{note}\n'
        logger.info(note)  # General notes are logged as info

    def parse(self):
        ''' split file into instrument settings '''
        data = pd.read_csv(str(self.filename),sep='\t')
        # data = data.loc[:, ~data.columns.str.contains('^Unnamed')]
        self.check_columns(data)
        self.adjust_column_types(data)
        #todo: check why year is 2024.0 not an int
        #self.script_extra_columns(data)
        self.build_records(data)

    def check_columns(self, data: pd.DataFrame):
        ''' checks that the expected columns exist '''
        expected_columns = [mapping.file_column_name for mapping in self.column_mappings]
    
        file_columns = [col for col in data.columns]
        self.missing_columns = list(set(expected_columns) - set(file_columns))
        self.extra_columns = list(set(file_columns) - set(expected_columns))
        if self.missing_columns:
            msg = f'Missing columns({len(self.missing_columns)}): {str(sorted(self.missing_columns))}'
            self._add_note(msg)
        if self.extra_columns:
            logger.debug(f'Extra columns({len(self.extra_columns)}): {sorted(self.extra_columns)}')
            # self._add_note(f'Extra columns({len(self.extra_columns)}): {sorted(self.extra_columns)}')

    def adjust_column_types(self, data: pd.DataFrame):
        ''' converts column types to whatever is in the mapping file (pandas just guesses) '''
        for mapping in self.column_mappings:
            dtype = mapping.pd_column_type
            key = mapping.file_column_name
            if key not in data.columns:
                # column missing so skip
                continue
            if (data[key].dtype) == dtype:
                # already matches so nothing to change
                continue
            logger.debug(f'Adjusting column "{key}" from type "{data[key].dtype}" to type "{dtype}"')
            data[key] = data[key].astype(dtype)

    def build_records(self, data: pd.DataFrame):
        ''' convert from dataframe to records '''
        self.records = data.to_dict("records")

    def save_data(self, db: DryerstatsDatabase) -> bool:
        ''' save file records '''
        db.start_transaction()
        self.new_record_count = 0
        for record in self.records:
            if self.save_record(record, db):
                self.new_record_count += 1
        db.end_transaction()
        self._add_note(f'Added {self.new_record_count} measurements')
        return True

    def save_record(self, record: dict, db: DryerstatsDatabase) -> bool:
        ''' adds record to database if it is new '''
        # if the unique column is missing we can't proceed.
        # The check columns will have already caught this.
        if self.KEY_COLUMN not in record:
            return False
        # check if record is already in database
        key_value = record[self.KEY_COLUMN]
        if not db.is_record_new(self.KEY_COLUMN, key_value):
            print("record is not new")
            return False
        # record is new
        db_record = {}
        
        # convert mapping list to dictionary for quick lookup
        mapping_lookup = {}
        for mapping in self.column_mappings:
            mapping_lookup[mapping.file_column_name] = mapping

        for field in record.keys():
            if field not in mapping_lookup:
                # this is handled in the column check so skip missing columns here
                continue
            mapping = mapping_lookup[field]
            
            if mapping.grouping is not None:
                db_field = mapping.db_column_name
                print(mapping.grouping)
                # if mapping.grouping == 'sample_analysis':
                #     if db_field not in sample_analysis_group:
                #         sample_analysis_group[db_field] = record[field]
                # elif mapping.grouping == 'mass_loadings':
                #     if db_field not in mass_loadings_group:
                #         mass_loadings_group[db_field] = record[field]
                # elif mapping.grouping == 'diag_calib':
                #     if db_field not in diag_calib_group:
                #         diag_calib_group[db_field] = record[field]
                # elif mapping.grouping == 'tps':
                #     if db_field not in tps_group:
                #         tps_group[db_field] = record[field]

            else:
                db_field = mapping.db_column_name
                if db_field is not None:
                    db_record[db_field] = record[field]

        db.save_measurement(db_record)
        return True
