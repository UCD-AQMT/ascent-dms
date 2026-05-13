'''
parses the smps data file
'''

import csv
import json
import pathlib
import re
import pandas as pd
from common.column_mapping import ColumnMapping
from smps.database import SmpsDatabase
from smps.instrument_settings import InstrumentSettings

class SmpsDataFile():
    ''' data file '''
    KEY_FILE_COLUMN = 'DateTime Sample Start'

    def __init__(self, file: pathlib.Path, measurement_mapping: list[ColumnMapping]):
        self.filename = file
        self.settings = InstrumentSettings()
        self.column_mappings = measurement_mapping
        self.records = []
        self.missing_columns = []
        self.extra_columns = []
        self.first_date = None
        self.import_notes = ''
        self.has_issues = False
        self.new_record_count = 0

    def _add_note(self, note: str, has_issue: bool = False):
        ''' used to collect all the notes for adding to logging '''
        print(' -', note)
        self.import_notes += f'{note}\n'
        if has_issue:
            self.has_issues = True

    def parse(self):
        ''' split file into instrument settings '''
        skiprows = 0
        instrument_settings = {}
        with open(str(self.filename), 'r', encoding='utf-8-sig') as csvfile:
            csvreader = csv.reader(csvfile)
            # get the settings
            for row in csvreader:
                skiprows += 1
                # if we hit a blank row we are done getting settings
                if not row[0]:
                    break
                instrument_settings[row[0]] = row[1]
            self.settings.load_settings(instrument_settings)
            # next go through the blank space
            for row in csvreader:
                # go until we find the first non blank row
                if row[0]:
                    break
                skiprows += 1
        data = pd.read_csv(str(self.filename), skiprows=skiprows, dayfirst=True, parse_dates=['DateTime Sample Start'])
        self.check_columns(data)
        self.adjust_column_types(data)
        self.update_first_timestamp(data)
        #self.script_extra_columns(data)
        self.build_records(data)

    def update_first_timestamp(self, data: pd.DataFrame):
        ''' finds the first timestamp from the file '''
        if self.KEY_FILE_COLUMN not in data.columns:
            return
        column = data[self.KEY_FILE_COLUMN]
        if len(column) < 1:
            return
        # assuming column is sorted
        self.first_date = column.iat[0]
        last_date = column.max()
        self._add_note(f'File date range: {self.first_date} - {last_date}')

    def _add_json_bin_field(self, field: str, json_column: str, column_group: str):
        ''' dynamically add size and concentration columns to the correct grouping '''
        stub = {
            'file_column_name': field,
            'db_column_name': json_column,
            'column_type': 'float64',
            'column_required': True,
            'value_required': True,
            'grouping': column_group
            }
        mapping = ColumnMapping(stub)
        self.column_mappings.append(mapping)

    def check_columns(self, data: pd.DataFrame):
        ''' checks that the expected columns exist '''
        expected_columns = [mapping.file_column_name for mapping in self.column_mappings]
        file_columns = [col for col in data.columns]
        self.missing_columns = list(set(expected_columns) - set(file_columns))
        self.extra_columns = list(set(file_columns) - set(expected_columns))
        extra_bin_count = 0
        for column in self.extra_columns.copy():
            if re.search(r'^\d+[.]\d+$', column) is not None:
                self._add_json_bin_field(column, 'concentration_json', 'concen')
                self.extra_columns.remove(column)
                extra_bin_count += 1
            elif re.search(r'^_\d+[.]\d+$', column) is not None:
                self._add_json_bin_field(column, 'raw_concentration_json', 'raw_concen')
                self.extra_columns.remove(column)
                extra_bin_count += 1
        if self.missing_columns:
            msg = f'Missing columns({len(self.missing_columns)}): {str(sorted(self.missing_columns))}'
            self._add_note(msg)
        if self.extra_columns:
            self._add_note(f'Extra columns({len(self.extra_columns)}): {sorted(self.extra_columns)}')
        if extra_bin_count:
            self._add_note(f'Added {extra_bin_count} size/concentration columns')

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
            self._add_note(f'Adjusting column "{key}" from type "{data[key].dtype}" to type "{dtype}"')
            data[key] = data[key].astype(dtype)

    def script_extra_columns(self, data: pd.DataFrame):
        ''' used for generating records for the column_mappings table '''
        sql_text = ''
        print('Generating column_mappings for extra columns')
        for column_name in data.columns:
            # we want the order to be file order
            if str(column_name) not in self.extra_columns:
                continue
            column_type = str(data[column_name].dtype)
            row_text = "\t('smps', 'sample_analysis', "
            row_text += f"'{column_name}',\t'blank', "
            row_text += f"'{column_type}', true, true, ''),\n"
            sql_text += row_text
        with open('column_mapping_text.txt', 'w', encoding='utf-8') as handle:
            handle.write(sql_text)

    def build_records(self, data: pd.DataFrame):
        ''' convert from dataframe to records '''
        self.records = data.to_dict("records")

    def save_data(self, db: SmpsDatabase) -> bool:
        ''' save file records '''
        db.start_transaction()
        # update settings
        self.settings.update_database(self.first_date, db)
        self.new_record_count = 0
        for record in self.records:
            if self.save_record(record, db):
                self.new_record_count += 1
        db.end_transaction()
        self._add_note(f'Added {self.new_record_count} measurements')
        return True

    def save_record(self, record: dict, db: SmpsDatabase) -> bool:
        ''' adds record to database if it is new '''
        # if the unique column is missing we can't proceed.
        #  The check columns will have already caught this.
        if self.KEY_FILE_COLUMN not in record:
            return False
        # convert mapping list to dictionary for quick lookup
        mapping_lookup = {}
        for mapping in self.column_mappings:
            mapping_lookup[mapping.file_column_name] = mapping
        # get the database column name for the key
        db_column_name = mapping_lookup[self.KEY_FILE_COLUMN].db_column_name
        key_value = record[self.KEY_FILE_COLUMN]
        # check if record is already in database
        if not db.is_record_new(db_column_name, key_value):
            return False
        # record is new
        db_record = {}
        grouped_fields = {}
        for field in record.keys():
            if field not in mapping_lookup:
                # this is handled in the column check so skip missing columns here
                continue
            mapping = mapping_lookup[field]
            if mapping.grouping is not None:
                db_field = mapping.db_column_name
                if db_field not in grouped_fields:
                    grouped_fields[db_field] = {}
                size_label = mapping.file_column_name
                # the concentration data has an extra '_' which we don't need here
                size_label = size_label.replace('_', '')
                grouped_fields[db_field][size_label] = record[field]
            else:
                db_field = mapping.db_column_name
                db_record[db_field] = record[field]
        # convert the grouped fields to json
        for (db_field, field_dict) in grouped_fields.items():
            field_json = json.dumps(field_dict)
            db_record[db_field] = field_json
        db.save_measurement(db_record)
        return True
