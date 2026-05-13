'''
parses the xact data file
'''

import pathlib
from datetime import datetime, timezone
import pandas as pd
from common.column_mapping import ColumnMapping
from common.helpers import remove_text_between_parens
from xact.database import XactDatabase
from xact.instrument_settings import InstrumentSettings


class XactDataFile():
    ''' data file '''
    def __init__(self, file: pathlib.Path, measurement_mapping: list[ColumnMapping]):
        self.column_names = []
        self.filename = file
        self.settings = InstrumentSettings()
        self.sample_datetime = ''
        self.column_mappings = measurement_mapping
        self.record = pd.DataFrame()
        self.uncertainity_vars = []
        self.environmental_vars = []
        self.env_par_vars = []
        self.element_vars = []
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
        ''' Parse the datafile into a dataframe'''
        self.record = pd.read_csv(self.filename, sep=",", skiprows=1, index_col=False)
        # convert mapping list to dictionary for quick lookup
        mapping_lookup = {}
        for mapping in self.column_mappings:
            mapping_lookup[mapping.file_column_name] = mapping.db_column_name
        # Rename the columns based on the mapping dictionary
        self.record.rename(columns=mapping_lookup, inplace=True)
        # remove left-side whitspaces from column names
        self.record.columns = self.record.columns.str.lstrip(" ")
        # remove units displayed inside the parantheses
        self.record.columns = [remove_text_between_parens(col) for col in self.record.columns]
        # remove right-side whitspaces from column names
        self.record.columns = self.record.columns.str.rstrip(" ")
        # replace whitespace with underscore
        self.record.columns = self.record.columns.str.replace(" ", "_")
        # convert to datatypes matching column schema
        self.record.dropna(axis=1, inplace=True)

        # datetime object containing current date and time in UTC
        now = datetime.now(timezone.utc).replace(microsecond=0)
        self.record.insert(1, "date_imported", now)

        self.sample_datetime = self.record["sample_datetime"][0]
        
        self.record["sample_datetime"] = pd.to_datetime(self.record["sample_datetime"])
        self.record["pump_start_time"] = pd.to_datetime(self.record["pump_start_time"])

        # columns organized by category to complete dataframe massaging for ingest
        self.column_names = self.record.columns.tolist()

        self.uncertainity_vars = [col for col in self.record.columns if "Uncert" in col]

        for mapping in self.column_mappings:
            if mapping.grouping == 'environmental_vars':
                self.environmental_vars.append(mapping.db_column_name)

        # these should be separated out into a single ID column, and separated table
        self.env_par_vars = [
            x for x in self.column_names if x not in self.uncertainity_vars
        ]  # sample_datetime + environmental variables + elements list
        self.element_vars = [
            x for x in self.env_par_vars if x not in self.environmental_vars
        ]  # sample_datetime + elements list

        instrument_settings = {}
        for mapping in self.column_mappings:
            if mapping.grouping == "instrument_settings":
                instrument_settings[mapping.db_column_name] = self.record[mapping.db_column_name][0]
        print(instrument_settings)
        self.settings.load_settings(instrument_settings)


    def save_data(self, db: XactDatabase) -> bool:
        '''Save data (both parent and child records), returns True if successful'''
        db.start_transaction()
        # update settings
        self.settings.update_database(self.sample_datetime, db)
        db.end_transaction()
        
        df_sample_analysis = self.record[self.environmental_vars]

        dat_import_col = df_sample_analysis.pop("date_imported")
        df_sample_analysis.insert(1, "date_imported", dat_import_col)

        # convert df into a tuple for use with CopyManager
        parent_record = df_sample_analysis
        try:
            # to be replaced by import from config file
            rm_table = "xact.raw_measurements"

            date_imported = self.record["date_imported"]
            date_imported = "".join(date_imported[0].strftime("%Y-%m-%d %H:%M:%S"))
            sample_datetime = self.record["sample_datetime"]
            sample_datetime = "".join(sample_datetime[0].strftime("%Y-%m-%d %H:%M:%S"))

            # pivot the uncertainities of elements
            df_unc = pd.melt(
                self.record,
                id_vars=(["date_imported"]),
                var_name="element",
                value_name="uncertainty",
                value_vars=self.uncertainity_vars,
            )

            # remove the underscores (should have probably been done in parsing function)
            df_unc["element"] = df_unc["element"].str.split("_").str[:-1].str.join("_")

            # pivot the actual values of elements
            df_par = pd.melt(
                self.record,
                id_vars=(["date_imported"]),
                var_name="element",
                value_name="value",
                value_vars=self.element_vars,
            )

            # remove the underscores (should have probably been done in parsing function)
            df_par["element"] = df_par["element"].str.split("_").str[:-1].str.join("_")

            # join the value and uncertainity dataframes by 'element' column
            df_raw_measurement = pd.concat(
                [df_unc.set_index("element"), df_par.set_index("element")],
                axis=1,
                join="inner",
            ).reset_index()

            # move element, value, uncertainity to the beginning columns of the table
            par_col = df_raw_measurement.pop("element")
            val_col = df_raw_measurement.pop("value")
            unc_col = df_raw_measurement.pop("uncertainty")
            pos = 1  # this is the postition after 'sample_datetime'
            df_raw_measurement.insert(pos, "element", par_col)
            df_raw_measurement.insert(pos + 1, "value", val_col)
            df_raw_measurement.insert(pos + 2, "uncertainty", unc_col)
            df_raw_measurement.pop("date_imported")

            child_records = df_raw_measurement

            db.save_record_with_child(parent_record, db.XACT_DATA_TABLE, child_records, rm_table)
        except Exception as e:
            print(f"Error in post_db_raw function: {e}")

        db.end_transaction()
        return True
   