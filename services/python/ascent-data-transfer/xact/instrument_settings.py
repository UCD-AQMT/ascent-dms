'''
For keeping track of the instrument settings (only XC_VER column)
'''

from datetime import datetime
from xact.database import XactDatabase

class InstrumentSettings():
    ''' instrument settings '''
    def __init__(self):
        self.settings = {}

    def load_settings(self, settings: dict):
        ''' takes a settings dict '''
        self.settings = settings
        #pprint(settings)

    def version(self):
        ''' gets version number from file '''
        key = 'xc_ver'
        if key in self.settings:
            return self.settings[key]
        return None

    def update_database(self, first_record_date: datetime, db: XactDatabase):
        ''' compare settings in file to database and update database '''
        db_records = db.get_settings()
        unmatched_keys = list(self.settings.keys())
        for db_record in db_records:
            db_field = db_record['name']
            if db_field in unmatched_keys:
                # matched so remove
                unmatched_keys.remove(db_field)
                # if key matches, we need to check for changes
                self._compare_field(db_field, db_record, first_record_date, db)
            else:
                # db_field is missing so we need to end it
                self._end_field(db_record, first_record_date, db)
        for unmatched_key in unmatched_keys:
            self._new_field(unmatched_key, first_record_date, db)

    def _compare_field(self, field_name: str, db_record: dict, record_date: datetime, db: XactDatabase):
        ''' compares the file setting field to the db record and updates the db as necessary '''
        db_value = db_record['value']
        if db_value == self.settings[field_name]:
            # both the same so no changes
            return
        # value changed, updated database, since we want to keep track of changes
        #  we will update end date on the old record and add a new record for the
        #  updated value.
        self._end_field(db_record, record_date, db)
        self._new_field(field_name, record_date, db)

    def _end_field(self, db_record: dict, record_date: datetime, db: XactDatabase):
        ''' field not found in file but found in database '''
        record_id = db_record['id']
        db.end_settings_record(record_id, record_date)

    def _new_field(self, field_name: str, record_date: datetime, db: XactDatabase):
        ''' field that was in file but not in database '''
        value = self.settings[field_name]
        db.add_settings_record(field_name, value, record_date)