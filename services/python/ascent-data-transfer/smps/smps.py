'''
handles SMPS
'''

import configparser
import logging
import pathlib
import traceback
from common.instrument import Instrument
from common.local_database import LocalDatabase
from common.remote_database import RemoteDatabase
from common.synch_record import SynchRecord
from smps.smps_datafile import SmpsDataFile
from smps.database import SmpsDatabase

class Smps(Instrument):
    ''' smps data processing '''
    def __init__(self, config: configparser.ConfigParser, logger: logging.Logger):
        Instrument.__init__(self, 'smps', config, logger)
        self.leave_latest_file = True

    def process_file(self, file: pathlib.Path, localdb: LocalDatabase) -> bool:
        ''' do stuff '''
        print(f'Processing file "{file}"')
        db = SmpsDatabase(localdb)
        mappings = db.get_column_mapping('smps', 'sample_analysis')
        try:
            data_file = SmpsDataFile(file, mappings)
            data_file.parse()
            if data_file.save_data(db):
                if data_file.new_record_count > 0:
                    msg = f'SMPS: Processing file "{file}":\n {data_file.import_notes}'
                    self.logger.info(msg)
                return True
        except Exception:
            msg = 'SMPS: Exception while processing "' + str(file) + '":\n'
            self.logger.exception(msg)
            traceback.print_exc()
        return False

    def upload_data(self, synch_record: SynchRecord, localdb: LocalDatabase, remotedb: RemoteDatabase) -> bool:
        ''' takes a synch record and updates cloud '''
        db = SmpsDatabase(localdb)
        if synch_record.table_name == db.SMPS_SETTINGS_TABLE:
            return self._sync_settings(synch_record, db, remotedb)
        if synch_record.table_name == db.SMPS_DATA_TABLE:
            return self._sync_measurement(synch_record, db, remotedb)
        msg = f'SMPS: Failed to recognized table "{synch_record.table_name}"'
        self.logger.error(msg)

    def _sync_settings(self, synch_record: SynchRecord, localdb: SmpsDatabase, remotedb: RemoteDatabase) -> bool:
        ''' synchs instrument settings '''
        record = localdb.get_setting(synch_record.record_id)
        if record is None:
            return False
        exists = remotedb.check_smps_setting(synch_record.record_id)
        if exists and synch_record.update_type == 'insert':
            msg = f'SMPS: Record "{synch_record.record_id}" already added to "{synch_record.table_name}".  Removing synch record.'
            # remove extra synch_record
            localdb.dbc.clear_synch_record(synch_record)
            raise RuntimeError(msg)
        if synch_record.update_type == 'insert':
            remotedb.save_smps_setting(record)
            # confirm that the save worked before removing synch record
            if not remotedb.check_smps_setting(synch_record.record_id):
                msg = f'SMPS: Synch failed for {synch_record.table_name} {synch_record.record_id}'
                self.logger.warning(msg)
                return False
            return True
        if synch_record.update_type == 'update':
            if not exists:
                msg = f'SMPS: Update requested but missing remote record {synch_record.table_name} {synch_record.record_id}'
                self.logger.warning(msg)
                remotedb.save_smps_setting(record)
            else:
                return remotedb.update_smps_setting(record)
        return False

    def _sync_measurement(self, synch_record: SynchRecord, localdb: SmpsDatabase, remotedb: RemoteDatabase) -> bool:
        ''' synchs measurement record '''
        record = localdb.get_measurement(synch_record.record_id)
        if record is None:
            return False
        exists = remotedb.check_smps_measurement(synch_record.record_id)
        if exists and synch_record.update_type == 'insert':
            msg = f'Record "{synch_record.record_id}" already added to "{synch_record.table_name}".  Removing synch record.'
            # remove extra synch_record
            localdb.dbc.clear_synch_record(synch_record)
            raise RuntimeError(msg)
        if synch_record.update_type == 'insert':
            remotedb.save_smps_measurement(record)
            # confirm that the save worked before removing synch record
            if not remotedb.check_smps_measurement(synch_record.record_id):
                msg = f'SMPS: Synch failed for {synch_record.table_name} {synch_record.record_id}'
                self.logger.warning(msg)
                return False
            return True
        if synch_record.update_type == 'update':
            raise NotImplementedError()
        return False
