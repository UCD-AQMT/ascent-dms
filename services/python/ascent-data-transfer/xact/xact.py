"""
handles Xact
"""

import configparser
import logging
import pathlib
import traceback
from common.local_database import LocalDatabase
from common.instrument import Instrument
from common.remote_database import RemoteDatabase
from common.synch_record import SynchRecord
from xact.xact_datafile import XactDataFile
from xact.database import XactDatabase
from custom_logger import setup_logger

LOGS_DIRECTORY = "logs"
# logger = setup_logger(LOGS_DIRECTORY, self.local_db)


class Xact(Instrument):
    ''' Xact Class definition'''
    def __init__(self, config: configparser.ConfigParser, logger: logging.Logger):
        Instrument.__init__(self, 'xact', config, logger)
        self.leave_latest_file = False

    def process_file(self, file: pathlib.Path, localdb: LocalDatabase) -> bool:
        '''Open, parse, and ingest Xact file'''
        print(f'Processing file "{file}"')
        db = XactDatabase(localdb)
        mappings = db.get_column_mapping('xact', 'sample_analysis')
        try:
            data_file = XactDataFile(file, mappings)
            data_file.parse()
            if data_file.save_data(db):
                if data_file.new_record_count > 0:
                    msg = f'Xact: Processing file "{file}":\n {data_file.import_notes}'
                    self.logger.info(msg)
                return True
        except Exception:
            msg = 'Xact: Exception while processing "' + str(file) + '":\n'
            self.logger.exception(msg)
            traceback.print_exc()
        return False
    def upload_data(self, synch_record: SynchRecord, localdb: LocalDatabase, remotedb: RemoteDatabase) -> bool:
        ''' takes a synch record and updates cloud '''
        db = XactDatabase(localdb)
        if synch_record.table_name == db.XACT_SETTINGS_TABLE:
            return self._sync_settings(synch_record, db, remotedb)
        if synch_record.table_name == db.XACT_DATA_TABLE:
            return self._sync_measurement(synch_record, db, remotedb)
        msg = f'Xact: Failed to recognize table "{synch_record.table_name}"'
        self.logger.error(msg)

    def _sync_settings(self, synch_record: SynchRecord, localdb: XactDatabase, remotedb: RemoteDatabase) -> bool:
        ''' synchs instrument settings '''
        record = localdb.get_setting(synch_record.record_id)
        if record is None:
            return False
        exists = remotedb.check_xact_setting(synch_record.record_id)
        if exists and synch_record.update_type == 'insert':
            msg = f'Xact: Record "{synch_record.record_id}" already added to "{synch_record.table_name}".  Removing synch record.'
            # remove extra synch_record
            localdb.dbc.clear_synch_record(synch_record)
            raise RuntimeError(msg)
        if synch_record.update_type == 'insert':
            remotedb.save_xact_setting(record)
            # confirm that the save worked before removing synch record
            if not remotedb.check_xact_setting(synch_record.record_id):
                msg = f'Xact: Synch failed for {synch_record.table_name} {synch_record.record_id}'
                self.logger.warning(msg)
                return False
            return True
        if synch_record.update_type == 'update':
            if not exists:
                msg = f'Xact: Update requested but missing remote record {synch_record.table_name} {synch_record.record_id}'
                self.logger.warning(msg)
                remotedb.save_xact_setting(record)
            else:
                return remotedb.update_xact_setting(record)
        return False

    def _sync_measurement(self, synch_record: SynchRecord, localdb: XactDatabase, remotedb: RemoteDatabase) -> bool:
        ''' syncs measurement record '''
        parent_record = localdb.get_measurement(synch_record.record_id)
        if parent_record is None:
            return False
        exists = remotedb.check_xact_measurement(synch_record.record_id)
        if exists and synch_record.update_type == 'insert':
            msg = f'Record "{synch_record.record_id}" already added to "{synch_record.table_name}".  Removing synch record.'
            # remove extra synch_record
            localdb.dbc.clear_synch_record(synch_record)
            raise RuntimeError(msg)
        if synch_record.update_type == 'insert':
            child_records = localdb.get_child_records(parent_record['id'])
            remotedb.save_xact_measurement(parent_record, child_records)
            # confirm that the save worked before removing synch record
            if not remotedb.check_xact_measurement(synch_record.record_id):
                msg = f'XACT: Synch failed for {synch_record.table_name} {synch_record.record_id}'
                self.logger.warning(msg)
                return False
            return True
        if synch_record.update_type == 'update':
            raise NotImplementedError()
        return False
