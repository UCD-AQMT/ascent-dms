'''
handles ACSM
'''

import configparser
import logging
import pathlib
import traceback
from common.instrument import Instrument
from common.local_database import LocalDatabase
from common.remote_database import RemoteDatabase
from common.synch_record import SynchRecord
from acsm.acsm_datafile import AcsmDataFile
from acsm.database import AcsmDatabase

ACSM_DB_TABLES = {'sample_analysis', 'mass_loadings', 'diag_calib', 'tps'}

class Acsm(Instrument):
    
    ACSM_DB_TABLES = {'sample_analysis', 'mass_loadings', 'diag_calib', 'tps'}
    ''' acsm data processing '''
    def __init__(self, config: configparser.ConfigParser, logger: logging.Logger):
        Instrument.__init__(self, 'acsm', config, logger)
        self.leave_latest_file = True
    
    @staticmethod
    def only_native_data_files(file: pathlib.Path):
        return file.name.endswith("nativeData.txt")

    def check_and_process_new_data(self, localdb: LocalDatabase) -> bool:
        ''' checks for new files and imports into database '''
        new_data = False
        try:
            self.file_watcher.check_and_move_new_data(self.leave_latest_file, filter_func=self.only_native_data_files)
            # process everything in the processing folder
            data_files = self.file_watcher.get_files_for_processing()
            #only ingest nativeData files (as there are also hourly and daily .txt files)
            data_files = sorted(f for f in data_files if f.name.endswith("nativeData.txt"))
            self.logger.debug(f"Files in processing: {data_files}, Type: {type(data_files)}")
            for data_file in data_files:
                if not self.process_file(data_file, localdb):
                    continue # failed to process file so leave it
                self.file_watcher.move_file_to_archive(data_file, self.leave_latest_file)
                new_data = True
        except Exception:
            msg = 'Exception while processing ' + self.instrument_type + ' files :: '
            self.logger.exception(msg)
            traceback.print_exc()
        return new_data

    def process_file(self, file: pathlib.Path, localdb: LocalDatabase) -> bool:
        """Processes an ACSM data file and saves records to the database."""
        self.logger.info(f'Processing file "{file}"')
        db = AcsmDatabase(localdb)
        mappings = []
        for table in ACSM_DB_TABLES:
            mappings += db.get_column_mapping('acsm', table)
        try:
            data_file = AcsmDataFile(file, mappings)
            data_file.parse()
            if data_file.save_data(db):
                if data_file.new_record_count > 0:
                    self.logger.info(f'ACSM: Processing file "{file}":\n {data_file.import_notes}')
                return True
            else:
                self.logger.warning(f'ACSM: No new records to import from file "{file}".')
        except Exception:
            self.logger.exception(f'ACSM: Exception while processing file "{file}".')
            traceback.print_exc()
        return False

    def upload_data(self, synch_record: SynchRecord, localdb: LocalDatabase, remotedb: RemoteDatabase) -> bool:
        ''' takes a synch record and updates cloud '''
        db = AcsmDatabase(localdb)
        if synch_record.table_name == db.ACSM_DATA_TABLE:
            return self._sync_measurement(synch_record, db, remotedb)
        msg = f'ACSM: Failed to recognized table "{synch_record.table_name}"'
        self.logger.error(msg)

    def _sync_settings(self, synch_record: SynchRecord, localdb: AcsmDatabase, remotedb: RemoteDatabase) -> bool:
        ''' synchs instrument settings '''
        record = localdb.get_setting(synch_record.record_id)
        if record is None:
            return False
        exists = remotedb.check_acsm_setting(synch_record.record_id)
        if exists and synch_record.update_type == 'insert':
            msg = f'SMPS: Record "{synch_record.record_id}" already added to "{synch_record.table_name}".  Removing synch record.'
            # remove extra synch_record
            localdb.dbc.clear_synch_record(synch_record)
            raise RuntimeError(msg)
        if synch_record.update_type == 'insert':
            remotedb.save_acsm_setting(record)
            # confirm that the save worked before removing synch record
            if not remotedb.check_acsm_setting(synch_record.record_id):
                msg = f'ACSM: Synch failed for {synch_record.table_name} {synch_record.record_id}'
                self.logger.warning(msg)
                return False
            return True
        if synch_record.update_type == 'update':
            if not exists:
                msg = f'ACSM: Update requested but missing remote record {synch_record.table_name} {synch_record.record_id}'
                self.logger.warning(msg)
                remotedb.save_acsm_setting(record)
            else:
                return remotedb.update_acsm_setting(record)
        return False

    def _sync_measurement(self, synch_record: SynchRecord, localdb: AcsmDatabase, remotedb: RemoteDatabase) -> bool:
        ''' synchs measurement record '''
        # assuming this is parent record
        record = localdb.get_measurement(synch_record.record_id)
        if record is None:
            return False
        exists = remotedb.check_acsm_measurement(synch_record.record_id)
        parent_id = synch_record.record_id
        if exists and synch_record.update_type == 'insert':
            msg = f'Record "{synch_record.record_id}" already added to "{synch_record.table_name}".  Removing synch record.'
            # remove extra synch_record
            localdb.dbc.clear_synch_record(synch_record)
            raise RuntimeError(msg)
        if synch_record.update_type == 'insert':
            # get its children that need to be inserted also
            children_records = []
            children_records = localdb.get_children_records(parent_id)
            remotedb.save_acsm_measurement(record, children_records)
            # confirm that the save worked before removing synch record
            if not remotedb.check_acsm_measurement(synch_record.record_id):
                msg = f'ACSM: Synch failed for {synch_record.table_name} {synch_record.record_id}'
                self.logger.warning(msg)
                return False
            return True
        if synch_record.update_type == 'update':
            raise NotImplementedError()
        return False