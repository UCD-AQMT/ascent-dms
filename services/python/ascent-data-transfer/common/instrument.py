'''
Common class for keeping track of instrument information
'''

import configparser
import logging
import pathlib
import traceback
from common.fileio import FileWatcher
from common.local_database import LocalDatabase
from common.remote_database import RemoteDatabase
from common.synch_record import SynchRecord

class Instrument():
    ''' Instrument base class '''
    def __init__(self, instrument_type: str, config: configparser.ConfigParser, logger: logging.Logger):
        logger.debug(f'Initializing {instrument_type} settings')
        self.instrument_type = instrument_type
        self.config = config[instrument_type]
        self.file_watcher = FileWatcher(instrument_type, config)
        self.logger = logger
        self.leave_latest_file = False

    @property
    def enabled(self):
        ''' from the config file '''
        value = self.config.getboolean('Enabled')
        return value

    def check_settings(self):
        ''' make sure everything is setup correctly '''
        self.file_watcher.check_directories()

    def check_and_process_new_data(self, localdb: LocalDatabase) -> bool:
        ''' checks for new files and imports into database '''
        new_data = False
        try:
            self.file_watcher.check_and_move_new_data(self.leave_latest_file)
            # process everything in the processing folder
            data_files = self.file_watcher.get_files_for_processing()
            print('Files in processing:', data_files, type(data_files))
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
        '''  needs to be overridden '''
        raise NotImplementedError()

    def upload_data(self, synch_record: SynchRecord, localdb: LocalDatabase, remotedb: RemoteDatabase) -> bool:
        ''' needs to be overridden '''
        raise NotImplementedError()
    