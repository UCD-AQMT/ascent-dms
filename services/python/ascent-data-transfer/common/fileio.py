'''
Collection of functions to deal with file system
'''

import os
import pathlib
import configparser
import shutil
import logging
import win32file

logger = logging.getLogger(__name__)  # Module-level logger

def create_path(path: str):
    ''' creates path if it doesn't already exist '''
    full_path = pathlib.Path(path)
    if full_path.exists():
        print(f'Path "{path}" already exists')
        return
    # check if parent path exists
    parent = full_path.parent
    if parent == full_path:
        # we are already at the drive level so abort
        raise Exception(f'Does the drive "{parent}" exist?')
    if not parent.exists:
        # parent directory needs to be created first
        create_path(str(parent))
    print(f'Creating directory "{full_path}"')
    full_path.mkdir()


class FileWatcher():
    ''' handles file actions  '''
    PROCESSING_BASE_PATH_KEY = 'ProcessingBasePath'
    PROCESSING_FINISHED_BASE_PATH_KEY = 'ProcessingFinishedBasePath'
    INSTRUMENT_DATA_FOLDER_KEY = 'InstrumentDataFolder'
    ARCHIVE_DATA_FOLDER_KEY = 'ArchiveDataFolder'
    FILE_EXTENSION_KEY = 'DataFileExtension'

    ''' watch for new files and move files around '''
    def __init__(self, config_section_key: str, config: configparser.ConfigParser):
        self.name = config_section_key
        file_section = config['file.storage']
        self.processing_path = FileWatcher._build_path(file_section[self.PROCESSING_BASE_PATH_KEY], self.name)
        self.local_archive_path = FileWatcher._build_path(file_section[self.PROCESSING_FINISHED_BASE_PATH_KEY], self.name)
        instrument_section = config[config_section_key]
        self.data_source = pathlib.Path(instrument_section[self.INSTRUMENT_DATA_FOLDER_KEY])
        # needs to be optional
        if self.ARCHIVE_DATA_FOLDER_KEY in instrument_section:
            self.external_archive_path = pathlib.Path(instrument_section[self.ARCHIVE_DATA_FOLDER_KEY])
        else:
            self.external_archive_path = None
        if self.FILE_EXTENSION_KEY in instrument_section:
            self.file_extension = instrument_section[self.FILE_EXTENSION_KEY]
        else:
            self.file_extension = '*.*'

    def _build_path(base_path, instrument_path_name) -> pathlib.Path:
        base = pathlib.Path(base_path)
        return base / instrument_path_name

    def check_directories(self):
        ''' checks that all the expected directories exist. '''
        # directories in the program folder, create if missing
        if not self.processing_path.exists():
            create_path(str(self.processing_path))
        if not self.local_archive_path.exists():
            create_path(str(self.local_archive_path))
        # directories not in the program folder, raise error if missing
        if not self.data_source.exists():
            raise Exception(f'Could not find "{self.name}" instrument data source folder "{self.data_source.resolve()}"')
        if self.external_archive_path and not self.external_archive_path.exists():
            raise Exception(f'Could not find "{self.name}" instrument archive data folder "{self.external_archive_path.resolve()}"')
        
    def check_and_move_new_data(self, leave_most_recent: bool, ignore_most_recent=False, filter_func=None,) -> bool:
        '''  checks if there any files in the data source folder.  Only copies one file per time called. '''
        files = sorted(self.data_source.glob(self.file_extension))
        
        if filter_func:
            files = [f for f in files if filter_func(f)]

        # no files found
        if not files:
            return False
        # want the oldest file first
        logger.info(f"Files found in source: {len(files)} file(s).")
        logger.debug(f"File list: {files}") 
        
        oldest_file = min(files, key=os.path.getctime)
        logger.info(f"Oldest file: {oldest_file}")

        # copy over to processing
        filename = oldest_file.name
        dest = self.processing_path / filename

        # check if we are on the last (i.e. newest) file
        if leave_most_recent and len(files) == 1:
            if ignore_most_recent:
                return True
            # copy file instead of moving it
            try:
                win32file.CopyFile(str(oldest_file), str(dest), False)
                logger.debug(f"Copied file {filename} to {str(dest)}")
            except Exception as e:
                logger.error(f"Could not copy {filename} to {str(dest)}: {e}")
                return False
        else:
            shutil.move(oldest_file, dest)
            logger.debug(f"Moved file {filename} to {dest}")
        return True
    
    def get_files_for_processing(self) -> list[pathlib.Path]:
        ''' return all files in processing folder '''
        files = self.processing_path.glob(self.file_extension)
        return files

    def move_file_to_archive(self, file: pathlib.Path, overwrite_allowed: bool = False):
        ''' moves file to archive '''
        filename = file.name
        # copy for the external archive (less likely to have issues)
        if self.external_archive_path:
            try:
                dest = self.external_archive_path / filename
                shutil.copy(file, dest)
                logger.debug(f'Copied data file {filename} to {self.external_archive_path}')
            except Exception as ex:
                logger.debug(f'Failed to copy file "{filename}" to external archive "{dest}"')
        # move the file for the local archive
        dest = self.local_archive_path / filename
        if overwrite_allowed and dest.exists():
            file.replace(dest)
        else:
            # will throw an exception if the file already exists
            file.rename(dest)
        logger.debug(f'Moved file {filename} to {self.local_archive_path}')

        