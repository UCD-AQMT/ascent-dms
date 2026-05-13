'''
Program entry point
'''

import configparser
import traceback
from datetime import datetime, timedelta
from time import sleep
from psycopg2 import OperationalError
from versions import print_versions

# library
from common.database import DatabaseConnectionParameters
from common.local_database import LocalDatabase
from common.remote_database import RemoteDatabase
from common.fileio import create_path
from common.synch_record import SynchRecord
from custom_logger import setup_logger

# instruments
from acsm.acsm import Acsm
from xact.xact import Xact
from smps.smps import Smps
from dryerstats.dryerstats import Dryerstats

EXPECTED_DB_VERSION = "2025.01"
LOGS_DIRECTORY = "logs"


class Main:
    """ setup and main loop """

    def __init__(self, config_file: str, credentials_file: str):
        self.config_file_path = config_file
        self.config = configparser.ConfigParser(interpolation=None)
        self.config.read(config_file)
        self.create_base_folders()
        (self.site_number, self.site_code) = self.load_site_info()
        # database
        self.local_db = None
        self.cloud_db = None
        self.setup_database_connections(credentials_file)
        # logger (uses local db connection and logs folder)
        self.logger = setup_logger(LOGS_DIRECTORY, self.local_db)
        self.logger.info("Test entry")
        # instruments - need to do this last
        self.setup_instruments()

    def create_base_folders(self):
        """ makes sure the main folders are created """
        key = "file.storage"
        section = self.config[key]
        create_path(section["ProcessingBasePath"])
        create_path(section["ProcessingFinishedBasePath"])
        create_path(section["LogsPath"])

    def load_site_info(self):
        """ gets site info from config """
        key = "site.info"
        section = self.config[key]
        site_number = int(section["SiteNumber"])
        site_code = section["SiteCode"]
        return (site_number, site_code)


    def setup_local_database_connection(self, credentials: configparser.ConfigParser):
        """ test and check version for local database """
        print("Loading local db settings...")
        key = "database.local"
        local_db_settings = DatabaseConnectionParameters(self.config[key], credentials[key])
        self.local_db = LocalDatabase(local_db_settings)
        if not self.local_db.test_connection():
            raise RuntimeError("Failed to connect to local database, aborting.")
        version = self.local_db.load_database_version()
        print("local db version:", version)
        if version not in EXPECTED_DB_VERSION:
            raise RuntimeError(f'Reported db version "{version}" did not match expected version "{EXPECTED_DB_VERSION}"')

    def setup_remote_database_connection(self, credentials: configparser.ConfigParser):
        """ setup and test connection to remote database """
        print("Loading remote db settings...")
        key = "database.cloud"
        cloud_db_settings = DatabaseConnectionParameters(
            self.config[key], credentials[key]
        )
        self.cloud_db = RemoteDatabase(cloud_db_settings)
        self.cloud_db.set_site(self.site_number, self.site_code)
        if not self.cloud_db.test_connection():
            print('WARNING: Failed to connect to remote/cloud database. Only ingesting files to local db.')
        else:
            self.cloud_db.validate_site()

    def setup_database_connections(self, credentials_file: str):
        """check that db credentials exist and test connection to database"""
        # pull user/pass from credentials file (formatted as config file)
        # name should match section in main config file
        credentials = configparser.ConfigParser(interpolation=None)
        credentials.read(credentials_file)
        self.setup_local_database_connection(credentials)
        self.setup_remote_database_connection(credentials)

    def setup_instruments(self):
        """initialize each instrument and add it to the queue."""
        self.instruments = []
        # acsm
        acsm = Acsm(self.config, self.logger)
        if acsm.enabled:
            acsm.check_settings()
            self.instruments.append(acsm)
        # smps
        smps = Smps(self.config, self.logger)
        if smps.enabled:
            smps.check_settings()
            self.instruments.append(smps)
        # xact
        xact = Xact(self.config, self.logger)
        if xact.enabled:
            xact.check_settings()
            self.instruments.append(xact)
        # dryerstats
        dryerstats = Dryerstats(self.config, self.logger)
        if dryerstats.enabled:
            dryerstats.check_settings()
            self.instruments.append(dryerstats)

    def next_interval_start_time(self, now: datetime) -> datetime:
        """figure out when the next loop time, also handles startup and picking the first time"""
        interval = timedelta(seconds=60)
        # want to be at 30 seconds after the minute to try and avoid conflicts
        target_time = datetime(now.year, now.month, now.day, now.hour, now.minute, 30)
        return target_time + interval

    def _synch_log_record(self, synch_record: SynchRecord):
        """ handles synching log records """
        try:
            record = self.local_db.get_log_record(synch_record.record_id)
            if record is None:
                return
            exists = self.cloud_db.check_app_log(synch_record.record_id)
            if exists and synch_record.update_type == 'insert':
                msg = f'Record "{synch_record.record_id}" already added to "{synch_record.table_name}".  Removing synch record.'
                # remove extra synch_record
                self.local_db.clear_synch_record(synch_record)
                raise RuntimeError(msg)
            if synch_record.update_type == 'insert':
                self.cloud_db.save_app_log(record)
                # confirm that the save worked before removing synch record
                if not self.cloud_db.check_app_log(synch_record.record_id):
                    msg = f'Synch failed for {synch_record.table_name} {synch_record.record_id}'
                    return
                self.local_db.clear_synch_record(synch_record)
            if synch_record.update_type == 'update':
                msg = 'Sql update not supported for app_logs: ' + str(synch_record)
                self.logger.error(msg)
                self.local_db.clear_synch_record(synch_record)
        except (OperationalError, RuntimeError):
            msg = 'Exception while synching "' + str(synch_record) + '" :: '
            self.logger.exception(msg)
            traceback.print_exc()

    def _synch_instrument_record(self, instrument, record: SynchRecord):
        """ passes off handling to instrument """
        try:
            if instrument.upload_data(record, self.local_db, self.cloud_db):
                # success, remove synch record
                self.local_db.clear_synch_record(record)
        except (OperationalError, RuntimeError):
            msg = 'Exception while synching "' + str(record) + '" :: '
            self.logger.exception(msg)
            traceback.print_exc()

    def process_synch_records(self):
        """ gets synch records and passes them on to the instruments """
        # make a lookup table rather than searching through the list every time
        instruments = {x.instrument_type: x for x in self.instruments}
        # get synch records for processing
        records = self.local_db.get_next_synch()
        print(f'Synching {len(records)} records:')
        for record in records:
            self.logger.info(f'Processing synch record {record}')
            print('.', end='')
            if record.table_name == self.local_db.APP_LOGS_TABLE:
                self._synch_log_record(record)
                continue
            elif record.instrument not in instruments:
                print('Missing enabled instrument for synch record:', record.instrument)
                continue
            instrument = instruments[record.instrument]
            try:
                if instrument.upload_data(record, self.local_db, self.cloud_db):
                    # success, remove synch record
                    self.local_db.clear_synch_record(record)
            except (OperationalError, RuntimeError):
                msg = 'Exception while synching "' + str(record) + '" :: '
                self.logger.exception(msg)
                traceback.print_exc()

    def loop(self):
        """main program loop"""
        interval_delay = False
        for instrument in self.instruments:
            self.logger.info(f"Starting monitoring for {instrument.instrument_type} data")
        print("------------------")
        # check for files once a minute
        while True:
            start = datetime.now()
            for instrument in self.instruments:
                instrument.check_and_process_new_data(self.local_db)
            if self.local_db.is_pending_synch():
                self.process_synch_records()            
            if interval_delay is True:
                start = datetime.now()
                now = datetime.now()
                target_time = self.next_interval_start_time(now)
                remaining = target_time - now
                # using logger.info causes an error
                print(
                    "start:", start,
                    "now:", now,
                    "remaining:", remaining,
                    "target:", target_time,
                )
                sleep(remaining.total_seconds())

    def start(self):
        ''' entry point '''
        if not self.instruments:
            self.logger.info("No instruments are enabled, exiting")
            return
        self.loop()


if __name__ == "__main__":
    print_versions()
    print('-----------------')
    main = Main("config.ini", "db_credentials/credentials.txt")
    main.start()
    print("Done.")
