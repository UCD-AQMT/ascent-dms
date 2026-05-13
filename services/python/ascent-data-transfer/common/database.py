'''
Collection of database related helpers
'''

import configparser
import psycopg2
import psycopg2.extensions
from psycopg2.extras import RealDictCursor
from common.column_mapping import ColumnMapping
from common.synch_record import SynchRecord


class DatabaseConnectionParameters():
    ''' keeps connection info and common db actions '''

    def __init__(self, database_config: configparser.SectionProxy, credentials_section: configparser.SectionProxy):
        self.load_config(database_config)
        self.load_credentials(credentials_section)

    def load_config(self, database_config: configparser.SectionProxy):
        ''' reads config file '''
        self.port = database_config['Port']
        self.host = database_config['Host']
        self.database_name = database_config['DatabaseName']

    def load_credentials(self, credentials_section: configparser.SectionProxy):
        ''' reads credentials config file '''
        self.username = credentials_section['User']
        self.password = credentials_section['Password']

    def to_dict(self):
        ''' used for passing to psycopg '''
        credentials = {
            "dbname": self.database_name,
            "user": self.username,
            "password": self.password,
            "host": self.host,
            "port": self.port,
        }
        return credentials


class DatabaseConnectionPg():
    ''' Connection for database '''

    # table names
    SITES_TABLE = 'common.sites'

    def __init__(self, connection_parameters: DatabaseConnectionParameters):
        self.connection_parameters = connection_parameters.to_dict()
        self.version = 'not set'
        self.is_reachable = False

    def connect(self) -> psycopg2.extensions.connection:
        ''' creates a connection object '''
        return psycopg2.connect(**self.connection_parameters)

    def test_connection(self) -> bool:
        ''' tests that the connection is good by checking if the sites table exists '''
        try:
            conn = self.connect()
            cursor = conn.cursor()
            sql_query = f'SELECT EXISTS (SELECT * FROM {self.SITES_TABLE})'
            cursor.execute(sql_query)
            exists = cursor.fetchone()[0]
            cursor.close()
            conn.close()
            self.is_reachable = exists
            return exists
        except psycopg2.OperationalError as ex:
            print(ex)
            return False

    def load_database_version(self) -> str:
        ''' grabs the database version setting, should be in the format yyyy.nn '''
        conn = self.connect()
        cursor = conn.cursor()
        sql_query = "SELECT value FROM common.settings WHERE name = 'db_version'"
        cursor.execute(sql_query)
        self.version = cursor.fetchone()[0]
        cursor.close()
        conn.close()
        return self.version