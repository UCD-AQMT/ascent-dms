'''
Get versions for different required software
'''

import sys
import pandas as pd
import psycopg2 as pg

def print_python_version():
    ''' gets python version '''
    print("python version:", sys.version)

def print_pandas_version():
    ''' gets pandas version '''
    print("pandas version:", pd.__version__)

def print_psycopg2_version():
    '''  gets psycopg2 version '''
    print("psycopg2 version:", pg.__version__)

def print_versions():
    ''' prints psycopg2 version '''
    print_python_version()
    print_pandas_version()
    print_psycopg2_version()


if __name__ == "__main__":
    print_versions()