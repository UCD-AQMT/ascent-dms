# logger_config.py
import logging
import os
import time
from datetime import datetime
from common.database import DatabaseConnectionPg


class PostgreSQLHandler(logging.Handler):
    """ sql connection """
    def __init__(self, connection: DatabaseConnectionPg):
        super().__init__()
        self.database_connection = connection
        self.conn = self.database_connection.connect()
        self.cursor = self.conn.cursor()

    def emit(self, record: logging.LogRecord):
        try:
            conn = self.database_connection.connect()
            cursor = conn.cursor()
            cursor.execute(
                """
                    INSERT INTO common.app_logs (log_time, log_level, log_message, module)
                    VALUES (%s, %s, %s, %s);
                """,
                (
                    datetime.utcnow(),
                    record.levelname,
                    record.getMessage(),
                    record.module,
                ),
            )
            conn.commit()
            cursor.close()
            conn.close()
        except Exception as e:
            self.handleError(record)


class DailyRotatingFileHandler(logging.FileHandler):
    def __init__(self, logs_directory):
        self.logs_directory = logs_directory
        self.current_utc_date = datetime.utcnow().strftime("%Y-%m-%d")
        log_file_path = self.get_daily_log_file_path()
        super().__init__(log_file_path)

    def get_daily_log_file_path(self) -> str:
        log_file_name = f"{self.current_utc_date}.txt"
        log_file_path = os.path.join(self.logs_directory, log_file_name)
        return log_file_path

    def emit(self, record):
        current_date = datetime.utcnow().strftime("%Y-%m-%d")
        if current_date != self.current_utc_date:
            self.current_utc_date = current_date
            self.close()
            self.baseFilename = self.get_daily_log_file_path()
            self.stream = open(self.baseFilename, "a")
        super().emit(record)


def setup_logger(
    logs_directory: str, database_connection: DatabaseConnectionPg
) -> logging.Logger:  # , __name__) -> logging.Logger:
    """Creates and returns logger"""
    logger = logging.getLogger()  # __name__)
    # disable propagation to parent loggers so that we can have
    # distinct handlers for different logging destinations
    # such as command line, log file, and database
    logger.propagate = False

    logging_level = logging.DEBUG  # to do: make this configurable
    logger.setLevel(logging_level)  # to do: make this configurable
    time_format = "%Y-%m-%d %H:%M:%S %Z"
    log_format = f"%(asctime)s - %(name)s - %(levelname)s - %(message)s"
    simple_format = "%(levelname)s: %(message)s"  # Cleaner console format
    formatter = logging.Formatter(log_format, time_format)
    formatter.converter = time.gmtime  # Use UTC time

    # File handler
    file_handler = DailyRotatingFileHandler(logs_directory)
    file_handler.setLevel(logging_level)  # to do: make this configurable
    file_handler.setFormatter(formatter)  # to do: make this configurable
    logger.addHandler(file_handler)

    # PostgreSQL handler
    db_handler = PostgreSQLHandler(database_connection)
    db_handler.setLevel(logging_level)  # to do: make this configurable
    db_handler.setFormatter(formatter)  # to do: make this configurable
    logger.addHandler(db_handler)

    # Create a StreamHandler to send log messages to the console
    console_formatter = logging.Formatter(simple_format)
    console_handler = logging.StreamHandler()
    console_handler.setLevel(logging.INFO)  # Only show important messages in the console
    console_handler.setFormatter(console_formatter)
    logger.addHandler(console_handler)
    # stream_handler = logging.StreamHandler()
    # stream_handler.setLevel(logging_level)  # to do: make this configurable
    # stream_handler.setFormatter(formatter)
    # logger.addHandler(stream_handler)

    return logger
