import re
import configparser
from datetime import datetime, timedelta

config_file = "config.ini"


def remove_text_between_parens(text):
    # https://stackoverflow.com/questions/14596884/remove-text-between-and-in-python
    n = 1  # run at least once
    while n:
        text, n = re.subn(
            r"\([^()]*\)", "", text
        )  # remove non-nested/flat balanced parts
    return text


def get_site_number():
    config_data = configparser.ConfigParser()
    config_data.read(config_file)
    site_number = config_data["site.info"]["SiteNumber"]
    return int(site_number)

def year_doy_to_timestamp(year, doy):
    """
    Converts a year and Day of Year (DOY) into a timestamp.

    Args:
        year (int): The year (e.g., 2025).
        doy (float): The Day of Year (e.g., 134.5).

    Returns:
        datetime: A Python datetime object representing the timestamp.
    """
    year = int(year)
    doy_int = int(doy)  # Get the integer part of DOY
    fractional_day = doy - doy_int  # Get the fractional part of DOY
    try:
        base_date = datetime.strptime(f"{year}{doy_int:03d}", "%Y%j")
    except ValueError as e:
        raise ValueError(f"Invalid DOY: {doy}. Ensure DOY is a valid day of year.") from e
    return base_date + timedelta(seconds=fractional_day * 86400)

def add_start_end_timestamps(df, year_col, start_doy_col, stop_doy_col):
    """
    Adds two new columns, 'start_date' and 'stop_date', to the DataFrame
    by converting year and DOY to timestamps.

    Args:
        df (pd.DataFrame): The input DataFrame.
        year_col (str): The column name for the year (e.g., 'Year').
        start_doy_col (str): The column name for the start DOY (e.g., 'start_doy').
        stop_doy_col (str): The column name for the stop DOY (e.g., 'stop_doy').

    Returns:
        pd.DataFrame: The DataFrame with new 'start_date' and 'stop_date' columns.
    """
    df['start_date'] = df.apply(lambda row: year_doy_to_timestamp(row[year_col], row[start_doy_col]), axis=1)
    df['stop_date'] = df.apply(lambda row: year_doy_to_timestamp(row[year_col], row[stop_doy_col]), axis=1)
    return df
