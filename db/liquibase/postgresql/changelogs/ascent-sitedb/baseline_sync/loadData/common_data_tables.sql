-- Load data for common.data_tables table
INSERT INTO common.data_tables (table_name, instrument)
VALUES
	('xact.sample_analysis', 'xact'),
    ('smps.sample_analysis', 'smps'),
    ('smps.instrument_settings', 'smps'),
    ('common.app_logs', 'N/A');

INSERT INTO common.data_tables (table_name, instrument) VALUES('xact.raw_measurements', 'xact');
INSERT INTO common.data_tables (table_name, instrument) VALUES('xact.instrument_settings', 'xact');

INSERT INTO common.data_tables (table_name,instrument)
	VALUES ('acsm.sample_analysis','acsm');
INSERT INTO common.data_tables (table_name,instrument)
	VALUES ('acsm.mass_loadings','acsm');
INSERT INTO common.data_tables (table_name,instrument)
	VALUES ('acsm.diag_calib','acsm');
INSERT INTO common.data_tables (table_name,instrument)
	VALUES ('acsm.tps','acsm');
INSERT INTO common.data_tables (table_name,instrument)
	VALUES ('acsm.dryer_stats','dryerstats');
