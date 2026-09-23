ALTER TABLE common.flags ADD COLUMN id SERIAL;

ALTER TABLE common.flags DROP CONSTRAINT flags_pkey;

ALTER TABLE common.flags ADD CONSTRAINT pk_flags PRIMARY KEY (id);

ALTER TABLE common.flags ADD CONSTRAINT uq_flags_flag UNIQUE (flag);
