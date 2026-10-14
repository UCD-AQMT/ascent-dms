CREATE TABLE common.qc_outcome (
    qc_outcome  INTEGER PRIMARY KEY,
    status      VARCHAR(10),
    validity    BOOLEAN NOT NULL,
    definition  TEXT
);
