INSERT INTO common.qc_outcome (qc_outcome, status, validity, definition)
VALUES
    (1, 'Good',    TRUE,  'Represents a good value where all quality control criteria are satisfied'),
    (3, 'Suspect', TRUE,  'Represents a suspect value that is questionable but potentially usable'),
    (4, 'Bad',     FALSE, 'Represents a bad value that failed quality control and should not be used'),
    (9, 'Missing', FALSE, 'Represents a missing value where no measurement was recorded');
