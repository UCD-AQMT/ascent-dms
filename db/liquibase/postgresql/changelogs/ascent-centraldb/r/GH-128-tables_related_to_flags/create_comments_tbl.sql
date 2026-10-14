-- Create table: flags_default_comments
CREATE TABLE common.flags_default_comments (
    id              INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    flag_id         INTEGER NOT NULL,
    instrument      VARCHAR(50) NOT NULL,
    default_comment TEXT NOT NULL,

    -- Foreign key to common.flags(id)
    CONSTRAINT fk_flags_default_comments_flag
        FOREIGN KEY (flag_id) 
        REFERENCES common.flags (id)
        ON UPDATE CASCADE
        ON DELETE RESTRICT -- Prevents deleting a parent flag if comments still point to it
);

-- Recommended index on the foreign key column for fast joins
CREATE INDEX idx_flags_default_comments_flag_id 
    ON common.flags_default_comments (flag_id);

-- Optional composite index if you frequently query by both instrument and flag_id
--CREATE INDEX idx_flags_default_comments_inst_flag 
--    ON flags_default_comments (instrument, flag_id);
