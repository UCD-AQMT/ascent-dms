# GH-126: Creation of Flags Lookup Table — Branch Dev Changes

**Date:** 2026-09-08  
**Author:** xiaoya.cheng  
**Branch:** dev  
**Target database:** `measurements_xycheng` on `aqrc-ascent-psqlsvr-branchdev-w2.postgres.database.azure.com`

---

## Overview

This change creates and populates a new lookup table `common.flags` that serves as a centralized reference for all EBAS data quality flag codes used across ASCENT instruments (AE33, ACSM, SMPS, Xact). Each flag maps to a `qc_outcome` severity and a human-readable description.

---

## Files Created

### SQL files

| File | Purpose |
|------|---------|
| `creation_flags_lut.sql` | Creates the `common.flags` table with columns `flag`, `qc_outcome`, and `description` |
| `load_data.sql` | Inserts all 23 flag records into `common.flags` |

### Liquibase changelog

| File | Purpose |
|------|---------|
| `20260904_GH-126-flags_lookup_table.xml` | Liquibase changelog referencing the two SQL files as separate changeSets |

---

## Table Schema

```sql
CREATE TABLE common.flags (
    flag        VARCHAR(10) PRIMARY KEY,
    qc_outcome  INTEGER NOT NULL,
    description TEXT
);
```

---

## Changelog Structure

The XML changelog contains two changeSets:

| id | Description | Rollback |
|----|-------------|---------|
| 1  | Create `common.flags` table schema | `DROP TABLE common.flags` |
| 2  | Load 23 flag records into `common.flags` | `DELETE FROM common.flags` |

The changelog was registered in `centraldb-root.xml`:

```xml
<include file="r/GH-126-flags_lookup_table/20260904_GH-126-flags_lookup_table.xml"
         relativeToChangelogFile="true"/>
```

---

## Steps to Apply to Branch Dev

> **Prerequisite:** `branchdev.liquibase.properties` must be configured with your credentials.  
> Commands must be run from the `ascent-centraldb/` directory.

### 1. Verify pending changes

```bash
liquibase status --defaults-file=branchdev.liquibase.properties
```

This lists all changeSets not yet applied to the target database.

### 2. Apply the changes

```bash
liquibase update --defaults-file=branchdev.liquibase.properties
```

Liquibase will apply only the changeSets not already recorded in `liquibase.databasechangelog`.

### 3. Verify the result

```bash
liquibase history --defaults-file=branchdev.liquibase.properties
```

Confirm that changeSet ids `1` and `2` from `20260904_GH-126-flags_lookup_table.xml` appear in the history.

---

## Rollback

To undo both changeSets (removes data then drops table):

```bash
liquibase rollbackCount 2 --defaults-file=branchdev.liquibase.properties
```

To undo only the data load (keeps the table, removes rows):

```bash
liquibase rollbackCount 1 --defaults-file=branchdev.liquibase.properties
```

---

## Next Step: Apply to Main Dev

See the pull request process — once the branch is merged, apply changes to the main dev database using a `maindev.liquibase.properties` pointed at `measurements` on `aqrc-ascent-psqlsvr-maindev-w2.postgres.database.azure.com`.
