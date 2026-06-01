# Liquibase Developer Setup Guide

## Purpose

ASCENT DMS uses Liquibase as the authoritative database version control system for PostgreSQL databases.

Liquibase is used to:

* Version control schema changes
* Deploy schema changes consistently across environments
* Track database change history
* Support CI/CD deployments
* Maintain auditability of database modifications

---

# Repository Structure

Database migrations are stored within the monorepo:

```text
db/
└── liquibase/
    └── postgresql/
        └── changelogs/
```

## Central Database

```text
ascent-centraldb/
├── centraldb-root.xml
├── python/
├── r/
├── shared/
└── stored-logic/
```

| Directory          | Purpose                            |
| ------------------ | ---------------------------------- |
| python             | Changes supporting Python services |
| r                  | Changes supporting R services      |
| shared             | Cross-service schema changes       |
| stored-logic       | Views, functions, procedures       |
| centraldb-root.xml | Master changelog                   |

## Site Database

```text
ascent-sitedb/
├── sitedb-root.xml
└── feature directories
```

---

# Prerequisites

## Java

Verify Java installation:

```bash
java --version
```

Recommended version:

```text
Java 21 LTS
```

## Liquibase

### Windows

```powershell
winget install Liquibase.Liquibase
```

or

```powershell
choco install liquibase
```

### macOS

```bash
brew install liquibase
```

### Linux

Follow the official installation guide:

https://docs.liquibase.com

Verify installation:

```bash
liquibase --version
```

Expected:

```text
Liquibase Version: 4.x.x
```

---

# PostgreSQL JDBC Driver

Download the PostgreSQL JDBC driver and place it in one of the following locations:

```text
<LIQUIBASE_HOME>/lib
```

or

```text
~/.liquibase/lib
```

Verify:

```bash
liquibase --version
```

Ensure the PostgreSQL JDBC driver appears in the loaded libraries list.

---

# Local Configuration

Each developer should create a local configuration file.

Do not commit this file to GitHub.

Example:

```text
db/liquibase/postgresql/liquibase.local.properties
```

Add to `.gitignore`:

```gitignore
liquibase.local.properties
```

Example configuration:

```properties
url=jdbc:postgresql://localhost:5432/ascent
username=postgres
password=********

driver=org.postgresql.Driver

changeLogFile=changelogs/ascent-centraldb/centraldb-root.xml

liquibase.command.defaultSchemaName=public
```

---

# Validation

Validate changelogs:

```bash
liquibase validate
```

---

# Preview SQL

Generate SQL without execution:

```bash
liquibase updateSQL
```

---

# Apply Migrations

Apply changes:

```bash
liquibase update
```

Liquibase automatically manages:

* DATABASECHANGELOG
* DATABASECHANGELOGLOCK

---

# Developer Workflow

1. Create GitHub issue.
2. Create feature branch.
3. Create migration file.
4. Validate changelog.
5. Generate SQL preview.
6. Apply locally.
7. Verify results.
8. Commit changes.
9. Open pull request.

---

# Pull Request Checklist

* [ ] Migration file added
* [ ] ChangeSet IDs are unique
* [ ] Rollback included
* [ ] Liquibase validate successful
* [ ] Liquibase update tested locally
* [ ] SQL output reviewed
* [ ] Documentation updated if necessary
