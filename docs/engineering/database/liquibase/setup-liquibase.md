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
├── pre-post_deploy.xml
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
| centraldb-root.xml | Master changelog                   |
| pre-post_deploy.xml| Pre/Post changelogs (run always)   |

## Site Database

```text
ascent-sitedb/
├── pre-post_deploy.xml
├── sitedb-root.xml
└── feature directories
```

---

# Liquibase Installation
Follow the Liquibase installation guide on the official Liquibase site

https://docs.liquibase.com/community/get-started-5-0/install-liquibase-on-windows

## Recommendation
It's recommended to install Liquibase to a folder within your user directory. This will ensure you have full ownership of the Liquibase directory and will avoid needing to authenticate as admin during your daily workflow. 

Example location:

```C:\Users\username\tools\liquibase```

#### Important
Make sure you complete Step 3 - Add Liquibase to your system PATH in the official Liquibase installation documention

Verify installation:

```bash
liquibase --version
```

Expected:

```text
Liquibase Version: 5.x.x
```

---

# PostgreSQL JDBC Driver

Use Liquibase Package Mangager (LPM) to install your driver:

```
liquibase lpm add postgresql --global
```

The ```--global``` parameter ensures that database drivers are added in the liquibase installation directory rather than a specific project directory.

Verify:

```bash
liquibase --version
```

Ensure the PostgreSQL JDBC driver appears in the loaded libraries list.

Example:
```bash
$ liquibase --version
...
Libraries:
- internal\lib\commons-collections4.jar: Apache Commons Collections 4.5.0 By The Apache Software Foundation
- internal\lib\commons-io.jar: Apache Commons IO 2.22.0 By The Apache Software Foundation
- internal\lib\commons-lang3.jar: Apache Commons Lang 3.20.0 By The Apache Software Foundation
- internal\lib\commons-text.jar: Apache Commons Text 1.15.0 By The Apache Software Foundation
- internal\lib\h2.jar: H2 Database Engine 2.4.240 By H2 Group
- internal\lib\opencsv.jar: opencsv 5.12.0
- internal\lib\picocli.jar: picocli 4.7.7 By Remko Popma
- internal\lib\snakeyaml.jar: SnakeYAML 2.6.0
- lib\mssql-jdbc-13.4.0.jre11.jar: Microsoft JDBC Driver for SQL Server 13.4.0.jre11 By Microsoft Corporation
- lib\postgresql-42.7.11.jar: PostgreSQL JDBC Driver 42.7.11 By PostgreSQL Global Development Group
```

---

# Local Configuration

Each developer should create local configuration files. Essentially, you will need to create a configuration file for each target database environment you plan to deploy Liquibase database migrations to (e.g. individual developer, main shared, prod, etc.)

Do not commit these files to GitHub. As long as the configuration files are located in the appropriate path for the respective database target, there should be a .gitignore entry and git should already ignore it.

Example:

```text
db/liquibase/postgresql/changelogs/ascent-centraldb/liquibase.local.properties
```

```git``` will ignore any file with the .properties extension at the following paths:
```text
db/liquibase/postgresql/changelogs/ascent-centraldb/
db/liquibase/postgresql/changelogs/ascent-sitedb/
```

See the following location for example configuration templates you can use to get started:

```properties
db/liquibase/templates/liquibase.properties.template
```

---
### Next, review [Developer Database Setup Guide](setup-dev-database.md) for detailed instructions on configuring your database development envirionments.


---
[Back to Index](README.md)