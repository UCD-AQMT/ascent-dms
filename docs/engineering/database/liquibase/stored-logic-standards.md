# Stored Logic Standards Guide

### A standard guide for 
- Directory location
- Procedure Naming
- View Naming
- Liquibase Changeset Wrapper

---
### Directory
```text
stored-logic/
├── procedures/
├── user-defined-functions/
└── views/
```

---
### Procedure Naming
```text
spName-vYYYY.Count.Minor (Year, Release Count, Patch)
spCreateSite-v2026.1.0.sql
spCreateSite-v2026.1.1.sql
spCreateSite-v2026.2.0.sql
```

Patch definition is if you are only fixing a bug inside a calculation or a WHERE clause without changing input/output parameters.

---
### View Naming
```text
viewName-vYYYY.Count.Minor (Year, Release Count, Patch)
vwSiteSummary-v2026.1.0.sql
```

---
### Liquibase Wrapper
```xml
<changeSet
    id="GH-212-001"
    author="jdoe">

    <sqlFile
        path="stored-logic/views/vwSiteSummary-v2026.1.0.sql"
        relativeToChangelogFile="true"/>

</changeSet>
```


---
[Back to Index](README.md)