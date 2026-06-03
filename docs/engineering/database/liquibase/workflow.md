
# Developer Workflow

1. Create GitHub issue.
2. Create feature branch.
3. Create migration file. (Including migration directory for best practice)
    - Directory names should align with GitHub work items.
        - Recommended:
        ```
        python/
        └── GH-145-add-user-authentication/
        ```
        - or
        ```
        python/
        └── GH-145/
        ```
    - Changelog Files
        - Format:
        ```
        YYYYMMDD_short-description.xml
        ```
        - Example
        ```
        20260601_add_site_table.xml
        ```
    - ChangeSet IDs
        - Must be globally unique.
        - Recommended:
        ```xml
        <changeSet
            id="GH-145-001"
            author="jdoe">
        ```
        - Examples:
        ```
        GH-145-001
        GH-145-002
        GH-145-003
        ```
        - Avoid when possible:
        ```
        1
        2
        3
        ```
    - Creating a Migration
        - Have a look at the ```centraldb-root.xml``` changelog as an example for how to effectively organize your changelogs for your migrations, especially when your feature will require multiple changlog files.
        - Here's an example of a single changelog file that lists several other changelog files related to the migration. The parent file points to the changelogs listed and they will be executed in order.
        ```xml
        <?xml version="1.0" encoding="UTF-8"?>

        <databaseChangeLog
                xmlns="http://www.liquibase.org/xml/ns/dbchangelog"
                xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                xsi:schemaLocation="http://www.liquibase.org/xml/ns/dbchangelog http://www.liquibase.org/xml/ns/dbchangelog/dbchangelog-latest.xsd">
                
                <!-- database schema generation -->
                <include file="baseline_sync_schemas.xml" relativeToChangelogFile="true"/>
                <include file="baseline_sync_schema_objects.xml" relativeToChangelogFile="true"/>

                <!-- load data into lookup tables -->
                <include file="baseline_sync_loadData_acsm_params.xml" relativeToChangelogFile="true" />
                <include file="baseline_sync_loadData_common_column_mappings.xml" relativeToChangelogFile="true" />
                <include file="baseline_sync_loadData_common_sites.xml" relativeToChangelogFile="true" />
                <include file="baseline_sync_loadData_purpleair_sensors.xml" relativeToChangelogFile="true" />
                <include file="baseline_sync_loadData_xact_element_params.xml" relativeToChangelogFile="true" />
                <include file="baseline_sync_loadData_xact_env_params.xml" relativeToChangelogFile="true" />
            
        </databaseChangeLog>
        ```
        - Example of the first changelog in the list. One of many ways to structure a changelog
        ```xml
        <?xml version="1.0" encoding="UTF-8"?>

        <databaseChangeLog
                xmlns="http://www.liquibase.org/xml/ns/dbchangelog"
                xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                xsi:schemaLocation="http://www.liquibase.org/xml/ns/dbchangelog http://www.liquibase.org/xml/ns/dbchangelog/dbchangelog-latest.xsd">

            <changeSet id="1" author="rudi.demarco" labels="issue-7, baseline-changelog-sync">
                <comment>create schema dependencies</comment>
                <sql>
                    CREATE SCHEMA IF NOT EXISTS acsm
                        AUTHORIZATION azure_pg_admin;
                        
                    CREATE SCHEMA IF NOT EXISTS common
                        AUTHORIZATION azure_pg_admin;

                    CREATE SCHEMA IF NOT EXISTS liquibase
                        AUTHORIZATION azure_pg_admin;

                    CREATE SCHEMA IF NOT EXISTS purpleair
                        AUTHORIZATION azure_pg_admin;

                    CREATE SCHEMA IF NOT EXISTS smps
                        AUTHORIZATION azure_pg_admin;

                    CREATE SCHEMA IF NOT EXISTS xact
                        AUTHORIZATION azure_pg_admin;

                    GRANT ALL ON SCHEMA xact TO azure_pg_admin;
                </sql>
            </changeSet>
            
        </databaseChangeLog>
        ```
        - Add a changeset entry in the centraldb-root.xml (or sitedb-root.xml) pointing to your parent changelog. Then follow the new entry with an updated changeset to tag the database with the next version:
        ```xml
        <!-- Base release initializes the project with the baseline database schema starting point -->
        <include file="shared/baseline_sync/20260109_baseline_sync.xml" relativeToChangelogFile="true"/>
        <changeSet author="rudi.demarco" id="tag-v2026.0">
            <tagDatabase tag="2026.0" />
        </changeSet>
        ```
    - **Note**: This is still a work in progress. These are general guidelines with room for some flexibility as needed. The key is to stay consistent within a standard set of rules. Please continue to reference the **official Liquibase documentation** for information on what is all available for working with database migrations and changelogs.
    - **Things to take into consideration:**
        - Do not continue to use the ```context``` attribute. Liquibase intends for this attribute to be used for specifying an environment (e.g. ```context="test"```, ```context="prod"```). We don't have this in our workflow as of right now, but we shouldn't use that attribute in case we decide to in the future.
        - The ```labels``` attribute is generally used for providing GitHub issue and issue description information.
        - Use the ```<comment>``` tag for providing additional information about the changeset (e.g. ```<comment>Adding a shipping_address column to the sites table for equipment package tracking</comment>```)
4. Validate changelog.
    - Always validate before committing.
    ```bash
    liquibase validate --defaults-file=(name of your liquibase.properties file)
    ```
    Expected:
    ```bash
    No validation errors found.
    ```
5. Generate SQL preview.
    - Generate SQL without execution
    ```bash
    liquibase updateSQL --defaults-file=(name of your liquibase.properties file)
    ```
    - Review output carefully
6. Apply locally.
    - Execute
    ```bash
    liquibase updateSQL --defaults-file=(name of your liquibase.properties file)
    ```
    - Liquibase will update:
    ```bash
    DATABASECHANGELOG
    DATABASECHANGELOGLOCK
    ```
7. Rollbacks
    - Every migration should include rollback logic whenever feasible.
    - Example:
    ```xml
    <changeSet
        id="GH-145-001"
        author="jdoe">

        <createTable tableName="site">
            ...
        </createTable>

        <rollback>
            <dropTable tableName="site"/>
        </rollback>

    </changeSet>
    ```
8. Verify results.
    - Confirm:
    ```sql
    SELECT *
    FROM databasechangelog
    ORDER BY dateexecuted DESC;
    ```
9. Commit changes.
10. Open pull request.

---

# Pull Request Checklist

* [ ] Migration file added
* [ ] ChangeSet IDs are unique
* [ ] Rollback included (if applicable)
* [ ] Liquibase validate successful
* [ ] Liquibase update tested locally
* [ ] SQL output reviewed
* [ ] Documentation updated if necessary


---
[Back to Index](README.md)