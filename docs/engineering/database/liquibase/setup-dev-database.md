# Developer Database Setup Guide

## Purpose

Each developer will have an individual development database that they use to build out features on a separate branch. Developers will merge their changes into a main shared development database via pull requests.


## Central Database

1. The first step is to connect with our admins to make sure you have a login for the required environments, and are a member of the appropriate database owner group role.
2. Copy the file db\liquibase\templates\liquibase.properties.template to the db\liquibase\postgresql\changelogs\ascent-centraldb directory. Rename the file so that it does not have the .template extension. Example, ```liquibase.properties```
    - ***Note:*** You will want to make a copy of the ```liquibase.properties``` configuration file for each target database you will apply Liquibase migrations to (e.g. local branch, main branch, production).
3. The following parameters are required to have values:
    - ```liquibase.command.url``` Host db connection URL
    - ```liquibase.command.username``` Your database username to login
    - ```liquibase.command.password``` Your password
    - Any other fields should already be pre-populated. However, make sure that this entry is present ```liquibaseSchemaName=liquibase```
    - ***Note:*** There should be a .gitignore policy in place. However, you should never commit this file to source control as it contains sensitive secrets.
4. Copy the file db\liquibase\templates\centraldb-setup.sh.template to the db\liquibase\postgresql\changelogs\ascent-centraldb directory. Rename the file so that it does not have the .template extension. Example ```centraldb-setup.sh```. The script file should already be executable. In case it isn't, you can make the Bash script executable by running the following command in your terminal:
    ```bash
    chmod +x centraldb-setup.sh 
    ```
5. Edit the centraldb-setup.sh script file and set values for the variables at the top of the file:
    ```bash
    # Set variable values
    dbhost=<development-database-host-url>
    dbuser=<your-database-user-name>
    dbname=measurements_<database-user-name>
    liquibasepropfile=<your-liquibase.properties>
    ```
    - These values should come directly from your liquibase.properties configuration file you set in step 3.
6. Once you've finished editing the centraldb-setup.sh file, execute the script in your terminal (be prepared to provide your password).
    ```bash
    ./centraldb-setup.sh
    ```
    - If everything is configured correctly, then the script should complete execution successfully and you will have a fully provisioned individual developer database.

    - ***IMPORTANT NOTE:*** You should only do steps 5. and 6. for your individual target database (not the main and definitely not production). The other targets (main/production) should already be setup. If they need to be re-provisioned, contact a tech lead for this process.


## Site Database
1. A local installation of PostgreSQL (version 14.xx) is required for the Site database development workflow.
    - https://www.enterprisedb.com/downloads/postgres-postgresql-downloads
2. Contact our admins to make sure you have a login for the required environments, and are a member of the appropriate database owner group role on the shared development database server.
3. After you install PostgreSQL locally on your system, run the following psql script to create the required database owner group role
    ```sql
    CREATE ROLE ascent_db_owner WITH
        NOLOGIN
        NOSUPERUSER
        INHERIT
        NOCREATEDB
        NOCREATEROLE
        NOREPLICATION
        NOBYPASSRLS;

    GRANT pg_monitor TO ascent_db_owner;
    ```
4. Ideally, and as a best practice, you will create a no superuser database login account for your development workflow and add it as a member of the database owner group role.
    ```sql
    CREATE ROLE <db_username> WITH
        LOGIN
        NOSUPERUSER
        INHERIT
        CREATEDB
        CREATEROLE
        PASSWORD '<enter-password>';

    GRANT ascent_db_owner TO <db_username> WITH ADMIN OPTION;
    ```
5. Copy the file db\liquibase\templates\liquibase.properties.template to the db\liquibase\postgresql\changelogs\ascent-sitedb directory. Rename the file so that it does not have the .template extension. Example, ```liquibase.properties```
    - ***Note:*** You will want to make a copy of the ```liquibase.properties``` configuration file for each target database you will apply Liquibase migrations to (e.g. local branch, main branch).
6. The following parameters are required to have values:
    - ```liquibase.command.url``` Host db connection URL (localhost, shared appdev)
    - ```liquibase.command.username``` Your database username to login
    - ```liquibase.command.password``` Your password
    - Any other fields should already be pre-populated. However, make sure that this entry is present ```liquibaseSchemaName=liquibase```
    - ***Note:*** There should be a .gitignore policy in place. However, you should never commit this file to source control as it contains sensitive secrets.
7. Copy the file db\liquibase\templates\sitedb-setup.sh.template to the db\liquibase\postgresql\changelogs\ascent-sitedb directory. Rename the file so that it does not have the .template extension. Example ```sitedb-setup.sh```. The script file should already be executable. In case it isn't, you can make the Bash script executable by running the following command in your terminal:
    ```bash
    chmod +x sitedb-setup.sh 
    ```
8. Edit the sitedb-setup.sh script file and set values for the variables at the top of the file:
    ```bash
    # Set variable values
    dbhost=<development-database-host-url> # e.g. localhost
    dbuser=<your-database-user-name> # e.g. your developer login (should be a member of the db owner role), postgres, etc.
    dbpassword=<your-database-user-password> # e.g. password for your developer login
    dbowner=<database-owner-role-name> # e.g. database owner role for the project (specify the role that should own the database and have permissions to create schemas and tables)
    dbname=<name-of-database> # e.g. measurements_dev
    dbconnection_options="options=-c%20role=$dbowner" # connection options to specify the role to connect with (needed for on-prem)
    liquibasepropfile=<your-liquibase.properties> # The liquibase.properties file configured to the database you specified above
    ```
    - Most of these values should come directly from your liquibase.properties configuration file you set in step 6.
9. Once you've finished editing the sitedb-setup.sh file, execute the script in your terminal (be prepared to provide your password).
    ```bash
    ./sitedb-setup.sh
    ```
    - If everything is configured correctly, then the script should complete execution successfully and you will have a fully provisioned individual developer database on your local system.

    - ***IMPORTANT NOTE:*** You should only do steps 8. and 9. for your local development target database (not the main shared database). The shared main development target should already be setup. If it needs to be re-provisioned, contact a tech lead for this process.

    
---
[Back to Index](README.md)