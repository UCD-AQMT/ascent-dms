# Provision Dev Database from Production Backup

This describes restoring a production Central (measurements) DB backup into your development environment. Use an isolated personal database (measurements_<your-db-user>).

Prerequisites
- Production `.backup` file (created via Azure/pgAdmin or other backup tool).
- Permissions on the development server and appropriate roles.
- pgAdmin or `pg_restore`/`psql` available.

Reference
- Video walkthrough (backup/restore example): https://youtu.be/-x4n_1M8Rjg?si=bUQVQPE0nU2JUgsa

Concise restore steps
1. Obtain the `.backup` file and ensure it is accessible to the machine running pgAdmin or `pg_restore`.
2. Drop any existing personal dev database:
   ```sql
   DROP DATABASE IF EXISTS measurements_<your-db-username>;
   ```
   Or delete via pgAdmin.
3. Create an empty database for your user:
   ```sql
   CREATE DATABASE measurements_<your-db-username> OWNER <db_owner_role>;
   ```
   Or create via pgAdmin (set Name and Owner).
4. Restore the `.backup` into the new database:
   - In pgAdmin: right-click the database → Restore… → select the `.backup` file → set Format (Custom/tar if applicable) → Start and wait.
   - Or use `pg_restore`:
     ```bash
     pg_restore -h <host> -U <user> -d measurements_<your-db-username> /path/to/backup.file
     ```
5. If objects are owned by production roles, adjust ownerships or role mappings as needed.
6. Verify the DB state by running Liquibase status with your branch's `liquibase.properties`:
   ```bash
   liquibase status --defaults-file=name-of-liquibase.properties-file
   ```

Warnings and tips
- Always restore into your personal dev instance, never into shared main/production.
- Treat backup files as sensitive — do not check them into source control and delete them securely when done.
- After restore, confirm the `liquibase` schema and changelog tables exist and that ownership/permissions align with your dev roles.

---

[Back to Index](README.md)
