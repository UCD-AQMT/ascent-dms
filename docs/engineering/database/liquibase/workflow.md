
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
    - **Note**: These are general guidelines with room for some flexibility as needed. The key is to stay consistent within a standard set of rules.
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
