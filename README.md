# ascent-dms
Monorepo for ASCENT Data Management System (DMS) services and infrastructure.

## Branch Naming Convention Policy
- Branch name must start with a matching pattern
  - (?-i)^((feature|bugfix|hotfix|docs|chore)/[a-z0-9-]+-[a-z0-9]+|release/v[0-9]{4}.[0-9]+(.[0-9]+)?)$
- Branch name examples:
  - Standard Branches (Prefix/Description-IssueID)
  - feature/user-login-T-123
  - bugfix/header-styling-issue-456
  - hotfix/critical-security-patch-prod-911
  - docs/update-readme-docs-55
  - task/update-dependencies-ch-99
- Release Branches (release/vYYYY.MAJOR.MINOR)
  - release/v2025.1.12
  - release/v2026.10.1
  - release/v2025.1	(Minor Optional)
  - release/v2026.10 	(Minor Optional)


