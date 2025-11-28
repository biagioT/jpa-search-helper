# Security Policy

## Supported Versions

We currently provide security updates for the following versions of the project. Please always use the latest patch version available for each release line.

| Version | Supported Spring Boot | Support Status |
| :------ | :-------------------- | :------------- |
| 3.6.x   | 4.0.x                 | :white_check_mark: Supported |
| 3.5.x   | 3.5.x                 | :warning: Limited Maintenance |
| 3.4.x   | 3.4.x                 | :x: End of Life |
| 3.3.x   | 3.4.x                 | :x: End of Life |
| < 3.0.0 | < 3.3.x               | :x: End of Life |

## Reporting a Vulnerability

We take the security of `jpa-search-helper` very seriously. If you believe you have found a security vulnerability, please **do not open a public Issue**.

### Reporting Process

To report a vulnerability privately, please use GitHub's **Private Vulnerability Reporting** feature (if enabled) or reach out directly.

1.  Go to the **Security** tab of the repository.
2.  Click on **"Report a vulnerability"** to open a private advisory.
3.  Please include enough details to reproduce the issue (e.g., code snippets, Entity configuration, or a sample `JPASearchInput` JSON payload).

*If private reporting is not enabled, please contact the maintainer via the email address found in the GitHub profile or the linked website.*

### What to Expect

* We will acknowledge your report within 48 hours.
* We will keep you updated on the progress of the resolution.
* Once the issue is resolved, a new version will be released, and a Security Advisory will be published.
