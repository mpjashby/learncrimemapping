# learncrimemapping 2.3.0

* Add `create_dirs()` function to setup required directory structure inside the
  course project.


# learncrimemapping 2.1.1

* Bug fixes identified in first run of marking workflow.


# learncrimemapping 2.1.0

* Export `check_workspace()` with a read-only, beginner-friendly cli report,
  suggested repairs and structured results. Check course dependencies from
  DESCRIPTION, Positron and file-path alignment, directory spelling and access,
  Air JSON-with-comments settings, and secret-safe CARTO configuration.
* Add isolated workspace regression tests and manual checks for editor
  behaviour, inherited settings, software updates and startup limitations.


# learncrimemapping 2.0.0

* Require R 4.1.0 and lintr 3.4.0, replacing the removed nested-if linter
  while retaining complexity checks.
* Execute submissions with callr and evaluate in a separate session, using
  an explicit directory or the submission's project root, with its directory
  as the fallback. Stop on the first error or after ten minutes.
* Preserve original document locations and exclude explicitly disabled chunks
  without creating or overwriting companion scripts.
* Capture warnings and errors separately, distinguish checker failures and
  environment findings, and ignore local lintr configuration.
* Return structured results invisibly and retain full reports and plots in
  session temporary storage.
* Add regression tests and cross-platform checker compatibility CI.


# learncrimemapping 1.0.0

* Initial version for 2025 version of _Learn Crime Mapping with R_.
