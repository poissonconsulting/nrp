# Changelog

## nrp 0.2.1.9000 (2026-10-15)

### Chore

- Format with air (#31).

### Uncategorized

- Merge pull request \#33 from poissonconsulting/replace-lubridate.

  - Replaced lubridate with dttr2 equivalent

- Merge pull request \#28 from
  poissonconsulting/update-evan-contact-email.

  Update Evan Amies-Galonski email to personal address

- Merge pull request \#27 from poissonconsulting/ctd-upcast.

  Ctd upcast

- Merge pull request \#26 from poissonconsulting/ctd-upcast.

  accomodate cast direction in ctd data

## nrp 0.2.1 (2026-03-24)

- Fix edge case in delineation of up/down casts with multiple max
  depths.

## nrp 0.2.0 (2026-02-22)

- Accommodate cast direction in CTD data
- Removed ems information from README
- Removed all instances of `.data$`.

## nrp 0.1.1 (2025-08-07)

- Removed EMS information from `README` as the EMS system, from a
  separate government database, has undergone major revisions. The new
  EMS system should be released November 2025, at which point we will
  discuss with the client if EMS functions should still be included and
  maintained.
- Removed all instances of `.data$` to align with current tidyverse
  syntax.

## nrp 0.1.0 (2025-08-07)

- Created `NEWS` file to record changes to package.
