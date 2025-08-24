## Submission

- This submission fixes an undefined behavior identified by CRAN on 2024-08-24.
- CRAN errors identified as of 2025-08-24 have been fixed.

## Test environments

* Arch Linux - R 4.5.1 (Local Install)
* Windows-Latest - Release (GitHub Actions)
* MacOS-Latest - Release (GitHub Actions)
* Ubuntu-Latest - Release (GitHub Actions)
* Ubuntu-Latest - Devel (GitHub Actions)
* Ubuntu-Latest - Oldrel-1 (Github Actions)
* rhub::rhub_check()
* devtools::check_win_release()
* devtools::check_win_devel()
* devtools::check_mac_release()

## R CMD check results

0 errors | 0 warnings | 2 notes

- "GNU make is a SystemRequirements."
  * This is listed as a system requirement because on some systems the CMake
    configuration of a dependent library creates a GNU make file.
- installed size is big
  * The package contains a dependent library and on some systems this library
    is large when compiled.
