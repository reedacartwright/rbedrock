## Submission

- CRAN errors identified as of 2025-03-25 have been fixed.

## Test environments

* Arch Linux - R 4.4.2 (Local Install)
* Windows-Latest - Release (GitHub Actions)
* MacOS-Latest - Release (GitHub Actions)
* Ubuntu-Latest - Release (GitHub Actions)
* Ubuntu-Latest - Devel (GitHub Actions)
* Ubuntu-Latest - Oldrel-1 (Github Actions)
* devtools::check_win_release()
* devtools::check_win_devel()
* devtools::check_mac_release()

## R CMD check results

There were no ERRORs or WARNINGS.

There were NOTEs.

- "GNU make is a SystemRequirements."
  * This is listed as a system requirement because on some systems the CMake
     configuration of a dependent library creates a GNU make file.
- installed size is [big]
  * The package contains a dependent library (Mojang's fork of leveldb) and on
    some systems (linux) this library is large when compiled.
