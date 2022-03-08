## Submission

- RBedrock has been updated to support Minecraft Bedrock 1.18.

- A previous CRAN failure due to a -Werror has been fixed.

## Test environments

* Arch Linux (Local Install) - R 4.1.2
* Windows-Latest (GitHub Actions) - R 4.1.2
* MacOS-Latest (GitHub Actions) - R 4.1.2
* Ubuntu-Latest (GitHub Actions) - R 4.1.2
* Ubuntu-Latest (GitHub Actions) - R (devel)
* devtools::check_rhub(interactive=FALSE)
* devtools::check_win_release()
* devtools::check_win_devel()
* rhub::check_on_windows(show_status=FALSE)
* rhub::check_with_valgrind(show_status=FALSE)
* rhub::check(platform="macos-highsierra-release-cran", show_status=FALSE)
* rhub::check(platform="ubuntu-rchk", show_status=FALSE)
* rhub::check(platform="macos-m1-bigsur-release", show_status=FALSE)

## R CMD check results

There were no ERRORs or WARNINGS.

There were NOTEs.

 -  Archived on 2021-12-15 as check problems were not corrected in time. Uses -Werror although warned against in WRE.
  * This has been fixed.

 - "GNU make is a SystemRequirements."
   * This is listed as a system requirement because on some systems the CMake
     configuration of a dependent library creates a GNU make file.

 - installed size is [big]
  * The package contains a dependent library (Mojang's fork of leveldb) and on
    some systems (linux) this library is large when compiled.

 - Possibly mis-spelled words in DESCRIPTION: Minecraft (2:47, 15:41)
  * This word is not misspelled.
