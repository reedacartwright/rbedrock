## Resubmission

This resubmission is being made because the previous submission (2021-07-21)
expired with no action being taken by CRAN. foghorn::incoming() no longer lists
rbedrock as an incoming package, and it has been archived by CRAN. Thus I am
making this resubmission to have the package reviewed again.

To be clear this version of my package has fixed all errors discovered in the 
previous version by CRAN. This includes:
  - Fixed failure to build on the MacOS builder.
  - Fixed failure to build on Solaris. (GCC is a SystemRequirement for Solaris.)
  - Fixed all issues detected by clang-UBSAN, gcc-UBSAN, and valgrind.

## Test environments

* Arch Linux (Local Install) - R 4.1.0
* FreeBSD 12.2 (Local Install) - R 4.1.0
* Windows-Latest (GitHub Actions) - R 4.1.0
* MacOS-Latest (GitHub Actions) - R 4.1.0
* Ubuntu-20.04 (GitHub Actions) - R 4.1.0
* Ubuntu-20.04 (GitHub Actions) - R (devel)
* devtools::check_rhub(interactive=FALSE)
* devtools::check_win_release()
* devtools::check_win_devel()
* rhub::check_with_valgrind(show_status=FALSE)
* rhub::check_with_sanitizers(show_status=FALSE)
* rhub::check(platform="macos-highsierra-release-cran", show_status=FALSE)
* rhub::check_on_windows(show_status=FALSE)
* rhub::check_on_solaris(show_status=FALSE)

## R CMD check results

There were no ERRORs or WARNINGS.

There were NOTEs.

 - New Submission / Package was archived on CRAN.
  * This submission is a patch to correct all issues identified CRAN,
    including UBSAN and Valgrind checks.
  * This patch also fixes the detection of CMake and enables CRAN to build
    binary MacOS packages. Simon Urbanek helpfully provided me with the location
    of CMake on the MacOS builder server.
  * This patch also allows rbedrock to be build on Solaris using gcc.
    This is noted in SystemRequirements.

 - "GNU make is a SystemRequirements."
   * This is listed as a system requirement because on some systems the CMake
     configuration of a dependent library creates a GNU make file.

 - installed size is 7.3Mb
  * The package contains a dependent library (Mojang's fork of leveldb) and on
    some systems (ubuntu) this library is large when compiled.

 - Possibly mis-spelled words in DESCRIPTION: Minecraft (2:47, 15:41)
  * This word is not misspelled.
