## Test environments
* Arch Linux (Local Install) - R 4.1.0
* FreeBSD 12.2 (Local Install) - R 4.1.0
* Windows-Latest (GitHub Actions) - R 4.1.0
* MacOS-Latest (GitHub Actions) - R 4.1.0
* Ubuntu-20.04 (GitHub Actions) - R 4.1.0
* Ubuntu-20.04 (GitHub Actions) - R (devel)
* devtools::check_rhub(interactive=FALSE)
* rhub::check_with_valgrind(show_status=FALSE)
* rhub::check_with_sanitizers(show_status=FALSE)
* rhub::rhub::check_on_solaris(show_status=FALSE)

## R CMD check results

There were no ERRRORs or WARNINGS.

There were NOTEs.

 - Days since last update: 4
  * This submission is a patch to correct issues identified by post-submission CRAN checks, including UBSAN and Valgrind checks.
  * This patch also improves Solaris support, and g++ is now marked as a Solaris requirement.

 - "GNU make is a SystemRequirements."
   * This is listed as a system requirement because on some systems the CMake configuration of a dependent library creates a GNU make file.

 - installed size is 7.3Mb
  * The package contains a dependent library (Mojang's fork of leveldb) and on some systems (ubuntu) this library is large when compiled.

 - Possibly mis-spelled words in DESCRIPTION: Minecraft (2:47, 15:41)
  * This word is not misspelled.
