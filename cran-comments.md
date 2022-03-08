## Resubmission



## Test environments

* Arch Linux (Local Install) - R 4.1.0
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

 - "GNU make is a SystemRequirements."
   * This is listed as a system requirement because on some systems the CMake
     configuration of a dependent library creates a GNU make file.

 - installed size is 7.3Mb
  * The package contains a dependent library (Mojang's fork of leveldb) and on
    some systems (ubuntu) this library is large when compiled.

 - Possibly mis-spelled words in DESCRIPTION: Minecraft (2:47, 15:41)
  * This word is not misspelled.
