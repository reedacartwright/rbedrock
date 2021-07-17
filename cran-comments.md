## Test environments
* Arch Linux (Local Install) - R 4.1.0
* FreeBSD 12.2 (Local Install) - R 4.1.0
* Windows-Latest (GitHub Actions) - R 4.1.0
* MacOS-Latest (GitHub Actions) - R 4.1.0
* Ubuntu-20.04 (GitHub Actions) - R 4.1.0
* Ubuntu-20.04 (GitHub Actions) - R (devel)
* rhub::check_for_cran()
* rhub::check_with_valgrind()
* rhub::check_with_sanitizers()

## R CMD check results

There were no ERRRORs or WARNINGS.

There were NOTEs.

 - "GNU make is a SystemRequirements."
   * This is listed as a system requirement because on some systems the CMake configuration of a dependent library creates a GNU make file.

 - installed size is 7.3Mb
  * The package contains a dependent library (Mojang's fork of leveldb) and on some systems (ubuntu) this library large when compiled.

