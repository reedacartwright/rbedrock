## Test environments
* Arch Linux (Local Install) - R 4.1.0
* Windows-Latest (GitHub Actions) - R 4.1.0
* MacOS-Latest (GitHub Actions) - R 4.1.0
* Ubuntu-20.04 (GitHub Actions) - R 4.1.0
* Ubuntu-20.04 (GitHub Actions) - R (devel)

## R CMD check results

There were no ERRRORs or WARNINGS.

There were NOTEs.

 - "GNU make is a SystemRequirements."
   * This is listed as a system requirement because on some systems the CMake configuration of a dependent library creates a GNU make file.

 - installed size is 7.3Mb
  * The package contains a dependent library (Mojang's fork of leveldb) and on some systems (ubuntu) this library large when compiled.

## Resubmission

This is a resubmission. I have fixed the following issues:

* changed http --> https, added trailing slashes, and followed moved content as appropriate.

* devtools::check(remote=TRUE) no longer emits NOTEs for the README.md.
