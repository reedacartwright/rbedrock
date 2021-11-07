## RBedrock development
 * Support subchunk version 9 introduced for Caves and Cliffs.
 * Change tag format to @x:z:d:t:s. This change was made because subtags can now be negative.
 * Add locate_blocks() function.
 * Introduce intermediate rnbt (raw nbt) format.
 * Introduce new NBT API based on S3 classes.
 * Remove layer and simplify arguments to get_subchunk_layers_value and friends.
 * Improved ChunkVersion API

## RBedrock 0.1.1

 * Fixed errors identified by UBSAN and Valgrind
 * Improved the detection of cmake binary.
 * RBedrock can now be compiled on Solaris using GCC.
 * RBedrock can now be compiled by CRAN's MacOS builder.
 * Updated README.md

## RBedrock 0.1.0

 * Initial Release

