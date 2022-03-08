## RBedrock 0.2.0
 * Support subchunk version 9 introduced for Caves and Cliffs.
 * Support Caves and Cliffs' 3DMaps.
 * Change tag format to `@x:z:d:t:s`. This change was made because subtags can now be negative.
 * Add `locate_blocks()` function.
 * Introduce intermediate `rnbt` (raw nbt) format.
 * Introduce new NBT API based on S3 classes.
 * Remove layer and simplify arguments to `get_subchunk_layers_value()` and friends.
 * Improved ChunkVersion API
 * Remove `-Werror` flag used by levedb on some platforms
 * Added 1.18 example world
 * Add support for 1.18's empty palettes.
 * Rename chunk tag to match internal bedrock names.
 * Add `starts_with` option to `get_values()`, and `db$mget_prefix()` and `bedrock_leveldb_mget_prefix()` function.
 * Add `get_subchunk_blocks_from_chunk()` function to efficiently lead subchunks from a single chunk.
 * Add utility functions for calculating spawning area and simulation area.

## RBedrock 0.1.1

 * Fixed errors identified by UBSAN and Valgrind
 * Improved the detection of cmake binary.
 * RBedrock can now be compiled on Solaris using GCC.
 * RBedrock can now be compiled by CRAN's MacOS builder.
 * Updated README.md

## RBedrock 0.1.0

 * Initial Release

