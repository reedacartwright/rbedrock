## RBedrock 0.3.1
 * Remove hardcoded C++17 requirement when calling CMake.

## RBedrock 0.3.0
 * Added support for reading and writing 1.18.30+ actor digests.
 * Chunk data functions have been expanded.
 * [Breaking Change] Biome functions now return CNC biomes data (CnC biome functions have been renamed.) Accessing legacy biome data can be done with legacy biome functions.
 * [Breaking Change] Chunk data functions and files have been renamed to match internal Mojang names.
 * [Breaking Change] Chunk data functions no longer default to reading all keys from `db` if no keys are present.
 * [Breaking Change] Chunk tag key descriptions have been changed to better match internal Mojang descriptions.
 * Maximum chunk key tag has been increased to 96.
 * [Breaking Change] Change key format to require prefixes in front of every key. This allows rbedrock to support multiple key formats.
 * Add nbt_raw_string to support reading and writing nbt_string data that contains embedded nulls.
 * Change default parameters to bedrockdb to match Minecraft. This reduces the size of worlds modified by rbedrock.
 * Generate random world seeds using 64-bits.
 * [Fix] Generate default package options on loading rbedrock. This fixes users not being able to find the world directory on Window.
 * Create 1.19+ worlds by default when using create_world()
 * Add functions for reading and writing actors data for a chunk.
 * Add utility functions for working with unique ids.

## RBedrock 0.2.0
 * Support subchunk version 9 introduced for Caves and Cliffs.
 * Support Caves and Cliffs' 3DMaps.
 * Change tag format to `@x:z:d:t:s`. This change was made because subtags can now be negative.
 * Add `locate_blocks()` function.
 * Introduce intermediate `rnbt` (raw nbt) format.
 * Introduce new NBT API based on S3 classes.
 * Remove layer and simplify arguments to `get_subchunk_layers_value()` and friends.
 * Improved ChunkVersion API
 * Remove `-Werror` flag used by leveldb on some platforms
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

