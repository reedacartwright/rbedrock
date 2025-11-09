# rbedrock (development version)

* Vanilla data was updated on 2025-11-09.
* Legacy checksum support was updated for compatibility with the most recent
  version of `digest.` (@eddelbuettel #14)

# rbedrock 0.4.1

* Undefined behavior identified by the M1-San system was fixed.

# rbedrock 0.4.0

## Breaking Changes

* This release includes a new API for reading and writing from the world
  database. The new API is designed to better support pipes and use a common
  database connection across functions. Most functions associated with reading
  and writing values to a world database were impacted. Consider the process of
  getting a set of values from a common prefix.
  
  ```R
  db <- bedrockdb(dbpath)
  # Previous API
  dat <- get_data(db, starts_with = prefix)
  # New API
  #   - automatically uses db
  #   - prefixes created with helper function
  dat <- get_data(starts_with(prefix))

  # Previous API
  put_data(db, dat)
  # New API
  put_data(dat)

  # Previous API
  put_values(db, names(dat), dat)
  # New API
  put_data(dat, names(dat))
  ```
* As part of the new API, the `get_values()`, `set_values()`, and related
  `_values()` functions were removed.

* The API for working NBT and rNBT values has been reimagined.
  - NBT lists are now treated as vectors where ever possible.
  - NBT longs are now stored as strings to protect against data loss.
  - NBT integers are now stored as doubles to protect against data loss.
* New `get_blocks_*` and `put_blocks_*` functions have replaced
  `get_chunk_blocks_*` and `put_chunk_blocks_*`. The new functions work per
  chunk and always return blocks as a 16 x 384 x 16 array, regardless of the
  dimension. 

## New Features

* New `default_db()` function to get/set a default db connection.
* New `print()` etc. commands for printing NBT using a tree structure.
* New `with_db()` and `local_db()` for temporarily changing the default db
  connection.

## Miscellaneous Fixes and Features

* New pkgdown-based website for rbedrock.
* The requirements for a key to be interpreted as a chunk key has been relaxed.
  Now any raw key of length 9, 10, 13, or 14 will be considered a chunk key
  if it contains at least one non-printable character. Collisions between
  plain and chunk keys can still occur, but they are not likely in this scheme.
* New `biome_name()` function for converting from a numeric id to biome name.
* New `read_rnbt_once()` function to parse NBT data that is mixed in with non
  NBT data. It returns a single NBT value along with how may bytes the value
  was.
* New `blocks_nbt()` and `blocks_str()` functions to convert block data between
  NBT and string representations.
* `spawning_area()` no longer requires a chunk to be withing 96 blocks of a
  player.
* Vendored LevelDB library is now built with exceptions enabled to avoid libcxx
  including `abort()` calls if exceptions are not enabled.
* The following dependencies were removed stringr, dplyr, purrr, magrittr,
  reader, vctrs, tidyr, and fs.

# RBedrock 0.3.3

* Fix notes identified by CRAN. (Unused calls to stderr and abort.)
* Update included leveldb library.

# RBedrock 0.3.2

* Fixed errors in usage of `Rf_error()`.
* Use `R_len_t` in more locations.

# RBedrock 0.3.1

* Remove hardcoded C++17 requirement when calling CMake.

# RBedrock 0.3.0

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

# RBedrock 0.2.0

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

# RBedrock 0.1.1

* Fixed errors identified by UBSAN and Valgrind
* Improved the detection of cmake binary.
* RBedrock can now be compiled on Solaris using GCC.
* RBedrock can now be compiled by CRAN's MacOS builder.
* Updated README.md

# RBedrock 0.1.0

* Initial Release
