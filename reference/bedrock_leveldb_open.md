# Open a LevelDB Database

Create a `leveldb` object, to interact with a LevelDB database.

## Usage

``` r
bedrock_leveldb_open(
  path,
  create_if_missing = NULL,
  error_if_exists = NULL,
  paranoid_checks = NULL,
  write_buffer_size = NULL,
  max_open_files = NULL,
  block_size = NULL,
  cache_capacity = NULL,
  bloom_filter_bits_per_key = NULL,
  compression_level = NULL
)
```

## Arguments

- path:

  The path to the database, as stored on the filesystem. This will
  create a *directory* at this path if one does not exist (and if
  `create_if_missing` is `TRUE`.

- create_if_missing:

  Create the database if one does not already exist at `path` (this
  defaults to `TRUE`, which differs from the LevelDB default of
  `FALSE`).

- error_if_exists:

  Throw an error if the database already exists at `path` (LevelDB
  default: `FALSE`).

- paranoid_checks:

  If `TRUE`, LevelDB will do aggressive checking of the data it is
  processing and will stop early if it detects any errors. This may have
  unforeseen ramifications: for example, a corruption of one DB entry
  may cause a large number of entries to become unreadable or for the
  entire DB to become unopenable. (LevelDB default: `FALSE`).

- write_buffer_size:

  Amount of data (in bytes) to build up in memory (backed by an unsorted
  log on disk) before converting to a sorted on-disk file. Larger values
  increase performance, especially during bulk loads. Up to two write
  buffers may be held in memory at the same time, so you may wish to
  adjust this parameter to control memory usage. Also, a larger write
  buffer will result in a longer recovery time the next time the
  database is opened.

- max_open_files:

  Number of files that can be used by the database. You may need to
  increase this if your database has a large working set (budget one
  open file per 2MB of working set). (LevelDB default: 1000).

- block_size:

  The approximate size of user data packed per block (user data is
  stored in a set of blocks, and a block is the unit of reading from
  disk). The block size here corresponds to uncompressed data; the
  actual size of the unit read from disk may be smaller if compression
  is enabled (LevelDB default: 4K)

- cache_capacity:

  The size of the cache to use. If non-`NULL` this must be a
  non-negative integer, indicating the size of the cache in bytes. If
  `NULL` (the default) then LevelDB will create an 8MB internal cache.

- bloom_filter_bits_per_key:

  If non-NULL, this sets up a 'filter policy' to reduce disk reads. A
  good value for bits_per_key is 10, which yields a filter with ~ 1%
  false positive rate. Further information from the LevelDB headers
  (filter_policy.h) "This object is responsible for creating a small
  filter from a set of keys. These filters are stored in leveldb and are
  consulted automatically by leveldb to decide whether or not to read
  some information from disk. In many cases, a filter can cut down the
  number of disk seeks form a handful to a single disk seek per
  `DB::Get()` call"

## Details

For all optional arguments (i.e., all but `path`) a value of `NULL`
means that we use the `LevelDB` default; the LevelDB default of each
argument is indicated in the argument documentation.

This function returns an 'R6' class with a number of methods.

## Author

Rich FitzJohn
