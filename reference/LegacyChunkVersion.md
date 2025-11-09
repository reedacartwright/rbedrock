# Read and write legacy chunk version data

**\[superseded\]**

LegacyChunkVersion data (tag 118) stores the chunk version number for a
chunk. In Minecraft version 1.16.100, chunk version data was moved from
tag 118 to tag 44.

- `get_legacy_chunk_version_value()` and
  `get_legacy_chunk_version_data()` load LegacyChunkVersion data from
  `db`. `get_legacy_chunk_version_value()` loads data for a single
  chunk, and `get_legacy_chunk_version_data()` loads data for multiple
  chunks.

- `put_legacy_chunk_version_value()` and
  `put_legacy_chunk_version_data()` store LegacyChunkVersion data into
  `db`.

## Usage

``` r
get_legacy_chunk_version_value(x, z, dimension, db = default_db())

get_legacy_chunk_version_data(x, z, dimension, db = default_db())

put_legacy_chunk_version_value(value, x, z, dimension, db = default_db())

put_legacy_chunk_version_data(values, x, z, dimension, db = default_db())
```

## Arguments

- x, z, dimension:

  Chunk coordinates to extract data from. `x` can also be a character
  vector of db keys.

- db:

  A bedrockdb object.

- value:

  An integer

- values:

  A (named) vector of LegacyChunkVersion values. If `x` is missing, the
  names of `values` will be taken as the keys.

## Value

`get_legacy_chunk_version_value()` returns a LegacyChunkVersion data
value. `get_legacy_chunk_version_data()` returns a named vector of
LegacyChunkVersion data values.

## See also

ChunkVersion
