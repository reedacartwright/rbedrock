# Read and write chunk version data

ChunkVersion data (tag 44) stores the chunk version number for a chunk.

## Usage

``` r
get_chunk_version_value(x, z, dimension, db = default_db())

get_chunk_version_data(x, z, dimension, db = default_db())

put_chunk_version_value(value, x, z, dimension, db = default_db())

put_chunk_version_data(values, x, z, dimension, db = default_db())
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

  A (named) vector of ChunkVersion values. If `x` is missing, the names
  of `values` will be taken as the keys.

## Value

`get_chunk_version_value()` returns a ChunkVersion data value.
`get_chunk_version_data()` returns a named vector of ChunkVersion data
values.

## Details

- `get_chunk_version_value()` and `get_chunk_version_data()` load
  ChunkVersion data from `db`. `get_chunk_version_value()` loads data
  for a single chunk, and `get_chunk_version_data()` loads data for
  multiple chunks.

- `put_chunk_version_value()` and `put_chunk_version_data()` store
  ChunkVersion data into `db`.

## See also

ChunkVersion
