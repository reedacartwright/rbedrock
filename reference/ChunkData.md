# Load and store raw chunk data

- `get_chunk_value()` and `get_chunk_data()` load raw chunk data from
  `db`. `get_chunk_value()` loads data for a single chunk, and
  `get_chunk_data()` loads data for multiple chunks.

- `put_chunk_value()` and `put_chunk_data()` store chunk data for one or
  multiple chunks into `db`.

## Usage

``` r
get_chunk_value(x, z, dimension, tag, subtag, db)

get_chunk_data(x, z, dimension, tag, subtag, db)

put_chunk_value(value, x, z, dimension, tag, subtag, db)

put_chunk_data(values, x, z, dimension, tag, subtag, db)
```

## Arguments

- x, z, dimension:

  Chunk coordinates to extract data from. `x` can also be a character
  vector of db keys.

- db:

  A bedrockdb object.

- value:

  A raw vector.

- values:

  A list of raw vectors.

## Value

`get_chunk_value()` returns a single raw vector. `get_chunk_data()`
returns a named list of raw data.
