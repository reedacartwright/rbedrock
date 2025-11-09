# Load and store NBT chunk data

- `get_chunk_nbt_value()` and `get_chunk_nbt_data()` load NBT data for a
  chunk from `db`. `get_chunk_nbt_value()` loads NBT data for a single
  chunk, and `get_chunk_nbt_data()` loads data for multiple chunks.

- `put_chunk_nbt_value()` and `put_chunk_nbt_data()` store NBT data for
  one or multiple chunks into `db`.

## Usage

``` r
get_chunk_nbt_data(x, z, dimension, tag, subtag, db)

get_chunk_nbt_value(x, z, dimension, tag, subtag, db)

put_chunk_nbt_data(values, x, z, dimension, tag, subtag, db)

put_chunk_nbt_value(value, x, z, dimension, tag, subtag, db)
```

## Arguments

- x, z, dimension:

  Chunk coordinates to extract data from. `x` can also be a character
  vector of db keys.

- db:

  A bedrockdb object.

- values:

  A (named) list of list of NBT objects.

- value:

  A list of NBT objects.

## Value

`get_chunk_nbt_value()` returns a list of NBT objects.
`get_chunk_nbt_data()` returns a named list of lists of NBT objects.
