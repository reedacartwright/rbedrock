# Load and store BlockEntity NBT data

BlockEntity data (tag 49) holds a list of NBT values for entity data
associated with specific blocks.

## Usage

``` r
get_block_entity_data(x, z, dimension, db = default_db())

get_block_entity_value(x, z, dimension, db = default_db())

put_block_entity_data(values, x, z, dimension, db = default_db())

put_block_entity_value(value, x, z, dimension, db = default_db())
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

`get_block_entity_value()` returns a list of NBT objects.
`get_block_entity_data()` returns a named list of lists of NBT objects.

## Details

- `get_block_entity_value()` and `get_block_entity_data()` load
  BlockEntity data from `db`. `get_block_entity_value()` loads data for
  a single chunk, and `get_block_entity_data()` loads data for multiple
  chunks.

- `put_block_entity_value()` and `put_block_entity_data()` store
  BlockEntity data for one or multiple chunks into `db`.
