# Read and write Blocks data

Blocks data stores information about blocks in a world and their
properties. Blocks data is stored per-subchunk as
[SubChunkBlocks](https://reedacartwright.github.io/rbedrock/reference/SubChunkBlocks.md)
data. These functions are wrappers around a SubChunkBlocks functions to
make it easy to save and load blocks for an entire chunk.

## Usage

``` r
get_blocks_value(
  x,
  z,
  dimension,
  db = default_db(),
  names_only = FALSE,
  extra_block = !names_only
)

get_blocks_data(
  x,
  z,
  dimension,
  db = default_db(),
  names_only = FALSE,
  extra_block = !names_only
)

put_blocks_value(value, x, z, dimension, db = default_db())

put_blocks_data(values, x, z, dimension, db = default_db())
```

## Arguments

- x, z, dimension:

  Chunk coordinates to extract data from. `x` can also be a character
  vector of db keys.

- db:

  A bedrockdb object.

- names_only:

  A logical scalar. Return only the names of the blocks, ignoring block
  states.

- extra_block:

  A logical scalar. Append the extra block layer to the output
  (separated by ";"). This is mostly useful if you have waterlogged
  blocks. If the extra block is air, it will not be appended.

- value:

  A 16x384x16 character array.

- values:

  A (named) list of Blocks values. If `x` is missing, the names of
  `values` will be taken as the keys.

## Value

`get_blocks_value()` returns a Blocks value. `get_blocks_data()` returns
a named list of Blocks values.

## Details

- `get_blocks_value()` and `get_blocks_data()` load Blocks data from
  `db`. `get_blocks_value()` loads data for a single chunk, and
  `get_blocks_data()` loads data for multiple chunks.

- `put_blocks_value()` and `put_blocks_data()` store Blocks data into
  `db`.

## See also

SubChunkBlocks
