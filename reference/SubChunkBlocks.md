# Load and store SubChunkBlocks data

SubChunkBlocks data (tag 47) stores information about the blocks in a
world. Each chunk is divided into multiple 16x16x16 subchunks, and the
blocks for each subchunk are stored separately. Blocks are stored per
subchunk in a palette-based format. Block storage is separated into
multiple layers where each layer has its own block palette and block
ids. In practices subchunks have either one or two layers, and the extra
layer is most-often used to store water for water-logged blocks.

## Usage

``` r
get_subchunk_blocks_value(x, z, dimension, subchunk, db = default_db())

get_subchunk_blocks_data(x, z, dimension, subchunk, db = default_db())

put_subchunk_blocks_value(
  value,
  x,
  z,
  dimension,
  subchunk,
  db = default_db(),
  version = 9L
)

put_subchunk_blocks_data(
  values,
  x,
  z,
  dimension,
  subchunk,
  db = default_db(),
  version = 9L
)

read_subchunk_blocks_value(rawvalue, subchunk_position = NA_integer_)

write_subchunk_blocks_value(value, subchunk_position, version = 9L)

subchunk_blocks_value_as_array(
  value,
  names_only = FALSE,
  extra_block = !names_only
)

subchunk_blocks_array_as_value(r)

subchunk_origins(keys)

subchunk_coords(ind, origins = subchunk_origins(names(ind)))
```

## Arguments

- x, z, dimension:

  Chunk coordinates to extract data from. `x` can also be a character
  vector of db keys.

- subchunk:

  Subchunk indexes

- db:

  A bedrockdb object.

- value:

  A SubChunkBlocks data value

- version:

  Which format of subchunk data to use

- values:

  A (named) list of SubChunkBlocks data values. If `x` is missing, the
  names of `values` will be taken as the keys.

- rawvalue:

  A raw vector

- subchunk_position:

  Optional, an integer. When reading a value, it will be used if the
  value's position attribute is missing. When writing a value, it will
  be used in place of the value's position attribute.

- names_only:

  A logical scalar. Return only the names of the blocks, ignoring block
  states.

- extra_block:

  A logical scalar. Append the extra block layer to the output
  (separated by ";"). This is mostly useful if you have waterlogged
  blocks. If the extra block is air, it will not be appended.

- r:

  A character array

- keys:

  A character vector of keys.

- ind:

  Numeric vector or a named list of numeric vectors containing indexes
  for blocks in a subchunk.

- origins:

  A matrix of subchunk origins.

## Value

`get_subchunk_blocks_value()` returns a SubChunkBlocks data value.
[`get_biomes_data()`](https://reedacartwright.github.io/rbedrock/reference/Biomes.md)
returns a named list of SubChunkBlocks data values.

## Details

The format description can be found at
<https://gist.github.com/Tomcc/a96af509e275b1af483b25c543cfbf37>.

- `get_subchunk_blocks_value()` and `get_subchunk_blocks_data()` load
  SubChunkBlocks data from `db`. `get_subchunk_blocks_value()` loads
  data for a single subchunk, and `get_subchunk_blocks_data()` loads
  data for multiple subchunks.

- `put_subchunk_blocks_value()` and `put_subchunk_blocks_data()` store
  SubChunkBlocks data into `db`.

- `write_subchunk_blocks_value()` encodes SubChunkBlocks data into a raw
  vector. `read_subchunk_blocks_value()` decodes binary SubChunkBlocks
  data.

- `subchunk_blocks_value_as_array()` converts SubChunkBlocks data into a
  character array.

- `subchunk_origins()` returns a matrix containing the block coordinate
  of the lower NW corner of subchunk keys.

- `subchunk_coords()` determines the block coordinates of blocks based
  on their array indexes and their subchunk origins.
