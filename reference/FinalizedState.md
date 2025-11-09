# Load and store FinalizedState data

FinalizedState data (tag 54) holds a number which indicates a chunk's
state of generation.

## Usage

``` r
get_finalized_state_value(x, z, dimension, db = default_db())

get_finalized_state_data(x, z, dimension, db = default_db())

put_finalized_state_value(value, x, z, dimension, db = default_db())

put_finalized_state_data(values, x, z, dimension, db = default_db())
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

  A (named) vector of FinalizedState values. If `x` is missing, the
  names of `values` will be taken as the keys.

## Value

`get_finalized_state_value()` returns a FinalizedState data value.
`get_finalized_state_data()` returns a named vector of FinalizedState
data values.

## Details

FinalizedState data contains the following information.

|       |                   |                                       |
|-------|-------------------|---------------------------------------|
| Value | Name              | Description                           |
| 0     | NeedsInstaticking | Chunk needs to be ticked              |
| 1     | NeedsPopulation   | Chunk needs to be populated with mobs |
| 2     | Done              | Chunk generation is fully complete    |

- `get_finalized_state_value()` and `get_finalized_state_data()` load
  FinalizedState data from `db`. `get_finalized_state_value()` loads
  data for a single chunk, and `get_finalized_state_data()` loads data
  for multiple chunks.

- `put_finalized_state_value()` and `put_finalized_state_data()` store
  FinalizedState data into `db`.
