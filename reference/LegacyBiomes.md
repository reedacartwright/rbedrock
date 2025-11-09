# Read and write legacy biomes data

**\[superseded\]**

Legacy Biomes data is stored as the second map in the
[Data2D](https://reedacartwright.github.io/rbedrock/reference/Data2D.md)
data (tag 45).

- `get_legacy_biomes_value()` and `get_legacy_biomes_data()` load legacy
  biomes data from `db`. `get_legacy_biomes_value()` loads data for a
  single chunk, and `get_legacy_biomes_data()` loads data for multiple
  chunks.

- `put_legacy_biomes_value()` and `put_legacy_biomes_data()` store
  legacy biomes data into `db`.

## Usage

``` r
get_legacy_biomes_value(
  x,
  z,
  dimension,
  db = default_db(),
  return_names = TRUE
)

get_legacy_biomes_data(x, z, dimension, db = default_db(), return_names = TRUE)

put_legacy_biomes_value(
  value,
  x,
  z,
  dimension,
  db = default_db(),
  missing_height = 0L
)

put_legacy_biomes_data(
  values,
  x,
  z,
  dimension,
  db = default_db(),
  missing_height = 0L
)
```

## Arguments

- x, z, dimension:

  Chunk coordinates to extract data from. `x` can also be a character
  vector of db keys.

- db:

  A bedrockdb object.

- return_names:

  return biome names instead of biome ids.

- value:

  A 16x16 matrix of biome ids.

- missing_height:

  if there is no existing height data, use this value for the chunk.

- values:

  A (named) list of LegacyBiomes values. If `x` is missing, the names of
  `values` will be taken as the keys.

## Value

`get_legacy_biomes_value()` returns a legacy biomes data value.
`get_legacy_biomes_data()` returns a named list of legacy biomes data
values. Legacy biomes data values are 16x16 matrices containing biome
data.

## See also

Biomes
