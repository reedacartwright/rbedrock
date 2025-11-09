# Read and write Biomes data

Biomes data is stored as the second map in the
[Data3D](https://reedacartwright.github.io/rbedrock/reference/Data3D.md)
data (tag 43).

## Usage

``` r
get_biomes_value(x, z, dimension, db = default_db(), return_names = TRUE)

get_biomes_data(x, z, dimension, db = default_db(), return_names = TRUE)

put_biomes_value(
  value,
  x,
  z,
  dimension,
  db = default_db(),
  missing_height = 0L
)

put_biomes_data(
  values,
  x,
  z,
  dimension,
  db = default_db(),
  missing_height = 0L
)

biome_id(value)

biome_name(value)
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

  An array of biome ids.

- missing_height:

  if there is no existing height data, use this value for the chunk.

- values:

  A (named) list of Biomes data values. If `x` is missing, the names of
  `values` will be taken as the keys.

## Value

`get_biomes_value()` returns a Biomes data value. `get_biomes_data()`
returns a named list of Biomes data values. Biomes data values are
16x384x16 arrays containing biome data.

## Details

- `get_biomes_value()` and `get_biomes_data()` load Biomes data from
  `db`. `get_biomes_value()` loads data for a single chunk, and
  `get_biomes_data()` loads data for multiple chunks.

- `put_biomes_value()` and `put_biomes_data()` store biomes data into
  `db`.

## See also

LegacyBiomes
