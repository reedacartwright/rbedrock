# Read and write Data2D data

**\[superseded\]**

Data2D data (tag 45) stores information about surface heights and biomes
in a chunk. Data2D data is 768 bytes long and consists of a 256 int16s
(heights) followed by 256 uint8s (biomes). The game no longer uses it
after 1.18.

- `get_data2d_value()` and `get_data2d_data()` load Data2D data from
  `db`. `get_data2d_value()` loads data for a single chunk, and
  `get_data2d_data()` loads data for multiple chunks.

- `put_data2d_value()` and `put_data2d_data()` store Data2D data into
  `db`.

- `write_data2d_value()` encodes Data2D data into a raw vector.
  `read_data2d_value()` decodes binary Data2D data.

## Usage

``` r
get_data2d_data(x, z, dimension, db = default_db())

get_data2d_value(x, z, dimension, db = default_db())

put_data2d_data(values, x, z, dimension, db = default_db())

put_data2d_value(value, x, z, dimension, db = default_db())

read_data2d_value(rawvalue)

write_data2d_value(value)
```

## Arguments

- x, z, dimension:

  Chunk coordinates to extract data from. `x` can also be a character
  vector of db keys.

- db:

  A bedrockdb object.

- values:

  A (named) list of Data2D values. If `x` is missing, the names of
  `values` will be taken as the keys.

- value:

  A Data2D value.

- rawvalue:

  A raw vector.

## Value

`get_data2d_value()` returns a Data2D value. `get_data2d_data()` returns
a named list of Data2D values. Data2D values are lists containing two
elements. The `height_map` element is a 16x16 matrix containing height
data. The `biome_map` element is a 16x16 matrix containing biome data.

## See also

Data3D
