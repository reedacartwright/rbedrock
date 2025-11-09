# Read and write Data3D data

Data3D data (tag 43) stores information about surface heights and biomes
in a chunk.

## Usage

``` r
get_data3d_value(x, z, dimension, db = default_db())

get_data3d_data(x, z, dimension, db = default_db())

put_data3d_value(value, x, z, dimension, db = default_db())

put_data3d_data(values, x, z, dimension, db = default_db())

read_data3d_value(rawvalue)

write_data3d_value(value)
```

## Arguments

- x, z, dimension:

  Chunk coordinates to extract data from. `x` can also be a character
  vector of db keys.

- db:

  A bedrockdb object.

- value:

  A Data3D value.

- values:

  A (named) list of Data3D values. If `x` is missing, the names of
  `values` will be taken as the keys.

- rawvalue:

  A raw vector.

## Value

`get_data3d_value()` returns a Data3D value. `get_data3d_data()` returns
a named list of Data3D values. Data3D values are lists containing two
elements. The `height_map` element is a 16x16 matrix containing height
data. The `biome_map` element is a 16x384x16 array containing biome
data.

## Details

- `get_data3d_value()` and `get_data3d_data()` load Data3D data from
  `db`. `get_data3d_value()` loads data for a single chunk, and
  `get_data3d_data()` loads data for multiple chunks.

- `put_data3d_value()` and `put_data3d_data()` store Data3D data into
  `db`.

- `write_data3d_value()` encodes Data3D data into a raw vector.
  `read_data3d_value()` decodes binary Data3D data.

## See also

Data2D
