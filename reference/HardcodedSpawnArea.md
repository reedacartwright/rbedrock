# Read and write HardcodedSpawnArea (HSA) data

**\[superseded\]**

HardcodedSpawnArea (HSA) data (tag 57) stores information about any
structure spawning locations in a chunk. An HSA is defined by a bounding
box that specifies the location of an HSA in a chunk and a tag that
specifies the type:

|       |                 |
|-------|-----------------|
| Value | Name            |
| 1     | NetherFortress  |
| 2     | SwampHut        |
| 3     | OceanMonument   |
| 4     | Removed Cat HSA |
| 5     | PillagerOutpost |
| 6     | Removed Cat HSA |

As of version 1.21.10, HSA data is no longer saved to the world db.

- `get_hsa_value()` and `get_hsa_data()` load HardcodedSpawnArea data
  from `db`. `get_hsa_value()` loads data for a single chunk, and
  `get_hsa_data()` loads data for multiple chunks.

- `put_hsa_value()` and `put_hsa_data()` store HardcodedSpawnArea data
  into `db`.

## Usage

``` r
get_hsa_value(x, z, dimension, db = default_db())

get_hsa_data(x, z, dimension, db = default_db())

put_hsa_value(value, x, z, dimension, db = default_db())

put_hsa_data(values, x, z, dimension, db = default_db())
```

## Arguments

- x, z, dimension:

  Chunk coordinates to extract data from. `x` can also be a character
  vector of db keys.

- db:

  A bedrockdb object.

- value:

  A HardcodedSpawnArea value

- values:

  A (named) vector of HardcodedSpawnArea values. If `x` is missing, the
  names of `values` will be taken as the keys.

## Value

`get_hsa_value()` returns a HardcodedSpawnArea data value.
`get_hsa_data()` returns a named vector of HardcodedSpawnArea data
values. HardcodedSpawnArea data values are integer matrices.
