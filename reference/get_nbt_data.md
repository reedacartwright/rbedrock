# Read and Write NBT Data

The Named Binary Tag (NBT) format is used by Minecraft for various data
types.

## Usage

``` r
get_nbt_data(keys, db = default_db(), readoptions = NULL, simplify = TRUE)

get_nbt_value(key, db = default_db(), readoptions = NULL, simplify = TRUE)

put_nbt_data(values, keys, db = default_db(), writeoptions = NULL)

put_nbt_value(value, key, db = default_db(), writeoptions = NULL)

read_nbt(
  rawvalue,
  format = c("little", "big", "network", "network_big"),
  simplify = TRUE
)

read_nbt_data(
  rawdata,
  format = c("little", "big", "network", "network_big"),
  simplify = TRUE
)

write_nbt(value, format = c("little", "big", "network", "network_big"))

write_nbt_data(values, format = c("little", "big", "network", "network_big"))
```

## Arguments

- keys:

  A character vector of keys

- db:

  A `bedrockdb` object

- readoptions:

  A `bedrock_leveldb_readoptions` object

- simplify:

  If TRUE, simplifies a list containing a single unnamed nbt value.

- key:

  A single key

- values:

  A list of values. Optionally named.

- writeoptions:

  A `bedrock_leveldb_writeoptions` object

- value:

  An nbt object or a list of nbt objects

- rawvalue:

  A `raw` vector

- format:

  A character string specifying which binary NBT format to use.

- rawdata:

  A list of `raw` vectors

## Details

`get_nbt_data()` and `get_nbt_value()` load nbt-formatted data from `db`
and parses it.

`put_nbt_data()` and `put_nbt_value()` store nbt data into `db` in
binary form.

`read_nbt()` reads NBT data from a `raw` vector.

`read_nbt_data()` calls `read_nbt()` on each element of a list.

`write_nbt()` encodes NBT data into a `raw` vector.

`write_nbt_data()` calls `write_nbt()` on each element of a list.
