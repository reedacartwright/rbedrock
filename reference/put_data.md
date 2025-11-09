# Write values to a bedrockdb.

Write values to a bedrockdb.

## Usage

``` r
put_data(values, keys, db = default_db(), writeoptions = NULL)

put_value(value, key, db = default_db(), writeoptions = NULL)
```

## Arguments

- values:

  A list of raw values.

- keys:

  A character vector of keys.

- db:

  A `bedrockdb` object

- writeoptions:

  A `bedrock_leveldb_writeoptions` object

- value:

  A raw vector that contains the information to be written.

- key:

  A key that will be used to store data.

## Value

An invisible copy of `db`.
