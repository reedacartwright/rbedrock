# Convert between key types.

rbedrock represents database keys two different ways. `chrkeys` are a
human-readable format understood by most functions. `rawkeys` are used
internally, by the methods of `bedrockdb` objects and
`bedrock_leveldb_*` functions.

## Usage

``` r
chrkeys_to_rawkeys(keys)

chrkeys_to_rawkeys_1(keys)

rawkeys_to_chrkeys(keys)
```

## Arguments

- keys:

  a character vector of chrkeys or a list or rawkeys

## Value

`chrkeys_to_rawkeys()` returns a list of raw vectors.

        `rawkeys_to_chrkeys()` returns a character vector.

        `chrkeys_to_rawkeys_1()` returns a raw vector.
