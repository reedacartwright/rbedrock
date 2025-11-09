# Convert block data from nbt format to a string

Block data is stored in nbt format, which includes a block's name and
properties. `blocks_str()` converts the nbt data into strings with the
following format: `blockname@prop1=val1@prop2=val2`. Blocks can have
zero or more properties. `blocks_nbt()` does the inverse operation.

## Usage

``` r
blocks_str(x, names_only = FALSE)

blocks_nbt(x)
```

## Arguments

- x:

  block data, either as a list of nbt values or a vector of strings.

- names_only:

  A logical scalar. Return only the names of the blocks, ignoring block
  properties.
