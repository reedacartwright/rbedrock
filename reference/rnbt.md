# Raw Named Binary Tag Format

`rnbt` is a recursive, intermediate data structure that closely
resembles how NBT data is encoded.

## Usage

``` r
read_rnbt(rawvalue, format = c("little", "big", "network", "network_big"))

read_rnbt_once(rawvalue, format = c("little", "big", "network", "network_big"))

write_rnbt(x, format = c("little", "big", "network", "network_big"))

from_rnbt(x)

to_rnbt(x)
```

## Arguments

- rawvalue:

  A `raw` vector

- format:

  A character string specifying which binary NBT format to use.

- x:

  An object

## Details

- `read_rnbt()` converts a `raw` vector to `rnbt` data.

- `write_rnbt()` converts `rnbt` data to a `raw` vector

- `from_rnbt()` converts `rnbt` data to `nbt` data.

- `to_rnbt()` converts `nbt` data to `rnbt` data.
