# Create an NBT value

The Named Binary Tag (NBT) format is used by Minecraft for various data
types. An NBT value holds a 'payload' of data and a 'tag' indicating the
type of data held.

## Usage

``` r
unnbt(x)

nbt_compound(...)

nbt_byte(x)

nbt_byte_array(x)

nbt_byte_list(x)

nbt_byte_array_list(x)

nbt_short(x)

nbt_short_list(x)

nbt_int(x)

nbt_int_array(x)

nbt_int_list(x)

nbt_int_array_list(x)

nbt_float(x)

nbt_float_list(x)

nbt_double(x)

nbt_double_list(x)

nbt_long(x)

nbt_long_array(x)

nbt_long_list(x)

nbt_long_array_list(x)

nbt_string(x)

nbt_raw_string(x)

nbt_string_list(x)

nbt_raw_string_list(x)

nbt_empty_list(x = list())

nbt_compound_list(x)

nbt_nested_list(x)
```

## Arguments

- x:

  An nbt payload.

- ...:

  NBT objects, possibly named.

## Details

- `nbt_*()` family of functions create nbt data types.

- `unnbt()` recursively strips NBT metadata from an NBT value.
