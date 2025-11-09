# Random Number Seeds for Minecraft

Minecraft uses several different kind of seeds during world generation
and gameplay.

## Usage

``` r
bedrock_random_create_seed(x, z, a, b, salt, type)
```

## Arguments

- x, z:

  chunk coordinates

- a, b:

  seed parameters

- salt:

  seed parameter

- type:

  which seed type to use

## Details

`bedrock_random_create_seed()` constructs a seed using the formulas type
1: `x*a ^ z*b ^ salt`, type 2: `x*a + z*b + salt`, and type 3:
`x*a + z*b ^ salt`.

## Examples

``` r
# identify slime chunks
g <- expand.grid(x=1:10, z=1:10)
is_slime_chunk <- mapply(g$x, g$z, FUN = function(x,z) {
  seed <- bedrock_random_create_seed(x,z,0x1f1f1f1f,1,0,type=1)
  bedrock_random_seed(seed)
  bedrock_random_get_uint(1,10) == 0
})
```
