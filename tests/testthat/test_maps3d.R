dbpath <- rbedrock_example_world("example2.mcworld")
db <- bedrockdb(dbpath)

as_raw <- function(...) {
    as.raw(c(...))
}

test_that("3DMaps is chunk tag 43", {
    expect_equal(chunk_tag_int("3DMaps"), 43L)
    expect_equal(chunk_tag_str(43L), "3DMaps")
})

test_that("Reading and Writing 3DMaps data works", {
    dat <- get_value(db, "@0:0:0:43")

    val <- read_3dmaps_value(dat)
    expect_named(val, c("height_map", "biome_map"))
    expect_type(val$height_map, "integer")
    expect_type(val$biome_map, "integer")

    expect_equal(dat, write_3dmaps_value(val))

    expect_equal(dat, write_3dmaps_value(val$height_map, val$biome_map))
})

test_that("Writing 3DMaps recycles values", {
    r <- write_3dmaps_value(0, 10)
    val <- read_3dmaps_value(r)

    expect_equal(val$height_map, array(0, c(16,16)))
    expect_equal(val$biome_map, array(10, c(16,16*24,16)))

    r <- write_3dmaps_value(0, 1:256)
    val <- read_3dmaps_value(r)
    expect_equal(val$biome_map[,1,], array(1:256, c(16,16)))
    expect_equal(val$biome_map[,64,], array(1:256, c(16,16)))
    expect_equal(val$biome_map[,100,], array(1:256, c(16,16)))
    expect_equal(val$biome_map[,384,], array(1:256, c(16,16)))
})

close(db)
fs::dir_delete(dbpath)
