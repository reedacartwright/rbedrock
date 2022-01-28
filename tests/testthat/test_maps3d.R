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
})


close(db)
fs::dir_delete(dbpath)
