dbpath <- rbedrock_example_world("example2.mcworld")
db <- bedrockdb(dbpath)

as_raw <- function(...) {
    as.raw(c(...))
}

test_that("Data3D is chunk tag 43", {
    expect_equal(chunk_tag_int("Data3D"), 43L)
    expect_equal(chunk_tag_str(43L), "Data3D")
})

test_that("Reading and Writing Data3D data works", {
    dat <- get_value(db, "chunk:0:0:0:43")

    val <- read_data3d_value(dat)
    expect_named(val, c("height_map", "biome_map"))
    expect_type(val$height_map, "integer")
    expect_type(val$biome_map, "integer")
    expect_equal(val$biome_map, array(186, c(16, 384, 16)))

    # Rbedrock will compress biomes as much as possible when
    # writing. Update expected value.
    dat2 <- dat[1:517]
    dat2 <- c(dat2, as_raw(rep(0xff, 23)))
    expect_equal(dat2, write_data3d_value(val))

    expect_equal(dat2, write_data3d_value(val$height_map, val$biome_map))
})

test_that("Writing Data3D recycles values", {
    r <- write_data3d_value(0, 10)
    val <- read_data3d_value(r)

    expect_equal(val$height_map, array(0, c(16, 16)))
    expect_equal(val$biome_map, array(10, c(16, 16 * 24, 16)))

    r <- write_data3d_value(0, 1:256)
    val <- read_data3d_value(r)
    expect_equal(val$biome_map[1, , 1], rep(1, 384))
    expect_equal(val$biome_map[16, , 16], rep(256, 384))
    expect_equal(val$biome_map[7, , 13], rep(199, 384))
    expect_equal(val$biome_map[13, , 7], rep(109, 384))

    expect_equal(val$biome_map[, 1, ], array(1:256, c(16, 16)))
    expect_equal(val$biome_map[, 64, ], array(1:256, c(16, 16)))
    expect_equal(val$biome_map[, 100, ], array(1:256, c(16, 16)))
    expect_equal(val$biome_map[, 384, ], array(1:256, c(16, 16)))
})

close(db)
fs::dir_delete(dbpath)
