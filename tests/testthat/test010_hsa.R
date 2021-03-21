dbpath <- rbedrock_example_world("example1.mcworld")
db <- bedrockdb(dbpath)

test_that("get_hsa can read HSA data.", {
    hsa <- get_hsa(db)
    expect_equal(nrow(hsa), 4L)
    expect_s3_class(hsa, "data.frame")
    expect_named(hsa, c("type", "key", "x1", "y1", "z1",
        "x2", "y2", "z2", "tag", "xspot", "yspot", "zspot",
        "dimension"))
    expect_equal(hsa$xspot, c(584L,584L,592L,592L))
    expect_equal(hsa$yspot, c(82L,82L,82L,82L))
    expect_equal(hsa$zspot, c(248L,256L,248L,256L))
    expect_equal(hsa$type, rep("PillagerOutpost",4L))
})

test_that("put_hsa can write HSA data.", {
    hsa <- put_hsa(db, 295, 64, 100, 300, 70, 110, 2L, 0L)
    expect_equal(hsa, matrix(c(295L,64L,100L,300L,70L,110L,2L), 1L, 7L))
    new_hsa <- get_hsa(db, "@18:6:0:57")
    expect_equal(nrow(new_hsa), 1L)
    expect_equal(new_hsa$xspot, (295+300+1) %/% 2)
    expect_equal(new_hsa$yspot, 70 - 4)
    expect_equal(new_hsa$zspot, (110+100+1) %/% 2)
    expect_equal(new_hsa$type, "SwampHut")
})

test_that("put_hsa can merge HSA data.", {
    hsa <- put_hsa(db, 295, 64, 100, 300, 70, 120, 3L, 0L)
    expect_equal(hsa, matrix(c(295L,64L,100L,300L,70L,111L,3L,
                               295L,64L,112L,300L,70L,120L,3L),
                        2L, 7L, byrow=TRUE))
    new_hsa <- get_hsa(db, c("@18:6:0:57","@18:7:0:57"))
    expect_equal(nrow(new_hsa), 3L)
    expect_equal(new_hsa$xspot, c(298, 298, 298))
    expect_equal(new_hsa$yspot, c(66,69,69))
    expect_equal(new_hsa$zspot, c(105,106,116))
    expect_equal(new_hsa$type, c("SwampHut","OceanMonument","OceanMonument"))
})

# clean up
close(db)
unlink(dirname(dbpath), recursive=TRUE, expand = FALSE)
