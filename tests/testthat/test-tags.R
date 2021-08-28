context("video tags")
test_that("tag functions work", {
    skip_if_not(ovideo:::ov_ffmpeg_ok(), "ffmpeg executable not found")
    newfile <- ov_set_video_meta(ov_example_video(), comment = "A comment")
    tgs <- ov_get_video_meta(newfile)
    expect_identical(tgs$comment, "A comment")
    unlink(newfile)

    ## using arbitrary tag names
    newfile <- ov_set_video_meta(ov_example_video(), my_custom_tag = "Another comment", movflags = TRUE)
    tgs <- ov_get_video_meta(newfile)
    expect_identical(tgs$my_custom_tag, "Another comment")
    unlink(newfile)

    ## but this will fail without movflags
    newfile <- ov_set_video_meta(ov_example_video(), my_custom_tag = "Another comment")
    tgs <- ov_get_video_meta(newfile)
    expect_null(tgs$my_custom_tag)
    unlink(newfile)
})
