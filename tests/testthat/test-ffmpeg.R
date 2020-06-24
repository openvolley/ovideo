context("ffmpeg-based functions")
test_that("ffmpeg functions work", {
    skip_if_not(ovideo:::ov_ffmpeg_exists(), "ffmpeg executable not found")
    chk <- ov_video_frame(system.file("extdata/2019_03_01-KATS-BEDS-clip.mp4", package = "ovideo"), t = 2+11/30, format = "png")
    expect_equal(file.size(system.file("extdata/2019_03_01-KATS-BEDS-frame.png", package = "ovideo")), file.size(chk))
    unlink(chk)
})
