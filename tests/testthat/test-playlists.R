context("Video functions")
test_that("ov_video_timing works as expected", {
    skills <- c("Serve", "Reception", "Set", "Attack", "Block", "Dig", "Freeball")
    def_timing <- rep(list(c(-5, 3)), length(skills))
    def_timing[[2]] <- c(-2, 6)
    names(def_timing) <- skills
    expect_identical(ov_video_timing(), def_timing)
    def_timing$Serve <- c(-2, 1)
    def_timing$Dig <- c(-2, 2)
    expect_identical(ov_video_timing(Serve = c(-2, 1), Dig = c(-2, 2)), def_timing)
    expect_warning(ov_video_timing(1), "must be named")
})

test_that("ov_video_playlist works as expected", {
    x <- datavolley::read_dv(datavolley::dv_example_file())
    expect_error(ov_video_playlist(x$plays[10, ], x$meta), "no video for")
    x$meta$video <- data.frame(camera = NA_character_, file = "myvideo.mp4")
    expect_error(px <- ov_video_playlist(x$plays[10, ], x$meta), "x has at least one missing video_time value")
    x$plays$video_time <- seq_len(nrow(x$plays)) ## dummy values
    px <- ov_video_playlist(x$plays[10, ], x$meta)
    expect_equal(nrow(px), 1L)
    expect_named(px, c("video_src", "start_time", "duration", "type", "seamless_start_time", "seamless_duration"))
    px2 <- ov_video_playlist(x$plays[10, ], x$meta, timing = ov_video_timing_df(data.frame(skill = "Serve", phase = "Serve", start_offset = -5, duration = 8)))
    expect_equivalent(px, px2)
    expect_equivalent(px, data.frame(video_src = "myvideo.mp4", start_time = 10-5, duration = 8, type = "local", seamless_start_time = 10-5, seamless_duration = 8, stringsAsFactors = FALSE))
    px <- ov_video_playlist(x$plays[10, ], x$meta, timing = list(Set = c(0, 0)))
    expect_equal(px$start_time, 10)
    expect_equal(px$duration, 0)
    px <- ov_video_playlist(x$plays[1:10, ], x$meta, extra = "player_name")
    expect_equal(nrow(px), sum(!is.na(x$plays$skill[1:10])))
    expect_named(px, c("video_src", "start_time", "duration", "type", "seamless_start_time", "seamless_duration", "player_name"))
})

test_that("ov_video_playlist copes with full youtube URLs", {
    x <- datavolley::read_dv(datavolley::dv_example_file())
    x$meta$video <- data.frame(camera = NA_character_, file = "https://www.youtube.com/watch?v=ZzbgMiklzzZ")
    x$plays$video_time <- 1L ## dummy values
    ## should guess type correctly, should cope with factors in x$meta$video
    px <- ov_video_playlist(x$plays[10, ], x$meta)
    expect_equal(px$type, "youtube")
    expect_equal(px$video_src, "ZzbgMiklzzZ")
    x$meta$video <- data.frame(camera = NA_character_, file = "http://www.youtube.com/watch?v=ZzbgMiklzzZ", stringsAsFactors = FALSE)
    px <- ov_video_playlist(x$plays[10, ], x$meta, type = "youtube")
    expect_equal(px$video_src, "ZzbgMiklzzZ")
    x$meta$video <- data.frame(camera = NA_character_, file = "https://random.domain/something?x=blah&v=ZzbgMiklzzZ&foo=bar", stringsAsFactors = FALSE)
    px <- ov_video_playlist(x$plays[10, ], x$meta, type = "youtube")
    expect_equal(px$video_src, "ZzbgMiklzzZ")
})
