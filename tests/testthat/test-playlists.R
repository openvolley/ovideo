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

test_that("playlist conversion to m3u works", {
    x <- datavolley::read_dv(datavolley::dv_example_file())
    x$meta$video <- data.frame(camera = NA_character_, file = "myvideo.mp4")
    x$plays$video_time <- seq_len(nrow(x$plays)) ## dummy values
    px <- ov_video_playlist(x$plays[10:11, ], x$meta)
    expect_equal(nrow(px), 2L)
    m3u <- ov_playlist_to_vlc(px, seamless = FALSE, no_paths = TRUE)
    expect_equal(readLines(m3u), c("#EXTM3U", "#EXTINF:8,. ", "#EXTVLCOPT:start-time=5", "#EXTVLCOPT:stop-time=13", "myvideo.mp4", "", "", "#EXTM3U", "#EXTINF:8,. ", "#EXTVLCOPT:start-time=6", "#EXTVLCOPT:stop-time=14", "myvideo.mp4", "", ""))
    m3u <- ov_playlist_to_vlc(px, seamless = TRUE, no_paths = TRUE)
    expect_equal(readLines(m3u), c("#EXTM3U", "#EXTINF:9,", "#EXTVLCOPT:start-time=5", "#EXTVLCOPT:stop-time=14", "myvideo.mp4", "", ""))
    unlink(m3u)
})

test_that("playlist conversion to HTML works", {
    skip_if_not(rmarkdown::pandoc_available(version = "1.12.3"))
    ply <- dplyr::tribble(~video_src, ~start_time, ~duration, ~type, ~seamless_start_time, ~seamless_duration, ~subtitle, ~subtitleskill, ~home_team, ~visiting_team, ~video_time, ~code, ~set_number, ~home_team_score, ~visiting_team_score, ~file,
                          "NisDpPFPQwU",  589, 8, "youtube",  589, 8,   "Set 1 - POLAND 2017 9 - 7 Iran 2017", "Michal KUBIAK - Pipe : - POLAND 2017 Iran 2017", "Poland", "Iran",  594, "*13AM-XP~83~H1", 1,  9,  7, NA_character_,
                          "NisDpPFPQwU", 1036, 8, "youtube", 1036, 8, "Set 1 - POLAND 2017 17 - 10 Iran 2017", "Michal KUBIAK - Pipe : # POLAND 2017 Iran 2017", "Poland", "Iran", 1041, "*13AM#XP~86~H1", 1, 17, 10, NA_character_)
    f <- ov_playlist_to_html(ply)
    expect_true(file.exists(f))
    unlink(f)
})
