#' Create video playlist
#'
#' @param x data.frame: a datavolleyplays object. Normally this will be a selected subset of the `plays` component of a datavolley object (i.e. a selected set of actions that you want the video playlist to contain)
#' @param meta list: either the `meta` component of a datavolley object, or a list of such objects, or a data.frame with the columns "match_id" and "video_src". Entries in `video_src` should be paths or URLs to the video file associated with the corresponding `match_id`
#' @param type string: currently "youtube", "twitch", or "local". If `type` is not specified as a parameter, and `meta` is a data.frame, then `type` can be provided as a column in `meta`. Alternatively, if `meta` is a `meta` component of a datavolley object, or a list of such objects, then `type` will be assumed to be "local". Note that a single playlist can't mix types, all entries must be of the same type
#' @param timing list: the relative timing for each skill type, either a named list as returned by [ov_video_timing()] or a data.frame as returned by [ov_video_timing_df()]. See [ov_video_timing()] for further details
#' @param extra_cols character: names of additional columns from `x` to include in the returned data frame
#' @param normalize_paths logical: if `TRUE`, apply `normalizePath` to local file paths. This will e.g. expand the tilde in paths like "~/path/to/video.mp4"
#'
#' @return A data.frame with columns `src`, `start_time`, `duration`, plus any extras specified in `extra_cols`
#'
#' @examples
#' ## read data file
#' x <- datavolley::dv_read(datavolley::dv_example_file())
#' ## note that this data file has no video specified, so put a dummy value in
#' dv_meta_video(x) <- "c:\\my_video.mp4"
#'
#' ## extract play-by-play data
#' px <- datavolley::plays(x)
#' ## and put dummy video_time values in, because those are missing too
#' px$video_time <- sample.int(2e3, size = nrow(px))
#'
#' ## find pipe (XP) attacks in transition
#' px <- px[which(px$attack_code == "XP" & px$phase == "Transition"), ]
#'
#' ## create playlist
#' ply <- ov_video_playlist(px, x$meta, timing = ov_video_timing())
#'
#' ## with custom timing
#' ply <- ov_video_playlist(px, x$meta,
#'   timing = ov_video_timing_df(data.frame(skill = "Attack", phase = "Transition",
#'                               start_offset = -5, duration = 10, stringsAsFactors = FALSE)))
#'
#' @export
ov_video_playlist <- function(x, meta, type = NULL, timing = ov_video_timing(), extra_cols = NULL, normalize_paths = TRUE) {
    assert_that(is.data.frame(x))
    assert_that(has_name(x, c("video_time", "skill", "match_id")))
    assert_that(is.flag(normalize_paths), !is.na(normalize_paths))
    if (is.data.frame(meta)) {
        assert_that(has_name(meta, c("match_id", "video_src")))
        if (is.null(type)) {
            if ("type" %in% names(meta)) type <- unique(na.omit(meta$type))
        }
    } else if (is.list(meta)) {
        if ("match_id" %in% names(meta)) {
            ## this is a single metadata object
            ## make it a list of (one) object
            meta <- list(meta)
        }
        ## assume meta is a list of metadata objects
        meta <- bind_rows(lapply(meta, function(z) tibble(match_id = match_id_from_meta(z), video_src = as.character(video_file_from_meta(z)))))
        if (is.null(type)) {
            if (all(is_youtube_id(meta$video_src) | grepl("https?://.*youtube", meta$video_src, ignore.case = TRUE) | grepl("https?://youtu\\.be", meta$video_src, ignore.case = TRUE))) {
                type <- "youtube"
            } else if (all(is_twitch_video(meta$video_src))) {
                type <- "twitch"
            } else {
                type <- "local"
            }
        }
    } else {
        stop("meta is an unexpected format")
    }
    assert_that(is.string(type))
    type <- match.arg(tolower(type), c("local", "twitch", "youtube"))
    meta <- dplyr::filter(meta, .data$match_id %in% na.omit(unique(x$match_id))) ## ignore meta about matches that don't appear in our data
    if (!all(x$match_id %in% meta$match_id)) stop("x contains match_ids that do not appear in meta")
    if (any(is.na(meta$video_src))) {
        missing_vid_matches <- meta$match_id[is.na(meta$video_src)]
        stop("no video for matches with match_id: ", paste(missing_vid_matches, collapse = ", "))
    }
    ## check for NA video_time
    if (any(is.na(x$video_time))) stop("x has at least one missing video_time value")
    if (!is.null(extra_cols)) assert_that(is.character(extra_cols))
    x <- left_join(x, meta, by = "match_id")
    if (!is.data.frame(timing) && is.list(timing)) {
        ## convert timing to a data.frame
        timing <- bind_rows(lapply(names(timing), function(z) tibble(skill = z, start_offset = timing[[z]][1], duration = abs(diff(timing[[z]])))))
        jby <- "skill"
    } else if (is.data.frame(timing)) {
        ## expect at least the columns "skill", "start_offset", "duration"
        if (!all(c("skill", "start_offset", "duration") %in% names(timing))) {
            stop("the timing data.frame needs columns \"skill\", \"start_offset\", and \"duration\"")
        }
        ## any others (e.g. "phase") will be used in the join operation
        jby <- setdiff(names(timing), c("start_offset", "duration"))
    } else {
        stop("unexpected 'timing' format, should be either named list or data.frame")
    }
    x <- left_join(x, timing, by = jby)
    if (any(is.na(x$start_offset) | is.na(x$duration))) warning("some NA start_time/duration values in playlist")
    x$start_time <- x$video_time + x$start_offset
    ## cope with negative start_time values
    idx <- !is.na(x$start_time) & x$start_time < 0
    x$duration[idx] <- x$duration[idx] + x$start_time[idx] ## shorten duration
    x$start_time[idx] <- 0
    x <- x[!is.na(x$skill) & !is.na(x$duration) & x$duration >= 0, ]
    x$video_src <- as.character(x$video_src)
    if (type == "youtube") {
        ## ensure that we have youtube IDs, not e.g. full URLs
        x$video_src <- vapply(x$video_src, youtube_url_to_id, FUN.VALUE = "", USE.NAMES = FALSE)
    } else if (type == "twitch") {
        ## ditto for twitch
        x$video_src <- vapply(x$video_src, twitch_url_to_id, FUN.VALUE = "", USE.NAMES = FALSE)
    }
    x$type <- type
    if (normalize_paths) {
        local_srcs <- which(x$type == "local" & !grepl("^https?://", x$video_src, ignore.case = TRUE))
        x$video_src[local_srcs] <- normalizePath(x$video_src[local_srcs], mustWork = FALSE)
    }
    ## add timings for seamless transitions
    x <- add_seamless_timings(x)
    x[, c("video_src", "start_time", "duration", "type", "seamless_start_time", "seamless_duration", extra_cols)]
}

#' Create video playlist per point_id
#'
#' @param x data.frame: a datavolleyplays object. Normally this will be a selected subset of the `plays` component of a datavolley object (i.e. a selected set of actions that you want the video playlist to contain)
#' @param meta list: either the `meta` component of a datavolley object, or a list of such objects, or a data.frame with the columns "match_id" and "video_src". Entries in `video_src` should be paths or URLs to the video file associated with the corresponding `match_id`
#' @param type string: currently "youtube", "twitch", or "local". If `type` is not specified as a parameter, and `meta` is a data.frame, then `type` can be provided as a column in `meta`. Alternatively, if `meta` is a `meta` component of a datavolley object, or a list of such objects, then `type` will be assumed to be "local"
#' @param extra_cols character: names of additional columns from `x` to include in the returned data frame
#' @param normalize_paths logical: if `TRUE, apply `normalizePath` to local file paths. This will e.g. expand the tilde in paths like "~/path/to/video.mp4"
#'
#' @return A data.frame with columns `src`, `start_time`, `duration`, plus any extras specified in `extra_cols`
#'
#' @export
ov_video_playlist_pid <- function(x, meta, type = NULL, extra_cols = NULL, normalize_paths = TRUE) {
    assert_that(is.data.frame(x))
    assert_that(has_name(x, c("video_time", "skill", "match_id")))
    assert_that(is.flag(normalize_paths), !is.na(normalize_paths))
    if (is.data.frame(meta)) {
        assert_that(has_name(meta, c("match_id", "video_src")))
        if (is.null(type)) {
            if ("type" %in% names(meta)) type <- unique(na.omit(meta$type))
        }
    } else if (is.list(meta)) {
        if ("match_id" %in% names(meta)) {
            ## this is a single metadata object
            ## make it a list of (one) object
            meta <- list(meta)
        }
        ## assume meta is a list of metadata objects
        meta <- bind_rows(lapply(meta, function(z) tibble(match_id = match_id_from_meta(z), video_src = as.character(video_file_from_meta(z)))))
        if (is.null(type)) {
            if (all(is_youtube_id(meta$video_src) | grepl("https?://.*youtube", meta$video_src, ignore.case = TRUE) | grepl("https?://youtu\\.be", meta$video_src, ignore.case = TRUE))) {
                type <- "youtube"
            } else if (all(is_twitch_video(meta$video_src))) {
                type <- "twitch"
            } else {
                type <- "local"
            }
        }
    } else {
        stop("meta is an unexpected format")
    }
    assert_that(is.string(type))
    type <- match.arg(tolower(type), c("local", "twitch", "youtube"))
    x <- x[!is.na(x$skill), ]
    if (!all(x$match_id %in% meta$match_id)) stop("x contains match_ids that do not appear in meta")
    if (any(is.na(meta$video_src))) {
        missing_vid_matches <- meta$match_id[is.na(meta$video_src)]
        stop("no video for matches with match_id: ", paste(missing_vid_matches, collapse = ", "))
    }
    if (!is.null(extra_cols)) assert_that(is.character(extra_cols))
    x <- left_join(x, meta, by = "match_id")

    timing <- ungroup(dplyr::summarize(group_by(x, .data$match_id, .data$point_id), start_time = min(.data$video_time, na.rm = TRUE), duration = diff(range(.data$video_time, na.rm = TRUE)) + 5))
    timing$start_time <- timing$start_time - 5 ## this is the default timing offset for serves, but should probably make this parameterizable
    x <- left_join(x, timing, by = c("match_id", "point_id"))
    ## TODO check for NA video_time
    x$video_src <- as.character(x$video_src)
    if (type == "youtube") {
        ## ensure that we have youtube IDs, not e.g. full URLs
        x$video_src <- vapply(x$video_src, youtube_url_to_id, FUN.VALUE = "", USE.NAMES = FALSE)
    } else if (type == "twitch") {
        ## ditto for twitch
        x$video_src <- vapply(x$video_src, twitch_url_to_id, FUN.VALUE = "", USE.NAMES = FALSE)
    }
    x$type <- type
    if (normalize_paths) {
        local_srcs <- which(x$type == "local" & !grepl("^https?://", x$video_src, ignore.case = TRUE))
        x$video_src[local_srcs] <- normalizePath(x$video_src[local_srcs], mustWork = FALSE)
    }
    add_seamless_timings(dplyr::distinct(x[, c("video_src", "start_time", "duration", "type", extra_cols)]))
}


video_file_from_meta <- function(z) {
    out <- z$meta$video
    if (is.null(out)) out <- z$video
    if (is.null(out)) {
        NA_character_
    } else if (nrow(out) < 1) {
        NA_character_
    } else if (nrow(out) > 1) {
        warning("multiple video files found")
        NA_character_
    } else {
        out$file
    }
}

match_id_from_meta <- function(z) {
    out <- z$meta$match_id
    if (is.null(out)) out <- z$match_id
    if (is.null(out) || !nzchar(out)) {
        NA_character_
    } else {
        out
    }
}

add_seamless_timings <- function(x) {
    x <- mutate(x, end_time = .data$start_time + .data$duration)
    x <- mutate(group_by(x, .data$video_src),
                ## a clip overlaps with its preceding one if the start time of clip i sits within the time period of clip (i-1)
                ## but if clip i starts *before* clip (i-1) then they do not overlap (cannot be played seamlessly) - maybe a randomly-ordered playlist
                ## remember though that e.g. serve and reception will have the *same* start time. Also allow a bit of margin, so start_time >= (lag(start_time - 1))
                overlap = dplyr::n() > 1 & .data$start_time <= lag(.data$end_time) & .data$start_time >= lag(.data$start_time - 1),
                overlap = case_when(is.na(.data$overlap) ~ FALSE, TRUE ~ .data$overlap), ## TRUE means that this event overlaps with previous
                ## may be better to calculate overlap in terms of point_id and/or team_touch_id?
                seamless_start_time = pmin(.data$video_time, case_when(is.na(lag(.data$end_time)) ~ .data$start_time, TRUE ~ (lag(.data$end_time) + .data$start_time)/2)),
                seamless_start_time = case_when(.data$overlap ~ .data$seamless_start_time, TRUE ~ .data$start_time),
                seamless_end_time = case_when(lead(.data$overlap) ~ lead(.data$seamless_start_time), TRUE ~ .data$end_time),
                seamless_duration = .data$seamless_end_time - .data$seamless_start_time)
    x <- dplyr::select(ungroup(x), -"end_time", -"seamless_end_time")
    if (any(x$seamless_duration < 0, na.rm = TRUE)) {
        warning("seamless durations < 0, needs checking")
        x$seamless_duration[which(x$seamless_duration < 0)] <- 0
    }
    x
}

#' Timing to use when creating video playlist
#'
#' By default, all skills except reception have a timing of `c(-5, 3)`, meaning that the video clip will start 5 seconds before the recorded time of the event and end 3 seconds after its recorded time. Reception has a timing of `c(-2, 6)` (because reception usually has the same timestamp as the serve)
#'
#' `ov_video_timing_df` accepts and returns a data.frame rather than a named list. The data.frame format also allows timings to be differentiated by play phase ("Reception" vs "Transition").
#'
#' @param ... : named parameters that will override the defaults. Each parameter should be a two-element numeric vector
#' @param x data.frame: a data.frame of timings that will override the defaults, with columns `skill`, `phase`, `start_offset` (start offset in seconds, default = -5), and `duration` (duration in seconds, default = 8)
#'
#' @return For `ov_video_timing` a named list, with names corresponding to skills ("Serve", "Reception", etc). For `ov_video_timing_df`, a data.frame with columns `skill`, `phase`, `start_offset`, and `duration`
#'
#' @seealso [ov_video_playlist()]
#'
#' @examples
#'
#' ## defaults
#' ov_video_timing()
#'
#' ## with different settings for serve and reception
#' ov_video_timing(serve = c(-2, 2), reception = c(-3, 1))
#'
#' ## as data.frame
#' ov_video_timing_df(data.frame(skill = "Set", phase = "Transition",
#'                               start_offset = -5, duration = 10))
#'
#' @export
ov_video_timing <- function(...) {
    skills <- c("Serve", "Reception", "Set", "Attack", "Block", "Dig", "Freeball")
    out <- rep(list(c(-5, 3)), length(skills))
    out[[2]] <- c(-2, 6) ## reception
    names(out) <- skills
    ## override with any user-specified parms
    user <- list(...)
    if (length(user) > 0 && is.null(names(user))) {
        warning("inputs to ov_video_timing must be named")
    } else {
        if (!all(nzchar(names(user)))) warning("all inputs to ov_video_timing must be named")
        user <- user[nzchar(names(user))]
        ## Convert To Title Case
        names(user) <- paste0(toupper(substr(names(user), 1, 1)), tolower(substr(names(user), 2, nchar(names(user)))))
        for (usr in names(user)) out[[usr]] <- user[[usr]]
    }
    out
}

#' @export
#' @rdname ov_video_timing
ov_video_timing_df <- function(x) {
    ## defaults
    def <- tibble(skill = c("Serve", "Reception", "Set", "Set", "Attack", "Attack", "Block", "Block", "Dig", "Freeball", "Freeball"),
                  phase = c("Serve", "Reception", "Reception", "Transition", "Reception", "Transition", "Reception", "Transition", "Transition", "Reception", "Transition"),
                  start_offset = rep(-5, 11),
                  duration = rep(8, 11))
    def$start_offset[2] <- -2 ## reception
    ## override with any user-specified parms
    if (!missing(x) && !is.null(x)) {
        if (!is.data.frame(x) || !all(c("skill", "phase", "start_offset", "duration") %in% names(x))) {
            stop("x should be a data.frame with columns \"skill\", \"phase\", \"start_offset\", and \"duration\"")
        }
        if (is.factor(x$skill)) x$skill <- as.character(x$skill)
        x$skill <- str_first_upper(x$skill)
        if (!all(x$skill %in% def$skill)) stop("x contains unexpected skill values")
        if (is.factor(x$phase)) x$phase <- as.character(x$phase)
        x$phase <- str_first_upper(x$phase)
        if (!all(x$phase %in% def$phase)) stop("x contains unexpected phase values")
        def <- dplyr::anti_join(def, x, by = c("skill", "phase"))
        dplyr::bind_rows(x, def)
    } else {
        def
    }
}

#' Merge two video timing dataframes
#'
#' @param x data.frame: video timings to use
#' @param default data.frame: default timings to use, for anything not provided in `x`
#'
#' @return A data.frame
#'
#' @seealso [ov_video_timing_df()]
#'
#' @examples
#'
#' my_timings <- data.frame(skill = "Attack", phase = "Reception", start_offset = 0)
#' ov_merge_video_timing_df(my_timings)
#'
#' @export
ov_merge_video_timing_df <- function(x, default = ov_video_timing_df()) {
    do_merge_timing_df(x, default = default, with_final_merge = TRUE)
}

do_merge_timing_df <- function(x, default, with_final_merge = FALSE) {
    if (!(is.data.frame(x) && all(c("skill", "phase") %in% names(x)) && any(c("start_offset", "duration") %in% names(x)) && nrow(x) > 0)) {
        stop("x should be a data.frame with the columns 'skill' and 'phase' and at least one of 'start_offset' or 'duration'")
    }
    if (!(is.data.frame(default) && all(c("skill", "phase", "start_offset", "duration") %in% names(default)) && nrow(default) > 0)) {
        stop("default should be a data.frame with the columns 'skill', 'phase', 'start_offset', and 'duration'")
    }
    if (!"start_offset" %in% names(x)) x$start_offset <- NA
    if (!"duration" %in% names(x)) x$duration <- NA
    if (is.factor(x$skill)) x$skill <- as.character(x$skill)
    if (is.factor(x$phase)) x$phase <- as.character(x$phase)
    for (i in seq_len(nrow(x))) {
        idx <- which(tolower(default$skill) == tolower(x$skill[i]) & tolower(default$phase) == tolower(x$phase[i]))
        if (length(idx) == 1) {
            if (!is.na(x$start_offset[i])) default$start_offset[idx] <- x$start_offset[i]
            if (!is.na(x$duration[i])) default$duration[idx] <- x$duration[i]
        } else if (length(idx) < 1) {
            ## add new row
            default <- bind_rows(default, x[i, ])
        }
    }
    default$skill <- paste0(toupper(substr(default$skill, 1, 1)), tolower(substr(default$skill, 2, 9999L)))
    default$phase <- paste0(toupper(substr(default$phase, 1, 1)), tolower(substr(default$phase, 2, 9999L)))
    ## and finally merge this with the default defaults, which will ensure that (a) we have all required entries, and (b) no duplicates, which might not happen if the default passed above is not complete/correct
    if (isTRUE(with_final_merge)) do_merge_timing_df(default, default = ov_video_timing_df(), with_final_merge = FALSE) else default
}

#' Convert playlist to 'onclick' string
#'
#' @param playlist data.frame: a playlist as returned by `ov_video_playlist`
#' @param video_id string: the id of the HTML video element to attach the playlist to
#' @param normalize_paths logical: if `TRUE`, apply `normalizePath` to local file paths. This will e.g. expand the tilde in paths like "~/path/to/video.mp4"
#' @param dvjs_fun string: the javascript function to use
#' @param seamless logical: if clips overlap, should we transition seamlessly from one to the next?
#' @param loop logical: should we loop endlessly over the playlist?
#' @param controller_var string: (for version 2 only) the js variable name of the controller object to assign this playlist to
#'
#' @return A string suitable for inclusion as an 'onclick' tag attribute
#'
#' @examples
#' \dontrun{
#'   library(shiny)
#'
#'   ## hand-crafted playlist for this example
#'   playlist <- data.frame(video_src = "NisDpPFPQwU",
#'                          start_time = c(624, 3373, 4320),
#'                          duration = 8,
#'                          type = "youtube")
#'   shinyApp(
#'       ui = fluidPage(
#'           ov_video_js(youtube = TRUE),
#'           ov_video_player(id = "yt_player", type = "youtube",
#'                           style = "height: 480px; background-color: black;"),
#'           tags$button("Go", onclick = ov_playlist_as_onclick(playlist, "yt_player"))
#'       ),
#'       server = function(input, output) {},
#'   )
#'
#'   ## or using v2, which supports multiple video elements in a page
#'   shinyApp(
#'       ui = fluidPage(
#'           ov_video_js(youtube = TRUE, version = 2),
#'           ## first player
#'           ov_video_player(id = "yt_player", type = "youtube",
#'                           style = "height: 480px; background-color: black;",
#'                           version = 2, controller_var = "my_dv"),
#'           tags$button("Go", onclick = ov_playlist_as_onclick(playlist, "yt_player",
#'                                                              controller_var = "my_dv")),
#'           ## second player
#'           ov_video_player(id = "yt_player2", type = "youtube",
#'                           style = "height: 480px; background-color: black;",
#'                           version = 2, controller_var = "my_dv2"),
#'           tags$button("Go", onclick = ov_playlist_as_onclick(playlist, "yt_player2",
#'                                                              controller_var = "my_dv2"))
#'       ),
#'       server = function(input, output) {},
#'   )
#'
#' }
#'
#' @export
ov_playlist_as_onclick <- function(playlist, video_id, normalize_paths = TRUE, dvjs_fun = "dvjs_set_playlist_and_play", seamless = TRUE, loop = FALSE, controller_var) {
    q2s <- function(z) gsub("\"", "'", gsub("'", "\\\\'", z))
    type <- unique(na.omit(playlist$type))
    if (is.factor(type)) type <- as.character(type)
    assert_that(is.string(type))
    assert_that(is.flag(normalize_paths), !is.na(normalize_paths))
    assert_that(is.flag(seamless), !is.na(seamless))
    if (!missing(controller_var) && !is.null(controller_var)) {
        assert_that(is.string(controller_var), !is.na(controller_var))
        dvjs_fun <- sub("^dvjs_", paste0(controller_var, "."), dvjs_fun)
    }
    if (normalize_paths) {
        local_srcs <- which(playlist$type == "local" & !grepl("^https?://", playlist$video_src, ignore.case = TRUE))
        if (length(local_srcs) > 0) {
            playlist$video_src[local_srcs] <- normalizePath(playlist$video_src[local_srcs], mustWork = FALSE)
        }
    }
    paste0(dvjs_fun, "(", q2s(jsonlite::toJSON(playlist)), ", '", video_id, "', '", type, "', ", ifelse(seamless, "true", "false"), ", ", ifelse(loop, "true", "false"), ");")
}


#' Convert playlist to VLC m3u format
#'
#' Converts a playlist object to a m3u file that can be opened with VLC. Note that this only works with local video files (not YouTube or other URLs) and the video files must be present on your local file system in order for this to work.
#'
#' @references <https://www.videolan.org/>, <https://wiki.videolan.org/M3U/>
#' @param playlist data.frame: as returned by [ov_video_playlist()]. If the playlist contains one or both of the columns `subtitle` or `subtitleskill`, these will be used as subtitle information associated with each clip
#' @param outfile string: the file name to write to. If not supplied, a file will be created in the temporary directory. Note that the directory of `outfile` must already exist
#' @param no_paths logical: if `TRUE`, remove the paths from video files. The m3u file must be saved into the same directory as the video source file(s)
#' @param seamless logical: if `TRUE`, merge adjacent items into a single clip
#'
#' @return The path to the m3u file
#'
#' @seealso [ov_video_playlist()] [ov_playlist_to_html()]
#'
#' @export
ov_playlist_to_vlc <- function(playlist, outfile, no_paths = FALSE, seamless = TRUE) {
    if (missing(playlist) || is.null(playlist) || nrow(playlist) < 1) return(character())
    if (any(playlist$type %in% c("youtube", "twitch")) || any(grepl("^https?://", playlist$video_src, ignore.case = TRUE))) {
        warning("ov_playlist_to_vlc only works with local video files, not remote video URLs")
    }
    if (missing(outfile) || length(outfile) < 1) outfile <- tempfile(fileext = ".m3u")
    assert_that(is.flag(no_paths), !is.na(no_paths))
    assert_that(is.flag(seamless), !is.na(seamless))
    if (seamless) playlist <- merge_seamless(playlist)
    if (!"subtitle" %in% names(playlist)) playlist$subtitle <- ""
    if (!"subtitleskill" %in% names(playlist)) playlist$subtitleskill <- ""
    if (no_paths) playlist$video_src <- basename(playlist$video_src)
    out <- vapply(seq_len(nrow(playlist)), function(i) item2m3u(playlist[i, ], seamless = seamless), FUN.VALUE = "", USE.NAMES = FALSE)
    cat(out, file = outfile, sep = "\n")
    outfile
}

## playlist entry to m3u item
item2m3u <- function(item, seamless = TRUE) {
    paste0("#EXTM3U\n#EXTINF:", item$duration, ",", item$subtitle, if (!seamless) ". ", if (!seamless) item$subtitleskill, "\n#EXTVLCOPT:start-time=", item$start_time, "\n#EXTVLCOPT:stop-time=", item$start_time + item$duration, "\n", item$video_src, "\n\n")
}

#' Convert playlist to standalone HTML file
#'
#' Converts a playlist object to an HTML file that can be opened in any browser. Note that if the playlist uses local video files, the HTML file will only work on a device that has access to those files. If the playlist uses YouTube (or other external) video URLs, the HTML file will be usable on any network-connected device.
#'
#' @param playlist data.frame: as returned by [ov_video_playlist()]. If the playlist contains one or both of the columns `subtitle` or `subtitleskill`, these will be shown as subtitle information that changes as each clip is played
#' @param playlist_name string: the name to use for the playlist
#' @param outfile string: the file name to write to. If not supplied, a file will be created in the temporary directory. Note that the directory of `outfile` must already exist
#' @param no_paths logical: if `TRUE`, remove the paths from video files (only applicable to local files, not YouTube or other external URLs). If `no_paths` is `TRUE`, The HTML file must be saved into the same directory as the video source file(s)
# @param seamless logical: if `TRUE`, merge adjacent items into a single clip
#' @param table_cols character: the names of columns in `playlist` to show in the plays table in the HTML file. If `table_cols` is empty, or contains no column names that are present in `playlist`, then no table will be shown
#' @param loop logical: should we loop endlessly over the playlist?
#' @param ... : additional arguments passed to the Rmd file used to generate the HTML. Currently these are:
#' * css : a string with additional css to apply to the file
#' * ui_header : a tag element to replace the default page header
#'
#' @return The path to the HTML file
#'
#' @seealso [ov_video_playlist()] [ov_playlist_to_vlc()]
#'
#' @examples
#' \dontrun{
#'   ## use data from the ovdata package
#'   library(ovdata) ## install via remotes::install_github("openvolley/ovdata") if needed
#'   x <- ovdata_example("190301_kats_beds-clip", as = "parsed")
#'
#'   ## make sure its video element points to our local copy of the corresponding video clip
#'   dv_meta_video(x) <- ovdata_example_video("190301_kats_beds")
#'
#'   ## extract the plays
#'   px <- datavolley::plays(x)
#'   ## it's a single rally, so we'll use all rows (just exclude NA skill rows)
#'   px <- px[!is.na(px$skill), ]
#'
#'   ## define columns to show in the table
#'   extra_cols <- c("home_team", "visiting_team", "video_time", "code", "set_number",
#'                   "home_team_score", "visiting_team_score")
#'
#'   ## make the playlist with extra columns included
#'   ply <- ov_video_playlist(px, x$meta, extra_cols = c(extra_cols, "player_name"))
#'
#'   ## use player name as the subtitle
#'   ply$subtitle <- ply$player_name
#'
#'   ## convert to HTML
#'   f <- ov_playlist_to_html(ply, table_cols = extra_cols)
#'
#'   ## and finally open it!
#'   browseFile(f)
#' }
#'
#' @export
ov_playlist_to_html <- function(playlist, playlist_name = "Playlist", outfile, no_paths = FALSE, table_cols = c(), loop = FALSE, ...) {
    seamless <- FALSE ## otherwise the plays table gets reduced to a smaller number of items
    if (missing(playlist) || is.null(playlist) || nrow(playlist) < 1) return(character())
    pltype <- playlist$type
    pltype[grepl("^https?://", playlist$video_src, ignore.case = TRUE) & !pltype %in% c("youtube", "twitch")] <- "remote"
    pltype <- unique(pltype)
    if (length(pltype) != 1) stop("must have playlist of single 'type' (e.g. not a mixture of YouTube URLs and local files)")
    if (missing(outfile) || length(outfile) < 1) outfile <- tempfile(fileext = ".html")
    assert_that(is.flag(no_paths), !is.na(no_paths))
    assert_that(is.flag(seamless), !is.na(seamless))
    if (seamless) playlist <- merge_seamless(playlist)
    if (!"subtitle" %in% names(playlist)) playlist$subtitle <- ""
    if (!"subtitleskill" %in% names(playlist)) playlist$subtitleskill <- ""
    if (no_paths) {
        fileidx <- playlist$type %in% "local" & !grepl("^https?://", playlist$video_src, ignore.case = TRUE)
        playlist$video_src[fileidx] <- basename(playlist$video_src[fileidx])
    }
    render_pl(playlist, pl_name = playlist_name, bannerdir = NULL, outfile = outfile, table_cols, loop = loop, ...)
}

if (FALSE) {
    pl <- ov_example_playlist()
    ##pl <- pl[, setdiff(names(pl), c("subtitle", "subtitleskill"))]
    f <- ov_playlist_to_html(pl)
    browseURL(f)
}

render_pl <- function(pl, pl_name, bannerdir, outfile, table_cols, ...) {
    pltype <- pl$type
    pltype[grepl("^https?://", pl$video_src, ignore.case = TRUE) & !pltype %in% c("youtube", "twitch")] <- "remote"
    pltype <- unique(pltype)
    if (length(pltype) != 1) stop("must have playlist of single 'type' (e.g. not a mixture of YouTube URLs and local files)")
    rmd_template_src <- system.file("extdata/ovplay.Rmd", package = "ovideo")
    working_dir <- tempfile()
    dir.create(working_dir)
    rmd_template <- file.path(working_dir, "ovplay.Rmd")
    if (!file.copy(from = rmd_template_src, to = rmd_template)) stop("cannot copy template file to temporary directory")
    vsx <- list(playlist = list(name = pl_name, playlist = pl, banner_directory = bannerdir, playlist_type = pltype, table_cols = table_cols), ...)
    blah <- knitr::knit_meta(class = NULL, clean = TRUE) ## may help stop memory allocation error
    rmarkdown::render(rmd_template, output_file = outfile, output_options = NULL, params = vsx)
}

merge_seamless <- function(playlist) {
    ## combine adjacent/overlapping clips into one
    keep <- rep(FALSE, nrow(playlist)); keep[1] <- TRUE
    last <- 1L
    dur <- playlist$duration
    for (i in seq_len(nrow(playlist))[-1]) {
        ## a clip overlaps with its preceding one if the start time of clip i sits within the time period of clip (i-1)
        ## but if clip i starts *before* clip (i-1) then they do not overlap (cannot be played seamlessly) - maybe a randomly-ordered playlist
        ## remember also that e.g. serve and reception will have the *same* start time. Also allow a bit of margin, so start_time >= (lag(start_time - 1))
        if (isTRUE(playlist$video_src[last] == playlist$video_src[i]) && ((playlist$start_time[last] + dur[last]) >= playlist$start_time[i]) && (playlist$start_time[i] >= playlist$start_time[last] - 1)) {
            dur[last] <- playlist$start_time[i] + dur[i] - playlist$start_time[last]
        } else {
            keep[i] <- TRUE
            last <- i
        }
    }
    playlist$seamless_duration <- playlist$duration <- dur
    playlist$seamless_start_time <- playlist$start_time
    playlist[keep, ]
}
