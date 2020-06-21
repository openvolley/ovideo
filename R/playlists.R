#' Create video playlist
#'
#' @param x data.frame: a datavolleyplays object. Normally this will be a selected subset of the `plays` component of a datavolley object (i.e. a selected set of actions that you want the video playlist to contain)
#' @param meta list: either the `meta` component of a datavolley object, or a list of such objects, or a data.frame with the columns "match_id" and "video_src". Entries in `video_src` should be paths or URLs to the video file associated with the corresponding `match_id`
#' @param type string: currently "youtube" or "local". If `type` is not specified as a parameter, and `meta` is a data.frame, then `type` can be provided as a column in `meta`. Alternatively, if `meta` is a `meta` component of a datavolley object, or a list of such objects, then `type` will be assumed to be "local". Note that a single playlist can't mix types, all entries must be of the same type
#' @param timing list: a named list giving the relative timing for each skill type. Each element in the list should be named (with the name of the skill, as it appears in `x`) and should consist of a two-element numeric vector giving the starting and ending time offset relative to the recorded `video_time` of the event. See \code{\link{ov_video_timing}} for further details
#' @param extra_cols character: names of additional columns from `x` to include in the returned data frame
#' @param normalize_paths logical: if \code{TRUE}, apply \code{normalizePath} to local file paths. This will e.g. expand the tilde in paths like "~/path/to/video.mp4"
#'
#' @return A data.frame with columns `src`, `start_time`, `duration`, plus any extras specified in `extra_cols`
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
        meta <- bind_rows(lapply(meta, function(z) tibble(match_id = match_id_from_meta(z), video_src = as.character(video_file_from_meta(z)))))
        if (is.null(type)) {
            if (all(is_youtube_id(meta$video_src) | grepl("https?://.*youtube", meta$video_src, ignore.case = TRUE))) {
                type <- "youtube"
            } else {
                type <- "local"
            }
        }
    } else {
        stop("meta is an unexpected format")
    }
    assert_that(is.string(type))
    type <- match.arg(tolower(type), c("local", "youtube"))
    if (!all(x$match_id %in% meta$match_id)) stop("x contains match_ids that do not appear in meta")
    if (any(is.na(meta$video_src))) {
        missing_vid_matches <- meta$match_id[is.na(meta$video_src)]
        stop("no video for matches with match_id: ", paste(missing_vid_matches, collapse = ", "))
    }
    if (!is.null(extra_cols)) assert_that(is.character(extra_cols))
    x <- left_join(x, meta, by = "match_id")
    ## convert timing to a data.frame
    timing <- bind_rows(lapply(names(timing), function(z) tibble(skill = z, start_offset = timing[[z]][1], duration = abs(diff(timing[[z]])))))
    x <- left_join(x, timing, by = "skill")
    x$start_time <- x$video_time + x$start_offset
    x <- x[!is.na(x$skill), ]
    ## TODO check for NA video_time
    x$video_src <- as.character(x$video_src)
    if (type == "youtube") {
        ## ensure that we have youtube IDs, not e.g. full URLs
        x$video_src <- vapply(x$video_src, function(z) {
            if (!is_youtube_id(z) && grepl("^https?://", z, ignore.case = TRUE)) {
                tryCatch({
                    temp <- httr::parse_url(z)
                    if (!is.null(temp$query$v) && length(temp$query$v) == 1) {
                        temp$query$v
                    } else {
                        z
                    }
                }, error = function(e) z)
            } else {
                z
            }
        }, FUN.VALUE = "", USE.NAMES = FALSE)
    }
    x$type <- type
    if (normalize_paths) {
        local_srcs <- which(x$type == "local")
        x$video_src[local_srcs] <- normalizePath(x$video_src[local_srcs], mustWork = FALSE)
    }
    x[, c("video_src", "start_time", "duration", "type", extra_cols)]
}

#' Create video playlist per point_id
#'
#' @param x data.frame: a datavolleyplays object. Normally this will be a selected subset of the `plays` component of a datavolley object (i.e. a selected set of actions that you want the video playlist to contain)
#' @param meta list: either the `meta` component of a datavolley object, or a list of such objects, or a data.frame with the columns "match_id" and "video_src". Entries in `video_src` should be paths or URLs to the video file associated with the corresponding `match_id`
#' @param type string: currently "youtube" or "local". If `type` is not specified as a parameter, and `meta` is a data.frame, then `type` can be provided as a column in `meta`. Alternatively, if `meta` is a `meta` component of a datavolley object, or a list of such objects, then `type` will be assumed to be "local"
#' @param extra_cols character: names of additional columns from `x` to include in the returned data frame
#' @param normalize_paths logical: if \code{TRUE}, apply \code{normalizePath} to local file paths. This will e.g. expand the tilde in paths like "~/path/to/video.mp4"
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
        meta <- bind_rows(lapply(meta, function(z) tibble(match_id = match_id_from_meta(z), video_src = video_file_from_meta(z))))
        if (is.null(type)) type <- "local"
    } else {
        stop("meta is an unexpected format")
    }
    assert_that(is.string(type))
    type <- match.arg(tolower(type), c("local", "youtube"))
    if (!all(x$match_id %in% meta$match_id)) stop("x contains match_ids that do not appear in meta")
    if (any(is.na(meta$video_src))) {
        missing_vid_matches <- meta$match_id[is.na(meta$video_src)]
        stop("no video for matches with match_id: ", paste(missing_vid_matches, collapse = ", "))
    }
    if (!is.null(extra_cols)) assert_that(is.character(extra_cols))
    x <- left_join(x, meta, by = "match_id")
    
    ## convert timing to a data.frame
    # timing <- plyr::ddply(x, .(point_id), plyr::summarize, start_time = min(.$video_time, na.rm = TRUE), 
    #                       duration = max(.$video_time, na.rm=TRUE) - min(.$video_time, na.rm = TRUE))
    
    timing_tmp <- dplyr::full_join(
        stats::aggregate(x = x$video_time, by = list(x$point_id), FUN = min,na.rm=TRUE),
        stats::aggregate(x = x$video_time, by = list(x$point_id), FUN = range,na.rm=TRUE), by = "Group.1")
    timing_tmp$duration <- apply(timing_tmp$x.y,1,diff) + 5
    timing <- timing_tmp[,c("Group.1", "x.x", "duration")]
    
    names(timing) <- c("point_id", "start_time", "duration")
    timing$start_time <- timing$start_time - 2
    
    x <- left_join(x, timing, by = "point_id")
    #x$start_time <- x$video_time + x$start_offset
    x <- x[!is.na(x$skill), ]
    ## TODO check for NA video_time
    x$video_src <- as.character(x$video_src)
    x$type <- type
    if (normalize_paths) {
        local_srcs <- which(x$type == "local")
        x$video_src[local_srcs] <- normalizePath(x$video_src[local_srcs], mustWork = FALSE)
    }
    dplyr::distinct(x[, c("video_src", "start_time", "duration", "type", extra_cols)])
}


#' Timing to use when creating video playlist
#'
#' By default, all skills have a timing of `c(-5, 3)`, meaning that the video clip will start 5 seconds before the recorded time of the event and end 3 seconds after its recorded time.
#'
#' @param ... : named parameters that will override the defaults. Each parameter should be a two-element numeric vector
#'
#' @return A named list, with names corresponding to skills ("Serve", "Reception", etc).
#'
#' @seealso \code{\link{ov_video_playlist}}
#'
#' @examples
#'
#' ## defaults
#' ov_video_timing()
#'
#' ## with different settings for serve and reception
#' ov_video_timing(serve = c(-2, 2), reception = c(-3, 1))
#'
#' @export
ov_video_timing <- function(...) {
    skills <- c("Serve", "Reception", "Set", "Attack", "Block", "Dig", "Freeball")
    out <- rep(list(c(-5, 3)), length(skills))
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


#' Convert playlist to 'onclick' string
#'
#' @param playlist data.frame: a playlist as returned by `ov_video_playlist`
#' @param video_id string: the id of the HTML video element to attach the playlist to
#' @param normalize_paths logical: if \code{TRUE}, apply \code{normalizePath} to local file paths. This will e.g. expand the tilde in paths like "~/path/to/video.mp4"
#' @param dvjs_fun string: the javascript function to use
#'
#' @return A string suitable for inclusion as an 'onclick' tag attribute
#'
#' @examples
#' \dontrun{
#'   library(shiny)
#'
#'   ## hand-crafted playlist for this example
#'   playlist <- data.frame(video_src = "xL7qEpOdtio",
#'                          start_time = c(5417, 7252, 6222, 7656, 7369),
#'                          duration = 8,
#'                          type = "youtube")
#'   shinyApp(
#'       ui = fluidPage(
#'           ov_video_js(youtube = TRUE),
#'           ov_video_player(id = "yt_player", style = "height: 480px; background-color: black;"),
#'           tags$button("Go", onclick = ov_playlist_as_onclick(playlist, "yt_player"))
#'       ),
#'       server = function(input, output) {},
#'   )
#' }
#'
#' @export
ov_playlist_as_onclick <- function(playlist, video_id, normalize_paths = TRUE, dvjs_fun = "dvjs_set_playlist_and_play") {
    q2s <- function(z) gsub("\"", "'", z)
    type <- unique(na.omit(playlist$type))
    if (is.factor(type)) type <- as.character(type)
    assert_that(is.string(type))
    assert_that(is.flag(normalize_paths), !is.na(normalize_paths))
    if (normalize_paths) {
        local_srcs <- which(playlist$type == "local")
        if (length(local_srcs) > 0) {
            playlist$video_src[local_srcs] <- normalizePath(playlist$video_src[local_srcs], mustWork = FALSE)
        }
    }
    paste0(dvjs_fun, "(", q2s(jsonlite::toJSON(playlist)), ", '", video_id, "', '", type, "');")
}
