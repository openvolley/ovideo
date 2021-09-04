#' Retrieve metadata tags from a video file
#'
#' Requires that ffmpeg is available on your system path.
#'
#' @param video_file string: path to the video file
#' @param debug logical: if `TRUE`, echo the ffmpeg output to the console
#'
#' @return A named list of metadata values
#' @seealso [ov_set_video_meta()]
#' @examples
#' \dontrun{
#'   newfile <- ov_set_video_meta(ov_example_video(), comment = "A comment")
#'   ov_get_video_meta(newfile)
#' }
#'
#' @export
ov_get_video_meta <- function(video_file, debug = FALSE) {
    if (!ov_ffmpeg_ok(do_error = FALSE)) {
        warning("could not find the ffmpeg executable")
        list()
    }
    tmpf <- tempfile()
    cargs <- c("-i", fs::path_real(video_file), "-f", "ffmetadata", tmpf)
    execfun <- if (isTRUE(debug)) sys::exec_wait else sys::exec_internal
    res <- execfun(ov_ffmpeg_exe(), cargs)
    mx <- strsplit(readLines(tmpf)[-1], "=")
    nms <- unlist(lapply(mx, function(z) z[1]))
    vals <- lapply(mx, function(z) z[2])
    setNames(vals, nms)
}

#' Set metadata tags in a video file
#'
#' Requires that ffmpeg is available on your system path.
#'
#' This function creates a new video file with the specified metadata added. This is always a file in the temporary directory. If `overwrite = TRUE`, the original file is deleted and replaced with the new file.
#'
#' Note that if `movflags = FALSE`, the supported video tag names (i.e. allowable names in the `...` parameters) depend on the video file type.
#'
#' @param video_file string: path to the video file
#' @param ... : named values to set
#' @param movflags logical: if `TRUE`, add "-movflags use_metadata_tags" to the command-line ffmpeg call. This allows arbitrary tag names to be used with mp4/m4v/mov video formats, but note that these may be stored in a manner that some video software cannot read. If `movflags = FALSE`, the supported video tag names (i.e. allowable names in the `...` parameters) depend on the video file type
#' @param overwrite logical: if `TRUE` overwrite the `video_file`, see Details
#' @param debug logical: if `TRUE`, echo the ffmpeg output to the console
#'
#' @return The path to the new video file, which if `overwrite = TRUE` will be the input file, otherwise a file in the temporary directory
#' @seealso [ov_get_video_meta()]
#'
#' @examples
#' \dontrun{
#'   newfile <- ov_set_video_meta(ov_example_video(), comment = "A comment")
#'   ov_get_video_meta(newfile)
#' }
#'
#' @export
ov_set_video_meta <- function(video_file, ..., movflags = FALSE, overwrite = FALSE, debug = FALSE) {
    ov_ffmpeg_ok(do_error = TRUE)
    mx <- list(...)
    if (length(mx) < 1) stop("no metadata provided")
    tmpf <- tempfile(fileext = paste0(".", fs::path_ext(video_file)))
    cargs <- c("-i", fs::path_real(video_file))
    if (isTRUE(movflags)) cargs <- c(cargs, "-movflags", "use_metadata_tags")
    for (i in seq_along(mx)) cargs <- c(cargs, "-metadata", paste0(names(mx)[i], "=", mx[[i]]))
    cargs <- c(cargs, "-c", "copy", tmpf)
    execfun <- if (isTRUE(debug)) sys::exec_wait else sys::exec_internal
    res <- execfun(ov_ffmpeg_exe(), cargs)
    res_status <- if (isTRUE(debug)) res else res$status
    if (res_status != 0) stop("failed to set video metadata, ", if (!isTRUE(debug)) rawToChar(res$stderr))
    if (isTRUE(overwrite)) {
        fs::file_delete(video_file)
        fs::file_move(tmpf, video_file)
        video_file
    } else {
        tmpf
    }
}

#' Retrieve a data object stored in a video file metadata tag
#'
#' @param video_file string: path to the video file
#' @param tag string: the tag name to use
#' @param b64 logical: was `obj` serialized and base64-encoded before storing?
#'
#' @return The stored information, or `NULL` if there was none
#'
#' @seealso [ov_set_video_data()]
#' @examples
#' \dontrun{
#'   if (interactive()) {
#'     ## mark the geometry of the court in the video
#'     ref <- ov_shiny_court_ref(video_file = ov_example_video(), t = 5)
#'
#'     ## store it
#'     newfile <- ov_set_video_data(ov_example_video(), obj = ref)
#'
#'     ## retrieve it
#'     ov_get_video_data(newfile)
#'   }
#' }
#' @export
ov_get_video_data <- function(video_file, tag = "ov_court_info", b64 = TRUE) {
    md <- ov_get_video_meta(video_file)[[tag]]
    if (!is.null(md) && isTRUE(b64)) md <- unserialize(base64enc::base64decode(md))
    md
}

#' Store a data object in a video file metadata tag
#'
#' This function stores an R data object (data frame, list, etc) within a metadata tag inside a video file. This is primarily intended to store video-specific information, so that this information is carried with the video file itself. By default the `ov_court_info` metadata tag is used (intended to store the geometry of the playing court in the video, see Examples).
#'
#' @param video_file string: path to the video file
#' @param obj : data object to store, typically a list as returned by [ovideo::ov_shiny_court_ref()]. `obj` will be serialized and base64-encoded before storing unless `b64 = FALSE`
#' @param tag string: the tag name to use
#' @param b64 logical: serialize `obj` and base64-encode before storing?
#' @param replace logical: if `FALSE` and the specified metadata tag is already present in the video file, don't replace it
#' @param overwrite logical: if `TRUE` overwrite the `video_file`, otherwise create a new file in the temporary directory. See [ov_set_video_meta()] for details
#'
#' @seealso [ov_get_video_data()], [ov_set_video_meta()]
#' @examples
#' \dontrun{
#'   if (interactive()) {
#'     ## mark the geometry of the court in the video
#'     ref <- ov_shiny_court_ref(video_file = ov_example_video(), t = 5)
#'
#'     ## store it
#'     newfile <- ov_set_video_data(ov_example_video(), obj = ref)
#'
#'     ## retrieve it
#'     ov_get_video_data(newfile)
#'   }
#' }
#'
#' @return The path to the video file
#'
#' @export
ov_set_video_data <- function(video_file, obj, tag = "ov_court_info", b64 = TRUE, replace = FALSE, overwrite = FALSE) {
    if (!isTRUE(replace)) {
        ## check first if it's already set (don't bother unserializing, just checking for existence)
        if (!is.null(ov_get_video_data(video_file, tag = tag, b64 = FALSE))) stop("video file already has metadata present in the ", tag, " field")
    }
    if (isTRUE(b64)) obj <- base64enc::base64encode(serialize(obj, NULL))
    rgs <- list(video_file = video_file, movflags = TRUE, overwrite = overwrite)
    rgs[[tag]] <- obj
    do.call(ov_set_video_meta, rgs)
}
