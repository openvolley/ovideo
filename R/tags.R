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
#' newfile <- ov_set_video_meta(ov_example_video(), comment = "A comment")
#' ov_get_video_meta(newfile)
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
#' newfile <- ov_set_video_meta(ov_example_video(), comment = "A comment")
#' ov_get_video_meta(newfile)
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

