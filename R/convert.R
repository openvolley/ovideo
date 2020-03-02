#' Make a self-contained video file from a playlist
#'
#' Requires that ffmpeg be available on the system path. Note that the processing of each clip is done inside of a `future_lapply` call, and so you can have this part of the processing done in parallel by setting an appropriate futures plan before calling this function.
#'
#' This function is experimental. In particular it is unlikely to work well with all video formats, and especially if the playlist comprises clips from different videos with different resolution/encoding/etc.
#'
#' @param playlist data.frame: a playlist as returned by `ov_video_playlist`. Note that only local video sources are supported
#' @param filename string: file to write to. If not specified (or `NULL`), a file in the temporary directory will be created. If `filename` exists, it will be overwritten. The extension of `filename` will determine the output format
#' @param subtitle_column string: if not `NULL`, a subtitle file will be produced using the contents of this column (in the playlist) as the subtitle for each clip. The subtitle file will have the same name as `filename` but with extension ".srt"
#'
#' @return A list with the filenames of the created video and subtitle files.
#'
#' @seealso \code{\link{ov_video_playlist}}
#' @examples
#' \dontrun{
#'   my_playlist <- ov_video_playlist(..., type = "local")
#'   video_file <- ov_create_video(my_playlist)
#'   browseURL(video_file[[1]])
#'
#'   ## run in parallel, with the scouted codes as subtitles
#'   library(dplyr)
#'   library(future.apply)
#'   plan(multisession)
#'   ## note that the example file doesn't have a video associated with it, so
#'   ##  this example won't actually work in practice
#'   x <- read_dv(dv_example_file())
#'   ## fudge the video entry
#'   x$meta$video <- tibble(camera = "Camera0", file = "~/my_video.mp4")
#'   ## make the playlist
#'   my_playlist <- ov_video_playlist(
#'     x$plays %>% dplyr::filter(skill == "Reception") %>% slice(1:10),
#'     meta = x$meta, extra_cols = "code")
#'   ## create the video and subtitles files
#'   video_file <- ov_create_video(my_playlist, subtitle_column = "code")
#' }
#'
#' @export
ov_create_video <- function(playlist, filename, subtitle_column = NULL) {
    if (missing(filename) || is.null(filename)) filename <- tempfile(fileext = ".mp4")
    ## find ffmpeg
    chk <- sys::exec_internal("ffmpeg", "-version")
    if (chk$status != 0) stop("could not find the ffmpeg executable")
    tempfiles <- future.apply::future_lapply(seq_len(nrow(playlist)), function(ri) {
        outfile <- tempfile(fileext = paste0(".", fs::path_ext(playlist$video_src[ri])))
        if (file.exists(outfile)) unlink(outfile)
        infile <- fs::path_real(playlist$video_src[ri])
        ##sys::exec_wait("ffmpeg", c("-i", infile, "-ss", playlist$start_time[ri], "-t", playlist$duration[ri], "-q", 0, "-c:a", "copy", outfile)) ## works, but slow because it uses very slow seek method to find the start of the clip
        ##sys::exec_wait("ffmpeg", c("-ss", playlist$start_time[ri], "-i", infile, "-t", playlist$duration[ri], "-q", 0, "-c:a", "copy", outfile)) ## faster but glitchy because keyframes are typically sparse
        ##sys::exec_wait("ffmpeg", c("-i", infile, "-ss", playlist$start_time[ri], "-t", playlist$duration[ri], "-c", "copy", outfile)) ## fast but glitchy because keyframes are typically sparse
        ## reencode
        res <- sys::exec_internal("ffmpeg", c("-ss", playlist$start_time[ri], "-i", infile, "-strict", "-2", "-t", playlist$duration[ri], outfile))
        if (res$status != 0) stop("failed to get video clip, ", rawToChar(res$stderr))
        outfile
    })
    tempfiles <- unlist(tempfiles)
    ## concatentate them
    cfile <- tempfile(fileext = ".txt")
    on.exit(unlink(c(cfile, tempfiles)))
    cat(paste0("file ", tempfiles), file = cfile, sep = "\n")
    if (file.exists(filename)) unlink(filename)
    res <- sys::exec_internal("ffmpeg", c("-safe", 0, "-f", "concat", "-i", cfile, "-c", "copy", filename))
    if (res$status != 0) stop("failed to combine clips, ", rawToChar(res$stderr))
    srtfile <- NULL
    if (!is.null(subtitle_column)) {
        srts <- ov_playlist_subtitles(playlist, subtitle_column = subtitle_column)
        srtfile <- sub(paste0(fs::path_ext(filename), "$"), "srt", filename)
        if (file.exists(srtfile)) unlink(srtfile)
        writeLines(srts, srtfile)
    }
    list(video = filename, subtitles = srtfile)
}

## not exported, yet
ov_playlist_subtitles <- function(playlist, subtitle_column) {
    if (missing(subtitle_column) || !subtitle_column %in% names(playlist)) {
        stop("subtitle_column must be supplied and present in the playlist dataframe")
    }
    format_hmsms <- function(z) {
        ## expecting z in decimal seconds
        h <- floor(z/3600)
        m <- floor((z-h*3600)/60)
        s <- floor((z-h*3600-m*60))
        ms <- round((z-floor(z))*1000)
        sprintf("%02d:%02d:%02d,%03d", h, m, s, ms)
    }
    unlist(lapply(seq_len(nrow(playlist)),
                  function(z) {
                      this_times <- c(cumsum(c(0, playlist$duration))[z], cumsum(playlist$duration)[z])
                      c(z, paste0(format_hmsms(this_times[1]), " --> ", format_hmsms(this_times[2])),
                        playlist[[subtitle_column]][z], "")
                  }))
}
