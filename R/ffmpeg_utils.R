#' Try and locate a video file, when the path embedded in the dvw file is for another computer
#'
#' @param dvw_filename string: the full path to the DataVolley file
#' @param video_filename character: one or more video file paths. If `NULL`, the video file name embedded in the DataVolley file will be used
#'
#' @return A character vector, with one entry per `video_filename`. Video files that could not be found will be `NA` here.
#'
#' @export
ov_find_video_file <- function(dvw_filename, video_filename = NULL) {
    assert_that(is.string(dvw_filename))
    if (is.null(video_filename)) {
        video_filename <- datavolley::dv_read(dvw_filename, metadata_only = TRUE)$meta$video
        if (nrow(video_filename) > 0) {
            return(ov_find_video_file(dvw_filename = dvw_filename, video_filename = video_filename$file))
        } else {
            video_filename <- NA_character_
        }
    }
    if (length(video_filename) > 1) {
        return(vapply(seq_len(nrow(video_filename)), function(z) ov_find_video_file(dvw_filename = dvw_filename, video_filename = video_filename$file[z]), FUN.VALUE = "", USE.NAMES = FALSE))
    }
    if (length(video_filename) == 1 && !is.na(video_filename) && nzchar(video_filename)) {
        if (fs::file_exists(video_filename)) return(video_filename) ## ok, the path in the dvw file is actually correct
        ## otherwise let's go looking for it
        this_dir <- dirname(dvw_filename) ## actual file has to be under the same path
        if (!fs::dir_exists(this_dir) && !fs::link_exists(this_dir)) return(NA_character_)
        possible_paths <- c(this_dir, fs::dir_ls(this_dir, type = "dir", recurse = TRUE))
        ff <- fs::path(possible_paths, basename(video_filename))
        ff <- ff[fs::file_exists(ff)]
        if (length(ff) ==1) ff else NA_character_
    } else {
        NA_character_
    }
}


#' Extract a single frame from a video file
#'
#' Requires that ffmpeg is available on your system path.
#'
#' @param video_file string: path to the video file
#' @param t numeric: the time of the frame to extract (in seconds)
#' @param format string: "jpg" or "png"
#' @param debug logical: if \code{TRUE}, echo the ffmpeg output to the console
#'
#' @return The path to the frame image file
#'
#' @seealso \code{\link{ov_video_frames}}
#' @export
ov_video_frame <- function(video_file, t, format = "jpg", debug = FALSE) {
    assert_that(is.string(video_file), fs::file_exists(video_file))
    assert_that(is.numeric(t), t >= 0)
    format <- match.arg(tolower(format), c("jpg", "png"))
    imfs <- tempfile(fileext = paste0(".", format))
    execfun <- if (isTRUE(debug)) sys::exec_wait else sys::exec_internal
    res <- execfun("ffmpeg", c("-y", "-ss", t, "-i", fs::path_real(video_file), "-vframes", 1, imfs))
    imfs
}

#' Extract clip from video file
#'
#' Requires that ffmpeg is available on your system path.
#'
#' @param video_file string: path to the input file
#' @param outfile string: path to the output file. If missing, a temporary file (with extension .mp4) will be used
#' @param start_time numeric: start time in seconds
#' @param duration numeric: duration in seconds. If missing, will be calculated from start_time and end_time
#' @param end_time numeric: end time in seconds. If missing, will be calculated from start_time and duration
#' @param extra : additional parameters passed to ffmpeg, in the form c("param", "value", "param2", "value2")
#' @param debug logical: if \code{TRUE}, echo the ffmpeg output to the console
#'
#' @return The path to the video clip file
#'
#' @export
ov_video_extract_clip <- function(video_file, outfile, start_time, duration, end_time, extra = NULL, debug = FALSE) {
    if (missing(duration) && !missing(end_time)) duration <- end_time - start_time
    if (missing(outfile)) outfile <- tempfile(fileext = ".mp4")
    cargs <- c("-ss", as.character(start_time), "-i", fs::path_real(video_file), "-t", as.character(duration), "-c", "copy", extra, outfile)
    execfun <- if (isTRUE(debug)) sys::exec_wait else sys::exec_internal
    res <- execfun("ffmpeg", cargs)
    outfile
}

#' Extract multiple frames from a video file
#'
#' Requires that ffmpeg is available on your system path.
#'
#' @param video_file string: path to the video file
#' @param start_time numeric: start time in seconds
#' @param duration numeric: duration in seconds. If missing, will be calculated from start_time and end_time
#' @param end_time numeric: end time in seconds. If missing, will be calculated from start_time and duration
#' @param outdir string: path to the output directory, which must exist. If missing, a temporary directory will be used
#' @param fps numeric: frames per second, default is to extract all frames
#' @param format string: "jpg" or "png"
#' @param jpg_quality numeric: jpg quality from 1-31, lower is better (this is passed to ffmpeg as the \code{-qscale:v} parameter)
#' @param extra : additional parameters passed to ffmpeg, in the form c("param", "value", "param2", "value2")
#' @param debug logical: if \code{TRUE}, echo the ffmpeg output to the console
#'
#' @return A character vector of file names, one per frame
#'
#' @seealso \code{\link{ov_video_frame}}
#'
#' @export
ov_video_frames <- function(video_file, start_time, duration, end_time, outdir, fps, format = "jpg", jpg_quality = 1, extra = NULL, debug = FALSE) {
    create_clip <- TRUE ## internal method choice
    if (missing(outdir) || is.null(outdir)) {
        outdir <- tempfile()
        dir.create(outdir)
    } else {
        if (!dir.exists(outdir)) {
            stop("outdir ", outdir, " does not exist")
        }
        outdir <- fs::path_real(outdir)
    }
    if (missing(end_time) && !missing(duration)) end_time <- start_time + duration
    format <- match.arg(tolower(format), c("jpg", "png"))
    if (!missing(start_time)) {
        if (create_clip) {
            ## create clip explicitly first
            vidclip <- ov_video_extract_clip(video_file, start_time = start_time, end_time = end_time)
            on.exit(unlink(vidclip))
            video_file <- vidclip
            vf <- if (!missing(fps)) c("-vf", paste0("fps=", fps)) else NULL
        } else {
            ## use filter to select time range
            vf <- c(if (!missing(fps)) paste0("fps=", fps), if (!missing(start_time)) paste0("\"select='between(t,", start_time, ",", end_time, ")'\""))
            if (length(vf) > 0) vf <- c("-vf", vf)
        }
    } else {
        vf <- NULL
    }
    ##av::av_video_images(vidclip, format = format)
    cargs <- c("-i", fs::path_real(video_file), vf, "-vsync", "0", "-qscale:v", jpg_quality, extra, file.path(outdir, paste0("image_%06d.", format)))
    execfun <- if (isTRUE(debug)) sys::exec_wait else sys::exec_internal
    res <- execfun("ffmpeg", cargs)
    dir(outdir, pattern = paste0("\\.", format, "$"), full.names = TRUE)
}

#' Encode a set of images into a video
#'
#' Input files can either be specified as a list of image files, or alternatively as a directory name and image file mask. For the latter, the images must be numbered in sequential order.
#'
#' @param input_dir string: path to the input directory
#' @param image_file_mask string: the mask that specifies the image files, e.g. "image_%06d.jpg" for images named "image_000001.jpg", "image_000002.jpg" etc
#' @param image_files character: vector of input image files, in order that they should appear in the video. Used if \code{input_dir} is missing
#' @param outfile string: the output file. If missing, a temporary file (with extension .mp4) will be used
#' @param fps numeric: frames per second
#' @param extra : additional parameters passed to ffmpeg, in the form c("param", "value", "param2", "value2"). For example, \code{c("-vb", "4096k")} could be used to control the output video bitrate
#' @param debug logical: if \code{TRUE}, echo the ffmpeg output to the console
#'
#' @return The path to the video file
#' @seealso \code{\link[av]{av_encode_video}} if you don't have ffmpeg installed
#'
#' @export
ov_images_to_video <- function(input_dir, image_file_mask = "image_%06d.jpg", image_files, outfile, fps = 30, extra = NULL, debug = FALSE) {
    if (missing(outfile)) outfile <- tempfile(fileext = ".mp4")
    if (grepl("mp4$", outfile)) extra <- c(extra, "-pix_fmt", "yuv420p") ## https://trac.ffmpeg.org/wiki/Slideshow: "when outputting H.264, adding -vf format=yuv420p or -pix_fmt yuv420p will ensure compatibility"
    if (missing(input_dir)) {
        demux_file <- tempfile(fileext = ".txt")
        cat(paste0("file ", image_files, "\nduration ", 1/fps), sep = "\n", file = demux_file)
        cat("file ", tail(image_files, 1), "\n", sep = "", file = demux_file, append = TRUE)
        on.exit(unlink(demux_file))
        cargs <- c("-f", "concat", "-safe", "0", "-i", demux_file, extra, fs::path_real(outfile))
    } else {
        cargs <- c("-framerate", as.character(fps), "-i", file.path(fs::path_real(input_dir), image_file_mask), extra, fs::path_real(outfile))
    }
    execfun <- if (isTRUE(debug)) sys::exec_wait else sys::exec_internal
    res <- execfun("ffmpeg", cargs)
    outfile
}

#' Playlist to video file
#'
#' Make a self-contained video file from a playlist.
#' 
#' Requires that ffmpeg be available on the system path. Note that the processing of each clip is done inside of a `future_lapply` call, and so you can have this part of the processing done in parallel by setting an appropriate futures plan before calling this function.
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
ov_playlist_to_video <- function(playlist, filename, subtitle_column = NULL) {
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


#' @title Playlist to video file
#' @description Make a self-contained video file from a playlist
#' @param playlist data.frame: a playlist as returned by `ov_video_playlist`. Note that only local video sources are supported
#' @param filename string: file to write to. If not specified (or `NULL`), a file in the temporary directory will be created. If `filename` exists, it will be overwritten. The extension of `filename` will determine the output format
#' @param subtitle_column string: if not `NULL`, a subtitle file will be produced using the contents of this column (in the playlist) as the subtitle for each clip. The subtitle file will have the same name as `filename` but with extension ".srt"
#' @return A list with the filenames of the created video and subtitle files.
#' @name ov_create_video-deprecated
#' @usage ov_create_video(playlist, filename, subtitle_column = NULL)
#' @keywords internal
#' @seealso \code{\link{ovideo-deprecated}}
NULL



#' @rdname ovideo-deprecated
#' @section \code{ov_create_video}:
#' For \code{ov_create_video}, use \code{\link{ov_playlist_to_video}}.
#'
#' @export
ov_create_video <- function(playlist, filename, subtitle_column = NULL) {
    .Deprecated("ov_playlist_to_video", package="ovideo")
    ov_playlist_to_video(playlist, filename = filename, subtitle_column = subtitle_column)
}
