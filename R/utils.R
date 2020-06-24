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
ov_encode_video <- function(input_dir, image_file_mask = "image_%06d.jpg", image_files, outfile, fps = 30, extra = NULL, debug = FALSE) {
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


#' Define the reference points on a court image
#'
#' This function is used to define the reference points on a court image, to be used with \code{\link{ov_transform_points}}.
#' The court coordinate system is that used in \code{\link[datavolley]{dv_court}}, \code{\link[datavolley]{ggcourt}}, and related functions.
#' Try \code{plot(c(0, 4), c(0, 7), type = "n", asp = 1); datavolley::dv_court()} or \code{ggplot2::ggplot() + datavolley::ggcourt() + ggplot2::theme_bw()} for a visual depiction.
#'
#' @param image_file string: path to an image file (jpg) containing the court image (not required if \code{video_file} is supplied)
#' @param video_file string: path to a video file from which to extract the court image (not required if \code{image_file} is supplied)
#' @param t numeric: the time of the video frame to use as the court image (not required if \code{image_file} is supplied)
#' @param type string: currently only "corners"
#'
#' @return A data.frame containing the reference information
#'
#' @seealso \code{\link{ov_transform_points}}, \code{\link[datavolley]{dv_court}},  \code{\link[datavolley]{ggcourt}}
#'
#' @examples
#' if (interactive()) {
#'   crt <- ov_get_court_ref(image_file = system.file("extdata/2019_03_01-KATS-BEDS-court.jpg",
#'                           package = "ovideo"))
#'
#' }
#' @export
ov_get_court_ref <- function(image_file, video_file, t = 60, type = "corners") {
  assert_that(is.string(type))
  type <- match.arg(tolower(type), c("corners")) ##, "10p"))
  if (missing(image_file) || is.null(image_file)) {
    image_file <- ov_video_frame(video_file, t)
  }
  img <- jpeg::readJPEG(image_file)
  plot(c(0, 1), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "", asp = dim(img)[1]/dim(img)[2])
  rasterImage(img, 0, 0, 1, 1)
  cat("Click the near left baseline corner\n")
  this <- locator(1)
  imx <- this$x
  imy <- this$y
  points(this$x, this$y, col = 3)
  cat("Click the near right baseline corner\n")
  this <- locator(1)
  points(this$x, this$y, col = 3)
  imx <- c(imx, this$x)
  imy <- c(imy, this$y)
  refx <- c(0.5, 3.5); refy <- c(0.5, 0.5)
  if (type == "10p") {
    ## near 3m line right side, centre line right side, centre line left side, near 3m line left side, far 3m line right side
  }
  cat("Click the far right baseline corner\n")
  this <- locator(1)
  points(this$x, this$y, col = 3)
  imx <- c(imx, this$x)
  imy <- c(imy, this$y)
  cat("Click the far left baseline corner\n")
  this <- locator(1)
  points(this$x, this$y, col = 3)
  imx <- c(imx, this$x)
  imy <- c(imy, this$y)
  refx <- c(refx, 3.5, 0.5); refy <- c(refy, 6.5, 6.5)
  if (type == "10p") {
    ## far 3m line left side
  }
  data.frame(image_x = imx, image_y = imy, court_x = refx, court_y = refy)
}

#' Transform points from image coordinates to court coordinates or vice-versa
#'
#' The court coordinate system is that used in \code{\link[datavolley]{dv_court}}, \code{\link[datavolley]{ggcourt}}, and related functions.
#' Try \code{plot(c(0, 4), c(0, 7), type = "n", asp = 1); datavolley::dv_court()} or \code{ggplot2::ggplot() + datavolley::ggcourt() + ggplot2::theme_bw()} for a visual depiction.
#'
#' @param x numeric: input x points. \code{x} can also be a two-column data.frame or matrix
#' @param y numeric: input y points
#' @param ref data.frame: reference, as returned by \code{\link{ov_get_court_ref}}
#' @param direction string: either "to_court" (to transform image coordinates to court coordinates) or "to_image" (the reverse) 
#'
#' @return A two-column data.frame with transformed values
#'
#' @seealso \code{\link{ov_get_court_ref}}, \code{\link[datavolley]{dv_court}},  \code{\link[datavolley]{ggcourt}}
#'
#' @examples
#' ## the ref data for the example iomage
#' crt <- data.frame(image_x = c(0.05397063, 0.95402573, 0.75039756, 0.28921230),
#'                   image_y = c(0.02129301, 0.02294600, 0.52049712, 0.51884413),
#'                   court_x = c(0.5, 3.5, 3.5, 0.5),
#'                   court_y = c(0.5, 0.5, 6.5, 6.5))
#'
#' ## show the image
#' img <- jpeg::readJPEG(system.file("extdata/2019_03_01-KATS-BEDS-court.jpg",
#'                           package = "ovideo"))
#' plot(c(0, 1), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "",
#'      asp = dim(img)[1]/dim(img)[2])
#' rasterImage(img, 0, 0, 1, 1)
#'
#' ## convert the ends of the 3m lines on court to image coordinates
#' check <- data.frame(x = c(0.5, 3.5, 0.5, 3.5),
#'                     y = c(2.5, 2.5, 4.5, 4.5))
#' ix <- ov_transform_points(check, ref = crt, direction = "to_image")
#'
#' ## and finally plot onto the image
#' points(ix$x, ix$y, pch = 21, bg = 4)
#'
#' @export
ov_transform_points <- function(x, y, ref, direction = "to_court") {
    direction <- match.arg(tolower(direction), c("to_court", "to_image"))
    if (direction == "to_image") {
      ## flip the ref definitions
      ref <- dplyr::rename(ref, image_x = "court_x", image_y = "court_y", court_x = "image_x", court_y = "image_y")
    }
    if (is.matrix(x)) {
        y <- x[, 2]
        x <- x[, 1]
    } else if (is.data.frame(x)) {
        y <- x[[2]]
        x <- x[[1]]
    } else {
        x <- as.vector(x)
        y <- as.vector(y)
    }
    if (!nrow(ref) %in% c(4, 10)) {
      stop("expecting ref to be a data.frame with 4 or 10 rows, as produced by ov_get_court_ref")
    }
    z <- as.vector(rep(1, length(x)))
    ## Using homography principles
    homography_transform = function(x, X) {
        matrix(c(-x[1],-x[2], -1, 0, 0, 0, x[1]*X[1], x[2]*X[1], X[1],0,0, 0, -x[1],-x[2], -1, x[1]*X[2], x[2]*X[2], X[2]), nrow = 2, byrow = TRUE)
    }
    idx <- if (nrow(ref) == 4) 1:4 else c(1, 2, 8, 9)
    PH <- do.call(rbind, lapply(idx, function(i) homography_transform(unlist(ref[i, c("image_x", "image_y")]), unlist(ref[i, c("court_x","court_y")]))))
    PH <- rbind(PH, c(rep(0, 8), 1))
    Hres <- solve(a = PH, b = matrix(c(rep(0, 8), 1), ncol = 1))
    H <- matrix(Hres, ncol = 3, byrow = TRUE)
    tmp <- apply(cbind(x, y, z), 1, function(xx) H %*% matrix(xx, ncol = 1))
    data.frame(x = tmp[1, ] / tmp[3, ], y = tmp[2, ] / tmp[3, ])
}
