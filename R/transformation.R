## functions for transforming image coordinates to court coordinates

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
  if (is.null(this)) return(NULL)
  imx <- this$x
  imy <- this$y
  points(this$x, this$y, col = 3)
  cat("Click the near right baseline corner\n")
  this <- locator(1)
  if (is.null(this)) return(NULL)
  points(this$x, this$y, col = 3)
  imx <- c(imx, this$x)
  imy <- c(imy, this$y)
  refx <- c(0.5, 3.5); refy <- c(0.5, 0.5)
  if (type == "10p") {
    ## near 3m line right side, centre line right side, centre line left side, near 3m line left side, far 3m line right side
  }
  cat("Click the far right baseline corner\n")
  this <- locator(1)
  if (is.null(this)) return(NULL)
  points(this$x, this$y, col = 3)
  imx <- c(imx, this$x)
  imy <- c(imy, this$y)
  cat("Click the far left baseline corner\n")
  this <- locator(1)
  if (is.null(this)) return(NULL)
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
#' Image coordinates are returned as normalized coordinates in the range \code{[0, 1]}. You may need to scale these by the width and height of the image, depending on how you are plotting things.
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
#' ## the ref data for the example image
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