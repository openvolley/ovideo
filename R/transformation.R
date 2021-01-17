## functions for transforming image coordinates to court coordinates

#' Define the reference points on a court image
#'
#' This function is used to define the reference points on a court image, to be used with [ov_transform_points()].
#' The court coordinate system is that used in [datavolley::dv_court()], [datavolley::ggcourt()], and related functions.
#' Try `plot(c(0, 4), c(0, 7), type = "n", asp = 1); datavolley::dv_court()` or `ggplot2::ggplot() + datavolley::ggcourt() + ggplot2::theme_bw()` for a visual depiction.
#'
#' @param image_file string: path to an image file (jpg) containing the court image (not required if `video_file` is supplied)
#' @param video_file string: path to a video file from which to extract the court image (not required if `image_file` is supplied)
#' @param t numeric: the time of the video frame to use as the court image (not required if `image_file` is supplied)
#' @param type string: currently only "corners"
#'
#' @return A data.frame containing the reference information
#'
#' @seealso [ov_transform_points()], [datavolley::dv_court()],  [datavolley::ggcourt()]
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
#' The court coordinate system is that used in [datavolley::dv_court()], [datavolley::ggcourt()], and related functions.
#' Try `plot(c(0, 4), c(0, 7), type = "n", asp = 1); datavolley::dv_court()` or `ggplot2::ggplot() + datavolley::ggcourt() + ggplot2::theme_bw()` for a visual depiction.
#' Image coordinates are returned as normalized coordinates in the range `[0, 1]`. You may need to scale these by the width and height of the image, depending on how you are plotting things.
#'
#' @param x numeric: input x points. `x` can also be a two-column data.frame or matrix
#' @param y numeric: input y points
#' @param ref data.frame: reference, as returned by [ov_get_court_ref()] or [ov_shiny_court_ref()]
#' @param direction string: either "to_court" (to transform image coordinates to court coordinates) or "to_image" (the reverse) 
#'
#' @return A two-column data.frame with transformed values
#'
#' @seealso [ov_get_court_ref()], [datavolley::dv_court()],  [datavolley::ggcourt()]
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


#' Estimate the camera matrix
#'
#' The camera matrix characterizes the mapping of a camera from 3D real-world coordinates to 2D coordinates in an image.
#'
#' @references <https://en.wikipedia.org/wiki/Camera_matrix>
#' @param X matrix or data.frame: Nx3 matrix of 3D real-world coordinates
#' @param x matrix or data.frame: Nx2 matrix of image coordinates
#'
#' @return A list with components `coef` (fitted transformation coefficients) and `rmse` (root mean squared error of the fitted transformation)
#'
#' @seealso [ov_cmat_apply()]
#'
#' @examples
#'
#' ## define real-world and corresponding image coordinates
#' xX <- dplyr::tribble(~image_x, ~image_y, ~court_x, ~court_y,   ~z,
#'                         0.054,    0.023,      0.5,      0.5,    0, ## near left baseline
#'                         0.951,    0.025,      3.5,      0.5,    0, ## near right baseline
#'                         0.752,    0.519,      3.5,      6.5,    0, ## far right baseline
#'                         0.288,    0.519,      0.5,      6.5,    0, ## far left baseline
#'                         0.199,    0.644,      0.5,      3.5, 2.43, ## left net top
#'                         0.208,    0.349,      0.5,      3.5, 0.00, ## left net floor
#'                         0.825,    0.644,      3.5,      3.5, 2.43, ## right net top
#'                         0.821,    0.349,      3.5,      3.5, 0.00) ## right net floor
#'
#' C <- ov_cmat_estimate(X = xX[, 3:5], x = xX[, 1:2])
#'
#' ## fitted image coordinates using C
#' ov_cmat_apply(C, X = xX[, 3:5])
#'
#' ## compare to actual image positions
#' xX[, 1:2]
#'
#' @export
ov_cmat_estimate <- function(X, x) {
    ## code here adapted from the StereoMorph package and elsewhere
    if (!is.matrix(X)) X <- as.matrix(X)
    if (!is.matrix(x)) x <- as.matrix(x)
    if (nrow(X) != nrow(x)) stop("X, x have different number of rows")
    coef <- matrix(NA_real_, nrow = 11, ncol = 1)
    nna_idx <- (rowSums(is.na(x)) < 1) & (rowSums(is.na(X)) < 1)
    if (sum(nna_idx) < 6) {
        warning("insufficient points to estimate camera matrix")
        return(NULL)
    }
    x <- x[nna_idx, ]
    X <- X[nna_idx, ]
    A <- matrix(NA_real_, nrow = 2 * nrow(X), ncol = 11)
    for (k in seq_len(nrow(X))) {
        A[2 * k - 1, ] <- c(X[k, 1], X[k, 2], X[k, 3], 1, 0, 0, 0, 0, -x[k, 1]*X[k, 1], -x[k, 1]*X[k, 2], -x[k, 1]*X[k, 3])
        A[2 * k, ] <- c(0, 0, 0, 0, X[k, 1], X[k, 2], X[k, 3], 1, -x[k, 2]*X[k, 1], -x[k, 2]*X[k, 2], -x[k, 2]*X[k, 3])
    }
    coef <- solve(t(A) %*% A) %*% (t(A) %*% c(t(x)))
    list(coef = coef, rmse = sqrt(mean((ov_cmat_apply(C = coef, X = X) - x)^2)))
}

#' Apply the camera matrix to 3D coordinates
#'
#' The camera matrix characterizes the mapping of a camera from 3D real-world coordinates to 2D coordinates in an image.
#'
#' @references <https://en.wikipedia.org/wiki/Camera_matrix>
#' @param C : camera matrix as returned by [ov_cmat_estimate()], or the coefficients from that object
#' @param X matrix or data.frame: Nx3 matrix of 3D real-world coordinates
#'
#' @return An Nx2 matrix of image coordinates
#'
#' @seealso [ov_cmat_estimate()]
#'
#' @examples
#'
#' ## define real-world and corresponding image coordinates
#' xX <- dplyr::tribble(~image_x, ~image_y, ~court_x, ~court_y,   ~z,
#'                         0.054,    0.023,      0.5,      0.5,    0, ## near left baseline
#'                         0.951,    0.025,      3.5,      0.5,    0, ## near right baseline
#'                         0.752,    0.519,      3.5,      6.5,    0, ## far right baseline
#'                         0.288,    0.519,      0.5,      6.5,    0, ## far left baseline
#'                         0.199,    0.644,      0.5,      3.5, 2.43, ## left net top
#'                         0.208,    0.349,      0.5,      3.5, 0.00, ## left net floor
#'                         0.825,    0.644,      3.5,      3.5, 2.43, ## right net top
#'                         0.821,    0.349,      3.5,      3.5, 0.00) ## right net floor
#'
#' C <- ov_cmat_estimate(X = xX[, 3:5], x = xX[, 1:2])
#'
#' ## fitted image coordinates using C
#' ov_cmat_apply(C, X = xX[, 3:5])
#'
#' ## compare to actual image positions
#' xX[, 1:2]
#'
#' @export
ov_cmat_apply <- function(C, X) {
    if (is.list(C) && "coef" %in% names(C)) C <- C$coef
    if (!is.matrix(X)) X <- as.matrix(X)
    cbind((X[, 1] * C[1] + X[, 2] * C[2] + X[, 3] * C[3] + C[4])/(X[, 1] * C[9] + X[, 2] * C[10] + X[, 3] * C[11] + 1),
          (X[, 1] * C[5] + X[, 2] * C[6] + X[, 3] * C[7] + C[8])/(X[, 1] * C[9] + X[, 2] * C[10] + X[, 3] * C[11] + 1))
}
