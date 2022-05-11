#' Generate data suitable for creating a court overlay plot
#'
#' @param zones logical: if \code{TRUE}, show zone lines
# @param subzones logical: if \code{TRUE}, show subzone lines
# @param cones logical or string: either \code{FALSE} (do not show cones) or one or more of "L", "R", or "M" to show the cones for "L"eft or "R"ight-side or "M"iddle attacks
#' @param serve_zones logical: if \code{TRUE}, show the serve zones behind the baselines
#' @param labels logical: if \code{TRUE}, label the zones
#' @param space string: if "court", the data will be in court coordinates. If "image", the data will be transformed to image coordinates via \code{\link{ov_transform_points}}
#' @param court_ref data.frame: as returned by \code{\link{ov_get_court_ref}}. Only required if \code{space} is "image"
#' @param crop logical: if \code{space} is "image", and \code{crop} is TRUE, the data will be cropped to the c(0, 1, 0, 1) bounding box (i.e. the limits of the image, in normalized coordinates)
#'
#' @return A list of data.frames
#'
#' @seealso \code{\link{ov_overlay_image}}
#' @export
ov_overlay_data <- function(zones = TRUE, serve_zones = TRUE, labels = FALSE, space = "court", court_ref, crop = TRUE) {
    cones = FALSE
    assert_that(is.flag(zones), !is.na(zones))
    assert_that(is.flag(serve_zones), !is.na(serve_zones))
    assert_that(is.flag(labels), !is.na(labels))
    assert_that(is.flag(cones), !is.na(cones))
    assert_that(is.string(space))
    space <- match.arg(tolower(space), c("court", "image"))
    assert_that(is.flag(crop), !is.na(crop))
    ##subzones = FALSE,
    labxy <- NULL
    polyxy <- NULL
    ## outer and 3m lines
    cxy <- data.frame(x = c(rep(0.5, 3), 0.5, 3.5, 0.5, 0.5),
                      xend = c(rep(3.5, 3), 0.5, 3.5, 3.5, 3.5),
                      y = c(0.5, 3.5, 6.5, 0.5, 0.5, 2.5, 4.5),
                      yend = c(0.5, 3.5, 6.5, 6.5, 6.5, 2.5, 4.5),
                      width = 1.0)
    ## serve zones
    if (serve_zones) {
        szlen <- 0.25 ## length of serve zone lines - make this at least 1 once clipping is implemented
        sxy <- data.frame(x = c(0.5, 1.1, 1.7, 2.3, 2.9, 3.5),
                      xend = c(0.5, 1.1, 1.7, 2.3, 2.9, 3.5),
                      y = rep(0.5, 6), yend = rep(0.5-szlen, 6), width = 0.75)
        cxy <- bind_rows(cxy, sxy)
        sxy[, c("x", "y")] <- dv_flip_xy(sxy[, c("x", "y")])
        sxy[, c("xend", "yend")] <- dv_flip_xy(sxy[, c("xend", "yend")])
        cxy <- bind_rows(cxy, sxy)
    }

    if (cones) {
        stop("no cones yet")
##        sz <- if (rdata$dvw$plays$start_zone[ridx] %in% c(4, 7, 5)) "L" else if (rdata$dvw$plays$start_zone[ridx] %in% c(3, 8, 6)) "M" else "R"
##        polyxy <- dv_cone_polygons(zone = sz, end = "upper")
##        Nc <- max(polyxy$cone_number)
##        polyxy$cone_number <- paste0(polyxy$cone_number, "U")
##        polyxy <- bind_rows(polyxy, mutate(dv_cone_polygons(zone = sz, end = "lower"), cone_number = paste0(.data$cone_number, "L")))
##        ## labels
##        labxy <- mutate(dv_cone2xy(rdata$dvw$plays$start_zone[ridx], end_cones = seq_len(Nc), end = "upper", xynames = c("x", "y")), label = row_number())
##        labxy <- bind_rows(labxy, mutate(dv_cone2xy(rdata$dvw$plays$start_zone[ridx], end_cones = seq_len(Nc), end = "lower", xynames = c("x", "y")), label = row_number()))
    }

    ## other zone lines
    if (zones) {
        cxy <- bind_rows(cxy, data.frame(x = c(0.5, 0.5, 1.5, 2.5),
                                         xend = c(3.5, 3.5, 1.5, 2.5),
                                         y = c(1.5, 5.5, 0.5, 0.5),
                                         yend = c(1.5, 5.5, 6.5, 6.5),
                                         width = 0.75))
        if (labels) {
            labxy <- data.frame(x = rep(c(1, 2, 3), 6), y = as.numeric(matrix(1:6, nrow = 3, ncol = 6, byrow = TRUE)),
                                label = c(5, 6, 1, 7, 8, 9, 4, 3, 2, 2, 3, 4, 9, 8, 7, 1, 6, 5))
        }
    }
    if (space == "image") {
        cxy[, c("x", "y")] <- ovideo::ov_transform_points(cxy[, c("x", "y")], ref = court_ref, direction = "to_image")
        cxy[, c("xend", "yend")] <- setNames(ovideo::ov_transform_points(cxy[, c("xend", "yend")], ref = court_ref, direction = "to_image"), c("xend", "yend"))
        if (!is.null(labxy)) {
            labxy[, c("x", "y")] <- ovideo::ov_transform_points(labxy[, c("x", "y")], ref = court_ref, direction = "to_image")
        }
        if (!is.null(polyxy)) {
            polyxy[, c("x", "y")] <- ovideo::ov_transform_points(polyxy[, c("x", "y")], ref = court_ref, direction = "to_image")
        }
        if (crop) {
            crop_seg <- function(z) {
                crpd <- clip_segment(z$x, z$y, z$xend, z$yend)
                z$x <- crpd[1]
                z$xend <- crpd[3]
                z$y <- crpd[2]
                z$yend <- crpd[4]
                z
            }
            cxy <- bind_rows(lapply(seq_len(nrow(cxy)), function(ii) crop_seg(cxy[ii, ])))
        }
    }
    list(courtxy = cxy, polyxy = polyxy, labxy = labxy)
}


#' Generate a court overlay image showing court boundary, 3m, zone, and other lines
#'
#' @param court_ref data.frame: as returned by \code{\link{ov_get_court_ref}}
#' @param height integer: height of image to produce in pixels
#' @param width integer: width of image to produce in pixels
#' @param filename string: image filename (png). If missing, a file will be created in the temporary directory
#' @param ... : arguments passed to \code{\link{ov_overlay_data}}
#'
#' @return The path to the generated file.
#'
#' @export
ov_overlay_image <- function(court_ref, height, width, filename, ...) {
    if (missing(filename) || is.null(filename) || !nzchar(filename) || is.na(filename)) {
        filename <- tempfile(fileext = ".png")
    }
    odata <- ov_overlay_data(..., space = "image", court_ref = court_ref)
    gg_tight <- list(theme(legend.position = "none", panel.background = element_rect(fill = "transparent", colour = NA), plot.background = element_rect(fill = "transparent", color = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.spacing = unit(0, "null"), plot.margin = rep(unit(0, "null"), 4), axis.ticks = element_blank(), axis.ticks.length = unit(0, "null"), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()), scale_x_continuous(limits = c(0, 1), expand = c(0, 0)), scale_y_continuous(limits = c(0, 1), expand = c(0, 0)))

    p <- ggplot(odata$courtxy, aes_string("x", "y", xend = "xend", yend = "yend", size = "width")) + geom_segment(color = "blue") + gg_tight + scale_size_continuous(range = c(0.5, 1.0))
    if (!is.null(odata$polyxy)) p <- p + geom_polygon(data = odata$polyxy, aes_string(x = "x", y = "y", group = "cone_number"), inherit.aes = FALSE, color = "blue", fill = NA)
    if (!is.null(odata$labxy)) {
        p <- p + geom_label(data = odata$labxy, aes_string(x = "x", y = "y", label = "label"), inherit.aes = FALSE, color = "blue", hjust = 0.5, vjust = 0.5)#, size = 1.5)
    }
    p <- p + gg_tight
    png(filename, height = height, width = width, bg = "transparent")
    print(p)
    dev.off()
    filename
}

## cohen-sutherland algorithm to clip a line segment to a bounding box
## line goes from (x0, y0) to (x1, y1)
## bbox is [xmin, xmax, ymin, ymax]
## https://en.wikipedia.org/wiki/Cohen%E2%80%93Sutherland_algorithm
clip_segment <- function(x0, y0, x1, y1, bbox = c(0, 1, 0, 1)) {
    bit <- list(inside = 0, left = 1, right = 2, bottom = 4, top = 8)
    outcode0 <- compute_out_code(x0, y0, bbox)
    outcode1 <- compute_out_code(x1, y1, bbox)
    accept <- FALSE
    while (TRUE) {
        if (!bitwOr(outcode0, outcode1)) {
            ## bitwise OR is 0: both points inside window; trivially accept and exit loop
            accept <- TRUE
            break
        } else if (bitwAnd(outcode0, outcode1)) {
            ## bitwise AND is not 0: both points share an outside zone (LEFT, RIGHT, TOP, or BOTTOM), so both must be outside window; exit loop (accept is false)
            break
        } else {
            ## failed both tests, so calculate the line segment to clip from an outside point to an intersection with clip edge
            ## At least one endpoint is outside the clip rectangle; pick it
            outcode_out <- if (outcode1 > outcode0) outcode1 else outcode0

            ## Now find the intersection point;
            ## use formulas:
            ##   slope = (y1 - y0) / (x1 - x0)
            ##   x = x0 + (1 / slope) * (ym - y0), where ym is ymin or ymax
            ##   y = y0 + slope * (xm - x0), where xm is xmin or xmax
            ## No need to worry about divide-by-zero because, in each case, the
            ## outcode bit being tested guarantees the denominator is non-zero
            if (bitwAnd(outcode_out, bit$top)) {  ## point is above the clip window
                x <- x0 + (x1 - x0) * (bbox[4] - y0) / (y1 - y0)
                y <- bbox[4]
            } else if (bitwAnd(outcode_out, bit$bottom)) { ## point is below the clip window
                x <- x0 + (x1 - x0) * (bbox[3] - y0) / (y1 - y0)
                y <- bbox[3]
            } else if (bitwAnd(outcode_out, bit$right)) {  ## point is to the right of clip window
                y <- y0 + (y1 - y0) * (bbox[2] - x0) / (x1 - x0)
                x <- bbox[2]
            } else if (bitwAnd(outcode_out, bit$left)) {   ## point is to the left of clip window
                y <- y0 + (y1 - y0) * (bbox[1] - x0) / (x1 - x0)
                x <- bbox[1]
            }
            ## Now we move outside point to intersection point to clip and get ready for next pass.
            if (outcode_out == outcode0) {
                x0 <- x
                y0 <- y
                outcode0 <- compute_out_code(x0, y0, bbox)
            } else {
                x1 <- x
                y1 <- y
                outcode1 = compute_out_code(x1, y1, bbox)
            }
        }
    }
    if (accept) c(x0, y0, x1, y1) else rep(NA_real_, 4)
}

compute_out_code <- function(x, y, bbox) {
    bit <- list(inside = 0, left = 1, right = 2, bottom = 4, top = 8)
    code <- bit$inside
    if (x < bbox[1]) code <- bitwOr(code, bit$left) ## to the left of clip window
    else if (x > bbox[2]) code <- bitwOr(code, bit$right) ## to the right of clip window
    if (y < bbox[3]) code <- bitwOr(code, bit$bottom) ## below the clip window
    else if (y > bbox[4]) code <- bitwOr(code, bit$top) ## above the clip window
    code
}
