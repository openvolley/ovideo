% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/overlay.R
\name{ov_overlay_data}
\alias{ov_overlay_data}
\title{Generate data suitable for creating a court overlay plot}
\usage{
ov_overlay_data(
  zones = TRUE,
  serve_zones = TRUE,
  labels = FALSE,
  space = "court",
  court_ref,
  crop = TRUE
)
}
\arguments{
\item{zones}{logical: if \code{TRUE}, show zone lines}

\item{serve_zones}{logical: if \code{TRUE}, show the serve zones behind the baselines}

\item{labels}{logical: if \code{TRUE}, label the zones}

\item{space}{string: if "court", the data will be in court coordinates. If "image", the data will be transformed to image coordinates via \code{\link{ov_transform_points}}}

\item{court_ref}{data.frame: as returned by \code{\link{ov_get_court_ref}}. Only required if \code{space} is "image"}

\item{crop}{logical: if \code{space} is "image", and \code{crop} is TRUE, the data will be cropped to the c(0, 1, 0, 1) bounding box (i.e. the limits of the image, in normalized coordinates)}
}
\value{
A list of data.frames
}
\description{
Generate data suitable for creating a court overlay plot
}
\seealso{
\code{\link{ov_overlay_image}}
}
