% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tags.R
\name{ov_get_video_meta}
\alias{ov_get_video_meta}
\title{Retrieve metadata tags from a video file}
\usage{
ov_get_video_meta(video_file, debug = FALSE)
}
\arguments{
\item{video_file}{string: path to the video file}

\item{debug}{logical: if \code{TRUE}, echo the ffmpeg output to the console}
}
\value{
A named list of metadata values
}
\description{
Requires that ffmpeg is available on your system path.
}
\examples{
\dontrun{
  newfile <- ov_set_video_meta(ov_example_video(), comment = "A comment")
  ov_get_video_meta(newfile)
}

}
\seealso{
\code{\link[=ov_set_video_meta]{ov_set_video_meta()}}
}
