% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shiny_courtref.R
\name{ov_shiny_court_ref}
\alias{ov_shiny_court_ref}
\title{A shiny app to define a court reference}
\usage{
ov_shiny_court_ref(
  image_file,
  video_file,
  t = 60,
  existing_ref = NULL,
  launch_browser = getOption("shiny.launch.browser", interactive()),
  ...
)
}
\arguments{
\item{image_file}{string: path to an image file (jpg) containing the court image (not required if \code{video_file} is supplied)}

\item{video_file}{string: path to a video file from which to extract the court image (not required if \code{image_file} is supplied)}

\item{t}{numeric: the time of the video frame to use as the court image (not required if \code{image_file} is supplied)}

\item{existing_ref}{list: (optional) the output from a previous call to \code{\link[=ov_shiny_court_ref]{ov_shiny_court_ref()}}, which can be edited}

\item{launch_browser}{logical: if \code{TRUE}, launch the app in the system browser}

\item{...}{: additional parameters (currently ignored)}
}
\value{
A list containing the reference information
}
\description{
A shiny app to define a court reference
}
\examples{
if (interactive()) {
  ## define a court reference from scratch
  ov_shiny_court_ref(video_file = ov_example_video(), t = 5)

  ## or modify an existing one
  crt <- data.frame(image_x = c(0.05397063, 0.95402573, 0.75039756, 0.28921230),
                    image_y = c(0.02129301, 0.02294600, 0.52049712, 0.51884413),
                    court_x = c(0.5, 3.5, 3.5, 0.5),
                    court_y = c(0.5, 0.5, 6.5, 6.5))
  ref <- list(court_ref = crt, net_height = 2.43)
  ov_shiny_court_ref(video_file = ov_example_video(), t = 5, existing_ref = ref)
}

}
