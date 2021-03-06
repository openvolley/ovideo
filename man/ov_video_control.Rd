% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/player.R
\name{ov_video_control}
\alias{ov_video_control}
\title{Functions for controlling the video player}
\usage{
ov_video_control(what, ...)
}
\arguments{
\item{what}{string: the command, currently one of:
\itemize{
\item "play" (note that this requires that the playlist has already been loaded)
\item "stop"
\item "pause"
\item "prev"
\item "next"
\item "jog" - move the video forward or backwards by a given number of seconds (pass this value as the \code{...} argument)
\item "set_playback_rate" - set the playback rate: 1 = normal speed, 2 = double speed, etc
}}

\item{...}{: parameters used by those commands. For version 2 of the video controller, \code{...} must include \code{controller_var = "my_controller_var"}}
}
\description{
The video element and the controls provided by this function are javascript-based, and so are probably most useful in Shiny apps.
}
\examples{
\dontrun{
  ov_video_control("jog", -1) ## rewind 1s
  ov_video_control("jog", 10) ## jump forwards 10s
  ov_video_control("set_playback_rate", 0.5) ## play at half speed
}
}
