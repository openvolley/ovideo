
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ovideo

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
![openvolley](https://img.shields.io/badge/openvolley-darkblue.svg?logo=data:image/svg%2bxml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyMTAiIGhlaWdodD0iMjEwIj48cGF0aCBkPSJNOTcuODMzIDE4Ny45OTdjLTQuNTUtLjM5Ni0xMi44MTItMS44ODYtMTMuNTgxLTIuNDQ5LS4yNDItLjE3Ny0xLjY5Mi0uNzUzLTMuMjIyLTEuMjgxLTI4LjY5Ni05Ljg5NS0zNS4xNy00NS45ODctMTMuODY4LTc3LjMyMyAyLjY3Mi0zLjkzIDIuNTc5LTQuMTktMS4zOTQtMy45MDYtMTIuNjQxLjktMjcuMiA2Ljk1Mi0zMy4wNjYgMTMuNzQ1LTUuOTg0IDYuOTI3LTcuMzI3IDE0LjUwNy00LjA1MiAyMi44NjIuNzE2IDEuODI2LS45MTgtLjE3LTEuODktMi4zMS03LjM1Mi0xNi4xNzQtOS4xODEtMzguNTYtNC4zMzctNTMuMDc0LjY5MS0yLjA3IDEuNDE1LTMuODY2IDEuNjEtMy45ODkuMTk0LS4xMjMuNzgyLTEuMDUzIDEuMzA3LTIuMDY2IDMuOTQ1LTcuNjE3IDkuNDU4LTEyLjg2MiAxNy44MzktMTYuOTcgMTIuMTcyLTUuOTY4IDI1LjU3NS01LjgyNCA0MS40My40NDUgNi4zMSAyLjQ5NSA4LjgwMiAzLjgwMSAxNi4wNDcgOC40MTMgNC4zNCAyLjc2MiA0LjIxMiAyLjg3NCAzLjU5NC0zLjE3My0yLjgyNi0yNy42ODEtMTYuOTA3LTQyLjE4NS0zNi4wNjgtMzcuMTUxLTQuMjU0IDEuMTE3IDUuMjQtMy4zMzggMTEuNjYtNS40NzMgMTMuMTgtNC4zOCAzOC45MzctNS43NzIgNDYuMDc0LTEuNDg4IDEuMjQ3LjU0NyAyLjIyOCAxLjA5NSAzLjI3NSAxLjYzIDQuMjkgMi4xMDcgMTEuNzMzIDcuNjk4IDE0LjI2NSAxMS40MjcuNDA3LjYgMS4yNyAxLjg2NiAxLjkxNyAyLjgxNCAxMS4zMDggMTYuNTY1IDguNjIzIDQxLjkxLTYuODM4IDY0LjU1Mi0zLjI0OSA0Ljc1OC0zLjI1OCA0Ljc0MiAyLjQ1IDQuMDE4IDMyLjQ4Mi00LjEyMiA0OC41MTUtMjEuOTM1IDM5LjU3OC00My45NzQtMS4xNC0yLjgwOSAxLjU2NiAxLjA2IDMuNTE4IDUuMDMyIDI5LjY5MyA2MC40MTctMjIuNTggMTA3Ljg1My03OS40OTggNzIuMTQzLTUuMDg0LTMuMTktNS4xMjMtMy4xNTItMy45MDIgMy44ODMgNC43MjEgMjcuMjIgMjUuNzgzIDQzLjU2MiA0NC4wODkgMzQuMjEgMS4zNjItLjY5NiAyLjIxLS43NSAyLjIxLS4xNDMtNi43NiAzLjg1Ny0xNi4wMTggNi41NTMtMjMuMTI2IDguMDkxLTcuNTU1IDEuNTQ3LTE4LjM2NiAyLjE3Mi0yNi4wMiAxLjUwNnoiIGZpbGw9IiNmZmYiLz48ZWxsaXBzZSBjeD0iMTA1Ljk3NSIgY3k9IjEwNC40NDEiIHJ4PSI5NC44NCIgcnk9IjkyLjU0MiIgZmlsbD0ibm9uZSIgc3Ryb2tlPSIjZmZmIiBzdHJva2Utd2lkdGg9IjEwLjc0Ii8+PC9zdmc+)
[![R-CMD-check](https://github.com/openvolley/ovideo/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/openvolley/ovideo/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Installation

``` r
install.packages("ovideo", repos = c("https://openvolley.r-universe.dev",
                                    "https://cloud.r-project.org"))

## or

## install.packages("remotes") ## if needed
remotes::install_github("openvolley/ovideo")
```

## Example usage

Create a standalone HTML file that plays a video playlist.

Start by loading our scout file, which is bundled with the `ovdata`
package.

``` r
library(ovdata) ## install via install.packages("ovdata", repos = c("https://openvolley.r-universe.dev", "https://cloud.r-project.org"))
library(ovideo)
x <- ovdata_example("190301_kats_beds-clip", as = "parsed")
```

We need to make sure that the video metadata element points somewhere
meaningful. We can either use a local video file as the source:

``` r
dv_meta_video(x) <- ovdata_example_video("190301_kats_beds")
```

But if we use a local video file, we can’t share the resulting HTML file
with anyone else. Instead we can use an online video URL, in this case
on YouTube:

``` r
dv_meta_video(x) <- "https://youtu.be/4YH89aSlc6M"
```

And now on with the rest of the process:

``` r
## extract the plays
px <- datavolley::plays(x)
## it's a single rally, so we'll use all rows (just exclude NA skill rows)
px <- px[!is.na(px$skill), ]

## define columns to show in the table
extra_cols <- c("home_team", "visiting_team", "video_time", "code", "set_number",
                "home_team_score", "visiting_team_score")

## make the playlist with extra columns included
ply <- ov_video_playlist(px, x$meta, extra_cols = c(extra_cols, "player_name"))

## use player name as the subtitle
ply$subtitle <- ply$player_name

## convert to HTML
f <- ov_playlist_to_html(ply, table_cols = extra_cols)

## and finally open it!
browseURL(f)
```

And you should see something like:

<img src="https://raw.githubusercontent.com/openvolley/ovideo/master/man/figures/ovideo_html_player.png" style="border:1px solid black;" />
