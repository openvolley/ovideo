s_courtref_ui <- function(app_data) {
    fluidPage(
        tags$script("Shiny.addCustomMessageHandler('evaljs', function(jsexpr) { eval(jsexpr) });"), ## handler for running js code directly
        tags$head(
            tags$style("#headerblock {border-radius:14px; padding:3px; margin-bottom:5px; min-height:120px; color:black; border: 1px solid #000766; background:#000766; background: linear-gradient(90deg, rgba(0,7,102,1) 0%, rgba(255,255,255,1) 65%, rgba(255,255,255,1) 100%);} #headerblock h1, #headerblock h2, #headerblock h3, #headerblock h4 {color:#fff;} h5 {font-weight: bold;}"),
            if (!is.null(app_data$css)) tags$style(app_data$css)
        ),
        if (!is.null(app_data$ui_header)) {
            app_data$ui_header
        } else {
            fluidRow(id = "headerblock", column(6, tags$h2("Court reference")),
                     column(3, offset = 3, tags$div(style = "text-align: center;", "Part of the", tags$br(), tags$img(src = "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyMTAiIGhlaWdodD0iMjEwIj48cGF0aCBkPSJNOTcuODMzIDE4Ny45OTdjLTQuNTUtLjM5Ni0xMi44MTItMS44ODYtMTMuNTgxLTIuNDQ5LS4yNDItLjE3Ny0xLjY5Mi0uNzUzLTMuMjIyLTEuMjgxLTI4LjY5Ni05Ljg5NS0zNS4xNy00NS45ODctMTMuODY4LTc3LjMyMyAyLjY3Mi0zLjkzIDIuNTc5LTQuMTktMS4zOTQtMy45MDYtMTIuNjQxLjktMjcuMiA2Ljk1Mi0zMy4wNjYgMTMuNzQ1LTUuOTg0IDYuOTI3LTcuMzI3IDE0LjUwNy00LjA1MiAyMi44NjIuNzE2IDEuODI2LS45MTgtLjE3LTEuODktMi4zMS03LjM1Mi0xNi4xNzQtOS4xODEtMzguNTYtNC4zMzctNTMuMDc0LjY5MS0yLjA3IDEuNDE1LTMuODY2IDEuNjEtMy45ODkuMTk0LS4xMjMuNzgyLTEuMDUzIDEuMzA3LTIuMDY2IDMuOTQ1LTcuNjE3IDkuNDU4LTEyLjg2MiAxNy44MzktMTYuOTcgMTIuMTcyLTUuOTY4IDI1LjU3NS01LjgyNCA0MS40My40NDUgNi4zMSAyLjQ5NSA4LjgwMiAzLjgwMSAxNi4wNDcgOC40MTMgNC4zNCAyLjc2MiA0LjIxMiAyLjg3NCAzLjU5NC0zLjE3My0yLjgyNi0yNy42ODEtMTYuOTA3LTQyLjE4NS0zNi4wNjgtMzcuMTUxLTQuMjU0IDEuMTE3IDUuMjQtMy4zMzggMTEuNjYtNS40NzMgMTMuMTgtNC4zOCAzOC45MzctNS43NzIgNDYuMDc0LTEuNDg4IDEuMjQ3LjU0NyAyLjIyOCAxLjA5NSAzLjI3NSAxLjYzIDQuMjkgMi4xMDcgMTEuNzMzIDcuNjk4IDE0LjI2NSAxMS40MjcuNDA3LjYgMS4yNyAxLjg2NiAxLjkxNyAyLjgxNCAxMS4zMDggMTYuNTY1IDguNjIzIDQxLjkxLTYuODM4IDY0LjU1Mi0zLjI0OSA0Ljc1OC0zLjI1OCA0Ljc0MiAyLjQ1IDQuMDE4IDMyLjQ4Mi00LjEyMiA0OC41MTUtMjEuOTM1IDM5LjU3OC00My45NzQtMS4xNC0yLjgwOSAxLjU2NiAxLjA2IDMuNTE4IDUuMDMyIDI5LjY5MyA2MC40MTctMjIuNTggMTA3Ljg1My03OS40OTggNzIuMTQzLTUuMDg0LTMuMTktNS4xMjMtMy4xNTItMy45MDIgMy44ODMgNC43MjEgMjcuMjIgMjUuNzgzIDQzLjU2MiA0NC4wODkgMzQuMjEgMS4zNjItLjY5NiAyLjIxLS43NSAyLjIxLS4xNDMtNi43NiAzLjg1Ny0xNi4wMTggNi41NTMtMjMuMTI2IDguMDkxLTcuNTU1IDEuNTQ3LTE4LjM2NiAyLjE3Mi0yNi4wMiAxLjUwNnoiIGZpbGw9IiMwMDA3NjYiLz48ZWxsaXBzZSBjeD0iMTA1Ljk3NSIgY3k9IjEwNC40NDEiIHJ4PSI5NC44NCIgcnk9IjkyLjU0MiIgZmlsbD0ibm9uZSIgc3Ryb2tlPSIjMDAwNzY2IiBzdHJva2Utd2lkdGg9IjEwLjc0Ii8+PC9zdmc+", style = "max-height:3em;"), tags$br(), tags$a(href = "https://openvolley.org/", "openvolley", target = "_blank"), "project")))
        },
        tags$hr(),
        s_courtref_ui_main(app_data)
    )
}

s_courtref_ui_main <- function(app_data) {
    tagList(fluidRow(column(9, plotOutput("srplot", height = "80vh", click = "sr_plot_click", hover = hoverOpts("sr_plot_hover", delay = 50, delayType = "throttle"))), ##height = paste0(ph, "px"))
                     column(3, uiOutput("srui_table"),
                            tags$hr(),
                            shiny::fixedRow(if (isTRUE(app_data$include_net)) column(6, textInput("net_height", label = "Net height (m):", value = if (!is.null(app_data$ref) && !is.null(app_data$ref$net_height) && !is.na(app_data$ref$net_height)) app_data$ref$net_height else "", width = "10ex")),
                                            column(6, textInput("video_framerate", label = "Video frame rate:", value = if (!is.null(app_data$ref) && !is.null(app_data$ref$video_framerate) && !is.na(app_data$ref$video_framerate)) app_data$ref$video_framerate else "", width = "10ex"))),
                            tags$hr(),
                            actionButton("exit_app", "Exit app"))
                     )
            )
}

s_courtref_server <- function(app_data) {
    function(input, output, session) {
        DEBUG <- 0L
        ## define points on court and their corresponding coordinates
        if (!is.null(app_data$court_refs_data)) {
            court_refs_data <- app_data$court_refs_data
        } else {
            court_refs_data <- tibble(pos = c("nlb", "nrb", "nl3", "nr3", "lm", "rm", "fl3", "fr3", "flb", "frb", "lnt", "rnt"),
                                      lab = c("Near left baseline corner", "Near right baseline corner", "Left end of near 3m line", "Right end of near 3m line", "Left end of the midline", "Right end of the midline", "Left end of far 3m line", "Right end of far 3m line", "Far left baseline corner", "Far right baseline corner", "Left top of the net", "Right top of the net"),
                                      court_x = c(0.5, 3.5, 0.5, 3.5, 0.5, 3.5, 0.5, 3.5, 0.5, 3.5, 0.5, 3.5),
                                      court_y = c(0.5, 0.5, 2.5, 2.5, 3.5, 3.5, 4.5, 4.5, 6.5, 6.5, 3.5, 3.5))
        }

        ## crvt holds the edited court ref data
        ## initially populate this from app_data
        ## TODO add net_height, possible video width, height, framerate
        crvt <- reactiveValues(court = if (!is.null(app_data$ref) && !is.null(app_data$ref$court_ref))
                                           left_join(app_data$ref$court_ref, court_refs_data[, c("court_x", "court_y", "pos")], by = c("court_x", "court_y")) ## add pos col
                                       else
                                           tibble(image_x = rep(NA_real_, 4), image_y = NA_real_, court_x = NA_real_, court_y = NA_real_, pos = NA_character_),
                               antenna = if (!is.null(app_data$ref) && !is.null(app_data$ref$antenna) && nrow(app_data$ref$antenna) == 4)
                                             app_data$ref$antenna
                                         else
                                             tibble(image_x = rep(NA_real_, 4), image_y = NA_real_,  antenna = c("left", "right", "right", "left"), where = c(rep("floor", 2), rep("net_top", 2))),
                               net_height = if (!is.null(app_data$ref) && !is.null(app_data$ref$net_height)) app_data$ref$net_height else NA_real_,
                               video_height = if (!is.null(app_data$ref) && !is.null(app_data$ref$video_height)) app_data$ref$video_height else NA_integer_,
                               video_width = if (!is.null(app_data$ref) && !is.null(app_data$ref$video_width)) app_data$ref$video_width else NA_integer_,
                               video_framerate = if (!is.null(app_data$ref) && !is.null(app_data$ref$video_framerate)) app_data$ref$video_framerate else NA_integer_)

        observeEvent(input$exit_app, {
            ref <- reactiveValuesToList(crvt)
            ref$court_ref <- dplyr::select(left_join(dplyr::select(ref$court, -"court_x", -"court_y"), court_refs_data[, c("court_x", "court_y", "pos")], by = "pos"), -"pos")
            ref$court <- NULL ## want it named court_ref
            if (!isTRUE(app_data$include_net)) {
                ref$antenna <- NULL
                ref$net_height <- NULL
            }
            shiny::stopApp(ref)
        })

        ## helper function to build dropdown inputs for ref positions
        cr_dropdown <- function(id, n, what = NULL) {
            chc <- setNames(court_refs_data$pos, court_refs_data$lab)
            def_sel <- c(1, 2, 10, 9, 6, 7, 11, 12)
            sel <- if (!is.null(what) && what %in% chc) what else chc[def_sel[n]]
            selectInput(id, label = paste0("Reference point ", n), choices = chc, selected = sel, multiple = FALSE)
        }

        ## the table on the right of the UI with the ref position definitions
        output$srui_table <- renderUI({
            ## transfer crvt values into ui
            cr <- crvt$court
            ant <- crvt$antenna
            do.call(tags$div, ## the four court ref points can vary
                    c(lapply(1:4, function(n) cr_dropdown(paste0("crdd", n), n = n, what = if (n <= nrow(cr)) cr$pos[n] else NULL)),
                      ## antenna points are fixed
                      if (isTRUE(app_data$include_net)) list(tags$div(tags$strong("Reference point 5"), "Left end of the midline"),
                                                            tags$div(tags$strong("Reference point 6"), "Right end of the midline"),
                                                            tags$div(tags$strong("Reference point 7"), "Top of net at right antenna"),
                                                            tags$div(tags$strong("Reference point 8"), "Top of net at left antenna"))
                      ))
        })
        ## watch these inputs
        observeEvent(input$crdd1, {if (nrow(crvt$court) > 0) { crvt$court$pos[1] <- input$crdd1; } })
        observeEvent(input$crdd2, {if (nrow(crvt$court) > 1) { crvt$court$pos[2] <- input$crdd2; } })
        observeEvent(input$crdd3, {if (nrow(crvt$court) > 2) { crvt$court$pos[3] <- input$crdd3; } })
        observeEvent(input$crdd4, {if (nrow(crvt$court) > 3) { crvt$court$pos[4] <- input$crdd4; } })
        observe({
            crvt$net_height <- if (!is.null(input$net_height) && nzchar(input$net_height)) as.numeric(input$net_height) else NA_real_
        })
        observe({
            crvt$video_framerate <- if (nzchar(input$video_framerate)) as.numeric(input$video_framerate) else NA_real_
        })

        crox <- reactive({
            tryCatch({
                cr <- crvt$court
                ## account for changes in dropdowns, i.e. the image location might now be assigned to a different court ref location
                if (!is.null(cr)) cr <- left_join(dplyr::select(cr, -"court_x", -"court_y"), court_refs_data[, c("court_x", "court_y", "pos")], by = "pos")
                if (!is.null(app_data$overlay_data_function)) {
                    out <- app_data$overlay_data_function(court_ref = cr, space = "image", crop = TRUE)
                } else {
                    out <- ovideo::ov_overlay_data(zones = FALSE, serve_zones = FALSE, space = "image", court_ref = cr, crop = TRUE)
                }
                out$courtxy <- dplyr::rename(out$courtxy, image_x = "x", image_y = "y")
                out
            }, error = function(e) NULL)
        })
        srplotdat <- throttle(reactive({
            antenna_colour <- "magenta"
            court_colour <- "red"
            if (!is.null(crimg()$image)) {
                ## plot in 0,1 norm coords
                p <- ggplot2::ggplot(mapping = aes_string(x = "image_x", y = "image_y")) +
                    ggplot2::annotation_custom(grid::rasterGrob(crimg()$image), xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
                    ggplot2::coord_fixed(ratio = crimg()$height/crimg()$width, xlim = c(0, 1), ylim = c(0, 1))
                if (!is.null(crox())) {
                    p <- p + geom_segment(data = crox()$courtxy, aes_string(xend = "xend", yend = "yend"), color = court_colour) + theme_bw()
                }
                if (!is.null(crvt$court)) {
                    p <- p + geom_label(data = mutate(crvt$court, point_num = paste0("  ", row_number(), "  ")), ## double check that point_num always matches the UI inputs ordering
                                        aes_string(label = "point_num"), color = "white", fill = court_colour, hjust = "outward", vjust = "outward")
                }
                if (isTRUE(app_data$include_net) && !is.null(crvt$antenna)) {
                    plotx <- mutate(crvt$antenna, n = case_when(.data$antenna == "left" & .data$where == "floor" ~ 5L,
                                                                .data$antenna == "right" & .data$where == "floor" ~ 6L,
                                                                .data$antenna == "right" & .data$where == "net_top" ~ 7L,
                                                                .data$antenna == "left" & .data$where == "net_top" ~ 8L))
                    p <- p + geom_path(data = plotx, aes_string(group = "antenna"), color = antenna_colour) +
                        geom_path(data = plotx[plotx$where == "net_top", ], color = antenna_colour) +
                        geom_path(data = plotx[plotx$where == "floor", ], color = antenna_colour) +
                        geom_label(data = plotx, aes_string(label = "n"), color = "white", fill = antenna_colour, hjust = "outward", vjust = "outward")
                }
                p + ggplot2::theme_void()
            } else {
                NULL
            }
        }), 500)
        output$srplot <- renderPlot({
            srplotdat()
        })
        evaljs("$('#srplot').mouseup(function() { Shiny.setInputValue('did_sr_plot_mouseup', new Date().getTime()) }); $('#srplot').mousedown(function() { Shiny.setInputValue('did_sr_plot_mousedown', new Date().getTime()) });")

        crimg <- reactive({
            if (!is.null(app_data$image)) {
                tryCatch({
                    img <- if (is.character(app_data$image)) jpeg::readJPEG(app_data$image, native = TRUE) else app_data$image
                    crvt$video_width <- dim(img)[2]
                    crvt$video_height <- dim(img)[1]
                    list(image = img, width = dim(img)[2], height = dim(img)[1])
                }, error = function(e) {
                    NULL
                })
            } else {
                list(image = NULL, width = NA_integer_, height = NA_integer_)
            }
        })

        sr_clickdrag <- reactiveValues(mousedown = NULL, closest_down = NULL, mouseup = NULL)
        observeEvent(input$did_sr_plot_mousedown, {
            closest <- NULL
            if (!is.null(input$sr_plot_click)) {
                ##px <- c(input$sr_plot_click$x, input$sr_plot_click$y)
                ## somehow the click location is slightly out of whack with the hover location, which breaks our drag detection!
                px <- c(input$sr_plot_hover$x, input$sr_plot_hover$y)
                isolate({
                    refpts <- bind_rows(mutate(crvt$court, what = "court", rownum = row_number()),
                                        mutate(crvt$antenna, what = "antenna", rownum = row_number() + 4L))
                    if (nrow(refpts) > 0) {
                        closest <- refpts$rownum[which.min(sqrt((refpts$image_x - px[1])^2 + (refpts$image_y - px[2])^2))]
                    }
                })
            } else {
                px <- NULL
            }
            sr_clickdrag$mousedown <- px
            if (DEBUG > 0L) cat("\nmouse down at: ", px, "\n")
            sr_clickdrag$closest_down <- closest
        })

        was_drag <- function(start, end) {
            if (DEBUG > 1L) {
                cat("start: ", start, "\n")
                cat("end: ", end, "\n")
            }
            if (is.null(start) || is.null(end)) {
                FALSE
            } else {
                mmt <- sqrt(sum(start - end)^2)
                if (DEBUG > 0L) cat("movement: ", mmt, "\n")
                mmt > 0.0001
            }
        }

        observeEvent(input$did_sr_plot_mouseup, {
            ## was it a click and not a drag?
            if (!is.null(sr_clickdrag$mousedown)) {
                isolate(px <- last_mouse_pos())
                if (is.null(px) || !was_drag(sr_clickdrag$mousedown, px)) {
                    ##if (is.null(px)) px <- sr_clickdrag$mousedown ## if hover is lagging use mouse down
                    if (DEBUG > 1L) cat("click\n")
                    ## enter new point if there is an empty slot, or ignore
                    if (is.null(crvt$court) || nrow(crvt$court) < 4) {
                        warning("empty crvt$court??")
                    } else if (any(is.na(crvt$court$image_x))) {
                        next_pt <- min(which(is.na(crvt$court$image_x)))
                        crvt$court[next_pt, c("image_x", "image_y")] <- as.list(px)
                        crvt$court$pos[next_pt] <- input[[paste0("crdd", next_pt)]]
                        ## TODO court_x and court_y here need updating
                    } else if (isTRUE(app_data$include_net)) {
                        if (is.null(crvt$antenna) || nrow(crvt$antenna) < 4) {
                            warning("empty crvt$antenna??")
                        } else if (any(is.na(crvt$antenna$image_x))) {
                            next_pt <- min(which(is.na(crvt$antenna$image_x)))
                            crvt$antenna[next_pt, c("image_x", "image_y")] <- as.list(px)
                        }
                    }
                } else {
                    if (DEBUG > 1L) cat("drag or null start/end point\n")
                    ## do nothing
                }
            }
            ## stop dragging
            isolate({
                sr_clickdrag$mousedown <- NULL
                sr_clickdrag$closest_down <- NULL
            })
        })

        last_mouse_pos <- reactiveVal(NULL)
        observeEvent(input$sr_plot_hover, {
            ## triggered when mouse moved over the plot
            px <- c(input$sr_plot_hover$x, input$sr_plot_hover$y)
            if (!is.null(px)) {
                if (DEBUG > 1L) cat("updating mouse pos: ", px, "\n")
                last_mouse_pos(px)
            }
        })
        last_refresh_time <- NA_real_
        observe({
            px <- last_mouse_pos() ##c(input$sr_plot_hover$x, input$sr_plot_hover$y) 
            if (!is.null(px) && !is.null(sr_clickdrag$mousedown) && was_drag(sr_clickdrag$mousedown, px)) {
                ## did previously click, so now dragging a point
                now_time <- R.utils::System$currentTimeMillis()
                if (is.na(last_refresh_time) || (now_time - last_refresh_time) > 300) {
                    ## debounce
                    last_refresh_time <<- now_time
                    refpts <- bind_rows(mutate(crvt$court, what = "court", rownum = row_number()),
                                        mutate(crvt$antenna, what = "antenna", rownum = row_number()))
                    if (nrow(refpts) > 0 && length(sr_clickdrag$closest_down) > 0) {
                        closest <- sr_clickdrag$closest_down
                        if (refpts$what[closest] == "court") {
                            crvt$court[refpts$rownum[closest], c("image_x", "image_y")] <- as.list(px)
                        } else {
                            crvt$antenna[refpts$rownum[closest], c("image_x", "image_y")] <- as.list(px)
                        }
                    }
                } else {
                    invalidateLater(100)
                }
            }
        })


    }
}

#' A shiny app to define a court reference
#'
#' @param image_file string: path to an image file (jpg) containing the court image (not required if `video_file` is supplied)
#' @param video_file string: path to a video file from which to extract the court image (not required if \code{image_file} is supplied)
#' @param t numeric: the time of the video frame to use as the court image (not required if \code{image_file} is supplied)
#' @param existing_ref list: (optional) the output from a previous call to [ov_shiny_court_ref()], which can be edited
#' @param launch_browser logical: if `TRUE`, launch the app in the system browser
#' @param ... : additional parameters (currently ignored)
#'
#' @return A list containing the reference information
#'
#' @examples
#' if (interactive()) {
#'   ## define a court reference from scratch
#'   ov_shiny_court_ref(video_file = ov_example_video(), t = 5)
#'
#'   ## or modify an existing one
#'   crt <- data.frame(image_x = c(0.05397063, 0.95402573, 0.75039756, 0.28921230),
#'                     image_y = c(0.02129301, 0.02294600, 0.52049712, 0.51884413),
#'                     court_x = c(0.5, 3.5, 3.5, 0.5),
#'                     court_y = c(0.5, 0.5, 6.5, 6.5))
#'   ref <- list(court_ref = crt, net_height = 2.43)
#'   ov_shiny_court_ref(video_file = ov_example_video(), t = 5, existing_ref = ref)
#' }
#'
#' @export
ov_shiny_court_ref <- function(image_file, video_file, t = 60, existing_ref = NULL, launch_browser = getOption("shiny.launch.browser", interactive()), ...) {
    if (missing(image_file) || is.null(image_file)) {
        image_file <- ov_video_frame(video_file, t)
    }
    if (!missing(video_file)) {
        framerate <- av::av_video_info(video_file)$video$framerate
        if (is.null(existing_ref)) {
            existing_ref <- list(video_framerate = framerate)
        } else if (is.null(existing_ref$video_framerate) || is.na(existing_ref$video_framerate)) {
            existing_ref$video_framerate <- framerate
        }
    }
    app_data <- list(image = image_file, ref = existing_ref, ...)
    if (!"include_net" %in% names(app_data)) app_data$include_net <- TRUE
    this_app <- list(ui = s_courtref_ui(app_data = app_data), server = s_courtref_server(app_data = app_data))
    shiny::runApp(this_app, display.mode = "normal", launch.browser = launch_browser)
}
