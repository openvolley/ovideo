#' \pkg{ovideo}
#'
#' Playlists and other video support functions from volleyball match files.
#'
#' @name ovideo
#' @docType package
#' @importFrom assertthat assert_that has_name is.flag is.string
#' @importFrom datavolley read_dv dv_example_file dv_flip_xy dv_write
#' @importFrom dplyr %>% .data bind_rows case_when group_by lag lead left_join mutate row_number tibble ungroup
#' @importFrom DT datatable
#' @importFrom ggplot2 aes_string element_blank element_rect geom_segment geom_label geom_path geom_polygon ggplot scale_size_continuous scale_x_continuous scale_y_continuous theme theme_bw unit coord_fixed ggtitle element_text geom_point
#' @importFrom graphics locator plot points rasterImage image layout contour par text
#' @importFrom grDevices dev.off png
#' @importFrom htmltools HTML tagList tags
#' @importFrom shiny actionButton column fluidPage fluidRow hoverOpts icon invalidateLater isolate observe observeEvent plotOutput reactive reactiveVal reactiveValues reactiveValuesToList renderPlot renderUI selectInput textInput throttle uiOutput
#' @importFrom stats na.omit quantile setNames
#' @importFrom utils tail combn
"_PACKAGE"
