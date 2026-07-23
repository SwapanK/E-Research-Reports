
################################################################################
# FILE   5 OF 12
################################################################################
# Name: VulsenAPP_html_report.R
# Path: R/VulsenAPP_utils/VulsenAPP_html_report.R
# Type: R
# Size: 0 MB
# Lines: 53
################################################################################


save_html_report <- function(obj, file) {
  
  tmp_dir <- tempdir()
  
  data_file <- file.path(tmp_dir, "input_data.rds")
  saveRDS(obj, data_file)
  
  rmarkdown::render(
    input = "templates/vulsens_report.Rmd",   #   NOW referencing real file
    output_file = file,
    params = list(data_path = data_file),
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )
}


# save_html_report <- function(obj, side_by_side = FALSE, file) {
#   plots <- build_all_static_gplots(obj, side_by_side = side_by_side)
#   tmp_dir <- tempfile("vulsens_html_")
#   dir.create(tmp_dir, recursive = TRUE)
#   img_paths <- c()
#   sections <- list()
#   add_section_plots <- function(plot_list, title_prefix) {
#     sec_tags <- list()
#     for (nm in intersect(PLOT_ORDER, names(plot_list))) {
#       safe <- gsub("[^A-Za-z0-9_]+", "_", paste0(title_prefix, "_", nm))
#       png_file <- file.path(tmp_dir, paste0(safe, ".png"))
#       ggplot2::ggsave(png_file, plot_list[[nm]], width = 15, height = 9, dpi = 180, limitsize = FALSE)
#       img_paths <<- c(img_paths, png_file)
#       sec_tags[[length(sec_tags) + 1]] <- htmltools::tags$div(class="plot-card",
#         htmltools::tags$h3(nm),
#         htmltools::tags$img(src = basename(png_file), style = "max-width:100%;height:auto;")
#       )
#     }
#     sec_tags
#   }
#   body <- htmltools::tagList(
#     htmltools::tags$head(htmltools::tags$link(rel = "stylesheet", href = "custom.css")),
#     htmltools::tags$div(class = "app-shell",
#       render_header(APP_TITLE),
#       htmltools::tags$div(class = "info-panel", htmltools::tags$h3("Standalone HTML report"), htmltools::tags$p("Generated from the app using the current uploaded dataset and comparison mode.")),
#       htmltools::tags$h2("Regionwise Comparison"),
#       add_section_plots(plots$region, "Region"),
#       htmltools::tags$h2("Statewise Comparison"),
#       add_section_plots(plots$state, "State"),
#       htmltools::tags$h2("Percentage Change Comparison"),
#       add_section_plots(plots$pct, "Pct")
#     )
#   )
#   # copy css and images to temp dir
#   file.copy("www/css/custom.css", file.path(tmp_dir, "custom.css"), overwrite = TRUE)
#   htmltools::save_html(body, file = file, libdir = NULL, background = "white")
# }



