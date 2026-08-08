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
  
  # Path updated to www/
  rmarkdown::render(
    input = "www/vulsens_report.Rmd",   # now referencing the file in www/
    output_file = file,
    params = list(data_path = data_file),
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )
}


# (commented-out old code removed)

