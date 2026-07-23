## =============================================================================
## HTML (R MARKDOWN) GENERATOR MODULE  (module/rmd_generator.R)
## =============================================================================
## Knits module/vulsen_report_template.Rmd into a self-contained, brandable
## .html report from any cart .rds file that follows the standard VulSen
## cart item structure (see module/cart_utils.R).
##
## This is the "web" counterpart to module/ppt_generator.R (slides) and
## module/docx_generator.R (Word) -- same content, same navy/teal branding,
## rendered as a single-page HTML document with a left-hand table of
## contents (via output: html_document; toc: true / toc_float: false --
## see FIX 2 below).
##
## FIX (this revision): the template's YAML front matter no longer sets
## `title:` / `author:` / `date:` -- those fields made Pandoc render an
## automatic header block above EVERYTHING else in the body, including the
## logo, regardless of source order. The template now builds its own
## masthead (logo -> title -> byline) by hand. The only side effect is
## that Pandoc no longer knows what to put in the <title> tag (browser
## tab), so that's restored here explicitly via pandoc_args.
##
## FIX 2 (this revision): toc_float: true handed the entire TOC over to a
## client-side jQuery plugin (jquery.tocify.js) -- the static #TOC div
## Pandoc would otherwise populate at render time shipped completely
## EMPTY in the HTML source and only filled in by scanning headings in
## the browser once its scripts ran. In any viewer that strips or blocks
## <script> tags (several corporate mail clients, some "preview as HTML"
## tools, HTML sanitizers, JS disabled), that left an empty, effectively
## invisible sidebar -- i.e. "HTML is missing TOC in left". toc_float is
## now off below so Pandoc bakes real links into #TOC at compile time
## (no JS needed); the template's own CSS pins that static TOC to the
## left with plain position: sticky so it keeps the same floating look.
##
## Requires the 'rmarkdown' package (and, for a fully self-contained file
## with embedded images/CSS, the 'pandoc' binary that ships with RStudio /
## most R installs):  install.packages("rmarkdown")
## =============================================================================

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || (is.character(a) && !nzchar(a))) b else a
}

## -----------------------------------------------------------------------
## generate_cart_html()
##
## cart_path     : path to a cart .rds file (e.g. cart/<username>_cart.rds)
## output_path   : where to write the .html (e.g. a downloadHandler's `file`)
## username      : shown in the report byline
## report_title  : title shown at the top of the report AND in the browser
##                 tab (set via pandoc_args -- see FIX note above)
## logo_path     : optional logo image embedded at the top, natural shape
##                 (no circular crop -- see .vulsen-logo CSS in the template)
## template_path : path to the .Rmd template to knit; defaults to the file
##                 shipped alongside this module
## -----------------------------------------------------------------------
generate_cart_html <- function(
    cart_path,
    output_path,
    username      = "User",
    report_title  = "VulSen Analytics Report",
    logo_path     = "www/logo-removebg-preview.png",
    template_path = file.path("R", "vulsen_report_template.Rmd")
) {

  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("The 'rmarkdown' package is required to generate HTML files. Install it with install.packages('rmarkdown').")
  }

  if (!file.exists(cart_path)) {
    stop("Cart file not found: ", cart_path)
  }

  if (!file.exists(template_path)) {
    stop("Rmd template not found: ", template_path,
         " (expected at module/vulsen_report_template.Rmd)")
  }

  cart_items <- readRDS(cart_path)

  if (length(cart_items) == 0) {
    stop("Cart is empty \u2014 nothing to export.")
  }

  ## rmarkdown::render() knits with its working directory set to the
  ## template's own folder (module/) by default, not the Shiny app's
  ## working directory. That breaks any relative path (cart_path,
  ## logo_path) passed in as a param -- hence "cannot open the
  ## connection" inside the setup chunk. Fix: resolve everything to
  ## absolute paths *before* handing them to render(), and pin
  ## knit_root_dir to the app's actual working directory as a
  ## belt-and-braces safeguard.
  app_wd       <- getwd()
  cart_path    <- normalizePath(cart_path, mustWork = TRUE)
  logo_path    <- if (!is.null(logo_path) && nzchar(logo_path) && file.exists(logo_path)) {
    normalizePath(logo_path, mustWork = TRUE)
  } else {
    NULL
  }

  output_path <- normalizePath(output_path, mustWork = FALSE)
  out_dir     <- dirname(output_path)
  out_file    <- basename(output_path)

  rmarkdown::render(
    input          = template_path,
    output_file    = out_file,
    output_dir     = out_dir,
    knit_root_dir  = app_wd,
    intermediates_dir = tempdir(),
    output_format  = rmarkdown::html_document(
      toc        = TRUE,
      ## FALSE so Pandoc emits a real, populated <div id="TOC"> at
      ## compile time instead of an empty one meant to be filled by
      ## client-side JS -- see FIX 2 note above. NB: this argument
      ## overrides whatever the .Rmd's own YAML says, so it has to be
      ## kept in sync with the toc_float setting in
      ## vulsen_report_template.Rmd.
      toc_float  = FALSE,
      theme      = "flatly",
      highlight  = "tango",
      df_print   = "kable",
      self_contained = TRUE,
      ## Sets <title> (the browser tab) now that the template no longer
      ## has a YAML `title:` field for Pandoc to pick that up from.
      pandoc_args = c("--metadata", paste0("pagetitle:", report_title))
    ),
    params = list(
      cart_path    = cart_path,
      username     = username,
      report_title = report_title,
      logo_path    = logo_path %||% ""
    ),
    envir        = new.env(parent = globalenv()),
    quiet        = TRUE
  )

  invisible(output_path)
}


################################################################################
