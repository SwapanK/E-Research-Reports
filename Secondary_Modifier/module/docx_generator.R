
## =============================================================================
## DOCX GENERATOR MODULE  (module/docx_generator.R)  -- SCIENTIFIC REPORT THEME
## =============================================================================
## DOCX GENERATOR MODULE  (module/docx_generator.R)  -- SCIENTIFIC REPORT THEME
## =============================================================================
## Builds a Word (.docx) report from any cart .rds file that follows the
## standard VulSen cart item structure (see module/cart_utils.R).
##
## This is the "paper" counterpart to module/ppt_generator.R: instead of a
## slide deck it produces a scrollable, print-ready scientific report --
## title page, an auto-updating table of contents, and one numbered section
## per cart item with a captioned figure and a block-quoted commentary.
##
## Design system (built entirely on officer's default Word template, so no
## extra .dotx dependency is required):
##   - Clean title page: logo (natural shape), report title, byline, a thin
##     accent rule
##   - Native Word Table of Contents field (updates when opened in Word /
##     right-click -> Update Field)
##   - "Heading 1" per cart item: "01. <Module>"
##   - Figures placed with an italic, muted caption: "Figure 1. <Module>"
##   - Commentary rendered as an indented, italic block-quote -- the
##     scientific-paper equivalent of an abstract/notes callout
##   - Consistent muted footer line: report name + generation date
##
## FIX (this revision): officer's built-in "heading 1" / "heading 2" Word
## styles ship with their own automatic outline numbering (numId=3 in the
## template's numbering.xml). Because we ALSO write a manual number into the
## heading text ("01. Vulnerability"), Word rendered a double number --
## "1.  01. Vulnerability" -- once the document was opened. .docx_strip_
## heading_numbering() below runs after the file is saved and removes the
## <w:numPr> auto-numbering from those styles, so only our manual "01."
## prefix (which also drives the readable TOC entries) shows up.
##
## FIX 2 (this revision): the previous implementation of .docx_strip_
## heading_numbering() rewrote styles.xml correctly but then re-zipped the
## .docx with utils::zip() *after* setwd()-ing into a temp extraction
## folder. utils::zip() shells out to an external "zip" binary -- on a
## plain Windows R install (no Rtools / no zip.exe on PATH, e.g. the
## C:/Users/hp/... setup this app ships from) that call fails silently
## (non-zero status, no R error), so the rewritten styles.xml was never
## written back into the real output file and the untouched, still-
## double-numbered .docx shipped to the user. Rewritten below to use the
## 'zip' R package instead (a hard dependency of officer itself, so it is
## always available, pure-R, and needs no external tool or working-
## directory juggling), and to always target an absolute path.
##
## Requires the 'officer' package:  install.packages("officer")
## =============================================================================

library(ggplot2)

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || (is.character(a) && !nzchar(a))) b else a
}

## -----------------------------------------------------------------------
## THEME -- kept close to .vulsen_theme (module/ppt_generator.R) so the
## PPT / DOCX / HTML exports all feel like the same brand family.
## -----------------------------------------------------------------------
.vulsen_docx_theme <- list(
  navy       = "#0F1B3D",
  accent     = "#00C9A7",
  text_dark  = "#1A1A2E",
  text_mute  = "#7A8296",
  rule_gray  = "#D8DCE6",
  font_head  = "Cambria",     # serif -- scientific / print feel
  font_body  = "Calibri"
)

## -----------------------------------------------------------------------
## Internal: fit an image into a max_w x max_h box in inches, preserving
## its real aspect ratio. Shared logic with ppt_generator.R's .fit_dims_in;
## duplicated here (private name) so this file has no load-order dependency
## on module/ppt_generator.R.
## -----------------------------------------------------------------------
.docx_fit_dims_in <- function(path, max_w, max_h, fallback_ratio = 2.2) {
  ext  <- tolower(tools::file_ext(path))
  dims <- tryCatch({
    if (ext == "png" && requireNamespace("png", quietly = TRUE)) {
      d <- dim(png::readPNG(path)); c(w = d[2], h = d[1])
    } else if (ext %in% c("jpg", "jpeg") && requireNamespace("jpeg", quietly = TRUE)) {
      d <- dim(jpeg::readJPEG(path)); c(w = d[2], h = d[1])
    } else NULL
  }, error = function(e) NULL)

  ratio <- if (is.null(dims)) fallback_ratio else unname(dims["w"] / dims["h"])

  if (ratio >= max_w / max_h) {
    c(width = max_w, height = max_w / ratio)
  } else {
    c(width = max_h * ratio, height = max_h)
  }
}

## -----------------------------------------------------------------------
## Internal: a thin horizontal accent rule, added as a bordered empty
## paragraph (no external image needed).
## -----------------------------------------------------------------------
.docx_add_rule <- function(doc, color, width_pt = 1.25, space_after = 10) {
  officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(""),
      fp_p = officer::fp_par(
        border.bottom = officer::fp_border(color = color, width = width_pt),
        padding.bottom = space_after
      )
    )
  )
}

## -----------------------------------------------------------------------
## Internal (NEW): strip the automatic outline numbering (<w:numPr>) that
## officer's default "heading 1"/"heading 2" styles carry, so it can't
## stack with the manual "01. <Module>" prefix we already write into the
## heading text. Runs as a light post-process directly on the saved .docx
## (which is just a zip of XML parts), so it needs no extra dependency
## beyond utils::zip/unzip (both base R).
## -----------------------------------------------------------------------
.docx_strip_heading_numbering <- function(docx_path) {

  ## Resolve to an absolute path up front. generate_cart_docx() is usually
  ## called with a Shiny downloadHandler's `file` argument (already
  ## absolute), but resolving here too makes this function safe on its own.
  docx_path <- normalizePath(docx_path, mustWork = TRUE)

  extract_dir <- tempfile("vulsen_docx_fix_")
  dir.create(extract_dir)
  utils::unzip(docx_path, exdir = extract_dir)

  styles_path <- file.path(extract_dir, "word", "styles.xml")

  if (file.exists(styles_path)) {
    xml_txt <- paste(readLines(styles_path, warn = FALSE, encoding = "UTF-8"), collapse = "")

    ## Only touch styles named "heading 1"/"heading 2" (matched via their
    ## <w:name w:val="heading N"/> tag), not every numPr in the document --
    ## this keeps the fix targeted and safe even if other numbered lists
    ## are added to the template later.
    xml_txt <- gsub(
      '(<w:name w:val="heading [12]"/>(?:(?!</w:style>).)*?)<w:numPr>.*?</w:numPr>',
      "\\1",
      xml_txt,
      perl = TRUE
    )

    writeLines(xml_txt, styles_path, useBytes = TRUE)
  }

  ## FIX: previously this used utils::zip(), which shells out to an
  ## external "zip" binary and (after a setwd() into extract_dir) wrote
  ## the rebuilt archive to a *relative* path -- on any machine without a
  ## zip executable on PATH (the common case on a plain Windows R install,
  ## e.g. the C:/Users/hp/... setup this app ships from) that call fails
  ## silently and the original, un-stripped .docx is left in place. That
  ## is why the shipped report still showed "1.  01. Vulnerability".
  ##
  ## zip::zip() is a pure-R re-implementation with no external-tool
  ## dependency, and it's already a hard dependency of 'officer' (already
  ## required above), so it needs no extra install. Both the archive path
  ## and its contents are absolute, so this no longer depends on the
  ## working directory or a setwd().
  if (!requireNamespace("zip", quietly = TRUE)) {
    warning(
      "Package 'zip' is not available \u2014 could not remove the ",
      "duplicate Word outline numbering from the heading styles. The ",
      "report was still generated, but headings may show a double ",
      "number (e.g. '1.  01. Vulnerability')."
    )
    return(invisible(docx_path))
  }

  all_files <- list.files(extract_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)

  zip::zip(zipfile = docx_path, files = all_files, root = extract_dir)

  invisible(docx_path)
}

## -----------------------------------------------------------------------
## generate_cart_docx()
##
## cart_path    : path to a cart .rds file (e.g. cart/<username>_cart.rds)
## output_path  : where to write the .docx (e.g. a downloadHandler's `file`)
## username     : shown on the title page
## report_title : title shown on the title page
## logo_path    : optional logo image placed on the title page, natural shape
## theme        : list of colors/fonts, defaults to .vulsen_docx_theme
## -----------------------------------------------------------------------
generate_cart_docx <- function(
    cart_path,
    output_path,
    username     = "User",
    report_title = "VulSen Analytics Report",
    logo_path    = "www/logo.png",
    theme        = .vulsen_docx_theme
) {

  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("The 'officer' package is required to generate DOCX files. Install it with install.packages('officer').")
  }

  if (!file.exists(cart_path)) {
    stop("Cart file not found: ", cart_path)
  }

  cart_items <- readRDS(cart_path)

  if (length(cart_items) == 0) {
    stop("Cart is empty \u2014 nothing to export.")
  }

  th      <- theme
  tmp_dir <- tempdir()

  doc <- officer::read_docx()

  ## -----------------------------------------------------------------------
  ## TITLE PAGE
  ## -----------------------------------------------------------------------

  if (!is.null(logo_path) && file.exists(logo_path)) {
    dims <- .docx_fit_dims_in(logo_path, max_w = 1.8, max_h = 1.1)
    doc <- officer::body_add_img(
      doc, src = logo_path,
      width = unname(dims["width"]), height = unname(dims["height"]),
      style = "centered"
    )
  }

  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(report_title, officer::fp_text(
        font.size = 30, bold = TRUE, color = th$navy, font.family = th$font_head
      )),
      fp_p = officer::fp_par(text.align = "center", padding.top = 24)
    )
  )

  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext("A Scientific Vulnerability Analytics Report", officer::fp_text(
        font.size = 13, italic = TRUE, color = th$text_mute, font.family = th$font_body
      )),
      fp_p = officer::fp_par(text.align = "center", padding.top = 4, padding.bottom = 20)
    )
  )

  doc <- .docx_add_rule(doc, color = th$accent, width_pt = 1.75, space_after = 18)

  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(paste0("Prepared for: ", username), officer::fp_text(
        font.size = 12, bold = TRUE, color = th$text_dark, font.family = th$font_body
      )),
      fp_p = officer::fp_par(text.align = "center", padding.top = 6)
    )
  )

  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(
        paste0(format(Sys.Date(), "%B %d, %Y"), "   \u2022   ",
               length(cart_items), " item(s) included"),
        officer::fp_text(font.size = 11, color = th$text_mute, font.family = th$font_body)
      ),
      fp_p = officer::fp_par(text.align = "center", padding.top = 2)
    )
  )

  doc <- officer::body_add_break(doc)

  ## -----------------------------------------------------------------------
  ## TABLE OF CONTENTS  (native Word field, built from Heading 1 styles
  ## used below -- right-click it in Word and choose "Update Field" after
  ## any manual edits)
  ## -----------------------------------------------------------------------

  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext("Contents", officer::fp_text(
        font.size = 18, bold = TRUE, color = th$navy, font.family = th$font_head
      )),
      fp_p = officer::fp_par(padding.top = 4, padding.bottom = 8)
    )
  )
  doc <- .docx_add_rule(doc, color = th$rule_gray, width_pt = 0.75, space_after = 8)
  doc <- officer::body_add_toc(doc, level = 1)
  doc <- officer::body_add_break(doc)

  ## -----------------------------------------------------------------------
  ## ONE NUMBERED SECTION PER CART ITEM
  ## -----------------------------------------------------------------------

  for (i in seq_along(cart_items)) {

    item  <- cart_items[[i]]
    label <- item$module %||% "Item"
    ts    <- tryCatch(format(item$timestamp, "%Y-%m-%d %H:%M"), error = function(e) "")

    ## ---- Section heading: "01. <Module>" (drives the TOC entry) ----
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(sprintf("%02d. %s", i, label), officer::fp_text(
          font.size = 20, bold = TRUE, color = th$navy, font.family = th$font_head
        )),
        fp_p = officer::fp_par(padding.top = 14, padding.bottom = 2)
      ),
      style = "heading 1"
    )

    if (nzchar(ts)) {
      doc <- officer::body_add_fpar(
        doc,
        officer::fpar(
          officer::ftext(paste0("Logged ", ts), officer::fp_text(
            font.size = 10, italic = TRUE, color = th$text_mute, font.family = th$font_body
          )),
          fp_p = officer::fp_par(padding.bottom = 8)
        )
      )
    }

    doc <- .docx_add_rule(doc, color = th$rule_gray, width_pt = 0.5, space_after = 10)

    ## ---- Figure, with a numbered scientific caption ----
    if (!is.null(item$plot)) {

      img_path <- tempfile(pattern = paste0("vulsen_docx_plot_", i, "_"),
                            tmpdir = tmp_dir, fileext = ".png")

      ## Render at the exact width/height/dpi captured when this item was
      ## added to the cart (see the "Capture dimensions for cart rendering"
      ## block in server/secmod_server.R and its counterpart in
      ## server/vulnerability_server.R). ggplot sizes text, legends, and
      ## margins relative to the physical save dimensions, so silently
      ## re-rendering at a fixed 8.5x4.9 here -- instead of what the user
      ## actually dialed in on the preview -- would distort every theme
      ## customization even though the underlying plot object is unchanged.
      item_w   <- item$width  %||% 8.5
      item_h   <- item$height %||% 4.9
      ## `max(...)` enforces a 300dpi floor even if a lower dpi was
      ## captured in the cart (the Secmod page's own dpi input defaults to
      ## 150), while still honoring a higher dpi if the user set one.
      item_dpi <- max(item$dpi %||% 300, 300)

      ## Honor the transparency choice captured when this plot was themed
      ## (see attr(p, "vulsen_bg") set in apply_plot_overrides()), so a
      ## plot exported with a transparent background on-screen stays
      ## transparent in the exported document instead of silently
      ## reverting to white.
      item_bg <- attr(item$plot, "vulsen_bg") %||% item$bg %||% "white"

      ggplot2::ggsave(
        filename = img_path,
        plot     = item$plot,
        width    = item_w, height = item_h, dpi = item_dpi, bg = item_bg
      )

      ## Fit that true-aspect-ratio PNG into the figure box, preserving its
      ## aspect ratio (same helper already used above for the logo), rather
      ## than forcing a fixed 6.3in x 3.63in box that could stretch or
      ## squash a differently-proportioned cart plot.
      fig_dims <- .docx_fit_dims_in(img_path, max_w = 6.3, max_h = 4.2)

      doc <- officer::body_add_img(
        doc, src = img_path,
        width  = unname(fig_dims["width"]),
        height = unname(fig_dims["height"]),
        style  = "centered"
      )

      doc <- officer::body_add_fpar(
        doc,
        officer::fpar(
          officer::ftext(sprintf("Figure %d. ", i), officer::fp_text(
            bold = TRUE, italic = TRUE, font.size = 10, color = th$text_dark,
            font.family = th$font_body
          )),
          officer::ftext(label, officer::fp_text(
            italic = TRUE, font.size = 10, color = th$text_mute, font.family = th$font_body
          )),
          fp_p = officer::fp_par(text.align = "center", padding.top = 4, padding.bottom = 14)
        )
      )
    }

    ## ---- Commentary, rendered as an indented italic block-quote ----
    if (!is.null(item$commentary) && nzchar(item$commentary)) {

      doc <- officer::body_add_fpar(
        doc,
        officer::fpar(
          officer::ftext("Commentary", officer::fp_text(
            bold = TRUE, font.size = 12, color = th$navy, font.family = th$font_head
          )),
          fp_p = officer::fp_par(padding.top = 4, padding.bottom = 4)
        )
      )

      lines <- strsplit(item$commentary, "\n")[[1]]
      lines <- lines[nzchar(trimws(lines))]

      for (ln in lines) {
        doc <- officer::body_add_fpar(
          doc,
          officer::fpar(
            officer::ftext(trimws(ln), officer::fp_text(
              italic = TRUE, font.size = 11, color = th$text_dark, font.family = th$font_body
            )),
            fp_p = officer::fp_par(
              text.align   = "left",
              padding.left = 18,
              padding.bottom = 6,
              border.left  = officer::fp_border(color = th$accent, width = 1.5)
            )
          )
        )
      }
    }

    if (i < length(cart_items)) {
      doc <- officer::body_add_break(doc)
    }
  }

  ## -----------------------------------------------------------------------
  ## CLOSING / FOOTER LINE
  ## -----------------------------------------------------------------------

  doc <- officer::body_add_break(doc)
  doc <- .docx_add_rule(doc, color = th$rule_gray, width_pt = 0.5, space_after = 6)
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(
        paste0("VulSen Analytics \u00b7 Generated ", format(Sys.Date(), "%B %d, %Y")),
        officer::fp_text(font.size = 9, italic = TRUE, color = th$text_mute, font.family = th$font_body)
      ),
      fp_p = officer::fp_par(text.align = "center")
    )
  )

  ## -----------------------------------------------------------------------
  ## SAVE  (+ post-process to remove the double-numbering issue)
  ## -----------------------------------------------------------------------

  print(doc, target = output_path)
  .docx_strip_heading_numbering(output_path)

  invisible(output_path)
}