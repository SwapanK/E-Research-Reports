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
##   - Commentary rendered as an indented, italic block-quote
##   - Closing footer line: report name + generation date
##
## FIX: remove double numbering, and set updateFields so TOC page numbers
## are correct on open.
##
## Requires the 'officer' package:  install.packages("officer")
## =============================================================================

library(ggplot2)

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || (is.character(a) && !nzchar(a))) b else a
}

## -----------------------------------------------------------------------
## APP COLOUR PALETTE (unified across all reports)
## -----------------------------------------------------------------------
app_palette <- list(
  primary   = "#6FACDE",
  accent    = "#0075BC",
  highlight = "#0075BC",
  dark      = "#0075BC",
  navy      = "#0F1B3D",
  text_dark = "#1A1A2E",
  text_mute = "#7A8296",
  rule_gray = "#D8DCE6",
  font_head = "Cambria",
  font_body = "Calibri"
)

## -----------------------------------------------------------------------
## THEME -- using app_palette
## -----------------------------------------------------------------------
.vulsen_docx_theme <- list(
  navy       = app_palette$navy,
  accent     = app_palette$accent,
  highlight  = app_palette$highlight,
  primary    = app_palette$primary,
  dark       = app_palette$dark,
  text_dark  = app_palette$text_dark,
  text_mute  = app_palette$text_mute,
  rule_gray  = app_palette$rule_gray,
  font_head  = app_palette$font_head,
  font_body  = app_palette$font_body
)

## -----------------------------------------------------------------------
## Internal: fit an image into a max_w x max_h box in inches, preserving
## its real aspect ratio.
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
## paragraph.
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
## Internal: strip the automatic outline numbering (<w:numPr>) that
## officer's default "heading 1"/"heading 2" styles carry.
## -----------------------------------------------------------------------
.docx_strip_heading_numbering <- function(docx_path) {
  
  docx_path <- normalizePath(docx_path, mustWork = TRUE)
  
  extract_dir <- tempfile("vulsen_docx_fix_")
  dir.create(extract_dir)
  utils::unzip(docx_path, exdir = extract_dir)
  
  styles_path <- file.path(extract_dir, "word", "styles.xml")
  
  if (file.exists(styles_path)) {
    xml_txt <- paste(readLines(styles_path, warn = FALSE, encoding = "UTF-8"), collapse = "")
    
    xml_txt <- gsub(
      '(<w:name w:val="heading [12]"/>(?:(?!</w:style>).)*?)<w:numPr>.*?</w:numPr>',
      "\\1",
      xml_txt,
      perl = TRUE
    )
    
    writeLines(xml_txt, styles_path, useBytes = TRUE)
  }
  
  ## ---- Also set updateFields in settings.xml so TOC page numbers refresh ----
  ## This tells Word "recalculate fields when this document is opened" at
  ## the document level (most Word versions honour it silently; a few will
  ## show an "update fields?" prompt instead of doing it automatically).
  settings_path <- file.path(extract_dir, "word", "settings.xml")
  if (file.exists(settings_path)) {
    settings_txt <- paste(readLines(settings_path, warn = FALSE, encoding = "UTF-8"), collapse = "")
    # Insert <w:updateFields w:val="true"/> inside <w:settings> if not present
    if (!grepl('<w:updateFields\\s+w:val="true"', settings_txt)) {
      settings_txt <- gsub(
        '(</w:settings>)',
        '  <w:updateFields w:val="true"/>\\1',
        settings_txt
      )
      writeLines(settings_txt, settings_path, useBytes = TRUE)
    }
  }

  ## ---- Mark every field (TOC entries, PAGEREF, and the footer's PAGE /
  ## NUMPAGES fields) as "dirty" so Word recalculates them unconditionally
  ## on open, regardless of the app-level "update automatically" setting
  ## above. Belt-and-braces: this is what actually kills the "TOC page
  ## numbers all show 1" symptom in practice, since the settings.xml
  ## switch alone is not honoured by every Word build/viewer. ----
  field_xml_paths <- c(
    file.path(extract_dir, "word", "document.xml"),
    Sys.glob(file.path(extract_dir, "word", "footer*.xml")),
    Sys.glob(file.path(extract_dir, "word", "header*.xml"))
  )
  field_xml_paths <- field_xml_paths[file.exists(field_xml_paths)]

  for (fp in field_xml_paths) {
    ftxt <- paste(readLines(fp, warn = FALSE, encoding = "UTF-8"), collapse = "")
    ftxt <- gsub(
      '<w:fldChar w:fldCharType="begin"\\s*/>',
      '<w:fldChar w:fldCharType="begin" w:dirty="true"/>',
      ftxt
    )
    writeLines(ftxt, fp, useBytes = TRUE)
  }

  if (!requireNamespace("zip", quietly = TRUE)) {
    warning(
      "Package 'zip' is not available \u2014 could not remove the ",
      "duplicate Word outline numbering or update TOC/page-number fields. ",
      "The report was still generated, but headings may show a double ",
      "number and TOC/footer page numbers may be incorrect until the ",
      "person manually selects all (Ctrl+A) and presses F9 in Word."
    )
    return(invisible(docx_path))
  }
  
  all_files <- list.files(extract_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  zip::zip(zipfile = docx_path, files = all_files, root = extract_dir)
  
  invisible(docx_path)
}

## -----------------------------------------------------------------------
## Internal: render the plot customization metadata (colors, font sizes,
## legend, background, dimensions, DPI) that was active when a cart item's
## plot was added to the cart, as a compact one-line settings string.
##
## item$customization is optional -- older cart items saved before this
## field existed simply won't have it, and this function returns the doc
## unchanged in that case (no settings block is rendered for them).
## -----------------------------------------------------------------------
.docx_add_customization_block <- function(doc, item, th) {
  cz <- item$customization
  if (is.null(cz)) return(doc)
  
  parts <- character(0)
  
  add_hex <- function(label, val) {
    if (!is.null(val) && length(val) == 1 && !is.na(val) && nzchar(as.character(val))) {
      parts <<- c(parts, sprintf("%s: %s", label, toupper(as.character(val))))
    }
  }
  add_hex("SFD", cz$col_sfd)
  add_hex("COM", cz$col_com)
  add_hex("Penalty", cz$col_pen)
  add_hex("Credit", cz$col_cred)
  
  # VulSen (Vulnerability) items don't use the 4 fixed Secmod colour
  # fields above -- they carry a variable-length per-class colour table
  # instead, attached as customization$class_colours: a named character
  # vector where names = legend class labels and values = hex codes.
  # Older cart items saved before this field existed simply won't have
  # it, so this is a no-op for them (same "nothing rendered" fallback as
  # the rest of this function).
  if (!is.null(cz$class_colours) && length(cz$class_colours) > 0) {
    cc <- cz$class_colours
    cc_labels <- names(cc)
    if (is.null(cc_labels) || any(!nzchar(cc_labels))) {
      cc_labels <- paste0("Class ", seq_along(cc))
    }
    for (i in seq_along(cc)) {
      add_hex(cc_labels[i], cc[[i]])
    }
  }
  
  if (!is.null(cz$axis_text)) parts <- c(parts, sprintf("Axis text: %spt", cz$axis_text))
  if (!is.null(cz$plot_title)) parts <- c(parts, sprintf("Title: %spt", cz$plot_title))
  
  legend_val <- cz$legend_show
  if (!is.null(legend_val)) {
    parts <- c(parts, sprintf("Legend: %s", if (isTRUE(legend_val)) "shown" else "hidden"))
  }
  
  bg_val <- cz$bg %||% cz$transparent_bg
  if (!is.null(bg_val)) {
    is_transp <- isTRUE(bg_val) ||
      (is.character(bg_val) && tolower(trimws(bg_val)) %in% c("transparent", "na", "none"))
    parts <- c(parts, sprintf("Background: %s", if (is_transp) "transparent" else "white"))
  }
  
  if (!is.null(cz$width_in) && !is.null(cz$height_in)) {
    parts <- c(parts, sprintf("Size: %.1f x %.1f in",
                              as.numeric(cz$width_in), as.numeric(cz$height_in)))
  }
  
  if (!is.null(cz$dpi)) parts <- c(parts, sprintf("DPI: %s", cz$dpi))
  
  if (length(parts) == 0) return(doc)
  
  settings_line <- paste("Settings \u2014", paste(parts, collapse = " \u00b7 "))
  
  officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(settings_line, officer::fp_text(
        italic = TRUE, font.size = 9, color = th$text_mute, font.family = th$font_body
      )),
      fp_p = officer::fp_par(text.align = "center", padding.top = 2, padding.bottom = 10)
    )
  )
}

## -----------------------------------------------------------------------
## generate_cart_docx()
## -----------------------------------------------------------------------
generate_cart_docx <- function(
    cart_path,
    output_path,
    username     = "User",
    report_title = "VulSen Analytics Report",
    logo_path    = "www/logo-removebg-preview.png",
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
  
  doc <- .docx_add_rule(doc, color = th$dark, width_pt = 1.75, space_after = 18)
  
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
  ## TABLE OF CONTENTS  (native Word field, built from Heading 1 styles)
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
    ## Numeral and module name both in the strategic accent colour.
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(sprintf("%02d. ", i), officer::fp_text(
          font.size = 20, bold = TRUE, color = th$dark, font.family = th$font_head
        )),
        officer::ftext(label, officer::fp_text(
          font.size = 20, bold = TRUE, color = th$dark, font.family = th$font_head
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
      
      item_w   <- item$width  %||% 8.5
      item_h   <- item$height %||% 4.9
      item_dpi <- max(item$dpi %||% 300, 300)
      item_bg <- attr(item$plot, "vulsen_bg") %||% item$bg %||% "white"
      
      ggplot2::ggsave(
        filename = img_path,
        plot     = item$plot,
        width    = item_w, height = item_h, dpi = item_dpi, bg = item_bg
      )
      
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
      
      ## ---- Plot customization settings (colors, fonts, legend, bg, size) ----
      ## Omitted automatically for older cart items that lack item$customization.
      doc <- .docx_add_customization_block(doc, item, th)
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
              border.left  = officer::fp_border(color = th$highlight, width = 1.5)
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
  ## PAGE FOOTER  (app name + live "Page X of Y", every page)
  ## -----------------------------------------------------------------------
  ## Built with officer's default-section footer, not an in-body paragraph,
  ## so it repeats automatically on every page like a real Word footer.
  ## PAGE / NUMPAGES are live Word fields (see .docx_strip_heading_numbering
  ## above, which force-marks all fields "dirty" so these numbers are
  ## correct the moment the document opens, not stuck at a cached value).
  
  footer_block <- tryCatch({
    footer_text_prop <- officer::fp_text(
      font.size = 9, italic = TRUE, color = "#000000", font.family = th$font_body
    )
    officer::block_list(
      officer::fpar(
        officer::ftext(paste0("VulSen Analytics  \u00b7  Page "), footer_text_prop),
        officer::run_word_field(field = "PAGE", prop = footer_text_prop),
        officer::ftext(" of ", footer_text_prop),
        officer::run_word_field(field = "NUMPAGES", prop = footer_text_prop),
        fp_p = officer::fp_par(
          text.align  = "center",
          border.top  = officer::fp_border(color = th$rule_gray, width = 0.75),
          padding.top = 6
        )
      )
    )
  }, error = function(e) {
    ## Older officer without run_word_field(): fall back to a static
    ## (non-live) footer line so the report still ships with an app-name
    ## footer, just without an auto-updating page count.
    officer::block_list(
      officer::fpar(
        officer::ftext("VulSen Analytics", officer::fp_text(
          font.size = 9, italic = TRUE, color = "#000000", font.family = th$font_body
        )),
        fp_p = officer::fp_par(
          text.align  = "center",
          border.top  = officer::fp_border(color = th$rule_gray, width = 0.75),
          padding.top = 6
        )
      )
    )
  })
  
  doc <- tryCatch(
    officer::body_set_default_section(
      doc,
      value = officer::prop_section(footer_default = footer_block)
    ),
    error = function(e) {
      warning(
        "Could not attach a live page footer (officer::body_set_default_section ",
        "failed): ", conditionMessage(e),
        ". The report was still generated without a running page footer."
      )
      doc
    }
  )
  
  ## -----------------------------------------------------------------------
  ## SAVE  (+ post-process to remove double-numbering and set updateFields)
  ## -----------------------------------------------------------------------
  
  print(doc, target = output_path)
  .docx_strip_heading_numbering(output_path)
  
  invisible(output_path)
}

