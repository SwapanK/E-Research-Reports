## =============================================================================
## PPT GENERATOR MODULE  (module/ppt_generator.R)  -- MODERN THEME
## =============================================================================
## Builds a PowerPoint from any cart .rds file that follows the standard
## VulSen cart item structure (see module/cart_utils.R).
##
## Design system (flat, no gradients, no extra packages beyond officer +
## ggplot2 which you already depend on):
##   - Navy title slide, light-gray content slides
##   - Slim navy header band + accent underline on every content slide
##   - Inline "Title | 01/04" item counter -- plain text, no shading/box
##   - Styled contents list (accent numerals, alternating row shading)
##   - Commentary rendered as a shaded "card" instead of a raw bullet
##   - Plots placed as white cards floating on the gray background
##   - Bottom strip on every slide: page number + wordmark, plus a
##     compact LaTeX-Beamer-style dot map on item slides showing overall
##     section progress
##
## Slide order produced:
##   1. Branding / title slide
##   2. One or more index (contents) slides
##   3. For each cart item, in order:
##        - a slide with the plot
##        - a slide with the commentary (only if commentary is present)
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
  primary   = "#6FACDE",   # light blue
  accent    = "#0075BC",   # dark blue (strategic accent)
  highlight = "#0075BC",   # dark blue (strategic accent)
  dark      = "#0075BC",   # dark blue
  navy      = "#0F1B3D",   # kept for background
  navy_dark = "#0A1329",
  bg_light  = "#6FACDE",   # primary light blue -- now the dominant page colour
  card_white= "#FFFFFF",
  text_dark = "#1A1A2E",
  text_mute = "#7A8296",
  row_shade = "#EAEDF4",
  font      = "Calibri"
)

## -----------------------------------------------------------------------
## WIDESCREEN TEMPLATE
## -----------------------------------------------------------------------
.default_widescreen_template <- function() {
  file.path("www", "widescreen_template.pptx")
}

## -----------------------------------------------------------------------
## THEME  -- using app_palette
## -----------------------------------------------------------------------
.vulsen_theme <- list(
  navy       = app_palette$navy,
  navy_dark  = app_palette$navy_dark,
  accent     = app_palette$accent,
  accent2    = app_palette$highlight,   # coral/orange used sparingly
  primary    = app_palette$primary,     # light blue -- header badges/counters
  dark       = app_palette$dark,        # dark blue -- index numerals, small print
  bg_light   = app_palette$bg_light,
  card_white = app_palette$card_white,
  text_dark  = app_palette$text_dark,
  text_mute  = app_palette$text_mute,
  row_shade  = app_palette$row_shade,
  font       = app_palette$font
)

## -----------------------------------------------------------------------
## Internal: build a flat-color PNG we can use as a full-bleed background
## or a header band. Pure base-R graphics, no extra deps.
## -----------------------------------------------------------------------
.make_flat_png <- function(path, width_in, height_in, fill, dpi = 150,
                           accent_strip = NULL, accent_strip_frac = 0.06) {
  grDevices::png(path, width = width_in, height = height_in,
                 units = "in", res = dpi, bg = "white")
  op <- graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
  graphics::rect(0, 0, 1, 1, col = fill, border = NA)
  if (!is.null(accent_strip)) {
    graphics::rect(0, 0, 1, accent_strip_frac, col = accent_strip, border = NA)
  }
  graphics::par(op)
  grDevices::dev.off()
  path
}

## -----------------------------------------------------------------------
## Internal: fit an image into a max_w x max_h box in inches, preserving
## its real aspect ratio.
## -----------------------------------------------------------------------
.fit_dims_in <- function(path, max_w, max_h, fallback_ratio = 2.2) {
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
## Internal: crop away transparent (or near-white) padding around a logo
## PNG so downstream sizing/placement is based on the actual visible mark,
## not the full canvas. Many "background removed" exports keep a large
## transparent margin, which makes .fit_dims_in() scale the *whole* box
## down to fit a target area -- shrinking the visible logo to a speck.
## Falls back to the original path on any failure (missing 'png' package,
## no alpha channel and nothing recognizably "background", etc.).
## -----------------------------------------------------------------------
.trim_logo_png <- function(path, tmp_dir, pad_frac = 0.05) {
  if (!requireNamespace("png", quietly = TRUE)) return(path)
  
  img <- tryCatch(png::readPNG(path), error = function(e) NULL)
  if (is.null(img) || length(dim(img)) < 3) return(path)
  
  h <- dim(img)[1]; w <- dim(img)[2]; ch <- dim(img)[3]
  
  mask <- if (ch >= 4) {
    img[, , 4] > 0.02                                    # has real alpha
  } else if (ch >= 3) {
    !(img[, , 1] > 0.96 & img[, , 2] > 0.96 & img[, , 3] > 0.96)  # near-white bg
  } else {
    !(img[, , 1] > 0.96)
  }
  
  if (!any(mask)) return(path)
  
  rows <- which(apply(mask, 1, any))
  cols <- which(apply(mask, 2, any))
  
  pad_r <- ceiling(pad_frac * h); pad_c <- ceiling(pad_frac * w)
  r1 <- max(1, min(rows) - pad_r); r2 <- min(h, max(rows) + pad_r)
  c1 <- max(1, min(cols) - pad_c); c2 <- min(w, max(cols) + pad_c)
  
  ## No meaningful padding to trim -- skip the round-trip.
  if (r1 <= 1 && r2 >= h && c1 <= 1 && c2 >= w) return(path)
  
  cropped  <- img[r1:r2, c1:c2, , drop = FALSE]
  out_path <- tempfile(pattern = "vulsen_logo_trim_", tmpdir = tmp_dir, fileext = ".png")
  
  ok <- tryCatch({ png::writePNG(cropped, out_path); TRUE }, error = function(e) FALSE)
  if (!ok) return(path)
  
  out_path
}

## -----------------------------------------------------------------------
## Internal: a plain white circle on a transparent background, used as a
## soft badge behind the logo on the title slide.
## -----------------------------------------------------------------------
.make_circle_png <- function(path, size_in, fill = "#FFFFFF", dpi = 150) {
  grDevices::png(path, width = size_in, height = size_in,
                 units = "in", res = dpi, bg = "transparent")
  op <- graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
  graphics::symbols(0.5, 0.5, circles = 0.49, inches = FALSE,
                    add = TRUE, bg = fill, fg = NA)
  graphics::par(op)
  grDevices::dev.off()
  path
}

## -----------------------------------------------------------------------
## Internal: add the header band + title + item counter to a content
## slide, and return the doc (call this right after add_slide()).
## -----------------------------------------------------------------------
.add_header <- function(doc, tmp_dir, title_text, badge_text = NULL,
                        slide_w = 13.333, band_h = 0.95, th = .vulsen_theme) {
  
  band_path <- tempfile(pattern = "hdr_", tmpdir = tmp_dir, fileext = ".png")
  .make_flat_png(band_path, width_in = slide_w, height_in = band_h,
                 fill = th$primary, accent_strip = th$dark, accent_strip_frac = 0.05)
  
  doc <- officer::ph_with(
    doc,
    value    = officer::external_img(band_path, width = slide_w, height = band_h),
    location = officer::ph_location(left = 0, top = 0, width = slide_w, height = band_h)
  )
  
  title_run <- officer::ftext(title_text, officer::fp_text(
    font.size = 21, bold = TRUE, color = th$navy, font.family = th$font
  ))
  
  title_pars <- list(title_run)
  
  if (!is.null(badge_text) && nzchar(badge_text)) {
    sep_run <- officer::ftext("   \u2758   ", officer::fp_text(
      font.size = 16, bold = FALSE, color = th$dark, font.family = th$font
    ))
    count_run <- officer::ftext(badge_text, officer::fp_text(
      font.size = 14, bold = FALSE, italic = TRUE, color = th$dark, font.family = th$font
    ))
    title_pars <- c(title_pars, list(sep_run, count_run))
  }
  
  doc <- officer::ph_with(
    doc,
    value    = officer::block_list(do.call(officer::fpar, c(
      title_pars,
      list(fp_p = officer::fp_par(text.align = "left", padding = 0))
    ))),
    location = officer::ph_location(left = 0.55, top = 0.22, width = slide_w - 0.9, height = 0.55)
  )
  
  doc
}

## -----------------------------------------------------------------------
## Internal: a row of small dots on a transparent background -- a compact
## "you are here" overview of all sections, LaTeX-Beamer-navigation style.
## -----------------------------------------------------------------------
.make_section_map_png <- function(path, width_in, height_in, n, current,
                                  th = .vulsen_theme, dpi = 150, max_dots = 20) {
  grDevices::png(path, width = width_in, height = height_in,
                 units = "in", res = dpi, bg = "transparent")
  op <- graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
  
  if (n > 0 && n <= max_dots) {
    xs <- seq(0, 1, length.out = n + 2)[2:(n + 1)]
    for (k in seq_len(n)) {
      if (k == current) {
        graphics::points(xs[k], 0.5, pch = 16, cex = 0.55, col = th$dark)
      } else {
        graphics::points(xs[k], 0.5, pch = 1, cex = 0.42,
                         col = "#4A5578", lwd = 1.1)
      }
    }
  } else if (n > max_dots) {
    track_y <- 0.56
    graphics::rect(0, track_y - 0.045, 1, track_y + 0.045, col = "#4A5578", border = NA)
    frac  <- if (n > 1) (current - 1) / (n - 1) else 0
    seg_w <- max(0.035, 1 / n)
    seg_x <- min(frac, 1 - seg_w)
    graphics::rect(seg_x, track_y - 0.045, seg_x + seg_w, track_y + 0.045,
                   col = th$dark, border = NA)
    graphics::text(0.5, 0.14, labels = sprintf("%d / %d", current, n),
                   col = th$navy, cex = 0.62, font = 3)
  }
  
  graphics::par(op)
  grDevices::dev.off()
  path
}

## -----------------------------------------------------------------------
## Internal: bottom strip added to every slide -- page number + wordmark
## and (on item slides) a small LaTeX-Beamer-style dot map.
## -----------------------------------------------------------------------
.add_footer <- function(doc, tmp_dir, page_no, slide_w = 13.333, slide_h = 7.5,
                        th = .vulsen_theme, section_current = NULL, section_total = NULL) {
  
  band_h <- 0.30
  band_path <- tempfile(pattern = "ftr_", tmpdir = tmp_dir, fileext = ".png")
  .make_flat_png(band_path, width_in = slide_w, height_in = band_h, fill = th$primary)
  doc <- officer::ph_with(
    doc,
    value    = officer::external_img(band_path, width = slide_w, height = band_h),
    location = officer::ph_location(left = 0, top = slide_h - band_h, width = slide_w, height = band_h)
  )
  
  footer_run <- officer::ftext(
    paste0("VulSen Analytics   \u00b7   ", page_no),
    officer::fp_text(font.size = 9, color = th$navy, italic = TRUE, font.family = th$font)
  )
  doc <- officer::ph_with(
    doc,
    value    = officer::block_list(officer::fpar(footer_run,
                                                 fp_p = officer::fp_par(text.align = "left", padding = 0))),
    location = officer::ph_location(left = 0.4, top = slide_h - band_h, width = 3.6, height = band_h)
  )
  
  if (!is.null(section_current) && !is.null(section_total) && section_total > 1) {
    dots_path <- tempfile(pattern = "secmap_", tmpdir = tmp_dir, fileext = ".png")
    .make_section_map_png(dots_path, width_in = 3.4, height_in = band_h,
                          n = section_total, current = section_current, th = th)
    doc <- officer::ph_with(
      doc,
      value    = officer::external_img(dots_path, width = 3.4, height = band_h),
      location = officer::ph_location(left = slide_w - 3.8, top = slide_h - band_h,
                                      width = 3.4, height = band_h)
    )
  }
  
  doc
}

## -----------------------------------------------------------------------
## generate_cart_ppt()
## -----------------------------------------------------------------------
generate_cart_ppt <- function(
    cart_path,
    output_path,
    username      = "User",
    report_title  = "VulSen Analytics Report",
    company_name  = "Gallagher Re",
    logo_path     = "www/logo-removebg-preview.png",
    theme         = .vulsen_theme,
    widescreen    = TRUE,
    template_path = .default_widescreen_template()
) {
  
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("The 'officer' package is required to generate PPT files. Install it with install.packages('officer').")
  }
  
  if (!file.exists(cart_path)) {
    stop("Cart file not found: ", cart_path)
  }
  
  cart_items <- readRDS(cart_path)
  
  if (length(cart_items) == 0) {
    stop("Cart is empty \u2014 nothing to export.")
  }
  
  th <- theme
  tmp_dir <- tempdir()
  
  ## ---- Slide canvas: widescreen (16:9) by default, classic (4:3) opt-out ----
  if (isTRUE(widescreen)) {
    slide_w <- 13.333; slide_h <- 7.5
    
    if (!is.null(template_path) && file.exists(template_path)) {
      doc <- officer::read_pptx(path = template_path)
    } else {
      warning(
        "Widescreen template not found at '", template_path, "' \u2014 falling back to ",
        "officer's stock 4:3 template. Ship the 16:9 template (see ",
        ".default_widescreen_template()) to get true widescreen output.",
        call. = FALSE
      )
      doc <- officer::read_pptx()
      slide_w <- 10; slide_h <- 7.5
    }
  } else {
    slide_w <- 10; slide_h <- 7.5
    doc <- officer::read_pptx()
  }
  
  plot_margin <- 0.65
  plot_w <- slide_w - (2 * plot_margin)
  plot_h <- 5.75
  
  page_no <- 0
  
  ## -----------------------------------------------------------------------
  ## SLIDE 1 \u2014 BRANDING / TITLE  (full primary light-blue background)
  ## -----------------------------------------------------------------------
  
  doc <- officer::add_slide(doc, layout = "Title Slide", master = "Office Theme")
  page_no <- page_no + 1
  
  bg_path <- file.path(tmp_dir, "vulsen_title_bg.png")
  .make_flat_png(bg_path, width_in = slide_w, height_in = slide_h,
                 fill = th$primary, accent_strip = th$accent, accent_strip_frac = 0.02)
  
  doc <- officer::ph_with(
    doc,
    value    = officer::external_img(bg_path, width = slide_w, height = slide_h),
    location = officer::ph_location(left = 0, top = 0, width = slide_w, height = slide_h)
  )
  
  ## ---- Logo, standing alone as the brand mark (no company-name text) ----
  ## Trimmed first so a padded "background removed" export doesn't get
  ## scaled down to a speck when fit into the target box.
  if (!is.null(logo_path) && nzchar(logo_path) && file.exists(logo_path)) {
    
    trimmed_logo <- .trim_logo_png(logo_path, tmp_dir)
    
    logo_box_w <- 3.8; logo_box_h <- 1.6
    logo_dims <- .fit_dims_in(trimmed_logo, max_w = logo_box_w, max_h = logo_box_h)
    logo_w <- unname(logo_dims["width"]); logo_h <- unname(logo_dims["height"])
    
    doc <- officer::ph_with(
      doc,
      value    = officer::external_img(trimmed_logo, width = logo_w, height = logo_h),
      location = officer::ph_location(
        left = (slide_w - logo_w) / 2,
        top  = 2.1,
        width = logo_w, height = logo_h
      )
    )
  }
  
  title_run <- officer::ftext(report_title, officer::fp_text(
    font.size = 36, bold = TRUE, color = th$navy, font.family = th$font
  ))
  doc <- officer::ph_with(
    doc,
    value    = officer::block_list(officer::fpar(title_run,
                                                 fp_p = officer::fp_par(text.align = "center", padding = 0))),
    location = officer::ph_location(left = 0.5, top = 3.05, width = slide_w - 1.0, height = 0.9)
  )
  
  divider_w <- 1.1
  divider_path <- file.path(tmp_dir, "vulsen_title_divider.png")
  .make_flat_png(divider_path, width_in = divider_w, height_in = 0.03, fill = th$dark)
  doc <- officer::ph_with(
    doc,
    value    = officer::external_img(divider_path, width = divider_w, height = 0.03),
    location = officer::ph_location(left = (slide_w - divider_w) / 2, top = 4.05,
                                    width = divider_w, height = 0.03)
  )
  
  subtitle_run <- officer::ftext(
    paste0(
      format(Sys.Date(), "%B %d, %Y"), "   \u2022   ",
      length(cart_items), " item(s) included"
    ),
    officer::fp_text(font.size = 14, color = th$navy, font.family = th$font)
  )
  doc <- officer::ph_with(
    doc,
    value    = officer::block_list(officer::fpar(subtitle_run,
                                                 fp_p = officer::fp_par(text.align = "center", padding = 0))),
    location = officer::ph_location(left = 0.5, top = 4.3, width = slide_w - 1.0, height = 0.5)
  )
  
  ## -----------------------------------------------------------------------
  ## SLIDE(S) \u2014 INDEX  (chunked so long carts spill onto extra slides)
  ## -----------------------------------------------------------------------
  
  chunk_size <- 12
  idx_chunks <- split(seq_along(cart_items), ceiling(seq_along(cart_items) / chunk_size))
  
  for (chunk in idx_chunks) {
    
    doc <- officer::add_slide(doc, layout = "Title Only", master = "Office Theme")
    page_no <- page_no + 1
    doc <- .add_header(doc, tmp_dir, "Contents", th = th, slide_w = slide_w)
    
    row_pars <- lapply(seq_along(chunk), function(k) {
      i    <- chunk[k]
      item <- cart_items[[i]]
      ts   <- tryCatch(format(item$timestamp, "%Y-%m-%d %H:%M"), error = function(e) "")
      
      num_run  <- officer::ftext(sprintf("%02d   ", i),
                                 officer::fp_text(bold = TRUE, color = th$dark, font.size = 13, font.family = th$font))
      name_run <- officer::ftext(paste0(item$module %||% "Item", "    "),
                                 officer::fp_text(bold = TRUE, color = th$navy, font.size = 13, font.family = th$font))
      ts_run   <- officer::ftext(ts,
                                 officer::fp_text(italic = TRUE, color = th$text_mute, font.size = 11, font.family = th$font))
      
      shade <- if (k %% 2 == 0) th$row_shade else th$card_white
      
      officer::fpar(num_run, name_run, ts_run,
                    fp_p = officer::fp_par(text.align = "left", padding = 8, shading.color = shade))
    })
    
    doc <- officer::ph_with(
      doc,
      value    = do.call(officer::block_list, row_pars),
      location = officer::ph_location(left = 0.6, top = 1.3, width = slide_w - 1.2, height = 5.55)
    )
    
    doc <- .add_footer(doc, tmp_dir, page_no, slide_w, slide_h, th)
  }
  
  ## -----------------------------------------------------------------------
  ## ONE PLOT SLIDE + ONE COMMENTARY SLIDE PER CART ITEM
  ## -----------------------------------------------------------------------
  
  for (i in seq_along(cart_items)) {
    
    item <- cart_items[[i]]
    label_prefix <- item$module %||% "Item"
    badge <- sprintf("%02d/%02d", i, length(cart_items))
    
    ## ---- Plot slide ----
    if (!is.null(item$plot)) {
      
      doc <- officer::add_slide(doc, layout = "Title Only", master = "Office Theme")
      page_no <- page_no + 1
      
      slide_bg <- file.path(tmp_dir, paste0("bg_content_", page_no, ".png"))
      .make_flat_png(slide_bg, width_in = slide_w, height_in = slide_h, fill = th$card_white)
      doc <- officer::ph_with(
        doc,
        value    = officer::external_img(slide_bg, width = slide_w, height = slide_h),
        location = officer::ph_location(left = 0, top = 0, width = slide_w, height = slide_h)
      )
      
      doc <- .add_header(doc, tmp_dir, paste0(label_prefix, " \u2014 Visualization"),
                         badge_text = badge, th = th, slide_w = slide_w)
      
      img_path <- tempfile(pattern = paste0("vulsen_plot_", i, "_"), tmpdir = tmp_dir, fileext = ".png")
      
      item_w   <- item$width  %||% plot_w
      item_h   <- item$height %||% plot_h
      item_dpi <- max(item$dpi %||% 300, 300)
      item_bg <- attr(item$plot, "vulsen_bg") %||% item$bg %||% "white"
      
      ggplot2::ggsave(
        filename = img_path,
        plot     = item$plot,
        width    = item_w, height = item_h, dpi = item_dpi, bg = item_bg
      )
      
      fit   <- .fit_dims_in(img_path, max_w = plot_w, max_h = plot_h)
      fit_w <- unname(fit["width"])
      fit_h <- unname(fit["height"])
      fit_left <- plot_margin + (plot_w - fit_w) / 2
      fit_top  <- 1.3 + (plot_h - fit_h) / 2
      
      doc <- officer::ph_with(
        doc,
        value    = officer::external_img(img_path, width = fit_w, height = fit_h),
        location = officer::ph_location(left = fit_left, top = fit_top, width = fit_w, height = fit_h)
      )
      
      doc <- .add_footer(doc, tmp_dir, page_no, slide_w, slide_h, th,
                         section_current = i, section_total = length(cart_items))
    }
    
    ## ---- Commentary slide ----
    if (!is.null(item$commentary) && nzchar(item$commentary)) {
      
      doc <- officer::add_slide(doc, layout = "Title Only", master = "Office Theme")
      page_no <- page_no + 1
      
      slide_bg <- file.path(tmp_dir, paste0("bg_content_", page_no, ".png"))
      .make_flat_png(slide_bg, width_in = slide_w, height_in = slide_h, fill = th$card_white)
      doc <- officer::ph_with(
        doc,
        value    = officer::external_img(slide_bg, width = slide_w, height = slide_h),
        location = officer::ph_location(left = 0, top = 0, width = slide_w, height = slide_h)
      )
      
      doc <- .add_header(doc, tmp_dir, paste0(label_prefix, " \u2014 Commentary"),
                         badge_text = badge, th = th, slide_w = slide_w)
      
      lines <- strsplit(item$commentary, "\n")[[1]]
      lines <- lines[nzchar(trimws(lines))]
      
      body_pars <- lapply(lines, function(ln) {
        bullet_run <- officer::ftext("\u25B8  ", officer::fp_text(
          bold = TRUE, color = th$accent, font.size = 14, font.family = th$font))
        text_run <- officer::ftext(trimws(ln), officer::fp_text(
          color = th$text_dark, font.size = 14, font.family = th$font))
        officer::fpar(bullet_run, text_run,
                      fp_p = officer::fp_par(text.align = "left", padding = 10, shading.color = th$card_white))
      })
      
      doc <- officer::ph_with(
        doc,
        value    = do.call(officer::block_list, body_pars),
        location = officer::ph_location(left = 0.75, top = 1.3, width = slide_w - 1.5, height = 5.75)
      )
      
      doc <- .add_footer(doc, tmp_dir, page_no, slide_w, slide_h, th,
                         section_current = i, section_total = length(cart_items))
    }
  }
  
  ## -----------------------------------------------------------------------
  ## SAVE
  ## -----------------------------------------------------------------------
  
  print(doc, target = output_path)
  
  invisible(output_path)
}


