
## =============================================================================
## PPT GENERATOR MODULE  (module/ppt_generator.R)  -- MODERN THEME
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
## FIX (this revision): the section-progress "dots" in the footer were
## sized off a formula (`min(0.05, 0.85 / (n * 4))`) that only really
## shrinks once a cart gets fairly large, so with a handful of items the
## dots rendered at their 0.05in ceiling and read as bold, chunky "strip
## balls" that fought visually with the wordmark next to them. They're now
## capped smaller by default, shrink earlier as the cart grows, and once a
## cart is large enough that individual dots would start to blur together
## the map switches to a compact "current / total" pill instead of packing
## in dozens of tiny dots -- so decks with many more sections stay legible
## instead of the footer becoming a smear of dots.
##
## Requires the 'officer' package:  install.packages("officer")
## =============================================================================

library(ggplot2)

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || (is.character(a) && !nzchar(a))) b else a
}

## -----------------------------------------------------------------------
## WIDESCREEN TEMPLATE
## -----------------------------------------------------------------------
## officer::read_pptx() with no `path` argument loads officer's bundled
## blank template, which is fixed at 4:3 (10in x 7.5in) -- there's no
## officer argument that widens it after the fact. To ship 16:9 decks we
## instead point read_pptx() at our own blank widescreen template (same
## "Office Theme" master, same "Title Slide" / "Title Only" layouts this
## module already uses, just declared at 13.333in x 7.5in / 16:9). Drop
## that file at module/../www/widescreen_template.pptx (see
## .default_widescreen_template()) -- if it's missing, generate_cart_ppt()
## falls back to officer's stock 4:3 template with a warning instead of
## failing outright.
## -----------------------------------------------------------------------
.default_widescreen_template <- function() {
  file.path("www", "widescreen_template.pptx")
}

## -----------------------------------------------------------------------
## THEME  -- tweak these to match VulSen's brand
## -----------------------------------------------------------------------
.vulsen_theme <- list(
  navy       = "#0F1B3D",
  navy_dark  = "#0A1329",
  accent     = "#00C9A7",   # teal - primary accent
  accent2    = "#FF5A5F",   # coral - used sparingly (e.g. warnings)
  bg_light   = "#F4F6FA",
  card_white = "#FFFFFF",
  text_dark  = "#1A1A2E",
  text_mute  = "#7A8296",
  row_shade  = "#EAEDF4",
  font       = "Calibri"    # swap for your house font if installed on render machine
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
## its real aspect ratio (so a landscape logo isn't squished into a
## square). Reads real pixel dimensions via 'png'/'jpeg' if available;
## falls back to a landscape assumption otherwise so it never errors.
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
## Internal: add the navy header band + title + item counter to a content
## slide, and return the doc (call this right after add_slide()).
##
## The band is intentionally slim (0.95in vs. the old 1.25in) so content
## slides get noticeably more vertical room for the plot/commentary.
##
## The item counter is rendered inline with the title as plain, un-boxed
## text -- e.g. "Heartbleed \u2014 Visualization  |  01/04" -- instead of the
## old shaded "ITEM 01 OF 04" pill badge. No shading.color is used anywhere
## here, so there is no text-highlighter background behind any of it.
## -----------------------------------------------------------------------
.add_header <- function(doc, tmp_dir, title_text, badge_text = NULL,
                         slide_w = 13.333, band_h = 0.95, th = .vulsen_theme) {

  band_path <- tempfile(pattern = "hdr_", tmpdir = tmp_dir, fileext = ".png")
  .make_flat_png(band_path, width_in = slide_w, height_in = band_h,
                  fill = th$navy, accent_strip = th$accent, accent_strip_frac = 0.10)

  doc <- officer::ph_with(
    doc,
    value    = officer::external_img(band_path, width = slide_w, height = band_h),
    location = officer::ph_location(left = 0, top = 0, width = slide_w, height = band_h)
  )

  title_run <- officer::ftext(title_text, officer::fp_text(
    font.size = 21, bold = TRUE, color = "#FFFFFF", font.family = th$font
  ))

  title_pars <- list(title_run)

  ## "Title | 01/04" -- counter sits right after the title on the same
  ## line, separated by a slim vertical bar, colored with the accent and
  ## with no shading/box behind it.
  if (!is.null(badge_text) && nzchar(badge_text)) {
    sep_run <- officer::ftext("   \u2758   ", officer::fp_text(
      font.size = 16, bold = FALSE, color = th$accent, font.family = th$font
    ))
    count_run <- officer::ftext(badge_text, officer::fp_text(
      font.size = 14, bold = FALSE, italic = TRUE, color = th$accent, font.family = th$font
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
## The dot for `current` is drawn in the accent color; all others are a
## muted tone. Skips drawing entirely if there's only one/zero sections.
##
## Sizing (revised): dots are drawn with graphics::points(pch = 16/1),
## sized in physical device points via `cex` rather than in plot-window
## user units via symbols(circles = ...). The previous symbols()-based
## approach sized the "circle" using the same value on both the x and y
## axes of a 0-1 plot window stretched across a ~11:1 wide-but-thin PNG
## (3.4in x 0.30in) -- so each dot actually rendered as an oversized,
## squashed ellipse, not a small round dot. points() is aspect-ratio
## independent, so dots are now genuinely small and round: a solid
## accent-filled dot marks the current section, a smaller hollow
## muted-outline dot marks the rest -- the classic LaTeX-Beamer
## navigation-dot convention.
##
## Scaling for large carts: past `max_dots` individual sections, drawing
## one dot per item stops being legible (they'd overlap or blur into a
## line), so the map switches to a small track + filled progress bar with
## a "current / total" caption instead -- this keeps decks with many more
## sections readable rather than cramming ever-smaller dots into the same
## space.
## -----------------------------------------------------------------------
.make_section_map_png <- function(path, width_in, height_in, n, current,
                                   th = .vulsen_theme, dpi = 150, max_dots = 20) {
  grDevices::png(path, width = width_in, height = height_in,
                  units = "in", res = dpi, bg = "transparent")
  op <- graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))

  if (n > 0 && n <= max_dots) {

    ## Dot mode: small, evenly spaced dots, one per section.
    ##
    ## FIX (aesthetics): this used to draw each dot with
    ## graphics::symbols(circles = r, inches = FALSE). With inches = FALSE,
    ## symbols() sizes a "circle" using the *user coordinate* units on both
    ## axes -- but plot.window() here spans 0-1 on x AND y while the PNG
    ## canvas itself is only 3.4in wide by 0.30in tall (an ~11:1 aspect
    ## ratio). One x-unit is therefore ~11x wider on the page than one
    ## y-unit, so every "circle" actually rendered as a squashed, oversized
    ## horizontal blob instead of a small round dot -- not the crisp
    ## LaTeX-Beamer navigation-dot look this was meant to have.
    ##
    ## graphics::points(pch = ...) sizes its glyph in physical device
    ## points (like text), completely independent of the x/y coordinate
    ## aspect ratio, so it always renders as a true, small circle. Current
    ## section gets a solid filled dot; the rest get a smaller, hollow,
    ## muted-outline dot -- the same "you are here" convention Beamer's
    ## own dot/circle navigation symbols use.
    xs <- seq(0, 1, length.out = n + 2)[2:(n + 1)]
    for (k in seq_len(n)) {
      if (k == current) {
        graphics::points(xs[k], 0.5, pch = 16, cex = 0.55, col = th$accent)
      } else {
        graphics::points(xs[k], 0.5, pch = 1, cex = 0.42,
                          col = "#4A5578", lwd = 1.1)
      }
    }

  } else if (n > max_dots) {

    ## Track mode for large carts: a thin muted track with a short accent
    ## segment marking progress, plus a "current/total" label -- scales to
    ## any number of sections without the map getting more cluttered.
    track_y <- 0.56
    graphics::rect(0, track_y - 0.045, 1, track_y + 0.045, col = "#4A5578", border = NA)

    frac  <- if (n > 1) (current - 1) / (n - 1) else 0
    seg_w <- max(0.035, 1 / n)
    seg_x <- min(frac, 1 - seg_w)
    graphics::rect(seg_x, track_y - 0.045, seg_x + seg_w, track_y + 0.045,
                   col = th$accent, border = NA)

    graphics::text(0.5, 0.14, labels = sprintf("%d / %d", current, n),
                   col = "#C7CCDA", cex = 0.62, font = 3)
  }

  graphics::par(op)
  grDevices::dev.off()
  path
}

## -----------------------------------------------------------------------
## Internal: bottom strip added to every slide -- page number + wordmark
## on the left, and (on item slides) a small LaTeX-Beamer-style dot map
## on the right giving an at-a-glance overview of where this slide sits
## among all cart-item sections.
## -----------------------------------------------------------------------
.add_footer <- function(doc, tmp_dir, page_no, slide_w = 13.333, slide_h = 7.5,
                         th = .vulsen_theme, section_current = NULL, section_total = NULL) {

  band_h <- 0.30
  band_path <- tempfile(pattern = "ftr_", tmpdir = tmp_dir, fileext = ".png")
  .make_flat_png(band_path, width_in = slide_w, height_in = band_h, fill = th$navy_dark)
  doc <- officer::ph_with(
    doc,
    value    = officer::external_img(band_path, width = slide_w, height = band_h),
    location = officer::ph_location(left = 0, top = slide_h - band_h, width = slide_w, height = band_h)
  )

  footer_run <- officer::ftext(
    paste0("VulSen Analytics   \u00b7   ", page_no),
    officer::fp_text(font.size = 9, color = "#C7CCDA", italic = TRUE, font.family = th$font)
  )
  doc <- officer::ph_with(
    doc,
    value    = officer::block_list(officer::fpar(footer_run,
                 fp_p = officer::fp_par(text.align = "left", padding = 0))),
    location = officer::ph_location(left = 0.4, top = slide_h - band_h, width = 3.6, height = band_h)
  )

  ## ---- Section overview map (only when we know where we are) ----
  ## Given a smaller, quieter default size, the map now also gets a touch
  ## more horizontal room (3.4in vs. the old 3.2in) so a mid-sized cart's
  ## dots stay comfortably spaced rather than crowding the wordmark.
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
##
## cart_path    : path to a cart .rds file (e.g. cart/<username>_cart.rds)
## output_path  : where to write the .pptx (e.g. a downloadHandler's `file`)
## username     : shown on the branding slide
## report_title : title shown on the branding slide
## company_name : plain-text placeholder shown above the title (no logo needed)
## logo_path    : unused on the title slide now; kept only for signature
##                compatibility with older calling code
## theme        : list of colors/fonts, defaults to .vulsen_theme (see above)
## -----------------------------------------------------------------------
generate_cart_ppt <- function(
    cart_path,
    output_path,
    username      = "User",
    report_title  = "VulSen Analytics Report",
    company_name  = "Company Name",
    logo_path     = NULL,
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
    slide_w <- 10; slide_h <- 7.5   # classic officer template dims (4:3)
    doc <- officer::read_pptx()
  }

  ## Horizontal margin used to frame the plot image on plot slides -- kept
  ## constant in inches so the plot simply gets wider (not just re-centered)
  ## on the widescreen canvas instead of leaving unused side gutters.
  plot_margin <- 0.65
  plot_w <- slide_w - (2 * plot_margin)
  plot_h <- 5.75

  page_no <- 0

  ## -----------------------------------------------------------------------
  ## SLIDE 1 \u2014 BRANDING / TITLE  (full navy background)
  ## -----------------------------------------------------------------------

  doc <- officer::add_slide(doc, layout = "Title Slide", master = "Office Theme")
  page_no <- page_no + 1

  bg_path <- file.path(tmp_dir, "vulsen_title_bg.png")
  .make_flat_png(bg_path, width_in = slide_w, height_in = slide_h,
                 fill = th$navy, accent_strip = th$accent, accent_strip_frac = 0.02)

  doc <- officer::ph_with(
    doc,
    value    = officer::external_img(bg_path, width = slide_w, height = slide_h),
    location = officer::ph_location(left = 0, top = 0, width = slide_w, height = slide_h)
  )

  ## ---- Company name, plain text placeholder (no logo needed) ----
  ## Bumped from 15pt -> 20pt so it reads as a proper eyebrow/kicker above
  ## the title instead of getting lost; box given a touch more height/top
  ## clearance to comfortably fit the larger text.
  company_run <- officer::ftext(toupper(company_name), officer::fp_text(
    font.size = 20, bold = TRUE, color = th$accent, font.family = th$font
  ))
  doc <- officer::ph_with(
    doc,
    value    = officer::block_list(officer::fpar(company_run,
                 fp_p = officer::fp_par(text.align = "center", padding = 0))),
    location = officer::ph_location(left = 0.5, top = 2.0, width = slide_w - 1.0, height = 0.55)
  )

  ## ---- Title, centered ----
  title_run <- officer::ftext(report_title, officer::fp_text(
    font.size = 36, bold = TRUE, color = "#FFFFFF", font.family = th$font
  ))
  doc <- officer::ph_with(
    doc,
    value    = officer::block_list(officer::fpar(title_run,
                 fp_p = officer::fp_par(text.align = "center", padding = 0))),
    location = officer::ph_location(left = 0.5, top = 3.05, width = slide_w - 1.0, height = 0.9)
  )

  ## ---- Thin centered accent divider ----
  divider_w <- 1.1
  divider_path <- file.path(tmp_dir, "vulsen_title_divider.png")
  .make_flat_png(divider_path, width_in = divider_w, height_in = 0.03, fill = th$accent)
  doc <- officer::ph_with(
    doc,
    value    = officer::external_img(divider_path, width = divider_w, height = 0.03),
    location = officer::ph_location(left = (slide_w - divider_w) / 2, top = 4.05,
                                     width = divider_w, height = 0.03)
  )

  ## ---- Subtitle, single centered line ----
  subtitle_run <- officer::ftext(
    paste0(
      "Prepared for ", username, "   \u2022   ",
      format(Sys.Date(), "%B %d, %Y"), "   \u2022   ",
      length(cart_items), " item(s) included"
    ),
    officer::fp_text(font.size = 14, color = th$accent, font.family = th$font)
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
                    officer::fp_text(bold = TRUE, color = th$accent, font.size = 13, font.family = th$font))
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

    ## ---- Plot slide (plot rendered as a white "card" on gray bg) ----
    if (!is.null(item$plot)) {

      doc <- officer::add_slide(doc, layout = "Title Only", master = "Office Theme")
      page_no <- page_no + 1

      slide_bg <- file.path(tmp_dir, paste0("bg_content_", page_no, ".png"))
      .make_flat_png(slide_bg, width_in = slide_w, height_in = slide_h, fill = th$bg_light)
      doc <- officer::ph_with(
        doc,
        value    = officer::external_img(slide_bg, width = slide_w, height = slide_h),
        location = officer::ph_location(left = 0, top = 0, width = slide_w, height = slide_h)
      )

      doc <- .add_header(doc, tmp_dir, paste0(label_prefix, " \u2014 Visualization"),
                          badge_text = badge, th = th, slide_w = slide_w)

      img_path <- tempfile(pattern = paste0("vulsen_plot_", i, "_"), tmpdir = tmp_dir, fileext = ".png")

      ## Render at the exact width/height/dpi captured when this item was
      ## added to the cart (see the "Capture dimensions for cart rendering"
      ## block in server/secmod_server.R and its counterpart in
      ## server/vulnerability_server.R), instead of the slide's fixed
      ## plot_w x plot_h box. ggplot sizes text, legends, and margins
      ## relative to the physical save dimensions, so re-rendering at a
      ## different box here would silently distort every theme
      ## customization the user dialed in on the preview.
      item_w   <- item$width  %||% plot_w
      item_h   <- item$height %||% plot_h
      ## `max(...)` enforces a 300dpi floor even if a lower dpi was
      ## captured in the cart (the Secmod page's own dpi input defaults to
      ## 150), while still honoring a higher dpi if the user set one.
      item_dpi <- max(item$dpi %||% 300, 300)

      ## Honor the transparency choice captured when this plot was themed
      ## (see attr(p, "vulsen_bg") set in apply_plot_overrides()), so a
      ## plot exported with a transparent background on-screen stays
      ## transparent in the exported deck instead of silently reverting
      ## to white.
      item_bg <- attr(item$plot, "vulsen_bg") %||% item$bg %||% "white"

      ggplot2::ggsave(
        filename = img_path,
        plot     = item$plot,
        width    = item_w, height = item_h, dpi = item_dpi, bg = item_bg
      )

      ## Fit that true-aspect-ratio PNG into the slide's plot box,
      ## preserving aspect ratio (reusing .fit_dims_in, already used above
      ## for the logo) instead of stretching it to exactly fill
      ## plot_w x plot_h, then center it in the box.
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

    ## ---- Commentary slide (styled as a shaded "card") ----
    if (!is.null(item$commentary) && nzchar(item$commentary)) {

      doc <- officer::add_slide(doc, layout = "Title Only", master = "Office Theme")
      page_no <- page_no + 1

      slide_bg <- file.path(tmp_dir, paste0("bg_content_", page_no, ".png"))
      .make_flat_png(slide_bg, width_in = slide_w, height_in = slide_h, fill = th$bg_light)
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