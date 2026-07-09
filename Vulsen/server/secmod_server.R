
library(shiny)
library(shinyjs)
library(ggplot2)

# =============================================================================
# SECONDARY MODIFIER SERVER  (real-data, function_Secmod.R driven)
# =============================================================================
# IMPORTANT — one-time setup required in App.R:
#   1. Open module/function_Secmod.R and REMOVE (or comment out) the very
#      first executable line, `rm(list=ls())`. Sourcing that line wipes the
#      whole app environment on startup.
#   2. Add: source("module/function_Secmod.R")  next to the other
#      source("module/...") calls, BEFORE source("server/secmod_server.R").
#      This makes finaltable(), finaltable_allUSA(), indmod(), STATEminmax(),
#      Countryminmax(), CountryminmaxTable(), Credit_Penalty() and
#      STATE_plot() all available here.
#
# A couple of functions in function_Secmod.R (indmod, STATE_plot) reference
# a free variable `mycolors` rather than taking it as an argument. Because
# source() defines them in the global environment, this server keeps
# `mycolors` in sync with the stage-1 color pickers via assign(..., envir =
# .GlobalEnv) right before every call that needs it.
# =============================================================================

# ---- small helpers ----------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

sec_sanitize_key <- function(x) gsub("[^A-Za-z0-9]+", "_", as.character(x))

sec_status_badge <- function(done) {
  if (isTRUE(done)) {
    tags$span(style = "font-size:11px;color:#085041;background:#E1F5EE;padding:2px 10px;border-radius:6px;", "done")
  } else {
    tags$span(style = "font-size:11px;color:#888780;background:#F1EFE8;padding:2px 10px;border-radius:6px;", "pending")
  }
}

default_secmod_name <- function() {
  data.frame(
    "modifer" = c("Unknown","ARCHITECT","CLADRATE","CLADSYS","CONSTQUALI","EXTORN","FOUNDSYS",
                  "GARAGING","MECHGROUND","RESISTOPEN","ROOFAGE","ROOFANCH","ROOFEQUIP","ROOFGEOM",
                  "ROOFMAINT","ROOFSYS","TODMGPROVISION","TOFLASHING","TREEDENSITY"),
    "name" = c("Unknown","Residential Exterior","Roof Sheathing Attachment","Cladding Type",
               "Construction Quality","Commercial   Exterior","Frame-Foundation Connection","Garaging",
               "Ground-Level Equipment","Opening Protection","Roof Age","Roof Anchor","Rooftop Equipment",
               "Roof Geometry","Roof Condition","Roof covering","Damage Provision",
               "Flashing and Coping Quality","Tree Density"),
    stringsAsFactors = FALSE
  )
}

# a single plot card with its own download + add-to-cart + size-override controls,
# used for every card in the stage 5 / stage 6 "one by one" galleries
#
# NOTE: the plot frame is now a uiOutput() instead of a fixed-height
# plotOutput(). The server renders the actual plotOutput() into it with a
# height that matches whatever the width/height override (or the gallery
# default) currently is, so the card's box always matches the image inside
# it -- no more dead space or overflow when you resize a plot, and no more
# uneven gaps between cards in the gallery.
sec_plot_card_gallery <- function(key, label, prefix, default_h = 5) {
  ov_id <- paste0(prefix, "_override_", key)

  div(
    class = "cart-item-card sec-plot-card",
    style = "margin-bottom:14px;",

    div(
      class = "cart-item-header",
      span(class = "cart-item-badge", label),
      div(
        style = "display:flex; gap:4px;",
        tags$button(
          onclick = sprintf("$('#%s').toggleClass('sec-open');", ov_id),
          class = "btn-icon-cart", title = "Adjust size",
          icon("sliders-h")
        ),
        tags$button(
          onclick = sprintf("secmodDownloadClick('%s|%s')", prefix, key),
          class = "btn-icon-cart", title = "Download",
          icon("download")
        ),
        tags$button(
          onclick = sprintf("secmodCartClick('%s|%s')", prefix, key),
          class = "btn-icon-cart", title = "Add to cart",
          icon("cart-plus")
        )
      )
    ),

    div(
      class = "sec-plot-frame",
      uiOutput(paste0(prefix, "_plot_frame_", key))
    ),

    div(
      id = ov_id, class = "sec-override-panel",
      numericInput(paste0(prefix, "_w_", key), "Width (in)", value = NA, min = 3, max = 20, step = 0.5, width = "110px"),
      numericInput(paste0(prefix, "_h_", key), "Height (in)", value = NA, min = 2, max = 15, step = 0.5, width = "110px")
    )
  )
}

sec_gallery_controls_ui <- function(prefix, n, default_w, default_h, default_dpi) {
  div(
    style = "display:flex; flex-wrap:wrap; gap:16px; align-items:center; background:#f7f7fb; padding:10px 14px; border-radius:8px; margin-bottom:10px;",
    span(style = "font-size:13px;color:#718096;", "Default size for this gallery"),
    numericInput(paste0(prefix, "_default_w"), "Width (in)", value = default_w, min = 3, max = 20, step = 0.5, width = "110px"),
    numericInput(paste0(prefix, "_default_h"), "Height (in)", value = default_h, min = 2, max = 15, step = 0.5, width = "110px"),
    numericInput(paste0(prefix, "_default_dpi"), "DPI", value = default_dpi, min = 72, max = 300, step = 10, width = "100px"),
    actionButton(paste0(prefix, "_apply_all"), paste0("Apply to all ", n, " plots"), class = "btn-glass")
  )
}

# =============================================================================

secmod_server <- function(input, output, session, cart, username) {

  rv <- reactiveValues(
    aal_State = NULL, aal_USA = NULL, SecMod_name = default_secmod_name(),
    type_colors = c(Max = "#F0B323", Min = "#6FACDE"),
    mycolors    = c("#6FACDE", "#F0B323"),
    aal_final = NULL, aal_final_USA = NULL,
    aalp = NULL, aalp_USA = NULL, table_minmax_USA = NULL,
    credit_penalty_plot = NULL,
    state_lob_plots = NULL,
    modifier_plots = NULL, modifier_labels = NULL,
    pending_download = NULL
  )

  ## =========================================================================
  ## COLLAPSE / EXPAND — stage 1 open by default, rest collapsed
  ## =========================================================================

  shinyjs::show("stage1_body")
  shinyjs::onclick("stage1_header", shinyjs::toggle(id = "stage1_body"))
  shinyjs::onclick("stage2_header", shinyjs::toggle(id = "stage2_body"))
  shinyjs::onclick("stage3_header", shinyjs::toggle(id = "stage3_body"))
  shinyjs::onclick("stage4_header", shinyjs::toggle(id = "stage4_body"))
  shinyjs::onclick("stage5_header", shinyjs::toggle(id = "stage5_body"))
  shinyjs::onclick("stage6_header", shinyjs::toggle(id = "stage6_body"))

  ## =========================================================================
  ## STAGE PROGRESS STRIP + PER-STAGE BADGES
  ## =========================================================================

  output$sec_stepper <- renderUI({
    statuses <- c(
      !is.null(rv$aal_State) && !is.null(rv$aal_USA),
      !is.null(rv$aal_final) && !is.null(rv$aal_final_USA),
      !is.null(rv$table_minmax_USA),
      !is.null(rv$credit_penalty_plot),
      !is.null(rv$state_lob_plots),
      !is.null(rv$modifier_plots)
    )
    labels <- c("Load data", "Build tables", "Min/max", "Credit/penalty", "State sensitivity", "Modifier detail")
    n       <- length(labels)
    n_done  <- sum(statuses)
    current <- if (n_done < n) n_done + 1L else n
    pct     <- round(100 * n_done / n)

    # % of the track (between the *centers* of the first and last dot) that
    # should be filled -- based on the last completed step, not the current one,
    # so the line only advances once a stage is actually done
    fill_pct <- if (n > 1) max(0, n_done - 1) / (n - 1) * 100 else 0

    div(
      class = "sec-stepper glass-card",
      style = "padding:14px 20px 18px;",

      div(
        style = "display:flex; align-items:baseline; justify-content:space-between; margin-bottom:14px;",
        span(style = "font-size:13px; font-weight:700; color:#2d3748;", "Pipeline progress"),
        span(
          style = "font-size:12px; color:#718096;",
          sprintf("%d of %d stages complete \u00B7 %d%%", n_done, n, pct)
        )
      ),

      div(
        style = "position:relative; display:flex; align-items:flex-start; justify-content:space-between; padding:0 4px;",

        # background track, running through the vertical center of the dots (16px radius)
        div(style = "position:absolute; top:16px; left:20px; right:20px; height:3px; background:#e9ecf3; border-radius:3px;"),
        # filled portion of the track
        div(style = sprintf(
          "position:absolute; top:16px; left:20px; right:20px; height:3px; border-radius:3px;
           background:#4338ca; transform-origin:left; transform:scaleX(%s); transition:transform 0.35s ease;",
          fill_pct / 100
        )),

        lapply(seq_along(labels), function(i) {
          done       <- statuses[i]
          is_current <- !done && i == current

          div(
            style = "position:relative; z-index:2; display:flex; flex-direction:column; align-items:center; gap:8px; flex:1; min-width:0;",
            span(
              class = if (is_current) "sec-stepper-dot sec-stepper-current" else "sec-stepper-dot",
              style = sprintf(
                "width:32px; height:32px; border-radius:50%%; display:flex; align-items:center; justify-content:center;
                 font-size:12px; font-weight:700; flex-shrink:0; border:2px solid %s; background:%s; color:%s;",
                if (done) "#4338ca" else if (is_current) "#4338ca" else "#dfe3ee",
                if (done) "#4338ca" else "#ffffff",
                if (done) "#ffffff" else if (is_current) "#4338ca" else "#a0aec0"
              ),
              if (done) icon("check") else as.character(i)
            ),
            span(
              style = sprintf(
                "font-size:11px; text-align:center; line-height:1.25; max-width:88px; font-weight:%s; color:%s;",
                if (is_current) "700" else "500",
                if (done) "#4338ca" else if (is_current) "#2d3748" else "#a0aec0"
              ),
              labels[i]
            )
          )
        })
      )
    )
  })

  output$stage1_status <- renderUI(sec_status_badge(!is.null(rv$aal_State) && !is.null(rv$aal_USA)))
  output$stage2_status <- renderUI(sec_status_badge(!is.null(rv$aal_final) && !is.null(rv$aal_final_USA)))
  output$stage3_status <- renderUI(sec_status_badge(!is.null(rv$table_minmax_USA)))
  output$stage4_status <- renderUI(sec_status_badge(!is.null(rv$credit_penalty_plot)))
  output$stage5_status <- renderUI(sec_status_badge(!is.null(rv$state_lob_plots)))
  output$stage6_status <- renderUI(sec_status_badge(!is.null(rv$modifier_plots)))

  ## =========================================================================
  ## STAGE 1 — LOAD DATA
  ## =========================================================================

  observeEvent(input$sec_load, {
    req(input$sec_file_state, input$sec_file_usa)

    read_any <- function(path) {
      ext <- tolower(tools::file_ext(path))
      if (ext == "rds") readRDS(path)
      else if (ext == "csv") utils::read.csv(path, stringsAsFactors = FALSE)
      else stop("Unsupported file type: ", ext)
    }

    tryCatch({
      rv$aal_State <- read_any(input$sec_file_state$datapath)
      rv$aal_USA   <- read_any(input$sec_file_usa$datapath)

      rv$SecMod_name <- if (isTRUE(input$sec_use_default_mapping) || is.null(input$sec_file_mapping)) {
        default_secmod_name()
      } else {
        utils::read.csv(input$sec_file_mapping$datapath, stringsAsFactors = FALSE)
      }

      rv$type_colors <- c(Max = input$sec_color_max, Min = input$sec_color_min)
      rv$mycolors    <- c(input$sec_color_sfd, input$sec_color_com)
      assign("mycolors", rv$mycolors, envir = .GlobalEnv)

      session$sendCustomMessage("show-toast", list(
        text = "Data loaded", type = "success", icon = "fa-check", duration = 2000
      ))

      shinyjs::hide("stage1_body")
      shinyjs::show("stage2_body")
    }, error = function(e) {
      session$sendCustomMessage("show-toast", list(
        text = paste("Load failed:", conditionMessage(e)), type = "error",
        icon = "fa-exclamation-circle", duration = 4000
      ))
    })
  })

  ## =========================================================================
  ## STAGE 2 — BUILD TABLES  (finaltable / finaltable_allUSA), or upload-skip
  ## =========================================================================

  observeEvent(input$sec_build, {
    req(rv$aal_State, rv$aal_USA)
    tryCatch({
      rv$aal_final     <- finaltable(rv$aal_State, rv$SecMod_name)
      rv$aal_final_USA <- finaltable_allUSA(rv$aal_USA, rv$SecMod_name)

      session$sendCustomMessage("show-toast", list(
        text = "Tables built", type = "success", icon = "fa-check", duration = 2000
      ))

      shinyjs::hide("stage2_body")
      shinyjs::show("stage3_body")
    }, error = function(e) {
      session$sendCustomMessage("show-toast", list(
        text = paste("Build failed:", conditionMessage(e)), type = "error",
        icon = "fa-exclamation-circle", duration = 4000
      ))
    })
  })

  output$sec_dl_final <- downloadHandler(
    filename = function() "aal_final.csv",
    content  = function(file) { req(rv$aal_final); write.csv(rv$aal_final, file, row.names = FALSE) }
  )

  output$sec_dl_final_usa <- downloadHandler(
    filename = function() "aal_final_USA.csv",
    content  = function(file) { req(rv$aal_final_USA); write.csv(rv$aal_final_USA, file, row.names = FALSE) }
  )

  # -- upload a previously downloaded aal_final / aal_final_USA to skip stage 1+2 --

  observeEvent(input$sec_upload_final, {
    req(input$sec_upload_final)
    tryCatch({
      df <- utils::read.csv(input$sec_upload_final$datapath, stringsAsFactors = FALSE)
      df$type     <- factor(df$type, levels = c("SFD", "COM"))
      df$modifier <- factor(df$modifier, levels = unique(df$modifier))
      rv$aal_final <- df

      session$sendCustomMessage("show-toast", list(
        text = "aal_final.csv loaded", type = "success", icon = "fa-check", duration = 2000
      ))
    }, error = function(e) {
      session$sendCustomMessage("show-toast", list(
        text = paste("Upload failed:", conditionMessage(e)), type = "error",
        icon = "fa-exclamation-circle", duration = 4000
      ))
    })
  })

  observeEvent(input$sec_upload_final_usa, {
    req(input$sec_upload_final_usa)
    tryCatch({
      df <- utils::read.csv(input$sec_upload_final_usa$datapath, stringsAsFactors = FALSE)
      df$type     <- factor(df$type, levels = c("SFD", "COM"))
      df$modifier <- factor(df$modifier, levels = unique(df$modifier))
      rv$aal_final_USA <- df

      session$sendCustomMessage("show-toast", list(
        text = "aal_final_USA.csv loaded", type = "success", icon = "fa-check", duration = 2000
      ))
    }, error = function(e) {
      session$sendCustomMessage("show-toast", list(
        text = paste("Upload failed:", conditionMessage(e)), type = "error",
        icon = "fa-exclamation-circle", duration = 4000
      ))
    })
  })

  output$sec_stage2_summary <- renderUI({
    if (is.null(rv$aal_final)) return(NULL)
    tags$p(
      class = "commentary-text",
      sprintf(
        "aal_final: %s rows across %s states. aal_final_USA: %s rows.",
        format(nrow(rv$aal_final), big.mark = ","),
        length(unique(rv$aal_final$STATECODE)),
        if (is.null(rv$aal_final_USA)) "-" else format(nrow(rv$aal_final_USA), big.mark = ",")
      )
    )
  })

  ## =========================================================================
  ## STAGE 3 — MIN/MAX SUMMARY
  ## =========================================================================

  observeEvent(input$sec_minmax, {
    req(rv$aal_final, rv$aal_final_USA)
    tryCatch({
      rv$aalp             <- STATEminmax(rv$aal_final, rv$SecMod_name)
      rv$aalp_USA          <- Countryminmax(rv$aal_final_USA, rv$SecMod_name)
      rv$table_minmax_USA  <- CountryminmaxTable(rv$aal_final_USA, rv$SecMod_name)

      session$sendCustomMessage("show-toast", list(
        text = "Min/max computed", type = "success", icon = "fa-check", duration = 2000
      ))

      shinyjs::hide("stage3_body")
      shinyjs::show("stage4_body")
    }, error = function(e) {
      session$sendCustomMessage("show-toast", list(
        text = paste("Computation failed:", conditionMessage(e)), type = "error",
        icon = "fa-exclamation-circle", duration = 4000
      ))
    })
  })

  output$sec_minmax_tbl <- renderTable({
    req(rv$table_minmax_USA)
    tbl <- rv$table_minmax_USA
    tbl$Max <- round(tbl$Max, 1)
    tbl$Min <- round(tbl$Min, 1)
    tbl
  })

  ## =========================================================================
  ## STAGE 4 — CREDIT / PENALTY  (single named plot, full control strip)
  ## =========================================================================

  observeEvent(input$sec_credit, {
    req(rv$table_minmax_USA)
    tryCatch({
      rv$credit_penalty_plot <- Credit_Penalty(rv$table_minmax_USA, rv$type_colors)

      session$sendCustomMessage("show-toast", list(
        text = "Credit/penalty chart generated", type = "success", icon = "fa-chart-bar", duration = 2000
      ))

      shinyjs::hide("stage4_body")
      shinyjs::show("stage5_body")
    }, error = function(e) {
      session$sendCustomMessage("show-toast", list(
        text = paste("Chart failed:", conditionMessage(e)), type = "error",
        icon = "fa-exclamation-circle", duration = 4000
      ))
    })
  })

  output$sec_credit_card <- renderUI({
    req(rv$credit_penalty_plot)
    div(
      class = "cart-item-card",
      div(
        class = "cart-item-header",
        span(class = "cart-item-badge", "Credit / Penalty"),
        div(
          style = "display:flex; gap:4px;",
          tags$button(onclick = "secmodDownloadClick('s4|credit_penalty')", class = "btn-icon-cart", title = "Download", icon("download")),
          tags$button(onclick = "secmodCartClick('s4|credit_penalty')", class = "btn-icon-cart", title = "Add to cart", icon("cart-plus"))
        )
      ),
      plotOutput("s4_plot", height = "420px"),
      div(
        style = "display:flex; gap:16px; margin-top:12px; padding-top:12px; border-top:0.5px solid #e2e8f0;",
        numericInput("sec_credit_w", "Width (in)", value = 9, min = 4, max = 20, step = 0.5, width = "110px"),
        numericInput("sec_credit_h", "Height (in)", value = 6, min = 3, max = 15, step = 0.5, width = "110px"),
        numericInput("sec_credit_dpi", "DPI", value = 150, min = 72, max = 300, step = 10, width = "100px")
      )
    )
  })

  output$s4_plot <- renderPlot({
    req(rv$credit_penalty_plot)
    rv$credit_penalty_plot
  },
  width  = function() (input$sec_credit_w %||% 9) * 96,
  height = function() (input$sec_credit_h %||% 6) * 96)

  ## =========================================================================
  ## STAGE 5 — STATE SENSITIVITY  (STATE_plot for every state x LOB)
  ## =========================================================================

  observeEvent(input$sec_state_plots, {
    req(rv$aalp)
    assign("mycolors", rv$mycolors, envir = .GlobalEnv)

    tryCatch({
      states <- sort(unique(rv$aalp$STATECODE))
      lobs   <- c("SFD", "COM")

      plots <- list()
      for (st in states) {
        for (lob in lobs) {
          key <- paste0(sec_sanitize_key(st), "_", lob)
          plots[[key]] <- STATE_plot(rv$aalp, LOB = lob, state_code = st)
        }
      }
      rv$state_lob_plots <- plots

      session$sendCustomMessage("show-toast", list(
        text = paste0(length(plots), " state plots generated"), type = "success",
        icon = "fa-chart-bar", duration = 2000
      ))
    }, error = function(e) {
      session$sendCustomMessage("show-toast", list(
        text = paste("Generation failed:", conditionMessage(e)), type = "error",
        icon = "fa-exclamation-circle", duration = 4000
      ))
    })
  })

  output$sec_stage5_gallery_controls <- renderUI({
    req(rv$state_lob_plots)
    sec_gallery_controls_ui("s5", length(rv$state_lob_plots), 9, 5, 150)
  })

  output$sec_stage5_gallery <- renderUI({
    req(rv$state_lob_plots)
    keys <- names(rv$state_lob_plots)
    tagList(lapply(keys, function(k) sec_plot_card_gallery(k, gsub("_", " ", k), "s5", default_h = 5)))
  })

  observeEvent(input$s5_apply_all, {
    req(rv$state_lob_plots)
    for (k in names(rv$state_lob_plots)) {
      updateNumericInput(session, paste0("s5_w_", k), value = NA)
      updateNumericInput(session, paste0("s5_h_", k), value = NA)
    }
  })

  observe({
    req(rv$state_lob_plots)
    keys <- names(rv$state_lob_plots)
    lapply(keys, function(key) {
      local({
        k <- key

        # container: height tracks the current override/default so the card
        # never shows dead space or clips the plot underneath it
        output[[paste0("s5_plot_frame_", k)]] <- renderUI({
          h_in <- (input[[paste0("s5_h_", k)]] %||% input$s5_default_h) %||% 5
          plotOutput(paste0("s5_plot_", k), height = paste0(round(h_in * 96), "px"))
        })

        output[[paste0("s5_plot_", k)]] <- renderPlot({
          rv$state_lob_plots[[k]]
        },
        width  = function() ((input[[paste0("s5_w_", k)]] %||% input$s5_default_w) %||% 9) * 96,
        height = function() ((input[[paste0("s5_h_", k)]] %||% input$s5_default_h) %||% 5) * 96)
      })
    })
  })

  ## =========================================================================
  ## STAGE 6 — MODIFIER DETAIL  (indmod() for every modifier)
  ## =========================================================================

  observeEvent(input$sec_modifier_plots, {
    req(rv$aal_final)
    assign("mycolors", rv$mycolors, envir = .GlobalEnv)

    tryCatch({
      classif <- levels(rv$aal_final$modifier)
      classif <- classif[classif != "Unknown"]

      pl   <- indmod(rv$aal_final)
      keys <- vapply(classif, sec_sanitize_key, character(1))
      names(pl) <- keys

      rv$modifier_plots  <- pl
      rv$modifier_labels <- setNames(classif, keys)

      session$sendCustomMessage("show-toast", list(
        text = paste0(length(pl), " modifier plots generated"), type = "success",
        icon = "fa-chart-bar", duration = 2000
      ))
    }, error = function(e) {
      session$sendCustomMessage("show-toast", list(
        text = paste("Generation failed:", conditionMessage(e)), type = "error",
        icon = "fa-exclamation-circle", duration = 4000
      ))
    })
  })

  output$sec_stage6_gallery_controls <- renderUI({
    req(rv$modifier_plots)
    sec_gallery_controls_ui("s6", length(rv$modifier_plots), 10, 6, 150)
  })

  output$sec_stage6_gallery <- renderUI({
    req(rv$modifier_plots)
    keys <- names(rv$modifier_plots)
    tagList(lapply(keys, function(k) sec_plot_card_gallery(k, rv$modifier_labels[[k]], "s6", default_h = 6)))
  })

  observeEvent(input$s6_apply_all, {
    req(rv$modifier_plots)
    for (k in names(rv$modifier_plots)) {
      updateNumericInput(session, paste0("s6_w_", k), value = NA)
      updateNumericInput(session, paste0("s6_h_", k), value = NA)
    }
  })

  observe({
    req(rv$modifier_plots)
    keys <- names(rv$modifier_plots)
    lapply(keys, function(key) {
      local({
        k <- key

        output[[paste0("s6_plot_frame_", k)]] <- renderUI({
          h_in <- (input[[paste0("s6_h_", k)]] %||% input$s6_default_h) %||% 6
          plotOutput(paste0("s6_plot_", k), height = paste0(round(h_in * 96), "px"))
        })

        output[[paste0("s6_plot_", k)]] <- renderPlot({
          rv$modifier_plots[[k]]
        },
        width  = function() ((input[[paste0("s6_w_", k)]] %||% input$s6_default_w) %||% 10) * 96,
        height = function() ((input[[paste0("s6_h_", k)]] %||% input$s6_default_h) %||% 6) * 96)
      })
    })
  })

  ## =========================================================================
  ## ADD TO CART  — one shared handler for stage 4 / 5 / 6 plot cards
  ## =========================================================================

  observeEvent(input$sec_cart_click, {
    key_full <- input$sec_cart_click$key
    parts    <- strsplit(key_full, "\\|")[[1]]
    prefix   <- parts[1]
    key      <- parts[2]

    plot_obj <- switch(prefix,
      "s4" = rv$credit_penalty_plot,
      "s5" = rv$state_lob_plots[[key]],
      "s6" = rv$modifier_plots[[key]]
    )
    req(plot_obj)

    item <- list(
      id         = generate_item_id(),
      module     = "Secondary Modifier",
      plot       = plot_obj,
      commentary = NULL,
      timestamp  = Sys.time()
    )

    current_cart <- cart()
    current_cart[[length(current_cart) + 1]] <- item
    cart(current_cart)
    save_cart(username(), current_cart)

    session$sendCustomMessage("show-toast", list(
      text = "Added to cart", type = "success", icon = "fa-cart-plus", duration = 2000
    ))
  })

  ## =========================================================================
  ## PER-PLOT DOWNLOAD  — click sets the pending key, then "clicks" the
  ## hidden downloadButton so the browser gets a real file download
  ## =========================================================================

  observeEvent(input$sec_download_click, {
    key_full <- input$sec_download_click$key
    parts    <- strsplit(key_full, "\\|")[[1]]
    rv$pending_download <- list(prefix = parts[1], key = parts[2])
    shinyjs::runjs("document.getElementById('sec_gallery_download').click();")
  })

  output$sec_gallery_download <- downloadHandler(
    filename = function() {
      pd <- rv$pending_download
      req(pd)
      paste0("secmod_", pd$prefix, "_", pd$key, ".png")
    },
    content = function(file) {
      pd <- rv$pending_download
      req(pd)

      if (pd$prefix == "s4") {
        plot_obj <- rv$credit_penalty_plot
        w   <- input$sec_credit_w %||% 9
        h   <- input$sec_credit_h %||% 6
        dpi <- input$sec_credit_dpi %||% 150
      } else if (pd$prefix == "s5") {
        plot_obj <- rv$state_lob_plots[[pd$key]]
        w   <- (input[[paste0("s5_w_", pd$key)]] %||% input$s5_default_w) %||% 9
        h   <- (input[[paste0("s5_h_", pd$key)]] %||% input$s5_default_h) %||% 5
        dpi <- input$s5_default_dpi %||% 150
      } else if (pd$prefix == "s6") {
        plot_obj <- rv$modifier_plots[[pd$key]]
        w   <- (input[[paste0("s6_w_", pd$key)]] %||% input$s6_default_w) %||% 10
        h   <- (input[[paste0("s6_h_", pd$key)]] %||% input$s6_default_h) %||% 6
        dpi <- input$s6_default_dpi %||% 150
      } else {
        plot_obj <- NULL
      }

      req(plot_obj)
      ggsave(file, plot = plot_obj, width = w, height = h, dpi = dpi, limitsize = FALSE)
    }
  )
}
