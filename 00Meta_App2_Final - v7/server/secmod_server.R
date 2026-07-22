library(shiny)
library(shinyjs)
library(ggplot2)
library(colourpicker)

# =============================================================================
# SECONDARY MODIFIER SERVER — UPDATED FOR 6-SECTION UI
# =============================================================================

# ---- helpers -----------------------------------------------------------------
is_transparent_bg <- function(x) {
  if (is.null(x)) return(FALSE)
  if (is.logical(x)) return(isTRUE(x))
  tolower(trimws(as.character(x))) %in% c("transparent", "na", "none")
}

sec_sanitize_key <- function(x) gsub("[^A-Za-z0-9]+", "_", as.character(x))

sec_status_badge <- function(done) {
  if (isTRUE(done)) {
    tags$span(class = "sec2-status done", icon("check"), "Done")
  } else {
    tags$span(class = "sec2-status pending", icon("clock"), "Pending")
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

safe_recolor_fill <- function(p, named_colors) {
  tryCatch({
    fill_quo <- p$mapping$fill
    if (is.null(fill_quo) && length(p$layers) > 0) {
      for (l in p$layers) {
        if (!is.null(l$mapping$fill)) { fill_quo <- l$mapping$fill; break }
      }
    }
    if (is.null(fill_quo)) return(p)
    
    fill_var <- tryCatch(rlang::as_label(fill_quo), error = function(e) NULL)
    if (is.null(fill_var) || !fill_var %in% names(p$data)) return(p)
    
    data_levels <- unique(as.character(p$data[[fill_var]]))
    data_levels <- data_levels[!is.na(data_levels)]
    if (length(data_levels) == 0) return(p)
    
    vals <- named_colors[names(named_colors) %in% data_levels]
    missing <- setdiff(data_levels, names(vals))
    if (length(missing) > 0) {
      vals <- c(vals, setNames(scales::hue_pal()(length(missing)), missing))
    }
    
    p + scale_fill_manual(values = vals)
  }, error = function(e) p)
}

count_facet_panels <- function(p) {
  tryCatch({
    built <- ggplot2::ggplot_build(p)
    n <- nrow(built$layout$layout)
    if (is.null(n) || is.na(n) || n < 1) 1L else n
  }, error = function(e) 1L)
}

apply_plot_overrides <- function(p, input, prefix, key, defaults, is_credit = FALSE) {
  # ---- Plot Layout Properties ----
  axis_text <- (input[[paste0(prefix, "_axis_text_", key)]] %||% defaults$axis_text) %||% 12
  axis_title <- (input[[paste0(prefix, "_axis_title_", key)]] %||% defaults$axis_title) %||% 14
  plot_title <- (input[[paste0(prefix, "_plot_title_", key)]] %||% defaults$plot_title) %||% 16
  strip_text <- (input[[paste0(prefix, "_strip_text_", key)]] %||% defaults$strip_text) %||% 12
  legend_text <- (input[[paste0(prefix, "_legend_text_", key)]] %||% defaults$legend_text) %||% 10
  axis_angle <- (input[[paste0(prefix, "_axis_angle_", key)]] %||% defaults$axis_angle) %||% 90
  show_legend <- input[[paste0(prefix, "_legend_show_", key)]]
  if (is.null(show_legend)) show_legend <- defaults$legend_show %||% TRUE
  legend_key_size <- (input[[paste0(prefix, "_legend_key_size_", key)]] %||% defaults$legend_key_size) %||% 0.8
  panel_spacing <- (input[[paste0(prefix, "_panel_spacing_", key)]] %||% defaults$panel_spacing) %||% 0.5
  axis_text_margin_t <- (input[[paste0(prefix, "_axis_text_margin_t_", key)]] %||% defaults$axis_text_margin_t) %||% 5
  axis_text_vjust <- (input[[paste0(prefix, "_axis_text_vjust_", key)]] %||% defaults$axis_text_vjust) %||% 1
  
  # ---- Colors ----
  col_sfd <- input[[paste0(prefix, "_col_sfd_", key)]] %||% defaults$col_sfd %||% "#6FACDE"
  col_com <- input[[paste0(prefix, "_col_com_", key)]] %||% defaults$col_com %||% "#F0B323"
  col_pen <- input[[paste0(prefix, "_col_pen_", key)]] %||% defaults$col_pen %||% "#F0B323"
  col_cred <- input[[paste0(prefix, "_col_cred_", key)]] %||% defaults$col_cred %||% "#6FACDE"
  
  # ---- Background ----
  # NOTE: must read the PER-KEY background override first, falling back to
  # `defaults$bg` (sourced from gallery_defaults_s5()/s6(), which only
  # updates when "Apply to all plots" is pressed). Reading the live
  # gallery-default input (`prefix_default_bg`) directly here would make
  # every plot's renderPlot() reactively depend on that control, so any
  # gallery-default panel tweak would instantly redraw the entire gallery
  # instead of waiting for the Apply button.
  bg_choice <- (input[[paste0(prefix, "_bg_", key)]] %||% defaults$bg) %||% FALSE
  is_transp <- is_transparent_bg(bg_choice)
  
  # ---- Dense scaling for many facets ----
  n_panels <- count_facet_panels(p)
  dense_scale <- if (n_panels > 12) max(0.18, sqrt(12 / n_panels)) else 1
  
  panel_spacing <- panel_spacing * dense_scale
  axis_text <- max(6, axis_text * max(dense_scale, 0.65))
  strip_text <- max(6, strip_text * max(dense_scale, 0.65))
  
  # ---- Apply theme ----
  p <- p + theme(
    axis.text.x = element_text(size = axis_text, angle = axis_angle,
                               hjust = 1, vjust = axis_text_vjust,
                               margin = margin(t = axis_text_margin_t)),
    axis.text.y = element_text(size = axis_text),
    axis.title.x = element_text(size = axis_title),
    axis.title.y = element_text(size = axis_title),
    plot.title = element_text(size = plot_title, hjust = 0.5),
    strip.text = element_text(size = strip_text),
    strip.background = element_blank(),
    legend.text = element_text(size = legend_text),
    legend.title = element_blank(),  # remove legend title
    legend.position = if (show_legend) "top" else "none",
    legend.key.size = unit(legend_key_size, "cm"),
    axis.line = element_blank(),  # removed axis line
    axis.ticks = element_line(colour = "black"),
    panel.background  = element_rect(fill = if (is_transp) NA else "white", colour = NA),
    plot.background   = element_rect(fill = if (is_transp) NA else "white", colour = NA),
    legend.background = element_rect(fill = if (is_transp) NA else "white", colour = NA),
    legend.box.background = element_blank(),
    legend.key        = element_rect(fill = if (is_transp) NA else "white", colour = NA),
    panel.grid.major = element_line(colour = "#e9ecf3"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(panel_spacing, "lines"),
    plot.margin = margin(t = 30, r = 10, b = 30, l = 10),  # fixed margins
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)
  )
  
  # ---- Enforce no x-axis title for state plots (prefix s5) ----
  if (prefix == "s5") {
    p <- p + labs(x = NULL) + theme(axis.title.x = element_blank())
  }
  
  # ---- Recolor ----
  p <- safe_recolor_fill(p, c("SFD" = col_sfd, "COM" = col_com,
                              "Max" = col_pen, "Min" = col_cred,
                              "Penalty" = col_pen, "Credit" = col_cred))
  
  p <- p + coord_cartesian(clip = "off")
  attr(p, "vulsen_bg") <- if (is_transp) "transparent" else "white"
  p
}

# =============================================================================
# MAIN SERVER FUNCTION (module)
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
    customized_report_generated = FALSE,
    customized_report_file = NULL
  )
  
  # ---- Report generation locks ----
  report_running <- reactiveVal(FALSE)
  report_generated_once <- reactiveVal(FALSE)
  
  inputs_ready <- reactive({
    !is.null(rv$aal_State) && !is.null(rv$aal_USA) && !is.null(rv$SecMod_name)
  })
  
  gallery_defaults_s5 <- reactiveVal(
    list(
      w = 13, h = 7.5,
      axis_text = 12, axis_title = 14, plot_title = 16, strip_text = 12,
      legend_text = 10,
      axis_angle = 90,
      legend_key_size = 0.8,
      panel_spacing = 0.5,
      col_sfd = "#6FACDE", col_com = "#F0B323",
      col_pen = "#F0B323", col_cred = "#6FACDE",
      axis_text_margin_t = 5, axis_text_vjust = 1,
      bg = FALSE
    )
  )
  
  gallery_defaults_s6 <- reactiveVal(
    list(
      w = 13, h = 9,
      axis_text = 12, axis_title = 14, plot_title = 16, strip_text = 12,
      legend_text = 10,
      axis_angle = 90,
      legend_key_size = 0.8,
      panel_spacing = 0.5,
      col_sfd = "#6FACDE", col_com = "#F0B323",
      col_pen = "#F0B323", col_cred = "#6FACDE",
      axis_text_margin_t = 5, axis_text_vjust = 1,
      bg = FALSE
    )
  )
  
  # ---- Status badge for Model and Exposure Settings ----
  output$sec_settings_status <- renderUI({
    all_filled <- nzchar(input$sec_vendor %||% "") &&
      nzchar(input$sec_country %||% "") &&
      nzchar(input$sec_peril %||% "") &&
      nzchar(input$sec_subperil %||% "") &&
      nzchar(input$sec_suffix %||% "")
    sec_status_badge(all_filled)
  })
  
  # ---- Dynamic subperil dropdown ----
  output$sec_subperil_ui <- renderUI({
    req(input$sec_peril)
    choices <- peril_lookup()[[input$sec_peril]]
    selectInput(session$ns("sec_subperil"), "Subperil",
                choices = choices,
                selected = choices[1],
                width = "100%", selectize = FALSE)
  })
  
  # ---- Sample file downloads (pointing to actual files) ----
  output$sec_dl_sample_state <- downloadHandler(
    filename = function() "AllPerils_secmod_average_state_HDv1.csv",
    content = function(file) {
      file.copy("data/secmod_sample/AllPerils_secmod_average_state_HDv1.csv", file, overwrite = TRUE)
    }
  )
  output$sec_dl_sample_country <- downloadHandler(
    filename = function() "AllPerils_secmod_average_USA_HDv1.csv",
    content = function(file) {
      file.copy("data/secmod_sample/AllPerils_secmod_average_USA_HDv1.csv", file, overwrite = TRUE)
    }
  )
  output$sec_dl_sample_mapping <- downloadHandler(
    filename = function() "modifer,name.csv",
    content = function(file) {
      file.copy("data/secmod_sample/modifer,name.csv", file, overwrite = TRUE)
    }
  )
  
  # ---- Helper: read any file (rds or csv) ----
  read_any <- function(datapath, original_name) {
    ext <- tolower(tools::file_ext(original_name))
    if (ext == "rds") readRDS(datapath)
    else if (ext == "csv") utils::read.csv(datapath, stringsAsFactors = FALSE)
    else stop("Unsupported file type: ", ext)
  }
  
  # ---- Helper: validate columns ----
  validate_columns <- function(df, required, label) {
    missing <- setdiff(required, names(df))
    if (length(missing) > 0) {
      stop(sprintf("%s file is missing required columns: %s", label, paste(missing, collapse = ", ")))
    }
  }
  
  # ---- ANALYSE INPUT (combines load, build, minmax) ----
  observeEvent(input$sec_analyse, {
    # Check if all three files are uploaded
    if (is.null(input$sec_file_state) || is.null(input$sec_file_usa) || is.null(input$sec_file_mapping)) {
      showNotification("Please upload all three required files: State AAL, Country AAL, and SecMod File.", type = "error")
      return()
    }
    
    tryCatch({
      # Read and validate State AAL
      state_df <- read_any(input$sec_file_state$datapath, input$sec_file_state$name)
      validate_columns(state_df, c("STATECODE", "Classification", "Description", "AAL"), "State AAL")
      
      # Read and validate Country AAL
      country_df <- read_any(input$sec_file_usa$datapath, input$sec_file_usa$name)
      validate_columns(country_df, c("Classification", "Description", "AAL"), "Country AAL")
      
      # Read and validate SecMod mapping
      mapping_df <- read_any(input$sec_file_mapping$datapath, input$sec_file_mapping$name)
      validate_columns(mapping_df, c("modifer", "name"), "SecMod mapping")
      
      # Store in reactive values
      rv$aal_State <- state_df
      rv$aal_USA   <- country_df
      rv$SecMod_name <- mapping_df
      
      # ---- Set colours from defaults (no longer from UI) ----
      rv$type_colors <- c(Max = "#F0B323", Min = "#6FACDE")
      rv$mycolors    <- c("#6FACDE", "#F0B323")
      assign("mycolors", rv$mycolors, envir = .GlobalEnv)
      
      # Build tables
      rv$aal_final     <- finaltable(rv$aal_State, rv$SecMod_name)
      rv$aal_final_USA <- finaltable_allUSA(rv$aal_USA, rv$SecMod_name)
      
      # Compute min/max
      rv$aalp            <- STATEminmax(rv$aal_final, rv$SecMod_name)
      rv$aalp_USA        <- Countryminmax(rv$aal_final_USA, rv$SecMod_name)
      rv$table_minmax_USA <- CountryminmaxTable(rv$aal_final_USA, rv$SecMod_name)
      
      showNotification("Analyse Input completed successfully. Min/Max table is now available.",
                       type = "message", duration = 5)
      
      updateTabsetPanel(session, "sec_output_tabs", selected = "Min / max Table")
      
    }, error = function(e) {
      showNotification(paste("Analyse Input failed:", conditionMessage(e)),
                       type = "error", duration = NULL)
    })
  })
  
  # ---- Status badges (matching new 6-section UI) ----
  output$sec_stage1_status <- renderUI({
    sec_status_badge(inputs_ready())
  })
  
  output$sec_stage2_status <- renderUI(sec_status_badge(!is.null(rv$aal_final) && !is.null(rv$aal_final_USA)))
  output$sec_stage3_status <- renderUI(sec_status_badge(!is.null(rv$table_minmax_USA)))
  output$sec_stage4_status <- renderUI(sec_status_badge(!is.null(rv$credit_penalty_plot)))
  output$sec_stage5_status <- renderUI(sec_status_badge(!is.null(rv$state_lob_plots)))
  output$sec_stage6_status <- renderUI(sec_status_badge(!is.null(rv$modifier_plots)))
  output$sec_stage7_status <- renderUI(sec_status_badge(isTRUE(rv$customized_report_generated)))
  
  # ---- New review tabs: State AAL, Country AAL, SecMod File ----
  output$sec_state_aal_summary <- renderUI({
    df <- rv$aal_State
    if (is.null(df)) {
      return(p("No State AAL data loaded yet."))
    }
    fname <- if (!is.null(input$sec_file_state)) input$sec_file_state$name else "Demo data"
    tagList(
      strong("File: "), fname, tags$br(),
      strong("Rows: "), format(nrow(df), big.mark = ","), tags$br(),
      strong("Columns: "), ncol(df), tags$br(),
      strong("Size: "), format(object.size(df), units = "auto")
    )
  })
  
  output$sec_state_aal_table <- renderDT({
    req(rv$aal_State)
    datatable(rv$aal_State,
              options = list(pageLength = 10, scrollX = TRUE, dom = 'Bfrtip'),
              rownames = FALSE)
  })
  
  output$sec_country_aal_summary <- renderUI({
    df <- rv$aal_USA
    if (is.null(df)) {
      return(p("No Country AAL data loaded yet."))
    }
    fname <- if (!is.null(input$sec_file_usa)) input$sec_file_usa$name else "Demo data"
    tagList(
      strong("File: "), fname, tags$br(),
      strong("Rows: "), format(nrow(df), big.mark = ","), tags$br(),
      strong("Columns: "), ncol(df), tags$br(),
      strong("Size: "), format(object.size(df), units = "auto")
    )
  })
  
  output$sec_country_aal_table <- renderDT({
    req(rv$aal_USA)
    datatable(rv$aal_USA,
              options = list(pageLength = 10, scrollX = TRUE, dom = 'Bfrtip'),
              rownames = FALSE)
  })
  
  output$sec_mod_file_summary <- renderUI({
    df <- rv$SecMod_name
    if (is.null(df)) {
      return(p("No SecMod mapping file loaded yet."))
    }
    fname <- if (!is.null(input$sec_file_mapping)) input$sec_file_mapping$name else "Demo data"
    tagList(
      strong("File: "), fname, tags$br(),
      strong("Rows: "), format(nrow(df), big.mark = ","), tags$br(),
      strong("Columns: "), ncol(df), tags$br(),
      strong("Size: "), format(object.size(df), units = "auto")
    )
  })
  
  output$sec_mod_file_table <- renderDT({
    req(rv$SecMod_name)
    datatable(rv$SecMod_name,
              options = list(pageLength = 10, scrollX = TRUE, dom = 'Bfrtip'),
              rownames = FALSE)
  })
  
  # ---- Min/max table ----
  output$sec_minmax_tbl <- renderDT({
    req(rv$table_minmax_USA)
    tbl <- rv$table_minmax_USA
    tbl$Max <- round(tbl$Max, 1)
    tbl$Min <- round(tbl$Min, 1)
    datatable(tbl,
              options = list(pageLength = 10, scrollX = TRUE, dom = 'Bfrtip'),
              rownames = FALSE)
  })
  
  # ---- Download handlers for aal_final CSVs ----
  output$sec_dl_final <- downloadHandler(
    filename = function() "aal_final.csv",
    content  = function(file) { req(rv$aal_final); write.csv(rv$aal_final, file, row.names = FALSE) }
  )
  output$sec_dl_final_usa <- downloadHandler(
    filename = function() "aal_final_USA.csv",
    content  = function(file) { req(rv$aal_final_USA); write.csv(rv$aal_final_USA, file, row.names = FALSE) }
  )
  
  # ---- Upload aal_final CSVs (skip analysis) ----
  observeEvent(input$sec_upload_final, {
    req(input$sec_upload_final)
    tryCatch({
      df <- utils::read.csv(input$sec_upload_final$datapath, stringsAsFactors = FALSE)
      df$type     <- factor(df$type, levels = c("SFD", "COM"))
      df$modifier <- factor(df$modifier, levels = unique(df$modifier))
      rv$aal_final <- df
      showNotification("aal_final.csv loaded", type = "message", duration = 5)
    }, error = function(e) {
      showNotification(paste("Upload failed:", conditionMessage(e)), type = "error", duration = NULL)
    })
  })
  
  observeEvent(input$sec_upload_final_usa, {
    req(input$sec_upload_final_usa)
    tryCatch({
      df <- utils::read.csv(input$sec_upload_final_usa$datapath, stringsAsFactors = FALSE)
      df$type     <- factor(df$type, levels = c("SFD", "COM"))
      df$modifier <- factor(df$modifier, levels = unique(df$modifier))
      rv$aal_final_USA <- df
      showNotification("aal_final_USA.csv loaded", type = "message", duration = 5)
    }, error = function(e) {
      showNotification(paste("Upload failed:", conditionMessage(e)), type = "error", duration = NULL)
    })
  })
  
  # ---- Summary text ----
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
  
  # =========================================================================
  # SECTION 3 — CREDIT / PENALTY
  # =========================================================================
  credit_penalty_plot_final <- reactive({
    req(rv$credit_penalty_plot)
    p <- rv$credit_penalty_plot

    # Live re-wrap of Description labels using the current "Label wrap width"
    # input. Credit_Penalty() only wraps once at generation time; without this,
    # changing the input after the initial "Generate" click has no effect.
    if (!is.null(rv$credit_penalty_desc_raw) &&
        !is.null(p$data) && "Description" %in% names(p$data) &&
        nrow(p$data) == length(rv$credit_penalty_desc_raw)) {
      p$data$Description <- str_wrap(rv$credit_penalty_desc_raw, width = input$s4_label_wrap_width %||% 12)
    }

    # Apply overrides (simplified)
    p <- p + theme(
      axis.text.x = element_text(size = input$s4_axis_text %||% 12,
                                 angle = input$s4_axis_angle %||% 20,
                                 hjust = 1, vjust = input$s4_axis_text_vjust %||% 1,
                                 margin = margin(t = input$s4_axis_text_margin_t %||% 5)),
      axis.text.y = element_text(size = input$s4_axis_text %||% 12),
      axis.title.x = element_text(size = input$s4_axis_title %||% 14),
      axis.title.y = element_text(size = input$s4_axis_title %||% 14),
      plot.title = element_text(size = input$s4_plot_title %||% 16, hjust = 0.5),
      strip.text = element_text(size = input$s4_strip_text %||% 12),
      strip.background = element_blank(),
      legend.text = element_text(size = input$s4_legend_text %||% 10),
      legend.title = element_blank(),
      legend.position = if (input$s4_legend_show %||% TRUE) "top" else "none",
      legend.key.size = unit(input$s4_legend_key_size %||% 0.8, "cm"),
      axis.line = element_blank(),
      axis.ticks = element_line(colour = "black"),
      panel.background  = element_rect(fill = if (isTRUE(input$s4_bg)) NA else "white", colour = NA),
      plot.background   = element_rect(fill = if (isTRUE(input$s4_bg)) NA else "white", colour = NA),
      legend.background = element_rect(fill = if (isTRUE(input$s4_bg)) NA else "white", colour = NA),
      panel.grid.major = element_line(colour = "#e9ecf3"),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(input$s4_panel_spacing %||% 0.5, "lines"),
      plot.margin = margin(t = 30, r = 10, b = 30, l = 10),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)
    ) +
      scale_fill_manual(
        values = c(
          "Max" = input$s4_col_pen %||% "#F0B323",
          "Min" = input$s4_col_cred %||% "#6FACDE"
        ),
        labels = c("Max" = "Penalty", "Min" = "Credit")
      ) +
      coord_cartesian(clip = "off")
    
    if (length(p$layers) > 0) {
      for (i in seq_along(p$layers)) {
        if (inherits(p$layers[[i]]$geom, "GeomText")) {
          p$layers[[i]]$aes_params$size <- input$s4_label_size %||% 3
          p$layers[[i]]$aes_params$angle <- input$s4_label_angle %||% 0
        }
      }
    }
    
    p
  })
  
  observeEvent(input$sec_credit, {
    if (is.null(rv$table_minmax_USA)) {
      showNotification("Please run 'Analyse Input' first to prepare the required data.", type = "error")
      return()
    }
    tryCatch({
      # Get label wrap width from UI
      label_wrap_width <- input$s4_label_wrap_width %||% 12
      rv$credit_penalty_plot <- Credit_Penalty(rv$table_minmax_USA, rv$type_colors, label_wrap_width = label_wrap_width)

      # Also keep the RAW (unwrapped) Description text, built with the exact
      # same transform/order/filter Credit_Penalty() uses internally, so that
      # credit_penalty_plot_final() can re-wrap live whenever the "Label wrap
      # width" input changes, instead of the wrap only ever being applied once
      # here at generation time.
      rv$credit_penalty_desc_raw <- {
        df_raw <- rv$table_minmax_USA %>%
          mutate(Class_Size = abs(Max) + abs(Min)) %>%
          pivot_longer(cols = c(Max, Min), names_to = "Type", values_to = "Value") %>%
          mutate(Description = ifelse(Type == "Max", Max_Description, Min_Description)) %>%
          mutate(Modifier = fct_reorder(Modifier, Class_Size, .desc = TRUE)) %>%
          filter(abs(Value) > 10)
        df_raw$Description
      }
      
      output$s4_dl <- downloadHandler(
        filename = function() { "secmod_credit_penalty.png" },
        content = function(file) {
          p <- credit_penalty_plot_final()
          req(p)
          width_in  <- input$s4_w   %||% 12.5
          height_in <- input$s4_h   %||% 8
          dpi_val   <- input$s4_dpi %||% 300
          bg_val <- if (isTRUE(input$s4_bg)) "transparent" else "white"
          ggsave(file, plot = p, width = width_in, height = height_in,
                 dpi = dpi_val, bg = bg_val, limitsize = FALSE)
        }
      )
      
      showNotification("Credit/penalty chart generated. Please wait a minute for the plot to render on the tab...", type = "message", duration = 6)
      updateTabsetPanel(session, "sec_output_tabs", selected = "Credit / Penalty")
    }, error = function(e) {
      showNotification(paste("Chart failed:", conditionMessage(e)), type = "error", duration = NULL)
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
          tags$button(
            onclick = "$('#s4_override_panel').toggleClass('sec-open');",
            class = "btn-icon-cart sec2-icon-btn", title = "Adjust size, colours, text & legend",
            icon("sliders-h")
          ),
          downloadButton(
            outputId = session$ns("s4_dl"),
            label = NULL,
            icon = icon("download"),
            class = "btn-icon-cart sec2-icon-btn",
            title = "Download"
          ),
          tags$button(onclick = "secmodCartClick('s4|credit_penalty')",
                      class = "btn-icon-cart sec2-icon-btn", title = "Add to cart", icon("cart-plus"))
        )
      ),
      div(class = "sec-plot-frame", uiOutput(session$ns("s4_plot_frame"))),
      div(
        id = "s4_override_panel", class = "sec-override-panel",
        # ---- 1. Plot Layout Properties ----
        sec_control_section(
          "Plot Layout Properties",
          numericInput(session$ns("s4_w"), "Width", value = 12.5, min = 3, max = 20, step = 0.5, width = "80px"),
          numericInput(session$ns("s4_h"), "Height", value = 8, min = 2, max = 15, step = 0.5, width = "80px"),
          numericInput(session$ns("s4_dpi"), "DPI", value = 300, min = 72, max = 600, step = 10, width = "80px"),
          checkboxInput(session$ns("s4_bg"), "Transparent background", value = FALSE),
          numericInput(session$ns("s4_plot_title"), "Plot title", value = 16, min = 6, max = 30, step = 1, width = "80px"),
          numericInput(session$ns("s4_strip_text"), "LOB text size", value = 12, min = 6, max = 30, step = 1, width = "80px")
        ),
        # ---- 2. Axis Properties ----
        sec_control_section(
          "Axis Properties",
          numericInput(session$ns("s4_axis_text"), "Axis text", value = 12, min = 6, max = 30, step = 1, width = "80px"),
          numericInput(session$ns("s4_axis_title"), "Axis title", value = 14, min = 6, max = 30, step = 1, width = "80px"),
          numericInput(session$ns("s4_axis_angle"), "X angle", value = 20, min = 0, max = 90, step = 5, width = "80px"),
          numericInput(session$ns("s4_axis_text_margin_t"), "X label gap", value = 5, min = 0, max = 50, step = 1, width = "80px"),
          numericInput(session$ns("s4_axis_text_vjust"), "X label vjust", value = 1, min = 0, max = 1, step = 0.05, width = "80px")
        ),
        # ---- 3. Legend & Panel Properties ----
        sec_control_section(
          "Legend & Panel Properties",
          checkboxInput(session$ns("s4_legend_show"), "Show legend", value = TRUE),
          numericInput(session$ns("s4_legend_text"), "Legend text", value = 10, min = 6, max = 30, step = 1, width = "80px"),
          numericInput(session$ns("s4_legend_key_size"), "Legend key", value = 0.8, min = 0.1, max = 3, step = 0.1, width = "80px"),
          numericInput(session$ns("s4_panel_spacing"), "Panel spacing", value = 0.5, min = 0, max = 10, step = 0.5, width = "80px")
        ),
        # ---- 4. Color Properties ----
        sec_control_section(
          "Color Properties",
          div(class = "sec2-swatch-field", colourInput(session$ns("s4_col_pen"), "Penalty", value = "#F0B323", showColour = "both", width = "80px")),
          div(class = "sec2-swatch-field", colourInput(session$ns("s4_col_cred"), "Credit", value = "#6FACDE", showColour = "both", width = "80px"))
        ),
        # ---- 5. Data Label Properties ----
        sec_control_section(
          "Data Label Properties",
          numericInput(session$ns("s4_label_size"), "Label size", value = 3, min = 2, max = 10, step = 0.2, width = "80px"),
          numericInput(session$ns("s4_label_angle"), "Label angle", value = 0, min = 0, max = 90, step = 5, width = "80px"),
          numericInput(session$ns("s4_label_wrap_width"), "Label wrap width", value = 12, min = 5, max = 50, step = 1, width = "80px")
        )
      )
    )
  })
  
  output$s4_plot_frame <- renderUI({
    h_in <- input$s4_h %||% 8
    plotOutput(session$ns("s4_plot"), height = paste0(round(h_in * 96), "px"))
  })
  
  observe({
    bg_val <- if (isTRUE(input$s4_bg)) "transparent" else "white"
    output$s4_plot <- renderPlot({
      credit_penalty_plot_final()
    },
    width = function() (input$s4_w %||% 12.5) * 96,
    height = function() (input$s4_h %||% 8) * 96,
    res = 96,
    bg = bg_val)
  })
  
  # =========================================================================
  # SECTION 4 — STATE SENSITIVITY
  # =========================================================================
  observeEvent(input$sec_state_plots, {
    if (is.null(rv$aalp)) {
      showNotification("Please run 'Analyse Input' first to prepare the required data.", type = "error")
      return()
    }
    assign("mycolors", rv$mycolors, envir = .GlobalEnv)
    
    showNotification("Starting state plot generation...", type = "message", duration = 5)
    
    tryCatch({
      states <- sort(unique(rv$aalp$STATECODE))
      lobs   <- c("SFD", "COM")
      plots <- list()
      
      for (lob in lobs) {
        for (st in states) {
          key <- paste0(sec_sanitize_key(st), "_", lob)
          plots[[key]] <- STATE_plot(rv$aalp, LOB = lob, state_code = st, palette = rv$mycolors)
        }
      }
      rv$state_lob_plots <- plots
      
      # Create download handlers for each plot
      lapply(names(plots), function(k) {
        local({
          key <- k
          output[[paste0("s5_dl_", key)]] <- downloadHandler(
            filename = function() paste0("secmod_s5_", key, ".png"),
            content = function(file) {
              p <- rv$state_lob_plots[[key]]
              def <- gallery_defaults_s5()
              defaults <- list(
                axis_text = def$axis_text %||% 12,
                axis_title = def$axis_title %||% 14,
                plot_title = def$plot_title %||% 16,
                strip_text = def$strip_text %||% 12,
                legend_text = def$legend_text %||% 10,
                axis_angle = def$axis_angle %||% 90,
                legend_show = TRUE,
                col_sfd = def$col_sfd %||% "#6FACDE",
                col_com = def$col_com %||% "#F0B323",
                col_pen = def$col_pen %||% "#F0B323",
                col_cred = def$col_cred %||% "#6FACDE",
                legend_key_size = def$legend_key_size %||% 0.8,
                panel_spacing = def$panel_spacing %||% 0.5,
                bg = def$bg %||% FALSE,
                axis_text_margin_t = def$axis_text_margin_t %||% 5,
                axis_text_vjust = def$axis_text_vjust %||% 1
              )
              p <- apply_plot_overrides(p, input, "s5", key, defaults, is_credit = FALSE)
              w <- (input[[paste0("s5_w_", key)]] %||% def$w) %||% 13
              h <- (input[[paste0("s5_h_", key)]] %||% def$h) %||% 7.5
              dpi <- input$s5_default_dpi %||% 300
              ggsave(file, plot = p, width = w, height = h, dpi = dpi,
                     bg = attr(p, "vulsen_bg") %||% "white", limitsize = FALSE)
            }
          )
        })
      })
      
      showNotification("State plots generated. Please wait a minute for the plots to render on the tab...", type = "message", duration = 6)
      updateTabsetPanel(session, "sec_output_tabs", selected = "State Sensitivity")
    }, error = function(e) {
      showNotification(paste("Generation failed:", conditionMessage(e)), type = "error", duration = NULL)
    })
  })
  
  output$sec_stage5_gallery_controls <- renderUI({
    req(rv$state_lob_plots)
    def <- gallery_defaults_s5()
    sec_gallery_controls_ui(session$ns, "s5", length(rv$state_lob_plots),
                            default_w = def$w %||% 13,
                            default_h = def$h %||% 7.5,
                            default_dpi = 300,
                            default_axis_text = def$axis_text %||% 12,
                            default_axis_title = def$axis_title %||% 14,
                            default_plot_title = def$plot_title %||% 16,
                            default_strip_text = def$strip_text %||% 12,
                            default_legend_text = def$legend_text %||% 10,
                            default_axis_angle = def$axis_angle %||% 90,
                            default_col_sfd = def$col_sfd %||% "#6FACDE",
                            default_col_com = def$col_com %||% "#F0B323",
                            default_col_pen = def$col_pen %||% "#F0B323",
                            default_col_cred = def$col_cred %||% "#6FACDE",
                            default_legend_key_size = def$legend_key_size %||% 0.8,
                            default_panel_spacing = def$panel_spacing %||% 0.5,
                            default_bg = def$bg %||% FALSE,
                            default_axis_text_margin_t = def$axis_text_margin_t %||% 5,
                            default_axis_text_vjust = def$axis_text_vjust %||% 1)
  })
  
  output$sec_stage5_gallery <- renderUI({
    req(rv$state_lob_plots)
    keys <- names(rv$state_lob_plots)
    def <- gallery_defaults_s5()
    defaults <- list(
      w = def$w %||% 13,
      h = def$h %||% 7.5,
      axis_text = def$axis_text %||% 12,
      axis_title = def$axis_title %||% 14,
      plot_title = def$plot_title %||% 16,
      strip_text = def$strip_text %||% 12,
      legend_text = def$legend_text %||% 10,
      axis_angle = def$axis_angle %||% 90,
      legend_key_size = def$legend_key_size %||% 0.8,
      panel_spacing = def$panel_spacing %||% 0.5,
      col_sfd = def$col_sfd %||% "#6FACDE",
      col_com = def$col_com %||% "#F0B323",
      col_pen = def$col_pen %||% "#F0B323",
      col_cred = def$col_cred %||% "#6FACDE",
      axis_text_margin_t = def$axis_text_margin_t %||% 5,
      axis_text_vjust = def$axis_text_vjust %||% 1,
      bg = def$bg %||% FALSE
    )
    tagList(lapply(keys, function(k) {
      sec_plot_card_gallery(
        ns = session$ns,
        key = k,
        label = gsub("_", " ", k),
        prefix = "s5",
        default_w = defaults$w,
        default_h = defaults$h,
        default_axis_text = defaults$axis_text,
        default_axis_title = defaults$axis_title,
        default_plot_title = defaults$plot_title,
        default_strip_text = defaults$strip_text,
        default_legend_text = defaults$legend_text,
        default_axis_angle = defaults$axis_angle,
        default_legend_key_size = defaults$legend_key_size,
        default_panel_spacing = defaults$panel_spacing,
        default_col_sfd = defaults$col_sfd,
        default_col_com = defaults$col_com,
        default_col_pen = defaults$col_pen,
        default_col_cred = defaults$col_cred,
        default_axis_text_margin_t = defaults$axis_text_margin_t,
        default_axis_text_vjust = defaults$axis_text_vjust
      )
    }))
  })
  
  observeEvent(input$s5_apply_all, {
    req(rv$state_lob_plots)
    def <- list(
      w = input$s5_default_w %||% 13,
      h = input$s5_default_h %||% 7.5,
      axis_text = input$s5_default_axis_text %||% 12,
      axis_title = input$s5_default_axis_title %||% 14,
      plot_title = input$s5_default_plot_title %||% 16,
      strip_text = input$s5_default_strip_text %||% 12,
      legend_text = input$s5_default_legend_text %||% 10,
      axis_angle = input$s5_default_axis_angle %||% 90,
      legend_key_size = input$s5_default_legend_key_size %||% 0.8,
      panel_spacing = input$s5_default_panel_spacing %||% 0.5,
      col_sfd = input$s5_default_col_sfd %||% "#6FACDE",
      col_com = input$s5_default_col_com %||% "#F0B323",
      col_pen = input$s5_default_col_pen %||% "#F0B323",
      col_cred = input$s5_default_col_cred %||% "#6FACDE",
      axis_text_margin_t = input$s5_default_axis_text_margin_t %||% 5,
      axis_text_vjust = input$s5_default_axis_text_vjust %||% 1,
      bg = input$s5_default_bg %||% FALSE
    )
    gallery_defaults_s5(def)
    
    for (k in names(rv$state_lob_plots)) {
      updateNumericInput(session, paste0("s5_w_", k), value = def$w)
      updateNumericInput(session, paste0("s5_h_", k), value = def$h)
      updateNumericInput(session, paste0("s5_axis_text_", k), value = def$axis_text)
      updateNumericInput(session, paste0("s5_axis_title_", k), value = def$axis_title)
      updateNumericInput(session, paste0("s5_plot_title_", k), value = def$plot_title)
      updateNumericInput(session, paste0("s5_legend_text_", k), value = def$legend_text)
      updateNumericInput(session, paste0("s5_axis_angle_", k), value = def$axis_angle)
      updateNumericInput(session, paste0("s5_legend_key_size_", k), value = def$legend_key_size)
      updateCheckboxInput(session, paste0("s5_legend_show_", k), value = TRUE)
      updateCheckboxInput(session, paste0("s5_bg_", k), value = isTRUE(def$bg))
      updateColourInput(session, paste0("s5_col_pen_", k), value = def$col_pen)
      updateColourInput(session, paste0("s5_col_cred_", k), value = def$col_cred)
      updateNumericInput(session, paste0("s5_axis_text_margin_t_", k), value = def$axis_text_margin_t)
      updateNumericInput(session, paste0("s5_axis_text_vjust_", k), value = def$axis_text_vjust)
    }
  })
  
  observe({
    req(rv$state_lob_plots)
    keys <- names(rv$state_lob_plots)
    def <- gallery_defaults_s5()
    lapply(keys, function(key) {
      local({
        k <- key
        output[[paste0("s5_plot_frame_", k)]] <- renderUI({
          h_in <- (input[[paste0("s5_h_", k)]] %||% def$h) %||% 7.5
          plotOutput(session$ns(paste0("s5_plot_", k)), height = paste0(round(h_in * 96), "px"))
        })
        output[[paste0("s5_plot_", k)]] <- renderPlot({
          p <- rv$state_lob_plots[[k]]
          defaults <- list(
            axis_text = def$axis_text %||% 12,
            axis_title = def$axis_title %||% 14,
            plot_title = def$plot_title %||% 16,
            strip_text = def$strip_text %||% 12,
            legend_text = def$legend_text %||% 10,
            axis_angle = def$axis_angle %||% 90,
            legend_show = TRUE,
            col_sfd = def$col_sfd %||% "#6FACDE",
            col_com = def$col_com %||% "#F0B323",
            col_pen = def$col_pen %||% "#F0B323",
            col_cred = def$col_cred %||% "#6FACDE",
            legend_key_size = def$legend_key_size %||% 0.8,
            panel_spacing = def$panel_spacing %||% 0.5,
            bg = def$bg %||% FALSE,
            axis_text_margin_t = def$axis_text_margin_t %||% 5,
            axis_text_vjust = def$axis_text_vjust %||% 1
          )
          apply_plot_overrides(p, input, "s5", k, defaults, is_credit = FALSE)
        },
        width = function() ((input[[paste0("s5_w_", k)]] %||% def$w) %||% 13) * 96,
        height = function() ((input[[paste0("s5_h_", k)]] %||% def$h) %||% 7.5) * 96,
        res = 96,
        bg = function() {
          # Same precedence as apply_plot_overrides(): per-plot override first,
          # falling back to the gallery default — so an individually-checked
          # "Transparent background" box is honoured even when the gallery
          # default is still white.
          bg_choice <- input[[paste0("s5_bg_", k)]] %||% def$bg %||% FALSE
          if (is_transparent_bg(bg_choice)) "transparent" else "white"
        })
      })
    })
  })
  
  # =========================================================================
  # SECTION 5 — INDIVIDUAL MODIFIER
  # =========================================================================
  observeEvent(input$sec_modifier_plots, {
    if (is.null(rv$aal_final)) {
      showNotification("Please run 'Analyse Input' first to prepare the required data.", type = "error")
      return()
    }
    assign("mycolors", rv$mycolors, envir = .GlobalEnv)
    
    showNotification("Starting modifier plot generation...", type = "message", duration = 5)
    
    tryCatch({
      pl <- indmod(rv$aal_final, palette = rv$mycolors)
      if (length(pl) == 0) {
        stop("No modifier categories found other than \"Unknown\" — check your data.")
      }
      classif <- names(pl)
      keys <- vapply(classif, sec_sanitize_key, character(1))
      names(pl) <- keys
      
      rv$modifier_plots  <- pl
      rv$modifier_labels <- setNames(classif, keys)
      
      lapply(names(pl), function(k) {
        local({
          key <- k
          output[[paste0("s6_dl_", key)]] <- downloadHandler(
            filename = function() paste0("secmod_s6_", key, ".png"),
            content = function(file) {
              p <- rv$modifier_plots[[key]]
              def <- gallery_defaults_s6()
              defaults <- list(
                axis_text = def$axis_text %||% 12,
                axis_title = def$axis_title %||% 14,
                plot_title = def$plot_title %||% 16,
                strip_text = def$strip_text %||% 12,
                legend_text = def$legend_text %||% 10,
                axis_angle = def$axis_angle %||% 90,
                legend_show = TRUE,
                col_sfd = def$col_sfd %||% "#6FACDE",
                col_com = def$col_com %||% "#F0B323",
                col_pen = def$col_pen %||% "#F0B323",
                col_cred = def$col_cred %||% "#6FACDE",
                legend_key_size = def$legend_key_size %||% 0.8,
                panel_spacing = def$panel_spacing %||% 0.5,
                bg = def$bg %||% FALSE,
                axis_text_margin_t = def$axis_text_margin_t %||% 5,
                axis_text_vjust = def$axis_text_vjust %||% 1
              )
              p <- apply_plot_overrides(p, input, "s6", key, defaults, is_credit = FALSE)
              w <- (input[[paste0("s6_w_", key)]] %||% def$w) %||% 13
              h <- (input[[paste0("s6_h_", key)]] %||% def$h) %||% 9
              dpi <- input$s6_default_dpi %||% 300
              ggsave(file, plot = p, width = w, height = h, dpi = dpi,
                     bg = attr(p, "vulsen_bg") %||% "white", limitsize = FALSE)
            }
          )
        })
      })
      
      showNotification("Modifier plots generated. Please wait a minute for the plots to render on the tab...", type = "message", duration = 6)
      updateTabsetPanel(session, "sec_output_tabs", selected = "Individual Modifier")
    }, error = function(e) {
      showNotification(paste("Generation failed:", conditionMessage(e)), type = "error", duration = NULL)
    })
  })
  
  output$sec_stage6_gallery_controls <- renderUI({
    req(rv$modifier_plots)
    def <- gallery_defaults_s6()
    sec_gallery_controls_ui(session$ns, "s6", length(rv$modifier_plots),
                            default_w = def$w %||% 13,
                            default_h = def$h %||% 9,
                            default_dpi = 300,
                            default_axis_text = def$axis_text %||% 12,
                            default_axis_title = def$axis_title %||% 14,
                            default_plot_title = def$plot_title %||% 16,
                            default_strip_text = def$strip_text %||% 12,
                            default_legend_text = def$legend_text %||% 10,
                            default_axis_angle = def$axis_angle %||% 90,
                            default_col_sfd = def$col_sfd %||% "#6FACDE",
                            default_col_com = def$col_com %||% "#F0B323",
                            default_col_pen = def$col_pen %||% "#F0B323",
                            default_col_cred = def$col_cred %||% "#6FACDE",
                            default_legend_key_size = def$legend_key_size %||% 0.8,
                            default_panel_spacing = def$panel_spacing %||% 0.5,
                            default_bg = def$bg %||% FALSE,
                            default_axis_text_margin_t = def$axis_text_margin_t %||% 5,
                            default_axis_text_vjust = def$axis_text_vjust %||% 1)
  })
  
  output$sec_stage6_gallery <- renderUI({
    req(rv$modifier_plots)
    keys <- names(rv$modifier_plots)
    def <- gallery_defaults_s6()
    defaults <- list(
      w = def$w %||% 13,
      h = def$h %||% 9,
      axis_text = def$axis_text %||% 12,
      axis_title = def$axis_title %||% 14,
      plot_title = def$plot_title %||% 16,
      strip_text = def$strip_text %||% 12,
      legend_text = def$legend_text %||% 10,
      axis_angle = def$axis_angle %||% 90,
      legend_key_size = def$legend_key_size %||% 0.8,
      panel_spacing = def$panel_spacing %||% 0.5,
      col_sfd = def$col_sfd %||% "#6FACDE",
      col_com = def$col_com %||% "#F0B323",
      col_pen = def$col_pen %||% "#F0B323",
      col_cred = def$col_cred %||% "#6FACDE",
      axis_text_margin_t = def$axis_text_margin_t %||% 5,
      axis_text_vjust = def$axis_text_vjust %||% 1,
      bg = def$bg %||% FALSE
    )
    tagList(lapply(keys, function(k) {
      sec_plot_card_gallery(
        ns = session$ns,
        key = k,
        label = rv$modifier_labels[[k]],
        prefix = "s6",
        default_w = defaults$w,
        default_h = defaults$h,
        default_axis_text = defaults$axis_text,
        default_axis_title = defaults$axis_title,
        default_plot_title = defaults$plot_title,
        default_strip_text = defaults$strip_text,
        default_legend_text = defaults$legend_text,
        default_axis_angle = defaults$axis_angle,
        default_legend_key_size = defaults$legend_key_size,
        default_panel_spacing = defaults$panel_spacing,
        default_col_sfd = defaults$col_sfd,
        default_col_com = defaults$col_com,
        default_col_pen = defaults$col_pen,
        default_col_cred = defaults$col_cred,
        default_axis_text_margin_t = defaults$axis_text_margin_t,
        default_axis_text_vjust = defaults$axis_text_vjust
      )
    }))
  })
  
  observeEvent(input$s6_apply_all, {
    req(rv$modifier_plots)
    def <- list(
      w = input$s6_default_w %||% 13,
      h = input$s6_default_h %||% 9,
      axis_text = input$s6_default_axis_text %||% 12,
      axis_title = input$s6_default_axis_title %||% 14,
      plot_title = input$s6_default_plot_title %||% 16,
      strip_text = input$s6_default_strip_text %||% 12,
      legend_text = input$s6_default_legend_text %||% 10,
      axis_angle = input$s6_default_axis_angle %||% 90,
      legend_key_size = input$s6_default_legend_key_size %||% 0.8,
      panel_spacing = input$s6_default_panel_spacing %||% 0.5,
      col_sfd = input$s6_default_col_sfd %||% "#6FACDE",
      col_com = input$s6_default_col_com %||% "#F0B323",
      col_pen = input$s6_default_col_pen %||% "#F0B323",
      col_cred = input$s6_default_col_cred %||% "#6FACDE",
      axis_text_margin_t = input$s6_default_axis_text_margin_t %||% 5,
      axis_text_vjust = input$s6_default_axis_text_vjust %||% 1,
      bg = input$s6_default_bg %||% FALSE
    )
    gallery_defaults_s6(def)
    
    for (k in names(rv$modifier_plots)) {
      updateNumericInput(session, paste0("s6_w_", k), value = def$w)
      updateNumericInput(session, paste0("s6_h_", k), value = def$h)
      updateNumericInput(session, paste0("s6_axis_text_", k), value = def$axis_text)
      updateNumericInput(session, paste0("s6_axis_title_", k), value = def$axis_title)
      updateNumericInput(session, paste0("s6_plot_title_", k), value = def$plot_title)
      updateNumericInput(session, paste0("s6_strip_text_", k), value = def$strip_text)
      updateNumericInput(session, paste0("s6_legend_text_", k), value = def$legend_text)
      updateNumericInput(session, paste0("s6_axis_angle_", k), value = def$axis_angle)
      updateNumericInput(session, paste0("s6_legend_key_size_", k), value = def$legend_key_size)
      updateNumericInput(session, paste0("s6_panel_spacing_", k), value = def$panel_spacing)
      updateCheckboxInput(session, paste0("s6_legend_show_", k), value = TRUE)
      updateCheckboxInput(session, paste0("s6_bg_", k), value = isTRUE(def$bg))
      updateColourInput(session, paste0("s6_col_sfd_", k), value = def$col_sfd)
      updateColourInput(session, paste0("s6_col_com_", k), value = def$col_com)
      updateNumericInput(session, paste0("s6_axis_text_margin_t_", k), value = def$axis_text_margin_t)
      updateNumericInput(session, paste0("s6_axis_text_vjust_", k), value = def$axis_text_vjust)
    }
  })
  
  observe({
    req(rv$modifier_plots)
    keys <- names(rv$modifier_plots)
    def <- gallery_defaults_s6()
    lapply(keys, function(key) {
      local({
        k <- key
        output[[paste0("s6_plot_frame_", k)]] <- renderUI({
          h_in <- (input[[paste0("s6_h_", k)]] %||% def$h) %||% 9
          plotOutput(session$ns(paste0("s6_plot_", k)), height = paste0(round(h_in * 96), "px"))
        })
        output[[paste0("s6_plot_", k)]] <- renderPlot({
          p <- rv$modifier_plots[[k]]
          defaults <- list(
            axis_text = def$axis_text %||% 12,
            axis_title = def$axis_title %||% 14,
            plot_title = def$plot_title %||% 16,
            strip_text = def$strip_text %||% 12,
            legend_text = def$legend_text %||% 10,
            axis_angle = def$axis_angle %||% 90,
            legend_show = TRUE,
            col_sfd = def$col_sfd %||% "#6FACDE",
            col_com = def$col_com %||% "#F0B323",
            col_pen = def$col_pen %||% "#F0B323",
            col_cred = def$col_cred %||% "#6FACDE",
            legend_key_size = def$legend_key_size %||% 0.8,
            panel_spacing = def$panel_spacing %||% 0.5,
            bg = def$bg %||% FALSE,
            axis_text_margin_t = def$axis_text_margin_t %||% 5,
            axis_text_vjust = def$axis_text_vjust %||% 1
          )
          apply_plot_overrides(p, input, "s6", k, defaults, is_credit = FALSE)
        },
        width = function() ((input[[paste0("s6_w_", k)]] %||% def$w) %||% 13) * 96,
        height = function() ((input[[paste0("s6_h_", k)]] %||% def$h) %||% 9) * 96,
        res = 96,
        bg = function() {
          bg_choice <- input[[paste0("s6_bg_", k)]] %||% def$bg %||% FALSE
          if (is_transparent_bg(bg_choice)) "transparent" else "white"
        })
      })
    })
  })
  
  # =========================================================================
  # SECTION 6 — REPORT
  # =========================================================================
  
  # ---- Observer to lock/unlock report buttons ----
  observe({
    stage456_done <- !is.null(rv$credit_penalty_plot) &&
      !is.null(rv$state_lob_plots) &&
      !is.null(rv$modifier_plots)
    
    if (isTRUE(stage456_done)) {
      shinyjs::enable(session$ns("sec_generate_customized"))
      shinyjs::removeClass(id = session$ns("sec_generate_customized"), class = "state-locked")
    } else {
      shinyjs::disable(session$ns("sec_generate_customized"))
      shinyjs::addClass(id = session$ns("sec_generate_customized"), class = "state-locked")
    }
    
    dl_state <- if (isTRUE(rv$customized_report_generated)) {
      "state-active"
    } else if (isTRUE(stage456_done)) {
      "state-ready"
    } else {
      "state-locked"
    }
    
    if (isTRUE(rv$customized_report_generated)) {
      shinyjs::enable(session$ns("sec_dl_customized"))
    } else {
      shinyjs::disable(session$ns("sec_dl_customized"))
    }
    
    session$sendCustomMessage("secmod-dl-state", list(
      id = session$ns("sec_dl_customized"),
      state = dl_state
    ))
  })
  
  # ---- Generate Report with lock, confirmation, and persistent notification ----
  observeEvent(input$sec_generate_customized, {
    # Check if already running
    if (report_running()) {
      showModal(modalDialog(
        title = "Report generation in progress",
        "Report generation is already running. Please wait for it to complete.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    # Check if prerequisites are met
    if (is.null(rv$credit_penalty_plot) || is.null(rv$state_lob_plots) || is.null(rv$modifier_plots)) {
      showNotification("Please complete sections 3, 4, and 5 first.", type = "error")
      return()
    }
    
    # If report has been generated once before, ask for confirmation
    if (report_generated_once()) {
      showModal(modalDialog(
        title = "Regenerate Report?",
        "Report has been generated previously. Are you sure you want to generate it once again?",
        footer = tagList(
          modalButton("No"),
          actionButton(session$ns("confirm_regenerate"), "Yes, regenerate", class = "btn-primary")
        ),
        easyClose = FALSE
      ))
      return()
    }
    
    # Proceed directly if first time
    generate_report()
  })
  
  # ---- Confirmation handler ----
  observeEvent(input$confirm_regenerate, {
    removeModal()
    generate_report()
  })
  
  # ---- Core report generation function ----
  generate_report <- function() {
    # Set lock
    report_running(TRUE)
    shinyjs::disable(session$ns("sec_generate_customized"))
    
    # Persistent notification
    notif_id <- showNotification(
      "Generating report... Please wait. This may take 1-3 minutes.",
      type = "message",
      duration = NULL,
      closeButton = FALSE
    )
    
    tryCatch({
      # ---- Build bundles ----
      credit_bundle <- list(
        plot   = credit_penalty_plot_final(),
        width  = input$s4_w %||% 12.5,
        height = input$s4_h %||% 8,
        dpi    = input$s4_dpi %||% 300,
        bg     = if (isTRUE(input$s4_bg)) "transparent" else "white",
        label  = "Credit / Penalty"
      )
      
      def5 <- gallery_defaults_s5()
      state_bundles <- list()
      for (k in names(rv$state_lob_plots)) {
        p <- rv$state_lob_plots[[k]]
        defaults <- list(
          axis_text = def5$axis_text %||% 12,
          axis_title = def5$axis_title %||% 14,
          plot_title = def5$plot_title %||% 16,
          strip_text = def5$strip_text %||% 12,
          legend_text = def5$legend_text %||% 10,
          axis_angle = def5$axis_angle %||% 90,
          legend_show = TRUE,
          col_sfd = def5$col_sfd %||% "#6FACDE",
          col_com = def5$col_com %||% "#F0B323",
          col_pen = def5$col_pen %||% "#F0B323",
          col_cred = def5$col_cred %||% "#6FACDE",
          legend_key_size = def5$legend_key_size %||% 0.8,
          panel_spacing = def5$panel_spacing %||% 0.5,
          bg = def5$bg %||% FALSE,
          axis_text_margin_t = def5$axis_text_margin_t %||% 5,
          axis_text_vjust = def5$axis_text_vjust %||% 1
        )
        styled_p <- apply_plot_overrides(p, input, "s5", k, defaults, is_credit = FALSE)
        w <- (input[[paste0("s5_w_", k)]] %||% def5$w) %||% 13
        h <- (input[[paste0("s5_h_", k)]] %||% def5$h) %||% 7.5
        dpi <- input$s5_default_dpi %||% 300
        bg <- attr(styled_p, "vulsen_bg") %||% "white"
        state_bundles[[k]] <- list(
          plot   = styled_p,
          width  = w,
          height = h,
          dpi    = dpi,
          bg     = bg,
          label  = gsub("_", " ", k)
        )
      }
      
      def6 <- gallery_defaults_s6()
      modifier_bundles <- list()
      for (k in names(rv$modifier_plots)) {
        p <- rv$modifier_plots[[k]]
        defaults <- list(
          axis_text = def6$axis_text %||% 12,
          axis_title = def6$axis_title %||% 14,
          plot_title = def6$plot_title %||% 16,
          strip_text = def6$strip_text %||% 12,
          legend_text = def6$legend_text %||% 10,
          axis_angle = def6$axis_angle %||% 90,
          legend_show = TRUE,
          col_sfd = def6$col_sfd %||% "#6FACDE",
          col_com = def6$col_com %||% "#F0B323",
          col_pen = def6$col_pen %||% "#F0B323",
          col_cred = def6$col_cred %||% "#6FACDE",
          legend_key_size = def6$legend_key_size %||% 0.8,
          panel_spacing = def6$panel_spacing %||% 0.5,
          bg = def6$bg %||% FALSE,
          axis_text_margin_t = def6$axis_text_margin_t %||% 5,
          axis_text_vjust = def6$axis_text_vjust %||% 1
        )
        styled_p <- apply_plot_overrides(p, input, "s6", k, defaults, is_credit = FALSE)
        w <- (input[[paste0("s6_w_", k)]] %||% def6$w) %||% 13
        h <- (input[[paste0("s6_h_", k)]] %||% def6$h) %||% 9
        dpi <- input$s6_default_dpi %||% 300
        bg <- attr(styled_p, "vulsen_bg") %||% "white"
        modifier_bundles[[k]] <- list(
          plot   = styled_p,
          width  = w,
          height = h,
          dpi    = dpi,
          bg     = bg,
          label  = rv$modifier_labels[[k]] %||% gsub("_", " ", k)
        )
      }
      
      tmp_report <- tempfile(fileext = ".html")
      template_path <- "R/secmod_customized_template.Rmd"
      if (!file.exists(template_path)) {
        stop("Customized template not found: secmod_customized_template.Rmd")
      }
      
      rmarkdown::render(
        input = template_path,
        output_file = tmp_report,
        params = list(
          credit_penalty   = credit_bundle,
          state_lob_plots  = state_bundles,
          modifier_plots   = modifier_bundles,
          table_minmax_USA = rv$table_minmax_USA
        ),
        envir = new.env(),
        knit_root_dir = getwd(),
        quiet = TRUE
      )
      
      rv$customized_report_file <- tmp_report
      rv$customized_report_generated <- TRUE
      report_generated_once(TRUE)
      
      # Remove persistent notification and show success
      removeNotification(notif_id)
      showNotification("Report generated successfully. Download is ready.", type = "message", duration = 5)
      
    }, error = function(e) {
      removeNotification(notif_id)
      showNotification(paste("Failed to generate report:", e$message), type = "error", duration = NULL)
    }, finally = {
      # Release lock and re-enable button
      report_running(FALSE)
      shinyjs::enable(session$ns("sec_generate_customized"))
    })
  }
  
  # ---- Download Report with dynamic filename ----
  output$sec_dl_customized <- downloadHandler(
    filename = function() {
      model <- input$sec_vendor %||% "Model"
      country <- input$sec_country %||% "US"
      peril <- input$sec_peril %||% "Peril"
      subperil <- input$sec_subperil %||% "Sub"
      suffix <- input$sec_suffix %||% "2026"
      date <- format(Sys.Date(), "%Y%m%d")
      paste0("SecMod_", model, "_", country, "_", peril, "_", subperil, "_", suffix, "_", date, ".html")
    },
    content = function(file) {
      req(rv$customized_report_file, file.exists(rv$customized_report_file))
      file.copy(rv$customized_report_file, file)
    }
  )
  
  # =========================================================================
  # ADD TO CART — shared handler (FIXED: use 'key' in s6 branch)
  # =========================================================================
  observeEvent(input$sec_cart_click, {
    key_full <- input$sec_cart_click$key
    parts    <- strsplit(key_full, "\\|")[[1]]
    prefix   <- parts[1]
    key      <- parts[2]
    
    plot_obj <- switch(prefix,
                       "s4" = {
                         credit_penalty_plot_final()
                       },
                       "s5" = {
                         p <- rv$state_lob_plots[[key]]
                         def <- gallery_defaults_s5()
                         defaults <- list(
                           axis_text = def$axis_text %||% 12,
                           axis_title = def$axis_title %||% 14,
                           plot_title = def$plot_title %||% 16,
                           strip_text = def$strip_text %||% 12,
                           legend_text = def$legend_text %||% 10,
                           axis_angle = def$axis_angle %||% 90,
                           legend_show = TRUE,
                           col_sfd = def$col_sfd %||% "#6FACDE",
                           col_com = def$col_com %||% "#F0B323",
                           col_pen = def$col_pen %||% "#F0B323",
                           col_cred = def$col_cred %||% "#6FACDE",
                           legend_key_size = def$legend_key_size %||% 0.8,
                           panel_spacing = def$panel_spacing %||% 0.5,
                           bg = def$bg %||% FALSE,
                           axis_text_margin_t = def$axis_text_margin_t %||% 5,
                           axis_text_vjust = def$axis_text_vjust %||% 1
                         )
                         apply_plot_overrides(p, input, "s5", key, defaults, is_credit = FALSE)
                       },
                       "s6" = {
                         p <- rv$modifier_plots[[key]]
                         def <- gallery_defaults_s6()
                         defaults <- list(
                           axis_text = def$axis_text %||% 12,
                           axis_title = def$axis_title %||% 14,
                           plot_title = def$plot_title %||% 16,
                           strip_text = def$strip_text %||% 12,
                           legend_text = def$legend_text %||% 10,
                           axis_angle = def$axis_angle %||% 90,
                           legend_show = TRUE,
                           col_sfd = def$col_sfd %||% "#6FACDE",
                           col_com = def$col_com %||% "#F0B323",
                           col_pen = def$col_pen %||% "#F0B323",
                           col_cred = def$col_cred %||% "#6FACDE",
                           legend_key_size = def$legend_key_size %||% 0.8,
                           panel_spacing = def$panel_spacing %||% 0.5,
                           bg = def$bg %||% FALSE,
                           axis_text_margin_t = def$axis_text_margin_t %||% 5,
                           axis_text_vjust = def$axis_text_vjust %||% 1
                         )
                         apply_plot_overrides(p, input, "s6", key, defaults, is_credit = FALSE)
                       }
    )
    req(plot_obj)
    
    # determine dimensions based on prefix
    if (prefix == "s4") {
      w <- input$s4_w %||% 12.5
      h <- input$s4_h %||% 8
      dpi <- input$s4_dpi %||% 300
    } else if (prefix == "s5") {
      def <- gallery_defaults_s5()
      w <- (input[[paste0("s5_w_", key)]] %||% def$w) %||% 13
      h <- (input[[paste0("s5_h_", key)]] %||% def$h) %||% 7.5
      dpi <- input$s5_default_dpi %||% 300
    } else if (prefix == "s6") {
      def <- gallery_defaults_s6()
      w <- (input[[paste0("s6_w_", key)]] %||% def$w) %||% 13
      h <- (input[[paste0("s6_h_", key)]] %||% def$h) %||% 9
      dpi <- input$s6_default_dpi %||% 300
    } else {
      w <- 9; h <- 5; dpi <- 300
    }
    
    item <- list(
      id         = generate_item_id(),
      module     = "Secondary Modifier",
      plot       = plot_obj,
      commentary = NULL,
      timestamp  = Sys.time(),
      width      = w,
      height     = h,
      dpi        = dpi
    )
    
    current_cart <- cart()
    current_cart[[length(current_cart) + 1]] <- item
    cart(current_cart)
    
    session$sendCustomMessage("show-toast", list(
      text = "Added to cart", type = "success", icon = "fa-cart-plus", duration = 2000
    ))
  })
  
  # =========================================================================
  # LOAD DEMO DATA
  # =========================================================================
  observeEvent(input$sec_load_demo, {
    tryCatch({
      # Read sample files from data/secmod_sample/
      state_path <- "data/secmod_sample/AllPerils_secmod_average_state_HDv1.csv"
      usa_path <- "data/secmod_sample/AllPerils_secmod_average_USA_HDv1.csv"
      mapping_path <- "data/secmod_sample/modifer,name.csv"
      
      if (!file.exists(state_path) || !file.exists(usa_path) || !file.exists(mapping_path)) {
        stop("Sample files not found in data/secmod_sample/. Please check the directory.")
      }
      
      state_df <- read.csv(state_path, stringsAsFactors = FALSE)
      country_df <- read.csv(usa_path, stringsAsFactors = FALSE)
      mapping_df <- read.csv(mapping_path, stringsAsFactors = FALSE)
      
      # Validate columns
      validate_columns(state_df, c("STATECODE", "Classification", "Description", "AAL"), "State AAL")
      validate_columns(country_df, c("Classification", "Description", "AAL"), "Country AAL")
      validate_columns(mapping_df, c("modifer", "name"), "SecMod mapping")
      
      # Store in reactive values
      rv$aal_State <- state_df
      rv$aal_USA   <- country_df
      rv$SecMod_name <- mapping_df
      
      # ---- Set colours from defaults (no longer from UI) ----
      rv$type_colors <- c(Max = "#F0B323", Min = "#6FACDE")
      rv$mycolors    <- c("#6FACDE", "#F0B323")
      assign("mycolors", rv$mycolors, envir = .GlobalEnv)
      
      # Build tables
      rv$aal_final     <- finaltable(rv$aal_State, rv$SecMod_name)
      rv$aal_final_USA <- finaltable_allUSA(rv$aal_USA, rv$SecMod_name)
      
      # Compute min/max
      rv$aalp            <- STATEminmax(rv$aal_final, rv$SecMod_name)
      rv$aalp_USA        <- Countryminmax(rv$aal_final_USA, rv$SecMod_name)
      rv$table_minmax_USA <- CountryminmaxTable(rv$aal_final_USA, rv$SecMod_name)
      
      showNotification("Demo data loaded and analysis completed successfully. Min/Max table is now available.",
                       type = "message", duration = 5)
      
      updateTabsetPanel(session, "sec_output_tabs", selected = "Min / max Table")
      
    }, error = function(e) {
      showNotification(paste("Loading demo data failed:", conditionMessage(e)),
                       type = "error", duration = NULL)
    })
  })
  
  # =========================================================================
  # ADD ALL PLOTS TO CART (s5 and s6)
  # =========================================================================
  observeEvent(input$s5_add_all_cart, {
    req(rv$state_lob_plots)
    count <- length(rv$state_lob_plots)
    if (count == 0) return()
    
    showModal(
      modalDialog(
        title = "Add all plots to cart?",
        paste0("This will add all ", count, " state plots to your cart. Are you sure?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("confirm_s5_add_all_cart"), "Yes, add all", class = "sec2-btn")
        ),
        easyClose = FALSE
      )
    )
  })
  
  observeEvent(input$confirm_s5_add_all_cart, {
    removeModal()
    req(rv$state_lob_plots)
    def <- gallery_defaults_s5()
    plots <- rv$state_lob_plots
    count <- length(plots)
    if (count == 0) return()
    
    added <- 0
    for (k in names(plots)) {
      p <- plots[[k]]
      defaults <- list(
        axis_text = def$axis_text %||% 12,
        axis_title = def$axis_title %||% 14,
        plot_title = def$plot_title %||% 16,
        strip_text = def$strip_text %||% 12,
        legend_text = def$legend_text %||% 10,
        axis_angle = def$axis_angle %||% 90,
        legend_show = TRUE,
        col_sfd = def$col_sfd %||% "#6FACDE",
        col_com = def$col_com %||% "#F0B323",
        col_pen = def$col_pen %||% "#F0B323",
        col_cred = def$col_cred %||% "#6FACDE",
        legend_key_size = def$legend_key_size %||% 0.8,
        panel_spacing = def$panel_spacing %||% 0.5,
        bg = def$bg %||% FALSE,
        axis_text_margin_t = def$axis_text_margin_t %||% 5,
        axis_text_vjust = def$axis_text_vjust %||% 1
      )
      styled_p <- apply_plot_overrides(p, input, "s5", k, defaults, is_credit = FALSE)
      w <- (input[[paste0("s5_w_", k)]] %||% def$w) %||% 13
      h <- (input[[paste0("s5_h_", k)]] %||% def$h) %||% 7.5
      dpi <- input$s5_default_dpi %||% 300
      
      item <- list(
        id         = generate_item_id(),
        module     = "Secondary Modifier",
        plot       = styled_p,
        commentary = NULL,
        timestamp  = Sys.time(),
        width      = w,
        height     = h,
        dpi        = dpi
      )
      current_cart <- cart()
      current_cart[[length(current_cart) + 1]] <- item
      cart(current_cart)
      added <- added + 1
    }
    
    session$sendCustomMessage("show-toast", list(
      text = paste("Added", added, "state plots to cart"),
      type = "success", icon = "fa-cart-plus", duration = 2500
    ))
  })
  
  observeEvent(input$s6_add_all_cart, {
    req(rv$modifier_plots)
    count <- length(rv$modifier_plots)
    if (count == 0) return()
    
    showModal(
      modalDialog(
        title = "Add all plots to cart?",
        paste0("This will add all ", count, " modifier plots to your cart. Are you sure?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("confirm_s6_add_all_cart"), "Yes, add all", class = "sec2-btn")
        ),
        easyClose = FALSE
      )
    )
  })
  
  observeEvent(input$confirm_s6_add_all_cart, {
    removeModal()
    req(rv$modifier_plots)
    def <- gallery_defaults_s6()
    plots <- rv$modifier_plots
    count <- length(plots)
    if (count == 0) return()
    
    added <- 0
    for (k in names(plots)) {
      p <- plots[[k]]
      defaults <- list(
        axis_text = def$axis_text %||% 12,
        axis_title = def$axis_title %||% 14,
        plot_title = def$plot_title %||% 16,
        strip_text = def$strip_text %||% 12,
        legend_text = def$legend_text %||% 10,
        axis_angle = def$axis_angle %||% 90,
        legend_show = TRUE,
        col_sfd = def$col_sfd %||% "#6FACDE",
        col_com = def$col_com %||% "#F0B323",
        col_pen = def$col_pen %||% "#F0B323",
        col_cred = def$col_cred %||% "#6FACDE",
        legend_key_size = def$legend_key_size %||% 0.8,
        panel_spacing = def$panel_spacing %||% 0.5,
        bg = def$bg %||% FALSE,
        axis_text_margin_t = def$axis_text_margin_t %||% 5,
        axis_text_vjust = def$axis_text_vjust %||% 1
      )
      styled_p <- apply_plot_overrides(p, input, "s6", k, defaults, is_credit = FALSE)
      w <- (input[[paste0("s6_w_", k)]] %||% def$w) %||% 13
      h <- (input[[paste0("s6_h_", k)]] %||% def$h) %||% 9
      dpi <- input$s6_default_dpi %||% 300
      
      item <- list(
        id         = generate_item_id(),
        module     = "Secondary Modifier",
        plot       = styled_p,
        commentary = NULL,
        timestamp  = Sys.time(),
        width      = w,
        height     = h,
        dpi        = dpi
      )
      current_cart <- cart()
      current_cart[[length(current_cart) + 1]] <- item
      cart(current_cart)
      added <- added + 1
    }
    
    session$sendCustomMessage("show-toast", list(
      text = paste("Added", added, "modifier plots to cart"),
      type = "success", icon = "fa-cart-plus", duration = 2500
    ))
  })
  
  # ---- Pipeline progress stepper (right panel) ----
  output$sec_progress_panel <- renderUI({
    # Use isTruthy for safety
    stages <- list(
      list(id = "sec-settings-card", label = "Settings", done = {
        isTruthy(input$sec_vendor) &&
          isTruthy(input$sec_country) &&
          isTruthy(input$sec_peril) &&
          isTruthy(input$sec_subperil) &&
          isTruthy(input$sec_suffix)
      }),
      list(id = "sec-inputs-card", label = "Inputs", done = inputs_ready()),
      list(id = "sec-credit-card", label = "Credit/Penalty", done = !is.null(rv$credit_penalty_plot)),
      list(id = "sec-state-card", label = "State Sensitivity", done = !is.null(rv$state_lob_plots)),
      list(id = "sec-modifier-card", label = "Individual Modifier", done = !is.null(rv$modifier_plots)),
      list(id = "sec-report-card", label = "Report", done = isTRUE(rv$customized_report_generated))
    )
    
    div(
      class = "sec-progress-panel",
      style = "margin: 16px 0 12px 0; padding: 12px 16px; background: #f8faff; border-radius: 16px; border: 1px solid #e2e8f0;",
      div(
        style = "display: flex; align-items: center; justify-content: space-between; gap: 4px;",
        lapply(seq_along(stages), function(i) {
          st <- stages[[i]]
          is_done <- st$done
          circle <- if (is_done) {
            tags$span(class = "sec-step-circle done", icon("check"))
          } else {
            tags$span(class = "sec-step-circle pending", i)
          }
          label <- tags$div(class = "sec-step-label", st$label)
          connector <- if (i < length(stages)) {
            tags$div(
              class = paste("sec-connector", if (is_done && stages[[i+1]]$done) "done" else "pending")
            )
          } else NULL
          tags$div(
            class = "sec-step-clickable",
            style = "display: flex; align-items: center; flex: 1;",
            onclick = sprintf("scrollToSection('%s')", st$id),
            tags$div(
              style = "display: flex; flex-direction: column; align-items: center;",
              circle,
              label
            ),
            connector
          )
        })
      )
    )
  })
  
} # end of secmod_server

