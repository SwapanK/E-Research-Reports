library(shiny)
library(shinyjs)
library(ggplot2)
library(colourpicker)

# =============================================================================
# SECONDARY MODIFIER SERVER — UPDATED FOR 6-SECTION UI
# =============================================================================

# ---- helpers -----------------------------------------------------------------
is_transparent_bg <- function(x) {
  !is.null(x) && tolower(trimws(as.character(x))) %in% c("transparent", "na", "none")
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
  axis_text <- (input[[paste0(prefix, "_axis_text_", key)]] %||% defaults$axis_text) %||% 12
  axis_title <- (input[[paste0(prefix, "_axis_title_", key)]] %||% defaults$axis_title) %||% 14
  plot_title <- (input[[paste0(prefix, "_plot_title_", key)]] %||% defaults$plot_title) %||% 16
  strip_text <- (input[[paste0(prefix, "_strip_text_", key)]] %||% defaults$strip_text) %||% 12
  legend_text <- (input[[paste0(prefix, "_legend_text_", key)]] %||% defaults$legend_text) %||% 10
  legend_title <- (input[[paste0(prefix, "_legend_title_", key)]] %||% defaults$legend_title) %||% 10
  axis_angle <- (input[[paste0(prefix, "_axis_angle_", key)]] %||% defaults$axis_angle) %||% 90
  legend_pos <- (input[[paste0(prefix, "_legend_pos_", key)]] %||% defaults$legend_pos) %||% "top"
  show_legend <- input[[paste0(prefix, "_legend_show_", key)]]
  if (is.null(show_legend)) show_legend <- defaults$legend_show %||% TRUE
  legend_key_size <- (input[[paste0(prefix, "_legend_key_size_", key)]] %||% defaults$legend_key_size) %||% 0.8
  title_hjust <- (input[[paste0(prefix, "_plot_title_hjust_", key)]] %||% defaults$title_hjust) %||% 0.5
  axis_line_col <- input[[paste0(prefix, "_axis_line_col_", key)]] %||% defaults$axis_line_col %||% "black"
  panel_fill <- input[[paste0(prefix, "_panel_fill_", key)]] %||% defaults$panel_fill %||% "white"
  bg_choice <- input[[paste0(prefix, "_default_bg")]] %||% defaults$bg %||% "white"
  grid_col <- input[[paste0(prefix, "_grid_col_", key)]] %||% defaults$grid_col %||% "#e9ecf3"
  panel_spacing <- (input[[paste0(prefix, "_panel_spacing_", key)]] %||% defaults$panel_spacing) %||% 3
  margin_t <- (input[[paste0(prefix, "_plot_margin_t_", key)]] %||% defaults$margin_t) %||% 30
  margin_r <- (input[[paste0(prefix, "_plot_margin_r_", key)]] %||% defaults$margin_r) %||% 10
  margin_b <- (input[[paste0(prefix, "_plot_margin_b_", key)]] %||% defaults$margin_b) %||% 30
  margin_l <- (input[[paste0(prefix, "_plot_margin_l_", key)]] %||% defaults$margin_l) %||% 10
  border_col <- input[[paste0(prefix, "_panel_border_col_", key)]] %||% defaults$border_col %||% "black"
  border_lwd <- (input[[paste0(prefix, "_panel_border_lwd_", key)]] %||% defaults$border_lwd) %||% 0.5
  
  col_sfd <- input[[paste0(prefix, "_col_sfd_", key)]] %||% defaults$col_sfd %||% "#6FACDE"
  col_com <- input[[paste0(prefix, "_col_com_", key)]] %||% defaults$col_com %||% "#F0B323"
  col_pen <- input[[paste0(prefix, "_col_pen_", key)]] %||% defaults$col_pen %||% "#F0B323"
  col_cred <- input[[paste0(prefix, "_col_cred_", key)]] %||% defaults$col_cred %||% "#6FACDE"
  
  is_transp <- is_transparent_bg(panel_fill) || is_transparent_bg(bg_choice)
  
  n_panels <- count_facet_panels(p)
  dense_scale <- if (n_panels > 12) max(0.18, sqrt(12 / n_panels)) else 1
  
  panel_spacing <- panel_spacing * dense_scale
  margin_t <- margin_t * dense_scale
  margin_r <- margin_r * dense_scale
  margin_b <- margin_b * dense_scale
  margin_l <- margin_l * dense_scale
  axis_text <- max(6, axis_text * max(dense_scale, 0.65))
  strip_text <- max(6, strip_text * max(dense_scale, 0.65))
  
  p <- p + theme(
    axis.text.x = element_text(size = axis_text, angle = axis_angle, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(size = axis_text),
    axis.title.x = element_text(size = axis_title),
    axis.title.y = element_text(size = axis_title),
    plot.title = element_text(size = plot_title, hjust = title_hjust),
    strip.text = element_text(size = strip_text),
    strip.background = element_blank(),
    legend.text = element_text(size = legend_text),
    legend.title = element_text(size = legend_title),
    legend.position = if (show_legend) legend_pos else "none",
    legend.key.size = unit(legend_key_size, "cm"),
    axis.line = element_line(colour = axis_line_col),
    axis.ticks = element_line(colour = axis_line_col),
    panel.background  = element_rect(fill = if (is_transp) NA else panel_fill, colour = NA),
    plot.background   = element_rect(fill = if (is_transp) NA else "white", colour = NA),
    legend.background = element_rect(fill = if (is_transp) NA else "white", colour = NA),
    legend.box.background = element_blank(),
    legend.key        = element_rect(fill = if (is_transp) NA else "white", colour = NA),
    panel.grid.major = element_line(colour = grid_col),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(panel_spacing, "lines"),
    plot.margin = margin(t = margin_t, r = margin_r, b = margin_b, l = margin_l),
    panel.border = element_rect(colour = border_col, fill = NA, linewidth = border_lwd)
  )
  
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
  
  
  # server/secmod_server.R
  
  inputs_ready <- reactive({
    !is.null(rv$aal_State) && !is.null(rv$aal_USA) && !is.null(rv$SecMod_name)
  })
  
  gallery_defaults_s5 <- reactiveVal(
    list(
      w = 13, h = 7.5,
      axis_text = 12, axis_title = 14, plot_title = 16, strip_text = 12,
      legend_text = 10, legend_title = 10, axis_angle = 90,
      legend_pos = "top", legend_key_size = 0.8, title_hjust = 0.5,
      panel_spacing = 3, margin_t = 30, margin_r = 10, margin_b = 30, margin_l = 10,
      border_lwd = 0.5,
      axis_line_col = "black", panel_fill = "white", bg = "white", grid_col = "#e9ecf3",
      border_col = "black",
      col_sfd = "#6FACDE", col_com = "#F0B323", col_pen = "#F0B323", col_cred = "#6FACDE"
    )
  )
  
  gallery_defaults_s6 <- reactiveVal(
    list(
      w = 13, h = 9,
      axis_text = 12, axis_title = 14, plot_title = 16, strip_text = 12,
      legend_text = 10, legend_title = 10, axis_angle = 90,
      legend_pos = "top", legend_key_size = 0.8, title_hjust = 0.5,
      panel_spacing = 0, margin_t = 30, margin_r = 10, margin_b = 30, margin_l = 10,
      border_lwd = 0.5,
      axis_line_col = "black", panel_fill = "white", bg = "white", grid_col = "#e9ecf3",
      border_col = "black",
      col_sfd = "#6FACDE", col_com = "#F0B323", col_pen = "#F0B323", col_cred = "#6FACDE"
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
      
      # Set colours from UI
      rv$type_colors <- c(Max = input$sec_color_max, Min = input$sec_color_min)
      rv$mycolors    <- c(input$sec_color_sfd, input$sec_color_com)
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
  
  # ---- Min/max table (renderTable for reliability, like old working code) ----
  # server/secmod_server.R (replace the existing block)
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
    
    p <- p + theme(
      axis.text.x = element_text(size = input$s4_axis_text %||% 12,
                                 angle = input$s4_axis_angle %||% 20,
                                 hjust = 1, vjust = 0.5),
      axis.text.y = element_text(size = input$s4_axis_text %||% 12),
      axis.title.x = element_text(size = input$s4_axis_title %||% 14),
      axis.title.y = element_text(size = input$s4_axis_title %||% 14),
      plot.title = element_text(size = input$s4_plot_title %||% 16, hjust = input$s4_title_hjust %||% 0.5),
      strip.text = element_text(size = input$s4_strip_text %||% 12),
      strip.background = element_blank(),
      legend.text = element_text(size = input$s4_legend_text %||% 10),
      legend.title = element_text(size = input$s4_legend_title %||% 10),
      legend.position = if (input$s4_legend_show %||% TRUE) (input$s4_legend_pos %||% "top") else "none",
      legend.key.size = unit(input$s4_legend_key_size %||% 0.8, "cm"),
      axis.line = element_line(colour = input$s4_axis_line_col %||% "black"),
      axis.ticks = element_line(colour = input$s4_axis_line_col %||% "black"),
      panel.background  = element_rect(fill = if ((input$s4_bg %||% "white") == "transparent") NA else (input$s4_panel_fill %||% "white"), colour = NA),
      plot.background   = element_rect(fill = if ((input$s4_bg %||% "white") == "transparent") NA else "white", colour = NA),
      legend.background = element_rect(fill = if ((input$s4_bg %||% "white") == "transparent") NA else "white", colour = NA),
      panel.grid.major = element_line(colour = input$s4_grid_col %||% "#e9ecf3"),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(input$s4_panel_spacing %||% 0, "lines"),
      plot.margin = margin(t = input$s4_margin_t %||% 30,
                           r = input$s4_margin_r %||% 10,
                           b = input$s4_margin_b %||% 30,
                           l = input$s4_margin_l %||% 10),
      panel.border = element_rect(colour = input$s4_border_col %||% "black",
                                  fill = NA,
                                  linewidth = input$s4_border_lwd %||% 0.5)
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
      rv$credit_penalty_plot <- Credit_Penalty(rv$table_minmax_USA, rv$type_colors)
      
      output$s4_dl <- downloadHandler(
        filename = function() { "secmod_credit_penalty.png" },
        content = function(file) {
          p <- credit_penalty_plot_final()
          req(p)
          width_in  <- input$s4_w   %||% 12.5
          height_in <- input$s4_h   %||% 8
          dpi_val   <- input$s4_dpi %||% 150
          bg_val <- input$s4_bg %||% "white"
          ggsave(file, plot = p, width = width_in, height = height_in,
                 dpi = dpi_val, bg = bg_val, limitsize = FALSE)
        }
      )
      
      showNotification("Credit/penalty chart generated", type = "message", duration = 5)
      updateTabsetPanel(session, "sec_output_tabs", selected = "Credit / penalty")
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
        numericInput(session$ns("s4_w"), "Width", value = 12.5, min = 3, max = 20, step = 0.5, width = "80px"),
        numericInput(session$ns("s4_h"), "Height", value = 8, min = 2, max = 15, step = 0.5, width = "80px"),
        numericInput(session$ns("s4_dpi"), "DPI", value = 150, min = 72, max = 300, step = 10, width = "80px"),
        selectInput(session$ns("s4_bg"), "Background",
                    choices = c("White" = "white", "Transparent" = "transparent"),
                    selected = "white", width = "100px"),
        numericInput(session$ns("s4_axis_text"), "Axis text", value = 12, min = 6, max = 30, step = 1, width = "80px"),
        numericInput(session$ns("s4_axis_title"), "Axis title", value = 14, min = 6, max = 30, step = 1, width = "80px"),
        numericInput(session$ns("s4_plot_title"), "Plot title", value = 16, min = 6, max = 30, step = 1, width = "80px"),
        numericInput(session$ns("s4_strip_text"), "Strip text", value = 12, min = 6, max = 30, step = 1, width = "80px"),
        numericInput(session$ns("s4_legend_text"), "Legend text", value = 10, min = 6, max = 30, step = 1, width = "80px"),
        numericInput(session$ns("s4_legend_title"), "Legend title", value = 10, min = 6, max = 30, step = 1, width = "80px"),
        numericInput(session$ns("s4_axis_angle"), "X angle", value = 20, min = 0, max = 90, step = 5, width = "80px"),
        numericInput(session$ns("s4_legend_key_size"), "Legend key", value = 0.8, min = 0.1, max = 3, step = 0.1, width = "80px"),
        numericInput(session$ns("s4_title_hjust"), "Title hjust", value = 0.5, min = 0, max = 1, step = 0.05, width = "80px"),
        numericInput(session$ns("s4_panel_spacing"), "Panel spacing", value = 0, min = 0, max = 10, step = 0.5, width = "80px"),
        numericInput(session$ns("s4_margin_t"), "Margin top", value = 30, min = 0, max = 100, step = 5, width = "80px"),
        numericInput(session$ns("s4_margin_r"), "Margin right", value = 10, min = 0, max = 100, step = 5, width = "80px"),
        numericInput(session$ns("s4_margin_b"), "Margin bottom", value = 30, min = 0, max = 100, step = 5, width = "80px"),
        numericInput(session$ns("s4_margin_l"), "Margin left", value = 10, min = 0, max = 100, step = 5, width = "80px"),
        selectInput(session$ns("s4_legend_pos"), "Legend",
                    choices = c("top","bottom","left","right","none"),
                    selected = "top", width = "80px"),
        checkboxInput(session$ns("s4_legend_show"), "Show legend", value = TRUE),
        colourInput(session$ns("s4_axis_line_col"), "Axis line", value = "black", showColour = "text", width = "80px"),
        colourInput(session$ns("s4_panel_fill"), "Panel bg", value = "white", showColour = "text", width = "80px"),
        colourInput(session$ns("s4_grid_col"), "Grid colour", value = "#e9ecf3", showColour = "text", width = "80px"),
        colourInput(session$ns("s4_border_col"), "Border colour", value = "black", showColour = "text", width = "80px"),
        numericInput(session$ns("s4_border_lwd"), "Border lwd", value = 0.5, min = 0, max = 5, step = 0.1, width = "80px"),
        colourInput(session$ns("s4_col_pen"), "Penalty", value = "#F0B323", showColour = "text", width = "80px"),
        colourInput(session$ns("s4_col_cred"), "Credit", value = "#6FACDE", showColour = "text", width = "80px"),
        numericInput(session$ns("s4_label_size"), "Label size", value = 3, min = 2, max = 10, step = 0.2, width = "80px"),
        numericInput(session$ns("s4_label_angle"), "Label angle", value = 0, min = 0, max = 90, step = 5, width = "80px")
      )
    )
  })
  
  output$s4_plot_frame <- renderUI({
    h_in <- input$s4_h %||% 8
    plotOutput(session$ns("s4_plot"), height = paste0(round(h_in * 96), "px"))
  })
  
  observe({
    bg_val <- if ((input$s4_bg %||% "white") == "transparent") "transparent" else "white"
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
          plots[[key]] <- STATE_plot(rv$aalp, LOB = lob, state_code = st)
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
                legend_title = def$legend_title %||% 10,
                axis_angle = def$axis_angle %||% 90,
                legend_pos = def$legend_pos %||% "top",
                legend_show = TRUE,
                col_sfd = def$col_sfd %||% "#6FACDE",
                col_com = def$col_com %||% "#F0B323",
                col_pen = def$col_pen %||% "#F0B323",
                col_cred = def$col_cred %||% "#6FACDE",
                legend_key_size = def$legend_key_size %||% 0.8,
                title_hjust = def$title_hjust %||% 0.5,
                axis_line_col = def$axis_line_col %||% "black",
                panel_fill = def$panel_fill %||% "white",
                bg = def$bg %||% "white",
                grid_col = def$grid_col %||% "#e9ecf3",
                panel_spacing = def$panel_spacing %||% 3,
                margin_t = def$margin_t %||% 30,
                margin_r = def$margin_r %||% 10,
                margin_b = def$margin_b %||% 30,
                margin_l = def$margin_l %||% 10,
                border_col = def$border_col %||% "black",
                border_lwd = def$border_lwd %||% 0.5
              )
              p <- apply_plot_overrides(p, input, "s5", key, defaults, is_credit = FALSE)
              w <- (input[[paste0("s5_w_", key)]] %||% def$w) %||% 13
              h <- (input[[paste0("s5_h_", key)]] %||% def$h) %||% 7.5
              dpi <- input$s5_default_dpi %||% 150
              ggsave(file, plot = p, width = w, height = h, dpi = dpi,
                     bg = attr(p, "vulsen_bg") %||% "white", limitsize = FALSE)
            }
          )
        })
      })
      
      showNotification("State plots generated", type = "message", duration = 5)
      updateTabsetPanel(session, "sec_output_tabs", selected = "State sensitivity")
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
                            default_dpi = 150,
                            default_axis_text = def$axis_text %||% 12,
                            default_axis_title = def$axis_title %||% 14,
                            default_plot_title = def$plot_title %||% 16,
                            default_strip_text = def$strip_text %||% 12,
                            default_legend_text = def$legend_text %||% 10,
                            default_legend_title = def$legend_title %||% 10,
                            default_axis_angle = def$axis_angle %||% 90,
                            default_legend_pos = def$legend_pos %||% "top",
                            default_col_sfd = def$col_sfd %||% "#6FACDE",
                            default_col_com = def$col_com %||% "#F0B323",
                            default_col_pen = def$col_pen %||% "#F0B323",
                            default_col_cred = def$col_cred %||% "#6FACDE",
                            default_legend_key_size = def$legend_key_size %||% 0.8,
                            default_title_hjust = def$title_hjust %||% 0.5,
                            default_axis_line_col = def$axis_line_col %||% "black",
                            default_panel_fill = def$panel_fill %||% "white",
                            default_grid_col = def$grid_col %||% "#e9ecf3",
                            default_panel_spacing = def$panel_spacing %||% 3,
                            default_margin_t = def$margin_t %||% 30,
                            default_margin_r = def$margin_r %||% 10,
                            default_margin_b = def$margin_b %||% 30,
                            default_margin_l = def$margin_l %||% 10,
                            default_border_col = def$border_col %||% "black",
                            default_border_lwd = def$border_lwd %||% 0.5,
                            default_bg = def$bg %||% "white")
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
      legend_title = def$legend_title %||% 10,
      axis_angle = def$axis_angle %||% 90,
      legend_key_size = def$legend_key_size %||% 0.8,
      title_hjust = def$title_hjust %||% 0.5,
      panel_spacing = def$panel_spacing %||% 3,
      margin_t = def$margin_t %||% 30,
      margin_r = def$margin_r %||% 10,
      margin_b = def$margin_b %||% 30,
      margin_l = def$margin_l %||% 10,
      border_lwd = def$border_lwd %||% 0.5,
      axis_line_col = def$axis_line_col %||% "black",
      panel_fill = def$panel_fill %||% "white",
      bg = def$bg %||% "white",
      grid_col = def$grid_col %||% "#e9ecf3",
      border_col = def$border_col %||% "black",
      col_sfd = def$col_sfd %||% "#6FACDE",
      col_com = def$col_com %||% "#F0B323",
      col_pen = def$col_pen %||% "#F0B323",
      col_cred = def$col_cred %||% "#6FACDE"
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
        default_legend_title = defaults$legend_title,
        default_axis_angle = defaults$axis_angle,
        default_legend_key_size = defaults$legend_key_size,
        default_title_hjust = defaults$title_hjust,
        default_panel_spacing = defaults$panel_spacing,
        default_margin_t = defaults$margin_t,
        default_margin_r = defaults$margin_r,
        default_margin_b = defaults$margin_b,
        default_margin_l = defaults$margin_l,
        default_border_lwd = defaults$border_lwd,
        default_axis_line_col = defaults$axis_line_col,
        default_panel_fill = defaults$panel_fill,
        default_grid_col = defaults$grid_col,
        default_border_col = defaults$border_col,
        default_col_sfd = defaults$col_sfd,
        default_col_com = defaults$col_com,
        default_col_pen = defaults$col_pen,
        default_col_cred = defaults$col_cred
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
      legend_title = input$s5_default_legend_title %||% 10,
      axis_angle = input$s5_default_axis_angle %||% 90,
      legend_pos = input$s5_default_legend_pos %||% "top",
      legend_key_size = input$s5_default_legend_key_size %||% 0.8,
      title_hjust = input$s5_default_title_hjust %||% 0.5,
      panel_spacing = input$s5_default_panel_spacing %||% 3,
      margin_t = input$s5_default_margin_t %||% 30,
      margin_r = input$s5_default_margin_r %||% 10,
      margin_b = input$s5_default_margin_b %||% 30,
      margin_l = input$s5_default_margin_l %||% 10,
      border_lwd = input$s5_default_border_lwd %||% 0.5,
      axis_line_col = input$s5_default_axis_line_col %||% "black",
      panel_fill = input$s5_default_panel_fill %||% "white",
      bg = input$s5_default_bg %||% "white",
      grid_col = input$s5_default_grid_col %||% "#e9ecf3",
      border_col = input$s5_default_border_col %||% "black",
      col_sfd = input$s5_default_col_sfd %||% "#6FACDE",
      col_com = input$s5_default_col_com %||% "#F0B323",
      col_pen = input$s5_default_col_pen %||% "#F0B323",
      col_cred = input$s5_default_col_cred %||% "#6FACDE"
    )
    gallery_defaults_s5(def)
    
    for (k in names(rv$state_lob_plots)) {
      updateNumericInput(session, paste0("s5_w_", k), value = def$w)
      updateNumericInput(session, paste0("s5_h_", k), value = def$h)
      updateNumericInput(session, paste0("s5_axis_text_", k), value = def$axis_text)
      updateNumericInput(session, paste0("s5_axis_title_", k), value = def$axis_title)
      updateNumericInput(session, paste0("s5_plot_title_", k), value = def$plot_title)
      updateNumericInput(session, paste0("s5_strip_text_", k), value = def$strip_text)
      updateNumericInput(session, paste0("s5_legend_text_", k), value = def$legend_text)
      updateNumericInput(session, paste0("s5_legend_title_", k), value = def$legend_title)
      updateNumericInput(session, paste0("s5_axis_angle_", k), value = def$axis_angle)
      updateNumericInput(session, paste0("s5_legend_key_size_", k), value = def$legend_key_size)
      updateNumericInput(session, paste0("s5_title_hjust_", k), value = def$title_hjust)
      updateNumericInput(session, paste0("s5_panel_spacing_", k), value = def$panel_spacing)
      updateNumericInput(session, paste0("s5_margin_t_", k), value = def$margin_t)
      updateNumericInput(session, paste0("s5_margin_r_", k), value = def$margin_r)
      updateNumericInput(session, paste0("s5_margin_b_", k), value = def$margin_b)
      updateNumericInput(session, paste0("s5_margin_l_", k), value = def$margin_l)
      updateNumericInput(session, paste0("s5_border_lwd_", k), value = def$border_lwd)
      updateSelectInput(session, paste0("s5_legend_pos_", k), selected = def$legend_pos)
      updateCheckboxInput(session, paste0("s5_legend_show_", k), value = TRUE)
      updateColourInput(session, paste0("s5_axis_line_col_", k), value = def$axis_line_col)
      updateColourInput(session, paste0("s5_panel_fill_", k), value = def$panel_fill)
      updateColourInput(session, paste0("s5_grid_col_", k), value = def$grid_col)
      updateColourInput(session, paste0("s5_border_col_", k), value = def$border_col)
      updateColourInput(session, paste0("s5_col_sfd_", k), value = def$col_sfd)
      updateColourInput(session, paste0("s5_col_com_", k), value = def$col_com)
      updateColourInput(session, paste0("s5_col_pen_", k), value = def$col_pen)
      updateColourInput(session, paste0("s5_col_cred_", k), value = def$col_cred)
    }
  })
  
  observe({
    req(rv$state_lob_plots)
    keys <- names(rv$state_lob_plots)
    def <- gallery_defaults_s5()
    bg_val <- if (is_transparent_bg(def$bg %||% "white")) "transparent" else "white"
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
            legend_title = def$legend_title %||% 10,
            axis_angle = def$axis_angle %||% 90,
            legend_pos = def$legend_pos %||% "top",
            legend_show = TRUE,
            col_sfd = def$col_sfd %||% "#6FACDE",
            col_com = def$col_com %||% "#F0B323",
            col_pen = def$col_pen %||% "#F0B323",
            col_cred = def$col_cred %||% "#6FACDE",
            legend_key_size = def$legend_key_size %||% 0.8,
            title_hjust = def$title_hjust %||% 0.5,
            axis_line_col = def$axis_line_col %||% "black",
            panel_fill = def$panel_fill %||% "white",
            bg = def$bg %||% "white",
            grid_col = def$grid_col %||% "#e9ecf3",
            panel_spacing = def$panel_spacing %||% 3,
            margin_t = def$margin_t %||% 30,
            margin_r = def$margin_r %||% 10,
            margin_b = def$margin_b %||% 30,
            margin_l = def$margin_l %||% 10,
            border_col = def$border_col %||% "black",
            border_lwd = def$border_lwd %||% 0.5
          )
          apply_plot_overrides(p, input, "s5", k, defaults, is_credit = FALSE)
        },
        width = function() ((input[[paste0("s5_w_", k)]] %||% def$w) %||% 13) * 96,
        height = function() ((input[[paste0("s5_h_", k)]] %||% def$h) %||% 7.5) * 96,
        res = 96,
        bg = bg_val)
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
      pl <- indmod(rv$aal_final)
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
                legend_title = def$legend_title %||% 10,
                axis_angle = def$axis_angle %||% 90,
                legend_pos = def$legend_pos %||% "top",
                legend_show = TRUE,
                col_sfd = def$col_sfd %||% "#6FACDE",
                col_com = def$col_com %||% "#F0B323",
                col_pen = def$col_pen %||% "#F0B323",
                col_cred = def$col_cred %||% "#6FACDE",
                legend_key_size = def$legend_key_size %||% 0.8,
                title_hjust = def$title_hjust %||% 0.5,
                axis_line_col = def$axis_line_col %||% "black",
                panel_fill = def$panel_fill %||% "white",
                bg = def$bg %||% "white",
                grid_col = def$grid_col %||% "#e9ecf3",
                panel_spacing = def$panel_spacing %||% 0,
                margin_t = def$margin_t %||% 30,
                margin_r = def$margin_r %||% 10,
                margin_b = def$margin_b %||% 30,
                margin_l = def$margin_l %||% 10,
                border_col = def$border_col %||% "black",
                border_lwd = def$border_lwd %||% 0.5
              )
              p <- apply_plot_overrides(p, input, "s6", key, defaults, is_credit = FALSE)
              w <- (input[[paste0("s6_w_", key)]] %||% def$w) %||% 13
              h <- (input[[paste0("s6_h_", key)]] %||% def$h) %||% 9
              dpi <- input$s6_default_dpi %||% 150
              ggsave(file, plot = p, width = w, height = h, dpi = dpi,
                     bg = attr(p, "vulsen_bg") %||% "white", limitsize = FALSE)
            }
          )
        })
      })
      
      showNotification("Modifier plots generated", type = "message", duration = 5)
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
                            default_dpi = 150,
                            default_axis_text = def$axis_text %||% 12,
                            default_axis_title = def$axis_title %||% 14,
                            default_plot_title = def$plot_title %||% 16,
                            default_strip_text = def$strip_text %||% 12,
                            default_legend_text = def$legend_text %||% 10,
                            default_legend_title = def$legend_title %||% 10,
                            default_axis_angle = def$axis_angle %||% 90,
                            default_legend_pos = def$legend_pos %||% "top",
                            default_col_sfd = def$col_sfd %||% "#6FACDE",
                            default_col_com = def$col_com %||% "#F0B323",
                            default_col_pen = def$col_pen %||% "#F0B323",
                            default_col_cred = def$col_cred %||% "#6FACDE",
                            default_legend_key_size = def$legend_key_size %||% 0.8,
                            default_title_hjust = def$title_hjust %||% 0.5,
                            default_axis_line_col = def$axis_line_col %||% "black",
                            default_panel_fill = def$panel_fill %||% "white",
                            default_grid_col = def$grid_col %||% "#e9ecf3",
                            default_panel_spacing = def$panel_spacing %||% 0,
                            default_margin_t = def$margin_t %||% 30,
                            default_margin_r = def$margin_r %||% 10,
                            default_margin_b = def$margin_b %||% 30,
                            default_margin_l = def$margin_l %||% 10,
                            default_border_col = def$border_col %||% "black",
                            default_border_lwd = def$border_lwd %||% 0.5,
                            default_bg = def$bg %||% "white")
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
      legend_title = def$legend_title %||% 10,
      axis_angle = def$axis_angle %||% 90,
      legend_key_size = def$legend_key_size %||% 0.8,
      title_hjust = def$title_hjust %||% 0.5,
      panel_spacing = def$panel_spacing %||% 0,
      margin_t = def$margin_t %||% 30,
      margin_r = def$margin_r %||% 10,
      margin_b = def$margin_b %||% 30,
      margin_l = def$margin_l %||% 10,
      border_lwd = def$border_lwd %||% 0.5,
      axis_line_col = def$axis_line_col %||% "black",
      panel_fill = def$panel_fill %||% "white",
      bg = def$bg %||% "white",
      grid_col = def$grid_col %||% "#e9ecf3",
      border_col = def$border_col %||% "black",
      col_sfd = def$col_sfd %||% "#6FACDE",
      col_com = def$col_com %||% "#F0B323",
      col_pen = def$col_pen %||% "#F0B323",
      col_cred = def$col_cred %||% "#6FACDE"
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
        default_legend_title = defaults$legend_title,
        default_axis_angle = defaults$axis_angle,
        default_legend_key_size = defaults$legend_key_size,
        default_title_hjust = defaults$title_hjust,
        default_panel_spacing = defaults$panel_spacing,
        default_margin_t = defaults$margin_t,
        default_margin_r = defaults$margin_r,
        default_margin_b = defaults$margin_b,
        default_margin_l = defaults$margin_l,
        default_border_lwd = defaults$border_lwd,
        default_axis_line_col = defaults$axis_line_col,
        default_panel_fill = defaults$panel_fill,
        default_grid_col = defaults$grid_col,
        default_border_col = defaults$border_col,
        default_col_sfd = defaults$col_sfd,
        default_col_com = defaults$col_com,
        default_col_pen = defaults$col_pen,
        default_col_cred = defaults$col_cred
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
      legend_title = input$s6_default_legend_title %||% 10,
      axis_angle = input$s6_default_axis_angle %||% 90,
      legend_pos = input$s6_default_legend_pos %||% "top",
      legend_key_size = input$s6_default_legend_key_size %||% 0.8,
      title_hjust = input$s6_default_title_hjust %||% 0.5,
      panel_spacing = input$s6_default_panel_spacing %||% 0,
      margin_t = input$s6_default_margin_t %||% 30,
      margin_r = input$s6_default_margin_r %||% 10,
      margin_b = input$s6_default_margin_b %||% 30,
      margin_l = input$s6_default_margin_l %||% 10,
      border_lwd = input$s6_default_border_lwd %||% 0.5,
      axis_line_col = input$s6_default_axis_line_col %||% "black",
      panel_fill = input$s6_default_panel_fill %||% "white",
      bg = input$s6_default_bg %||% "white",
      grid_col = input$s6_default_grid_col %||% "#e9ecf3",
      border_col = input$s6_default_border_col %||% "black",
      col_sfd = input$s6_default_col_sfd %||% "#6FACDE",
      col_com = input$s6_default_col_com %||% "#F0B323",
      col_pen = input$s6_default_col_pen %||% "#F0B323",
      col_cred = input$s6_default_col_cred %||% "#6FACDE"
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
      updateNumericInput(session, paste0("s6_legend_title_", k), value = def$legend_title)
      updateNumericInput(session, paste0("s6_axis_angle_", k), value = def$axis_angle)
      updateNumericInput(session, paste0("s6_legend_key_size_", k), value = def$legend_key_size)
      updateNumericInput(session, paste0("s6_title_hjust_", k), value = def$title_hjust)
      updateNumericInput(session, paste0("s6_panel_spacing_", k), value = def$panel_spacing)
      updateNumericInput(session, paste0("s6_margin_t_", k), value = def$margin_t)
      updateNumericInput(session, paste0("s6_margin_r_", k), value = def$margin_r)
      updateNumericInput(session, paste0("s6_margin_b_", k), value = def$margin_b)
      updateNumericInput(session, paste0("s6_margin_l_", k), value = def$margin_l)
      updateNumericInput(session, paste0("s6_border_lwd_", k), value = def$border_lwd)
      updateSelectInput(session, paste0("s6_legend_pos_", k), selected = def$legend_pos)
      updateCheckboxInput(session, paste0("s6_legend_show_", k), value = TRUE)
      updateColourInput(session, paste0("s6_axis_line_col_", k), value = def$axis_line_col)
      updateColourInput(session, paste0("s6_panel_fill_", k), value = def$panel_fill)
      updateColourInput(session, paste0("s6_grid_col_", k), value = def$grid_col)
      updateColourInput(session, paste0("s6_border_col_", k), value = def$border_col)
      updateColourInput(session, paste0("s6_col_sfd_", k), value = def$col_sfd)
      updateColourInput(session, paste0("s6_col_com_", k), value = def$col_com)
      updateColourInput(session, paste0("s6_col_pen_", k), value = def$col_pen)
      updateColourInput(session, paste0("s6_col_cred_", k), value = def$col_cred)
    }
  })
  
  observe({
    req(rv$modifier_plots)
    keys <- names(rv$modifier_plots)
    def <- gallery_defaults_s6()
    bg_val <- if (is_transparent_bg(def$bg %||% "white")) "transparent" else "white"
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
            legend_title = def$legend_title %||% 10,
            axis_angle = def$axis_angle %||% 90,
            legend_pos = def$legend_pos %||% "top",
            legend_show = TRUE,
            col_sfd = def$col_sfd %||% "#6FACDE",
            col_com = def$col_com %||% "#F0B323",
            col_pen = def$col_pen %||% "#F0B323",
            col_cred = def$col_cred %||% "#6FACDE",
            legend_key_size = def$legend_key_size %||% 0.8,
            title_hjust = def$title_hjust %||% 0.5,
            axis_line_col = def$axis_line_col %||% "black",
            panel_fill = def$panel_fill %||% "white",
            bg = def$bg %||% "white",
            grid_col = def$grid_col %||% "#e9ecf3",
            panel_spacing = def$panel_spacing %||% 0,
            margin_t = def$margin_t %||% 30,
            margin_r = def$margin_r %||% 10,
            margin_b = def$margin_b %||% 30,
            margin_l = def$margin_l %||% 10,
            border_col = def$border_col %||% "black",
            border_lwd = def$border_lwd %||% 0.5
          )
          apply_plot_overrides(p, input, "s6", k, defaults, is_credit = FALSE)
        },
        width = function() ((input[[paste0("s6_w_", k)]] %||% def$w) %||% 13) * 96,
        height = function() ((input[[paste0("s6_h_", k)]] %||% def$h) %||% 9) * 96,
        res = 96,
        bg = bg_val)
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
  
  # ---- Generate Report ----
  observeEvent(input$sec_generate_customized, {
    if (is.null(rv$credit_penalty_plot) || is.null(rv$state_lob_plots) || is.null(rv$modifier_plots)) {
      showNotification("Please complete sections 3, 4, and 5 first.", type = "error")
      return()
    }
    
    credit_bundle <- list(
      plot   = credit_penalty_plot_final(),
      width  = input$s4_w %||% 12.5,
      height = input$s4_h %||% 8,
      dpi    = input$s4_dpi %||% 150,
      bg     = input$s4_bg %||% "white",
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
        legend_title = def5$legend_title %||% 10,
        axis_angle = def5$axis_angle %||% 90,
        legend_pos = def5$legend_pos %||% "top",
        legend_show = TRUE,
        col_sfd = def5$col_sfd %||% "#6FACDE",
        col_com = def5$col_com %||% "#F0B323",
        col_pen = def5$col_pen %||% "#F0B323",
        col_cred = def5$col_cred %||% "#6FACDE",
        legend_key_size = def5$legend_key_size %||% 0.8,
        title_hjust = def5$title_hjust %||% 0.5,
        axis_line_col = def5$axis_line_col %||% "black",
        panel_fill = def5$panel_fill %||% "white",
        bg = def5$bg %||% "white",
        grid_col = def5$grid_col %||% "#e9ecf3",
        panel_spacing = def5$panel_spacing %||% 3,
        margin_t = def5$margin_t %||% 30,
        margin_r = def5$margin_r %||% 10,
        margin_b = def5$margin_b %||% 30,
        margin_l = def5$margin_l %||% 10,
        border_col = def5$border_col %||% "black",
        border_lwd = def5$border_lwd %||% 0.5
      )
      styled_p <- apply_plot_overrides(p, input, "s5", k, defaults, is_credit = FALSE)
      w <- (input[[paste0("s5_w_", k)]] %||% def5$w) %||% 13
      h <- (input[[paste0("s5_h_", k)]] %||% def5$h) %||% 7.5
      dpi <- input$s5_default_dpi %||% 150
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
        legend_title = def6$legend_title %||% 10,
        axis_angle = def6$axis_angle %||% 90,
        legend_pos = def6$legend_pos %||% "top",
        legend_show = TRUE,
        col_sfd = def6$col_sfd %||% "#6FACDE",
        col_com = def6$col_com %||% "#F0B323",
        col_pen = def6$col_pen %||% "#F0B323",
        col_cred = def6$col_cred %||% "#6FACDE",
        legend_key_size = def6$legend_key_size %||% 0.8,
        title_hjust = def6$title_hjust %||% 0.5,
        axis_line_col = def6$axis_line_col %||% "black",
        panel_fill = def6$panel_fill %||% "white",
        bg = def6$bg %||% "white",
        grid_col = def6$grid_col %||% "#e9ecf3",
        panel_spacing = def6$panel_spacing %||% 0,
        margin_t = def6$margin_t %||% 30,
        margin_r = def6$margin_r %||% 10,
        margin_b = def6$margin_b %||% 30,
        margin_l = def6$margin_l %||% 10,
        border_col = def6$border_col %||% "black",
        border_lwd = def6$border_lwd %||% 0.5
      )
      styled_p <- apply_plot_overrides(p, input, "s6", k, defaults, is_credit = FALSE)
      w <- (input[[paste0("s6_w_", k)]] %||% def6$w) %||% 13
      h <- (input[[paste0("s6_h_", k)]] %||% def6$h) %||% 9
      dpi <- input$s6_default_dpi %||% 150
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
      showNotification("Customized template not found: secmod_customized_template.Rmd", type = "error", duration = NULL)
      return()
    }
    
    showNotification("Generating customized report...", type = "message", duration = 5)
    
    tryCatch({
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
      
      showNotification("Customized report generated successfully.", type = "message", duration = 5)
    }, error = function(e) {
      showNotification(paste("Failed to generate customized report:", e$message), type = "error", duration = NULL)
    })
  })
  
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
  # ADD TO CART — shared handler
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
                           legend_title = def$legend_title %||% 10,
                           axis_angle = def$axis_angle %||% 90,
                           legend_pos = def$legend_pos %||% "top",
                           legend_show = TRUE,
                           col_sfd = def$col_sfd %||% "#6FACDE",
                           col_com = def$col_com %||% "#F0B323",
                           col_pen = def$col_pen %||% "#F0B323",
                           col_cred = def$col_cred %||% "#6FACDE",
                           legend_key_size = def$legend_key_size %||% 0.8,
                           title_hjust = def$title_hjust %||% 0.5,
                           axis_line_col = def$axis_line_col %||% "black",
                           panel_fill = def$panel_fill %||% "white",
                           bg = def$bg %||% "white",
                           grid_col = def$grid_col %||% "#e9ecf3",
                           panel_spacing = def$panel_spacing %||% 3,
                           margin_t = def$margin_t %||% 30,
                           margin_r = def$margin_r %||% 10,
                           margin_b = def$margin_b %||% 30,
                           margin_l = def$margin_l %||% 10,
                           border_col = def$border_col %||% "black",
                           border_lwd = def$border_lwd %||% 0.5
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
                           legend_title = def$legend_title %||% 10,
                           axis_angle = def$axis_angle %||% 90,
                           legend_pos = def$legend_pos %||% "top",
                           legend_show = TRUE,
                           col_sfd = def$col_sfd %||% "#6FACDE",
                           col_com = def$col_com %||% "#F0B323",
                           col_pen = def$col_pen %||% "#F0B323",
                           col_cred = def$col_cred %||% "#6FACDE",
                           legend_key_size = def$legend_key_size %||% 0.8,
                           title_hjust = def$title_hjust %||% 0.5,
                           axis_line_col = def$axis_line_col %||% "black",
                           panel_fill = def$panel_fill %||% "white",
                           bg = def$bg %||% "white",
                           grid_col = def$grid_col %||% "#e9ecf3",
                           panel_spacing = def$panel_spacing %||% 0,
                           margin_t = def$margin_t %||% 30,
                           margin_r = def$margin_r %||% 10,
                           margin_b = def$margin_b %||% 30,
                           margin_l = def$margin_l %||% 10,
                           border_col = def$border_col %||% "black",
                           border_lwd = def$border_lwd %||% 0.5
                         )
                         apply_plot_overrides(p, input, "s6", k, defaults, is_credit = FALSE)
                       }
    )
    req(plot_obj)
    
    if (prefix == "s4") {
      w <- input$s4_w %||% 12.5
      h <- input$s4_h %||% 8
      dpi <- input$s4_dpi %||% 150
    } else if (prefix == "s5") {
      def <- gallery_defaults_s5()
      w <- (input[[paste0("s5_w_", key)]] %||% def$w) %||% 13
      h <- (input[[paste0("s5_h_", key)]] %||% def$h) %||% 7.5
      dpi <- input$s5_default_dpi %||% 150
    } else if (prefix == "s6") {
      def <- gallery_defaults_s6()
      w <- (input[[paste0("s6_w_", key)]] %||% def$w) %||% 13
      h <- (input[[paste0("s6_h_", key)]] %||% def$h) %||% 9
      dpi <- input$s6_default_dpi %||% 150
    } else {
      w <- 9; h <- 5; dpi <- 150
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # ---- Pipeline progress stepper (right panel) ----
  output$sec_progress_panel <- renderUI({
    stages <- list(
      list(id = "sec-settings-card", label = "Settings", done = {
        nzchar(input$sec_vendor %||% "") &&
          nzchar(input$sec_country %||% "") &&
          nzchar(input$sec_peril %||% "") &&
          nzchar(input$sec_subperil %||% "") &&
          nzchar(input$sec_suffix %||% "")
      }),
      list(id = "sec-inputs-card", label = "Inputs", done = inputs_ready()),
      list(id = "sec-credit-card", label = "Credit", done = !is.null(rv$credit_penalty_plot)),
      list(id = "sec-state-card", label = "State", done = !is.null(rv$state_lob_plots)),
      list(id = "sec-modifier-card", label = "Indiv Modifier", done = !is.null(rv$modifier_plots)),
      list(id = "sec-report-card", label = "Report", done = isTRUE(rv$customized_report_generated))
    )
    
    # Build stepper
    div(
      class = "sec-progress-panel",
      style = "margin: 16px 0 12px 0; padding: 12px 16px; background: #f8faff; border-radius: 16px; border: 1px solid #e2e8f0;",
      div(
        style = "display: flex; align-items: center; justify-content: space-between; gap: 4px;",
        lapply(seq_along(stages), function(i) {
          st <- stages[[i]]
          is_done <- st$done
          # Circle
          circle <- if (is_done) {
            tags$span(
              class = "sec-step-circle done",
              icon("check")
            )
          } else {
            tags$span(
              class = "sec-step-circle pending",
              i
            )
          }
          # Label
          label <- tags$div(
            class = "sec-step-label",
            st$label
          )
          # Connector (except after last)
          connector <- if (i < length(stages)) {
            tags$div(
              class = paste("sec-connector", if (is_done && stages[[i+1]]$done) "done" else "pending")
            )
          } else NULL
          # Wrap in clickable div
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
  
  
  
  
  
   
  
  
  
}