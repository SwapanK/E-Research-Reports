library(shiny)
library(shinyjs)
library(ggplot2)
library(colourpicker)

# =============================================================================
# SECONDARY MODIFIER SERVER — TAB‑BASED NAVIGATION
# =============================================================================

# ---- helpers -----------------------------------------------------------------
is_transparent_bg <- function(x) {
  !is.null(x) && tolower(trimws(as.character(x))) %in% c("transparent", "na", "none")
}

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

# -----------------------------------------------------------------------------
# Helper to apply theme and colour overrides to a plot (stages 5 & 6)
# -----------------------------------------------------------------------------
apply_plot_overrides <- function(p, input, prefix, key, defaults, is_credit = FALSE) {
  # Extract values with defaults
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
  
  # Colours
  col_sfd <- input[[paste0(prefix, "_col_sfd_", key)]] %||% defaults$col_sfd %||% "#6FACDE"
  col_com <- input[[paste0(prefix, "_col_com_", key)]] %||% defaults$col_com %||% "#F0B323"
  col_pen <- input[[paste0(prefix, "_col_pen_", key)]] %||% defaults$col_pen %||% "#F0B323"
  col_cred <- input[[paste0(prefix, "_col_cred_", key)]] %||% defaults$col_cred %||% "#6FACDE"
  
  is_transp <- is_transparent_bg(panel_fill) || is_transparent_bg(bg_choice)
  
  p <- p + theme(
    axis.text.x = element_text(size = axis_text, angle = axis_angle, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(size = axis_text),
    axis.title.x = element_text(size = axis_title),
    axis.title.y = element_text(size = axis_title),
    plot.title = element_text(size = plot_title, hjust = title_hjust),
    strip.text = element_text(size = strip_text),
    legend.text = element_text(size = legend_text),
    legend.title = element_text(size = legend_title),
    legend.position = if (show_legend) legend_pos else "none",
    legend.key.size = unit(legend_key_size, "cm"),
    axis.line = element_line(colour = axis_line_col),
    axis.ticks = element_line(colour = axis_line_col),
    panel.background  = element_rect(fill = if (is_transp) NA else panel_fill, colour = NA),
    plot.background   = element_rect(fill = if (is_transp) NA else "white", colour = NA),
    legend.background = element_rect(fill = if (is_transp) NA else "white", colour = NA),
    legend.key        = element_rect(fill = if (is_transp) NA else "white", colour = NA),
    panel.grid.major = element_line(colour = grid_col),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(panel_spacing, "lines"),
    plot.margin = margin(t = margin_t, r = margin_r, b = margin_b, l = margin_l),
    panel.border = element_rect(colour = border_col, fill = NA, linewidth = border_lwd)
  ) +
    scale_fill_manual(values = c("SFD" = col_sfd, "COM" = col_com,
                                 "Max" = col_pen, "Min" = col_cred,
                                 "Penalty" = col_pen, "Credit" = col_cred)) +
    coord_cartesian(clip = "off")  # prevent label clipping
  
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
    modifier_plots = NULL, modifier_labels = NULL
  )
  
  ## =========================================================================
  ## STAGE PROGRESS STRIP (brown themed)
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
    pct     <- round(100 * n_done / n)
    fill_pct <- if (n > 1) max(0, n_done - 1) / (n - 1) * 100 else 0
    
    div(
      class = "sec-stepper glass-card",
      div(
        class = "vulsen-progress-head",
        span(class = "vulsen-progress-title", icon("chart-line"), "Assessment progress"),
        span(class = "vulsen-progress-meta", sprintf("%d of %d stages · %d%%", n_done, n, pct))
      ),
      div(
        class = "vulsen-progress-track",
        div(class = "vulsen-progress-fill", style = sprintf("width:%s%%;", fill_pct))
      ),
      div(
        class = "vulsen-progress-chips",
        lapply(seq_along(labels), function(i) {
          span(
            class = paste("vulsen-progress-chip", if (statuses[i]) "is-done" else ""),
            if (statuses[i]) icon("circle-check") else icon("circle"),
            labels[i]
          )
        })
      )
    )
  })
  
  # Per‑stage status badges (shown inside each tab)
  output$sec_stage1_status <- renderUI(sec_status_badge(!is.null(rv$aal_State) && !is.null(rv$aal_USA)))
  output$sec_stage2_status <- renderUI(sec_status_badge(!is.null(rv$aal_final) && !is.null(rv$aal_final_USA)))
  output$sec_stage3_status <- renderUI(sec_status_badge(!is.null(rv$table_minmax_USA)))
  output$sec_stage4_status <- renderUI(sec_status_badge(!is.null(rv$credit_penalty_plot)))
  output$sec_stage5_status <- renderUI(sec_status_badge(!is.null(rv$state_lob_plots)))
  output$sec_stage6_status <- renderUI(sec_status_badge(!is.null(rv$modifier_plots)))
  
  ## =========================================================================
  ## STAGE 1 — LOAD DATA
  ## =========================================================================
  
  observeEvent(input$sec_load, {
    req(input$sec_file_state, input$sec_file_usa)
    
    read_any <- function(datapath, original_name) {
      ext <- tolower(tools::file_ext(original_name))
      if (ext == "rds") readRDS(datapath)
      else if (ext == "csv") utils::read.csv(datapath, stringsAsFactors = FALSE)
      else stop("Unsupported file type: ", ext)
    }
    
    tryCatch({
      rv$aal_State <- read_any(input$sec_file_state$datapath, input$sec_file_state$name)
      rv$aal_USA   <- read_any(input$sec_file_usa$datapath,   input$sec_file_usa$name)
      
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
      
      # Move to stage 2
      updateTabsetPanel(session, "sec_tabs", selected = "stage2")
    }, error = function(e) {
      session$sendCustomMessage("show-toast", list(
        text = paste("Load failed:", conditionMessage(e)), type = "error",
        icon = "fa-exclamation-circle", duration = 4000
      ))
    })
  })
  
  ## =========================================================================
  ## STAGE 2 — BUILD TABLES (finaltable / finaltable_allUSA) or upload‑skip
  ## =========================================================================
  
  observeEvent(input$sec_build, {
    req(rv$aal_State, rv$aal_USA)
    tryCatch({
      rv$aal_final     <- finaltable(rv$aal_State, rv$SecMod_name)
      rv$aal_final_USA <- finaltable_allUSA(rv$aal_USA, rv$SecMod_name)
      
      session$sendCustomMessage("show-toast", list(
        text = "Tables built", type = "success", icon = "fa-check", duration = 2000
      ))
      
      updateTabsetPanel(session, "sec_tabs", selected = "stage3")
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
  
  # Upload pre‑built CSVs to skip stage 1+2
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
      
      updateTabsetPanel(session, "sec_tabs", selected = "stage4")
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
  }, striped = TRUE, hover = TRUE, bordered = FALSE, spacing = "m",
  width = "100%", align = "lcrrll")
  
  ## =========================================================================
  ## STAGE 4 — CREDIT / PENALTY
  ## =========================================================================
  
  # ---- Reactive that builds the final plot with all overrides ----
  credit_penalty_plot_final <- reactive({
    req(rv$credit_penalty_plot)
    p <- rv$credit_penalty_plot
    
    p <- p + theme(
      axis.text.x = element_text(size = input$s4_axis_text %||% 12,
                                 angle = input$s4_axis_angle %||% 90,
                                 hjust = 1, vjust = 0.5),
      axis.text.y = element_text(size = input$s4_axis_text %||% 12),
      axis.title.x = element_text(size = input$s4_axis_title %||% 14),
      axis.title.y = element_text(size = input$s4_axis_title %||% 14),
      plot.title = element_text(size = input$s4_plot_title %||% 16, hjust = input$s4_title_hjust %||% 0.5),
      strip.text = element_text(size = input$s4_strip_text %||% 12),
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
      panel.spacing = unit(input$s4_panel_spacing %||% 3, "lines"),
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
      coord_cartesian(clip = "off")  # prevent label clipping
    
    # Adjust geom_text layers for label size/angle if present
    if (length(p$layers) > 0) {
      for (i in seq_along(p$layers)) {
        if (inherits(p$layers[[i]]$geom, "GeomText")) {
          p$layers[[i]]$aes_params$size <- input$s4_label_size %||% 5.2
          p$layers[[i]]$aes_params$angle <- input$s4_label_angle %||% 0
        }
      }
    }
    
    p
  })
  
  observeEvent(input$sec_credit, {
    req(rv$table_minmax_USA)
    tryCatch({
      rv$credit_penalty_plot <- Credit_Penalty(rv$table_minmax_USA, rv$type_colors)
      
      # Register the download handler for stage 4
      output$s4_dl <- downloadHandler(
        filename = function() { "secmod_credit_penalty.png" },
        content = function(file) {
          p <- credit_penalty_plot_final()
          req(p)
          width_in  <- input$s4_w   %||% 9
          height_in <- input$s4_h   %||% 6
          dpi_val   <- input$s4_dpi %||% 150
          bg_val <- input$s4_bg %||% "white"
          ggsave(file, plot = p, width = width_in, height = height_in,
                 dpi = dpi_val, bg = bg_val, limitsize = FALSE)
        }
      )
      
      session$sendCustomMessage("show-toast", list(
        text = "Credit/penalty chart generated", type = "success", icon = "fa-chart-bar", duration = 2000
      ))
      
      updateTabsetPanel(session, "sec_tabs", selected = "stage5")
    }, error = function(e) {
      session$sendCustomMessage("show-toast", list(
        text = paste("Chart failed:", conditionMessage(e)), type = "error",
        icon = "fa-exclamation-circle", duration = 4000
      ))
    })
  })
  
  # Render the credit card UI
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
            class = "btn-icon-cart", title = "Adjust size, colours, text & legend",
            icon("sliders-h")
          ),
          downloadButton(
            outputId = session$ns("s4_dl"),
            label = NULL,
            icon = icon("download"),
            class = "btn-icon-cart",
            title = "Download"
          ),
          tags$button(onclick = "secmodCartClick('s4|credit_penalty')",
                      class = "btn-icon-cart", title = "Add to cart", icon("cart-plus"))
        )
      ),
      div(class = "sec-plot-frame", uiOutput(session$ns("s4_plot_frame"))),
      div(
        id = "s4_override_panel", class = "sec-override-panel",
        numericInput(session$ns("s4_w"), "Width", value = 9, min = 3, max = 20, step = 0.5, width = "80px"),
        numericInput(session$ns("s4_h"), "Height", value = 6, min = 2, max = 15, step = 0.5, width = "80px"),
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
        numericInput(session$ns("s4_axis_angle"), "X angle", value = 90, min = 0, max = 90, step = 5, width = "80px"),
        numericInput(session$ns("s4_legend_key_size"), "Legend key", value = 0.8, min = 0.1, max = 3, step = 0.1, width = "80px"),
        numericInput(session$ns("s4_title_hjust"), "Title hjust", value = 0.5, min = 0, max = 1, step = 0.05, width = "80px"),
        numericInput(session$ns("s4_panel_spacing"), "Panel spacing", value = 3, min = 0, max = 10, step = 0.5, width = "80px"),
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
        numericInput(session$ns("s4_label_size"), "Label size", value = 5.2, min = 2, max = 10, step = 0.2, width = "80px"),
        numericInput(session$ns("s4_label_angle"), "Label angle", value = 0, min = 0, max = 90, step = 5, width = "80px")
      )
    )
  })
  
  output$s4_plot_frame <- renderUI({
    h_in <- input$s4_h %||% 6
    plotOutput(session$ns("s4_plot"), height = paste0(round(h_in * 96), "px"))
  })
  
  observe({
    bg_val <- if ((input$s4_bg %||% "white") == "transparent") "transparent" else "white"
    output$s4_plot <- renderPlot({
      credit_penalty_plot_final()
    },
    width = function() (input$s4_w %||% 9) * 96,
    height = function() (input$s4_h %||% 6) * 96,
    res = 96,
    bg = bg_val)
  })
  
  ## =========================================================================
  ## STAGE 5 — STATE SENSITIVITY (gallery)
  ## =========================================================================
  
  observeEvent(input$sec_state_plots, {
    req(rv$aalp)
    assign("mycolors", rv$mycolors, envir = .GlobalEnv)
    
    session$sendCustomMessage("show-toast", list(
      text = "Starting state plot generation…", type = "info", icon = "fa-spinner", duration = 60000
    ))
    
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
      
      # Register download handlers for each state plot
      lapply(names(plots), function(k) {
        local({
          key <- k
          output[[paste0("s5_dl_", key)]] <- downloadHandler(
            filename = function() paste0("secmod_s5_", key, ".png"),
            content = function(file) {
              p <- rv$state_lob_plots[[key]]
              defaults <- list(
                axis_text = input$s5_default_axis_text %||% 12,
                axis_title = input$s5_default_axis_title %||% 14,
                plot_title = input$s5_default_plot_title %||% 16,
                strip_text = input$s5_default_strip_text %||% 12,
                legend_text = input$s5_default_legend_text %||% 10,
                legend_title = input$s5_default_legend_title %||% 10,
                axis_angle = input$s5_default_axis_angle %||% 90,
                legend_pos = input$s5_default_legend_pos %||% "top",
                legend_show = TRUE,
                col_sfd = input$s5_default_col_sfd %||% "#6FACDE",
                col_com = input$s5_default_col_com %||% "#F0B323",
                col_pen = input$s5_default_col_pen %||% "#F0B323",
                col_cred = input$s5_default_col_cred %||% "#6FACDE",
                legend_key_size = input$s5_default_legend_key_size %||% 0.8,
                title_hjust = input$s5_default_title_hjust %||% 0.5,
                axis_line_col = input$s5_default_axis_line_col %||% "black",
                panel_fill = input$s5_default_panel_fill %||% "white",
                bg = input$s5_default_bg %||% "white",
                grid_col = input$s5_default_grid_col %||% "#e9ecf3",
                panel_spacing = input$s5_default_panel_spacing %||% 3,
                margin_t = input$s5_default_margin_t %||% 30,
                margin_r = input$s5_default_margin_r %||% 10,
                margin_b = input$s5_default_margin_b %||% 30,
                margin_l = input$s5_default_margin_l %||% 10,
                border_col = input$s5_default_border_col %||% "black",
                border_lwd = input$s5_default_border_lwd %||% 0.5
              )
              p <- apply_plot_overrides(p, input, "s5", key, defaults, is_credit = FALSE)
              w <- (input[[paste0("s5_w_", key)]] %||% input$s5_default_w) %||% 9
              h <- (input[[paste0("s5_h_", key)]] %||% input$s5_default_h) %||% 5
              dpi <- input$s5_default_dpi %||% 150
              ggsave(file, plot = p, width = w, height = h, dpi = dpi,
                     bg = attr(p, "vulsen_bg") %||% "white", limitsize = FALSE)
            }
          )
        })
      })
      
      session$sendCustomMessage("show-toast", list(
        text = "State plots generated", type = "success",
        icon = "fa-chart-bar", duration = 3000
      ))
    }, error = function(e) {
      session$sendCustomMessage("show-toast", list(
        text = paste("Generation failed:", conditionMessage(e)), type = "error",
        icon = "fa-exclamation-circle", duration = 4000
      ))
    })
  })
  
  # Gallery controls and rendering (pass ns from session)
  output$sec_stage5_gallery_controls <- renderUI({
    req(rv$state_lob_plots)
    sec_gallery_controls_ui(session$ns, "s5", length(rv$state_lob_plots), 9, 5, 150,
                            default_axis_text = 12, default_axis_title = 14,
                            default_plot_title = 16, default_strip_text = 12,
                            default_legend_text = 10, default_legend_title = 10,
                            default_axis_angle = 90, default_legend_pos = "top",
                            default_col_sfd = "#6FACDE", default_col_com = "#F0B323",
                            default_col_pen = "#F0B323", default_col_cred = "#6FACDE",
                            default_legend_key_size = 0.8, default_title_hjust = 0.5,
                            default_axis_line_col = "black", default_panel_fill = "white",
                            default_grid_col = "#e9ecf3",
                            default_panel_spacing = 3, default_margin_t = 30,
                            default_margin_r = 10, default_margin_b = 30, default_margin_l = 10,
                            default_border_col = "black", default_border_lwd = 0.5)
  })
  
  output$sec_stage5_gallery <- renderUI({
    req(rv$state_lob_plots)
    keys <- names(rv$state_lob_plots)
    defaults <- list(
      w = input$s5_default_w %||% 9,
      h = input$s5_default_h %||% 5,
      axis_text = input$s5_default_axis_text %||% 12,
      axis_title = input$s5_default_axis_title %||% 14,
      plot_title = input$s5_default_plot_title %||% 16,
      strip_text = input$s5_default_strip_text %||% 12,
      legend_text = input$s5_default_legend_text %||% 10,
      legend_title = input$s5_default_legend_title %||% 10,
      axis_angle = input$s5_default_axis_angle %||% 90,
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
    for (k in names(rv$state_lob_plots)) {
      updateNumericInput(session, paste0("s5_w_", k), value = input$s5_default_w)
      updateNumericInput(session, paste0("s5_h_", k), value = input$s5_default_h)
      updateNumericInput(session, paste0("s5_axis_text_", k), value = input$s5_default_axis_text)
      updateNumericInput(session, paste0("s5_axis_title_", k), value = input$s5_default_axis_title)
      updateNumericInput(session, paste0("s5_plot_title_", k), value = input$s5_default_plot_title)
      updateNumericInput(session, paste0("s5_strip_text_", k), value = input$s5_default_strip_text)
      updateNumericInput(session, paste0("s5_legend_text_", k), value = input$s5_default_legend_text)
      updateNumericInput(session, paste0("s5_legend_title_", k), value = input$s5_default_legend_title)
      updateNumericInput(session, paste0("s5_axis_angle_", k), value = input$s5_default_axis_angle)
      updateNumericInput(session, paste0("s5_legend_key_size_", k), value = input$s5_default_legend_key_size)
      updateNumericInput(session, paste0("s5_title_hjust_", k), value = input$s5_default_title_hjust)
      updateNumericInput(session, paste0("s5_panel_spacing_", k), value = input$s5_default_panel_spacing)
      updateNumericInput(session, paste0("s5_margin_t_", k), value = input$s5_default_margin_t)
      updateNumericInput(session, paste0("s5_margin_r_", k), value = input$s5_default_margin_r)
      updateNumericInput(session, paste0("s5_margin_b_", k), value = input$s5_default_margin_b)
      updateNumericInput(session, paste0("s5_margin_l_", k), value = input$s5_default_margin_l)
      updateNumericInput(session, paste0("s5_border_lwd_", k), value = input$s5_default_border_lwd)
      updateSelectInput(session, paste0("s5_legend_pos_", k), selected = input$s5_default_legend_pos)
      updateCheckboxInput(session, paste0("s5_legend_show_", k), value = TRUE)
      updateColourInput(session, paste0("s5_axis_line_col_", k), value = input$s5_default_axis_line_col)
      updateColourInput(session, paste0("s5_panel_fill_", k), value = input$s5_default_panel_fill)
      updateColourInput(session, paste0("s5_grid_col_", k), value = input$s5_default_grid_col)
      updateColourInput(session, paste0("s5_border_col_", k), value = input$s5_default_border_col)
      updateColourInput(session, paste0("s5_col_sfd_", k), value = input$s5_default_col_sfd)
      updateColourInput(session, paste0("s5_col_com_", k), value = input$s5_default_col_com)
      updateColourInput(session, paste0("s5_col_pen_", k), value = input$s5_default_col_pen)
      updateColourInput(session, paste0("s5_col_cred_", k), value = input$s5_default_col_cred)
    }
  })
  
  observe({
    req(rv$state_lob_plots)
    keys <- names(rv$state_lob_plots)
    bg_choice <- input$s5_default_bg %||% "white"
    bg_val <- if (is_transparent_bg(bg_choice)) "transparent" else "white"
    lapply(keys, function(key) {
      local({
        k <- key
        output[[paste0("s5_plot_frame_", k)]] <- renderUI({
          h_in <- (input[[paste0("s5_h_", k)]] %||% input$s5_default_h) %||% 5
          plotOutput(session$ns(paste0("s5_plot_", k)), height = paste0(round(h_in * 96), "px"))
        })
        output[[paste0("s5_plot_", k)]] <- renderPlot({
          p <- rv$state_lob_plots[[k]]
          defaults <- list(
            axis_text = input$s5_default_axis_text %||% 12,
            axis_title = input$s5_default_axis_title %||% 14,
            plot_title = input$s5_default_plot_title %||% 16,
            strip_text = input$s5_default_strip_text %||% 12,
            legend_text = input$s5_default_legend_text %||% 10,
            legend_title = input$s5_default_legend_title %||% 10,
            axis_angle = input$s5_default_axis_angle %||% 90,
            legend_pos = input$s5_default_legend_pos %||% "top",
            legend_show = TRUE,
            col_sfd = input$s5_default_col_sfd %||% "#6FACDE",
            col_com = input$s5_default_col_com %||% "#F0B323",
            col_pen = input$s5_default_col_pen %||% "#F0B323",
            col_cred = input$s5_default_col_cred %||% "#6FACDE",
            legend_key_size = input$s5_default_legend_key_size %||% 0.8,
            title_hjust = input$s5_default_title_hjust %||% 0.5,
            axis_line_col = input$s5_default_axis_line_col %||% "black",
            panel_fill = input$s5_default_panel_fill %||% "white",
            bg = input$s5_default_bg %||% "white",
            grid_col = input$s5_default_grid_col %||% "#e9ecf3",
            panel_spacing = input$s5_default_panel_spacing %||% 3,
            margin_t = input$s5_default_margin_t %||% 30,
            margin_r = input$s5_default_margin_r %||% 10,
            margin_b = input$s5_default_margin_b %||% 30,
            margin_l = input$s5_default_margin_l %||% 10,
            border_col = input$s5_default_border_col %||% "black",
            border_lwd = input$s5_default_border_lwd %||% 0.5
          )
          apply_plot_overrides(p, input, "s5", k, defaults, is_credit = FALSE)
        },
        width = function() ((input[[paste0("s5_w_", k)]] %||% input$s5_default_w) %||% 9) * 96,
        height = function() ((input[[paste0("s5_h_", k)]] %||% input$s5_default_h) %||% 5) * 96,
        res = 96,
        bg = bg_val)
      })
    })
  })
  
  ## =========================================================================
  ## STAGE 6 — MODIFIER DETAIL (gallery)
  ## =========================================================================
  
  observeEvent(input$sec_modifier_plots, {
    req(rv$aal_final)
    assign("mycolors", rv$mycolors, envir = .GlobalEnv)
    
    session$sendCustomMessage("show-toast", list(
      text = "Starting modifier plot generation…", type = "info", icon = "fa-spinner", duration = 60000
    ))
    
    tryCatch({
      pl <- indmod(rv$aal_final)
      if (length(pl) == 0) {
        stop("No modifier categories found other than \"Unknown\" — check how rv$aal_final$modifier is being classified upstream.")
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
              defaults <- list(
                axis_text = input$s6_default_axis_text %||% 12,
                axis_title = input$s6_default_axis_title %||% 14,
                plot_title = input$s6_default_plot_title %||% 16,
                strip_text = input$s6_default_strip_text %||% 12,
                legend_text = input$s6_default_legend_text %||% 10,
                legend_title = input$s6_default_legend_title %||% 10,
                axis_angle = input$s6_default_axis_angle %||% 90,
                legend_pos = input$s6_default_legend_pos %||% "top",
                legend_show = TRUE,
                col_sfd = input$s6_default_col_sfd %||% "#6FACDE",
                col_com = input$s6_default_col_com %||% "#F0B323",
                col_pen = input$s6_default_col_pen %||% "#F0B323",
                col_cred = input$s6_default_col_cred %||% "#6FACDE",
                legend_key_size = input$s6_default_legend_key_size %||% 0.8,
                title_hjust = input$s6_default_title_hjust %||% 0.5,
                axis_line_col = input$s6_default_axis_line_col %||% "black",
                panel_fill = input$s6_default_panel_fill %||% "white",
                bg = input$s6_default_bg %||% "white",
                grid_col = input$s6_default_grid_col %||% "#e9ecf3",
                panel_spacing = input$s6_default_panel_spacing %||% 3,
                margin_t = input$s6_default_margin_t %||% 30,
                margin_r = input$s6_default_margin_r %||% 10,
                margin_b = input$s6_default_margin_b %||% 30,
                margin_l = input$s6_default_margin_l %||% 10,
                border_col = input$s6_default_border_col %||% "black",
                border_lwd = input$s6_default_border_lwd %||% 0.5
              )
              p <- apply_plot_overrides(p, input, "s6", key, defaults, is_credit = FALSE)
              w <- (input[[paste0("s6_w_", key)]] %||% input$s6_default_w) %||% 10
              h <- (input[[paste0("s6_h_", key)]] %||% input$s6_default_h) %||% 6
              dpi <- input$s6_default_dpi %||% 150
              ggsave(file, plot = p, width = w, height = h, dpi = dpi,
                     bg = attr(p, "vulsen_bg") %||% "white", limitsize = FALSE)
            }
          )
        })
      })
      
      session$sendCustomMessage("show-toast", list(
        text = "Modifier plots generated", type = "success",
        icon = "fa-chart-bar", duration = 3000
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
    sec_gallery_controls_ui(session$ns, "s6", length(rv$modifier_plots), 10, 6, 150,
                            default_axis_text = 12, default_axis_title = 14,
                            default_plot_title = 16, default_strip_text = 12,
                            default_legend_text = 10, default_legend_title = 10,
                            default_axis_angle = 90, default_legend_pos = "top",
                            default_col_sfd = "#6FACDE", default_col_com = "#F0B323",
                            default_col_pen = "#F0B323", default_col_cred = "#6FACDE",
                            default_legend_key_size = 0.8, default_title_hjust = 0.5,
                            default_axis_line_col = "black", default_panel_fill = "white",
                            default_grid_col = "#e9ecf3",
                            default_panel_spacing = 3, default_margin_t = 30,
                            default_margin_r = 10, default_margin_b = 30, default_margin_l = 10,
                            default_border_col = "black", default_border_lwd = 0.5)
  })
  
  output$sec_stage6_gallery <- renderUI({
    req(rv$modifier_plots)
    keys <- names(rv$modifier_plots)
    defaults <- list(
      w = input$s6_default_w %||% 10,
      h = input$s6_default_h %||% 6,
      axis_text = input$s6_default_axis_text %||% 12,
      axis_title = input$s6_default_axis_title %||% 14,
      plot_title = input$s6_default_plot_title %||% 16,
      strip_text = input$s6_default_strip_text %||% 12,
      legend_text = input$s6_default_legend_text %||% 10,
      legend_title = input$s6_default_legend_title %||% 10,
      axis_angle = input$s6_default_axis_angle %||% 90,
      legend_key_size = input$s6_default_legend_key_size %||% 0.8,
      title_hjust = input$s6_default_title_hjust %||% 0.5,
      panel_spacing = input$s6_default_panel_spacing %||% 3,
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
    for (k in names(rv$modifier_plots)) {
      updateNumericInput(session, paste0("s6_w_", k), value = input$s6_default_w)
      updateNumericInput(session, paste0("s6_h_", k), value = input$s6_default_h)
      updateNumericInput(session, paste0("s6_axis_text_", k), value = input$s6_default_axis_text)
      updateNumericInput(session, paste0("s6_axis_title_", k), value = input$s6_default_axis_title)
      updateNumericInput(session, paste0("s6_plot_title_", k), value = input$s6_default_plot_title)
      updateNumericInput(session, paste0("s6_strip_text_", k), value = input$s6_default_strip_text)
      updateNumericInput(session, paste0("s6_legend_text_", k), value = input$s6_default_legend_text)
      updateNumericInput(session, paste0("s6_legend_title_", k), value = input$s6_default_legend_title)
      updateNumericInput(session, paste0("s6_axis_angle_", k), value = input$s6_default_axis_angle)
      updateNumericInput(session, paste0("s6_legend_key_size_", k), value = input$s6_default_legend_key_size)
      updateNumericInput(session, paste0("s6_title_hjust_", k), value = input$s6_default_title_hjust)
      updateNumericInput(session, paste0("s6_panel_spacing_", k), value = input$s6_default_panel_spacing)
      updateNumericInput(session, paste0("s6_margin_t_", k), value = input$s6_default_margin_t)
      updateNumericInput(session, paste0("s6_margin_r_", k), value = input$s6_default_margin_r)
      updateNumericInput(session, paste0("s6_margin_b_", k), value = input$s6_default_margin_b)
      updateNumericInput(session, paste0("s6_margin_l_", k), value = input$s6_default_margin_l)
      updateNumericInput(session, paste0("s6_border_lwd_", k), value = input$s6_default_border_lwd)
      updateSelectInput(session, paste0("s6_legend_pos_", k), selected = input$s6_default_legend_pos)
      updateCheckboxInput(session, paste0("s6_legend_show_", k), value = TRUE)
      updateColourInput(session, paste0("s6_axis_line_col_", k), value = input$s6_default_axis_line_col)
      updateColourInput(session, paste0("s6_panel_fill_", k), value = input$s6_default_panel_fill)
      updateColourInput(session, paste0("s6_grid_col_", k), value = input$s6_default_grid_col)
      updateColourInput(session, paste0("s6_border_col_", k), value = input$s6_default_border_col)
      updateColourInput(session, paste0("s6_col_sfd_", k), value = input$s6_default_col_sfd)
      updateColourInput(session, paste0("s6_col_com_", k), value = input$s6_default_col_com)
      updateColourInput(session, paste0("s6_col_pen_", k), value = input$s6_default_col_pen)
      updateColourInput(session, paste0("s6_col_cred_", k), value = input$s6_default_col_cred)
    }
  })
  
  observe({
    req(rv$modifier_plots)
    keys <- names(rv$modifier_plots)
    bg_choice <- input$s6_default_bg %||% "white"
    bg_val <- if (is_transparent_bg(bg_choice)) "transparent" else "white"
    lapply(keys, function(key) {
      local({
        k <- key
        output[[paste0("s6_plot_frame_", k)]] <- renderUI({
          h_in <- (input[[paste0("s6_h_", k)]] %||% input$s6_default_h) %||% 6
          plotOutput(session$ns(paste0("s6_plot_", k)), height = paste0(round(h_in * 96), "px"))
        })
        output[[paste0("s6_plot_", k)]] <- renderPlot({
          # Preview without overrides (matches old behaviour)
          rv$modifier_plots[[k]]
        },
        width = function() ((input[[paste0("s6_w_", k)]] %||% input$s6_default_w) %||% 10) * 96,
        height = function() ((input[[paste0("s6_h_", k)]] %||% input$s6_default_h) %||% 6) * 96,
        res = 96,
        bg = bg_val)
      })
    })
  })
  
  ## =========================================================================
  ## ADD TO CART — shared handler for stage 4 / 5 / 6
  ## =========================================================================
  
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
                         defaults <- list(
                           axis_text = input$s5_default_axis_text %||% 12,
                           axis_title = input$s5_default_axis_title %||% 14,
                           plot_title = input$s5_default_plot_title %||% 16,
                           strip_text = input$s5_default_strip_text %||% 12,
                           legend_text = input$s5_default_legend_text %||% 10,
                           legend_title = input$s5_default_legend_title %||% 10,
                           axis_angle = input$s5_default_axis_angle %||% 90,
                           legend_pos = input$s5_default_legend_pos %||% "top",
                           legend_show = TRUE,
                           col_sfd = input$s5_default_col_sfd %||% "#6FACDE",
                           col_com = input$s5_default_col_com %||% "#F0B323",
                           col_pen = input$s5_default_col_pen %||% "#F0B323",
                           col_cred = input$s5_default_col_cred %||% "#6FACDE",
                           legend_key_size = input$s5_default_legend_key_size %||% 0.8,
                           title_hjust = input$s5_default_title_hjust %||% 0.5,
                           axis_line_col = input$s5_default_axis_line_col %||% "black",
                           panel_fill = input$s5_default_panel_fill %||% "white",
                           bg = input$s5_default_bg %||% "white",
                           grid_col = input$s5_default_grid_col %||% "#e9ecf3",
                           panel_spacing = input$s5_default_panel_spacing %||% 3,
                           margin_t = input$s5_default_margin_t %||% 30,
                           margin_r = input$s5_default_margin_r %||% 10,
                           margin_b = input$s5_default_margin_b %||% 30,
                           margin_l = input$s5_default_margin_l %||% 10,
                           border_col = input$s5_default_border_col %||% "black",
                           border_lwd = input$s5_default_border_lwd %||% 0.5
                         )
                         apply_plot_overrides(p, input, "s5", key, defaults, is_credit = FALSE)
                       },
                       "s6" = {
                         p <- rv$modifier_plots[[key]]
                         defaults <- list(
                           axis_text = input$s6_default_axis_text %||% 12,
                           axis_title = input$s6_default_axis_title %||% 14,
                           plot_title = input$s6_default_plot_title %||% 16,
                           strip_text = input$s6_default_strip_text %||% 12,
                           legend_text = input$s6_default_legend_text %||% 10,
                           legend_title = input$s6_default_legend_title %||% 10,
                           axis_angle = input$s6_default_axis_angle %||% 90,
                           legend_pos = input$s6_default_legend_pos %||% "top",
                           legend_show = TRUE,
                           col_sfd = input$s6_default_col_sfd %||% "#6FACDE",
                           col_com = input$s6_default_col_com %||% "#F0B323",
                           col_pen = input$s6_default_col_pen %||% "#F0B323",
                           col_cred = input$s6_default_col_cred %||% "#6FACDE",
                           legend_key_size = input$s6_default_legend_key_size %||% 0.8,
                           title_hjust = input$s6_default_title_hjust %||% 0.5,
                           axis_line_col = input$s6_default_axis_line_col %||% "black",
                           panel_fill = input$s6_default_panel_fill %||% "white",
                           bg = input$s6_default_bg %||% "white",
                           grid_col = input$s6_default_grid_col %||% "#e9ecf3",
                           panel_spacing = input$s6_default_panel_spacing %||% 3,
                           margin_t = input$s6_default_margin_t %||% 30,
                           margin_r = input$s6_default_margin_r %||% 10,
                           margin_b = input$s6_default_margin_b %||% 30,
                           margin_l = input$s6_default_margin_l %||% 10,
                           border_col = input$s6_default_border_col %||% "black",
                           border_lwd = input$s6_default_border_lwd %||% 0.5
                         )
                         apply_plot_overrides(p, input, "s6", key, defaults, is_credit = FALSE)
                       }
    )
    req(plot_obj)
    
    # Capture dimensions for cart rendering
    if (prefix == "s4") {
      w <- input$s4_w %||% 9
      h <- input$s4_h %||% 6
      dpi <- input$s4_dpi %||% 150
    } else if (prefix == "s5") {
      w <- (input[[paste0("s5_w_", key)]] %||% input$s5_default_w) %||% 9
      h <- (input[[paste0("s5_h_", key)]] %||% input$s5_default_h) %||% 5
      dpi <- input$s5_default_dpi %||% 150
    } else if (prefix == "s6") {
      w <- (input[[paste0("s6_w_", key)]] %||% input$s6_default_w) %||% 10
      h <- (input[[paste0("s6_h_", key)]] %||% input$s6_default_h) %||% 6
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
  
}