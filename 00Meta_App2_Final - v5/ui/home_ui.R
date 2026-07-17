# =============================================================================
# HOME UI MODULE
# Landing page: brand, workflow pipeline demo, feature cards
# =============================================================================

library(shiny)

# -----------------------------------------------------------------------------
# Helper: one feature / module card
# -----------------------------------------------------------------------------
home_feature_card <- function(ns, input_id, icon_name, title, desc, tag_label, accent = "blue") {
  tags$button(
    id = ns(input_id),
    class = paste0("home-card home-card--", accent),
    onclick = sprintf("Shiny.setInputValue('%s', Math.random());", ns(input_id)),
    type = "button",
    div(class = "home-card-icon", icon(icon_name)),
    div(class = "home-card-body",
        h3(class = "home-card-title", title),
        p(class = "home-card-desc", desc)
    ),
    div(class = "home-card-footer",
        span(class = "home-card-tag", tag_label),
        span(class = "home-card-arrow", icon("arrow-right"))
    )
  )
}

# -----------------------------------------------------------------------------
# MAIN HOME UI
# -----------------------------------------------------------------------------
homeUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    div(
      class = "single-scroll-panel home-scroll",
      
      # ============================ HERO ============================
      div(
        class = "home-hero",
        div(class = "home-hero-blob home-hero-blob--1"),
        div(class = "home-hero-blob home-hero-blob--2"),
        div(class = "home-hero-grid"),
        
        div(
          class = "home-hero-inner",
          # Logo removed from hero
          div(class = "home-hero-eyebrow", icon("shield-halved"), "MODEL TESTING WORKSPACE"),
          h1(class = "home-hero-title",
             "Vulsens", span(class = "home-hero-title-amp", " & "), "Secmod", br(),
             span(class = "home-hero-title-sub", "Toolkit")),
          p(class = "home-hero-lede",
            "Prepare, extract, analyze and report Vulnerability Sensitivity and Secondary ",
            "Modifier testing across Moody\u2019s RMS and Verisk \u2014 one connected workspace, ",
            "start to finished report."),
          div(
            class = "home-hero-actions",
            tags$button(id = ns("go_prep_hero"), type = "button", class = "home-btn home-btn--primary",
                        onclick = sprintf("Shiny.setInputValue('%s', Math.random());", ns("go_prep_hero")),
                        icon("bolt"), "Start a New Workflow"),
            tags$button(id = ns("go_cart_hero"), type = "button", class = "home-btn home-btn--ghost",
                        onclick = sprintf("Shiny.setInputValue('%s', Math.random());", ns("go_cart_hero")),
                        icon("shopping-cart"), "View Cart", uiOutput(ns("hero_cart_badge"), inline = TRUE))
          ),
          div(
            class = "home-hero-stats",
            div(class = "home-stat", div(class = "home-stat-num", "2"), div(class = "home-stat-label", "Vendor platforms")),
            div(class = "home-stat-divider"),
            div(class = "home-stat", div(class = "home-stat-num", "2"), div(class = "home-stat-label", "Test modules")),
            div(class = "home-stat-divider"),
            div(class = "home-stat", div(class = "home-stat-num", "3"), div(class = "home-stat-label", "Export formats")),
            div(class = "home-stat-divider"),
            div(class = "home-stat", uiOutput(ns("hero_cart_count"), inline = TRUE), div(class = "home-stat-label", "Items in cart"))
          )
        )
      ),
      
      # ========================= FEATURE CARDS =========================
      div(
        class = "home-section",
        div(class = "home-section-head",
            div(class = "home-section-eyebrow", "MODULES"),
            h2(class = "home-section-title", "Everything you need, in one place"),
            p(class = "home-section-sub", "Jump directly into any module from here.")
        ),
        div(
          class = "home-cards-grid",
          home_feature_card(ns, "go_prep_card",
                            "file-import", "Input File Creation",
                            "Configure vendor, module, exposure settings and generate standardized Location & Account files for RMS or Verisk.",
                            "Moody\u2019s \u00b7 Verisk", "blue"),
          home_feature_card(ns, "go_extract_card",
                            "database", "Result Extraction",
                            "Connect to exposure and result databases, load ExposureSet / ResultSet data and export Location, State, County or Region outputs.",
                            "EDM / RDM \u00b7 TSE / TSR", "teal"),
          home_feature_card(ns, "go_secmod_card",
                            "shield-halved", "Secondary Modifier",
                            "Run the full sensitivity workflow, style every chart, and build a curated gallery of results.",
                            "Fully customizable plots", "amber"),
          home_feature_card(ns, "go_cart_card",
                            "shopping-cart", "Cart & Reports",
                            "Collect saved plots and commentary, then export a polished PowerPoint, Word or HTML report.",
                            "PPTX \u00b7 DOCX \u00b7 HTML", "green")
        )
      ),
      
      # ============================ FOOTER ============================
      div(
        class = "home-footer",
        div(class = "home-footer-brand"),
        div(class = "home-footer-text", "Vulsens & Secmod Toolkit \u2014 internal model testing workspace")
      )
    )
  )
}