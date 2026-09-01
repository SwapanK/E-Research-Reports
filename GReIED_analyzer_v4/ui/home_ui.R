# =============================================================================
# ui/home_ui.R
# Home landing page - uses .home-hero / .home-card classes from
# www/dashboard_style.css unmodified (same theme as the Vulnerability module).
# =============================================================================

home_ui <- function() {
  tagList(
    div(
      class = "home-scroll",

      # -----------------------------------------------------------------
      # HERO
      # -----------------------------------------------------------------
      div(
        class = "home-hero",
        div(
          class = "home-hero-inner",
          div(
            class = "home-hero-eyebrow",
            icon("shield-halved"), " PROPERTY RESEARCH | CATASTROPHE MODEL DIAGNOSTICS"
          ),
          h1(class = "home-hero-title", "GReIED Analyzer"),
          p(
            class = "home-hero-lede",
            "Explore Average Annual Loss (AAL) and Exceedance Probability (EP) ",
            "output by peril, state, and line of business. Upload your own extracts ",
            "or use the packaged GReIED dataset to review contribution shares, ",
            "state profiles, geographic patterns, and return-period curves."
          ),
          div(
            class = "home-hero-actions",
            actionButton("go_preview", tagList(icon("table"), " Load / Preview Data"),
                         class = "home-btn home-btn--primary"),
            actionButton("go_summary", tagList(icon("chart-pie"), " Jump to Executive Summary"),
                         class = "home-btn home-btn--ghost")
          ),
          div(
            class = "home-hero-stats",
            div(class = "home-stat",
                div(class = "home-stat-num", "4"),
                div(class = "home-stat-label", "Subperils Modeled")),
            div(class = "home-stat-divider"),
            div(class = "home-stat",
                div(class = "home-stat-num", "GU / GR"),
                div(class = "home-stat-label", "Loss Perspectives")),
            div(class = "home-stat-divider"),
            div(class = "home-stat",
                div(class = "home-stat-num", "5"),
                div(class = "home-stat-label", "Analytics Views"))
          )
        )
      ),

      # -----------------------------------------------------------------
      # FEATURE CARDS -> jump straight into the analyzer page + correct subtab
      # -----------------------------------------------------------------
      div(
        class = "home-section",
        div(
          class = "home-section-head",
          div(class = "home-section-eyebrow", "WHERE TO GO"),
          div(class = "home-section-title", "Analytics subtabs"),
          div(class = "home-section-sub",
              "All five views live on one page \u2014 GReIED Analyzer \u2014 with a ",
              "shared filter panel on the left and subtabs on the right.")
        ),

        div(
          class = "home-cards-grid",

          div(
            class = "home-card", onclick = "Shiny.setInputValue('go_preview', Math.random());",
            div(class = "home-card-icon", icon("table")),
            div(class = "home-card-body",
                div(class = "home-card-title", "Data Preview"),
                div(class = "home-card-desc",
                    "Inspect the loaded AAL and EP datasets in searchable, ",
                    "exportable tables before you analyse them.")),
            div(class = "home-card-footer",
                span(class = "home-card-tag", "Preview"),
                icon("arrow-right", class = "home-card-arrow"))
          ),

          div(
            class = "home-card", onclick = "Shiny.setInputValue('go_summary', Math.random());",
            div(class = "home-card-icon", icon("chart-pie")),
            div(class = "home-card-body",
                div(class = "home-card-title", "Executive Summary"),
                div(class = "home-card-desc",
                    "KPI tiles, a contribution donut, and a state-by-state ",
                    "stacked profile for the selected perils.")),
            div(class = "home-card-footer",
                span(class = "home-card-tag", "AAL"),
                icon("arrow-right", class = "home-card-arrow"))
          ),

          div(
            class = "home-card", onclick = "Shiny.setInputValue('go_profile', Math.random());",
            div(class = "home-card-icon", icon("chart-bar")),
            div(class = "home-card-body",
                div(class = "home-card-title", "State Profile"),
                div(class = "home-card-desc",
                    "AAL by peril for the selected state, a contribution table, ",
                    "and Ground-Up vs Gross comparisons.")),
            div(class = "home-card-footer",
                span(class = "home-card-tag", "AAL"),
                icon("arrow-right", class = "home-card-arrow"))
          ),

          div(
            class = "home-card", onclick = "Shiny.setInputValue('go_maps', Math.random());",
            div(class = "home-card-icon", icon("globe")),
            div(class = "home-card-body",
                div(class = "home-card-title", "Geo Analytics"),
                div(class = "home-card-desc",
                    "Choropleth maps of value and subperil contribution share ",
                    "by state, from AAL or EP data.")),
            div(class = "home-card-footer",
                span(class = "home-card-tag", "Maps"),
                icon("arrow-right", class = "home-card-arrow"))
          ),

          div(
            class = "home-card", onclick = "Shiny.setInputValue('go_ep', Math.random());",
            div(class = "home-card-icon", icon("chart-line")),
            div(class = "home-card-body",
                div(class = "home-card-title", "EP Analytics"),
                div(class = "home-card-desc",
                    "Exceedance probability curves, subperil pies, and ",
                    "state contribution profiles at a chosen return period.")),
            div(class = "home-card-footer",
                span(class = "home-card-tag", "EP"),
                icon("arrow-right", class = "home-card-arrow"))
          )
        )
      ),

      # -----------------------------------------------------------------
      # FOOTER
      # -----------------------------------------------------------------
      div(
        class = "home-footer",
        tags$img(src = "logo.png", class = "home-footer-logo",
                 onerror = "this.style.display='none'"),
        div(class = "home-footer-text",
            "GReIED Analyzer | Internal analytical tool | Human review required before stakeholder use")
      )
    )
  )
}
