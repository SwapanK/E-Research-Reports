## =============================================================================
## PLOTTING FUNCTION MODULE  (module/plotting_function.R)
## =============================================================================
## One general-purpose function used by BOTH the Vulnerability page and the
## Secondary Modifier page. Given a module name and optional filters, it
## returns a list containing a ggplot object and an auto-generated
## commentary string:
##
##   list(module = ..., plot = <ggplot>, commentary = "<text>", timestamp = ...)
##
## Because both pages call this SAME function, plot styling and the
## plot/commentary card layout stay perfectly consistent across the app.
## Swap in real data/queries inside each branch below when ready — the
## return contract (list with $plot and $commentary) is all the UI/server
## code depends on.
## =============================================================================

library(ggplot2)

## Null-coalescing helper
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || (is.character(a) && !nzchar(a))) b else a
}

## -----------------------------------------------------------------------
## THEME COLORS — kept aligned with the gradient palette in www/styles.css
## (#667EEA -> #764BA2 -> #F093FB, blue accent #3B82F6)
## -----------------------------------------------------------------------
VULSEN_PALETTE <- list(
  primary   = "#667EEA",
  secondary = "#764BA2",
  accent    = "#F093FB",
  blue      = "#3B82F6",
  text      = "#374151",
  title     = "#4C1D95",
  sub       = "#6B7280"
)

## -----------------------------------------------------------------------
## Shared ggplot theme so every VulSen chart looks consistent
## -----------------------------------------------------------------------
vulsen_plot_theme <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold", size = 17, color = VULSEN_PALETTE$title, margin = margin(b = 4)),
      plot.subtitle    = element_text(size = 12, color = VULSEN_PALETTE$sub, margin = margin(b = 12)),
      axis.title       = element_text(face = "bold", color = VULSEN_PALETTE$text, size = 11),
      axis.text        = element_text(color = VULSEN_PALETTE$sub),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#E5E7EB"),
      legend.position  = "none",
      plot.background  = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA)
    )
}

## =============================================================================
## MAIN FUNCTION
## =============================================================================
generate_plot_commentary <- function(
    module = c("vulnerability", "secmod"),
    lob    = NULL,
    state  = NULL,
    ...
) {

  module <- match.arg(module)

  if (module == "vulnerability") {

    ## ---------------------------------------------------------------------
    ## VULNERABILITY CURVE  (damage ratio vs hazard intensity)
    ## ---------------------------------------------------------------------

    x <- seq(0, 10, by = 0.2)
    y <- 1 - exp(-0.35 * x)
    y <- pmin(pmax(y + rnorm(length(x), 0, 0.015), 0), 1)

    df <- data.frame(Intensity = x, DamageRatio = y)

    slope   <- diff(df$DamageRatio) / diff(df$Intensity)
    peak_ix <- df$Intensity[which.max(slope)]
    max_dr  <- round(max(df$DamageRatio) * 100, 1)

    plot_obj <- ggplot(df, aes(Intensity, DamageRatio)) +
      geom_area(fill = VULSEN_PALETTE$primary, alpha = 0.12) +
      geom_line(color = VULSEN_PALETTE$primary, linewidth = 1.3) +
      geom_point(color = VULSEN_PALETTE$secondary, size = 1.6, alpha = 0.75) +
      scale_y_continuous(labels = function(v) paste0(round(v * 100), "%")) +
      labs(
        title    = "Vulnerability Curve",
        subtitle = paste0("Line of Business: ", lob %||% "All", "  |  State: ", state %||% "All States"),
        x = "Hazard Intensity",
        y = "Mean Damage Ratio"
      ) +
      vulsen_plot_theme()

    commentary <- paste0(
      "The vulnerability curve rises steeply through low-to-moderate hazard intensities and levels off near ",
      max_dr, "% damage ratio at the highest intensities modelled. ",
      "The fastest rate of damage accumulation occurs around an intensity of ", round(peak_ix, 1), ", ",
      "which marks the critical threshold for this scope (Line of Business: ", lob %||% "All",
      ", State: ", state %||% "All States", "). ",
      "Exposure concentrated near or beyond this threshold is most sensitive to further increases in hazard ",
      "severity, and may warrant closer review of mitigation credits or deductible structure."
    )

  } else {

    ## ---------------------------------------------------------------------
    ## SECONDARY MODIFIER IMPACT  (modifier factor by rating characteristic)
    ## ---------------------------------------------------------------------

    categories <- c("Roof Age", "Wind Mitigation", "Distance to Coast", "Construction Type", "Year Built")

    seed_val <- sum(utf8ToInt(paste0(lob %||% "ALL", state %||% "ALL"))) %% 9999
    set.seed(seed_val)
    factors <- round(runif(length(categories), 0.85, 1.35), 2)

    df <- data.frame(Category = categories, Modifier = factors)
    df <- df[order(-df$Modifier), ]
    df$Category <- factor(df$Category, levels = df$Category)

    top_cat <- as.character(df$Category[1])
    top_val <- df$Modifier[1]
    low_cat <- as.character(df$Category[nrow(df)])
    low_val <- df$Modifier[nrow(df)]

    plot_obj <- ggplot(df, aes(Category, Modifier, fill = Modifier)) +
      geom_col(width = 0.62) +
      geom_text(aes(label = sprintf("%.2f", Modifier)), vjust = -0.5, fontface = "bold",
                color = VULSEN_PALETTE$text, size = 4) +
      scale_fill_gradient(low = VULSEN_PALETTE$blue, high = VULSEN_PALETTE$secondary) +
      coord_cartesian(ylim = c(0, max(df$Modifier) * 1.18)) +
      labs(
        title    = "Secondary Modifier Impact",
        subtitle = paste0("Line of Business: ", lob %||% "All", "  |  State: ", state %||% "All States"),
        x = NULL,
        y = "Modifier Factor"
      ) +
      vulsen_plot_theme() +
      theme(axis.text.x = element_text(angle = 18, hjust = 1))

    commentary <- paste0(
      "Among the secondary modifiers reviewed, '", top_cat, "' applies the largest adjustment at a factor of ",
      sprintf("%.2f", top_val), ", while '", low_cat, "' contributes the smallest at ",
      sprintf("%.2f", low_val), ". ",
      "This spread (Line of Business: ", lob %||% "All", ", State: ", state %||% "All States",
      ") suggests underwriting and rating decisions are most sensitive to '", top_cat,
      "', and refinements to that characteristic's capture or validation are likely to have the greatest ",
      "impact on rate accuracy."
    )
  }

  list(
    module     = if (module == "vulnerability") "Vulnerability" else "Secondary Modifier",
    plot       = plot_obj,
    commentary = commentary,
    timestamp  = Sys.time()
  )
}
