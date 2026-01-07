# ==============================================================================
# Runtime Setup (Safe for Deployment)
# Author: Steven Ponce
# Purpose: Constants, options, and helpers — NO INSTALLATIONS
# ==============================================================================

# --- Color Palette  -----------------------------

pharma_colors <- list(
  primary   = "#0066CC",  # Science Blue
  secondary = "#00A86B",  # Medical Green
  success   = "#28A745",  # Success Green
  warning   = "#FFC107",  # Caution Gold
  danger    = "#DC3545",  # Risk Red
  dark      = "#2C3E50",  # Dark Slate
  light     = "#F8F9FA",  # Light Gray
  text      = "#212529",  # Dark Text
  early     = "#6C757D",  # Gray (early stage)
  mid       = "#17A2B8",  # Teal (mid stage)
  late      = "#007BFF"   # Blue (late stage)
)

# --- ggplot2 Theme  -----------------------------------------

theme_pharma <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold", 
        size = base_size * 1.3, 
        color = pharma_colors$dark
      ),
      plot.subtitle = ggplot2::element_text(
        color = pharma_colors$text, 
        margin = ggplot2::margin(b = 10)
      ),
      axis.title = ggplot2::element_text(
        face = "bold", 
        color = pharma_colors$text
      ),
      axis.text = ggplot2::element_text(color = pharma_colors$text),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(
        color = "#E1DFDD", 
        linewidth = 0.3
      ),
      legend.position = "top",
      legend.title = ggplot2::element_text(
        face = "bold", 
        size = base_size * 0.9
      ),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}

# --- Number Formatting Helpers ------------------------------------------------

fmt_currency <- function(x, suffix = "M", scale = 1e-6, digits = 1) {
  # Default: millions
  # Usage: fmt_currency(172700000) → "$172.7M"
  paste0(
    "$", 
    format(
      round(x * scale, digits), 
      big.mark = ",", 
      scientific = FALSE
    ), 
    suffix
  )
}

fmt_percent <- function(x, digits = 1) {
  # Usage: fmt_percent(0.47) → "47.0%"
  paste0(round(x * 100, digits), "%")
}

fmt_years <- function(x, digits = 1) {
  # Usage: fmt_years(1.6) → "1.6 years"
  paste0(round(x, digits), ifelse(x == 1, " year", " years"))
}

# --- Shiny Options (Performance & UX) -----------------------------------------

options(
  shiny.maxRequestSize = 10 * 1024^2,  # 10MB upload limit
  spinner.type = 4,                     # Loading spinner style
  spinner.color = pharma_colors$primary
)

