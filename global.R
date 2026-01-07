# ============================================================================ #
# Global Environment Setup - Pharma R&D Strategic Simulator
# ============================================================================ #
# This file runs ONCE when the app starts (not per user session)
# Keep lightweight for fast deployment
# ============================================================================ #

# Load required packages (explicit loading, no tidyverse)
library(shiny)
library(shiny.semantic)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggiraph)
library(reactable)

# Source helper functions
source("R/simulation_functions.R")
source("R/scenario_builder.R")
source("R/setup_runtime.R", local = FALSE)  # local = FALSE makes it global


# Source UI modules
source("modules/mod_executive_brief.R")
source("modules/mod_portfolio_builder.R")
source("modules/mod_tradeoff_explorer.R")
source("modules/mod_methods.R")

# ============================================================================ #
# Load Data Files (Load once, shared across sessions)
# ============================================================================ #

# Preset scenarios (for Executive Brief and Trade-off Explorer)
preset_scenarios <- readRDS("data/scenarios/preset_scenarios.rds")

# Parameter tables (for Methods tab and calculations)
phase_transitions <- readRDS("data/processed/phase_transitions.rds")
trial_durations <- readRDS("data/processed/trial_durations.rds")
cost_estimates <- readRDS("data/processed/cost_estimates.rds")
therapeutic_areas <- readRDS("data/processed/therapeutic_areas.rds")

# ============================================================================ #
# Constants and Helper Values
# ============================================================================ #

# Strategic objectives list (used by Executive Brief)
STRATEGIC_OBJECTIVES <- c(
  "Maximize Approvals" = "max_approvals",
  "Predictability" = "predictability",
  "Maximize Learning" = "max_learning",
  "Speed to Market" = "speed",
  "Capital Efficiency" = "efficiency"
)

# Therapeutic area options (for dropdowns)
THERAPEUTIC_AREAS <- c(
  "Overall" = "Overall",
  "Oncology" = "Oncology",
  "Vaccines" = "Vaccines",
  "Neurology" = "Neurology",
  "Rare Disease" = "Rare_Disease"
)

# Cost scenario options
COST_SCENARIOS <- c(
  "Conservative" = "Conservative",
  "Base" = "Base",
  "Aggressive" = "Aggressive"
)

# Brand colors (for consistent theming)
BRAND_COLORS <- list(
  primary_blue = "#0078D4",
  primary_dark = "#004578",
  primary_light = "#50A0D8",
  success_green = "#107C10",
  warning_amber = "#CA5010",
  neutral_gray = "#323130",
  light_gray = "#605E5C"
)

# ============================================================================ #
# Environment Check (Optional - useful for debugging)
# ============================================================================ #

# Verify critical functions loaded
if (!exists("calculate_portfolio_outcomes")) {
  warning("simulation_functions.R may not have loaded correctly")
}

if (!exists("get_preset_scenarios")) {
  warning("scenario_builder.R may not have loaded correctly")
}

# Verify data loaded
if (!exists("preset_scenarios")) {
  warning("preset_scenarios.rds did not load - check data/scenarios/ directory")
}

# Print confirmation (only shows in local development)
message("✓ Global environment initialized successfully")
message("✓ Loaded ", nrow(phase_transitions), " phase transition records")
message("✓ Loaded ", length(preset_scenarios), " preset scenarios")
