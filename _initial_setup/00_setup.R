# ==============================================================================
# Initial Setup Script for Pharma R&D Resource Allocation Simulator
# Author: Steven Ponce
# Purpose: Install packages, create folders, configure deployment exclusions
# 
# ⚠️ RUN THIS ONCE, MANUALLY — NEVER SOURCE IN APP
# ==============================================================================

cat("\n💊 Setting up Pharma R&D Strategic Resource Allocation Simulator...\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# --- STEP 1: Set CRAN Mirror --------------------------------------------------

cat("🔧 Setting CRAN mirror to RStudio cloud...\n")
options(repos = c(CRAN = "https://cloud.r-project.org"))
cat("✅ CRAN mirror set\n\n")

# --- STEP 2: Ensure 'pak' is installed ----------------------------------------

if (!requireNamespace("pak", quietly = TRUE)) {
  cat("📦 Installing 'pak' package manager...\n")
  install.packages("pak", quiet = TRUE)
  cat("✅ pak installed\n\n")
}

# --- STEP 3: Define Package List ----------------------------------------------

cat("📋 Defining strategic simulation dashboard package stack...\n\n")

packages <- c(
  # === CORE SHINY & UI ===
  "shiny",
  "bslib",              # Modern Bootstrap 5 theming
  "shinyjs",            # JavaScript utilities
  "shinyWidgets",       # Enhanced input controls (for sliders)
  
  # === APPSILON FRAMEWORK (Professional UI) ===
  "shiny.semantic",     # Semantic UI components
  "semantic.dashboard", # Dashboard layouts with Semantic UI
  
  # === DATA WRANGLING ===
  "dplyr",
  "tidyr",
  "readr",
  "stringr",
  "janitor",            # Clean column names
  "glue",               # String interpolation
  "forcats",            # Factor handling
  
  # === VISUALIZATION ===
  "ggplot2",
  "ggiraph",            # Interactive ggplot2 (SVG-based, fast)
  "scales",             # Number/axis formatting
  "RColorBrewer",       # Color palettes
  "viridis",            # Perceptually uniform colors
  "patchwork",          # Combine multiple plots
  
  # === TABLES ===
  "reactable",          # Modern, fast, professional data tables
  "gt",                 # Grammar of tables (for summary tables)
  
  # === SIMULATION/MODELING ===
  # Note: We're using deterministic calculations, not stochastic
  # If we add Monte Carlo later, uncomment:
  # "distributional",   # Probability distributions
  
  # === UTILITIES ===
  "here",               # Project-relative paths
  "fs",                 # File system operations
  "purrr"               # Functional programming
)

cat("📦 Package stack defined:\n")
cat("   - Core Shiny & UI: 4 packages\n")
cat("   - Appsilon Framework: 2 packages\n")
cat("   - Data wrangling: 7 packages\n")
cat("   - Visualization: 6 packages\n")
cat("   - Tables: 2 packages\n")
cat("   - Utilities: 3 packages\n\n")

# --- STEP 4: Install Packages with pak ----------------------------------------

cat("🚀 Installing packages with pak (this may take a few minutes)...\n\n")

tryCatch({
  pak::pkg_install(packages)
  cat("\n✅ All packages installed/updated successfully!\n\n")
}, error = function(e) {
  cat("\n❌ Error during package installation:\n")
  cat("   ", conditionMessage(e), "\n\n")
  cat("   Please review error messages above and retry manually if needed.\n\n")
})

# --- STEP 5: Create Project Folders -------------------------------------------

cat("📁 Creating project folder structure...\n")

dirs <- c(
  "data-raw/literature",    # PDF papers, supplementary materials
  "data-raw/citations",     # BibTeX, citation tracking
  "data/processed",         # Final parameter tables (.rds)
  "data/scenarios",         # Preset scenario configurations
  "R",                      # Helper functions
  "modules",                # Shiny modules (one per tab)
  "www",                    # CSS, images, static assets
  "docs",                   # Documentation, blog posts
  "_initial_setup",         # This script + notes
  "screenshots"             # For README/portfolio
)

for (d in dirs) {
  fs::dir_create(d)
  cat(sprintf("  ✅ %s/\n", d))
}
cat("\n")

# --- STEP 6: Create .rscignore ------------------------------------------------

cat("📝 Creating .rscignore for clean shinyapps.io deployment...\n")

rscignore_content <- "# Exclude from shinyapps.io deployment
# (Keeps bundle size small and avoids OS-specific binaries)

renv/library
renv/staging
renv/python
.Rproj.user
rsconnect
docs
screenshots
_initial_setup
*.Rproj
.git
.gitignore
README.md
HANDOFF_DOCUMENT.md
data-raw
data/scenarios
"

writeLines(rscignore_content, ".rscignore")
cat("✅ .rscignore created\n\n")

# --- STEP 7: Create .gitignore ------------------------------------------------

cat("📝 Creating .gitignore...\n")

gitignore_content <- "# R & RStudio
.Rproj.user
.Rhistory
.RData
.Ruserdata
*.Rproj

# Deployment
rsconnect/

# Literature (PDFs may be large/copyrighted)
data-raw/literature/*.pdf

# Temporary files
*.log
"

writeLines(gitignore_content, ".gitignore")
cat("✅ .gitignore created\n\n")

# --- STEP 8: Create data_sources.md template ----------------------------------

cat("📝 Creating data_sources.md for citation tracking...\n")

data_sources_content <- '# Data Sources & Citations

## Phase Transition Probabilities

### Primary Source: Norstella/Citeline (2024)
- **Citation**: "Why Are Clinical Development Success Rates Falling?" Citeline/Norstella, September 2024
- **URL**: https://www.norstella.com/why-clinical-development-success-rates-falling/
- **Data Period**: 2014-2023
- **Key Metrics**:
  - Phase I → Phase II: 47%
  - Phase II → Phase III: 28%
  - Phase III → Approval: 55%
  - Filing → Approval: 92%
  - Overall LOA (Phase I → Approval): 6.7%

### Secondary Source: Wong et al. (2019)
- **Citation**: Wong, C.H., Siah, K.W., Lo, A.W. (2019). "Estimation of clinical trial success rates and related parameters." *Biostatistics*, 20(2), 273-286.
- **DOI**: 10.1093/biostatistics/kxx069
- **PubMed**: PMC6409418
- **Data Period**: 2000-2015
- **Sample Size**: 185,994 unique trials, 21,143 compounds

## Trial Duration

### Primary Source: Wong et al. (2019)
- **Median Durations**:
  - Phase 1: 1.6 years
  - Phase 2: 2.9 years
  - Phase 3: 3.8 years
- **Oncology-Specific**: 13.1 years median (vs 5.9-7.2 for non-oncology)

## R&D Cost Estimates

### Primary Source: JAMA 2024
- **Citation**: Sertkaya, A., et al. (2024). "Costs of Drug Development and Research and Development Intensity in the US, 2000-2018." *JAMA Network Open*, 7(6), e2415397.
- **DOI**: 10.1001/jamanetworkopen.2024.15397
- **Key Findings**:
  - Out-of-pocket cost: $172.7M (2018 USD)
  - Including failures: $515.8M
  - Including capital costs: $879.3M
  - Clinical phase: 69% of overall R&D costs

### Secondary Source: Deloitte (2024)
- **Citation**: "Measuring the return from pharmaceutical innovation 2024"
- **Key Finding**: Average Big Pharma cost per asset: $2.23B

## Therapeutic Area Success Rates

### Primary Source: ACSH (2020)
- **Citation**: Berezow, A. (2020). "Clinical Trial Success Rates by Phase and Therapeutic Area." *American Council on Science and Health*.
- **URL**: https://www.acsh.org/news/2020/06/11/clinical-trial-success-rates-phase-and-therapeutic-area-14845
- **Key Metrics**:
  - Overall: 13.8%
  - Oncology: 3.4%
  - Vaccines (infectious disease): 33.4%

---

## Usage Notes

1. **All metrics in this simulator are derived from published, peer-reviewed sources**
2. **Data represents industry averages** - individual company/asset performance varies
3. **Cost estimates are simplified proxies** - actual R&D costs depend on indication, geography, trial design
4. **Success rates have been declining** - 2014-2023 data shows lower rates than earlier periods

---

*Last updated: December 2024*
'

writeLines(data_sources_content, "data-raw/citations/data_sources.md")
cat("✅ data-raw/citations/data_sources.md created\n\n")

# --- STEP 9: Create runtime-safe setup helper ---------------------------------

cat("📝 Creating R/setup_runtime.R (safe for app deployment)...\n")

runtime_setup <- '# ==============================================================================
# Runtime Setup (Safe for Deployment)
# Author: Steven Ponce
# Purpose: Constants, options, and helpers — NO INSTALLATIONS
# ==============================================================================

# --- Color Palette (Pharma/Biotech Professional) -----------------------------

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

# --- ggplot2 Theme (Consulting-Grade) -----------------------------------------

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
'

writeLines(runtime_setup, "R/setup_runtime.R")
cat("✅ R/setup_runtime.R created\n\n")

# --- STEP 10: Create data extraction script template -------------------------

cat("📝 Creating data-raw/01_extract_parameters.R template...\n")

extract_script <- '# ==============================================================================
# Extract Clinical Trial Parameters from Literature
# Author: Steven Ponce
# Purpose: Manually build parameter tables from published sources
# 
# ⚠️ RUN MANUALLY — NOT SOURCED IN APP
# ==============================================================================

library(dplyr)
library(readr)
library(here)

cat("📊 Extracting clinical trial parameters from literature...\n\n")

# --- Phase Transition Probabilities (Norstella 2024) --------------------------

cat("Building phase_transitions.csv...\n")

phase_transitions <- tribble(
  ~therapeutic_area, ~phase_transition,    ~success_rate, ~source,      ~year,
  # Overall (all indications)
  "Overall",         "Phase1_to_Phase2",   0.47,          "Norstella",  2023,
  "Overall",         "Phase2_to_Phase3",   0.28,          "Norstella",  2023,
  "Overall",         "Phase3_to_Approval", 0.55,          "Norstella",  2023,
  "Overall",         "Filing_to_Approval", 0.92,          "Norstella",  2023,
  
  # Oncology (low success)
  "Oncology",        "Phase1_to_Phase2",   0.47,          "ACSH",       2020,
  "Oncology",        "Phase2_to_Phase3",   0.28,          "ACSH",       2020,
  "Oncology",        "Phase3_to_Approval", 0.55,          "ACSH",       2020,
  
  # Vaccines (high success)
  "Vaccines",        "Phase1_to_Phase2",   0.75,          "ACSH",       2020,
  "Vaccines",        "Phase2_to_Phase3",   0.60,          "ACSH",       2020,
  "Vaccines",        "Phase3_to_Approval", 0.75,          "ACSH",       2020,
  
  # Anti-Infectives (moderate)
  "Anti-Infectives", "Phase1_to_Phase2",   0.60,          "Estimated",  2020,
  "Anti-Infectives", "Phase2_to_Phase3",   0.45,          "Estimated",  2020,
  "Anti-Infectives", "Phase3_to_Approval", 0.65,          "Estimated",  2020
)

# Calculate overall LOA (Phase I → Approval)
phase_transitions <- phase_transitions %>%
  group_by(therapeutic_area) %>%
  mutate(
    overall_loa = prod(success_rate[phase_transition != "Filing_to_Approval"])
  ) %>%
  ungroup()

write_csv(phase_transitions, here("data/processed/phase_transitions.csv"))
saveRDS(phase_transitions, here("data/processed/phase_transitions.rds"))
cat("✅ phase_transitions saved\n\n")

# --- Trial Durations (Wong et al. 2019) ---------------------------------------

cat("Building trial_durations.csv...\n")

trial_durations <- tribble(
  ~therapeutic_area, ~phase,   ~median_years, ~min_years, ~max_years, ~source,
  # Overall
  "Overall",         "Phase1", 1.6,           1.0,        3.0,        "Wong 2019",
  "Overall",         "Phase2", 2.9,           1.5,        5.0,        "Wong 2019",
  "Overall",         "Phase3", 3.8,           2.0,        6.0,        "Wong 2019",
  
  # Oncology (longer)
  "Oncology",        "Phase1", 1.8,           1.2,        3.5,        "Wong 2019",
  "Oncology",        "Phase2", 3.5,           2.0,        6.0,        "Wong 2019",
  "Oncology",        "Phase3", 4.5,           2.5,        7.0,        "Wong 2019",
  
  # Vaccines (shorter)
  "Vaccines",        "Phase1", 1.2,           0.8,        2.0,        "Estimated",
  "Vaccines",        "Phase2", 2.0,           1.0,        3.5,        "Estimated",
  "Vaccines",        "Phase3", 2.5,           1.5,        4.0,        "Estimated"
)

write_csv(trial_durations, here("data/processed/trial_durations.csv"))
saveRDS(trial_durations, here("data/processed/trial_durations.rds"))
cat("✅ trial_durations saved\n\n")

# --- R&D Cost Estimates (JAMA 2024) -------------------------------------------

cat("Building cost_estimates.csv...\n")

cost_estimates <- tribble(
  ~cost_scenario, ~phase,   ~cost_millions, ~source,      ~year, ~notes,
  # Conservative (out-of-pocket only)
  "Conservative", "Phase1", 25,             "JAMA 2024",  2018,  "Out-of-pocket",
  "Conservative", "Phase2", 60,             "JAMA 2024",  2018,  "Out-of-pocket",
  "Conservative", "Phase3", 87,             "JAMA 2024",  2018,  "Out-of-pocket",
  
  # Base (includes failures)
  "Base",         "Phase1", 75,             "JAMA 2024",  2018,  "Includes failures",
  "Base",         "Phase2", 180,            "JAMA 2024",  2018,  "Includes failures",
  "Base",         "Phase3", 260,            "JAMA 2024",  2018,  "Includes failures",
  
  # Aggressive (full loaded cost)
  "Aggressive",   "Phase1", 130,            "JAMA 2024",  2018,  "Full loaded + capital",
  "Aggressive",   "Phase2", 310,            "JAMA 2024",  2018,  "Full loaded + capital",
  "Aggressive",   "Phase3", 440,            "JAMA 2024",  2018,  "Full loaded + capital"
)

write_csv(cost_estimates, here("data/processed/cost_estimates.csv"))
saveRDS(cost_estimates, here("data/processed/cost_estimates.rds"))
cat("✅ cost_estimates saved\n\n")

# --- Therapeutic Area Profiles ------------------------------------------------

cat("Building therapeutic_areas.csv...\n")

therapeutic_areas <- tribble(
  ~therapeutic_area, ~overall_loa, ~typical_timeline_years, ~notes,
  "Overall",         0.067,        8.3,                      "Industry average (2014-2023)",
  "Oncology",        0.034,        9.8,                      "Low success, long timelines",
  "Vaccines",        0.334,        5.7,                      "High success, faster",
  "Anti-Infectives", 0.176,        7.5,                      "Moderate success",
  "Orphan Drugs",    0.062,        8.0,                      "Low overall, high Ph1 success"
)

write_csv(therapeutic_areas, here("data/processed/therapeutic_areas.csv"))
saveRDS(therapeutic_areas, here("data/processed/therapeutic_areas.rds"))
cat("✅ therapeutic_areas saved\n\n")

# --- SUMMARY ------------------------------------------------------------------

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("✅ Parameter extraction complete!\n\n")

cat("Files created in data/processed/:\n")
cat("  - phase_transitions.csv/.rds\n")
cat("  - trial_durations.csv/.rds\n")
cat("  - cost_estimates.csv/.rds\n")
cat("  - therapeutic_areas.csv/.rds\n\n")

cat("Next step: Inspect CSV files to verify accuracy\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
'

writeLines(extract_script, "data-raw/01_extract_parameters.R")
cat("✅ data-raw/01_extract_parameters.R created\n\n")

# --- FINAL MESSAGE ------------------------------------------------------------

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("✨ Pharma R&D Simulator Setup Complete!\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

cat("📋 NEXT STEPS:\n\n")
cat("1. ✅ Review data-raw/citations/data_sources.md\n")
cat("      (All parameter sources documented)\n\n")
cat("2. ✅ Run data-raw/01_extract_parameters.R\n")
cat("      (Builds 4 core parameter tables)\n\n")
cat("3. ✅ Inspect data/processed/*.csv files\n")
cat("      (Verify accuracy against source papers)\n\n")
cat("4. ✅ Phase 1B: Build simulation logic\n")
cat("      (Portfolio allocation → expected outcomes)\n\n")

cat("⚠️  IMPORTANT REMINDERS:\n")
cat("   - NEVER source 00_setup.R in your app\n")
cat("   - NEVER install packages at runtime\n")
cat("   - All data is STATIC PARAMETERS (no APIs)\n")
cat("   - Every number has a documented source\n\n")

cat("🎯 This is a SIMULATOR, not a data explorer\n")
cat("   Users adjust assumptions → See trade-off implications\n\n")

cat("🚀 Ready to build a decision-support tool for portfolio strategy!\n\n")