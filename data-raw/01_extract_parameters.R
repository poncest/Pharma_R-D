# ==============================================================================
# Extract Clinical Trial Parameters from Literature
# Author: Steven Ponce
# Purpose: Manually build parameter tables from published sources
# 
# ⚠️ RUN MANUALLY — NOT SOURCED IN APP
# ==============================================================================

library(dplyr)
library(readr)
library(here)

cat("📊 Extracting clinical trial parameters from literature...

")

# --- Phase Transition Probabilities (Norstella 2024) --------------------------

cat("Building phase_transitions.csv...
")

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
cat("✅ phase_transitions saved

")

# --- Trial Durations (Wong et al. 2019) ---------------------------------------

cat("Building trial_durations.csv...
")

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
cat("✅ trial_durations saved

")

# --- R&D Cost Estimates (JAMA 2024) -------------------------------------------

cat("Building cost_estimates.csv...
")

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
cat("✅ cost_estimates saved

")

# --- Therapeutic Area Profiles ------------------------------------------------

cat("Building therapeutic_areas.csv...
")

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
cat("✅ therapeutic_areas saved

")

# --- SUMMARY ------------------------------------------------------------------

cat(paste(rep("=", 70), collapse = ""), "
")
cat("✅ Parameter extraction complete!

")

cat("Files created in data/processed/:
")
cat("  - phase_transitions.csv/.rds
")
cat("  - trial_durations.csv/.rds
")
cat("  - cost_estimates.csv/.rds
")
cat("  - therapeutic_areas.csv/.rds

")

cat("Next step: Inspect CSV files to verify accuracy
")
cat(paste(rep("=", 70), collapse = ""), "
")

