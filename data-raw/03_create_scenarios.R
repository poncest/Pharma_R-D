# ==============================================================================
# Create Preset Scenarios
# Author: Steven Ponce
# Purpose: Generate scenario configuration files for the Shiny app
# 
# ⚠️ RUN THIS ONCE AFTER test_simulation.R PASSES
# ==============================================================================

library(dplyr)
library(readr)
library(here)
library(purrr)

cat("\n📋 Creating Preset Scenario Configurations...\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# --- STEP 1: Source Scenario Builder ------------------------------------------

cat("🔧 Loading scenario builder functions...\n")
source(here("R/scenario_builder.R"))
cat("✅ Functions loaded\n\n")

# --- STEP 2: Verify Files Were Created ----------------------------------------

cat("🔍 Verifying scenario files were created...\n\n")

expected_files <- c(
  "data/scenarios/preset_scenarios.rds",
  "data/scenarios/preset_scenarios.csv",
  "data/scenarios/tradeoff_definitions.rds",
  "data/scenarios/therapeutic_recommendations.rds",
  "data/scenarios/portfolio_constraints.rds"
)

files_exist <- file.exists(here(expected_files))

if (all(files_exist)) {
  cat("✅ All scenario files created successfully:\n")
  for (f in expected_files) {
    cat(sprintf("   - %s\n", f))
  }
  cat("\n")
} else {
  cat("❌ Some files missing. Re-run scenario_builder.R\n\n")
  stop("Scenario generation failed")
}

# --- STEP 3: Preview Preset Scenarios -----------------------------------------

cat("📊 Preview: Preset Scenarios\n\n")

preset_scenarios <- readRDS(here("data/scenarios/preset_scenarios.rds"))
print(preset_scenarios %>% select(scenario_name, phase1_count, phase2_count, phase3_count, description))
cat("\n")

# --- STEP 4: Preview Therapeutic Recommendations ------------------------------

cat("📊 Preview: Therapeutic Area Recommendations\n\n")

therapeutic_recs <- readRDS(here("data/scenarios/therapeutic_recommendations.rds"))

cat("Oncology (Low Success):\n")
cat(sprintf("  Allocation: %d Ph1, %d Ph2, %d Ph3\n", 
            therapeutic_recs$oncology$allocation["phase1"],
            therapeutic_recs$oncology$allocation["phase2"],
            therapeutic_recs$oncology$allocation["phase3"]))
cat(sprintf("  Rationale: %s\n\n", therapeutic_recs$oncology$rationale))

cat("Vaccines (High Success):\n")
cat(sprintf("  Allocation: %d Ph1, %d Ph2, %d Ph3\n",
            therapeutic_recs$vaccines$allocation["phase1"],
            therapeutic_recs$vaccines$allocation["phase2"],
            therapeutic_recs$vaccines$allocation["phase3"]))
cat(sprintf("  Rationale: %s\n\n", therapeutic_recs$vaccines$rationale))

# --- STEP 5: Create Example Comparison ----------------------------------------

cat("🧮 Example: Calculate Outcomes for All Preset Scenarios\n\n")

# Load parameters
phase_transitions <- readRDS(here("data/processed/phase_transitions.rds"))
trial_durations <- readRDS(here("data/processed/trial_durations.rds"))
cost_estimates <- readRDS(here("data/processed/cost_estimates.rds"))

# Load simulation functions
source(here("R/simulation_functions.R"))

# Calculate outcomes for each preset scenario
scenario_outcomes <- pmap_dfr(preset_scenarios, function(
    scenario_name, phase1_count, phase2_count, phase3_count, ...
) {
  allocation <- c(phase1 = phase1_count, phase2 = phase2_count, phase3 = phase3_count)
  
  outcomes <- calculate_portfolio_outcomes(
    portfolio_allocation = allocation,
    therapeutic_area = "Overall",
    cost_scenario = "Base",
    phase_transitions = phase_transitions,
    trial_durations = trial_durations,
    cost_estimates = cost_estimates
  )
  
  tibble(
    scenario = scenario_name,
    expected_approvals = outcomes$expected_approvals,
    total_cost = outcomes$total_cost_millions,
    cost_per_approval = outcomes$cost_per_approval_millions,
    timeline = outcomes$avg_timeline_years,
    risk_adj_return = calculate_risk_adjusted_return(outcomes)
  )
})

print(scenario_outcomes)
cat("\n")

# Save comparison table
write_csv(scenario_outcomes, here("data/scenarios/preset_scenario_outcomes.csv"))
cat("✅ Saved preset_scenario_outcomes.csv\n\n")

# --- STEP 6: Create Quick Reference Guide -------------------------------------

cat("📝 Creating quick reference guide...\n")

quick_ref <- "# Quick Reference: Preset Scenarios

## Scenario Comparison (20 total assets, Overall therapeutic area, Base cost)

| Scenario | Ph1 | Ph2 | Ph3 | Expected Approvals | Total Cost ($M) | Cost/Approval ($M) | Timeline (yrs) |
|----------|-----|-----|-----|-------------------|-----------------|-------------------|----------------|
"

for (i in 1:nrow(scenario_outcomes)) {
  row <- scenario_outcomes[i, ]
  quick_ref <- paste0(quick_ref, sprintf(
    "| %s | %d | %d | %d | %.2f | %.1f | %.1f | %.1f |\n",
    row$scenario,
    preset_scenarios$phase1_count[i],
    preset_scenarios$phase2_count[i],
    preset_scenarios$phase3_count[i],
    row$expected_approvals,
    row$total_cost,
    row$cost_per_approval,
    row$timeline
  ))
}

quick_ref <- paste0(quick_ref, "

## Key Insights

1. **Late-Stage Focus** yields highest expected approvals (", 
                    sprintf("%.2f", max(scenario_outcomes$expected_approvals)), 
                    ") but at highest cost ($", 
                    sprintf("%.1fM", max(scenario_outcomes$total_cost)), ")

2. **Early-Stage Focus** has lowest expected approvals (", 
                    sprintf("%.2f", min(scenario_outcomes$expected_approvals)), 
                    ") but longest timeline (", 
                    sprintf("%.1f years", max(scenario_outcomes$timeline)), ")

3. **Barbell Strategy** (heavy early + late, minimal mid) represents a bet on avoiding Phase 2 attrition

4. **Risk-Adjusted Return** metric balances approvals against cost and time:
   - Higher = better efficiency
   - Best scenario: ", 
                    scenario_outcomes$scenario[which.max(scenario_outcomes$risk_adj_return)], "

## Usage in Shiny App

Users can:
- Select a preset scenario as starting point
- Adjust allocation via sliders
- Compare custom allocation to presets
- See real-time trade-off visualizations
")

writeLines(quick_ref, here("data/scenarios/QUICK_REFERENCE.md"))
cat("✅ Saved QUICK_REFERENCE.md\n\n")

# --- SUMMARY ------------------------------------------------------------------

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("✅ Scenario Configuration Complete!\n\n")

cat("Files created:\n")
cat("  ✓ preset_scenarios.rds/.csv\n")
cat("  ✓ tradeoff_definitions.rds\n")
cat("  ✓ therapeutic_recommendations.rds\n")
cat("  ✓ portfolio_constraints.rds\n")
cat("  ✓ preset_scenario_outcomes.csv\n")
cat("  ✓ QUICK_REFERENCE.md\n\n")

cat("Next steps:\n")
cat("1. Review QUICK_REFERENCE.md to see scenario comparison\n")
cat("2. Begin Shiny UI development (app.R + modules/)\n")
cat("3. Design Executive Brief tab (first impression matters)\n\n")

cat(paste(rep("=", 70), collapse = ""), "\n")