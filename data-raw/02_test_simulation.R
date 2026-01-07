# ==============================================================================
# Test Simulation Functions
# Author: Steven Ponce
# Purpose: Validate that simulation logic produces sensible results
# 
# ⚠️ RUN THIS MANUALLY TO VERIFY EVERYTHING WORKS
# ==============================================================================

library(dplyr)
library(readr)
library(here)

cat("\n🧪 Testing Pharma R&D Simulation Functions...\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# --- STEP 1: Load Parameters --------------------------------------------------

cat("📊 Loading parameter tables...\n")

phase_transitions <- readRDS(here("data/processed/phase_transitions.rds"))
trial_durations <- readRDS(here("data/processed/trial_durations.rds"))
cost_estimates <- readRDS(here("data/processed/cost_estimates.rds"))
therapeutic_areas <- readRDS(here("data/processed/therapeutic_areas.rds"))

cat("✅ Parameters loaded\n")
cat(sprintf("   - %d phase transitions\n", nrow(phase_transitions)))
cat(sprintf("   - %d duration estimates\n", nrow(trial_durations)))
cat(sprintf("   - %d cost scenarios\n", nrow(cost_estimates)))
cat(sprintf("   - %d therapeutic areas\n\n", nrow(therapeutic_areas)))

# --- STEP 2: Load Simulation Functions ----------------------------------------

cat("🔧 Loading simulation functions...\n")
source(here("R/simulation_functions.R"))
cat("✅ Functions loaded\n\n")

# --- STEP 3: Test Basic Simulation --------------------------------------------

cat("🧮 Test 1: Calculate outcomes for balanced portfolio...\n\n")

# Balanced portfolio: 10 Phase 1, 6 Phase 2, 4 Phase 3
balanced_allocation <- c(phase1 = 10, phase2 = 6, phase3 = 4)

balanced_outcomes <- calculate_portfolio_outcomes(
  portfolio_allocation = balanced_allocation,
  therapeutic_area = "Overall",
  cost_scenario = "Base",
  phase_transitions = phase_transitions,
  trial_durations = trial_durations,
  cost_estimates = cost_estimates
)

cat("Portfolio: 10 Ph1, 6 Ph2, 4 Ph3\n")
cat(sprintf("Expected Approvals: %.2f\n", balanced_outcomes$expected_approvals))
cat(sprintf("Total Cost: $%.1fM\n", balanced_outcomes$total_cost_millions))
cat(sprintf("Cost per Approval: $%.1fM\n", balanced_outcomes$cost_per_approval_millions))
cat(sprintf("Avg Timeline: %.1f years\n\n", balanced_outcomes$avg_timeline_years))

# --- STEP 4: Compare Scenarios -------------------------------------------------

cat("🧮 Test 2: Compare early-stage vs late-stage focus...\n\n")

early_stage_allocation <- c(phase1 = 14, phase2 = 4, phase3 = 2)
late_stage_allocation <- c(phase1 = 4, phase2 = 6, phase3 = 10)

early_outcomes <- calculate_portfolio_outcomes(
  portfolio_allocation = early_stage_allocation,
  therapeutic_area = "Overall",
  cost_scenario = "Base",
  phase_transitions = phase_transitions,
  trial_durations = trial_durations,
  cost_estimates = cost_estimates
)

late_outcomes <- calculate_portfolio_outcomes(
  portfolio_allocation = late_stage_allocation,
  therapeutic_area = "Overall",
  cost_scenario = "Base",
  phase_transitions = phase_transitions,
  trial_durations = trial_durations,
  cost_estimates = cost_estimates
)

comparison <- compare_scenarios(early_outcomes, late_outcomes)
print(comparison)
cat("\n")

# --- STEP 5: Test Risk-Adjusted Return ----------------------------------------

cat("🧮 Test 3: Calculate risk-adjusted returns...\n\n")

balanced_rar <- calculate_risk_adjusted_return(balanced_outcomes)
early_rar <- calculate_risk_adjusted_return(early_outcomes)
late_rar <- calculate_risk_adjusted_return(late_outcomes)

cat(sprintf("Balanced Portfolio RAR: %.3f\n", balanced_rar))
cat(sprintf("Early-Stage Focus RAR: %.3f\n", early_rar))
cat(sprintf("Late-Stage Focus RAR: %.3f\n\n", late_rar))

# --- STEP 6: Test Oncology vs Vaccines -----------------------------------------

cat("🧮 Test 4: Compare therapeutic areas (Oncology vs Vaccines)...\n\n")

oncology_outcomes <- calculate_portfolio_outcomes(
  portfolio_allocation = balanced_allocation,
  therapeutic_area = "Oncology",
  cost_scenario = "Base",
  phase_transitions = phase_transitions,
  trial_durations = trial_durations,
  cost_estimates = cost_estimates
)

vaccine_outcomes <- calculate_portfolio_outcomes(
  portfolio_allocation = balanced_allocation,
  therapeutic_area = "Vaccines",
  cost_scenario = "Base",
  phase_transitions = phase_transitions,
  trial_durations = trial_durations,
  cost_estimates = cost_estimates
)

cat("Same portfolio (10/6/4), different therapeutic areas:\n\n")
cat(sprintf("Oncology - Expected Approvals: %.2f\n", oncology_outcomes$expected_approvals))
cat(sprintf("Vaccines - Expected Approvals: %.2f\n\n", vaccine_outcomes$expected_approvals))

cat(sprintf("Oncology - Cost per Approval: $%.1fM\n", oncology_outcomes$cost_per_approval_millions))
cat(sprintf("Vaccines - Cost per Approval: $%.1fM\n\n", vaccine_outcomes$cost_per_approval_millions))

# --- STEP 7: Verify Attrition Logic --------------------------------------------

cat("🧮 Test 5: Verify attrition calculations...\n\n")

cat("Balanced portfolio attrition:\n")
print(balanced_outcomes$attrition_table)
cat("\n")

# --- STEP 8: Sanity Checks -----------------------------------------------------

cat("🔍 Sanity Checks:\n\n")

# Check 1: Late-stage should have higher expected approvals (same total assets)
check1 <- late_outcomes$expected_approvals > early_outcomes$expected_approvals
cat(sprintf("✓ Late-stage has more approvals than early-stage: %s\n", check1))

# Check 2: Late-stage should cost more (Phase 3 is expensive)
check2 <- late_outcomes$total_cost_millions > early_outcomes$total_cost_millions
cat(sprintf("✓ Late-stage costs more than early-stage: %s\n", check2))

# Check 3: Early-stage should take longer (more phases to complete)
check3 <- early_outcomes$avg_timeline_years > late_outcomes$avg_timeline_years
cat(sprintf("✓ Early-stage timeline longer than late-stage: %s\n", check3))

# Check 4: Vaccines should outperform Oncology (higher success rates)
check4 <- vaccine_outcomes$expected_approvals > oncology_outcomes$expected_approvals
cat(sprintf("✓ Vaccines yield more approvals than Oncology: %s\n", check4))

# Check 5: Cost per approval should be finite and positive
check5 <- all(!is.na(c(balanced_outcomes$cost_per_approval_millions, 
                       early_outcomes$cost_per_approval_millions,
                       late_outcomes$cost_per_approval_millions))) &&
  all(c(balanced_outcomes$cost_per_approval_millions,
        early_outcomes$cost_per_approval_millions,
        late_outcomes$cost_per_approval_millions) > 0)
cat(sprintf("✓ All cost-per-approval values are valid: %s\n\n", check5))

# --- SUMMARY ------------------------------------------------------------------

all_checks_pass <- all(check1, check2, check3, check4, check5)

cat(paste(rep("=", 70), collapse = ""), "\n")
if (all_checks_pass) {
  cat("✅ ALL TESTS PASSED - Simulation logic is working correctly!\n\n")
  cat("Next steps:\n")
  cat("1. Run data-raw/02_create_scenarios.R to build preset scenarios\n")
  cat("2. Start building Shiny UI modules\n")
  cat("3. Design Executive Brief tab layout\n")
} else {
  cat("❌ SOME TESTS FAILED - Review simulation logic\n\n")
}
cat(paste(rep("=", 70), collapse = ""), "\n")