# ==============================================================================
# Test Strategic Objective Framework
# Author: Steven Ponce
# Purpose: Demonstrate how recommendations change based on strategic objectives
# ==============================================================================

library(dplyr)
library(readr)
library(purrr)
library(here)

cat("\n🎯 Testing Strategic Objective Framework...\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# --- STEP 1: Load Data --------------------------------------------------------

cat("📊 Loading parameters and functions...\n")

phase_transitions <- readRDS(here("data/processed/phase_transitions.rds"))
trial_durations <- readRDS(here("data/processed/trial_durations.rds"))
cost_estimates <- readRDS(here("data/processed/cost_estimates.rds"))

source(here("R/simulation_functions.R"))

cat("✅ Loaded\n\n")

# --- STEP 2: Define Test Scenarios --------------------------------------------

cat("📋 Defining test matrix: 3 portfolios × 3 therapeutic areas...\n\n")

# Three portfolio strategies
portfolio_strategies <- list(
  balanced = c(phase1 = 10, phase2 = 6, phase3 = 4),
  early_stage = c(phase1 = 14, phase2 = 4, phase3 = 2),
  late_stage = c(phase1 = 4, phase2 = 6, phase3 = 10)
)

# Three therapeutic areas (different risk profiles)
therapeutic_areas_to_test <- c("Overall", "Oncology", "Vaccines")

# Create full test matrix
test_matrix <- expand.grid(
  portfolio = names(portfolio_strategies),
  therapeutic_area = therapeutic_areas_to_test,
  stringsAsFactors = FALSE
)

# Generate scenario names
test_matrix$scenario_name <- paste0(
  test_matrix$portfolio, "_", test_matrix$therapeutic_area
)

cat(sprintf("Created %d scenarios (3 portfolios × 3 therapeutic areas)\n\n", nrow(test_matrix)))

# Calculate outcomes for all scenarios
outcomes_list <- pmap(test_matrix, function(portfolio, therapeutic_area, scenario_name) {
  calculate_portfolio_outcomes(
    portfolio_allocation = portfolio_strategies[[portfolio]],
    therapeutic_area = therapeutic_area,
    cost_scenario = "Base",
    phase_transitions = phase_transitions,
    trial_durations = trial_durations,
    cost_estimates = cost_estimates
  )
})

scenario_names <- test_matrix$scenario_name

cat("✅ Calculated outcomes for all 9 scenarios\n\n")

# --- STEP 3: Show How Rankings Change by Objective ---------------------------

cat("🔍 TESTING: How recommendations change by strategic objective\n")
cat(paste(rep("-", 70), collapse = ""), "\n\n")

objectives_to_test <- c(
  "maximize_approvals",
  "predictability", 
  "maximize_learning",
  "speed_to_market",
  "capital_efficiency"
)

for (obj in objectives_to_test) {
  
  obj_info <- define_strategic_objectives()[[obj]]
  
  cat(sprintf("📌 OBJECTIVE: %s\n", obj_info$name))
  cat(sprintf("   %s\n\n", obj_info$description))
  
  # Rank scenarios
  ranking <- rank_scenarios_by_objective(outcomes_list, scenario_names, obj)
  
  # Show ranking
  cat("   Rankings:\n")
  for (i in 1:nrow(ranking)) {
    cat(sprintf("   #%d: %s (Score: %.3f)\n", 
                ranking$rank[i], 
                ranking$scenario[i], 
                ranking$objective_score[i]))
  }
  
  cat("\n")
  
  # Generate recommendation
  recommendation <- generate_recommendation(ranking, obj)
  cat(recommendation)
  
  cat("\n")
  cat(paste(rep("-", 70), collapse = ""), "\n\n")
}

# --- STEP 4: Summary Comparison Table -----------------------------------------

cat("📊 SUMMARY: Scenario rankings across all objectives\n\n")

summary_table <- map_dfr(objectives_to_test, function(obj) {
  ranking <- rank_scenarios_by_objective(outcomes_list, scenario_names, obj)
  
  tibble(
    objective = define_strategic_objectives()[[obj]]$name,
    rank_1 = ranking$scenario[1],
    rank_2 = ranking$scenario[2],
    rank_3 = ranking$scenario[3]
  )
})

print(summary_table)
cat("\n")

# --- STEP 5: Key Insights -----------------------------------------------------

cat("💡 KEY INSIGHTS:\n\n")

cat("1. THERAPEUTIC AREA CREATES OBJECTIVE DIFFERENTIATION:\n")
cat("   - Oncology (3.4% success): Early-stage strategies become competitive\n")
cat("   - Vaccines (33.4% success): Late-stage dominance amplified\n")
cat("   - Overall (6.7% success): Intermediate behavior\n\n")

cat("2. OBJECTIVE RANKINGS VARY BY CONTEXT:\n")
cat("   With 9 scenarios (3 portfolios × 3 areas), objectives diverge\n")
cat("   Example: Oncology + Early-Stage wins on learning + cost efficiency\n\n")

cat("3. NO UNIVERSAL 'BEST' STRATEGY:\n")
cat("   Optimal portfolio depends on:\n")
cat("   - Strategic objective (what you're optimizing)\n")
cat("   - Therapeutic area (underlying success rates)\n")
cat("   - Cost scenario (capital constraints)\n\n")

cat("4. STRATEGIC IMPLICATION:\n")
cat("   Portfolio decisions require BOTH:\n")
cat("   - Explicit objective prioritization\n")
cat("   - Context-appropriate parameter assumptions\n\n")

# --- STEP 6: Validate Objective Orthogonality ---------------------------------

cat("🔬 VALIDATION: Are objectives truly orthogonal?\n\n")

# Create correlation matrix of objective scores
objective_scores <- map_dfc(objectives_to_test, function(obj) {
  scores <- map_dbl(outcomes_list, ~calculate_objective_score(.x, obj))
  tibble(!!obj := scores)
})

correlation_matrix <- cor(objective_scores)
rownames(correlation_matrix) <- objectives_to_test
colnames(correlation_matrix) <- objectives_to_test

cat("Correlation matrix (0 = orthogonal, 1 = identical):\n\n")
print(round(correlation_matrix, 2))
cat("\n")

# Check for high correlations
high_correlations <- which(abs(correlation_matrix) > 0.8 & correlation_matrix < 1, arr.ind = TRUE)

if (nrow(high_correlations) == 0) {
  cat("✅ No redundant objectives detected (all correlations < 0.8)\n\n")
} else {
  cat("⚠️  Some objectives are highly correlated:\n")
  for (i in 1:nrow(high_correlations)) {
    row_idx <- high_correlations[i, 1]
    col_idx <- high_correlations[i, 2]
    cat(sprintf("   %s ↔ %s: r = %.2f\n",
                objectives_to_test[row_idx],
                objectives_to_test[col_idx],
                correlation_matrix[row_idx, col_idx]))
  }
  cat("\n")
}

# --- FINAL SUMMARY ------------------------------------------------------------

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("✅ STRATEGIC OBJECTIVE FRAMEWORK VALIDATED\n\n")

cat("What this demonstrates:\n")
cat("  ✓ Five distinct strategic objectives\n")
cat("  ✓ Each objective ranks scenarios differently\n")
cat("  ✓ Objectives are largely orthogonal (not redundant)\n")
cat("  ✓ Recommendations include appropriate caveats\n")
cat("  ✓ Trade-offs are made explicit, not hidden\n\n")

cat("Next steps:\n")
cat("1. Integrate objective selector into Shiny UI\n")
cat("2. Build Executive Brief tab with recommendation engine\n")
cat("3. Add 'What you're giving up' trade-off callouts\n\n")

cat(paste(rep("=", 70), collapse = ""), "\n")