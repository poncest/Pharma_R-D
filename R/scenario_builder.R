# ==============================================================================
# Preset Scenario Configurations
# Author: Steven Ponce
# Purpose: Define realistic R&D portfolio allocation strategies
# ==============================================================================

library(dplyr)
library(here)

# ==============================================================================
# SCENARIO DEFINITIONS
# ==============================================================================

#' Generate Preset Portfolio Scenarios
#' 
#' Each scenario represents a strategic posture toward R&D risk and return
#' 
#' @param total_assets Integer: Total pipeline size (default 20)
#' @return Data frame with scenario definitions
create_preset_scenarios <- function(total_assets = 20) {
  
  scenarios <- tibble(
    scenario_name = c(
      "Balanced Portfolio",
      "Early-Stage Focus (High Risk)",
      "Late-Stage Focus (Low Risk)",
      "Barbell Strategy",
      "Speed-to-Market"
    ),
    
    description = c(
      "Industry-average allocation balancing risk and timeline",
      "Maximum early-stage volume for future optionality",
      "Conservative focus on high-probability late-stage assets",
      "Bimodal: heavy early + late, minimal mid-stage",
      "Prioritizes fastest path to approval (late-stage heavy)"
    ),
    
    phase1_pct = c(0.50, 0.70, 0.20, 0.50, 0.10),
    phase2_pct = c(0.30, 0.20, 0.30, 0.10, 0.30),
    phase3_pct = c(0.20, 0.10, 0.50, 0.40, 0.60),
    
    strategic_rationale = c(
      "Diversified risk profile with pipeline continuity",
      "Bet on high-volume screening to find winners",
      "Minimize attrition risk, accept lower total opportunities",
      "Avoid 'valley of death' in Phase 2, concentrate bets",
      "Maximize near-term approval likelihood, accept higher cost"
    )
  )
  
  # Calculate actual asset counts
  scenarios <- scenarios %>%
    mutate(
      phase1_count = round(total_assets * phase1_pct),
      phase2_count = round(total_assets * phase2_pct),
      phase3_count = round(total_assets * phase3_pct)
    ) %>%
    # Adjust for rounding (ensure sum = total_assets)
    rowwise() %>%
    mutate(
      total = phase1_count + phase2_count + phase3_count,
      phase1_count = if_else(total != total_assets, 
                             phase1_count + (total_assets - total), 
                             phase1_count)
    ) %>%
    select(-total)
  
  scenarios
}


# ==============================================================================
# TRADE-OFF DIMENSION DEFINITIONS
# ==============================================================================

#' Define Key Strategic Trade-offs
#' 
#' These are the decision dimensions users explore
#' 
#' @return List of trade-off definitions
define_tradeoff_dimensions <- function() {
  list(
    
    # Trade-off 1: Volume vs Probability
    volume_vs_probability = list(
      name = "Early-Stage Volume vs Late-Stage Probability",
      description = "More Phase 1 assets = more shots on goal but lower individual success rates. More Phase 3 assets = fewer total opportunities but higher approval likelihood.",
      x_axis = "Phase 1 Asset Count",
      y_axis = "Expected Approvals",
      interpretation = "Steep initial slope suggests early-stage volume pays off; flattening indicates diminishing returns"
    ),
    
    # Trade-off 2: Speed vs Cost
    speed_vs_cost = list(
      name = "Time-to-Market vs Total Investment",
      description = "Late-stage assets reach approval faster but cost more per asset. Early-stage assets take longer but spread risk across more candidates.",
      x_axis = "Average Portfolio Timeline (years)",
      y_axis = "Total Cost ($M)",
      interpretation = "Lower-left quadrant = fast + cheap (rare). Upper-right = slow + expensive (avoid)."
    ),
    
    # Trade-off 3: Breadth vs Depth
    breadth_vs_depth = list(
      name = "Portfolio Breadth vs Depth",
      description = "Spreading assets across all phases (breadth) vs concentrating in one phase (depth). Breadth = continuity. Depth = conviction.",
      x_axis = "Portfolio Concentration (Gini coefficient)",
      y_axis = "Risk-Adjusted Return",
      interpretation = "Moderate concentration often optimal; extreme concentration (all one phase) is fragile"
    ),
    
    # Trade-off 4: Risk vs Return
    risk_vs_return = list(
      name = "Attrition Risk vs Expected Approvals",
      description = "Higher early-stage allocation = more total failures but more eventual approvals. Late-stage focus = fewer failures but fewer total wins.",
      x_axis = "Expected Failures (Asset Count)",
      y_axis = "Expected Approvals",
      interpretation = "Efficient frontier exists; some allocations dominated by others"
    )
  )
}


# ==============================================================================
# THERAPEUTIC AREA STRATEGY TEMPLATES
# ==============================================================================

#' Recommend Portfolio Strategy by Therapeutic Area
#' 
#' Different disease areas have different risk/return profiles
#' 
#' @param therapeutic_area Character
#' @return Named vector with recommended allocation
recommend_allocation_by_area <- function(therapeutic_area) {
  
  recommendations <- list(
    
    "Oncology" = list(
      allocation = c(phase1 = 14, phase2 = 4, phase3 = 2),
      rationale = "Very low overall success rate (3.4%) requires high early-stage volume. Accept that most will fail but a few may break through."
    ),
    
    "Vaccines" = list(
      allocation = c(phase1 = 6, phase2 = 6, phase3 = 8),
      rationale = "High success rate (33.4%) justifies late-stage focus. Lower risk allows concentration in Phase 3."
    ),
    
    "Anti-Infectives" = list(
      allocation = c(phase1 = 10, phase2 = 6, phase3 = 4),
      rationale = "Moderate success (18%) suggests balanced approach. Maintain pipeline continuity across phases."
    ),
    
    "Overall" = list(
      allocation = c(phase1 = 10, phase2 = 6, phase3 = 4),
      rationale = "Industry-average allocation (6.7% LOA). Standard risk profile for diversified portfolio."
    ),
    
    "Orphan Drugs" = list(
      allocation = c(phase1 = 12, phase2 = 5, phase3 = 3),
      rationale = "Phase 1 success is high (75.9%) but Phase 2/3 very challenging. Front-load pipeline, expect attrition later."
    )
  )
  
  recommendations[[therapeutic_area]]
}


# ==============================================================================
# CONSTRAINT DEFINITIONS
# ==============================================================================

#' Define Realistic Portfolio Constraints
#' 
#' Boundaries for slider ranges and validation
#' 
#' @return List of constraint parameters
define_portfolio_constraints <- function() {
  list(
    min_total_assets = 5,
    max_total_assets = 50,
    default_total_assets = 20,
    
    min_phase_allocation = 0,  # Can have zero in a phase
    
    # Budget constraints (millions)
    typical_rd_budget_range = c(500, 5000),
    
    # Timeline constraints (years)
    min_acceptable_timeline = 3,  # Unrealistic to expect approval in < 3 years
    max_acceptable_timeline = 15, # > 15 years suggests portfolio too early-stage
    
    # Success rate constraints (for sensitivity analysis)
    min_phase_success = 0.10,  # No phase has < 10% success historically
    max_phase_success = 0.95   # No phase has > 95% success
  )
}


# ==============================================================================
# SAVE SCENARIOS TO DISK
# ==============================================================================

# Generate and save preset scenarios
preset_scenarios <- create_preset_scenarios(total_assets = 20)
saveRDS(preset_scenarios, here("data/scenarios/preset_scenarios.rds"))
write.csv(preset_scenarios, here("data/scenarios/preset_scenarios.csv"), row.names = FALSE)

# Save trade-off definitions
tradeoff_definitions <- define_tradeoff_dimensions()
saveRDS(tradeoff_definitions, here("data/scenarios/tradeoff_definitions.rds"))

# Save therapeutic area recommendations
therapeutic_recommendations <- list(
  oncology = recommend_allocation_by_area("Oncology"),
  vaccines = recommend_allocation_by_area("Vaccines"),
  anti_infectives = recommend_allocation_by_area("Anti-Infectives"),
  overall = recommend_allocation_by_area("Overall"),
  orphan = recommend_allocation_by_area("Orphan Drugs")
)
saveRDS(therapeutic_recommendations, here("data/scenarios/therapeutic_recommendations.rds"))

# Save constraints
portfolio_constraints <- define_portfolio_constraints()
saveRDS(portfolio_constraints, here("data/scenarios/portfolio_constraints.rds"))

cat("✅ Scenario configurations saved to data/scenarios/\n")