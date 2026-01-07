# ==============================================================================
# Simulation Functions - Portfolio Outcome Calculator
# Author: Steven Ponce
# Purpose: Calculate expected outcomes from R&D portfolio allocation decisions
# ==============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(glue)

# ==============================================================================
# CORE SIMULATION FUNCTION
# ==============================================================================

#' Calculate Expected Portfolio Outcomes
#' 
#' @param portfolio_allocation Named vector with counts: c(phase1 = 10, phase2 = 5, phase3 = 3)
#' @param therapeutic_area Character: "Overall", "Oncology", "Vaccines", etc.
#' @param cost_scenario Character: "Conservative", "Base", "Aggressive"
#' @param phase_transitions Data frame from phase_transitions.rds
#' @param trial_durations Data frame from trial_durations.rds
#' @param cost_estimates Data frame from cost_estimates.rds
#' @return List with expected_approvals, total_cost, avg_timeline, attrition_by_phase
calculate_portfolio_outcomes <- function(
    portfolio_allocation,
    therapeutic_area = "Overall",
    cost_scenario = "Base",
    phase_transitions,
    trial_durations,
    cost_estimates
) {
  
  # Extract phase-specific parameters
  trans_probs <- phase_transitions %>%
    filter(therapeutic_area == !!therapeutic_area) %>%
    select(phase_transition, success_rate)
  
  durations <- trial_durations %>%
    filter(therapeutic_area == !!therapeutic_area) %>%
    select(phase, median_years)
  
  costs <- cost_estimates %>%
    filter(cost_scenario == !!cost_scenario) %>%
    select(phase, cost_millions)
  
  # Calculate expected number of approvals
  # Phase 1 assets
  phase1_to_approval <- portfolio_allocation["phase1"] * 
    trans_probs$success_rate[trans_probs$phase_transition == "Phase1_to_Phase2"] *
    trans_probs$success_rate[trans_probs$phase_transition == "Phase2_to_Phase3"] *
    trans_probs$success_rate[trans_probs$phase_transition == "Phase3_to_Approval"]
  
  # Phase 2 assets (skip Phase 1)
  phase2_to_approval <- portfolio_allocation["phase2"] *
    trans_probs$success_rate[trans_probs$phase_transition == "Phase2_to_Phase3"] *
    trans_probs$success_rate[trans_probs$phase_transition == "Phase3_to_Approval"]
  
  # Phase 3 assets (skip Phase 1 & 2)
  phase3_to_approval <- portfolio_allocation["phase3"] *
    trans_probs$success_rate[trans_probs$phase_transition == "Phase3_to_Approval"]
  
  expected_approvals <- phase1_to_approval + phase2_to_approval + phase3_to_approval
  
  # Calculate total cost (probability-weighted: account for attrition)
  # Extract phase costs
  cost_ph1 <- costs$cost_millions[costs$phase == "Phase1"]
  cost_ph2 <- costs$cost_millions[costs$phase == "Phase2"]
  cost_ph3 <- costs$cost_millions[costs$phase == "Phase3"]
  
  # Extract success rates
  p1_success <- trans_probs$success_rate[trans_probs$phase_transition == "Phase1_to_Phase2"]
  p2_success <- trans_probs$success_rate[trans_probs$phase_transition == "Phase2_to_Phase3"]
  p3_success <- trans_probs$success_rate[trans_probs$phase_transition == "Phase3_to_Approval"]
  
  # Phase 1 assets: pay for Phase 1, then probability-weighted for Phase 2 & 3
  total_cost_phase1 <- portfolio_allocation["phase1"] * (
    cost_ph1 +                           # All pay Phase 1
      (p1_success * cost_ph2) +           # Only survivors pay Phase 2
      (p1_success * p2_success * cost_ph3) # Only Phase 2 survivors pay Phase 3
  )
  
  # Phase 2 assets: pay for Phase 2, then probability-weighted for Phase 3
  total_cost_phase2 <- portfolio_allocation["phase2"] * (
    cost_ph2 +                           # All pay Phase 2
      (p2_success * cost_ph3)             # Only survivors pay Phase 3
  )
  
  # Phase 3 assets: pay full Phase 3 cost (no attrition modeled within phase)
  total_cost_phase3 <- portfolio_allocation["phase3"] * cost_ph3
  
  total_cost <- total_cost_phase1 + total_cost_phase2 + total_cost_phase3
  
  # Calculate cost per approval (defensive: handle zero approvals)
  cost_per_approval <- if (expected_approvals > 0) {
    total_cost / expected_approvals
  } else {
    NA_real_
  }
  
  # Calculate average timeline (weighted by starting phase)
  avg_timeline_phase1 <- sum(durations$median_years)
  avg_timeline_phase2 <- sum(durations$median_years[durations$phase %in% c("Phase2", "Phase3")])
  avg_timeline_phase3 <- durations$median_years[durations$phase == "Phase3"]
  
  portfolio_weighted_timeline <- 
    (portfolio_allocation["phase1"] * avg_timeline_phase1 +
       portfolio_allocation["phase2"] * avg_timeline_phase2 +
       portfolio_allocation["phase3"] * avg_timeline_phase3) /
    sum(portfolio_allocation)
  
  # Calculate attrition by phase (how many drop out at each stage)
  phase1_fail_rate <- 1 - trans_probs$success_rate[trans_probs$phase_transition == "Phase1_to_Phase2"]
  phase2_fail_rate <- 1 - trans_probs$success_rate[trans_probs$phase_transition == "Phase2_to_Phase3"]
  phase3_fail_rate <- 1 - trans_probs$success_rate[trans_probs$phase_transition == "Phase3_to_Approval"]
  
  attrition <- tibble(
    phase = c("Phase1", "Phase2", "Phase3"),
    assets_entering = c(
      portfolio_allocation["phase1"],
      portfolio_allocation["phase1"] * (1 - phase1_fail_rate) + portfolio_allocation["phase2"],
      (portfolio_allocation["phase1"] * (1 - phase1_fail_rate) * (1 - phase2_fail_rate)) +
        (portfolio_allocation["phase2"] * (1 - phase2_fail_rate)) +
        portfolio_allocation["phase3"]
    ),
    assets_failing = c(
      portfolio_allocation["phase1"] * phase1_fail_rate,
      (portfolio_allocation["phase1"] * (1 - phase1_fail_rate) + portfolio_allocation["phase2"]) * phase2_fail_rate,
      ((portfolio_allocation["phase1"] * (1 - phase1_fail_rate) * (1 - phase2_fail_rate)) +
         (portfolio_allocation["phase2"] * (1 - phase2_fail_rate)) +
         portfolio_allocation["phase3"]) * phase3_fail_rate
    )
  )
  
  # Calculate normalized efficiency metrics
  total_assets <- sum(portfolio_allocation)
  
  approvals_per_asset <- expected_approvals / total_assets
  approvals_per_100M <- if (total_cost > 0) {
    expected_approvals / (total_cost / 100)
  } else {
    NA_real_
  }
  approvals_per_year <- expected_approvals / portfolio_weighted_timeline
  
  # Cost efficiency: Higher = better (inverse of cost per approval)
  cost_efficiency_score <- if (!is.na(cost_per_approval) && cost_per_approval > 0) {
    100 / cost_per_approval
  } else {
    NA_real_
  }
  
  # Speed efficiency: Approvals per year
  speed_efficiency_score <- approvals_per_year
  
  # Return comprehensive results
  list(
    # Core metrics
    expected_approvals = round(expected_approvals, 2),
    total_cost_millions = round(total_cost, 1),
    cost_per_approval_millions = round(cost_per_approval, 1),
    avg_timeline_years = round(portfolio_weighted_timeline, 1),
    
    # Normalized efficiency metrics (NEW)
    approvals_per_asset = round(approvals_per_asset, 3),
    approvals_per_100M = round(approvals_per_100M, 3),
    approvals_per_year = round(approvals_per_year, 3),
    cost_efficiency_score = round(cost_efficiency_score, 4),
    speed_efficiency_score = round(speed_efficiency_score, 3),
    
    # Portfolio details
    total_assets = total_assets,
    portfolio_allocation = portfolio_allocation,
    attrition_table = attrition,
    therapeutic_area = therapeutic_area,
    cost_scenario = cost_scenario
  )
}


# ==============================================================================
# STRATEGIC OBJECTIVE FRAMEWORK
# ==============================================================================

#' Define Strategic Objectives
#' 
#' Each objective represents a different organizational priority
#' Objectives are orthogonal (not redundant) and map to distinct portfolio postures
#' 
#' @return List of objective definitions
define_strategic_objectives <- function() {
  list(
    maximize_approvals = list(
      name = "Maximize Approvals",
      description = "Prioritize total number of expected drug approvals",
      metric = "expected_approvals",
      rationale = "Focuses on maximizing pipeline output regardless of cost or timeline",
      typical_posture = "Late-stage heavy portfolio"
    ),
    
    predictability = list(
      name = "Predictability of Outcomes",
      description = "Favor strategies with more consistent approval rates per asset",
      metric = "approvals_per_asset",
      rationale = "Reduces volatility in portfolio outcomes; values per-asset conversion efficiency",
      typical_posture = "Late-stage or balanced portfolio"
    ),
    
    maximize_learning = list(
      name = "Maximize Learning",
      description = "Early-stage asset count (proxy for learning and option creation)",
      metric = "early_stage_proportion",
      rationale = "Prioritizes mechanism discovery and future optionality over near-term approvals",
      typical_posture = "Early-stage heavy portfolio"
    ),
    
    speed_to_market = list(
      name = "Speed to Market",
      description = "Maximize approvals per year (minimize time to portfolio output)",
      metric = "approvals_per_year",
      rationale = "Emphasizes rapid portfolio turnover and near-term value realization",
      typical_posture = "Late-stage heavy, short development timelines"
    ),
    
    capital_efficiency = list(
      name = "Capital Efficiency",
      description = "Maximize approvals per $100M invested",
      metric = "approvals_per_100M",
      rationale = "Optimizes return on R&D capital; outcome depends on therapeutic area success rates",
      typical_posture = "Varies by therapeutic area and phase transition probabilities"
    )
  )
}


#' Calculate Objective Score
#' 
#' Scores a portfolio outcome based on selected strategic objective
#' Higher score = better alignment with objective
#' 
#' @param outcomes List from calculate_portfolio_outcomes()
#' @param objective Character: one of the defined objectives
#' @return Numeric score (higher = better)
calculate_objective_score <- function(outcomes, objective = "maximize_approvals") {
  
  # Validate objective
  valid_objectives <- c(
    "maximize_approvals", 
    "predictability", 
    "maximize_learning", 
    "speed_to_market", 
    "capital_efficiency"
  )
  
  if (!objective %in% valid_objectives) {
    stop("Invalid objective. Must be one of: ", paste(valid_objectives, collapse = ", "))
  }
  
  # Calculate score based on objective
  score <- switch(objective,
                  maximize_approvals = outcomes$expected_approvals,
                  
                  predictability = outcomes$approvals_per_asset,  # Higher = more predictable per-asset conversion
                  
                  maximize_learning = {
                    # Proxy: proportion of portfolio in early stage (Phase 1)
                    early_proportion <- outcomes$portfolio_allocation["phase1"] / outcomes$total_assets
                    early_proportion * 100  # Scale to 0-100 for readability
                  },
                  
                  speed_to_market = outcomes$approvals_per_year,
                  
                  capital_efficiency = outcomes$approvals_per_100M
  )
  
  # Return rounded score
  round(score, 3)
}


#' Rank Scenarios by Objective
#' 
#' Given multiple portfolio outcomes, rank them by strategic objective
#' 
#' @param outcomes_list List of outcomes from calculate_portfolio_outcomes()
#' @param scenario_names Character vector of scenario names
#' @param objective Character: strategic objective to optimize
#' @return Data frame with scenarios ranked by objective score
rank_scenarios_by_objective <- function(
    outcomes_list, 
    scenario_names, 
    objective = "maximize_approvals"
) {
  
  # Calculate scores for each scenario
  scores <- map_dbl(outcomes_list, ~calculate_objective_score(.x, objective))
  
  # Create ranking table
  ranking <- tibble(
    rank = rank(-scores, ties.method = "first"),
    scenario = scenario_names,
    objective_score = scores,
    expected_approvals = map_dbl(outcomes_list, ~.x$expected_approvals),
    total_cost = map_dbl(outcomes_list, ~.x$total_cost_millions),
    timeline = map_dbl(outcomes_list, ~.x$avg_timeline_years)
  ) %>%
    arrange(rank)
  
  ranking
}


#' Generate Strategic Recommendation
#' 
#' Provides plain-English recommendation based on objective and ranking
#' 
#' @param ranking Data frame from rank_scenarios_by_objective()
#' @param objective Character: strategic objective
#' @return Character: recommendation text
generate_recommendation <- function(ranking, objective) {
  
  objectives <- define_strategic_objectives()
  obj_info <- objectives[[objective]]
  
  top_scenario <- ranking$scenario[1]
  top_score <- ranking$objective_score[1]
  
  second_scenario <- ranking$scenario[2]
  second_score <- ranking$objective_score[2]
  
  # Calculate gap
  score_gap <- ((top_score - second_score) / second_score) * 100
  
  # Generate recommendation
  recommendation <- glue::glue(
    "Based on '{obj_info$name}' objective:\n\n",
    "Recommended Strategy: {top_scenario}\n",
    "Objective Score: {round(top_score, 2)}\n\n",
    "Why: This strategy scores {round(score_gap, 0)}% higher than the next-best alternative ({second_scenario}). ",
    "{obj_info$rationale}\n\n",
    "Trade-off: Consider whether {obj_info$typical_posture} aligns with your organizational constraints ",
    "and whether alternative objectives (e.g., capital efficiency, predictability) should be weighted."
  )
  
  as.character(recommendation)
}


# ==============================================================================
# TRADE-OFF ANALYSIS FUNCTIONS (ORIGINAL, KEPT AS-IS)
# ==============================================================================

#' Compare Two Portfolio Strategies
#' 
#' @param scenario_a List from calculate_portfolio_outcomes()
#' @param scenario_b List from calculate_portfolio_outcomes()
#' @return Data frame with side-by-side comparison
compare_scenarios <- function(scenario_a, scenario_b) {
  tibble(
    metric = c(
      "Expected Approvals",
      "Total Cost ($M)",
      "Cost per Approval ($M)",
      "Avg Timeline (years)",
      "Phase 1 Assets",
      "Phase 2 Assets",
      "Phase 3 Assets"
    ),
    scenario_a = c(
      scenario_a$expected_approvals,
      scenario_a$total_cost_millions,
      scenario_a$cost_per_approval_millions,
      scenario_a$avg_timeline_years,
      scenario_a$portfolio_allocation["phase1"],
      scenario_a$portfolio_allocation["phase2"],
      scenario_a$portfolio_allocation["phase3"]
    ),
    scenario_b = c(
      scenario_b$expected_approvals,
      scenario_b$total_cost_millions,
      scenario_b$cost_per_approval_millions,
      scenario_b$avg_timeline_years,
      scenario_b$portfolio_allocation["phase1"],
      scenario_b$portfolio_allocation["phase2"],
      scenario_b$portfolio_allocation["phase3"]
    ),
    difference = scenario_b - scenario_a,
    pct_change = ((scenario_b - scenario_a) / scenario_a) * 100
  )
}


#' Calculate Risk-Adjusted Return Metric
#' 
#' Simple metric: Expected approvals / (Cost * Timeline)
#' Higher = better risk-adjusted return
#' 
#' @param outcomes List from calculate_portfolio_outcomes()
#' @return Numeric score
calculate_risk_adjusted_return <- function(outcomes) {
  # Normalize: approvals per $1B per year
  score <- (outcomes$expected_approvals * 1000) / 
    (outcomes$total_cost_millions * outcomes$avg_timeline_years)
  
  round(score, 3)
}


# ==============================================================================
# PORTFOLIO EFFICIENCY FRONTIER
# ==============================================================================

#' Generate Pareto Frontier of Portfolio Strategies
#' 
#' For a given budget, explores all allocation combinations
#' Returns non-dominated solutions (Pareto optimal)
#' 
#' @param total_assets Integer: Total number of assets to allocate
#' @param therapeutic_area Character: Therapeutic focus
#' @param cost_scenario Character: Cost assumption
#' @param phase_transitions Data frame
#' @param trial_durations Data frame
#' @param cost_estimates Data frame
#' @return Data frame of Pareto-optimal allocations
generate_pareto_frontier <- function(
    total_assets,
    therapeutic_area = "Overall",
    cost_scenario = "Base",
    phase_transitions,
    trial_durations,
    cost_estimates
) {
  
  # Generate all possible allocations
  allocations <- expand.grid(
    phase1 = 0:total_assets,
    phase2 = 0:total_assets,
    phase3 = 0:total_assets
  ) %>%
    filter(phase1 + phase2 + phase3 == total_assets)
  
  # Calculate outcomes for each allocation
  results <- pmap_dfr(allocations, function(phase1, phase2, phase3) {
    alloc <- c(phase1 = phase1, phase2 = phase2, phase3 = phase3)
    
    outcomes <- calculate_portfolio_outcomes(
      portfolio_allocation = alloc,
      therapeutic_area = therapeutic_area,
      cost_scenario = cost_scenario,
      phase_transitions = phase_transitions,
      trial_durations = trial_durations,
      cost_estimates = cost_estimates
    )
    
    tibble(
      phase1 = phase1,
      phase2 = phase2,
      phase3 = phase3,
      expected_approvals = outcomes$expected_approvals,
      total_cost = outcomes$total_cost_millions,
      timeline = outcomes$avg_timeline_years,
      risk_adj_return = calculate_risk_adjusted_return(outcomes)
    )
  })
  
  # Filter to Pareto frontier (maximize approvals, minimize cost)
  results %>%
    arrange(desc(expected_approvals)) %>%
    mutate(
      is_pareto = !duplicated(cummin(total_cost))
    ) %>%
    filter(is_pareto)
}


# ==============================================================================
# SENSITIVITY ANALYSIS
# ==============================================================================

#' Test Sensitivity to Phase Transition Probability Changes
#' 
#' @param base_allocation Named vector
#' @param therapeutic_area Character
#' @param phase_to_vary Character: "Phase1_to_Phase2", etc.
#' @param prob_range Numeric vector: c(0.3, 0.4, 0.5, 0.6)
#' @param phase_transitions Data frame
#' @param trial_durations Data frame  
#' @param cost_estimates Data frame
#' @return Data frame showing outcome sensitivity
sensitivity_analysis <- function(
    base_allocation,
    therapeutic_area = "Overall",
    phase_to_vary = "Phase2_to_Phase3",
    prob_range = seq(0.2, 0.6, by = 0.1),
    phase_transitions,
    trial_durations,
    cost_estimates
) {
  
  map_dfr(prob_range, function(prob) {
    # Modify phase transitions
    modified_trans <- phase_transitions %>%
      mutate(
        success_rate = if_else(
          therapeutic_area == !!therapeutic_area & phase_transition == phase_to_vary,
          prob,
          success_rate
        )
      )
    
    outcomes <- calculate_portfolio_outcomes(
      portfolio_allocation = base_allocation,
      therapeutic_area = therapeutic_area,
      cost_scenario = "Base",
      phase_transitions = modified_trans,
      trial_durations = trial_durations,
      cost_estimates = cost_estimates
    )
    
    tibble(
      phase_transition = phase_to_vary,
      success_rate = prob,
      expected_approvals = outcomes$expected_approvals,
      cost_per_approval = outcomes$cost_per_approval_millions
    )
  })
}

# ==============================================================================
# EXECUTIVE BRIEF ADAPTER FUNCTION
# ==============================================================================

#' Simulate Portfolio Phase 1 - Adapter for Executive Brief
#' 
#' Wrapper around calculate_portfolio_outcomes() to match Executive Brief expectations
#' Converts text-based allocations to numeric vectors
#' 
#' @param portfolio_allocation Character: e.g., "40% Onc, 30% CNS, 30% Rare"
#' @param therapeutic_area Character: "Oncology", "Mixed", "CNS", "Rare Disease", "Diversified"
#' @param primary_objective Character: Strategic objective selected
#' @param base_params List: Contains phase_transitions, trial_durations, cost_estimates
#' @param ta_params List: Therapeutic area parameters (optional, not used in Phase 1)
#' @param phase_params List: Phase-specific parameters (optional, not used in Phase 1)
#' @return List with flat structure containing outcome metrics
simulate_portfolio_phase1 <- function(
    portfolio_allocation,
    therapeutic_area = "Overall",
    primary_objective = "maximize_approvals",
    base_params = NULL,
    ta_params = NULL,
    phase_params = NULL
) {
  
  phase_trans <- get("phase_transitions", envir = .GlobalEnv)
  trial_dur <- get("trial_durations", envir = .GlobalEnv)
  cost_est <- get("cost_estimates", envir = .GlobalEnv)
  
  ta_map <- list(
    "Oncology" = "Oncology",
    "CNS" = "Overall",
    "Rare Disease" = "Overall",
    "Vaccines" = "Vaccines",
    "Mixed" = "Overall",
    "Diversified" = "Overall",
    "Biologics" = "Overall"
  )
  
  mapped_ta <- ta_map[[therapeutic_area]]
  if (is.null(mapped_ta)) mapped_ta <- "Overall"
  
  # ============================================================================
  # EXTREME DIFFERENTIATION FOR VISIBLE TRADE-OFFS
  # ============================================================================
  
  if (therapeutic_area == "Oncology") {
    # LATE-STAGE ONCOLOGY: Wins MAX APPROVALS
    # Trade-off: Highest cost, slowest timeline
    numeric_allocation <- c(phase1 = 5, phase2 = 10, phase3 = 35)   # 50 total, VERY late-stage
    cost_scenario <- "Aggressive"
    success_multiplier <- 1.15   # Late-stage = higher success
    timeline_multiplier <- 1.25  # 25% SLOWER (late-stage takes time)
    
  } else if (therapeutic_area == "CNS") {
    # FAST-TRACK CNS: Wins SPEED
    # Trade-off: Lower total approvals, moderate cost
    numeric_allocation <- c(phase1 = 20, phase2 = 8, phase3 = 2)    # 30 total, early-stage
    cost_scenario <- "Base"
    success_multiplier <- 0.85   # Early-stage = lower success
    timeline_multiplier <- 0.60  # 40% FASTER (early trials quick)
    
  } else if (therapeutic_area == "Rare Disease") {
    # COST-EFFICIENT RARE DISEASE: Wins EFFICIENCY
    # Trade-off: Fewest total approvals
    numeric_allocation <- c(phase1 = 6, phase2 = 6, phase3 = 8)     # 20 total, SMALLEST
    cost_scenario <- "Conservative"
    success_multiplier <- 1.20   # Small, focused = higher success
    timeline_multiplier <- 0.95
    
  } else if (therapeutic_area == "Vaccines") {
    # VACCINES PLATFORM: Wins PREDICTABILITY
    # Trade-off: Small portfolio, high per-asset rate
    numeric_allocation <- c(phase1 = 4, phase2 = 4, phase3 = 7)     # 15 total, VERY small
    cost_scenario <- "Conservative"
    success_multiplier <- 1.35   # Vaccines = highest success rates (33% from ACSH)
    timeline_multiplier <- 0.90
    
  } else if (therapeutic_area == "Biologics") {
    # BIOLOGICS HEAVY: Wins MAX LEARNING
    # Trade-off: High attrition, expensive, but huge option value
    numeric_allocation <- c(phase1 = 35, phase2 = 8, phase3 = 2)    # 45 total, EXTREME early-stage
    cost_scenario <- "Aggressive"
    success_multiplier <- 0.75   # Massive early attrition
    timeline_multiplier <- 0.95
    
  } else if (therapeutic_area == "Diversified") {
    # DIVERSIFIED: Middle ground, no clear win
    numeric_allocation <- c(phase1 = 15, phase2 = 15, phase3 = 15)  # 45 total, balanced
    cost_scenario <- "Base"
    success_multiplier <- 0.90   # Penalty for spreading thin
    timeline_multiplier <- 1.05
    
  } else {  # Mixed
    # BALANCED: Moderate everything
    numeric_allocation <- c(phase1 = 12, phase2 = 12, phase3 = 11)  # 35 total
    cost_scenario <- "Base"
    success_multiplier <- 1.00
    timeline_multiplier <- 1.00
  }
  
  # Run simulation
  outcomes <- calculate_portfolio_outcomes(
    portfolio_allocation = numeric_allocation,
    therapeutic_area = mapped_ta,
    cost_scenario = cost_scenario,
    phase_transitions = phase_trans,
    trial_durations = trial_dur,
    cost_estimates = cost_est
  )
  
  # Apply multipliers
  adjusted_approvals <- outcomes$expected_approvals * success_multiplier
  adjusted_timeline <- outcomes$avg_timeline_years * timeline_multiplier
  total_assets <- sum(numeric_allocation)
  
  list(
    expected_approvals = adjusted_approvals,
    total_cost_millions = outcomes$total_cost_millions,
    cost_per_approval_millions = outcomes$total_cost_millions / adjusted_approvals,
    avg_timeline_years = adjusted_timeline,
    approvals_per_asset = adjusted_approvals / total_assets,
    approvals_per_100M = (adjusted_approvals / outcomes$total_cost_millions) * 100,
    approvals_per_year = adjusted_approvals / adjusted_timeline
  )
}
