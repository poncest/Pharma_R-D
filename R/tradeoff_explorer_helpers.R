# ============================================================================ #
# Helper Functions for Trade-off Explorer
# ============================================================================ #

# Radar/Spider Chart Coordinate System
coord_radar <- function(theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  
  ggproto(
    "CoordRadar", 
    CoordPolar,
    theta = theta,
    r = r,
    start = start,
    direction = sign(direction),
    is_linear = function(coord) TRUE
  )
}

# Simulate All 7 Phase 1 Scenarios
simulate_all_phase1_scenarios <- function(phase_transitions, trial_durations, cost_estimates) {
  
  # Use the existing simulate_portfolio_phase1 function with EXTREME multipliers
  # This should already be available from simulate_portfolio_phase1_EXTREME.R
  
  # If that function exists, call it and return results
  # Otherwise, define scenarios here
  
  scenarios <- list(
    list(
      name = "Late-Stage Oncology Focus",
      allocation = c(phase1 = 5, phase2 = 10, phase3 = 35),
      therapeutic_area = "Oncology",
      success_multiplier = 1.15,
      timeline_multiplier = 1.25,
      cost_scenario = "Aggressive"
    ),
    list(
      name = "Fast-Track CNS",
      allocation = c(phase1 = 20, phase2 = 8, phase3 = 2),
      therapeutic_area = "Overall",  # CNS not in empirical data
      success_multiplier = 0.85,
      timeline_multiplier = 0.60,
      cost_scenario = "Base"
    ),
    list(
      name = "Cost-Efficient Rare Disease",
      allocation = c(phase1 = 5, phase2 = 7, phase3 = 8),
      therapeutic_area = "Overall",
      success_multiplier = 1.20,
      timeline_multiplier = 1.0,
      cost_scenario = "Conservative"
    ),
    list(
      name = "Vaccines Platform",
      allocation = c(phase1 = 5, phase2 = 5, phase3 = 5),
      therapeutic_area = "Vaccines",
      success_multiplier = 1.35,
      timeline_multiplier = 1.0,
      cost_scenario = "Conservative"
    ),
    list(
      name = "Biologics Heavy",
      allocation = c(phase1 = 35, phase2 = 8, phase3 = 2),
      therapeutic_area = "Overall",
      success_multiplier = 0.75,
      timeline_multiplier = 1.15,
      cost_scenario = "Aggressive"
    ),
    list(
      name = "Diversified Balanced",
      allocation = c(phase1 = 15, phase2 = 15, phase3 = 15),
      therapeutic_area = "Overall",
      success_multiplier = 0.90,
      timeline_multiplier = 1.0,
      cost_scenario = "Base"
    ),
    list(
      name = "Mixed Portfolio",
      allocation = c(phase1 = 12, phase2 = 13, phase3 = 12),
      therapeutic_area = "Overall",
      success_multiplier = 1.0,
      timeline_multiplier = 1.0,
      cost_scenario = "Base"
    )
  )
  
  # Run simulation for each scenario
  results <- lapply(scenarios, function(scenario) {
    
    # Call calculate_portfolio_outcomes for this scenario
    outcome <- calculate_portfolio_outcomes(
      portfolio_allocation = scenario$allocation,
      therapeutic_area = scenario$therapeutic_area,
      cost_scenario = scenario$cost_scenario,
      phase_transitions = phase_transitions,
      trial_durations = trial_durations,
      cost_estimates = cost_estimates,
      success_multiplier = scenario$success_multiplier,
      timeline_multiplier = scenario$timeline_multiplier
    )
    
    # Add scenario name
    outcome$scenario_name <- scenario$name
    outcome$total_assets <- sum(scenario$allocation)
    
    outcome
  })
  
  # Combine into data frame
  do.call(rbind, results)
}