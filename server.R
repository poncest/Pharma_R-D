# ============================================================================ #
# Server Logic - Pharma R&D Strategic Simulator
# ============================================================================ #

server <- function(input, output, session) {
  
  # ========================================================================== #
  # Create app_data object with all necessary data tables
  # ========================================================================== #
  
  app_data <- list(
    phase_transitions = phase_transitions,
    trial_durations = trial_durations,
    cost_estimates = cost_estimates,
    therapeutic_areas = therapeutic_areas,
    base_params = NULL,   
    ta_params = NULL,     
    phase_params = NULL   
  )
  
  # ========================================================================== #
  # Call Module Servers WITH app_data
  # ========================================================================== #
  
  mod_executive_brief_server("executive", app_data = app_data)
  mod_portfolio_builder_server("builder", app_data = app_data)
  mod_tradeoff_explorer_server("tradeoffs", app_data = app_data)
  mod_methods_server("methods", app_data = app_data)
  
 }