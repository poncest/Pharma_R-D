# ============================================================================ #
# Module: Trade-off Explorer
# Purpose: Visualize multi-objective trade-offs across scenarios
# ============================================================================ #

# Radar chart coordinate system (inline helper)
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

mod_tradeoff_explorer_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      style = "padding: 2em 0;",
      
      # Header
      h2("Trade-off Explorer", style = "color: #006B75;"),
      p(
        style = "color: #605E5C; margin-bottom: 2em;",
        "Compare multiple portfolio configurations to understand when strategic objectives align versus diverge."
      ),
      
      # Scenario Selector
      div(
        class = "ui segment",
        style = "background: #F8F9FA; margin-bottom: 2em;",
        h3("Select Scenarios to Compare", style = "color: #006B75; margin-top: 0;"),
        p(
          style = "color: #605E5C; font-size: 0.9em; margin-bottom: 1em;",
          "Choose 2-5 scenarios to compare across all strategic objectives. More than 5 becomes difficult to interpret."
        ),
        
        div(
          class = "ui stackable three column grid",
          
          # Column 1
          div(
            class = "column",
            checkboxInput(
              ns("scenario_oncology"),
              "Late-Stage Oncology Focus",
              value = TRUE
            ),
            checkboxInput(
              ns("scenario_cns"),
              "Fast-Track CNS",
              value = FALSE
            ),
            checkboxInput(
              ns("scenario_rare"),
              "Cost-Efficient Rare Disease",
              value = TRUE
            )
          ),
          
          # Column 2
          div(
            class = "column",
            checkboxInput(
              ns("scenario_vaccines"),
              "Vaccines Platform",
              value = FALSE
            ),
            checkboxInput(
              ns("scenario_biologics"),
              "Biologics Heavy",
              value = FALSE
            )
          ),
          
          # Column 3
          div(
            class = "column",
            checkboxInput(
              ns("scenario_diversified"),
              "Diversified Balanced",
              value = TRUE
            ),
            checkboxInput(
              ns("scenario_mixed"),
              "Mixed Portfolio",
              value = FALSE
            )
          )
        ),
        
        # Selection counter
        div(
          style = "margin-top: 1em; padding: 0.75em; background: white; border-left: 3px solid #006B75;",
          uiOutput(ns("selection_count"))
        )
      ),
      
      # Comparison Content
      div(
        class = "ui stackable two column grid",
        
        # LEFT: Radar Chart
        div(
          class = "column",
          div(
            class = "ui segment",
            h3("Multi-Objective Comparison", style = "color: #006B75; margin-top: 0;"),
            p(
              style = "color: #605E5C; font-size: 0.9em; margin-bottom: 1em;",
              "Normalized scores across five strategic objectives. Larger area = better performance on that objective."
            ),
            girafeOutput(ns("radar_chart"), width = "100%", height = "500px")
          )
        ),
        
        # RIGHT: Metrics Table
        div(
          class = "column",
          div(
            class = "ui segment",
            h3("Detailed Metrics", style = "color: #006B75; margin-top: 0;"),
            p(
              style = "color: #605E5C; font-size: 0.9em; margin-bottom: 1em;",
              "Raw values for each scenario. Click column headers to sort."
            ),
            div(
              style = "overflow-x: auto;",
              uiOutput(ns("comparison_table"))
            )
          )
        )
      ),
      
      # Interpretation Section
      div(
        class = "ui segment",
        style = "background: #F0F6FC; margin-top: 2em;",
        h3("Interpretation", style = "color: #006B75; margin-top: 0;"),
        uiOutput(ns("interpretation_text"))
      )
    )
  )
}

# ============================================================================ #
# SERVER
# ============================================================================ #

mod_tradeoff_explorer_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    
    # -------------------------------------------------------------------------
    # Reactive: Get all 7 scenario results (INLINE - NO EXTERNAL FUNCTION)
    # -------------------------------------------------------------------------
    all_scenarios <- reactive({
      
      # CRITICAL: Capture data tables OUTSIDE lapply to avoid scoping issues
      pt <- app_data$phase_transitions
      td <- app_data$trial_durations
      ce <- app_data$cost_estimates
      
      # Define 7 scenarios inline
      scenarios <- list(
        list(
          name = "Late-Stage Oncology Focus",
          allocation = c(phase1 = 5, phase2 = 10, phase3 = 35),
          therapeutic_area = "Oncology",
          cost_scenario = "Aggressive"
        ),
        list(
          name = "Fast-Track CNS",
          allocation = c(phase1 = 20, phase2 = 8, phase3 = 2),
          therapeutic_area = "Overall",
          cost_scenario = "Base"
        ),
        list(
          name = "Cost-Efficient Rare Disease",
          allocation = c(phase1 = 5, phase2 = 7, phase3 = 8),
          therapeutic_area = "Overall",
          cost_scenario = "Conservative"
        ),
        list(
          name = "Vaccines Platform",
          allocation = c(phase1 = 5, phase2 = 5, phase3 = 5),
          therapeutic_area = "Vaccines",
          cost_scenario = "Conservative"
        ),
        list(
          name = "Biologics Heavy",
          allocation = c(phase1 = 35, phase2 = 8, phase3 = 2),
          therapeutic_area = "Overall",
          cost_scenario = "Aggressive"
        ),
        list(
          name = "Diversified Balanced",
          allocation = c(phase1 = 15, phase2 = 15, phase3 = 15),
          therapeutic_area = "Overall",
          cost_scenario = "Base"
        ),
        list(
          name = "Mixed Portfolio",
          allocation = c(phase1 = 12, phase2 = 13, phase3 = 12),
          therapeutic_area = "Overall",
          cost_scenario = "Base"
        )
      )
      
      # Run simulation for each scenario
      results <- lapply(scenarios, function(scenario) {
        
        # Defensive check - print what we're trying
        cat("\n>>> Simulating:", scenario$name, "\n")
        cat("    Therapeutic Area:", scenario$therapeutic_area, "\n")
        cat("    Cost Scenario:", scenario$cost_scenario, "\n")
        
        # Try to run simulation with error handling
        tryCatch({
          outcome <- calculate_portfolio_outcomes(
            portfolio_allocation = scenario$allocation,
            therapeutic_area = scenario$therapeutic_area,
            cost_scenario = scenario$cost_scenario,
            phase_transitions = pt,  # Use captured variables
            trial_durations = td,
            cost_estimates = ce
          )
          
          # Add scenario metadata
          outcome$scenario_name <- scenario$name
          outcome$total_assets <- sum(scenario$allocation)
          outcome$phase1_assets <- scenario$allocation["phase1"]  # For Learning Options metric
          
          cat("    ✓ Success! Approvals:", outcome$expected_approvals, "\n")
          
          outcome
        }, error = function(e) {
          cat("    ✗ ERROR:", conditionMessage(e), "\n")
          # Return NULL on error
          NULL
        })
      })
      
      # Remove NULL results (failed scenarios)
      results <- results[!sapply(results, is.null)]
      
      # If no results succeeded, return empty data frame
      if (length(results) == 0) {
        return(data.frame(
          scenario_name = character(0),
          expected_approvals = numeric(0),
          total_cost_millions = numeric(0),
          avg_timeline_years = numeric(0),
          approvals_per_asset = numeric(0),
          approvals_per_year = numeric(0),
          approvals_per_100M = numeric(0),
          total_assets = numeric(0)
        ))
      }
      
      # Combine into data frame - convert lists to proper rows
      if (length(results) == 0) {
        return(data.frame(
          scenario_name = character(0),
          expected_approvals = numeric(0),
          total_cost_millions = numeric(0),
          avg_timeline_years = numeric(0),
          approvals_per_asset = numeric(0),
          approvals_per_year = numeric(0),
          approvals_per_100M = numeric(0),
          total_assets = numeric(0),
          phase1_assets = numeric(0)
        ))
      }
      
      # Convert each list to a data frame row
      df_results <- do.call(rbind, lapply(results, function(x) {
        data.frame(
          scenario_name = x$scenario_name,
          total_assets = x$total_assets,
          phase1_assets = as.numeric(x$phase1_assets),  # Phase 1 count for Learning
          expected_approvals = as.numeric(x$expected_approvals),
          total_cost_millions = as.numeric(x$total_cost_millions),
          avg_timeline_years = as.numeric(x$avg_timeline_years),
          approvals_per_asset = as.numeric(x$approvals_per_asset),
          approvals_per_year = as.numeric(x$approvals_per_year),
          approvals_per_100M = as.numeric(x$approvals_per_100M),
          stringsAsFactors = FALSE
        )
      }))
      
      df_results
    })
    
    # -------------------------------------------------------------------------
    # Reactive: Filter to selected scenarios
    # -------------------------------------------------------------------------
    selected_scenarios <- reactive({
      all_results <- all_scenarios()
      
      # Check if we have any results at all
      if (is.null(all_results) || nrow(all_results) == 0) {
        return(data.frame(
          scenario_name = character(0),
          expected_approvals = numeric(0),
          total_cost_millions = numeric(0),
          avg_timeline_years = numeric(0),
          approvals_per_asset = numeric(0),
          approvals_per_year = numeric(0),
          approvals_per_100M = numeric(0),
          total_assets = numeric(0)
        ))
      }
      
      # Map checkbox inputs to scenario names
      selected <- character(0)
      
      if (input$scenario_oncology) selected <- c(selected, "Late-Stage Oncology Focus")
      if (input$scenario_cns) selected <- c(selected, "Fast-Track CNS")
      if (input$scenario_rare) selected <- c(selected, "Cost-Efficient Rare Disease")
      if (input$scenario_vaccines) selected <- c(selected, "Vaccines Platform")
      if (input$scenario_biologics) selected <- c(selected, "Biologics Heavy")
      if (input$scenario_diversified) selected <- c(selected, "Diversified Balanced")
      if (input$scenario_mixed) selected <- c(selected, "Mixed Portfolio")
      
      # Filter results
      all_results[all_results$scenario_name %in% selected, ]
    })
    
    # -------------------------------------------------------------------------
    # OUTPUT: Selection Counter
    # -------------------------------------------------------------------------
    output$selection_count <- renderUI({
      n_selected <- nrow(selected_scenarios())
      
      if (n_selected == 0) {
        div(
          style = "color: #D13438;",
          icon("exclamation triangle"),
          " Please select at least 2 scenarios to compare."
        )
      } else if (n_selected == 1) {
        div(
          style = "color: #CA5010;",
          icon("info circle"),
          " ", n_selected, " scenario selected. Select at least one more for comparison."
        )
      } else if (n_selected > 5) {
        div(
          style = "color: #CA5010;",
          icon("exclamation circle"),
          " ", n_selected, " scenarios selected. More than 5 can be difficult to interpret."
        )
      } else {
        div(
          style = "color: #107C10;",
          icon("check circle"),
          " ", n_selected, " scenarios selected for comparison."
        )
      }
    })
    
    # -------------------------------------------------------------------------
    # OUTPUT: Radar Chart
    # -------------------------------------------------------------------------
    output$radar_chart <- renderGirafe({
      data <- selected_scenarios()
      req(nrow(data) >= 2)
      
      # Normalize each metric to 0-1 scale for fair comparison
      # Defensive normalize function - handles edge case where all values are equal
      normalize <- function(x) {
        if (max(x) == min(x)) return(rep(0.5, length(x)))  # All equal = mid-point
        (x - min(x)) / (max(x) - min(x))
      }
      
      # Create normalized data for radar chart
      radar_data <- data %>%
        mutate(
          norm_approvals = normalize(as.numeric(expected_approvals)),
          norm_predictability = normalize(as.numeric(approvals_per_asset)),
          norm_speed = normalize(as.numeric(approvals_per_year)),
          norm_efficiency = normalize(as.numeric(approvals_per_100M)),
          norm_learning = normalize(as.numeric(phase1_assets))  # ✅ FIXED: Use Phase 1 count
        ) %>%
        select(scenario_name, starts_with("norm_")) %>%
        tidyr::pivot_longer(
          cols = starts_with("norm_"),
          names_to = "objective",
          values_to = "score"
        ) %>%
        mutate(
          objective = case_when(
            objective == "norm_approvals" ~ "Total Approvals",
            objective == "norm_predictability" ~ "Predictability",
            objective == "norm_speed" ~ "Speed to Market",
            objective == "norm_efficiency" ~ "Capital Efficiency",
            objective == "norm_learning" ~ "Learning Options",
            TRUE ~ objective
          )
        )
      
      # Create radar/spider chart
      p <- ggplot(radar_data, aes(x = objective, y = score, group = scenario_name, color = scenario_name)) +
        geom_polygon(aes(fill = scenario_name), alpha = 0.15, linewidth = 1) +  # Lower alpha
        geom_point(size = 3) +
        coord_radar() +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
        scale_color_manual(values = c(
          "Late-Stage Oncology Focus" = "#0078D4",
          "Fast-Track CNS" = "#107C10",
          "Cost-Efficient Rare Disease" = "#CA5010",
          "Vaccines Platform" = "#8764B8",
          "Biologics Heavy" = "#D13438",
          "Diversified Balanced" = "#767676",
          "Mixed Portfolio" = "#004578"
        )) +
        scale_fill_manual(values = c(
          "Late-Stage Oncology Focus" = "#0078D4",
          "Fast-Track CNS" = "#107C10",
          "Cost-Efficient Rare Disease" = "#CA5010",
          "Vaccines Platform" = "#8764B8",
          "Biologics Heavy" = "#D13438",
          "Diversified Balanced" = "#767676",
          "Mixed Portfolio" = "#004578"
        )) +
        labs(
          title = "Normalized Performance Across Strategic Objectives",
          subtitle = "Each axis is normalized across selected scenarios (min-max scaling)",
          x = NULL,
          y = "Normalized Score (0-1)",
          color = "Scenario",
          fill = "Scenario"
        ) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          legend.title = element_text(face = "bold"),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
          plot.subtitle = element_text(hjust = 0.5, size = 10, color = "#605E5C"),
          axis.text.x = element_text(size = 11, face = "bold")  # Increased from 10
        )
      
      girafe(
        ggobj = p,
        width_svg = 7,
        height_svg = 7,
        options = list(
          opts_sizing(rescale = TRUE, width = 1)
        )
      )
    })
    
    # -------------------------------------------------------------------------
    # OUTPUT: Comparison Table (Simple HTML)
    # -------------------------------------------------------------------------
    output$comparison_table <- renderUI({
      data <- selected_scenarios()
      req(nrow(data) >= 1)
      
      # Prepare table data
      table_data <- data %>%
        select(
          Scenario = scenario_name,
          `Total Assets` = total_assets,
          `Expected Approvals` = expected_approvals,
          `Total Cost` = total_cost_millions,
          `Avg Timeline` = avg_timeline_years,
          `Efficiency` = approvals_per_100M
        ) %>%
        mutate(
          `Expected Approvals` = round(as.numeric(`Expected Approvals`), 1),
          `Total Cost` = paste0("$", round(as.numeric(`Total Cost`) / 1000, 1), "B"),
          `Avg Timeline` = paste0(round(as.numeric(`Avg Timeline`), 1), " yrs"),
          `Efficiency` = round(as.numeric(`Efficiency`), 3)
        )
      
      # Create HTML table
      tags$table(
        class = "ui celled striped table",
        style = "font-size: 0.9em;",
        tags$thead(
          tags$tr(
            tags$th("Scenario", style = "background: #F8F9FA;"),
            tags$th("Assets", style = "background: #F8F9FA; text-align: center;"),
            tags$th("Approvals", style = "background: #F8F9FA; text-align: center;"),
            tags$th("Total Cost", style = "background: #F8F9FA; text-align: center;"),
            tags$th("Timeline", style = "background: #F8F9FA; text-align: center;"),
            tags$th("Efficiency", style = "background: #F8F9FA; text-align: center;")
          )
        ),
        tags$tbody(
          lapply(1:nrow(table_data), function(i) {
            tags$tr(
              tags$td(
                style = "font-weight: bold; color: #006B75;",
                table_data$Scenario[i]
              ),
              tags$td(
                style = "text-align: center;",
                table_data$`Total Assets`[i]
              ),
              tags$td(
                style = "text-align: center;",
                table_data$`Expected Approvals`[i]
              ),
              tags$td(
                style = "text-align: center;",
                table_data$`Total Cost`[i]
              ),
              tags$td(
                style = "text-align: center;",
                table_data$`Avg Timeline`[i]
              ),
              tags$td(
                style = "text-align: center;",
                table_data$Efficiency[i]
              )
            )
          })
        )
      )
    })
    
    # -------------------------------------------------------------------------
    # OUTPUT: Interpretation Text
    # -------------------------------------------------------------------------
    output$interpretation_text <- renderUI({
      data <- selected_scenarios()
      req(nrow(data) >= 2)
      
      # Calculate some insights
      max_approvals <- data %>% slice_max(as.numeric(expected_approvals), n = 1)
      min_cost <- data %>% slice_min(as.numeric(total_cost_millions), n = 1)
      fastest <- data %>% slice_min(as.numeric(avg_timeline_years), n = 1)
      most_efficient <- data %>% slice_max(as.numeric(approvals_per_100M), n = 1)
      
      tagList(
        p(
          style = "color: #323130; line-height: 1.8;",
          strong("Key Insights from Selected Scenarios:")
        ),
        tags$ul(
          style = "color: #323130; line-height: 1.8;",
          tags$li(
            strong("Maximum Approvals: "), 
            max_approvals$scenario_name, " (",
            round(as.numeric(max_approvals$expected_approvals), 1), " approvals)"
          ),
          tags$li(
            strong("Lowest Cost: "),
            min_cost$scenario_name, " ($",
            round(as.numeric(min_cost$total_cost_millions) / 1000, 1), "B)"
          ),
          tags$li(
            strong("Fastest Timeline: "),
            fastest$scenario_name, " (",
            round(as.numeric(fastest$avg_timeline_years), 1), " years)"
          ),
          tags$li(
            strong("Best Efficiency: "),
            most_efficient$scenario_name, " (",
            round(as.numeric(most_efficient$approvals_per_100M), 3), " approvals per $100M)"
          )
        ),
        p(
          style = "color: #605E5C; font-style: italic; margin-top: 1em;",
          "The radar chart shows where objectives align (scenarios overlap) versus diverge (scenarios separate). ",
          "Scenarios rarely excel at all objectives simultaneously—strategic choices involve trade-offs. ",
          "Output-maximizing strategies typically cluster together, while learning-focused portfolios trade ",
          "near-term performance for optionality and future flexibility."
        )
      )
    })
    
  })
}