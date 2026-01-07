# ============================================================================ #
# Module: Portfolio Builder
# Purpose: Interactive portfolio configuration with live outcome calculation
# ============================================================================ #

mod_portfolio_builder_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # CSS fix for sliders
    tags$style(HTML("
      /* Fix slider rendering */
      .irs {
        font-family: 'Segoe UI', sans-serif !important;
      }
      .irs-bar {
        background: #006B75 !important;
        border: none !important;
      }
      .irs-from, .irs-to, .irs-single {
        background: #006B75 !important;
      }
      .irs-handle {
        border: 2px solid #006B75 !important;
        background: white !important;
      }
      .irs-line {
        background: #D2D0CE !important;
        border: none !important;
      }
      /* Ensure proper spacing */
      .shiny-input-container {
        margin-bottom: 0 !important;
      }
    ")),
    
    div(
      style = "padding: 2em 0;",
      
      # Header
      h2("Interactive Portfolio Builder", style = "color: #006B75;"),
      p(
        style = "color: #605E5C; margin-bottom: 2em;",
        "Configure your custom portfolio allocation and compare projected outcomes against preset scenarios."
      ),
      
      # Two-column layout: Controls + Results
      div(
        class = "ui stackable two column grid",
        
        # ====================================================================
        # LEFT COLUMN: Portfolio Configuration Controls
        # ====================================================================
        div(
          class = "column",
          
          # Phase Allocation Panel
          div(
            class = "ui segment",
            style = "background: #F8F9FA;",
            h3("Phase Allocation", style = "color: #006B75; margin-top: 0;"),
            p(
              style = "color: #605E5C; font-size: 0.9em; margin-bottom: 1em;",
              "Distribute your portfolio across clinical phases. Total assets constrained to 50."
            ),
            
            # Preset Buttons
            div(
              style = "margin-bottom: 1.5em; padding-bottom: 1em; border-bottom: 1px solid #D2D0CE;",
              strong("Quick Presets:", style = "color: #323130; display: block; margin-bottom: 0.5em;"),
              div(
                class = "ui three buttons",
                style = "margin-top: 0.5em;",
                actionButton(
                  ns("preset_early"),
                  "Early-Stage Heavy",
                  class = "ui button",
                  style = "font-size: 0.85em;"
                ),
                actionButton(
                  ns("preset_balanced"),
                  "Balanced",
                  class = "ui button",
                  style = "font-size: 0.85em;"
                ),
                actionButton(
                  ns("preset_late"),
                  "Late-Stage Heavy",
                  class = "ui button",
                  style = "font-size: 0.85em;"
                )
              ),
              actionButton(
                ns("reset"),
                "Reset to Default",
                icon = icon("undo"),
                class = "ui basic button",
                style = "margin-top: 0.5em; font-size: 0.85em;"
              )
            ),
            
            # Phase 1 Slider
            div(
              style = "margin-bottom: 1.5em;",
              div(
                style = "display: flex; justify-content: space-between; margin-bottom: 0.5em;",
                strong("Phase 1:", style = "color: #323130;"),
                div(
                  style = "color: #006B75; font-weight: bold;",
                  uiOutput(ns("phase1_value"))
                )
              ),
              sliderInput(
                ns("phase1"),
                label = NULL,
                min = 0,
                max = 50,
                value = 15,
                step = 1,
                width = "100%",
                ticks = FALSE  # Remove tick marks for cleaner display
              )
            ),
            
            # Phase 2 Slider
            div(
              style = "margin-bottom: 1.5em;",
              div(
                style = "display: flex; justify-content: space-between; margin-bottom: 0.5em;",
                strong("Phase 2:", style = "color: #323130;"),
                div(
                  style = "color: #006B75; font-weight: bold;",
                  uiOutput(ns("phase2_value"))
                )
              ),
              sliderInput(
                ns("phase2"),
                label = NULL,
                min = 0,
                max = 50,
                value = 15,
                step = 1,
                width = "100%",
                ticks = FALSE
              )
            ),
            
            # Phase 3 Slider
            div(
              style = "margin-bottom: 1.5em;",
              div(
                style = "display: flex; justify-content: space-between; margin-bottom: 0.5em;",
                strong("Phase 3:", style = "color: #323130;"),
                div(
                  style = "color: #006B75; font-weight: bold;",
                  uiOutput(ns("phase3_value"))
                )
              ),
              sliderInput(
                ns("phase3"),
                label = NULL,
                min = 0,
                max = 50,
                value = 20,
                step = 1,
                width = "100%",
                ticks = FALSE
              )
            ),
            
            # Total Assets Display
            div(
              style = "padding: 1em; background: white; border-left: 4px solid #006B75; margin-top: 1em;",
              div(
                style = "display: flex; justify-content: space-between; align-items: center;",
                strong("Total Assets:", style = "color: #323130; font-size: 1.1em;"),
                div(
                  style = "font-size: 1.5em; font-weight: bold;",
                  uiOutput(ns("total_assets"), inline = TRUE)
                )
              ),
              div(
                style = "margin-top: 0.5em; font-size: 0.9em;",
                uiOutput(ns("constraint_message"), inline = TRUE)
              ),
              div(
                style = "margin-top: 0.5em; font-size: 0.85em; color: #605E5C; font-style: italic;",
                "Total must equal 50 assets across all three phases."
              )
            )
          ),
          
          # Strategy Settings Panel
          div(
            class = "ui segment",
            style = "background: #F8F9FA; margin-top: 1.5em;",
            h3("Strategy Settings", style = "color: #006B75; margin-top: 0;"),
            
            # Therapeutic Area
            div(
              style = "margin-bottom: 1.5em;",
              strong("Therapeutic Focus:", style = "color: #323130; display: block; margin-bottom: 0.5em;"),
              selectInput(
                ns("therapeutic_area"),
                label = NULL,
                choices = c(
                  "Overall (Industry Average)" = "Overall",
                  "Oncology" = "Oncology",
                  "Vaccines" = "Vaccines"
                ),
                selected = "Overall",
                width = "100%"
              ),
              div(
                style = "margin-top: 0.5em; font-size: 0.85em; color: #605E5C;",
                icon("info circle"),
                " Empirical data from published literature. Strategic scenarios (CNS, Rare Disease, etc.) available in Executive Brief."
              )
            ),
            
            # Cost Scenario
            div(
              strong("Cost Scenario:", style = "color: #323130; display: block; margin-bottom: 0.5em;"),
              selectInput(
                ns("cost_scenario"),
                label = NULL,
                choices = c(
                  "Conservative (Lower bound)" = "Conservative",
                  "Base Case (Industry average)" = "Base",
                  "Aggressive (Upper bound)" = "Aggressive"
                ),
                selected = "Base",
                width = "100%"
              )
            )
          )
        ),
        
        # ====================================================================
        # RIGHT COLUMN: Results Display
        # ====================================================================
        div(
          class = "column",
          
          # Custom Portfolio Results
          div(
            class = "ui segment",
            style = "background: #F0F6FC;",
            h3("Your Custom Portfolio", style = "color: #006B75; margin-top: 0;"),
            
            # KPI Grid
            div(
              class = "ui stackable two column grid",
              style = "margin-top: 1em;",
              
              # Expected Approvals
              div(
                class = "column",
                div(
                  class = "ui segment",
                  style = "text-align: center; padding: 1.5em;",
                  div(
                    style = "font-size: 2em; font-weight: bold; color: #006B75;",
                    uiOutput(ns("custom_approvals"))
                  ),
                  div(
                    style = "color: #323130; margin-top: 0.5em;",
                    "Expected Approvals"
                  ),
                  div(
                    style = "color: #605E5C; font-size: 0.85em; margin-top: 0.25em;",
                    uiOutput(ns("custom_approvals_delta"))
                  )
                )
              ),
              
              # Total Cost
              div(
                class = "column",
                div(
                  class = "ui segment",
                  style = "text-align: center; padding: 1.5em;",
                  div(
                    style = "font-size: 2em; font-weight: bold; color: #006B75;",
                    uiOutput(ns("custom_cost"))
                  ),
                  div(
                    style = "color: #323130; margin-top: 0.5em;",
                    "Total Cost"
                  ),
                  div(
                    style = "color: #605E5C; font-size: 0.85em; margin-top: 0.25em;",
                    uiOutput(ns("custom_cost_delta"))
                  )
                )
              ),
              
              # Timeline
              div(
                class = "column",
                div(
                  class = "ui segment",
                  style = "text-align: center; padding: 1.5em;",
                  div(
                    style = "font-size: 2em; font-weight: bold; color: #006B75;",
                    uiOutput(ns("custom_timeline"))
                  ),
                  div(
                    style = "color: #323130; margin-top: 0.5em;",
                    "Avg Time to Approval"
                  ),
                  div(
                    style = "color: #605E5C; font-size: 0.85em; margin-top: 0.25em;",
                    "Years to approval"
                  )
                )
              ),
              
              # Efficiency
              div(
                class = "column",
                div(
                  class = "ui segment",
                  style = "text-align: center; padding: 1.5em;",
                  div(
                    style = "font-size: 2em; font-weight: bold; color: #006B75;",
                    uiOutput(ns("custom_efficiency"))
                  ),
                  div(
                    style = "color: #323130; margin-top: 0.5em;",
                    "Capital Efficiency"
                  ),
                  div(
                    style = "color: #605E5C; font-size: 0.85em; margin-top: 0.25em;",
                    "Approvals per $100M"
                  )
                )
              )
            )
          ),
          
          # Comparison to Presets
          div(
            class = "ui segment",
            style = "margin-top: 1.5em;",
            h3("Comparison to Preset Scenarios", style = "color: #006B75; margin-top: 0;"),
            p(
              style = "color: #605E5C; font-size: 0.9em;",
              "See how your custom portfolio compares to our strategic presets."
            ),
            uiOutput(ns("comparison_table"))
          )
        )
      )
    )
  )
}

# ============================================================================ #
# SERVER
# ============================================================================ #

mod_portfolio_builder_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    
    # -------------------------------------------------------------------------
    # Preset Button Handlers
    # -------------------------------------------------------------------------
    
    # Early-stage heavy (35, 10, 5)
    observeEvent(input$preset_early, {
      updateSliderInput(session, "phase1", value = 35)
      updateSliderInput(session, "phase2", value = 10)
      updateSliderInput(session, "phase3", value = 5)
    })
    
    # Balanced (15, 15, 20)
    observeEvent(input$preset_balanced, {
      updateSliderInput(session, "phase1", value = 15)
      updateSliderInput(session, "phase2", value = 15)
      updateSliderInput(session, "phase3", value = 20)
    })
    
    # Late-stage heavy (5, 10, 35)
    observeEvent(input$preset_late, {
      updateSliderInput(session, "phase1", value = 5)
      updateSliderInput(session, "phase2", value = 10)
      updateSliderInput(session, "phase3", value = 35)
    })
    
    # Reset to default (Balanced)
    observeEvent(input$reset, {
      updateSliderInput(session, "phase1", value = 15)
      updateSliderInput(session, "phase2", value = 15)
      updateSliderInput(session, "phase3", value = 20)
    })
    
    # -------------------------------------------------------------------------
    # Baseline Results (for comparison deltas)
    # -------------------------------------------------------------------------
    baseline_results <- reactive({
      # Balanced portfolio: 15, 15, 20
      allocation <- c(phase1 = 15, phase2 = 15, phase3 = 20)
      
      calculate_portfolio_outcomes(
        portfolio_allocation = allocation,
        therapeutic_area = "Overall",
        cost_scenario = "Base",
        phase_transitions = app_data$phase_transitions,
        trial_durations = app_data$trial_durations,
        cost_estimates = app_data$cost_estimates
      )
    })
    
    # -------------------------------------------------------------------------
    # Reactive: Calculate total assets
    # -------------------------------------------------------------------------
    total_assets <- reactive({
      input$phase1 + input$phase2 + input$phase3
    })
    
    # -------------------------------------------------------------------------
    # Reactive: Calculate custom portfolio outcomes
    # -------------------------------------------------------------------------
    custom_results <- reactive({
      # Wait for inputs
      req(input$phase1, input$phase2, input$phase3)
      req(input$therapeutic_area, input$cost_scenario)
      
      # Create allocation vector
      allocation <- c(
        phase1 = input$phase1,
        phase2 = input$phase2,
        phase3 = input$phase3
      )
      
      # Run simulation
      calculate_portfolio_outcomes(
        portfolio_allocation = allocation,
        therapeutic_area = input$therapeutic_area,
        cost_scenario = input$cost_scenario,
        phase_transitions = app_data$phase_transitions,
        trial_durations = app_data$trial_durations,
        cost_estimates = app_data$cost_estimates
      )
    })
    
    # -------------------------------------------------------------------------
    # OUTPUTS: Phase Value Displays
    # -------------------------------------------------------------------------
    output$phase1_value <- renderText({
      paste(input$phase1, "assets")
    })
    
    output$phase2_value <- renderText({
      paste(input$phase2, "assets")
    })
    
    output$phase3_value <- renderText({
      paste(input$phase3, "assets")
    })
    
    # -------------------------------------------------------------------------
    # OUTPUT: Total Assets with Constraint Check
    # -------------------------------------------------------------------------
    output$total_assets <- renderUI({
      total <- total_assets()
      color <- if (total == 50) "#107C10" else if (total > 50) "#D13438" else "#605E5C"
      
      tags$span(
        style = paste0("color: ", color, ";"),
        as.character(total)
      )
    })
    
    output$constraint_message <- renderUI({
      total <- total_assets()
      
      if (total == 50) {
        div(style = "color: #107C10;", icon("check circle"), " Portfolio at target size")
      } else if (total > 50) {
        div(style = "color: #D13438;", icon("exclamation triangle"), " Portfolio exceeds 50 assets")
      } else {
        div(style = "color: #605E5C;", icon("info circle"), " ", 50 - total, " assets remaining")
      }
    })
    
    # -------------------------------------------------------------------------
    # OUTPUTS: Custom Portfolio KPIs
    # -------------------------------------------------------------------------
    output$custom_approvals <- renderText({
      results <- custom_results()
      format(round(results$expected_approvals, 1), nsmall = 1)
    })
    
    output$custom_approvals_delta <- renderUI({
      custom <- custom_results()
      baseline <- baseline_results()
      
      delta <- custom$expected_approvals - baseline$expected_approvals
      color <- if (delta > 0) "#107C10" else if (delta < 0) "#D13438" else "#605E5C"
      sign <- if (delta > 0) "+" else ""
      
      div(
        style = paste0("color: ", color, ";"),
        paste0(sign, round(delta, 1), " vs Balanced")
      )
    })
    
    output$custom_cost <- renderText({
      results <- custom_results()
      paste0("$", format(round(results$total_cost_millions / 1000, 1), nsmall = 1), "B")
    })
    
    output$custom_cost_delta <- renderUI({
      custom <- custom_results()
      baseline <- baseline_results()
      
      delta_billions <- (custom$total_cost_millions - baseline$total_cost_millions) / 1000
      # Note: Lower cost is better, so flip the color logic
      color <- if (delta_billions < 0) "#107C10" else if (delta_billions > 0) "#D13438" else "#605E5C"
      sign <- if (delta_billions > 0) "+" else ""
      
      div(
        style = paste0("color: ", color, ";"),
        paste0(sign, "$", round(abs(delta_billions), 1), "B vs Balanced")
      )
    })
    
    output$custom_timeline <- renderText({
      results <- custom_results()
      paste0(format(round(results$avg_timeline_years, 1), nsmall = 1), " yrs")
    })
    
    output$custom_efficiency <- renderText({
      results <- custom_results()
      efficiency <- (results$expected_approvals / results$total_cost_millions) * 100
      format(round(efficiency, 3), nsmall = 3)
    })
    
    # -------------------------------------------------------------------------
    # OUTPUT: Comparison Table (simplified)
    # -------------------------------------------------------------------------
    output$comparison_table <- renderUI({
      results <- custom_results()
      baseline <- baseline_results()
      
      # Determine closest preset based on phase allocation
      p1 <- input$phase1
      p3 <- input$phase3
      
      closest_preset <- if (p1 > 25) {
        "Early-Stage Heavy"
      } else if (p3 > 25) {
        "Late-Stage Heavy"  
      } else {
        "Balanced"
      }
      
      # Create comparison
      div(
        style = "padding: 1em; background: #F8F9FA; border-radius: 4px;",
        p(
          style = "color: #323130; margin-bottom: 0.5em;",
          strong("Your Portfolio:"), " ",
          round(results$expected_approvals, 1), " approvals, ",
          "$", round(results$total_cost_millions / 1000, 1), "B cost"
        ),
        p(
          style = "color: #006B75; margin-bottom: 1em; font-weight: 500;",
          icon("check circle"), " Closest preset: ", closest_preset
        ),
        p(
          style = "color: #605E5C; font-size: 0.9em; margin-bottom: 0;",
          "Use the Trade-off Explorer tab to compare against multiple preset scenarios simultaneously."
        )
      )
    })
    
  })
}