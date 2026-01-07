# ============================================================================ #
# Module: Portfolio Builder (Cleaned UI + Dynamic Logic)
# Purpose: Professional simulation with high-fidelity sliders
# ============================================================================ #

mod_methods_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
      /* 1. Fix the 'Crowded' Slider: Hide overlapping grid numbers */
      .irs-grid-text, .irs-grid-pol { display: none !important; }
      .irs-min, .irs-max { display: none !important; }
      .irs { margin-bottom: 5px !important; height: 40px !important; }
      
      /* 2. Professional Slider Branding */
      .irs-bar { background: #006B75 !important; border: none !important; }
      .irs-from, .irs-to, .irs-single { background: #006B75 !important; font-size: 11px !important; }
      .irs-handle { border: 2px solid #006B75 !important; background: white !important; width: 16px !important; height: 16px !important; top: 25px !important; }
      
      /* 3. Dashboard KPI Styling */
      .ui.statistic > .label { 
        color: #605E5C !important; 
        font-size: 0.75em !important; 
        text-transform: uppercase !important;
        letter-spacing: 1px !important;
      }
      .ui.statistic > .value { color: #006B75 !important; font-weight: 700 !important; }
      .delta-text { font-size: 0.85em; margin-top: 0.4em; font-weight: 500; }
    ")),
    
    div(
      style = "padding: 1em 0;",
      h2("Interactive Portfolio Builder", style = "color: #006B75;"),
      p(style = "color: #605E5C; margin-bottom: 2em;", "Adjust allocation to see trade-offs vs the Balanced benchmark."),
      
      div(
        class = "ui stackable grid",
        
        # --- LEFT COLUMN: INPUTS (6-wide) ---
        div(
          class = "six wide column",
          div(
            class = "ui raised segment",
            style = "border-top: 3px solid #006B75;",
            h3(class = "ui header", icon("sliders"), "Phase Allocation"),
            
            # Refined Preset Buttons
            div(class = "ui three mini compact basic buttons", style = "margin: 1em 0; width: 100%;",
                actionButton(ns("preset_early"), "Early"),
                actionButton(ns("preset_balanced"), "Balanced"),
                actionButton(ns("preset_late"), "Late")),
            
            lapply(1:3, function(i) {
              div(style = "margin-bottom: 1.2em;",
                  div(style = "display: flex; justify-content: space-between;",
                      strong(paste("Phase", i), style = "color: #323130;"),
                      uiOutput(ns(paste0("phase", i, "_val")), inline = TRUE, style = "color:#006B75; font-weight:bold;")),
                  sliderInput(ns(paste0("phase", i)), label = NULL, min = 0, max = 50, value = 15, width = "100%"))
            }),
            
            uiOutput(ns("total_assets_card"))
          ),
          
          div(class = "ui segment", style = "background: #F8F9FA;",
              selectInput(ns("therapeutic_area"), "Therapeutic Focus", choices = c("Overall", "Oncology", "Vaccines"), width = "100%"),
              selectInput(ns("cost_scenario"), "Cost Scenario", choices = c("Conservative", "Base", "Aggressive"), selected = "Base", width = "100%"))
        ),
        
        # --- RIGHT COLUMN: OUTPUTS (10-wide) ---
        div(
          class = "ten wide column",
          div(
            class = "ui raised segment",
            h3(class = "ui header", style = "color: #006B75;", "Projected Strategic Outcomes"),
            div(class = "ui divider"),
            
            div(class = "ui four column stackable grid", style = "text-align: center; padding: 1em 0;",
                # Approvals
                div(class = "column",
                    div(class = "ui tiny statistic", 
                        div(class = "value", uiOutput(ns("custom_approvals"))),
                        div(class = "label", "Approvals")),
                    uiOutput(ns("custom_approvals_delta"))),
                # Cost
                div(class = "column",
                    div(class = "ui tiny statistic", 
                        div(class = "value", uiOutput(ns("custom_cost"))),
                        div(class = "label", "Total Cost")),
                    uiOutput(ns("custom_cost_delta"))),
                # Timeline
                div(class = "column",
                    div(class = "ui tiny statistic", 
                        div(class = "value", uiOutput(ns("custom_timeline"))),
                        div(class = "label", "Avg Years")),
                    div(class = "delta-text", style="color:#605E5C;", "to approval")),
                # Efficiency
                div(class = "column",
                    div(class = "ui tiny statistic", 
                        div(class = "value", uiOutput(ns("custom_efficiency"))),
                        div(class = "label", "Efficiency")),
                    div(class = "delta-text", style="color:#605E5C;", "per $100M"))
            )
          ),
          
          uiOutput(ns("dynamic_insight_note"))
        )
      )
    )
  )
}

mod_methods_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    
    # Core Logic
    total_assets <- reactive({ (input$phase1 %||% 0) + (input$phase2 %||% 0) + (input$phase3 %||% 0) })
    
    baseline_results <- reactive({
      calculate_portfolio_outcomes(c(phase1=15, phase2=15, phase3=20), "Overall", "Base",
                                   app_data$phase_transitions, app_data$trial_durations, app_data$cost_estimates)
    })
    
    custom_results <- reactive({
      req(input$phase1, input$phase2, input$phase3)
      calculate_portfolio_outcomes(c(phase1=input$phase1, phase2=input$phase2, phase3=input$phase3),
                                   input$therapeutic_area, input$cost_scenario,
                                   app_data$phase_transitions, app_data$trial_durations, app_data$cost_estimates)
    })
    
    # Value Labels
    output$phase1_val <- renderText({ paste(input$phase1, "assets") })
    output$phase2_val <- renderText({ paste(input$phase2, "assets") })
    output$phase3_val <- renderText({ paste(input$phase3, "assets") })
    
    # KPIs with Context
    output$custom_approvals <- renderUI({ tags$span(format(round(custom_results()$expected_approvals, 1), nsmall = 1)) })
    output$custom_approvals_delta <- renderUI({
      delta <- custom_results()$expected_approvals - baseline_results()$expected_approvals
      color <- if(delta > 0) "#107C10" else if(delta < 0) "#D13438" else "#605E5C"
      div(class = "delta-text", style = paste0("color:", color), paste0(if(delta > 0) "+" else "", round(delta, 1), " vs Balanced"))
    })
    
    output$custom_cost <- renderUI({ tags$span(paste0("$", round(custom_results()$total_cost_millions/1000, 1), "B")) })
    output$custom_cost_delta <- renderUI({
      delta_b <- (custom_results()$total_cost_millions - baseline_results()$total_cost_millions) / 1000
      color <- if(delta_b < 0) "#107C10" else if(delta_b > 0) "#D13438" else "#605E5C"
      div(class = "delta-text", style = paste0("color:", color), paste0(if(delta_b > 0) "+" else "", "$", abs(round(delta_b, 1)), "B vs Balanced"))
    })
    
    output$custom_timeline <- renderUI({ tags$span(round(custom_results()$avg_timeline_years, 1)) })
    output$custom_efficiency <- renderUI({ tags$span(round((custom_results()$expected_approvals / custom_results()$total_cost_millions) * 100, 3)) })
    
    # Dynamic Insight
    output$dynamic_insight_note <- renderUI({
      p1 <- input$phase1; p3 <- input$phase3
      strategy <- if(p1 > 25) "Early-Stage Heavy" else if(p3 > 25) "Late-Stage Heavy" else "Balanced"
      message <- switch(strategy,
                        "Early-Stage Heavy" = "Focusing on early phases maximizes long-term optionality but delays clinical approvals.",
                        "Late-Stage Heavy"  = "Prioritizing Phase 3 maximizes immediate delivery and predictability of the current portfolio.",
                        "Balanced"          = "Maintaining this distribution ensures a sustainable cadence of R&D delivery.")
      
      div(class = "ui icon info message", style = "margin-top: 1.5em; background: #F0F6FC; border: none;",
          icon("lightbulb", style = "color: #006B75;"),
          div(class = "content", div(class = "header", paste("Strategic Note:", strategy)), p(message)))
    })
    
    # Constraint Card
    output$total_assets_card <- renderUI({
      total <- total_assets(); is_valid <- (total == 50)
      div(class = paste("ui message", if(is_valid) "success" else "warning"), style="padding:0.8em; margin-top:1em;",
          div(class="header", style="display:flex; justify-content:space-between;", span("Total Assets"), span(total)),
          p(style="font-size:0.85em; margin-top:5px;", if(is_valid) "Allocation target reached." else "Adjust to reach 50 assets."))
    })
    
    # Presets
    observeEvent(input$preset_early, { updateSliderInput(session, "phase1", value = 35); updateSliderInput(session, "phase2", value = 10); updateSliderInput(session, "phase3", value = 5) })
    observeEvent(input$preset_balanced, { updateSliderInput(session, "phase1", value = 15); updateSliderInput(session, "phase2", value = 15); updateSliderInput(session, "phase3", value = 20) })
    observeEvent(input$preset_late, { updateSliderInput(session, "phase1", value = 5); updateSliderInput(session, "phase2", value = 10); updateSliderInput(session, "phase3", value = 35) })
  })
}