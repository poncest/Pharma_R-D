# ============================================================================ #
# Module: Executive Brief
# Purpose: Rapid decision support with objective-driven recommendations
# ============================================================================ #

mod_executive_brief_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Custom CSS for radio button selection (shiny.semantic compatible)
    tags$style(HTML("
      /* Radio button container */
      .radio {
        margin: 0.5em 0 !important;
        padding: 0.75em 1em !important;
        border-radius: 4px;
        transition: all 0.2s ease;
      }
      /* Prevent line breaks in radio labels */
      .radio label {
        white-space: nowrap !important;
        display: inline !important;
      }
      /* Selected radio button background */
      .radio:has(input[type='radio']:checked) {
        background-color: #E6F4F1 !important;
        border-left: 3px solid #006B75 !important;
        padding-left: calc(1em - 3px) !important;
      }
      /* Change radio button circle color when checked */
      input[type='radio']:checked {
        accent-color: #006B75 !important;
      }
      /* Selected label styling */
      input[type='radio']:checked ~ label {
        color: #006B75 !important;
        font-weight: 600 !important;
      }
      /* Default label */
      input[type='radio'] ~ label {
        transition: all 0.2s ease;
        cursor: pointer;
      }
      /* Hover effect */
      .radio:hover {
        background-color: #F5F5F5 !important;
      }
    ")),
    
    div(
      style = "padding: 2em 0;",
      
      # Introduction
      div(
        class = "ui message info",
        icon("info circle"),
        div(
          class = "content",
          div(class = "header", "Strategic Decision Support"),
          p(
            "Select your primary strategic objective to see which portfolio configuration ",
            "aligns best under base assumptions—and what trade-offs that choice entails."
          )
        )
      ),
      
      # Portfolio Assumptions Context
      div(
        class = "ui segment",
        style = "background: #F8F9FA; border-left: 4px solid #006B75; margin-top: 1.5em;",
        h4("Portfolio Assumptions", style = "color: #006B75; margin-top: 0;"),
        div(
          style = "color: #323130;",
          div(
            style = "margin-bottom: 1em;",
            strong("Therapeutic Areas:"),
            div(
              style = "margin-left: 1em; margin-top: 0.5em; line-height: 1.6;",
              "• ", strong("Empirical data:"), " Overall, Oncology, Vaccines (published literature)",
              br(),
              "• ", strong("Modeled scenarios:"), " CNS Fast-Track, Rare Disease, Biologics, Diversified (strategic assumptions applied to empirical baselines)"
            )
          ),
          div(
            style = "display: grid; grid-template-columns: 1fr 1fr; gap: 1em;",
            div(
              strong("Time Horizon:"), " 10 years"
            ),
            div(
              strong("Success Rates:"), " Industry average (Norstella 2024, Wong 2019)"
            ),
            div(
              strong("Cost Models:"), " Conservative, Base, Aggressive scenarios"
            ),
            div(
              strong("Approach:"), " Literature-derived parameters with strategic multipliers"
            )
          )
        )
      ),
      
      # Strategic Priority Selector
      div(
        class = "ui segment",
        style = "background: #F0F6FC; margin-top: 1.5em;",
        h3("Strategic Priority", 
           class = "section-header",
           style = "color: #006B75; border-bottom: 2px solid #006B75; padding-bottom: 0.5em;"),
        div(
          style = "color: #605E5C; margin-bottom: 1em; font-style: italic;",
          "What matters most right now?"
        ),
        
        # AGGRESSIVE CSS to override shiny.semantic checkbox styling
        tags$style(HTML("
          /* Force all radio inputs to look like radio buttons */
          input[type='radio'] {
            -webkit-appearance: radio !important;
            -moz-appearance: radio !important;
            appearance: radio !important;
            width: 18px !important;
            height: 18px !important;
            min-width: 18px !important;
            min-height: 18px !important;
            border-radius: 50% !important;
            border: 2px solid #767676 !important;
            background: white !important;
            margin-right: 10px !important;
            cursor: pointer !important;
            position: relative !important;
            vertical-align: middle !important;
          }
          
          /* Radio button when checked */
          input[type='radio']:checked {
            border-color: #0078D4 !important;
            background: white !important;
          }
          
          input[type='radio']:checked::before {
            content: '' !important;
            display: block !important;
            width: 10px !important;
            height: 10px !important;
            border-radius: 50% !important;
            background: #0078D4 !important;
            position: absolute !important;
            top: 50% !important;
            left: 50% !important;
            transform: translate(-50%, -50%) !important;
          }
          
          /* Override shiny.semantic's checkbox-like styling */
          .ui.radio.checkbox input[type='radio'] {
            opacity: 1 !important;
            z-index: 1 !important;
          }
          
          .ui.radio.checkbox label:before,
          .ui.radio.checkbox label:after {
            display: none !important;
          }
          
          /* Ensure labels are clickable and aligned */
          .radio label {
            cursor: pointer !important;
            font-weight: normal !important;
            padding-left: 0 !important;
            display: inline-flex !important;
            align-items: center !important;
          }
        ")),
        
        radioButtons(
          inputId = ns("objective"),
          label = NULL,
          choices = c(
            "🎯 Drive Near-Term Approvals" = "max_approvals",
            "🛡️ Minimize Execution Risk" = "predictability",
            "🧪 Build Long-Term Optionality" = "max_learning",
            "⚡ Accelerate Time to Market" = "speed",
            "💰 Maximize Capital Efficiency" = "efficiency"
          ),
          selected = "max_approvals"
        )
      ),
      
      # Two-Column Layout: Scenario Overview + KPIs
      h3("Recommended Portfolio Configuration", 
         class = "section-header",
         style = "color: #006B75; border-bottom: 2px solid #006B75; padding-bottom: 0.5em;"),
      
      div(
        class = "ui stackable two column grid",
        style = "margin-bottom: 2em;",
        
        # =====================================================================
        # LEFT COLUMN: Scenario Overview
        # =====================================================================
        div(
          class = "column",
          div(
            class = "ui segment",
            style = "padding: 2em; min-height: 400px;",
            
            # Best For Tag
            div(
              style = "display: inline-block; padding: 0.5em 1em; background: linear-gradient(135deg, #006B75 0%, #004A50 100%); color: white; border-radius: 20px; font-size: 0.85em; font-weight: 600; margin-bottom: 1em;",
              icon("trophy"),
              " BEST FOR: ",
              uiOutput(ns("best_for_tag"), inline = TRUE)
            ),
            
            # Scenario Name
            h3(
              uiOutput(ns("scenario_name")),
              style = "color: #006B75; margin-top: 0.5em;"
            ),
            
            # Portfolio Composition
            div(
              style = "margin-top: 1.5em;",
              h4("Portfolio Composition:", style = "color: #323130; margin-bottom: 0.5em;"),
              uiOutput(ns("portfolio_composition"))
            ),
            
            # Therapeutic Focus
            div(
              style = "margin-top: 1.5em;",
              h4("Therapeutic Focus:", style = "color: #323130; margin-bottom: 0.5em;"),
              uiOutput(ns("therapeutic_focus"))
            ),
            
            # Why This Wins
            div(
              style = "margin-top: 1.5em;",
              h4("Why This Scenario Wins:", style = "color: #323130; margin-bottom: 0.5em;"),
              div(
                style = "color: #107C10; font-weight: 500;",
                uiOutput(ns("why_wins"))
              )
            ),
            
            # What You're Giving Up
            div(
              style = "margin-top: 1.5em;",
              h4("What You're Giving Up:", style = "color: #323130; margin-bottom: 0.5em;"),
              div(
                style = "color: #D13438;",
                uiOutput(ns("trade_offs"))
              )
            )
          )
        ),
        
        # =====================================================================
        # RIGHT COLUMN: KPI Cards (Stacked Vertically)
        # =====================================================================
        div(
          class = "column",
          
          # KPI 1: Expected Approvals
          div(
            class = "ui segment",
            style = "text-align: center; padding: 1.5em; margin-bottom: 1em;",
            div(
              style = "font-size: 2.5em; font-weight: bold; color: #006B75;",
              uiOutput(ns("kpi_approvals"))
            ),
            div(
              style = "font-size: 1em; color: #323130; margin-top: 0.5em;",
              "Expected Approvals"
            ),
            div(
              style = "font-size: 0.85em; color: #605E5C; margin-top: 0.25em;",
              uiOutput(ns("kpi_approvals_context"))
            )
          ),
          
          # KPI 2: Total Cost
          div(
            class = "ui segment",
            style = "text-align: center; padding: 1.5em; margin-bottom: 1em;",
            div(
              style = "font-size: 2.5em; font-weight: bold; color: #006B75;",
              uiOutput(ns("kpi_cost"))
            ),
            div(
              style = "font-size: 1em; color: #323130; margin-top: 0.5em;",
              "Total Cost"
            ),
            div(
              style = "font-size: 0.85em; color: #605E5C; margin-top: 0.25em;",
              "Probability-weighted"
            )
          ),
          
          # KPI 3: Avg Timeline
          div(
            class = "ui segment",
            style = "text-align: center; padding: 1.5em; margin-bottom: 1em;",
            div(
              style = "font-size: 2.5em; font-weight: bold; color: #006B75;",
              uiOutput(ns("kpi_timeline"))
            ),
            div(
              style = "font-size: 1em; color: #323130; margin-top: 0.5em;",
              "Avg Time to Approval"
            ),
            div(
              style = "font-size: 0.85em; color: #605E5C; margin-top: 0.25em;",
              uiOutput(ns("kpi_timeline_context"))
            )
          ),
          
          # KPI 4: Efficiency
          div(
            class = "ui segment",
            style = "text-align: center; padding: 1.5em;",
            div(
              style = "font-size: 2.5em; font-weight: bold; color: #006B75;",
              uiOutput(ns("kpi_efficiency"))
            ),
            div(
              style = "font-size: 1em; color: #323130; margin-top: 0.5em;",
              "Capital Efficiency"
            ),
            div(
              style = "font-size: 0.85em; color: #605E5C; margin-top: 0.25em;",
              uiOutput(ns("kpi_efficiency_context"))
            )
          )
        )
      ),
      
      # Recommendation Card
      h3("Strategic Recommendation", 
         class = "section-header",
         style = "color: #006B75; border-bottom: 2px solid #006B75; padding-bottom: 0.5em;"),
      div(
        class = "ui segment",
        style = "background: #F0F6FC; border-left: 4px solid #006B75;",
        uiOutput(ns("recommendation_content"))
      ),
      
      # Trade-off Callout
      h3("What You're Prioritizing", 
         class = "section-header",
         style = "color: #006B75; border-bottom: 2px solid #006B75; padding-bottom: 0.5em;"),
      div(
        class = "ui message warning",
        icon("exchange"),
        div(
          class = "content",
          uiOutput(ns("tradeoff_content"))
        )
      ),
      
      # Disclaimer
      div(
        class = "ui message",
        style = "background: #F8F8F8; border: 1px solid #D2D0CE;",
        p(
          style = "color: #605E5C; font-size: 0.9em; margin: 0;",
          strong("Important:"),
          " This recommendation is based on industry-average parameters from published ",
          "literature (Norstella 2024, Wong 2019, JAMA 2024). Actual company-specific ",
          "parameters, competitive dynamics, and therapeutic area nuances may materially ",
          "affect outcomes. This is a strategic framework, not a prescription."
        )
      )
    )
  )
}

mod_executive_brief_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    
    # =========================================================================
    
    # -------------------------------------------------------------------------
    # REACTIVE: Recommended Scenario Based on Selected Objective
    # -------------------------------------------------------------------------
    recommended_scenario <- reactive({
      # FORCE reactivity on input change
      objective_value <- input$objective
      req(objective_value)
      
      cat("\n>>> REACTIVE FIRING - Objective:", objective_value, "<<<\n")
      
      # Map UI selection to simulation parameter
      local_objective_map <- list(
        "max_approvals" = "maximize_approvals",
        "predictability" = "predictability",
        "max_learning" = "max_learning",
        "speed" = "speed",
        "efficiency" = "efficiency"
      )
      
      local_selected_objective <- local_objective_map[[objective_value]]
      cat("Mapped to:", local_selected_objective, "\n")
      
      # Define all 7 possible scenarios
      local_scenarios <- data.frame(
        scenario_name = c(
          "Late-Stage Oncology Focus",
          "Fast-Track CNS",
          "Cost-Efficient Rare Disease",
          "Vaccines Platform",
          "Biologics Heavy (Early-Stage)",
          "Diversified Balanced",
          "Mixed Portfolio"
        ),
        portfolio_allocation = c(
          "70% Oncology late-stage",
          "60% CNS early-stage",
          "Rare Disease focused",
          "Vaccines platform",
          "Biologics early-stage heavy",
          "Equal across all areas",
          "Balanced mix"
        ),
        therapeutic_area = c(
          "Oncology", 
          "CNS", 
          "Rare Disease", 
          "Vaccines",
          "Biologics",
          "Diversified",
          "Mixed"
        ),
        stringsAsFactors = FALSE
      )
      
      # Run Phase 1 for each scenario
      local_results <- vector("list", nrow(local_scenarios))
      
      for (i in 1:nrow(local_scenarios)) {
        local_sim_result <- simulate_portfolio_phase1(
          portfolio_allocation = local_scenarios$portfolio_allocation[i],
          therapeutic_area = local_scenarios$therapeutic_area[i],
          primary_objective = local_selected_objective,
          base_params = app_data$base_params,
          ta_params = app_data$ta_params,
          phase_params = app_data$phase_params
        )
        
        # Store result with scenario metadata
        local_results[[i]] <- c(
          scenario_name = local_scenarios$scenario_name[i],
          portfolio_allocation = local_scenarios$portfolio_allocation[i],
          therapeutic_area = local_scenarios$therapeutic_area[i],
          local_sim_result  # This is already a flat list
        )
      }
      
      # DEBUG: Show all scenario results WITH ALL METRICS
      for (i in 1:length(local_results)) {
        cat(sprintf("Scenario %d: %s\n", i, local_results[[i]]$scenario_name))
        cat(sprintf("  Approvals: %.2f | Cost: $%.1fB | Timeline: %.1f yrs\n",
                    as.numeric(local_results[[i]]$expected_approvals),
                    as.numeric(local_results[[i]]$total_cost_millions) / 1000,
                    as.numeric(local_results[[i]]$avg_timeline_years)))
        cat(sprintf("  Approvals/Asset: %.3f | Approvals/$100M: %.3f | Approvals/Year: %.3f\n",
                    as.numeric(local_results[[i]]$approvals_per_asset),
                    as.numeric(local_results[[i]]$approvals_per_100M),
                    as.numeric(local_results[[i]]$approvals_per_year)))
      }
      
      # =========================================================================
      # NORMALIZED SELECTION - Scale each metric to [0,1] for fair comparison
      # =========================================================================
      
      # Helper function: Normalize values to 0-1 scale
      normalize <- function(x) {
        if (length(unique(x)) == 1) return(rep(0.5, length(x)))  # All same
        (x - min(x)) / (max(x) - min(x))
      }
      
      local_best_idx <- if (local_selected_objective == "maximize_approvals") {
        # Maximize total approvals
        values <- sapply(local_results, function(x) as.numeric(x["expected_approvals"]))
        normalized <- normalize(values)
        idx <- which.max(normalized)
        cat("\n>>> MAXIMIZE APPROVALS <<<\n")
        idx
        
      } else if (local_selected_objective == "speed") {
        # Maximize approvals per year (fastest pipeline)
        values <- sapply(local_results, function(x) as.numeric(x["approvals_per_year"]))
        normalized <- normalize(values)
        idx <- which.max(normalized)
        cat("\n>>> SPEED (Approvals/Year) <<<\n")
        idx
        
      } else if (local_selected_objective == "efficiency") {
        # Maximize approvals per $100M (capital efficiency)
        values <- sapply(local_results, function(x) as.numeric(x["approvals_per_100M"]))
        normalized <- normalize(values)
        idx <- which.max(normalized)
        cat("\n>>> EFFICIENCY (Approvals/$100M) <<<\n")
        idx
        
      } else if (local_selected_objective == "predictability") {
        # Maximize approvals per asset (per-asset consistency)
        values <- sapply(local_results, function(x) as.numeric(x["approvals_per_asset"]))
        normalized <- normalize(values)
        idx <- which.max(normalized)
        cat("\n>>> PREDICTABILITY (Approvals/Asset) <<<\n")
        idx
        
      } else {  # max_learning
        # Composite score with NORMALIZED metrics (fair weighting)
        approvals_norm <- normalize(sapply(local_results, function(x) as.numeric(x["expected_approvals"])))
        efficiency_norm <- normalize(sapply(local_results, function(x) as.numeric(x["approvals_per_100M"])))
        predict_norm <- normalize(sapply(local_results, function(x) as.numeric(x["approvals_per_asset"])))
        
        # Weighted composite: balance across dimensions
        composite_scores <- approvals_norm * 0.3 + efficiency_norm * 0.4 + predict_norm * 0.3
        idx <- which.max(composite_scores)
        cat("\n>>> MAX LEARNING (Composite) <<<\n")
        idx
      }
      
      local_top_scenario <- local_results[[local_best_idx]]
      
      cat("\n>>> RETURNING SCENARIO:", local_top_scenario$scenario_name)
      cat(" | Approvals:", round(as.numeric(local_top_scenario$expected_approvals), 1))
      cat(" | Cost: $", round(as.numeric(local_top_scenario$total_cost_millions)/1000, 1), "B <<<\n\n")
      
      # FIX: Return flat structure directly - no nested $outcomes
      # Phase 1 already returns: expected_approvals, total_cost_millions, etc.
      local_top_scenario
    })
    
    # -------------------------------------------------------------------------
    # Helper: Return ALL scenario results for comparison
    # -------------------------------------------------------------------------
    all_scenario_results <- reactive({
      local_selected_objective <- input$objective
      
      # Define scenarios (same as in recommended_scenario)
      local_scenarios <- data.frame(
        scenario_name = c(
          "Late-Stage Oncology Focus",
          "Fast-Track CNS",
          "Cost-Efficient Rare Disease",
          "Vaccines Platform",
          "Biologics Heavy (Early-Stage)",
          "Diversified Balanced",
          "Mixed Portfolio"
        ),
        portfolio_allocation = c(
          "70% Oncology late-stage",
          "60% CNS early-stage",
          "Rare Disease focused",
          "Vaccines platform",
          "Biologics early-stage heavy",
          "Equal across all areas",
          "Balanced mix"
        ),
        therapeutic_area = c(
          "Oncology", 
          "CNS", 
          "Rare Disease", 
          "Vaccines",
          "Biologics",
          "Diversified",
          "Mixed"
        ),
        stringsAsFactors = FALSE
      )
      
      # Run Phase 1 for each scenario
      local_results <- vector("list", nrow(local_scenarios))
      
      for (i in 1:nrow(local_scenarios)) {
        local_sim_result <- simulate_portfolio_phase1(
          portfolio_allocation = local_scenarios$portfolio_allocation[i],
          therapeutic_area = local_scenarios$therapeutic_area[i],
          primary_objective = local_selected_objective,
          base_params = app_data$base_params,
          ta_params = app_data$ta_params,
          phase_params = app_data$phase_params
        )
        
        local_results[[i]] <- c(
          scenario_name = local_scenarios$scenario_name[i],
          portfolio_allocation = local_scenarios$portfolio_allocation[i],
          therapeutic_area = local_scenarios$therapeutic_area[i],
          local_sim_result
        )
      }
      
      local_results
    })
    
    # -------------------------------------------------------------------------
    # SCENARIO DETAIL OUTPUTS (Left Column)
    # -------------------------------------------------------------------------
    
    # Scenario Name
    output$scenario_name <- renderText({
      scenario <- recommended_scenario()
      as.character(scenario$scenario_name)
    })
    
    # Portfolio Composition
    output$portfolio_composition <- renderUI({
      scenario <- recommended_scenario()
      
      # Extract phase allocation from portfolio_allocation string
      # This is a simplified display - adjust based on actual data structure
      tagList(
        div(style = "color: #323130; line-height: 1.8;",
            tags$ul(
              style = "margin-left: 1em;",
              tags$li(strong("Total Assets: "), "15-50 (varies by scenario)"),
              tags$li(strong("Phase Distribution: "), "Early, Mid, or Late-stage focus"),
              tags$li(strong("Cost Scenario: "), "Conservative, Base, or Aggressive")
            )
        )
      )
    })
    
    # Best For Tag
    output$best_for_tag <- renderText({
      objective_labels <- c(
        "max_approvals" = "Maximizing Near-Term Approvals",
        "predictability" = "Minimizing Execution Risk",
        "max_learning" = "Building Long-Term Optionality",
        "speed" = "Accelerating Time to Market",
        "efficiency" = "Maximizing Capital Efficiency"
      )
      objective_labels[input$objective]
    })
    
    # Therapeutic Focus  
    output$therapeutic_focus <- renderUI({
      scenario <- recommended_scenario()
      ta <- as.character(scenario$therapeutic_area)
      
      # Map therapeutic areas to descriptions
      ta_desc <- list(
        "Oncology" = "Oncology (70%), Mixed (30%)",
        "CNS" = "CNS (60%), Mixed (40%)",
        "Rare Disease" = "Rare Disease focused",
        "Vaccines" = "Vaccines platform",
        "Biologics" = "Biologics (early-stage heavy)",
        "Diversified" = "Balanced across all areas",
        "Mixed" = "Balanced therapeutic mix"
      )
      
      focus_text <- ta_desc[[ta]]
      if (is.null(focus_text)) focus_text <- ta
      
      div(style = "color: #323130;",
          tags$ul(
            style = "margin-left: 1em;",
            tags$li(focus_text)
          )
      )
    })
    
    # Why This Wins
    output$why_wins <- renderUI({
      obj <- input$objective
      scenario <- recommended_scenario()
      
      win_text <- if (obj == "max_approvals") {
        paste0("✓ Highest expected approvals under current assumptions (", round(as.numeric(scenario$expected_approvals), 1), ")")
      } else if (obj == "predictability") {
        paste0("✓ Highest per-asset success rate under current assumptions (", round(as.numeric(scenario$approvals_per_asset) * 100, 1), "%)")
      } else if (obj == "speed") {
        paste0("✓ Fastest approval rate under current assumptions (", round(as.numeric(scenario$approvals_per_year), 2), " per year)")
      } else if (obj == "efficiency") {
        paste0("✓ Best capital efficiency under current assumptions (", round(as.numeric(scenario$approvals_per_100M), 3), " approvals per $100M)")
      } else {  # max_learning
        "✓ Balanced composite score across all metrics under current assumptions"
      }
      
      div(style = "font-size: 1.1em;", win_text)
    })
    
    # What You're Giving Up (renamed from Strategic Trade-offs)
    output$trade_offs <- renderUI({
      obj <- input$objective
      scenario <- recommended_scenario()
      
      cost <- as.numeric(scenario$total_cost_millions) / 1000
      timeline <- as.numeric(scenario$avg_timeline_years)
      efficiency <- as.numeric(scenario$approvals_per_100M)
      
      trade_off_items <- list()
      
      if (obj == "max_approvals") {
        trade_off_items <- list(
          tags$li("⚠ Highest cost ($", round(cost, 1), "B)"),
          tags$li("⚠ Longest timeline (", round(timeline, 1), " years)"),
          tags$li("⚠ Lower capital efficiency")
        )
      } else if (obj == "efficiency") {
        trade_off_items <- list(
          tags$li("⚠ Fewer total approvals"),
          tags$li("⚠ Smaller portfolio size"),
          tags$li("⚠ Limited therapeutic diversity")
        )
      } else if (obj == "speed") {
        trade_off_items <- list(
          tags$li("⚠ Higher early-stage attrition risk"),
          tags$li("⚠ Fewer late-stage assets"),
          tags$li("⚠ Moderate total approvals")
        )
      } else if (obj == "predictability") {
        trade_off_items <- list(
          tags$li("⚠ Smallest portfolio size"),
          tags$li("⚠ Limited therapeutic breadth"),
          tags$li("⚠ Lower total output")
        )
      } else {  # max_learning
        trade_off_items <- list(
          tags$li("⚠ Higher early-stage attrition"),
          tags$li("⚠ Longer development cycles"),
          tags$li("⚠ Higher capital requirements")
        )
      }
      
      tags$ul(
        style = "margin-left: 1em; line-height: 1.8;",
        trade_off_items
      )
    })
    
    # -------------------------------------------------------------------------
    # KPI OUTPUTS (Fixed to use flat structure)
    # -------------------------------------------------------------------------
    
    # Expected Approvals
    output$kpi_approvals <- renderText({
      # Force reactivity on objective change
      input$objective
      val <- as.numeric(recommended_scenario()$expected_approvals)
      format(round(val, 1), nsmall = 1)
    })
    
    output$kpi_approvals_context <- renderUI({
      obj <- input$objective
      current_val <- as.numeric(recommended_scenario()$expected_approvals)
      all_vals <- sapply(all_scenario_results(), function(x) as.numeric(x$expected_approvals))
      
      # Calculate rank
      rank <- sum(all_vals > current_val) + 1
      total <- length(all_vals)
      
      # Determine bar color and width
      bar_pct <- ((total - rank + 1) / total) * 100
      bar_color <- if (rank <= 2) "#107C10" else if (rank >= 6) "#D13438" else "#CA5010"
      
      # Text based on objective
      base_text <- if (obj == "max_approvals") {
        "Over 10-year horizon • ★ Highest total approvals"
      } else if (obj == "predictability") {
        "Over 10-year horizon • ★ Best per-asset consistency"
      } else {
        "Over 10-year horizon"
      }
      
      tagList(
        div(style = "margin-bottom: 0.5em;", base_text),
        div(
          style = "width: 100%; height: 6px; background: #E0E0E0; border-radius: 3px; margin: 0.5em 0; position: relative;",
          div(
            style = paste0("width: ", bar_pct, "%; height: 100%; background: ", bar_color, "; border-radius: 3px;")
          )
        ),
        div(
          style = paste0("color: ", bar_color, "; font-size: 0.85em; font-weight: 500;"),
          "#", rank, " of ", total, " scenarios"
        )
      )
    })
    
    # Total Cost (kpi_cost for UI compatibility)
    output$kpi_cost <- renderText({
      val <- as.numeric(recommended_scenario()$total_cost_millions)
      paste0("$", format(round(val / 1000, 1), nsmall = 1), "B")
    })
    
    # Total Investment (alias for backwards compatibility)
    output$kpi_investment <- renderText({
      val <- as.numeric(recommended_scenario()$total_cost_millions)
      paste0("$", format(round(val / 1000, 1), nsmall = 1), "B")
    })
    
    output$kpi_investment_context <- renderText({
      "Total portfolio investment"
    })
    
    # Avg Timeline
    output$kpi_timeline <- renderText({
      val <- as.numeric(recommended_scenario()$avg_timeline_years)
      paste0(format(round(val, 1), nsmall = 1), " yrs")
    })
    
    output$kpi_timeline_context <- renderText({
      obj <- input$objective
      if (obj == "speed") {
        "Years to approval • ★ Fastest approvals/year"
      } else {
        "Years to approval"
      }
    })
    
    # Efficiency Metric
    output$kpi_efficiency <- renderText({
      val <- as.numeric(recommended_scenario()$approvals_per_100M)
      format(round(val, 3), nsmall = 3)
    })
    
    output$kpi_efficiency_context <- renderUI({
      obj <- input$objective
      current_val <- as.numeric(recommended_scenario()$approvals_per_100M)
      all_vals <- sapply(all_scenario_results(), function(x) as.numeric(x$approvals_per_100M))
      
      # Calculate rank (higher is better for efficiency)
      rank <- sum(all_vals > current_val) + 1
      total <- length(all_vals)
      
      # Determine bar color and width
      bar_pct <- ((total - rank + 1) / total) * 100
      bar_color <- if (rank <= 2) "#107C10" else if (rank >= 6) "#D13438" else "#CA5010"
      
      # Text based on objective
      base_text <- if (obj == "efficiency") {
        "Approvals per $100M invested • ★ Best capital efficiency"
      } else {
        "Approvals per $100M"
      }
      
      tagList(
        div(style = "margin-bottom: 0.5em;", base_text),
        div(
          style = "width: 100%; height: 6px; background: #E0E0E0; border-radius: 3px; margin: 0.5em 0; position: relative;",
          div(
            style = paste0("width: ", bar_pct, "%; height: 100%; background: ", bar_color, "; border-radius: 3px;")
          )
        ),
        div(
          style = paste0("color: ", bar_color, "; font-size: 0.85em; font-weight: 500;"),
          "#", rank, " of ", total, " scenarios"
        )
      )
    })
    
    # Strategic Velocity
    output$kpi_velocity <- renderText({
      val <- as.numeric(recommended_scenario()$approvals_per_year)
      format(round(val, 2), nsmall = 2)
    })
    
    output$kpi_velocity_context <- renderText({
      "Average approvals per year"
    })
    
    # Asset Productivity
    output$kpi_productivity <- renderText({
      val <- as.numeric(recommended_scenario()$approvals_per_asset)
      paste0(format(round(val * 100, 1), nsmall = 1), "%")
    })
    
    output$kpi_productivity_context <- renderText({
      "Expected success rate per asset"
    })
    
    # -------------------------------------------------------------------------
    # RECOMMENDATION OUTPUTS (Fixed to use flat structure)
    # -------------------------------------------------------------------------
    
    output$recommendation_title <- renderText({
      recommended_scenario()$scenario_name
    })
    
    output$recommendation_allocation <- renderText({
      recommended_scenario()$portfolio_allocation
    })
    
    output$recommendation_tradeoffs <- renderUI({
      local_scenario <- recommended_scenario()
      local_obj <- input$objective
      
      local_tradeoff_text <- if (local_obj == "max_approvals") {
        paste0(
          "This portfolio maximizes expected approvals (",
          format(round(as.numeric(local_scenario$expected_approvals), 1), nsmall = 1),
          ") at a cost of $",
          format(round(as.numeric(local_scenario$total_cost_millions) / 1000, 1), nsmall = 1),
          "B over ",
          format(round(as.numeric(local_scenario$avg_timeline_years), 1), nsmall = 1),
          " years."
        )
      } else if (local_obj == "speed") {
        paste0(
          "This portfolio optimizes for speed (",
          format(round(as.numeric(local_scenario$approvals_per_year), 2), nsmall = 2),
          " approvals/year) with ",
          format(round(as.numeric(local_scenario$expected_approvals), 1), nsmall = 1),
          " total expected approvals."
        )
      } else if (local_obj == "efficiency") {
        paste0(
          "This portfolio maximizes capital efficiency (",
          format(round(as.numeric(local_scenario$approvals_per_100M), 3), nsmall = 3),
          " approvals/$100M) with ",
          format(round(as.numeric(local_scenario$expected_approvals), 1), nsmall = 1),
          " expected approvals."
        )
      } else if (local_obj == "predictability") {
        paste0(
          "This portfolio optimizes for predictability (",
          format(round(as.numeric(local_scenario$approvals_per_asset) * 100, 1), nsmall = 1),
          "% success rate per asset) with ",
          format(round(as.numeric(local_scenario$expected_approvals), 1), nsmall = 1),
          " expected approvals."
        )
      } else {  # max_learning
        paste0(
          "This portfolio maximizes learning opportunities with ",
          format(round(as.numeric(local_scenario$expected_approvals), 1), nsmall = 1),
          " expected approvals at $",
          format(round(as.numeric(local_scenario$total_cost_millions) / 1000, 1), nsmall = 1),
          "B."
        )
      }
      
      p(style = "color: #323130; line-height: 1.6;", local_tradeoff_text)
    })
    
    # -------------------------------------------------------------------------
    # RECOMMENDATION CONTENT (Combined output for UI)
    # -------------------------------------------------------------------------
    
    output$recommendation_content <- renderUI({
      local_scenario <- recommended_scenario()
      
      tagList(
        h4(local_scenario$scenario_name, style = "color: #006B75; margin: 0 0 0.5em 0;"),
        p(
          style = "color: #323130; margin: 0 0 0.5em 0;",
          strong("Portfolio Allocation: "), local_scenario$portfolio_allocation
        ),
        p(
          style = "color: #605E5C; line-height: 1.6; margin: 0;",
          "This configuration optimizes for your selected strategic objective based on ",
          "industry-average success rates and cost structures."
        )
      )
    })
    
    # -------------------------------------------------------------------------
    # TRADEOFF CONTENT (What You're Prioritizing)
    # -------------------------------------------------------------------------
    
    output$tradeoff_content <- renderUI({
      local_scenario <- recommended_scenario()
      local_obj <- input$objective
      
      local_tradeoff_text <- if (local_obj == "max_approvals") {
        paste0(
          "Maximizing approvals (",
          format(round(as.numeric(local_scenario$expected_approvals), 1), nsmall = 1),
          ") requires accepting higher capital requirements ($",
          format(round(as.numeric(local_scenario$total_cost_millions) / 1000, 1), nsmall = 1),
          "B) and longer timelines (",
          format(round(as.numeric(local_scenario$avg_timeline_years), 1), nsmall = 1),
          " years)."
        )
      } else if (local_obj == "speed") {
        paste0(
          "Optimizing for speed (",
          format(round(as.numeric(local_scenario$approvals_per_year), 2), nsmall = 2),
          " approvals/year) may reduce total pipeline output and requires careful therapeutic area selection."
        )
      } else if (local_obj == "efficiency") {
        paste0(
          "Maximizing capital efficiency (",
          format(round(as.numeric(local_scenario$approvals_per_100M), 3), nsmall = 3),
          " approvals/$100M) may result in fewer total approvals but better resource utilization."
        )
      } else if (local_obj == "predictability") {
        paste0(
          "Optimizing for predictability (",
          format(round(as.numeric(local_scenario$approvals_per_asset) * 100, 1), nsmall = 1),
          "% per-asset success rate) typically favors late-stage assets but may sacrifice upside potential."
        )
      } else {  # max_learning
        paste0(
          "Maximizing learning opportunities emphasizes early-stage pipeline, accepting higher attrition ",
          "in exchange for strategic option value and future opportunities."
        )
      }
      
      p(style = "color: #323130; line-height: 1.6; margin: 0;", local_tradeoff_text)
    })
    
    # -------------------------------------------------------------------------
    # OBJECTIVE DESCRIPTION
    # -------------------------------------------------------------------------
    
    output$objective_description <- renderUI({
      local_desc <- switch(
        input$objective,
        "max_approvals" = "Optimize for maximum number of regulatory approvals over 10-year horizon.",
        "predictability" = "Optimize for consistent per-asset conversion rates, minimizing variance.",
        "max_learning" = "Maximize early-stage pipeline for maximum option value and learning.",
        "speed" = "Achieve highest approval velocity (approvals per year), focusing on speed to market.",
        "efficiency" = "Maximize capital efficiency (approvals per $100M invested)."
      )
      
      p(style = "color: #605E5C; font-style: italic; margin-top: 1em;", local_desc)
    })
    
  })
}