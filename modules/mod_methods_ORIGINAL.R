# ============================================================================ #
# Module: Methods & Data (Enhanced with Parameter Tables)
# Purpose: Methodological transparency and data sources
# ============================================================================ #

mod_methods_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      style = "padding: 2em 0;",
      
      h2("Methods & Data Sources", style = "color: #006B75;"),
      p(
        style = "color: #605E5C;",
        "Complete documentation of data sources, methodology, and modeling assumptions."
      ),
      
      # Data Sources Section
      div(
        class = "ui segment",
        h3("Data Sources", style = "color: #006B75; border-bottom: 2px solid #006B75; padding-bottom: 0.5em;"),
        
        h4("Phase Transition Probabilities", style = "color: #006B75;"),
        p(
          strong("Source: "), "Norstella/Citeline (2024)",
          br(),
          strong("Coverage: "), "2014-2023 industry data",
          br(),
          strong("URL: "),
          a(
            href = "https://www.norstella.com/why-clinical-development-success-rates-falling/",
            target = "_blank",
            style = "color: #006B75;",
            "https://www.norstella.com/why-clinical-development-success-rates-falling/"
          )
        ),
        
        h4("Trial Durations", style = "color: #006B75;"),
        p(
          strong("Source: "), "Wong, C.H., Siah, K.W., & Lo, A.W. (2019)",
          br(),
          strong("Journal: "), "Biostatistics",
          br(),
          strong("DOI: "), "10.1093/biostatistics/kxx069",
          br(),
          strong("PubMed: "), "PMC6409418",
          br(),
          strong("Sample: "), "185,994 trials"
        ),
        
        h4("R&D Cost Estimates", style = "color: #006B75;"),
        p(
          strong("Source: "), "JAMA Network Open (2024)",
          br(),
          strong("DOI: "), "10.1001/jamanetworkopen.2024.15397",
          br(),
          strong("Coverage: "), "2000-2018 industry costs"
        ),
        
        h4("Therapeutic Area Success Rates", style = "color: #006B75;"),
        p(
          strong("Source: "), "American Council on Science and Health (2020)",
          br(),
          strong("URL: "),
          a(
            href = "https://www.acsh.org/news/2020/06/11/clinical-trial-success-rates-phase-and-therapeutic-area-14845",
            target = "_blank",
            style = "color: #006B75;",
            "ACSH Clinical Trial Analysis"
          )
        )
      ),
      
      # Methodology Section
      div(
        class = "ui segment",
        h3("Methodology", style = "color: #006B75; border-bottom: 2px solid #006B75; padding-bottom: 0.5em;"),
        
        h4("Cost Modeling Approach", style = "color: #006B75;"),
        p(
          style = "line-height: 1.6;",
          "This simulator uses ", strong("probability-weighted attrition modeling"), " to ",
          "calculate realistic R&D costs. Rather than assuming all assets complete all phases ",
          "(unrealistic), costs are weighted by the probability of reaching each phase based on ",
          "historical success rates."
        ),
        p(
          style = "line-height: 1.6; margin-top: 1em;",
          strong("Example:"), " If 100 Phase 1 assets enter development with a 60% success rate ",
          "to Phase 2, only 60 assets incur Phase 2 costs. This approach reflects actual portfolio ",
          "economics where early-stage failures reduce total cost burden."
        ),
        
        h4("Strategic Objectives Framework", style = "color: #006B75;"),
        tags$ul(
          style = "line-height: 1.6;",
          tags$li(strong("Maximize Approvals:"), " Total expected approval count"),
          tags$li(strong("Predictability:"), " Per-asset conversion rate (approvals/total assets)"),
          tags$li(strong("Maximize Learning:"), " Early-stage asset count (proxy for mechanism discovery and option creation)"),
          tags$li(strong("Speed to Market:"), " Approvals per year metric"),
          tags$li(strong("Capital Efficiency:"), " Approvals per $100M invested")
        ),
        
        h4("Normalization Approach", style = "color: #006B75;"),
        p(
          style = "line-height: 1.6;",
          "To enable fair comparison across different portfolio sizes, this simulator calculates ",
          "multiple efficiency metrics: per-asset, per-$100M, and per-year. This prevents 'bigger ",
          "is always better' bias and allows meaningful trade-off analysis."
        )
      ),
      
      # Assumptions & Limitations Section
      div(
        class = "ui segment",
        style = "background: #FFF8F0; border-left: 4px solid #CA5010;",
        h3("Assumptions & Limitations", style = "color: #006B75; border-bottom: 2px solid #006B75; padding-bottom: 0.5em;"),
        tags$ul(
          style = "line-height: 1.8;",
          tags$li(
            "Uses ", strong("industry-average parameters"), " from published literature. ",
            "Actual company-specific parameters may vary significantly."
          ),
          tags$li(
            "Assumes ", strong("independent trial outcomes"), ". Platform technology advantages ",
            "or correlated failures not modeled."
          ),
          tags$li(
            strong("Simplified cost framework"), " does not include: discounted cash flows, ",
            "opportunity costs, capital constraints, or competitive dynamics."
          ),
          tags$li(
            "'Maximize Learning' uses early-stage count as a ", strong("proxy"), " for ",
            "mechanism discovery. Actual learning depends on therapeutic area biology, ",
            "target validation, and organizational capabilities."
          ),
          tags$li(
            "Therapeutic area parameters reflect ", strong("historical averages"), ". ",
            "Regulatory changes, scientific breakthroughs, or emerging modalities could ",
            "alter success rates."
          )
        ),
        p(
          style = "color: #605E5C; font-style: italic; margin-top: 1em;",
          "This simulator is a strategic framework for exploring resource allocation trade-offs, ",
          "not a substitute for comprehensive financial analysis or strategic planning."
        )
      ),
      
      # Parameter Tables Section
      div(
        class = "ui segment",
        h3("Parameter Tables", style = "color: #006B75; border-bottom: 2px solid #006B75; padding-bottom: 0.5em;"),
        
        # Phase Transition Probabilities Table
        h4("Phase Transition Success Rates", style = "color: #006B75; margin-top: 1.5em;"),
        p(
          style = "color: #605E5C; font-size: 0.9em; margin-bottom: 1em;",
          "Probability of successfully advancing from one phase to the next, by therapeutic area."
        ),
        uiOutput(ns("phase_transitions_table")),
        
        # Trial Durations Table
        h4("Trial Durations (Median Years)", style = "color: #006B75; margin-top: 2em;"),
        p(
          style = "color: #605E5C; font-size: 0.9em; margin-bottom: 1em;",
          "Median time spent in each clinical phase, by therapeutic area."
        ),
        uiOutput(ns("trial_durations_table")),
        
        # Cost Estimates Table
        h4("R&D Cost Estimates ($ Millions)", style = "color: #006B75; margin-top: 2em;"),
        p(
          style = "color: #605E5C; font-size: 0.9em; margin-bottom: 1em;",
          "Per-asset costs by phase and scenario (Conservative, Base, Aggressive)."
        ),
        uiOutput(ns("cost_estimates_table"))
      )
    )
  )
}

# ============================================================================ #
# SERVER
# ============================================================================ #

mod_methods_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    
    # -------------------------------------------------------------------------
    # Phase Transitions Table
    # -------------------------------------------------------------------------
    output$phase_transitions_table <- renderUI({
      data <- app_data$phase_transitions %>%
        mutate(
          success_rate_pct = paste0(round(success_rate * 100, 1), "%")
        ) %>%
        select(
          `Therapeutic Area` = therapeutic_area,
          `Transition` = phase_transition,
          `Success Rate` = success_rate_pct
        ) %>%
        tidyr::pivot_wider(
          names_from = `Transition`,
          values_from = `Success Rate`
        )
      
      # Create HTML table
      tags$table(
        class = "ui celled striped table",
        style = "font-size: 0.95em;",
        tags$thead(
          tags$tr(
            tags$th("Therapeutic Area", style = "background: #F0F6FC;"),
            tags$th("Phase 1 → 2", style = "background: #F0F6FC; text-align: center;"),
            tags$th("Phase 2 → 3", style = "background: #F0F6FC; text-align: center;"),
            tags$th("Phase 3 → Approval", style = "background: #F0F6FC; text-align: center;")
          )
        ),
        tags$tbody(
          lapply(1:nrow(data), function(i) {
            tags$tr(
              tags$td(
                style = "font-weight: bold; color: #006B75;",
                data$`Therapeutic Area`[i]
              ),
              tags$td(
                style = "text-align: center;",
                data$`Phase1_to_Phase2`[i]
              ),
              tags$td(
                style = "text-align: center;",
                data$`Phase2_to_Phase3`[i]
              ),
              tags$td(
                style = "text-align: center;",
                data$`Phase3_to_Approval`[i]
              )
            )
          })
        )
      )
    })
    
    # -------------------------------------------------------------------------
    # Trial Durations Table
    # -------------------------------------------------------------------------
    output$trial_durations_table <- renderUI({
      data <- app_data$trial_durations %>%
        mutate(
          duration_display = paste0(median_years, " yrs")
        ) %>%
        select(
          `Therapeutic Area` = therapeutic_area,
          `Phase` = phase,
          `Duration` = duration_display
        ) %>%
        tidyr::pivot_wider(
          names_from = `Phase`,
          values_from = `Duration`
        )
      
      tags$table(
        class = "ui celled striped table",
        style = "font-size: 0.95em;",
        tags$thead(
          tags$tr(
            tags$th("Therapeutic Area", style = "background: #F0F6FC;"),
            tags$th("Phase 1", style = "background: #F0F6FC; text-align: center;"),
            tags$th("Phase 2", style = "background: #F0F6FC; text-align: center;"),
            tags$th("Phase 3", style = "background: #F0F6FC; text-align: center;")
          )
        ),
        tags$tbody(
          lapply(1:nrow(data), function(i) {
            tags$tr(
              tags$td(
                style = "font-weight: bold; color: #006B75;",
                data$`Therapeutic Area`[i]
              ),
              tags$td(
                style = "text-align: center;",
                data$Phase1[i]
              ),
              tags$td(
                style = "text-align: center;",
                data$Phase2[i]
              ),
              tags$td(
                style = "text-align: center;",
                data$Phase3[i]
              )
            )
          })
        )
      )
    })
    
    # -------------------------------------------------------------------------
    # Cost Estimates Table
    # -------------------------------------------------------------------------
    output$cost_estimates_table <- renderUI({
      data <- app_data$cost_estimates %>%
        mutate(
          cost_display = paste0("$", round(cost_millions, 1), "M")
        ) %>%
        select(
          `Cost Scenario` = cost_scenario,
          `Phase` = phase,
          `Cost` = cost_display
        ) %>%
        tidyr::pivot_wider(
          names_from = `Phase`,
          values_from = `Cost`
        )
      
      tags$table(
        class = "ui celled striped table",
        style = "font-size: 0.95em;",
        tags$thead(
          tags$tr(
            tags$th("Cost Scenario", style = "background: #F0F6FC;"),
            tags$th("Phase 1", style = "background: #F0F6FC; text-align: center;"),
            tags$th("Phase 2", style = "background: #F0F6FC; text-align: center;"),
            tags$th("Phase 3", style = "background: #F0F6FC; text-align: center;")
          )
        ),
        tags$tbody(
          lapply(1:nrow(data), function(i) {
            tags$tr(
              tags$td(
                style = "font-weight: bold; color: #006B75;",
                data$`Cost Scenario`[i]
              ),
              tags$td(
                style = "text-align: center;",
                data$Phase1[i]
              ),
              tags$td(
                style = "text-align: center;",
                data$Phase2[i]
              ),
              tags$td(
                style = "text-align: center;",
                data$Phase3[i]
              )
            )
          })
        )
      )
    })
    
  })
}