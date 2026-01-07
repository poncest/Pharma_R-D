# ============================================================================ #
# UI Definition - Pharma R&D Strategic Simulator (Full Enhanced Version)
# ============================================================================ #

ui <- semanticPage(
  title = "Pharma R&D Strategic Simulator",
  
  # 1. Custom CSS Styling
  tags$head(
    tags$style(HTML("
      body {
        background-color: #F8F8F8;
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      }
      
      /* Main Header Styling */
      .main-header {
          background: linear-gradient(135deg, #006B75 0%, #004A50 100%);
          color: white;
          padding: 2.5em 0;
          margin-bottom: 2em;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .main-header h1 { color: white; margin: 0; font-size: 2.5em; font-weight: 300; }
      .main-header p { color: #E0E0E0; margin: 0.5em 0 0 0; font-size: 1.1em; }
      
      /* Navigation Tabs - Brand Customization */
      .ui.secondary.pointing.menu {
        border-bottom: 2px solid #D2D0CE;
        margin-bottom: 2em;
      }
      .ui.secondary.pointing.menu .item {
        color: #605E5C !important;
        font-weight: 600 !important;
        transition: all 0.2s ease;
      }
      /* Active Tab Color (Matching Header) */
      .ui.secondary.pointing.menu .active.item {
        border-color: #006B75 !important;
        color: #006B75 !important;
      }
      /* Hover Effect */
      .ui.secondary.pointing.menu .item:hover {
        color: #006B75 !important;
      }
      
      /* Segment and Header Styling */
      .ui.segment { box-shadow: 0 2px 4px rgba(0,0,0,0.08); border: 1px solid #D2D0CE; }
      .section-header { 
        color: #004578; 
        border-bottom: 2px solid #0078D4; 
        padding-bottom: 0.5em; 
        margin: 2em 0 1em 0; 
      }
    "))
  ),
  
  # 2. Page Header
  div(
    class = "main-header",
    div(
      class = "ui container",
      h1("Pharma R&D Strategic Resource Allocation Simulator"),
      p("Multi-objective decision support for portfolio management")
    )
  ),
  
  # 3. Main Content
  div(
    class = "ui container",
    
    # About Button
    div(
      style = "text-align: right; margin-bottom: 1.5em;",
      actionButton(
        "show_about",
        label = tagList(icon("info circle"), " About This Simulator"),
        class = "ui teal basic button",
        onclick = "$('.ui.modal.about').modal('show');"
      )
    ),
    
    shiny.semantic::tabset(
      id = "main_tabs",
      menu_class = "ui four item labeled icon secondary pointing menu", 
      tabs = list(
        list(
          id = "executive", 
          menu = tagList(icon("dashboard"), "Executive Brief"), 
          content = mod_executive_brief_ui("executive")
        ),
        list(
          id = "builder",   
          menu = tagList(icon("sliders"), "Portfolio Builder"),   
          content = mod_portfolio_builder_ui("builder")
        ),
        list(
          id = "tradeoffs", 
          menu = tagList(icon("exchange"), "Trade-off Explorer"),  
          content = mod_tradeoff_explorer_ui("tradeoffs")
        ),
        list(
          id = "methods",   
          menu = tagList(icon("file text"), "Methods & Data"),     
          content = mod_methods_ui("methods")
        )
      )
    )
  ),
  
  # 4. Footer
  div(
    class = "ui inverted vertical segment",
    style = "margin-top: 5em; padding: 3em 0; background-color: #2C2C2C;",
    div(
      class = "ui center aligned container",
      p("Strategic Resource Allocation Simulator | Portfolio Project | Steven Ponce"),
      div(
        class = "ui horizontal inverted small divided link list",
        a(class = "item", href = "https://github.com/poncest/Pharma_R&D", target="_blank", "Source Code"),
        a(class = "item", href = "https://www.linkedin.com/in/stevenponce/", target="_blank", "LinkedIn"),
        a(class = "item", href = "https://stevenponce.netlify.app/", target="_blank", "Portfolio")
      ),
      p(style = "color: #888; font-size: 0.85em; margin-top: 1.5em;", 
        "Data sources: Norstella 2024, Wong et al. 2019, JAMA 2024")
    )
  ),
  
  # 5. The About Modal
  div(
    class = "ui modal scrolling about",
    div(
      class = "header",
      style = "background: #006B75; color: white;",
      icon("info circle"), " About This Simulator"
    ),
    
    div(
      class = "scrolling content",
      
      h3(class = "ui header", style = "color: #006B75;", "Purpose"),
      p(style = "font-size: 1.1em; line-height: 1.6;",
        "This simulator explores pharmaceutical R&D allocation across five key objectives: ",
        "approvals, predictability, learning, speed, and efficiency."),
      
      div(class = "ui stackable grid",
          div(class = "eight wide column",
              h4(class = "ui header", icon("database"), "Data Sources"),
              div(class = "ui bulleted list",
                  div(class = "item", "Norstella/Citeline (2024) - Success Rates"),
                  div(class = "item", "Wong et al. (2019) - Trial Durations"),
                  div(class = "item", "JAMA (2024) - Cost Estimates")
              )
          ),
          div(class = "eight wide column",
              h4(class = "ui header", icon("help circle"), "How to Use"),
              div(class = "ui ordered list",
                  div(class = "item", "Select strategy in Executive Brief"),
                  div(class = "item", "Refine values in Portfolio Builder"),
                  div(class = "item", "Analyze charts in Trade-off Explorer")
              )
          )
      ),
      
      div(class = "ui divider"),
      
      # Disclaimer
      div(
        class = "ui icon warning message",
        icon("exclamation triangle"),
        div(
          class = "content",
          div(class = "header", "Project Disclaimer"),
          p("Demonstration project only. Consult professionals for actual strategic decisions.")
        )
      ),
      
      # Bottom Links & Stack
      div(
        class = "ui stackable grid",
        style = "margin-top: 1em;",
        div(
          class = "six wide column",
          h4("Technology Stack"),
          div(class = "ui labels",
              span(class = "ui label", "R Shiny"), span(class = "ui label", "dplyr"),
              span(class = "ui label", "ggiraph"), span(class = "ui label", "Semantic UI"))
        ),
        div(
          class = "ten wide column",
          h4("Connect"),
          div(
            class = "ui three labeled icon buttons",
            a(class = "ui black button", href = "https://github.com/poncest/Pharma_R&D", target="_blank", icon("github"), "Source"),
            a(class = "ui linkedin button", href = "https://www.linkedin.com/in/stevenponce/", target="_blank", icon("linkedin"), "LinkedIn"),
            a(class = "ui teal button", href = "https://stevenponce.netlify.app/", target="_blank", icon("globe"), "Portfolio")
          )
        )
      )
    ),
    
    div(
      class = "actions",
      div(
        class = "ui grey basic cancel button",
        onclick = "$('.ui.modal.about').modal('hide');",
        "Close"
      )
    )
  )
)