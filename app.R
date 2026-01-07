# ============================================================================ #
# Pharma R&D Strategic Resource Allocation Simulator
# Portfolio Project - Steven Ponce
# ============================================================================ #
#
# Main app file - delegates to global.R, ui.R, server.R
#
# Structure:
#   global.R  - Package loading, data initialization, constants
#   ui.R      - UI definition
#   server.R  - Server logic
#   app.R     - This file (launches app)
#
# ============================================================================ #

# Run the application
shinyApp(ui = ui, server = server)

