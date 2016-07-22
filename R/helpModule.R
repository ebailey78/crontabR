helpOutput <- function(id) {
  ns <- NS(id)

  miniTabPanel("Help", icon = icon("question"),
               miniContentPanel(
                 tags$p("Help coming soon.")
               )
  )
}
