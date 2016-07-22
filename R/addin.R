#'@export
crontabRAddin <- function() {

  library(crontabR)

  ui <- miniPage(
    miniTabstripPanel(id = "whichTab",
      createOutput("create"),
      viewOutput("view"),
      logsOutput("logs"),
      optionsOutput("options"),
      helpOutput("help")
    )
  )

  server <- function(input, output, session) {

    job <- callModule(create, "create")
    jobs <- callModule(view, "view", listCronjobs())
    logs <- callModule(logs, "logs")
    opts <- callModule(optionsServer, "options")

  }

  viewer <- dialogViewer("crontabR", width = 864, height = 648)


  runGadget(ui, server, viewer = viewer)

}
