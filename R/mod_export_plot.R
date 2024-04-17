export_plot_ui <- function(id) {
  ns <- NS(id)

  actionButton(
    ns("export_plot"),
    label = NULL,
    icon = icon("download"),
    style = "background-color: transparent; border: none; padding: 0;"
  )
}


export_plot_server <- function(id, exported_plot) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # Return the UI for a modal dialog with data selection input
    export_plot_modal <- function() {
      modalDialog(
        title = "Export plot",
        fluidRow(
          column(
            width = 4,
            numericInput(ns("height"), label = "Height", value = 7)
          ),
          column(
            width = 4,
            numericInput(ns("width"), label = "Width", value = 7)
          ),
          column(
            width = 4,
            selectInput(ns("format"), label = "Format", choices = c("png", "jpg", "svg", "pdf"), selected = "png")
          )
        ),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          downloadButton(outputId = ns("download"))
        )
      )
    }

    # Show modal when button is clicked
    observeEvent(input$export_plot, {
      showModal(export_plot_modal())
    })

    # When Download button is pressed attempt to download plot
    output$download <- downloadHandler(
      filename = function() { paste("glossa_plot_", format(Sys.time(), "%D_%X"), ".", input$format, sep="") },
      content = function(file) {
        ggsave(file, plot = exported_plot, device = input$format)
      }
    )
  })
}
