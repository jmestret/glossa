file_input_area_ui <- function(id, label = "Input text: ", multiple = FALSE, accept = NULL,
                               width = NULL, button_label = "Browse...", icon_name = NULL) {
  ns <- NS(id)

  # Restore the input value if it exists
  restored_value <- restoreInput(id = id, default = NULL)

  # Check if the restored value exists and has the correct format
  if (!is.null(restored_value) && !is.data.frame(restored_value)) {
    warning("Restored value for ", id, " has incorrect format.")
    restored_value <- NULL
  }

  # Convert restored value to JSON if it exists
  if (!is.null(restored_value)) {
    restored_value <- jsonlite::toJSON(restored_value, strict_atomic = FALSE)
  }

  # Create the input tag
  input_tag <- tags$input(
    id = ns("file_input"),
    type = "file",
    style = "position: absolute !important; top: -99999px !important; left: -99999px !important;",
    `data-restore` = restored_value
  )

  # Handle multiple file selection
  if (multiple)
    input_tag$attribs$multiple <- "multiple"

  # Specify accepted file types
  if (length(accept) > 0)
    input_tag$attribs$accept <- paste(accept, collapse = ",")

  # Create the HTML structure
  input_structure <- div(
    class = "form-group shiny-input-container w-100",
    style = htmltools::css(width = htmltools::validateCssUnit(width)),
    shiny:::shinyInputLabel(id, ""),
    div(
      class = "input-group mb-3",
      tags$label(
        class = "input-group-btn input-group-prepend w-100",
        span(
          class = "btn btn-area w-100",
          input_tag,
          div(p(label), style = "font-size: 1.1rem; font-weight: 700; padding-top: 2rem;"),
          div(p(button_label), style = "font-size: 1rem; font-weight: 400;"),
          div(icon(icon_name, style = "font-size:5rem; color:#495057;"), style = "margin-bottom: 2rem;")
        )
      )
    ),

    tags$style(".btn-area {
                  color: #232b2b;
                  border-color: #495057;
                  border-style: none;
                  border-width: 2px;
                  border-radius: 20px !important;
                  background-color: #DFF2FF;
                }
                .btn-area:hover {
                  color: #495057;
                  background-color: #DFF2FF;
                  transform: scale(1.1);
                }")
  )

  # Return the HTML structure
  return(input_structure)
}


file_input_area_server <- function(id) {
  moduleServer(id, function(input, output, session){
    # Reactive expression for uploaded file data
    uploaded_data <- reactive({
      x <- input$file_input
      if (!is.null(x)) {
        data <- as.data.frame(x[, c("name", "size", "type", "datapath")])
        data[, "date"] <- format(Sys.time())
        return(data)
      }
      return(NULL)
    })

    # Return the uploaded data
    return(uploaded_data)
  })
}
