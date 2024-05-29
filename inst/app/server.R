function(input, output, session) {
  # load polygon
  default_polygon <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
    sf::st_as_sfc() %>%
    sf::st_union() %>%
    sf::st_make_valid() %>%
    sf::st_wrap_dateline() %>%
    glossa::invert_polygon(bbox = c(xmin = -180, ymin =-90, xmax = 180, ymax = 90))
  study_area_poly <- shiny::reactiveVal(value = default_polygon)
  non_study_area_poly <- shiny::reactiveVal(value = glossa::invert_polygon(default_polygon))

  # header server ----
  shiny::observeEvent(input$new_analysis_header, {
    bs4Dash::updateTabItems(session, "sidebar_menu", "new_analysis")
  })

  shiny::observeEvent(input$new_analysis_home, {
    bs4Dash::updateTabItems(session, "sidebar_menu", "new_analysis")
  })


  # new_analysis server ----
  # Open advanced options sidebar
  observeEvent(input$toggle_advanced_options, {
    updateControlbar(id = "advanced_options_sidebar", session = session)
  })

  # Previsualization of input
  output$previsualization_plot <- renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::setView(0, 0, zoom = 1)
  })

  # plot proxy to update leaflet visualization
  previsualization_plot <- leaflet::leafletProxy("previsualization_plot", session)

  observe({
    req(pa_data())
    updatePickerInput(session, "previsualization_plot_species", choices = c(names(pa_data()[!sapply(pa_data(), is.null)]), "None"))
  })

  observe({
    req(hist_layers())
    updatePickerInput(session, "previsualization_plot_layer", choices = c(names(hist_layers()), "None"))
  })

  observeEvent(input$previsualization_plot_species, {
    req(pa_data())
    if(input$previsualization_plot_species != "None"){
      previsualization_plot %>%
        leaflet::setView(0, 0, zoom = 1) %>%
        leaflet::clearMarkers() %>%
        leaflet::addCircleMarkers(data = pa_data()[[input$previsualization_plot_species]], lat = ~decimalLatitude, lng = ~decimalLongitude,
                                  color = ~ifelse(pa == 1, "blue", "red"), radius = 5)
    } else {
      previsualization_plot %>%
        leaflet::clearMarkers()
    }
  })

  observeEvent(input$previsualization_plot_layer, {
    req(hist_layers())
    if (input$previsualization_plot_layer != "None"){
      previsualization_plot %>%
        leaflet::clearImages() %>%
        leaflet::addRasterImage(terra::crop(hist_layers()[input$previsualization_plot_layer], ext(-180, 180, -87, 87)), opacity = 0.7)
    } else {
      previsualization_plot %>%
        leaflet::clearImages()
    }
  })

  observeEvent(input$previsualization_plot_extent, {
    req(extent_poly())

    if(!is.null(extent_poly()[[1]]) & input$previsualization_plot_extent){
      previsualization_plot %>%
        leaflet::clearShapes() %>%
        addPolygons(data = extent_poly(), color = "#353839", fill = TRUE, fillColor = "antiquewhite", fillOpacity = 1)
    } else{
      previsualization_plot %>%
        leaflet::clearShapes()
    }
  })

  pa_files_input <- glossa::file_input_area_server("pa_files")
  hist_layers_input <- glossa::file_input_area_server("hist_layers")
  proj_layers_input <- glossa::file_input_area_server("proj_layers")
  extent_poly_input <- glossa::file_input_area_server("extent_poly")

  # Validate inputs
  pa_data <- reactive({
    # Check is not null and it was possible to upload the data
    if (is.null(pa_files_input())){
      NULL
    } else {
      # Turn on waiter
      w <- waiter::Waiter$new(id = "data_upload",
                              html = tagList(
                                img(src = "logo_glossa.gif", height = "200px")
                              ),
                              color = waiter::transparent(0.8)
      )
      w$show()

      # Read files
      pa_data <- apply(pa_files_input(), 1, function(x){
        glossa::validate_presences_absences_csv(x)
      })
      if (is.null(pa_data)){
        pa_data <- list(pa_data)
      }

      names(pa_data) <- pa_files_input()[, "name"]

      w$hide()
      pa_data
    }
  })


  pa_validation_table <- reactive({
    if (is.null(pa_files_input()) | is.null(pa_data())){
      NULL
    } else {
      # Check which ones where properly loaded
      pa_validation_table <- as.data.frame(pa_files_input())
      pa_validation_table[, "validation"] <- !sapply(pa_data(), is.null)
      pa_validation_table[, "name"] <- paste(as.character(icon("map-location-dot",style = "font-size:2rem; color:#007bff;")), pa_validation_table[, "name"])
      pa_validation_table
    }
  })

  hist_layers <- reactive({
    # Check is not null and it was possible to upload the data
    if (is.null(hist_layers_input())){
      NULL
    } else {
      # Turn on waiter
      w <- waiter::Waiter$new(id = "data_upload",
                              html = tagList(
                                img(src = "logo_glossa.gif", height = "200px")
                              ),
                              color = waiter::transparent(0.8)
      )
      w$show()

      # Read files
      hist_layers <- glossa::validate_historical_layers_zip(hist_layers_input())
      hist_layers <- list(hist_layers)

      w$hide()
      hist_layers
    }
  })

  hist_validation_table <- reactive({
    if (is.null(hist_layers_input()) | is.null(hist_layers())){
      NULL
    } else {
      # Check which ones where properly loaded
      hist_validation_table <- as.data.frame(hist_layers_input())
      hist_validation_table[, "validation"] <-!sapply(hist_layers(), is.null)
      hist_validation_table[, "name"] <- paste(as.character(icon("layer-group",style = "font-size:2rem; color:#007bff;")), hist_validation_table[, "name"])
      hist_validation_table
    }
  })

  proj_layers <-  reactive({
    # Check is not null and it was possible to upload the data
    if (is.null(proj_layers_input())){
      NULL
    } else {
      # Turn on waiter
      w <- waiter::Waiter$new(id = "data_upload",
                              html = tagList(
                                img(src = "logo_glossa.gif", height = "200px")
                              ),
                              color = waiter::transparent(0.8)
      )
      w$show()

      # Read files
      proj_layers <- apply(proj_layers_input(), 1, function(x){
        glossa::validate_projection_layers_zip(x)
      })
      if (is.null(proj_layers)){
        proj_layers <- list(proj_layers)
      }

      w$hide()
      proj_layers
    }
  })

  proj_validation_table <- reactive({
    if (is.null(proj_layers_input()) | is.null(proj_layers())){
      NULL
    } else {
      # Check which ones where properly loaded
      proj_validation_table <- as.data.frame(proj_layers_input())
      proj_validation_table[, "name"] <- paste(as.character(icon("forward",style = "font-size:2rem; color:#007bff;")), proj_validation_table[, "name"])

      if(!is.null(hist_layers_input())){
        proj_validation_table[, "validation"] <- !sapply(proj_layers(), is.null) & sapply(proj_layers_input()[, "datapath"], function(x){glossa::validate_hist_proj_layers(hist_layers_input()[, "datapath"], x)})
      } else {
        proj_validation_table[, "validation"] <- !sapply(proj_layers(), is.null)
      }

      proj_validation_table
    }
  })

  extent_poly <- reactive({
    # Check is not null and it was possible to upload the data
    if (is.null(extent_poly_input())){
      NULL
    } else {
      # Turn on waiter
      w <- waiter::Waiter$new(id = "data_upload",
                              html = tagList(
                                img(src = "logo_glossa.gif", height = "200px")
                              ),
                              color = waiter::transparent(0.8)
      )
      w$show()

      # Read files
      extent_poly <- glossa::validate_extent_poly(extent_poly_input())
      extent_poly <- list(extent_poly)

      w$hide()
      extent_poly
    }
  })

  extent_validation_table <- reactive({
    if (is.null(extent_poly_input()) | is.null(extent_poly())){
      NULL
    } else {
      # Check which ones where properly loaded
      extent_validation_table <- as.data.frame(extent_poly_input())
      extent_validation_table[, "validation"] <- !sapply(extent_poly(), is.null)
      extent_validation_table[, "name"] <- paste(as.character(icon("crop",style = "font-size:2rem; color:#007bff;")), extent_validation_table[, "name"])
      extent_validation_table
    }
  })

  species_files_names <- shiny::reactive({
    data <- pa_files_input()
    if (!is.null(data)) {
      data$name
    } else{
      NULL
    }
  })

  predictor_variables <- shiny::reactive({
    req(hist_layers())
    return(names(hist_layers()))
  })

  # Select predictor variable for each species
  output$predictor_selector <- renderUI({
    if (is.null(species_files_names()) | is.null(predictor_variables())) {
      validate("Upload species ocurrences and historical layers")
    }

    lapply(1:length(species_files_names()), function(i){
      selectInput(inputId = paste0("pred_vars_", i), label = species_files_names()[i], choices = predictor_variables(), selected = predictor_variables(), multiple = TRUE)
    })
  })

  study_area_plot <- shiny::reactive({
    ggplot2::ggplot() +
      geom_sf(data = study_area_poly(), color = "#353839", fill = "#39a6d5") +
      theme(
        panel.background = element_rect(fill = "white"),
        axis.title = element_blank()
      )
  })
  output$study_area_plot <- renderPlot({
    study_area_plot()
  })

  # Render uploaded files as a DT table
  output$uploaded_files <- DT::renderDT(
    if (is.null(rbind(pa_validation_table(), hist_validation_table(), proj_validation_table(), extent_validation_table()))) {
      DT::datatable(NULL)
    } else {
      rbind(pa_validation_table(), hist_validation_table(), proj_validation_table(), extent_validation_table()) %>%
        dplyr::select(name, size, validation) %>%
        dplyr::mutate(validation = ifelse(
          validation,
          as.character(icon("circle-check", class = "fa-solid", style = "font-size:2rem;color:#418B24")),
          as.character(icon("circle-xmark", class = "fa-solid", style = "font-size:2rem;color:#E90C00"))
        )) %>%
        DT::datatable(
          options = list(
            dom = "t",
            ordering = FALSE,
            paging = FALSE,
            searching = FALSE
          ),
          selection = "none",
          class = 'row-border',
          escape = FALSE,
          colnames = c("File name", "File size", "Format Validation"),
          rownames = TRUE,
          filter = "none",
          width = 500
        )
    }
  )

  # Info buttons ----
  shiny::observeEvent(input$analysis_options_nr_info, {
    addPopover(
      id = "analysis_options_nr_info",
      options = list(
        content = "Vivamus sagittis lacus vel augue laoreet rutrum faucibus.",
        title = "Server popover",
        placement = "bottom",
        trigger = "hover"
      )
    )
  })

  # Run analysis ----
  # Output variables to be filled
  presence_absence_list <- shiny::reactiveVal()
  covariate_list <- shiny::reactiveVal()
  prediction_results <- shiny::reactiveVal()
  other_results <- shiny::reactiveVal()
  pa_cutoff <- shiny::reactiveVal()
  habitat_suitability <- shiny::reactiveVal()

  # Confirmation dialog
  shiny::observeEvent(input$run_button, {
    shinyWidgets::ask_confirmation(
      inputId = "run_button_confirmation",
      type = "question",
      title = "Want to confirm?",
      text = "GLOSSA analysis may require some time. Please double-check all inputs before proceeding.",
      btn_labels = c("Cancel", "Confirm"),
    )
  })

  shiny::observeEvent(input$run_button_confirmation, {

    req(input$run_button_confirmation == TRUE)

    # Validate input
    # Messages
    if (is.null(pa_files_input())) {
      showNotification("Please upload a P/A file", type = "error")
    }
    if (!all(pa_validation_table()[, "validation"] == TRUE)) {
      showNotification("Please upload valid P/A files", type = "error")
    }

    if (is.null(hist_layers_input())) {
      showNotification("Please upload historical layers", type = "error")
    }
    if (!all(hist_validation_table()[, "validation"] == TRUE)) {
      showNotification("Please upload valid historical layers", type = "error")
    }

    if ("future" %in% input$analysis_options_nr | "future" %in% input$analysis_options_sh){
      if (is.null(proj_layers_input())) {
        showNotification("Please upload future layers", type = "error")
      }
      if (!all(proj_validation_table()[, "validation"] == TRUE)) {
        showNotification("Please upload valid future layers", type = "error")
      }
    }

    if (is.null(c(input$analysis_options_nr, input$analysis_options_sh))){
      showNotification("Select at least one option to compute from Native range and/or Suitable habitat", type = "error")
    }

    # Req
    req(pa_files_input(), all(pa_validation_table()[, "validation"] == TRUE))
    req(hist_layers_input(), all(hist_validation_table()[, "validation"] == TRUE))
    if ("future" %in% input$analysis_options_nr | "future" %in% input$analysis_options_sh) {
      req(proj_layers_input(), all(proj_validation_table()[, "validation"] == TRUE))
    }
    req(c(input$analysis_options_nr, input$analysis_options_sh))

    # Create waiter
    w <- waiter::Waiter$new(
      html = tagList(
        img(src = "logo_glossa.gif", height = "200px"),
        h4("")
      )
    )
    w$show()
    # Get predictor variables for each sp
    predictor_variables <- lapply(1:length(species_files_names()), function(i){
      input[[paste0("pred_vars_", i)]]
    })

    # Run GLOSSA analysis
    glossa_results <- glossa::glossa_analysis(
      pa_files = pa_files_input()[,"datapath"],
      historical_files = hist_layers_input()[,"datapath"],
      future_files = proj_layers_input()[,"datapath"],
      future_scenario_names = sub("\\.zip$", "", proj_layers_input()[,"name"]),
      study_area_poly = study_area_poly(),
      predictor_variables = predictor_variables,
      decimal_digits = switch(input$round_digits + 1, NULL, input$decimal_digits),
      scale_layers = input$scale_layers,
      native_range = input$analysis_options_nr,
      suitable_habitat = input$analysis_options_sh,
      other_analysis = input$analysis_options_other,
      seed = input$seed,
      waiter = w
    )
    w$hide()

    showNotification("GLOSSA analysis done!", duration = NULL, closeButton = TRUE, type = "message")

    presence_absence_list(glossa_results$presence_absence_list)
    covariate_list(glossa_results$covariate_list)
    prediction_results(glossa_results$prediction_results)
    other_results(glossa_results$other_results)
    pa_cutoff(glossa_results$pa_cutoff)
    habitat_suitability(glossa_results$habitat_suitability)

  })

  # reports server ----
  # * Sp names report selectizer ----
  observe({
    req(presence_absence_list())
    updatePickerInput(session, "sp", label = NULL, choices = names(presence_absence_list()$model_pa), choicesOpt = list(icon = rep("fa-solid fa-globe", length(names(presence_absence_list()$model_pa)))))
  })

  # * Prediction plot selectizers ----
  # Update pred_plot_time picker
  observe({
    req(prediction_results())
    updatePickerInput(session, "pred_plot_time", choices = names(prediction_results()[!unlist(lapply(prediction_results(),is.null))]))
  })

  # Update pred_plot_mode picker
  observe({
    req(input$pred_plot_time)
    display_choices <- names(prediction_results()[[input$pred_plot_time]])
    display_val <- input$pred_plot_mode
    if (!is.null(display_val)){
      if (!(display_val %in% display_choices)) {
        display_val <- NULL
      }
    }
    updatePickerInput(session, "pred_plot_mode", choices = display_choices, selected = display_val)
  })

  # Update pred_plot_value picker
  observe({
    req(input$pred_plot_mode)
    if (input$pred_plot_time != "future") {
      updatePickerInput(session, "pred_plot_value", choices = names(prediction_results()[[input$pred_plot_time]][[input$pred_plot_mode]][[input$sp]]))
    } else {
      req(input$pred_plot_scenario_future)
      updatePickerInput(session, "pred_plot_value", choices = names(prediction_results()[[input$pred_plot_time]][[input$pred_plot_mode]][[input$sp]][[input$pred_plot_scenario_future]]))
    }
  })

  # Update pred_plot_scenario_future picker
  output$pred_plot_scenario_picker <- renderUI({
    req(input$pred_plot_time == "future")
    pickerInput("pred_plot_scenario_future", label = NULL, width = "90%", choices = names(prediction_results()[[input$pred_plot_time]][[input$pred_plot_mode]][[input$sp]]))
  })

  # Update pred_plot_year_past picker
  output$pred_plot_year_past_slider <- renderUI({
    req(input$pred_plot_time == "past")
    sliderInput(inputId = "pred_plot_year_past", label = "Year", round = TRUE, step = 1, width = "90%", value = 1, min = 1, max = length(prediction_results()[[input$pred_plot_time]][[input$pred_plot_mode]][[input$sp]]))
  })

  # Update pred_plot_year_future picker
  output$pred_plot_year_future_slider <- renderUI({
    req(input$pred_plot_time == "future")
    req(input$pred_plot_scenario_future)
    sliderInput(inputId = "pred_plot_year_future", label = "Year", value = 1, min = 1, max = length(prediction_results()[[input$pred_plot_time]][[input$pred_plot_mode]][[input$sp]][[input$pred_plot_scenario_future]]))
  })

  # * Layers plot selectizer ----
  observe({
    req(covariate_list())
    updatePickerInput(session, "layers_plot_time", choices = names(covariate_list()[!unlist(lapply(covariate_list(),is.null))]))
  })

  observe({
    req(input$layers_plot_time)
    if (input$layers_plot_time == "historical") {
      req(input$layers_plot_scaled)
      updatePickerInput(session, "layers_plot_cov", choices = names(covariate_list()[[input$layers_plot_time]][[input$layers_plot_scaled]]))
    } else if (input$layers_plot_time == "past") {
      updatePickerInput(session, "layers_plot_cov", choices = names(covariate_list()[[input$layers_plot_time]]))
    } else if (input$layers_plot_time == "future") {
      req(input$layers_plot_scenario)
      updatePickerInput(session, "layers_plot_cov", choices = names(covariate_list()[[input$layers_plot_time]][[input$layers_plot_scenario]]))
    }
  })

  output$layers_plot_scaled_picker <- renderUI({
    req(input$layers_plot_time == "historical")
    pickerInput("layers_plot_scaled", label = NULL, width = "90%", choices = names(covariate_list()[[input$layers_plot_time]]))
  })

  output$layers_plot_scenario_picker <- renderUI({
    req(input$layers_plot_time == "future")
    pickerInput("layers_plot_scenario", label = NULL, width = "90%", choices = names(covariate_list()[[input$layers_plot_time]]))
  })

  output$layers_plot_year_past_slider <- renderUI({
    req(input$layers_plot_time == "past")
    sliderInput(inputId = "layers_plot_year_past", label = "Year", round = TRUE, step = 1, width = "90%", value = 1, min = 1, max = terra::nlyr(covariate_list()[[input$layers_plot_time]][[input$layers_plot_cov]]))
  })

  output$layers_plot_year_future_slider <- renderUI({
    req(input$layers_plot_time == "future")
    req(input$layers_plot_scenario)
    sliderInput(inputId = "layers_plot_year_future", label = "Year", round = TRUE, step = 1, width = "90%", value = 1, min = 1, max = terra::nlyr(covariate_list()[[input$layers_plot_time]][[input$layers_plot_scenario]][[input$layers_plot_cov]]))
  })

  # * Functional responses plot selectizer ----
  observe({
    req(other_results())
    req(other_results()[["response_curve"]])
    req(input$sp)
    updatePickerInput(session, "fr_plot_cov", choices = names(other_results()[["response_curve"]][[input$sp]]))
  })

  # * Variable importance plot selectizer ----
  observe({
    req(other_results())
    req(other_results()[["variable_importance"]])
    updatePickerInput(session, "varimp_plot_mode", choices = names(other_results()[["variable_importance"]]))
  })

  # * Cross-validation plot selectizer ----
  observe({
    req(other_results())
    req(other_results()[["cross_validation"]])
    req(input$sp)
    updatePickerInput(session, "cv_plot_mode", choices = names(other_results()[["cross_validation"]]))
  })

  # * Sparkline box ----
  output$spark_boxes <- renderUI({
    if (is.null(input$sp)){
      sparkline_data1 <- rep(0, 30)
      sparkline_data2 <- rep(0, 30)
      sparkline_data3 <-  c(0, 0)
      description1_2 <- "%"
    } else {
      # Define default values
      sparkline_data1 <- rep(0, 30)
      sparkline_data2 <- rep(0, 30)
      description1_2 <- "%"

      if (input$pred_plot_time == "future" & !is.null(habitat_suitability()[[input$pred_plot_time]]) & !is.null(input$pred_plot_scenario_future)){
        # Check if future data along with input scenario are available
        sparkline_data1 <- habitat_suitability()[[input$pred_plot_time]][["covered_area"]][[input$sp]][[input$pred_plot_scenario_future]]
        sparkline_data2 <- habitat_suitability()[[input$pred_plot_time]][["suit_prob"]][[input$sp]][[input$pred_plot_scenario_future]]
        description1_2 <- paste("%", input$pred_plot_scenario_future)
      } else if (input$pred_plot_time != "future" & !is.null(habitat_suitability()[[input$pred_plot_time]])) {
        sparkline_data1 <- habitat_suitability()[[input$pred_plot_time]][["covered_area"]][[input$sp]]
        sparkline_data2 <- habitat_suitability()[[input$pred_plot_time]][["suit_prob"]][[input$sp]]
        description1_2 <- paste("%", input$pred_plot_scenario_future)
      }

      sparkline_data3 <-  c(
        sum(presence_absence_list()[["model_pa"]][[input$sp]][, "pa"] == 1),
        sum(presence_absence_list()[["model_pa"]][[input$sp]][, "pa"] == 0)
      )
    }

    fluidRow(
      glossa::sparkvalueBox(
        title = "Potential suitable area (km2)",
        sparkline_data = sparkline_data1,
        description = description1_2,
        elevation = 2
      ),

      glossa::sparkvalueBox(
        title = "Mean suitable probability",
        sparkline_data = sparkline_data2,
        description = description1_2,
        elevation = 2
      ),

      glossa::sparkvalueBox(
        title = "Presences/Absences",
        sparkline_data = sparkline_data3,
        description = "ratio P/A",
        elevation = 2,
        type = "bar"
      )
    )
  })

  # Plot reports
  # * Prediction plot ----
  prediction_plot <- reactive({
    prediction_layer <- NULL
    pa_points <- NULL
    legend_label <- NULL

    if (!is.null(input$pred_plot_value)) {
      if (input$pred_plot_time == "historical") {
        prediction_layer <- prediction_results()[[input$pred_plot_time]][[input$pred_plot_mode]][[input$sp]][[input$pred_plot_value]]
      } else if (input$pred_plot_time == "past") {
        req(input$pred_plot_year_past)
        prediction_layer <- prediction_results()[[input$pred_plot_time]][[input$pred_plot_mode]][[input$sp]][[input$pred_plot_year_past]][[input$pred_plot_value]]
      } else if (input$pred_plot_time == "future") {
        req(input$pred_plot_year_future)
        prediction_layer <- prediction_results()[[input$pred_plot_time]][[input$pred_plot_mode]][[input$sp]][[input$pred_plot_scenario_future]][[input$pred_plot_year_future]][[input$pred_plot_value]]
      }
      legend_label <- input$pred_plot_value
    }

    if (!is.null(input$sp) & input$pa_points) {
      pa_points <- presence_absence_list()$model_pa[[input$sp]]
    }

    glossa::generate_prediction_plot(prediction_layer, pa_points, legend_label, non_study_area_poly())
  })
  output$prediction_plot <- renderPlot({
    prediction_plot()
  })

  # * Layers plot ----
  cov_layers_plot <- reactive({
    p <- ggplot2::ggplot()

    if (!is.null(input$layers_plot_cov)){
      if (input$layers_plot_time == "historical") {
        plot_layers <- covariate_list()[[input$layers_plot_time]][[input$layers_plot_scaled]][[input$layers_plot_cov]]
      } else if (input$layers_plot_time == "past") {
        req(input$layers_plot_year_past)
        plot_layers <- covariate_list()[[input$layers_plot_time]][[input$layers_plot_cov]][[input$layers_plot_year_past]]
      } else if (input$layers_plot_time == "future") {
        req(input$layers_plot_year_future)
        plot_layers <- covariate_list()[[input$layers_plot_time]][[input$layers_plot_scenario]][[input$layers_plot_cov]][[input$layers_plot_year_future]]
      }
      legend_label <- input$layers_plot_cov

      p <- p +
        geom_spatraster(data = plot_layers) +
        scale_fill_gradientn(colours = c("#A1D4B1","#2BAF90","#F1A512","#DD4111","#8C0027"),
                             name = legend_label)
    }

    p +
      geom_sf(data = non_study_area_poly(), color = "#353839", fill = "antiquewhite") +
      theme(
        panel.grid.major = element_line(
          color = gray(.5),
          linetype = "dashed",
          linewidth = 0.5
        ),
        panel.background = element_rect(fill = "white"),
        axis.title = element_blank()
      )
  })
  output$cov_layers_plot<-renderPlot({
    cov_layers_plot()
  })

  # * Observations plot ----
  observations_plot <- reactive({
    p <- ggplot2::ggplot() +
      geom_sf(data = non_study_area_poly(), color = "#353839", fill = "antiquewhite")

    if (!is.null(input$sp)) {
      model_points <- presence_absence_list()$model_pa[[input$sp]]
      model_points <- model_points[model_points[, "pa"] == 1, c("decimalLongitude", "decimalLatitude", "pa")]
      model_points$type <- "keeped"

      raw_points <- presence_absence_list()$raw_pa[[input$sp]]
      raw_points <- raw_points[raw_points[, "pa"] == 1, c("decimalLongitude", "decimalLatitude", "pa")]
      decimal_digits <- switch(input$round_digits + 1, NULL, input$decimal_digits)
      if (!is.null(decimal_digits)) {
        raw_points[, "decimalLongitude"] <- round(raw_points[, "decimalLongitude"], decimal_digits)
        raw_points[, "decimalLatitude"] <- round(raw_points[, "decimalLatitude"], decimal_digits)
      }
      raw_points <- dplyr::anti_join(raw_points, model_points, by = c("decimalLongitude", "decimalLatitude"))
      raw_points$type <- "discarded"

      p <- p +
        geom_point(data = rbind(raw_points, model_points), ggplot2::aes(x = decimalLongitude, y = decimalLatitude, color = type)) +
        scale_color_manual(values = c("keeped" = "#65c4d8", "discarded" = "#f67d33"))
    }

    p +
      theme(
        panel.grid.major = element_line(
          color = gray(.5),
          linetype = "dashed",
          linewidth = 0.5
        ),
        panel.background = element_rect(fill = "white"),
        axis.title = element_blank()
      )
  })
  output$observations_plot<-renderPlot({
    observations_plot()
  })


  # * Functional responses plot ----
  fr_plot <- reactive({
    p <- ggplot2::ggplot(data = data.frame(y = 0:1), ggplot2::aes(y = y))

    if (!is.null(input$fr_plot_cov)) {
      p <- ggplot2::ggplot(data = other_results()[["response_curve"]][[input$sp]][[input$fr_plot_cov]], ggplot2::aes(x = value)) +
        geom_ribbon(ggplot2::aes(ymin = q25, ymax = q975), fill = "#65c4d8", alpha = 0.3) +
        geom_line(ggplot2::aes(y = mean), color = "#004172", linewidth = 1) +
        xlab(input$fr_plot_cov)
    }

    p +
      ylab("Probability") +
      theme_minimal()
  })
  output$fr_plot<-renderPlot({
    fr_plot()
  })

  # * Variable importance plot ----
  varimp_plot <- reactive({
    p <- ggplot2::ggplot(data.frame(x = paste("var", 1:3), y = c(0, 0, 0)), ggplot2::aes(x = x, y = y))

    if (!is.null(input$varimp_plot_mode)) {
      x <- other_results()[["variable_importance"]][[input$varimp_plot_mode]][[input$sp]]
      p <- ggplot2::ggplot(data.frame(x = factor(names(x), levels = names(x)), y = x), ggplot2::aes(x = x, y = y))
    }

    p +
      geom_bar(stat = "identity", fill="#007bff") +
      ylab("Variable importance") +
      xlab("") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  })
  output$varimp_plot <-renderPlot({
    varimp_plot()
  })

  # * Cross-validation plot ----
  cv_plot <- reactive({
    if (!is.null(input$cv_plot_mode)){
      x <- other_results()[["cross_validation"]][[input$cv_plot_mode]][[input$sp]]
    } else {
      x <- data.frame(PREC = 0, SEN = 0, SPC = 0, FDR = 0, NPV = 0, FNR = 0, FPR = 0, Fscore = 0, ACC = 0, BA = 0)
    }
    glossa::generate_cv_plot(x)
  })
  output$cv_plot<-renderPlot({
    cv_plot()
  })


  # * Export plots ----
  # Export layers plot
  glossa::export_plot_server("export_previsualization_plot", prediction_plot())
  glossa::export_plot_server("export_pred_plot", prediction_plot())
  glossa::export_plot_server("export_layers_plot", cov_layers_plot())
  glossa::export_plot_server("export_observations_plot", observations_plot())
  glossa::export_plot_server("export_fr_plot", fr_plot())
  glossa::export_plot_server("export_varimp_plot", varimp_plot())
  glossa::export_plot_server("export_cv_plot", cv_plot())

  # Exports server ----
  # Update selectizers
  observe({
    req(presence_absence_list())
    req(prediction_results())
    updateSelectInput(session, "export_sp", choices = names(presence_absence_list()$model_pa))
    export_time_periods <- names(prediction_results()[!unlist(lapply(prediction_results(),is.null))])
    updateSelectInput(session, "export_time", choices = export_time_periods)
    updateSelectInput(session, "export_mods", choices = unique(as.vector((unlist((sapply(prediction_results()[export_time_periods], names)))))))
    updateSelectInput(session, "export_fields", choices = c("mean", "median", "sd", "q0.025", "q0.975", "diff", "potential_presences"))
    updateSelectInput(session, "export_layer_format", choices = c("tif", "asc", "nc"))
  })

  # Exports downloadHandler
  output$export_button <- downloadHandler(
    filename = function() { paste("glossa_", format(Sys.time(), "%D_%X"), ".zip", sep="") },
    content = function(file) {
      w <- waiter::Waiter$new(html = tagList(
        img(src = "logo_glossa.gif", height = "200px")
      ),
      color = waiter::transparent(0.8)
      )
      w$show()
      export_files <- glossa_export(species = input$export_sp, mods = input$export_mods,
                                    time = input$export_time, fields = input$export_fields,
                                    model_data = input$export_model_data, fr = input$export_fr,
                                    prob_cut = input$export_pa_cutoff, varimp = input$export_var_imp,
                                    cross_val = input$export_cv, layer_format = input$export_layer_format,
                                    prediction_results = prediction_results(),
                                    presence_absence_list = presence_absence_list(),
                                    other_results = other_results(), pa_cutoff = pa_cutoff())

      w$hide()
      zip::zip(zipfile = file, files = export_files, mode = "cherry-pick")
    }
  )
}
