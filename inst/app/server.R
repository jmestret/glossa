function(input, output, session) {
  #=========================================================#
  # Initialization ----
  #=========================================================#

  # * Reactive values ----
  # Inputs
  pa_data <- reactiveVal()
  pa_validation_table <- reactiveVal()
  fit_layers <- reactiveVal()
  fit_layers_validation_table <- reactiveVal()
  proj_layers <- reactiveVal()
  proj_validation_table <- reactiveVal()
  study_area_poly <- reactiveVal()
  extent_validation_table <- reactiveVal()

  # Analysis results
  presence_absence_list <- reactiveVal()
  covariate_list <- reactiveVal()
  projections_results <- reactiveVal()
  other_results <- reactiveVal()
  pa_cutoff <- reactiveVal()
  habitat_suitability <- reactiveVal()

  #=========================================================#
  # Reactive expressions ----
  #=========================================================#

  # * File inputs ----
  pa_files_input <- glossa::file_input_area_server("pa_files")
  fit_layers_input <- glossa::file_input_area_server("fit_layers")
  proj_layers_input <- glossa::file_input_area_server("proj_layers")
  study_area_poly_input <- glossa::file_input_area_server("study_area_poly")

  # Get long lat coordinate colnames
  long_lat_cols <- reactive({
    req(pa_data())
    colnames(pa_data()[[1]])[c(1,2)]
  })

  # Invert study area polygon for plotting
  inv_study_area_poly <- reactive({
    if(is.null(study_area_poly()) | is.null(extent_validation_table())){
      NULL
    } else {
      if(extent_validation_table()[, "validation"] == FALSE){
        NULL
      } else {
        glossa::invert_polygon(study_area_poly())
      }
    }
  })

  # Get file names of species occurrences for picker inputs
  species_files_names <- reactive({
    data <- pa_files_input()
    if (!is.null(data)) {
      data$name
    } else{
      NULL
    }
  })

  # name of uploaded predictor variables (name of files)
  predictor_variables <- reactive({
    req(fit_layers())
    return(names(fit_layers()))
  })

  #=========================================================#
  # Observers ----
  #=========================================================#

  # * Header server ----
  observeEvent(input$new_analysis_header, {
    bs4Dash::updateTabItems(session, "sidebar_menu", "new_analysis")
  })

  observeEvent(input$new_analysis_home, {
    bs4Dash::updateTabItems(session, "sidebar_menu", "new_analysis")
  })

  # show help modal
  observeEvent(input$show_help, {
    showModal(modalDialog(
      title = "Welcome to the GLOSSA App!",
      tags$div(
        tags$ol(
          tags$li(tags$strong("Explore the Demo: "), "If you're new, start by watching our quick demo video to see how GLOSSA works. Click the 'Watch Demo' button to get started."),
          tags$li(tags$strong("New Analysis: "), "Start by uploading your species occurrences and environmental layers. Then, tune the analysis options to customize your model."),
          tags$li(tags$strong("Reports: "), "After running the analysis, view the results in the Reports tab. Here, you can explore predictions of species suitable habitat and other insights."),
          tags$li(tags$strong("Export: "), "Once you're satisfied with the results, head over to the Export tab to save your findings."),
          tags$li(tags$strong("Documentation: "), "Access detailed documentation and user guides in the Documentation tab."),
          tags$li(tags$strong("How to Cite: "), "Find information on how to cite GLOSSA in your publications in the How to Cite tab."),
          tags$li(tags$strong("FAQs: "), "Have questions? Check out the FAQs tab for answers to commonly asked questions."),
        ),
        tags$p("Need more help? Feel free to reach out to us directly via the Contact tab."),
        tags$p("Happy modeling!")
      ),
      easyClose = TRUE
    ))
  })

  # * New Analysis server ----
  # Open advanced options sidebar
  observeEvent(input$toggle_advanced_options, {
    updateControlbar(id = "advanced_options_sidebar", session = session)
  })

  # leaflet previsualization plots selectizers
  observe({
    req(pa_data())
    updatePickerInput(session, "previsualization_plot_species", choices = c(names(pa_data()[!sapply(pa_data(), is.null)]), "None"))
  })

  observe({
    req(fit_layers())
    updatePickerInput(session, "previsualization_plot_layer", choices = c(names(fit_layers()), "None"))
  })

  # * Validate inputs ----
  observeEvent(pa_files_input(), {
    # Check is not null and it was possible to upload the data
    if (is.null(pa_files_input())){
      pa_data(NULL)
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
      data <- apply(pa_files_input(), 1, function(x){
        glossa::read_presences_absences_csv(x["datapath"], x["name"], show_modal = TRUE)
      })
      if (is.null(data)){
        data <- list(data)
      }
      names(data) <- sub(" ", "_", sub("([^.]+)\\.[[:alnum:]]+$", "\\1", pa_files_input()[, "name"]))

      pa_data(data)
      w$hide()
    }
  })

  observeEvent(fit_layers_input(), {
    # Check is not null and it was possible to upload the data
    if (is.null(fit_layers_input())){
      fit_layers(NULL)
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
      fit_layers(glossa::read_fit_layers_zip(fit_layers_input()[, "datapath"], show_modal = TRUE))

      w$hide()
    }
  })

  observeEvent(proj_layers_input(), {
    # Check is not null and it was possible to upload the data
    if (is.null(proj_layers_input())){
      proj_layers(NULL)
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
      layers <- apply(proj_layers_input(), 1, function(x){
        glossa::validate_projection_layers_zip(x["datapath"], show_modal = TRUE)
      })
      if (is.null(layers)){
        layers <- list(layers)
      }
      names(layers) <- sub("\\.zip$", "", proj_layers_input()[,"name"])

      proj_layers(layers)
      w$hide()
    }
  })

  observeEvent(study_area_poly_input(), {
    # Check is not null and it was possible to upload the data
    if (is.null(study_area_poly_input())){
      study_area_poly(NULL)
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
      study_area_poly(glossa::read_extent_poly(study_area_poly_input()["datapath"], show_modal = TRUE))

      w$hide()
    }
  })

  observeEvent(pa_files_input(), {
    if (is.null(pa_files_input()) | is.null(pa_data())){
      pa_validation_table(NULL)
    } else {
      # Check which ones where properly loaded
      validation_table <- as.data.frame(pa_files_input())
      validation_table[, "validation"] <- !sapply(pa_data(), is.null)
      validation_table[, "name"] <- paste(as.character(icon("map-location-dot",style = "font-size:2rem; color:#007bff;")), validation_table[, "name"])
      pa_validation_table(validation_table)
    }
  })

  observeEvent(fit_layers_input(), {
    if (is.null(fit_layers_input())){
      fit_layers_validation_table(NULL)
    } else {
      validation_table <- as.data.frame(fit_layers_input())
      validation_table[, "name"] <- paste(as.character(icon("layer-group", style = "font-size:2rem; color:#007bff;")), validation_table[, "name"])
      if (is.null(fit_layers())){
        validation_table[, "validation"] <- FALSE
      } else {
        validation_table[, "validation"] <- !is.null(fit_layers())
      }
      fit_layers_validation_table(validation_table)
    }
  })

  observeEvent(c(proj_layers_input(), fit_layers_input()), {
    if (is.null(proj_layers_input()) | is.null(proj_layers())){
      proj_validation_table(NULL)
    } else {
      # Check which ones where properly loaded
      validation_table <- as.data.frame(proj_layers_input())
      validation_table[, "name"] <- paste(as.character(icon("forward",style = "font-size:2rem; color:#007bff;")), validation_table[, "name"])

      if(!is.null(fit_layers_input())){
        validation_table[, "validation"] <- !sapply(proj_layers(), is.null) & sapply(proj_layers_input()[, "datapath"], function(x){glossa::validate_fit_proj_layers(fit_layers_input()[, "datapath"], x, show_modal = TRUE)})
      } else {
        validation_table[, "validation"] <- !sapply(proj_layers(), is.null)
      }
      proj_validation_table(validation_table)
    }
  })

  observeEvent(study_area_poly_input(), {
    if (is.null(study_area_poly_input())){
      extent_validation_table(NULL)
    } else {
      validation_table <- as.data.frame(study_area_poly_input())
      validation_table[, "name"] <- paste(as.character(icon("crop",style = "font-size:2rem; color:#007bff;")), validation_table[, "name"])
      if (is.null(study_area_poly())){
        validation_table[, "validation"] <- FALSE
      } else {
        validation_table[, "validation"] <- !is.null(study_area_poly())
      }
      extent_validation_table(validation_table)
    }
  })

  # * Info buttons ----
  observe({
    bs4Dash::addPopover(
      id = "data_upload_info",
      options = list(
        content = "1) Presence/Absence Data: Upload a CSV file containing latitude and longitude columns for species occurrences, with an optional column 'pa' indicating presence (1) or absence (0).
        2) Environmental Layers: Upload a ZIP file containing raster layers of environmental variables.
        3) Projection Layers: Upload a ZIP file containing layers for projecting species distribution.
        4) Study Area: Upload a polygon defining the study area if you want to delimit the extent.",
        title = "Data upload",
        placement = "bottom",
        trigger = "hover"
      )
    )
  })

  observe({
    bs4Dash::addPopover(
      id = "analysis_options_options_info",
      options = list(
        content = "1) fit_layers: Fit the model and perform spatial prediction in the model fitting layers.
        2) projections: Predict in new layers (projection layers).",
        title = "Model options",
        placement = "bottom",
        trigger = "hover"
      )
    )
  })

  observe({
    bs4Dash::addPopover(
      id = "analysis_options_nr_info",
      options = list(
        content = "Model incorporating environmental data and spatial smoothing, with latitude and longitude as covariates.",
        title = "Native range",
        placement = "bottom",
        trigger = "hover"
      )
    )
  })

  observe({
    bs4Dash::addPopover(
      id = "analysis_options_sh_info",
      options = list(
        content = "Model estimating spatial probability of niche based solely on environmental variables.",
        title = "Suitable habitat",
        placement = "bottom",
        trigger = "hover"
      )
    )
  })

  observe({
    bs4Dash::addPopover(
      id = "analysis_options_others_info",
      options = list(
        content = "1) Functional Responses: Estimate the response curve of the probability for each value of the environmental variable.
        2) Cross-validation: Perform k-fold cross-validation. Warning: It may take some time.",
        title = "Others",
        placement = "bottom",
        trigger = "hover"
      )
    )
  })

  observe({
    bs4Dash::addPopover(
      id = "predictor_variables_info",
      options = list(
        content = "In this section, you can select which variables to include in the model for each species.",
        title = "Predictor variables",
        placement = "bottom",
        trigger = "hover"
      )
    )
  })

  # * Run GLOSSA analysis ----
  # Reset button
  observeEvent(input$reset_input, {
    # Reset data upload
    pa_data(NULL)
    pa_validation_table(NULL)
    fit_layers(NULL)
    fit_layers_validation_table(NULL)
    proj_layers(NULL)
    proj_validation_table(NULL)
    study_area_poly(NULL)
    extent_validation_table(NULL)

    # Reset analysis options
    updatePrettyCheckboxGroup(inputId = "analysis_options_nr", selected = character(0))
    updatePrettyCheckboxGroup(inputId = "analysis_options_sh", selected = character(0))
    updatePrettyCheckboxGroup(inputId = "analysis_options_other", selected = character(0))
    updatePrettySwitch(inputId = "round_digits", value = FALSE)
    updatePrettySwitch(inputId = "scale_layers", value = FALSE)
    updateNumericInput(inputId = "seed", value = numeric(0))
  })

  # Confirmation dialog
  observeEvent(input$run_button, {
    shinyWidgets::ask_confirmation(
      inputId = "run_button_confirmation",
      type = "question",
      title = "Want to confirm?",
      text = "GLOSSA analysis may require some time. Please double-check all inputs before proceeding.",
      btn_labels = c("Cancel", "Confirm"),
    )
  })

  observeEvent(input$run_button_confirmation, {

    req(input$run_button_confirmation == TRUE)

    # Validate input
    # Messages
    if (is.null(pa_files_input())) {
      showNotification("Please upload a P/A file", type = "error")
    }
    if (!all(pa_validation_table()[, "validation"] == TRUE)) {
      showNotification("Please upload valid P/A files", type = "error")
    }

    if (is.null(fit_layers_input())) {
      showNotification("Please upload model fit layers", type = "error")
    }
    if (!all(fit_layers_validation_table()[, "validation"] == TRUE)) {
      showNotification("Please upload valid model fit layers", type = "error")
    }

    if ("projections" %in% input$analysis_options_nr | "projections" %in% input$analysis_options_sh){
      if (is.null(proj_layers_input())) {
        showNotification("Please upload projection layers", type = "error")
      }
      if (!all(proj_validation_table()[, "validation"] == TRUE)) {
        showNotification("Please upload valid projection layers", type = "error")
      }
    }

    if (is.null(c(input$analysis_options_nr, input$analysis_options_sh))){
      showNotification("Select at least one option to compute from Native range and/or Suitable habitat", type = "error")
    }

    # Req
    req(pa_files_input(), all(pa_validation_table()[, "validation"] == TRUE))
    req(fit_layers_input(), all(fit_layers_validation_table()[, "validation"] == TRUE))
    if ("projections" %in% input$analysis_options_nr | "projections" %in% input$analysis_options_sh) {
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
      pa_data = pa_data(),
      fit_layers = fit_layers(),
      proj_files = proj_layers(),
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

    # Move to Reports tab
    bs4Dash::updateTabItems(session, "sidebar_menu", "reports")

    # Hide waiter
    w$hide()

    showNotification("GLOSSA analysis done!", duration = NULL, closeButton = TRUE, type = "message")

    presence_absence_list(glossa_results$presence_absence_list)
    covariate_list(glossa_results$covariate_list)
    projections_results(glossa_results$projections_results)
    other_results(glossa_results$other_results)
    pa_cutoff(glossa_results$pa_cutoff)
    habitat_suitability(glossa_results$habitat_suitability)

  })

  # * Reports server ----
  # Sp names report selectizer
  observe({
    req(presence_absence_list())
    updatePickerInput(session, "sp", label = NULL, choices = names(presence_absence_list()$model_pa), choicesOpt = list(icon = rep("fa-solid fa-globe", length(names(presence_absence_list()$model_pa)))))
  })

  # ** Prediction plot selectizers ----
  # Update pred_plot_layers picker
  observe({
    req(projections_results())
    updatePickerInput(session, "pred_plot_layers", choices = names(projections_results()[!unlist(lapply(projections_results(),is.null))]))
  })

  # Update pred_plot_model picker
  observe({
    req(input$pred_plot_layers)
    display_choices <- names(projections_results()[[input$pred_plot_layers]])
    display_val <- input$pred_plot_model
    if (!is.null(display_val)){
      if (!(display_val %in% display_choices)) {
        display_val <- NULL
      }
    }
    updatePickerInput(session, "pred_plot_model", choices = display_choices, selected = display_val)
  })

  # Update pred_plot_value picker
  observe({
    req(input$pred_plot_model)
    if (input$pred_plot_layers != "projections") {
      updatePickerInput(session, "pred_plot_value", choices = names(projections_results()[[input$pred_plot_layers]][[input$pred_plot_model]][[input$sp]]))
    } else {
      req(input$pred_plot_scenario)
      req(input$pred_plot_year)
      req(input$pred_plot_year <= length(projections_results()[[input$pred_plot_layers]][[input$pred_plot_model]][[input$sp]][[input$pred_plot_scenario]]))
      updatePickerInput(session, "pred_plot_value", choices = names(projections_results()[[input$pred_plot_layers]][[input$pred_plot_model]][[input$sp]][[input$pred_plot_scenario]][[input$pred_plot_year]]))
    }
  })

  # ** Layers plot selectizer ----
  observe({
    req(covariate_list())
    updatePickerInput(session, "layers_plot_mode", choices = names(covariate_list()[!unlist(lapply(covariate_list(),is.null))]))
  })

  observe({
    req(input$layers_plot_mode)
    if (input$layers_plot_mode == "fit_layers") {
      updatePickerInput(session, "layers_plot_cov", choices = names(covariate_list()[[input$layers_plot_mode]]))
    } else if (input$layers_plot_mode == "projections") {
      req(input$layers_plot_scenario)
      req(input$layers_plot_year)
      req(input$layers_plot_year <= length(covariate_list()[[input$layers_plot_mode]][[input$layers_plot_scenario]]))
      updatePickerInput(session, "layers_plot_cov", choices = names(covariate_list()[[input$layers_plot_mode]][[input$layers_plot_scenario]][[input$layers_plot_year]]))
    }
  })

  # ** Functional responses plot selectizer ----
  observe({
    req(other_results())
    req(other_results()[["response_curve"]])
    req(input$sp)
    updatePickerInput(session, "fr_plot_cov", choices = names(other_results()[["response_curve"]][[input$sp]]))
  })

  # ** Variable importance plot selectizer ----
  observe({
    req(other_results())
    req(other_results()[["variable_importance"]])
    updatePickerInput(session, "varimp_plot_mode", choices = names(other_results()[["variable_importance"]]))
  })

  # ** Cross-validation plot selectizer ----
  observe({
    req(other_results())
    req(other_results()[["cross_validation"]])
    req(input$sp)
    updatePickerInput(session, "cv_plot_mode", choices = names(other_results()[["cross_validation"]]))
  })

  # * Exports server ----
  # Update selectizers
  observe({
    req(presence_absence_list())
    req(projections_results())
    updateSelectInput(session, "export_sp", choices = names(presence_absence_list()$model_pa))
    export_layer_results <- names(projections_results()[!unlist(lapply(projections_results(),is.null))])
    updateSelectInput(session, "export_results", choices = export_layer_results)
    updateSelectInput(session, "export_models", choices = unique(as.vector((unlist((sapply(projections_results()[export_layer_results], names)))))))
    updateSelectInput(session, "export_values", choices = c("mean", "median", "sd", "q0.025", "q0.975", "diff", "potential_presences"))
    updateSelectInput(session, "export_layer_format", choices = c("tif", "asc", "nc"))
  })

  observeEvent(input$export_all, {
    req(presence_absence_list())
    req(projections_results())
    updateSelectInput(session, "export_sp", selected = names(presence_absence_list()$model_pa))
    export_layer_results <- names(projections_results()[!unlist(lapply(projections_results(),is.null))])
    updateSelectInput(session, "export_results", selected = export_layer_results)
    updateSelectInput(session, "export_models", selected = unique(as.vector((unlist((sapply(projections_results()[export_layer_results], names)))))))
    updateSelectInput(session, "export_values", selected = c("mean", "median", "sd", "q0.025", "q0.975", "diff", "potential_presences"))
    shinyWidgets::updatePrettySwitch(inputId = "export_model_data", value = TRUE)
    shinyWidgets::updatePrettySwitch(inputId = "export_var_imp", value = TRUE)
    shinyWidgets::updatePrettySwitch(inputId = "export_fr", value = TRUE)
    shinyWidgets::updatePrettySwitch(inputId = "export_cv", value = TRUE)
    shinyWidgets::updatePrettySwitch(inputId = "export_pa_cutoff", value = TRUE)
  })

  #=========================================================#
  # Outputs ----
  #=========================================================#

  # * Render selectizers ----
  # Select predictor variable for each species in new analysis tab
  output$predictor_selector <- renderUI({
    if (is.null(species_files_names()) | is.null(predictor_variables())) {
      validate("Upload species ocurrences and fit layers")
    }

    lapply(1:length(species_files_names()), function(i){
      selectInput(inputId = paste0("pred_vars_", i), label = species_files_names()[i], choices = predictor_variables(), selected = predictor_variables(), multiple = TRUE)
    })
  })

  # Update pred_plot_scenario picker
  output$pred_plot_scenario_picker <- renderUI({
    req(input$pred_plot_layers == "projections")
    pickerInput("pred_plot_scenario", label = NULL, width = "90%", choices = names(projections_results()[[input$pred_plot_layers]][[input$pred_plot_model]][[input$sp]]))
  })

  # Update pred_plot_year picker
  output$pred_plot_year_slider <- renderUI({
    req(input$pred_plot_layers == "projections")
    req(input$pred_plot_scenario)
    sliderInput(inputId = "pred_plot_year", label = "Year", value = 1, step = 1, round = TRUE, min = 1, max = length(projections_results()[[input$pred_plot_layers]][[input$pred_plot_model]][[input$sp]][[input$pred_plot_scenario]]))
  })

  # Update layers plot selectizers
  output$layers_plot_scenario_picker <- renderUI({
    req(input$layers_plot_mode == "projections")
    pickerInput("layers_plot_scenario", label = NULL, width = "90%", choices = names(covariate_list()[[input$layers_plot_mode]]))
  })

  output$layers_plot_year_slider <- renderUI({
    req(input$layers_plot_mode == "projections")
    req(input$layers_plot_scenario)
    sliderInput(inputId = "layers_plot_year", label = "Year", round = TRUE, step = 1, width = "90%", value = 1, min = 1, max = length(covariate_list()[[input$layers_plot_mode]][[input$layers_plot_scenario]]))
  })

  # * Input validation table ----
  # Render uploaded files as a DT table
  output$uploaded_files <- DT::renderDT(
    if (is.null(rbind(pa_validation_table(), fit_layers_validation_table(), proj_validation_table(), extent_validation_table()))) {
      DT::datatable(NULL)
    } else {
      rbind(pa_validation_table(), fit_layers_validation_table(), proj_validation_table(), extent_validation_table()) %>%
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

  # * Previsualization of input ----
  # base plot
  output$previsualization_plot <- renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::setView(0, 0, zoom = 1)
  })

  # plot proxy to update leaflet visualization
  previsualization_plot <- leaflet::leafletProxy("previsualization_plot", session)

  # add presence/absence points
  observeEvent(input$previsualization_plot_species, {
    req(pa_data())
    if(input$previsualization_plot_species != "None"){
      previsualization_plot %>%
        leaflet::setView(0, 0, zoom = 1) %>%
        leaflet::clearMarkers() %>%
        leaflet::addCircleMarkers(data = pa_data()[[input$previsualization_plot_species]],
                                  lng = pa_data()[[input$previsualization_plot_species]][, long_lat_cols()[1]],
                                  lat = pa_data()[[input$previsualization_plot_species]][, long_lat_cols()[2]],
                                  color = ~ifelse(pa == 1, "green", "black"), radius = 5)
    } else {
      previsualization_plot %>%
        leaflet::clearMarkers()
    }
  })

  # add environmental raster layers
  observeEvent(input$previsualization_plot_layer, {
    req(fit_layers())
    if (input$previsualization_plot_layer != "None"){
      previsualization_plot %>%
        leaflet::clearImages() %>%
        leaflet::addRasterImage(terra::crop(fit_layers()[input$previsualization_plot_layer], ext(-180, 180, -87, 87)),
                                colors = c("#A1D4B1","#2BAF90","#F1A512","#DD4111","#8C0027"), opacity = 0.5)
    } else {
      previsualization_plot %>%
        leaflet::clearImages()
    }
  })

  # add study area polygon
  observeEvent(input$previsualization_plot_extent, {
    req(study_area_poly())

    if(!is.null(study_area_poly()[[1]]) & input$previsualization_plot_extent){
      previsualization_plot %>%
        leaflet::clearShapes() %>%
        addPolygons(data = study_area_poly(), color = "#353839", fill = TRUE, fillColor = "353839", fillOpacity = 0.25)
    } else{
      previsualization_plot %>%
        leaflet::clearShapes()
    }
  })
  observeEvent(study_area_poly(), {
    req(study_area_poly())
    shinyWidgets::updatePrettySwitch(inputId = "previsualization_plot_extent", value = TRUE)
  })


  # * Sparkline plots box ----
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

      if (input$pred_plot_layers == "projections" & !is.null(habitat_suitability()[[input$pred_plot_layers]]) & !is.null(input$pred_plot_scenario)){
        # Check if future data along with input scenario are available
        sparkline_data1 <- habitat_suitability()[[input$pred_plot_layers]][["covered_area"]][[input$sp]][[input$pred_plot_scenario]]
        sparkline_data2 <- habitat_suitability()[[input$pred_plot_layers]][["suit_prob"]][[input$sp]][[input$pred_plot_scenario]]
        description1_2 <- paste("%", input$pred_plot_scenario)
      } else if (input$pred_plot_layers != "projections" & !is.null(habitat_suitability()[[input$pred_plot_layers]])) {
        sparkline_data1 <- habitat_suitability()[[input$pred_plot_layers]][["covered_area"]][[input$sp]]
        sparkline_data2 <- habitat_suitability()[[input$pred_plot_layers]][["suit_prob"]][[input$sp]]
        description1_2 <- paste("%", input$pred_plot_scenario)
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
  outputOptions(output, "spark_boxes", suspendWhenHidden = FALSE) # To render before showing the tab

  # * Prediction plot ----
  prediction_plot <- reactive({
    prediction_layer <- NULL
    pa_points <- NULL
    legend_label <- NULL

    if (!is.null(input$pred_plot_value)) {
      if (input$pred_plot_layers == "fit_layers") {
        prediction_layer <- projections_results()[[input$pred_plot_layers]][[input$pred_plot_model]][[input$sp]][[input$pred_plot_value]]
      } else if (input$pred_plot_layers == "projections") {
        req(input$pred_plot_year)
        req(input$pred_plot_year <= length(projections_results()[[input$pred_plot_layers]][[input$pred_plot_model]][[input$sp]][[input$pred_plot_scenario]]))
        prediction_layer <- projections_results()[[input$pred_plot_layers]][[input$pred_plot_model]][[input$sp]][[input$pred_plot_scenario]][[input$pred_plot_year]][[input$pred_plot_value]]
      }
      legend_label <- input$pred_plot_value
    }

    if (!is.null(input$sp) & input$pa_points) {
      pa_points <- presence_absence_list()$model_pa[[input$sp]]
    }

    glossa::generate_prediction_plot(prediction_layer, pa_points, legend_label, inv_study_area_poly(), coords = long_lat_cols())
  })
  output$prediction_plot <- renderPlot({
    prediction_plot()
  })

  # * Layers plot ----
  cov_layers_plot <- reactive({
    p <- ggplot2::ggplot()

    if (!is.null(input$layers_plot_cov)){
      legend_label <- input$layers_plot_cov
      if (input$layers_plot_mode == "fit_layers") {
        plot_layers <- covariate_list()[[input$layers_plot_mode]][[input$layers_plot_cov]]

        p <- p +
          geom_spatraster(data = plot_layers) +
          scale_fill_gradientn(colours = c("#A1D4B1","#2BAF90","#F1A512","#DD4111","#8C0027"), na.value = "white",
                               name = legend_label)

      } else if (input$layers_plot_mode == "projections") {
        req(input$layers_plot_year)
        req(input$layers_plot_year <= length(covariate_list()[[input$layers_plot_mode]][[input$layers_plot_scenario]]))
        plot_layers <- covariate_list()[[input$layers_plot_mode]][[input$layers_plot_scenario]][[input$layers_plot_year]][[input$layers_plot_cov]]

        p <- p +
          geom_spatraster(data = plot_layers) +
          scale_fill_gradientn(colours = c("#A1D4B1","#2BAF90","#F1A512","#DD4111","#8C0027"), na.value = "white",
                               name = legend_label)
      }
    }

    if (!is.null(inv_study_area_poly())){
      p <- p +
        geom_sf(data = inv_study_area_poly(), color = "#353839", fill = "antiquewhite")
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
  output$cov_layers_plot<-renderPlot({
    cov_layers_plot()
  })

  # * Observations plot ----
  observations_plot <- reactive({
    p <- ggplot2::ggplot()

    if (!is.null(inv_study_area_poly())){
      p <- p +
        geom_sf(data = inv_study_area_poly(), color = "#353839", fill = "antiquewhite")
    }

    if (!is.null(input$sp)) {
      model_points <- presence_absence_list()$model_pa[[input$sp]]
      model_points <- model_points[model_points[, "pa"] == 1, c(long_lat_cols(), "pa")]
      model_points$type <- "keeped"

      raw_points <- presence_absence_list()$raw_pa[[input$sp]]
      raw_points <- raw_points[raw_points[, "pa"] == 1, c(long_lat_cols(), "pa")]
      decimal_digits <- switch(input$round_digits + 1, NULL, input$decimal_digits)
      if (!is.null(decimal_digits)) {
        raw_points[, long_lat_cols()[1]] <- round(raw_points[, long_lat_cols()[1]], decimal_digits)
        raw_points[, long_lat_cols()[2]] <- round(raw_points[, long_lat_cols()[2]], decimal_digits)
      }
      raw_points <- dplyr::anti_join(raw_points, model_points, by = long_lat_cols())
      raw_points$type <- "discarded"

      tmp_data <- rbind(raw_points, model_points)

      p <- p +
        geom_point(data = tmp_data, ggplot2::aes(x = tmp_data[, long_lat_cols()[1]], y = tmp_data[, long_lat_cols()[2]], color = type)) +
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

  #=========================================================#
  # Exports ----
  #=========================================================#

  # * Export plots ----
  glossa::export_plot_server("export_previsualization_plot", prediction_plot())
  glossa::export_plot_server("export_pred_plot", prediction_plot())
  glossa::export_plot_server("export_layers_plot", cov_layers_plot())
  glossa::export_plot_server("export_observations_plot", observations_plot())
  glossa::export_plot_server("export_fr_plot", fr_plot())
  glossa::export_plot_server("export_varimp_plot", varimp_plot())
  glossa::export_plot_server("export_cv_plot", cv_plot())

  # * Export results downloadHandler ----
  output$export_button <- downloadHandler(
    filename = function() { paste("glossa_", format(Sys.time(), "%D_%X"), ".zip", sep="") },
    content = function(file) {
      w <- waiter::Waiter$new(html = tagList(
        img(src = "logo_glossa.gif", height = "200px")
      ),
      color = waiter::transparent(0.8)
      )
      w$show()
      export_files <- glossa_export(species = input$export_sp, models = input$export_models,
                                    layer_results = input$export_results, fields = input$export_values,
                                    model_data = input$export_model_data, fr = input$export_fr,
                                    prob_cut = input$export_pa_cutoff, varimp = input$export_var_imp,
                                    cross_val = input$export_cv, layer_format = input$export_layer_format,
                                    projections_results = projections_results(),
                                    presence_absence_list = presence_absence_list(),
                                    other_results = other_results(), pa_cutoff = pa_cutoff())

      zip::zip(zipfile = file, files = export_files, mode = "cherry-pick")
      w$hide()
    }
  )

  #=========================================================#
  # Stop app ----
  #=========================================================#
  observeEvent(input$stop_app,{
    stopApp()
  })
}
