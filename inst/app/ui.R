# Header ----
header <- bs4Dash::bs4DashNavbar(
  # Logo
  title = bs4Dash::dashboardBrand(
    title = strong("GLOSSA"),
    color = NULL,
    href = "https://github.com/jmestret",
    image = "logo_glossa.png",
    opacity = 1
  ),

  # Header options
  titleWidth = NULL,
  fixed = TRUE, # Fix navbar to top
  skin = "light",
  status = "white",
  border = TRUE,
  compact = FALSE,
  sidebarIcon = shiny::icon("bars"),

  # Right ui
  rightUi = tagList(
    # New analysis button
    tags$li(
      class = 'dropdown',
      bs4Dash::actionButton(
        inputId = "new_analysis_header",
        label = "New Analysis",
        icon = icon("circle-plus"),
        status = "primary",
        outline = FALSE
      )
    ),

    # Help button
    bs4Dash::bs4DropdownMenu(
      type = "notifications",
      badgeStatus = NULL,
      icon = icon("question"),
      headerText = strong("Help"),

      bs4Dash::notificationItem(
        text = "Data upload and analysis setup",
        icon = icon("circle-plus", class = "fa-solid"),
        status = "primary"
      ),
      bs4Dash::notificationItem(
        text = "Plots and results from the analysis",
        icon = icon("chart-simple"),
        status = "primary"
      ),
      bs4Dash::notificationItem(
        text = "Download your results",
        icon = icon("download"),
        status = "primary"
      ),
      bs4Dash::notificationItem(
        text = "Frequently asked questions",
        icon = icon("circle-question", class = "fa-solid"),
        status = "primary"
      ),
      bs4Dash::notificationItem(
        text = "Read or quick start guide",
        icon = icon("book"),
        status = "primary"
      )
    )
  )
)

# Sidebar ----
sidebar <- bs4Dash::bs4DashSidebar(
  # Sidebar options
  skin = "light",
  status = "primary",
  collapsed = FALSE,
  minified = TRUE,
  expandOnHover = TRUE,
  fixed = TRUE,
  id = "sidebar",
  customArea = NULL,

  bs4Dash::sidebarMenu(
    id = "sidebar_menu",

    # Home tab
    bs4Dash::menuItem(
      tabName = "home",
      icon = icon("house"),
      text = strong("Home")
    ),

    bs4Dash::sidebarHeader("Modelling"),
    # Input tab
    bs4Dash::menuItem(
      tabName = "new_analysis",
      icon = icon("circle-plus", class = "fa-solid"),
      text = "New Analysis"
    ),

    # Results tab
    bs4Dash::menuItem(
      tabName = "reports",
      icon = icon("chart-simple"),
      text = "Reports"
    ),

    # Export tab
    bs4Dash::menuItem(
      tabName = "exports",
      icon = icon("download"),
      text = "Exports"
    ),

    bs4Dash::sidebarHeader("Resources"),
    # Documentation tab
    bs4Dash::menuItem(
      tabName = "documentation",
      icon = icon("book"),
      text = "Documentation"
    ),

    # Paper tab
    bs4Dash::menuItem(
      tabName = "papers",
      icon = icon("bookmark", class = "fa-solid"),
      text = "Publications"
    ),

    # Updates tab
    bs4Dash::menuItem(
      tabName = "updates",
      icon = icon("bell", class = "fa-solid"),
      text = "Lastest updates"
    ),

    # FAQs tab
    bs4Dash::menuItem(
      tabName = "faqs",
      icon = icon("circle-question", class = "fa-solid"),
      text = "FAQs"
    ),

    # Contact tab
    bs4Dash::menuItem(
      tabName = "contact",
      icon = icon("envelope", class = "fa-solid"),
      text = "Contact"
    )
  )
)

# Body ----
body <- bs4Dash::bs4DashBody(
  waiter::useWaiter(), # include dependencies

  bs4Dash::tabItems(
    # * home tab ----
    bs4Dash::tabItem(
      tabName = "home",
      # Start first row
      shiny::fluidRow(
        # Welcome box
        bs4Dash::box(
          title = strong("Welcome to GLOSSA"),
          status = NULL,
          width = 6,
          height = 250,
          solidHeader = FALSE,
          background = NULL,
          collapsible = FALSE,
          headerBorder = FALSE,
          elevation = 2,
          label = icon("handshake", class = "fa-regular", style = "font-size:2rem"),
          shiny::fluidRow(
            # welcome text
            bs4Dash::column(
              width = 6,
              "Welcome to GLOSSA (GLobal Ocean Species Spatiotemporal Analysis). Explore marine species distributions worldwide, from past to future, across diverse climate scenarios."
            ),
            # welcome figure
            bs4Dash::column(
              width = 6,
              align = "center",
              img(src = "logo_glossa.png", height = "30%")
            )
          ),
          shiny::fluidRow(
            div(
              style = "position:absolute; bottom:10px; left:50%; transform: translateX(-50%); width:100%",
              bs4Dash::column(
                width = 12,
                align = "center",
                bs4Dash::actionButton(
                  inputId = "new_analysis_home",
                  label = "New Analysis",
                  icon = icon("circle-plus"),
                  status = "primary",
                  outline = TRUE,
                  width = "100%"
                )
              )
            )
          )
        ), # End welcome box

        # Demo box
        bs4Dash::box(
          title = strong("Demo Run"),
          status = NULL,
          width = 3,
          height = 250,
          solidHeader = FALSE,
          background = "lightblue",
          collapsible = FALSE,
          headerBorder = FALSE,
          elevation = 2,
          label = icon("rocket", style = "font-size:2rem"),
          shiny::fluidRow(
            bs4Dash::column(
              width = 12,
              "New to GLOSSA? Start with our quick demo. See GLOSSA in action with our toy example — explore marine species distribution modeling in minutes"
            )
          ),

          shiny::fluidRow(
            div(
              style = "position:absolute; bottom:10px; left:50%; transform: translateX(-50%); width:100%",
              bs4Dash::column(
                width = 12,
                align = "center",
                bs4Dash::actionButton(
                  inputId = "try_demo",
                  label = "View Example Report",
                  icon = NULL,
                  status = "secondary",
                  outline = FALSE,
                  width = "100%"
                )
              )
            )
          )
        ), # End demo box

        # Group box
        bs4Dash::box(
          title = strong("Meet Our Research Team"),
          status = NULL,
          width = 3,
          height = 250,
          solidHeader = FALSE,
          background = "purple",
          collapsible = FALSE,
          headerBorder = FALSE,
          elevation = 2,
          label = icon("people-group", style = "font-size:2rem"),
          shiny::fluidRow(
            bs4Dash::column(
              width = 12,
              "Discover the team behind GLOSSA at the iMARES group, ICM-CSIC, Barcelona, Spain. Get to know the developers on GitHub. Click below to explore our research group."
            )
          ),

          shiny::fluidRow(
            div(
              style = "position:absolute; bottom:10px; left:50%; transform: translateX(-50%); width:100%",
              bs4Dash::column(
                width = 12,
                align = "center",
                bs4Dash::actionButton(
                  inputId = "know_the_group",
                  label = "About Us",
                  icon = NULL,
                  status = "secondary",
                  outline = FALSE,
                  width = "100%"
                )
              )
            )
          )
        ) # End group box
      ), # End first row

      # Start second row
      shiny::fluidRow(
        # Documentation and guidelines
        bs4Dash::box(
          title = strong("Documentation and Guidelines"),
          status = NULL,
          width = 6,
          height = 180,
          solidHeader = FALSE,
          background = NULL,
          collapsible = FALSE,
          headerBorder = FALSE,
          elevation = 2,
          label = icon("book", style = "font-size:2rem"),
          shiny::fluidRow(
            bs4Dash::column(
              width = 12,
              "Curious about data preparation, result exportation, and our modeling procedure? Dive into our comprehensive GLOSSA documentation. Discover step-by-step tutorials, example datasets, and FAQs for optimized utilization. Begin your journey now to unleash GLOSSA's full potential!"
            )
          ),

          shiny::fluidRow(
            div(
              style = "position:absolute; bottom:10px; left:50%; transform: translateX(-50%); width:100%",
              bs4Dash::column(
                width = 12,
                align = "center",
                bs4Dash::actionButton(
                  inputId = "documentation_and_guidelines",
                  label = "Explore Documentation",
                  icon = NULL,
                  status = "primary",
                  outline = TRUE,
                  width = "100%"
                )
              )
            )
          )
        ), # End documentation and guidelines

        # Download data tutorials
        bs4Dash::box(
          title = strong("Download and Process Data: GBIF and ISIMIP"),
          status = NULL,
          width = 6,
          height = 180,
          solidHeader = FALSE,
          background = NULL,
          collapsible = FALSE,
          headerBorder = FALSE,
          elevation = 2,
          label = icon("globe", style = "font-size:2rem"),
          shiny::fluidRow(
            bs4Dash::column(
              width = 12,
              "Prepare your data effectively for GLOSSA with our guide. Learn how to download GBIF presence data and environmental layers from ISIMIP. Follow our simplified processing guidelines for smooth integration with the GLOSSA modeling framework."
            )
          ),

          shiny::fluidRow(
            div(
              style = "position:absolute; bottom:10px; left:50%; transform: translateX(-50%); width:100%",
              bs4Dash::column(
                width = 12,
                align = "center",
                shiny::fluidRow(
                  bs4Dash::column(
                    width = 6,
                    bs4Dash::actionButton(
                      inputId = "tutorial_1",
                      label = "Download GBIF Occurrences",
                      icon = NULL,
                      status = "primary",
                      outline = TRUE,
                      width = "100%"
                    )
                  ),
                  bs4Dash::column(
                    width = 6,
                    bs4Dash::actionButton(
                      inputId = "tutorial_2",
                      label = "Download ISIMIP Variables",
                      icon = NULL,
                      status = "primary",
                      outline = TRUE,
                      width = "100%"
                    )
                  )
                )
              )
            )
          )
        ) # End download data tutorials
      ) # End second row
    ),

    # * new_analysis tab ----
    bs4Dash::tabItem(
      tabName = "new_analysis",

      # Start first row
      shiny::fluidRow(
        # Upload data files
        bs4Dash::column(
          width = 6,
          bs4Dash::box(id = "data_upload",
              title = strong("Data Upload"),
              status = NULL,
              width = 12,
              height = 300,
              solidHeader = FALSE,
              background = NULL,
              collapsible = FALSE,
              headerBorder = FALSE,
              elevation = 2,
              label = actionButton(
                "data_upload_info",
                label = NULL,
                icon = icon("circle-info", class = "fa-solid fa-circle-info", style = "color:#007bff;"),
                style = "background-color: transparent; border: none; padding: 0;"
              ),

              shiny::fluidRow(
                bs4Dash::column(
                  width = 4,
                  glossa::file_input_area_ui(
                    "pa_files",
                    label = "Presence/Absence",
                    button_label = "Add CSV files",
                    multiple = TRUE,
                    accept = ".csv",
                    icon_name = "map-location-dot"
                  )
                ),

                bs4Dash::column(
                  width = 4,
                  glossa::file_input_area_ui(
                    "hist_layers",
                    label = "Study period",
                    button_label = "Add ZIP layers",
                    multiple = FALSE,
                    accept = ".zip",
                    icon_name = "layer-group"
                  )
                ),

                bs4Dash::column(
                  width = 4,
                  glossa::file_input_area_ui(
                    "fut_layers",
                    label = "Non-study period",
                    button_label = "Add ZIP layers",
                    multiple = TRUE,
                    accept = ".zip",
                    icon_name = "forward"
                  )
                )
              )
          )
        ), # End upload data files

        # Start analysis options
        bs4Dash::column(
          width = 6,
          tags$style(HTML("
          .pretty .state label span {
            font-weight: normal !important;
          }")),
          bs4Dash::box(
            title = strong("Analysis options"),
            status = NULL,
            width = 12,
            height = 300,
            solidHeader = FALSE,
            background = NULL,
            collapsible = FALSE,
            headerBorder = FALSE,
            elevation = 2,
            label = NULL,

            shiny::fluidRow(
              bs4Dash::column(
                width = 8,

                shiny::fluidRow(
                  bs4Dash::column(
                    width = 6,
                    shinyWidgets::prettySwitch(
                      inputId = "round_digits",
                      label = "Round coordinates",
                      status = "primary",
                      fill = TRUE
                    ),
                    style = 'display: inline-block; vertical-align: middle;'
                  ),

                  bs4Dash::column(
                    width = 6,
                    # Only show this panel if the round_digits is selected
                    conditionalPanel(
                      condition = "input['round_digits']",
                      numericInput("decimal_digits", NULL, 4, min = 0, max = 100, width = "50%"),
                    ),
                    style = 'display: inline-block; vertical-align: middle;'
                  )
                ),

                shiny::fluidRow(
                  bs4Dash::column(
                    width = 12,
                    shinyWidgets::prettySwitch(
                      inputId = "scale_layers",
                      label = "Standardize covariates",
                      status = "primary",
                      fill = TRUE
                    )
                  )
                )
              ),
              bs4Dash::column(
                width = 4,
                numericInput(
                  inputId = "seed",
                  label = "Set a seed",
                  value = NULL
                )
              )
            ),

            strong("Select one or more options to compute"),

            shiny::fluidRow(
              bs4Dash::column(
                width = 4,
                shinyWidgets::prettyCheckboxGroup(
                  inputId = "analysis_options_nr",
                  label = tags$span("Native range", shiny::actionButton("analysis_options_nr_info", label = "", icon = icon("circle-info", class = "fa-solid fa-circle-info", style = "color:#007bff;"), class = "btn btn-default action-button btn-xs", style="background-color:transparent;border-radius:0px;border-width:0px")),
                  choiceNames = c("Aggregated prediction", "Study projections", "Non-study projections"),
                  choiceValues = c("historical", "past", "future"),
                  selected = NULL,
                  status = "primary",
                  shape = "curve"
                )
              ),
              bs4Dash::column(
                width = 4,
                shinyWidgets::prettyCheckboxGroup(
                  inputId = "analysis_options_sh",
                  label = "Suitable habitat",
                  choiceNames = c("Aggregated prediction", "Study projections", "Non-study projections"),
                  choiceValues = c("historical", "past", "future"),
                  selected = NULL,
                  status = "primary",
                  shape = "curve"
                )
              ),
              bs4Dash::column(
                width = 4,
                shinyWidgets::prettyCheckboxGroup(
                  inputId = "analysis_options_other",
                  label = "Others",
                  choiceNames = c("Functional repsonses", "Cross-validation"),
                  choiceValues = c("functional_responses", "cross_validation"),
                  selected = NULL,
                  status = "primary",
                  shape = "curve"
                )
              )
            ),

            shiny::fluidRow(
              bs4Dash::actionButton(
                "run_button",
                "Run Job",
                icon = icon("play"),
                status = "primary",
                style = "margin-right:5px;"
              ),
              style="position:absolute;bottom:10px;"
            )
          )
        ) # End analysis options column
      ), # End first row

      # Start second row
      shiny::fluidRow(
        bs4Dash::column(
          width = 6,
          bs4Dash::box(
            title = strong("Predictor variables"),
            status = NULL,
            width = 12,
            height = 250,
            solidHeader = FALSE,
            background = NULL,
            collapsible = FALSE,
            headerBorder = FALSE,
            elevation = 2,
            label = NULL,

            bs4Dash::column(
              width = 12,
              uiOutput(outputId = "predictor_selector"),
              style = "overflow-y: scroll;height:210px"
            )
          )
        ),

        # Start study area poly  options
        bs4Dash::column(
          width = 6,
          bs4Dash::box(
            title = strong("Study area"),
            status = NULL,
            width = 12,
            height = 225,
            solidHeader = FALSE,
            background = NULL,
            collapsible = FALSE,
            headerBorder = FALSE,
            elevation = 2,
            label = glossa::customFileInput(
                "study_area_file",
                buttonLabel = "Upload polygon",
                accept = ".gpkg"
              ),

            plotOutput("study_area_plot", height = "100%")
          )
        ) # End study area poly column
      ), # End second row

      # Start third row
      shiny::fluidRow(
        bs4Dash::column(
          width = 12,
          bs4Dash::box(
            title = strong("Uploaded files"),
            status = NULL,
            width = 12,
            solidHeader = FALSE,
            background = NULL,
            collapsible = FALSE,
            headerBorder = FALSE,
            elevation = 2,
            label = NULL,

            bs4Dash::column(
              width = 12,
              DT::DTOutput("uploaded_files")
            )
          )
        )
      ) # End third row
    ),

    # * reports tab ----
    bs4Dash::tabItem(
      tabName = "reports",

      shiny::fluidRow(
        bs4Dash::box(
          title = NULL,
          status = NULL,
          width = 4,
          height = 95,
          solidHeader = FALSE,
          background = NULL,
          collapsible = FALSE,
          headerBorder = FALSE,
          elevation = 2,
          label = NULL,

          bs4Dash::column(
            width = 12,
            shinyWidgets::pickerInput(
              inputId = "sp",
              label = NULL,
              choices = NULL,
              choicesOpt = list()
            )
          )
        ),

        bs4Dash::column(
          width = 8,
          uiOutput("spark_boxes")
        )
      ),


      shiny::fluidRow(
        bs4Dash::column(
          width = 8,

          # Prediction map
          shiny::fluidRow(
            bs4Dash::box(
              title = strong("GLOSSA predictions"),
              status = NULL,
              width = 12,
              height = 465,
              solidHeader = FALSE,
              background = NULL,
              maximizable = TRUE,
              collapsible = FALSE,
              headerBorder = FALSE,
              elevation = 2,
              label = glossa::export_plot_ui("export_pred_plot"),

              sidebar = bs4Dash::boxSidebar(
                startOpen = FALSE,
                id = "pred_plot_sidebar",
                background = "#A97D87",
                icon = icon("ellipsis", class = "fa-solid fa-ellipsis", style = "color:#3b444b;"),
                shinyWidgets::pickerInput(
                  inputId = "pred_plot_time",
                  label = NULL,
                  choices = NULL,
                  width = "90%"
                ),

                shinyWidgets::pickerInput(
                  inputId = "pred_plot_mode",
                  label = NULL,
                  choices = NULL,
                  width = "90%"
                ),

                shinyWidgets::pickerInput(
                  inputId = "pred_plot_value",
                  label = NULL,
                  choices = NULL,
                  width = "90%",
                  options = list(size = 5)
                ),

                uiOutput("pred_plot_scenario_picker"), # Only shows for future prediction

                uiOutput("pred_plot_year_past_slider"), # Only shows for past prediction
                uiOutput("pred_plot_year_future_slider"), # Only shows for future prediction


                shinyWidgets::prettySwitch(
                  inputId = "pa_points",
                  label = "Show points used for model fitting",
                  status = "primary",
                  fill = TRUE
                )
              ),

              plotOutput("prediction_plot", height = "100%")
            )
          )
        ),

        bs4Dash::column(
          width = 4,
          shiny::fluidRow(
            bs4Dash::box(
              title = strong("Environmental variables"),
              status = NULL,
              width = 12,
              height = 200,
              solidHeader = FALSE,
              background = NULL,
              maximizable = TRUE,
              collapsible = FALSE,
              headerBorder = FALSE,
              elevation = 2,
              label = glossa::export_plot_ui("export_layers_plot"),

              sidebar = bs4Dash::boxSidebar(
                startOpen = FALSE,
                id = "layers_plot_sidebar",
                background = "#A97D87",
                icon = icon("ellipsis", class = "fa-solid fa-ellipsis", style = "color:#3b444b;"),
                shinyWidgets::pickerInput(
                  inputId = "layers_plot_time",
                  label = NULL,
                  choices = NULL,
                  width = "90%"
                ),

                shinyWidgets::pickerInput(
                  inputId = "layers_plot_cov",
                  label = NULL,
                  choices = NULL,
                  width = "90%",
                  options = list(size = 5)
                ),

                uiOutput("layers_plot_scaled_picker"), # Only shows for historical prediction

                uiOutput("layers_plot_scenario_picker"), # Only shows for future prediction

                uiOutput("layers_plot_year_past_slider"), # Only shows for past prediction
                uiOutput("layers_plot_year_future_slider"), # Only shows for future prediction

              ),

              plotOutput("cov_layers_plot", height = "100%")
            )
          ),
          shiny::fluidRow(
            bs4Dash::box(
              title = strong("Presence validation"),
              status = NULL,
              width = 12,
              height = 200,
              solidHeader = FALSE,
              background = NULL,
              maximizable = TRUE,
              collapsible = FALSE,
              headerBorder = FALSE,
              elevation = 2,
              label = glossa::export_plot_ui("export_observations_plot"),

              sidebar = bs4Dash::boxSidebar(
                startOpen = FALSE,
                id = "observations_plot_sidebar",
                background = "#A97D87",
                icon = icon("ellipsis", class = "fa-solid fa-ellipsis", style = "color:#3b444b;")
              ),

              plotOutput("observations_plot", height = "100%")
            )
          )
        )
      ),

      shiny::fluidRow(
        bs4Dash::box(
          title = strong("Functional responses"),
          status = NULL,
          width = 4,
          height = 200,
          solidHeader = FALSE,
          background = NULL,
          maximizable = TRUE,
          collapsible = FALSE,
          headerBorder = FALSE,
          elevation = 2,
          label = glossa::export_plot_ui("export_fr_plot"),

          sidebar = bs4Dash::boxSidebar(
            startOpen = FALSE,
            id = "fr_plot_sidebar",
            background = "#A97D87",
            icon = icon("ellipsis", class = "fa-solid fa-ellipsis", style = "color:#3b444b;"),
            shinyWidgets::pickerInput(
              inputId = "fr_plot_cov",
              label = NULL,
              choices = NULL,
              width = "90%",
              options = list(size = 5)
            )
          ),

          plotOutput("fr_plot", height = "100%")
        ),

        bs4Dash::box(
          title = strong("Variable importance"),
          status = NULL,
          width = 4,
          height = 200,
          solidHeader = FALSE,
          background = NULL,
          maximizable = TRUE,
          collapsible = FALSE,
          headerBorder = FALSE,
          elevation = 2,
          label = glossa::export_plot_ui("export_varimp_plot"),

          sidebar = bs4Dash::boxSidebar(
            startOpen = FALSE,
            id = "varimp_plot_sidebar",
            background = "#A97D87",
            icon = icon("ellipsis", class = "fa-solid fa-ellipsis", style = "color:#3b444b;"),
            shinyWidgets::pickerInput(
              inputId = "varimp_plot_mode",
              label = NULL,
              choices = NULL,
              width = "90%"
            ),
          ),

          plotOutput("varimp_plot", height = "100%")
        ),

        bs4Dash::box(
          title = strong("Cross-validation"),
          status = NULL,
          width = 4,
          height = 200,
          solidHeader = FALSE,
          background = NULL,
          maximizable = TRUE,
          collapsible = FALSE,
          headerBorder = FALSE,
          elevation = 2,
          label = glossa::export_plot_ui("export_cv_plot"),

          sidebar = bs4Dash::boxSidebar(
            startOpen = FALSE,
            id = "fr_plot_sidebar",
            background = "#A97D87",
            icon = icon("ellipsis", class = "fa-solid fa-ellipsis", style = "color:#3b444b;"),
            shinyWidgets::pickerInput(
              inputId = "cv_plot_mode",
              label = NULL,
              choices = NULL,
              width = "90%",
              options = list(size = 5)
            )
          ),

          plotOutput("cv_plot", height = "100%")
        )
      )
    ),
    # * exports tab ----
    bs4Dash::tabItem(
      tabName = "exports",

      shiny::fluidRow(
        bs4Dash::column(
          width = 6, offset = 3,
          bs4Dash::box(id = "export_details",
              title = strong("Export details"),
              status = NULL,
              width = 12,
              solidHeader = FALSE,
              background = NULL,
              collapsible = FALSE,
              headerBorder = FALSE,
              elevation = 2,
              label = NULL,

              selectInput(
                "export_sp",
                label = "Species name",
                choices = NULL,
                selected = NULL,
                multiple = TRUE
              ),

              selectInput(
                "export_time",
                label = "Time period",
                choices = NULL,
                selected = NULL,
                multiple = TRUE
              ),

              selectInput(
                "export_mods",
                label = "Model prediction",
                choices = NULL,
                selected = NULL,
                multiple = TRUE
              ),

              selectInput(
                "export_fields",
                label = "Fields",
                choices = NULL,
                selected = NULL,
                multiple = TRUE
              ),

              selectInput(
                "export_layer_format",
                label = "Layers file type",
                choices = NULL,
                selected = NULL,
                multiple = FALSE
              ),

              strong("Other results"),

              shinyWidgets::prettySwitch(
                inputId = "export_model_data",
                label = "Export data used to fit the models",
                status = "primary",
                fill = TRUE
              ),

              shinyWidgets::prettySwitch(
                inputId = "export_var_imp",
                label = "Variable importance",
                status = "primary",
                fill = TRUE
              ),

              shinyWidgets::prettySwitch(
                inputId = "export_fr",
                label = "Functional responses",
                status = "primary",
                fill = TRUE
              ),

              shinyWidgets::prettySwitch(
                inputId = "export_cv",
                label = "Cross-validation metrics",
                status = "primary",
                fill = TRUE
              ),

              shinyWidgets::prettySwitch(
                inputId = "export_pa_cutoff",
                label = "P/A probability cutoff value",
                status = "primary",
                fill = TRUE
              ),

              glossa::downloadActionButton(
                outputId = "export_button",
                label = "Save GLOSSA results",
                icon = NULL,
                status = "primary",
                outline = FALSE,
                width = "100%"
              )
          ) # End box
        ) # End column
      ) # End fluidRow
    ) # End exports tab
  )
)

# UI ----
bs4Dash::bs4DashPage(
  header,
  sidebar,
  body,
  controlbar = NULL,
  title = NULL,
  freshTheme = NULL,
  preloader = list(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading ...")), color = "#3b444b"),
  options = NULL,
  fullscreen = FALSE,
  help = NULL,
  dark = FALSE,
  scrollToTop = FALSE,
  footer = bs4Dash::bs4DashFooter	(
    fixed = FALSE,
    left = span(
      "Developed by ",
      a(href = "https://github.com/jmestret", target = "_blank", "@jmestret"),
      " and ",
      a(href = "https://github.com/AlbaFuster", target = "_blank", "@AlbaFuster")
    ),
    right = tagList(
      img(src = "logo_csic.png", height="25px", align="center"),
      img(src = "logo_icm.png", height="25px", align="center"),
      img(src = "logo_so.png", height="25px", align="center"),
      img(src = "logo_imares.png", height="25px", align="center"),
      img(src = "logo_prooceans.png", height="25px", align="center")
    )
  )
)
