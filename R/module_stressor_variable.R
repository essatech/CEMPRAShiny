#' stressor_variable UI
#'
#' The UI portion of the main map module
#'
#' @param id The id for this module
#'
#' @return a tagList containing UI elements
#'
module_stressor_variable_ui <- function(id, stressor_name = NULL) {
  ns <- NS(id)

  # Use stressor_name if provided, otherwise extract from namespaced id
  # The id comes in as "main_map-Stressor_Name", we need just "Stressor_Name"
  if(is.null(stressor_name)) {
    # Extract stressor name by removing the "main_map-" prefix
    stressor_name <- sub("^main_map-", "", id)
  }

  # Use onclick attribute directly in HTML to avoid shinyjs registration issues
  # This ensures click handlers survive UI re-renders
  onclick_js <- sprintf(
    "Shiny.setInputValue('main_map-stressor_click', '%s', {priority: 'event'});",
    stressor_name
  )

  tags$div(
    class = "stack-box map-variable", id = ns("var_id"),
    onclick = onclick_js,
    style = "cursor: pointer;",
    shinydashboard::box(
      width = 12,
      background = "light-blue",
      class = "stack-box-border",
      htmlOutput(ns("variable_label")),
      htmlOutput(ns("variable_val_raw")),
      tags$div(actionButton(ns("response_plot"), icon("chart-line"), class = "response-button"),
        style = "float: right; display: inline-block;"
      ),
      tags$p("", style = "float: right;"),
      tags$div(numericInput(ns("hiddenload"), label = "hidden", value = 0), style = "display:none;")
    ) # end of box
  ) # end of div
}


#' stressor_variable Server
#'
#' The Server portion of the stressor_variable module
#'
#' @param mlabel Character. Label of the variable
#'
#'
#' @return None
#'
module_stressor_variable_server <- function(id, stressor_index = NA) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      # Store the module ID (stressor name) for dynamic index lookup
      module_stressor_id <- id

      # Helper function to dynamically look up current index for this stressor
      # This ensures the module always uses the correct index even after workbook replacement
      get_current_index <- function() {
        snames <- session$userData$rv_stressor_response$stressor_names
        inames <- session$userData$rv_stressor_response$interaction_names

        # First check if it's a regular stressor
        idx <- which(snames == module_stressor_id)
        if(length(idx) > 0) {
          return(idx[1])
        }

        # Then check if it's an interaction matrix
        idx <- which(inames == module_stressor_id)
        if(length(idx) > 0) {
          return(length(snames) + idx[1])
        }

        # Not found - return NA
        return(NA)
      }


      # Set the label
      output$variable_label <- renderUI({
        # print("Variable Label")
        current_index <- get_current_index()
        if(is.na(current_index)) return(NULL)

        label <- session$userData$rv_stressor_response$pretty_names[current_index]
        # Adjust if matrix interaction term
        if (current_index > length(session$userData$rv_stressor_response$pretty_names)) {
          label <- session$userData$rv_stressor_response$interaction_names[current_index - length(session$userData$rv_stressor_response$pretty_names)]
        }
        label <- paste0(label, "  ")
        tags$p(label, style = "float: left;")
      })

      # Change mouse-over raw value
      output$variable_val_raw <- renderUI({
        current_index <- get_current_index()
        if(is.na(current_index)) return(NULL)

        sname <- session$userData$rv_stressor_response$stressor_names[current_index]

        # Adjust if matrix interaction term
        if (current_index > length(session$userData$rv_stressor_response$stressor_names)) {
          sname <- session$userData$rv_stressor_response$interaction_names[current_index - length(session$userData$rv_stressor_response$pretty_names)]
        }

        if (is.null(session$userData$rv_stressor_response$active_values_raw)) {
          tags$p(" ", style = "float: left; display:inline;")
        } else {
          raw_vals <- session$userData$rv_stressor_response$active_values_raw$Mean
          names <- session$userData$rv_stressor_response$active_values_raw$Stressor
          target_val <- raw_vals[which(names == sname)]
          target_val <- ifelse(is.na(target_val), " ", paste0(" ", target_val))
          tags$p(target_val, style = "float: left; display:inline; padding-left: 8px; font-size: 90%;")
        }
      })


      # ------------------------------------------------------
      # Set selected class as selected variable
      # Should it be styled as selected or light blue un-selected
      # ------------------------------------------------------
      observe({
        req(input$hiddenload)
        # print("Selected Variable")

        # MJB added here June 6 2023

        # Get current index dynamically
        current_index <- get_current_index()

        # Set the stressor response object as a reactive value
        if (!(is.na(current_index))) {

          active <- session$userData$rv_stressor_response$active_layer
          current <- session$userData$rv_stressor_response$stressor_names[current_index]


          # Fix name if selecting an interaction matrix
          if (is.na(current)) {
            # Assume interaction matrix
            current <- session$userData$rv_stressor_response$interaction_names[current_index - length(session$userData$rv_stressor_response$pretty_names)]
          }


         if(is.null(current)) {
           current <- NA # MJB added - fix exception for null
         }

          if (!(is.na(current)) & !(is.na(active))) {

            # Special case for system_capacity
            if (active == "system_capacity") {
              q_code <- paste0("jQuery('#main_map-var_id').addClass('var-selected');")
              shinyjs::runjs(code = q_code)
            }
            if (active != "system_capacity") {
              q_code <- paste0("jQuery('#main_map-var_id').removeClass('var-selected');")
              shinyjs::runjs(code = q_code)
            }

            current_id <- current

            # JS can not have space in IDs
            current_id <- gsub(" ", "__", current_id, fixed = TRUE)
            active_id <- gsub(" ", "__", active, fixed = TRUE)

            # deal with space and jQuery ID
            if(grepl(" ", current_id)) {
              current_id <- gsub(" ", "\\\ ", current_id, fixed = TRUE)
            }

            if (active_id == current) {
              # print("Adding class")
              q_code <- paste0("jQuery('#main_map-", current_id, "-var_id').addClass('var-selected');")
              shinyjs::runjs(code = q_code)
            } else {
              q_code <- paste0("jQuery('#main_map-", current_id, "-var_id').removeClass('var-selected');")
              shinyjs::runjs(code = q_code)
            }
          }
        }
      })


      # NOTE: Click events for stressor selection are now handled via direct HTML onclick
      # in the UI function (module_stressor_variable_ui) which calls Shiny.setInputValue().
      # This approach survives UI re-renders, unlike shinyjs::onclick().
      # The observer for input$stressor_click is in module_main_map.R.



      #-------------------------------------------------------
      # Stressor response modal dialog
      #-------------------------------------------------------
      # Open the stressor response dialog box
      observeEvent(input$response_plot, {

        # Dont load until button clicked
        req(input$hiddenload)

        # Get current index dynamically
        current_index <- get_current_index()
        if(is.na(current_index)) return(NULL)

        this_var <- session$userData$rv_stressor_response$pretty_names[current_index]
        # print(paste0("Stressor response modal is open for ... ", this_var))

        # Increment modal refresh counter
        print("----SR MODAL OPEN ------")
        print(current_index)

        # -------------------------------------------------------
        # If interaction matrix - show but not editable
        if (is.na(this_var)) {
          # Assume interaction matrix
          this_var <- session$userData$rv_stressor_response$interaction_names[current_index - length(session$userData$rv_stressor_response$pretty_names)]

          showModal(modalDialog(
            title = paste0("Stressor-Response Relationship: ", this_var),
            tagList(
              tags$p("Custom 2-way matrix interaction surface: The 2-factor interaction matrices are defined in the Stressor-Response Excel workbook. Customizable 2-factor interaction matrices may be (optionally) included by users to specify non-additive interactions between stressor variables (e.g., antagonistic, synergistic, etc.). If included, these matrices define the mean system capacity at different combination levels between two stressors. This can be especially important to capture conditional effects, attenuating or exacerbating factors, and/or compound variance and uncertainties. The 2-factor interaction matrices can also be a convenient mechanism to explore hypothetical and experimental scenarios."),
              tags$b("Mean System Capacity:"),
              fluidRow(
                shinydashboard::box(
                  width = 12,
                  tags$p(textOutput(ns("text_interaction_matrix"))),
                  DT::dataTableOutput(ns('interaction_matrix_main'))
                )
              ),
              tags$b("SD:"),
              fluidRow(
                shinydashboard::box(
                  width = 12,
                  tags$p(textOutput(ns("text_interaction_matrix"))),
                  DT::dataTableOutput(ns('interaction_matrix_sd'))
                )
              ),
              tags$b("Lower Limit:"),
              fluidRow(
                shinydashboard::box(
                  width = 12,
                  tags$p(textOutput(ns("text_interaction_matrix"))),
                  DT::dataTableOutput(ns('interaction_matrix_ll'))
                )
              ),
              tags$b("Upper Limit:"),
              fluidRow(
                shinydashboard::box(
                  width = 12,
                  tags$p(textOutput(ns("text_interaction_matrix"))),
                  DT::dataTableOutput(ns('interaction_matrix_ul'))
                )
              )
            ),
            easyClose = TRUE,
            size = "l",
            footer = NULL
          ))

          # end of matrix interaction surface
        } else {

          # -------------------------------------------------------
          # Normal variable - not a matrix interaction surface
          # Currently selected variable name
          # note can not use this_var <- session$userData$rv_stressor_response$active_layer

          this_var_pretty <- gsub("_", " ", this_var)

          # Main sheet attributes
          this_main <- session$userData$rv_stressor_response$main_sheet

          # -------------------------------------------------------
          # Get the current stressor name (not pretty name)
          this_stressor_name <- session$userData$rv_stressor_response$stressor_names[current_index]

          # Get current row from main_sheet for this stressor
          current_row <- this_main[this_main$Stressors == this_stressor_name, ]

          # Extract current values (handle NA/NULL gracefully)
          current_interaction <- if(nrow(current_row) > 0 && !is.null(current_row$Interaction)) current_row$Interaction[1] else NA
          current_linked <- if(nrow(current_row) > 0 && !is.null(current_row$Linked)) current_row$Linked[1] else NA
          current_function <- if(nrow(current_row) > 0 && !is.null(current_row$Function)) current_row$Function[1] else "continuous"
          current_stress_scale <- if(nrow(current_row) > 0 && !is.null(current_row$Stress_Scale)) current_row$Stress_Scale[1] else "linear"
          current_model <- if(nrow(current_row) > 0 && !is.null(current_row$Model)) current_row$Model[1] else "All"
          current_life_stages <- if(nrow(current_row) > 0 && !is.null(current_row$Life_stages)) current_row$Life_stages[1] else "adult"
          current_parameters <- if(nrow(current_row) > 0 && !is.null(current_row$Parameters)) current_row$Parameters[1] else "survival"
          current_units <- if(nrow(current_row) > 0 && !is.null(current_row$Units)) current_row$Units[1] else ""

          # Handle NA values for display
          current_interaction <- ifelse(is.na(current_interaction), "NA", as.character(current_interaction))
          current_linked <- ifelse(is.na(current_linked), "NA", as.character(current_linked))
          current_units <- ifelse(is.na(current_units), "", as.character(current_units))

          # Build dynamic life stages choices from life cycle profile
          life_stage_choices <- c(
            "adult" = "adult",
            "all" = "all",
            "sub_adult" = "sub_adult",
            "spawners" = "spawners",
            "stage_e" = "stage_e",
            "stage_0" = "stage_0",
            "stage_1" = "stage_1",
            "stage_2" = "stage_2",
            "stage_3" = "stage_3",
            "stage_4" = "stage_4",
            "stage_5" = "stage_5",
            "stage_Pb_1" = "stage_Pb_1",
            "stage_Pb_2" = "stage_Pb_2",
            "stage_Pb_3" = "stage_Pb_3",
            "stage_B_2" = "stage_B_2",
            "stage_B_3" = "stage_B_3",
            "stage_B_4" = "stage_B_4",
            "u" = "u",
            "smig" = "smig"
          )

          # Ensure current value is in choices
          if(!is.na(current_life_stages) && !(current_life_stages %in% life_stage_choices)) {
            life_stage_choices <- c(life_stage_choices, setNames(current_life_stages, current_life_stages))
          }

          showModal(modalDialog(
            title = paste0("Stressor-Response Relationship: ", this_var_pretty),
            class = "sr-modal",
            tagList(

              fluidRow(shinydashboard::box(
                width = 12,
                class = "sr-main-sheet-box",

                # Brief intro with expandable help
                tags$div(
                  class = "sr-help-section",
                  tags$p("Configure how this stressor-response relationship links to the focal species/systems.",
                         class = "small-helper-text", style = "display: inline;"),
                  actionLink(ns("toggle_sr_help"), "Learn more...", class = "help-toggle-link")
                ),

                # Expandable detailed help
                shinyjs::hidden(
                  tags$div(
                    id = ns("sr_help_expanded"),
                    class = "sr-help-expanded",
                    tags$p("The following inputs are sourced from the Main worksheet of the Stressor-Response workbook. Adjust the dropdowns to control min/max interactions, change the functional form, the model endpoint (Population Model vs Joe Model), and vital rate linkages (if associated with the Population Model).",
                           class = "small-helper-text")
                  )
                ),

                fluidRow(
                  column(
                    width = 3,
                    selectInput(
                      ns("s_Interaction"),
                      "Interactions:",
                      c(
                        "NA" = "NA",
                        "Minimum" = "Minimum",
                        "Maximum" = "Maximum"
                      ),
                      selected = current_interaction
                    ),
                    bsTooltip(
                      id = ns("s_Interaction"),
                      title = "Is the stressor part of a group of highly correlated stressors? If so, choose Minimum (or Maximum) and assign a linked group. Only the stressor with the lowest (or highest) response score will be used.",
                      placement = "top",
                      trigger = "hover"
                    ),
                    class = "sr-input-box"
                  ),
                  column(
                    width = 3,
                    selectInput(
                      ns("s_Linked"),
                      "Linked Groups:",
                      c(
                        "NA" = "NA",
                        "A" = "A",
                        "B" = "B",
                        "C" = "C",
                        "D" = "D",
                        "E" = "E",
                        "F" = "F",
                        "G" = "G",
                        "H" = "H"
                      ),
                      selected = current_linked
                    ),
                    bsTooltip(
                      id = ns("s_Linked"),
                      title = "Assign a group name if this stressor is linked to others. All stressors in the group must share the same Interaction type (Minimum or Maximum).",
                      placement = "top",
                      trigger = "hover"
                    ),
                    class = "sr-input-box"
                  ),

                  column(
                    width = 3,
                    selectInput(
                      ns("s_Function"),
                      "Function Type:",
                      c("continuous" = "continuous", "step" = "step"),
                      selected = current_function
                    ),
                    bsTooltip(
                      id = ns("s_Function"),
                      title = "Use 'continuous' for smooth interpolation (e.g., temperature). Use 'step' for discrete values (e.g., barrier counts).",
                      placement = "top",
                      trigger = "hover"
                    ),
                    class = "sr-input-box"
                  ),

                  column(
                    width = 3,
                    selectInput(
                      ns("s_Stress_Scale"),
                      "Raw Stressor Scale:",
                      c("linear" = "linear", "log" = "log"),
                      selected = current_stress_scale
                    ),
                    bsTooltip(
                      id = ns("s_Stress_Scale"),
                      title = "Choose how interpolation should occur: 'linear' for linear interpolation (default) or 'log' for natural logarithm.",
                      placement = "top",
                      trigger = "hover"
                    ),
                    class = "sr-input-box"
                  )
                ),

                fluidRow(
                  column(
                    width = 3,
                    selectInput(
                      ns("s_Model"),
                      "Model Endpoint:",
                      c(
                        "All" = "All",
                        "Joe Model" = "Joe Model",
                        "Population Model" = "Population Model"
                      ),
                      selected = current_model
                    ),
                    bsTooltip(
                      id = ns("s_Model"),
                      title = "Target model for this stressor: 'All' (both models), 'Joe Model', or 'Population Model'.",
                      placement = "top",
                      trigger = "hover"
                    ),
                    class = "sr-input-box"
                  ),
                  column(
                    width = 3,
                    selectInput(
                      ns("s_Life_stages"),
                      "Life Stages (Pop. Only):",
                      life_stage_choices,
                      selected = current_life_stages
                    ),
                    bsTooltip(
                      id = ns("s_Life_stages"),
                      title = "For Population Model: define which life stage this stressor affects (e.g., stage_0, stage_1, adult).",
                      placement = "top",
                      trigger = "hover"
                    ),
                    class = "sr-input-box"
                  ),
                  column(
                    width = 3,
                    selectInput(
                      ns("s_Parameters"),
                      "Vital Rate (Pop. Only):",
                      c(
                        "survival" = "survival",
                        "capacity" = "capacity",
                        "fecundity" = "fecundity"
                      ),
                      selected = current_parameters
                    ),
                    bsTooltip(
                      id = ns("s_Parameters"),
                      title = "For Population Model: which vital rate parameter is modified (survival, capacity, or fecundity).",
                      placement = "top",
                      trigger = "hover"
                    ),
                    class = "sr-input-box"
                  ),
                  column(
                    width = 3,
                    textInput(ns("s_Units"), "Raw Stressor Units:", value = current_units),
                    bsTooltip(
                      id = ns("s_Units"),
                      title = "Specify the units for the raw stressor values (e.g., Celsius, mg/L, count).",
                      placement = "top",
                      trigger = "hover"
                    ),
                    class = "sr-input-box"
                  )
                ),

              )), 
              
              
              
              
              fluidRow(
                shinydashboard::box(
                  width = 12,
                  class = "sr-chart-box",

                  # Brief intro with expandable help for chart
                  tags$div(
                    class = "sr-help-section",
                    tags$p("The graph shows the dose-response relationship. ",
                           class = "small-helper-text", style = "display: inline;"),
                    actionLink(ns("toggle_chart_help"), "Learn more...", class = "help-toggle-link")
                  ),

                  # Expandable detailed help for chart
                  shinyjs::hidden(
                    tags$div(
                      id = ns("chart_help_expanded"),
                      class = "sr-help-expanded",
                      tags$p("The x-axis shows raw stressor values and the y-axis shows the stressor-response score (mean system capacity). The red line represents the mean value, red shading shows one standard deviation, and grey shading represents upper/lower bounds. Click and drag to zoom in; double-click to reset.",
                             class = "small-helper-text")
                    )
                  ),

                  dygraphOutput(ns("dose_response_plot"))
                )
              ),
              fluidRow(
                shinydashboard::box(
                  width = 12,
                  
                  navset_card_underline(
                    nav_panel(
                      "Stressor-Response Input Data",
                      
                      tagList(
                        tags$p(
                          "Edit the underlying Stressor-Response Relationship. Double-click a cell to edit its value. Remember the SD, Lower Limit, and Upper Limit define stochasticity and uncertainty of the response score for a given level of a stressor. Do not forget to click the green save button at the bottom before closing the tab.",
                          class = "small-helper-text"
                        ),
                        DTOutput(ns("stressor_response_dt"))
                      )
                    ),
                    
                    nav_panel("Raw Distribution", tagList(
                      tags$p(
                        "This histogram shows the distribution of raw stressor magnitude values across all locations. Purple dashed lines indicate the breakpoints defined in the stressor-response curve.",
                        class = "small-helper-text"
                      ),
                      fluidRow(
                        column(
                          width = 12,
                          tags$b("Distribution of Raw Stressor Values:"),
                          tags$p(textOutput(ns("text_preview"))),
                          plotOutput(ns("hist_vals_plot"), width = "100%", height = "300px")
                        )
                      )
                    )), 
                    
                    nav_panel("Correlated Stressors", tagList(
                      tags$p(
                        "This table shows stressors that are most correlated with the current stressor based on their magnitude values across all locations. High correlations (R\u00b2 close to 1) may indicate stressors that measure similar environmental conditions or share common drivers. Consider using the Interaction/Linked Groups settings to avoid double-counting highly correlated stressors in the cumulative effects assessment.",
                        class = "small-helper-text"
                      ),
                      fluidRow(
                        column(
                          width = 12,
                          tags$b("Top Correlated Stressors (Pearson R\u00b2):"),
                          tableOutput(ns("correlated_stressors_table"))
                        )
                      )
                    )),
                    nav_panel("Location SR Scores", tagList(
                      tags$p(
                        "This plot displays the stressor-response scores for each location, sorted from highest to lowest. Click the button below to generate the plot. For datasets with many locations, this may take a moment to render.",
                        class = "small-helper-text"
                      ),
                      fluidRow(
                        column(
                          width = 12,
                          actionButton(
                            ns("generate_location_plot"),
                            label = tagList(icon("chart-bar"), " Generate Location Plot"),
                            class = "btn btn-primary",
                            style = "margin-bottom: 15px; color: white;"
                          ),
                          uiOutput(ns("location_sr_plot_ui"))
                        )
                      )
                    ))
                    
                  ),
                  
                  actionButton(
                    ns("close_sr_modal"),
                    label = tagList(icon("save"), "Save SR Function Updates and Close Module"),
                    style = "margin: 15px; color: white;",
                    class = "btn btn-success"
                  )
                )
              )
            ),
            easyClose = TRUE,
            size = "l",
            footer = NULL
          ))
        }
      })
      
      # main_map-BEC Unit Score runoff-var_id
      # #main_map-MWAT Max. Wekly Avg. Strm Tmp-var_id


      #-------------------------------------------------------
      # Close stressor response modal with custom button
      # Save form values back to main_sheet reactive object
      #-------------------------------------------------------
      observeEvent(input$close_sr_modal, {
        print("sr modal closed - saving main sheet updates")

        # Get current index dynamically
        current_index <- get_current_index()

        # Get the current stressor name
        this_stressor_name <- if(!is.na(current_index)) {
          session$userData$rv_stressor_response$stressor_names[current_index]
        } else {
          module_stressor_id  # Fallback to module ID
        }

        # Find the row index in main_sheet for this stressor
        row_idx <- which(session$userData$rv_stressor_response$main_sheet$Stressors == this_stressor_name)

        if(length(row_idx) > 0) {
          # Update main_sheet with form values
          # Handle NA values for Interaction and Linked
          interaction_val <- input$s_Interaction
          interaction_val <- ifelse(interaction_val == "NA" || is.null(interaction_val), NA, interaction_val)

          linked_val <- input$s_Linked
          linked_val <- ifelse(linked_val == "NA" || is.null(linked_val), NA, linked_val)

          # Update each column in main_sheet
          session$userData$rv_stressor_response$main_sheet$Interaction[row_idx] <- interaction_val
          session$userData$rv_stressor_response$main_sheet$Linked[row_idx] <- linked_val
          session$userData$rv_stressor_response$main_sheet$Function[row_idx] <- input$s_Function
          session$userData$rv_stressor_response$main_sheet$Stress_Scale[row_idx] <- input$s_Stress_Scale
          session$userData$rv_stressor_response$main_sheet$Model[row_idx] <- input$s_Model
          session$userData$rv_stressor_response$main_sheet$Life_stages[row_idx] <- input$s_Life_stages
          session$userData$rv_stressor_response$main_sheet$Parameters[row_idx] <- input$s_Parameters
          session$userData$rv_stressor_response$main_sheet$Units[row_idx] <- input$s_Units

          print(paste0("Updated main_sheet for stressor: ", this_stressor_name))
          print(paste0("  Interaction: ", interaction_val))
          print(paste0("  Linked: ", linked_val))
          print(paste0("  Function: ", input$s_Function))
          print(paste0("  Stress_Scale: ", input$s_Stress_Scale))
          print(paste0("  Model: ", input$s_Model))
          print(paste0("  Life_stages: ", input$s_Life_stages))
          print(paste0("  Parameters: ", input$s_Parameters))
          print(paste0("  Units: ", input$s_Units))
        }

        # Trigger reload of data
        session$userData$rv_srdt$reload <- 2
        print(session$userData$rv_srdt$reload)
        removeModal()
      })


      #-------------------------------------------------------
      # Toggle observers for expandable help sections
      #-------------------------------------------------------

      # Toggle main sheet help
      observeEvent(input$toggle_sr_help, {
        shinyjs::toggle("sr_help_expanded")
        if (input$toggle_sr_help %% 2 == 1) {
          shinyjs::html("toggle_sr_help", "Show less")
        } else {
          shinyjs::html("toggle_sr_help", "Learn more...")
        }
      })

      # Toggle chart help
      observeEvent(input$toggle_chart_help, {
        shinyjs::toggle("chart_help_expanded")
        if (input$toggle_chart_help %% 2 == 1) {
          shinyjs::html("toggle_chart_help", "Show less")
        } else {
          shinyjs::html("toggle_chart_help", "Learn more...")
        }
      })


      #-------------------------------------------------------
      # Stressor response modal dialog
      #-------------------------------------------------------
      # Add table data as data table
      # This is the doese response relationship for each stressor
      output$stressor_response_dt <- renderDT({
        
        names(session$userData$rv_stressor_response)
        session$userData$rv_stressor_response$interaction_names
        session$userData$rv_stressor_response$active_layer
        session$userData$rv_stressor_response$main_sheet
        
        # Get the associated data from the main sheet
        m_sheet <- session$userData$rv_stressor_response$main_sheet[session$userData$rv_stressor_response$main_sheet$Stressors == session$userData$rv_stressor_response$active_layer, ]
        
        # Do not run on app load
        req(session$userData$rv_stressor_response$active_layer)
        
        # Trigger reloaf of data
        print("----SR MODAL DATA ------")
        session$userData$rv_srdt$reload <- 1
        print(session$userData$rv_srdt$reload)
        
        # Get all SR data
        sr_data <- isolate(session$userData$rv_stressor_response$sr_dat)
        
        # Nov 9 2024 update MJB isolate causing issue with refresh
        # sr_data <- session$userData$rv_stressor_response$sr_dat
        
        # Filter for target layer
        this_var <- session$userData$rv_stressor_response$active_layer # e.g., temperature
        table_vals <- sr_data[[this_var]] # e.g., temperature

        # Build the JS DT Data Table Object
        DT::datatable(
          table_vals,
          # The Stressor column is not editable
          editable = TRUE, # list(target = "cell", disable = list(columns = c(1))),
          colnames = c(
            "Raw Stressor Value" = "value", "Stressor-Response Score (0-100)" = "mean_system_capacity",
            "SD of Resp. (0-100)" = "sd", "Lower Limit of Resp. (0)" = "lwr", "Upper Limit of Resp. (100)" = "upr"
          ),
          filter = "none",
          selection = "none",
          rownames = FALSE,
          class = "cell-border stripe",
          options = list(
            pageLength = 500,
            info = FALSE,
            dom = "t",
            ordering = FALSE,
            columnDefs = list(list(
              className = "dt-left", targets = "_all"
            ))
          )
        )
      })

      # Create a proxy for the above table
      dt_proxy <- DT::dataTableProxy("main_map-Foot_flow-stressor_response_dt")
      
      
      #-------------------------------------------------------
      # Summary histogram of values
      #-------------------------------------------------------
      output$hist_vals_plot <- renderPlot({
        
        # Do not run on app load
        req(session$userData$rv_stressor_response$active_layer)
        
        print("Generating barplot...")
        
        # Get all SR data - and keep updating the plot as values are moved
        sr_data <- session$userData$rv_stressor_response$sr_dat
        # Filter for target layer
        this_var <- session$userData$rv_stressor_response$active_layer # e.g., temperature
        table_vals <- sr_data[[this_var]] # e.g., temperature
        
        # Get all stressor values
        # head(session$userData$rv_stressor_magnitude$sm_dat)
        
        sm_dat <- isolate(session$userData$rv_stressor_magnitude$sm_dat)
        sm_dat <- sm_dat[which(sm_dat$Stressor == this_var), ]

        # Check for valid data before plotting histogram
        valid_means <- sm_dat$Mean[!is.na(sm_dat$Mean)]
        if (length(valid_means) < 2) {
          # Not enough data for histogram - show message instead
          plot.new()
          text(0.5, 0.5, "Insufficient data to generate histogram.\nPlease check stressor values.",
               cex = 1.2, col = "gray50")
        } else {
          hist(valid_means, main = NA, xlab = this_var)
          for(ll in 1:length(table_vals$value)) {
            abline(v = table_vals$value[ll], col = "purple", lty = 2, lwd = 2)
          }
        }
        
      })


      #-------------------------------------------------------
      # Populate summary statistics for stressor in modal
      #-------------------------------------------------------
      # Display mean min and max
      output$text_preview <- renderText({
        #this_var <- isolate(session$userData$rv_stressor_response$active_layer) # e.g., temperature # OLD
        this_var <- session$userData$rv_stressor_response$active_layer # New Nov 9th 2024
        # Stressor magnitude data
        sm_df <- session$userData$rv_stressor_magnitude$sm_dat
        # Subset to target variable
        sm_sub <- sm_df[which(sm_df$Stressor == this_var), ]

        # Check for valid data
        valid_means <- sm_sub$Mean[!is.na(sm_sub$Mean)]
        if (length(valid_means) == 0) {
          return("No valid stressor values available. Please check that stressor magnitude data has been loaded correctly.")
        }

        my_mean <- round(mean(valid_means), 2)
        my_median <- round(median(valid_means), 2)
        my_min <- round(min(valid_means), 2)
        my_max <- round(max(valid_means), 2)

        data_rng_txt <- paste0("Summary of raw stressor values, Mean: ", my_mean, ", Median: ", my_median, " (Min: ", my_min, ", Max: ", my_max, ")")
        return(data_rng_txt)
      })


      #-------------------------------------------------------
      # Correlated Stressors Table
      #-------------------------------------------------------
      # Calculate and display top 10 most correlated stressors
      output$correlated_stressors_table <- renderTable({

        # Do not run on app load
        req(session$userData$rv_stressor_response$active_layer)

        # Get the current target stressor
        this_var <- session$userData$rv_stressor_response$active_layer

        # Get stressor magnitude data
        sm_dat <- session$userData$rv_stressor_magnitude$sm_dat

        # Get all unique stressors
        all_stressors <- unique(sm_dat$Stressor)

        # Need at least 2 stressors to calculate correlations
        if(length(all_stressors) < 2) {
          return(data.frame(
            Stressor = "Insufficient data",
            `R-squared` = NA,
            check.names = FALSE
          ))
        }

        # Reshape data from long to wide format (locations as rows, stressors as columns)
        # Use Mean values for correlation calculation
        sm_wide <- tryCatch({
          reshape2::dcast(sm_dat, HUC_ID ~ Stressor, value.var = "Mean", fun.aggregate = mean)
        }, error = function(e) {
          return(NULL)
        })

        if(is.null(sm_wide) || !(this_var %in% colnames(sm_wide))) {
          return(data.frame(
            Stressor = "Unable to calculate correlations",
            `R-squared` = NA,
            check.names = FALSE
          ))
        }

        # Get the target stressor values
        target_values <- sm_wide[[this_var]]

        # Calculate correlations with all other stressors
        other_stressors <- setdiff(colnames(sm_wide), c("HUC_ID", this_var))

        if(length(other_stressors) == 0) {
          return(data.frame(
            Stressor = "No other stressors available",
            `R-squared` = NA,
            check.names = FALSE
          ))
        }

        # Calculate Pearson R-squared for each stressor
        correlations <- sapply(other_stressors, function(stressor) {
          other_values <- sm_wide[[stressor]]
          # Remove NA pairs
          valid_idx <- !is.na(target_values) & !is.na(other_values)
          if(sum(valid_idx) < 3) {
            return(NA)
          }
          cor_val <- cor(target_values[valid_idx], other_values[valid_idx], method = "pearson")
          return(cor_val^2)  # R-squared
        })

        # Create data frame and sort by R-squared (descending)
        cor_df <- data.frame(
          Stressor = names(correlations),
          R_squared = as.numeric(correlations),
          stringsAsFactors = FALSE
        )

        # Remove NA values and sort
        cor_df <- cor_df[!is.na(cor_df$R_squared), ]
        cor_df <- cor_df[order(cor_df$R_squared, decreasing = TRUE), ]

        # Take top 10
        cor_df <- head(cor_df, 10)

        # Format R-squared for display
        cor_df$R_squared <- round(cor_df$R_squared, 3)

        # Rename columns for display
        colnames(cor_df) <- c("Stressor", "R\u00b2")

        # Add rank column
        if(nrow(cor_df) > 0) {
          cor_df <- cbind(Rank = 1:nrow(cor_df), cor_df)
        }

        return(cor_df)
      }, striped = TRUE, hover = TRUE, bordered = TRUE)


      #-------------------------------------------------------
      # Location SR Scores Plot (Lazy Loading)
      #-------------------------------------------------------
      # Reactive value to track if plot should be generated
      location_plot_trigger <- reactiveVal(0)

      # Reset trigger when modal opens (new stressor selected)
      observeEvent(input$response_plot, {
        location_plot_trigger(0)
      })

      # Generate plot when button is clicked
      observeEvent(input$generate_location_plot, {
        location_plot_trigger(location_plot_trigger() + 1)
      })

      # Render the plot UI only when triggered
      output$location_sr_plot_ui <- renderUI({
        req(location_plot_trigger() > 0)

        # Calculate dynamic height based on number of locations
        # Filter to only locations with valid data for this stressor
        sm_dat <- session$userData$rv_stressor_magnitude$sm_dat
        this_var <- session$userData$rv_stressor_response$active_layer
        sm_sub <- sm_dat[sm_dat$Stressor == this_var & !is.na(sm_dat$Mean), ]
        n_locations <- length(unique(sm_sub$HUC_ID))

        # Scale height with number of locations (14px per location, minimum 300px)
        plot_height <- max(300, n_locations * 14)

        tagList(
          plotOutput(ns("location_sr_dotplot"), height = paste0(plot_height, "px"), width = "100%")
        )
      })

      # Render the horizontal dot plot
      output$location_sr_dotplot <- renderPlot({
        req(location_plot_trigger() > 0)
        req(session$userData$rv_stressor_response$active_layer)

        # Get the current target stressor
        this_var <- session$userData$rv_stressor_response$active_layer

        # Get stressor magnitude data
        sm_dat <- session$userData$rv_stressor_magnitude$sm_dat

        # Filter for the target stressor and omit locations with missing/NA values
        sm_sub <- sm_dat[sm_dat$Stressor == this_var & !is.na(sm_dat$Mean), ]

        if(nrow(sm_sub) == 0) {
          plot.new()
          text(0.5, 0.5, "No data available for this stressor", cex = 1.2)
          return()
        }

        # Get stressor-response data to calculate scores
        sr_dat <- session$userData$rv_stressor_response$sr_dat[[this_var]]

        # Check if SR data exists for this stressor
        if(is.null(sr_dat) || nrow(sr_dat) == 0) {
          plot.new()
          text(0.5, 0.5, "No stressor-response curve defined for this stressor", cex = 1.2)
          return()
        }

        main_sheet <- session$userData$rv_stressor_response$main_sheet
        stress_scale <- main_sheet$Stress_Scale[main_sheet$Stressors == this_var]
        func_type <- main_sheet$Function[main_sheet$Stressors == this_var]

        # Default values if not found
        if(length(stress_scale) == 0 || is.na(stress_scale)) stress_scale <- "linear"
        if(length(func_type) == 0 || is.na(func_type)) func_type <- "continuous"

        # Calculate SR scores for each location using interpolation
        calculate_sr_score <- function(raw_value, sr_data, scale, func) {
          if(is.na(raw_value)) return(NA)

          x_vals <- sr_data$value
          y_vals <- sr_data$mean_system_capacity

          if(scale == "log") {
            x_vals <- log(x_vals + 1)
            raw_value <- log(raw_value + 1)
          }

          if(func == "step") {
            # Step function: find the closest lower breakpoint
            idx <- max(which(x_vals <= raw_value), 1, na.rm = TRUE)
            if(length(idx) == 0 || is.na(idx)) idx <- 1
            return(y_vals[idx])
          } else {
            # Continuous: linear interpolation
            score <- approx(x_vals, y_vals, xout = raw_value, rule = 2)$y
            return(score)
          }
        }

        # Calculate scores for all locations
        sm_sub$SR_Score <- sapply(sm_sub$Mean, function(x) {
          calculate_sr_score(x, sr_dat, stress_scale, func_type)
        })

        # Remove locations with NA scores
        sm_sub <- sm_sub[!is.na(sm_sub$SR_Score), ]

        if(nrow(sm_sub) == 0) {
          plot.new()
          text(0.5, 0.5, "No valid SR scores could be calculated", cex = 1.2)
          return()
        }

        # Get NAME column if it exists, otherwise use empty strings
        name_col <- if("NAME" %in% colnames(sm_sub)) sm_sub$NAME else rep("", nrow(sm_sub))

        # Create location labels (HUC_ID - NAME, max 20 chars)
        sm_sub$Location_Label <- mapply(function(huc_id, name) {
          if(is.na(name) || name == "") {
            label <- as.character(huc_id)
          } else {
            label <- paste0(huc_id, " - ", name)
          }
          if(nchar(label) > 20) {
            label <- paste0(substr(label, 1, 17), "...")
          }
          return(label)
        }, sm_sub$HUC_ID, name_col, SIMPLIFY = TRUE)

        # Sort by SR_Score (highest to lowest)
        sm_sub <- sm_sub[order(sm_sub$SR_Score, decreasing = FALSE), ]

        # Convert to factor to preserve order in plot
        sm_sub$Location_Label <- factor(sm_sub$Location_Label, levels = sm_sub$Location_Label)

        # Create the horizontal dot plot using ggplot2
        p <- ggplot(sm_sub, aes(x = SR_Score, y = Location_Label)) +
          geom_point(color = "#3b9ab2", size = 3) +
          geom_segment(aes(x = 0, xend = SR_Score, y = Location_Label, yend = Location_Label),
                       color = "#3b9ab2", alpha = 0.5) +
          scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
          labs(
            x = "Stressor-Response Score (%)",
            y = "Location",
            title = paste0("SR Scores by Location: ", gsub("_", " ", this_var))
          ) +
          theme_minimal() +
          theme(
            axis.text.y = element_text(size = 8),
            axis.text.x = element_text(size = 10),
            axis.title = element_text(size = 11),
            plot.title = element_text(size = 12, face = "bold"),
            panel.grid.major.y = element_line(color = "grey90"),
            panel.grid.minor = element_blank()
          )

        print(p)
      })


      #------------------------------------------------------------------------
      # Update a data value cell
      #------------------------------------------------------------------------
      # When there is an edit to a cell
      # update the stessor response reactive values
      observeEvent(input$stressor_response_dt_cell_edit, {
        
        # Get new value of edited cell
        info <- input$stressor_response_dt_cell_edit

        # Index list of stressor names
        this_var <- isolate(session$userData$rv_stressor_response$active_layer) # e.g., temperature

        # print(paste0("Edit value for ... ", this_var))

        # HUCs currently selected
        selected_raw <- session$userData$rv_clickedIds$ids
        # Fix format
        getID <- function(x) {
           strsplit(x, "\\|")[[1]][1]
        }
        selected_ids <- lapply(selected_raw, getID) %>% unlist()

        info$value <- as.numeric(info$value)
        info$value <- ifelse(is.na(info$value), 0, info$value)

        i <- as.numeric(info$row)
        j <- as.numeric(info$col)
        j <- j + 1 # Weird issue with row names missing
        k <- as.numeric(info$value)

        # Ensure value is ok
        if (j > 1) {
          # Keep system capacity within reasonable bounds
          k <- ifelse(k < 0, 0, k)
          k <- ifelse(k > 100, 100, k)
        }

        var <- c("value", "mean_system_capacity", "sd", "lwr", "upr")
        
        # Ensure that lower and upper bounds are never greater than or less than msc
        srow_vals <- isolate(session$userData$rv_stressor_response$sr_dat[[this_var]][i,])
        # Current mean system capacity
        c_msc <- srow_vals$mean_system_capacity
        
        if (j == 4) { # lwr
          # Lower bounds must always be less than msc
          k <- ifelse(k > c_msc, c_msc, k)
        }
        if (j == 5) { # upr
          # Upper bounds must always be greater than msc
          k <- ifelse(k < c_msc, c_msc, k)
        }
        
        # Update the table without reloading completely
        updated_data_tmp <- isolate(session$userData$rv_stressor_response$sr_dat[[this_var]])
        updated_data_tmp[i, j] <- k
        replaceData(dt_proxy, updated_data_tmp, resetPaging = FALSE) 
        # Set resetPaging to FALSE to keep the current paging

        # Update stressor response value
        session$userData$rv_stressor_response$sr_dat[[this_var]][i, j] <- k
        # Update the DT data table so the user knows what they have just done

        # Also invalidate the cumulative system capacity score in the stressor magnitude table
        print("click csc invalidated...")
        session$userData$rv_clickedIds_csc$csc <- NA
        

        
      })



      #------------------------------------------------------------------------
      # renderDygraph charts data visualization
      #------------------------------------------------------------------------
      output$dose_response_plot <- renderDygraph({

        # Filter for target layer
        this_var <- session$userData$rv_stressor_response$active_layer # e.g., temperature

        # print(paste0(this_var, " this_var in renderDygraph()"))
        # Get all SR data
        table_vals <- session$userData$rv_stressor_response$sr_dat[[this_var]]
        table_vals$lwr_sd <- table_vals$mean_system_capacity - table_vals$sd
        table_vals$upr_sd <- table_vals$mean_system_capacity + table_vals$sd
        table_vals <- table_vals[order(table_vals$mean_system_capacity), ]
        table_vals <- table_vals[order(table_vals$value), ]

        # Fix lower and upper sd bounds to be within range of limits
        table_vals$lwr_sd <- ifelse(table_vals$lwr_sd < table_vals$lwr, table_vals$lwr, table_vals$lwr_sd)
        table_vals$upr_sd <- ifelse(table_vals$upr_sd > table_vals$upr, table_vals$upr, table_vals$upr_sd)

        # Ensure no bad values
        table_vals <- table_vals[, c("value", "mean_system_capacity", "lwr", "upr", "lwr_sd", "upr_sd")]

        # Pretty label for plot
        pretty_lab <- gsub("_", " ", paste0("Stressor-Response Curve for ", this_var))

        # X-axis mouse-over formatting
        myvFormatter <- "function formatValue(v) {
              var prefix = 'Raw Stressor: ';
              return prefix + String(v);
        }"
        
        # Get additional stressor attributes
        
        # Create the x-axis label for the plot
        m_ms <- session$userData$rv_stressor_response$main_sheet
        m_ms <- m_ms[m_ms$Stressors == this_var, ]
        m_units <- m_ms$Units
        m_units <- ifelse(length(m_units) == 0, "", m_units)
        m_units <- ifelse(is.na(m_units), "", m_units)
        m_name <- gsub("_", " ", this_var)
        if(m_units != "") {
          m_name <- paste0(m_name, " (", m_units, ")")
        }
        xlab_plot <- paste0("Raw Stressor Magnitude Values: ", m_name)

        # Create the y-axis label for the plot
        m_ls <- m_ms$Life_stages
        m_ls <- ifelse(length(m_ls) == 0, "", m_ls)
        m_ls <- ifelse(is.na(m_ls), "", m_ls)
        m_param <- m_ms$Parameters
        m_param <- ifelse(length(m_param) == 0, "", m_param)
        m_param <- ifelse(is.na(m_param), "", m_param)
        m_name <- ": ("
        if(m_ls != "") {
          m_name <- paste0(m_name, "", m_ls)
        }
        if(m_param != "") {
          if(m_ls != "") {
            m_name <- paste0(m_name, " ", m_param)
          } else {
            m_name <- paste0(m_name, "", m_param)
          }
        }
        m_name <- paste0(m_name, ")")
        if(m_name == ": ()") {
          m_name <- ""
        }
        ylab_plot <- paste0("Stressor Response Score (%)", m_name)
        
        # Start and return the dygraph plot
        dygraph(table_vals, main = pretty_lab) %>%
          dyAxis("x", label = xlab_plot, valueFormatter = JS(myvFormatter)) %>%
          dyAxis("y", label = ylab_plot) %>%
          dySeries(c("lwr", "mean_system_capacity", "upr"), label = "msc", color = "grey") %>%
          dySeries(c("lwr_sd", "mean_system_capacity", "upr_sd"), label = "Response ", color = "red")
        
      })
      
      
      



      #------------------------------------------------------------------------
      #------------------------------------------------------------------------
      # render 2-factor interaction matrix table
      #------------------------------------------------------------------------
      #------------------------------------------------------------------------

      # Helper to get interaction matrix variable name
      get_interaction_var <- function() {
        current_index <- get_current_index()
        if(is.na(current_index)) return(NULL)
        snames <- session$userData$rv_stressor_response$stressor_names
        if(current_index > length(snames)) {
          return(session$userData$rv_stressor_response$interaction_names[current_index - length(snames)])
        }
        return(NULL)
      }

      output$interaction_matrix_main <- DT::renderDataTable({
          print("Rendering matrix intraction table...")
          this_var <- get_interaction_var()
          req(this_var)
          mat_data <- session$userData$rv_stressor_response$interaction_values[[this_var]]
          mtable <- mat_data$mat_msc[, 2:ncol(mat_data$mat_msc)]
          rnms <- unlist(mat_data$mat_msc[, 1])
          rnms <- as.numeric(rnms)
          mtable <- data.frame(mtable)
          rownames(mtable) <- rnms
          colnames_cust <- gsub("X", "", colnames(mtable))
          datatable(mtable,
          colnames = colnames_cust,
          )
      })

      output$interaction_matrix_sd <- DT::renderDataTable({
          print("Rendering matrix intraction table...")
          this_var <- get_interaction_var()
          req(this_var)
          mat_data <- session$userData$rv_stressor_response$interaction_values[[this_var]]
          mtable <- mat_data$mat_sd[, 2:ncol(mat_data$mat_sd)]
          rnms <- unlist(mat_data$mat_sd[, 1])
          rnms <- as.numeric(rnms)
          mtable <- data.frame(mtable)
          rownames(mtable) <- rnms
          colnames_cust <- gsub("X", "", colnames(mtable))
          datatable(mtable,
          colnames = colnames_cust,
          )
      })

      output$interaction_matrix_ll <- DT::renderDataTable({
          print("Rendering matrix intraction table...")
          this_var <- get_interaction_var()
          req(this_var)
          mat_data <- session$userData$rv_stressor_response$interaction_values[[this_var]]
          mtable <- mat_data$mat_ll[, 2:ncol(mat_data$mat_ll)]
          rnms <- unlist(mat_data$mat_ll[, 1])
          rnms <- as.numeric(rnms)
          mtable <- data.frame(mtable)
          rownames(mtable) <- rnms
          colnames_cust <- gsub("X", "", colnames(mtable))
          datatable(mtable,
          colnames = colnames_cust,
          )
      })

      output$interaction_matrix_ul <- DT::renderDataTable({
          print("Rendering matrix intraction table...")
          this_var <- get_interaction_var()
          req(this_var)
          mat_data <- session$userData$rv_stressor_response$interaction_values[[this_var]]
          mtable <- mat_data$mat_ul[, 2:ncol(mat_data$mat_ul)]
          rnms <- unlist(mat_data$mat_ul[, 1])
          rnms <- as.numeric(rnms)
          mtable <- data.frame(mtable)
          rownames(mtable) <- rnms
          colnames_cust <- gsub("X", "", colnames(mtable))
          datatable(mtable,
          colnames = colnames_cust,
          )
      })

      # Interaction matrix text
      output$text_interaction_matrix <- renderText({
          this_var <- get_interaction_var()
          req(this_var)
          mat_data <- session$userData$rv_stressor_response$interaction_values[[this_var]]
          mcol <- mat_data$Columns
          mrow <- mat_data$Rows
          paste0("Interpolation matrix columns are stressor ", mcol, " and rows are stressor ", mrow, ".")
      })



      # Finally return the module stressor ID
      return(module_stressor_id)
    }
  )
}
