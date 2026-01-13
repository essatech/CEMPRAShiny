#' Matrix Model UI
#'
#' The UI portion of the matrix model
#'
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom shinydashboard box
#'
#' @param id The id for this module
#'
#' @return a tagList containing UI elements
#'
module_matrix_model_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinydashboard::box(
      width = 12,
      tags$h3("Population Model"),
      tags$div(
        tags$a(
          href = "https://mattjbayly.github.io/CEMPRA_documentation/07_life_cycle_model.html",
          "See guidance document help section: Life Cycle Model",
          target = "_blank"
        )
      ),

      tags$p(
        "Navigate through the tab panels below to adjust the survival, growth, reproduction, and density-dependent
        constraints on the life cycle model. Life cycle model vital rate files can be edited here, exported
        (on the All Inputs tab), and then re-uploaded using the file upload inputs.",
        class = "pm-ht",
        style = "margin-top: 10px;"
      ),

      # Static Workflow Stepper
      tags$div(
        class = "workflow-stepper",
        style = "margin-top: 20px; margin-bottom: 10px;",

        # Step 1 - Upload Life Cycles
        tags$div(
          class = "workflow-step",
          tags$div(class = "step-number-container",
            tags$div(class = "step-circle", tags$span("1"))
          ),
          tags$div(class = "step-content",
            tags$h5(class = "step-title", "Upload Life Cycles"),
            tags$p(class = "step-description", "Load vital rates CSV file")
          ),
          tags$div(class = "step-connector")
        ),

        # Step 2 - Adjust Scenario Parameters
        tags$div(
          class = "workflow-step",
          tags$div(class = "step-number-container",
            tags$div(class = "step-circle", tags$span("2"))
          ),
          tags$div(class = "step-content",
            tags$h5(class = "step-title", "Adjust Scenario Parameters"),
            tags$p(class = "step-description", "Configure survival, growth, reproduction")
          ),
          tags$div(class = "step-connector")
        ),

        # Step 3 - Run Model
        tags$div(
          class = "workflow-step",
          tags$div(class = "step-number-container",
            tags$div(class = "step-circle", tags$span("3"))
          ),
          tags$div(class = "step-content",
            tags$h5(class = "step-title", "Run Model"),
            tags$p(class = "step-description", "Execute population simulations")
          ),
          tags$div(class = "step-connector")
        ),

        # Step 4 - Compare Scenarios
        tags$div(
          class = "workflow-step last",
          tags$div(class = "step-number-container",
            tags$div(class = "step-circle", tags$span("4"))
          ),
          tags$div(class = "step-content",
            tags$h5(class = "step-title", "Compare Scenarios"),
            tags$p(class = "step-description", "Review and compare results")
          )
        )
      )
    ),

    fluidRow(
            column(
              width = 8,
              shinydashboard::box(
                width = 12,
                tags$h4("Population Model Input Files"),
                module_matrix_model_inputs_ui(ns("mm_inputs"))
              ),
            ),
            column(
              width = 4,
              shinydashboard::box(
                width = 12,
                tags$div(tags$h4("Matrix Elements"), style = "text-aling: center;"),
                module_matrix_life_cycle_diagram_ui(ns("matrix_life_cycle_diagram")),
                module_matrix_model_elements_ui(ns("mm_elements"))
              )
            )
          ))
}


#' Matrix Model Server
#'
#' @param none
#'
#' @return None
#'
module_matrix_model_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 
                 print("Matrix model main...")
                 
                 # Call sub module for HUC results
                 module_matrix_model_inputs_server("mm_inputs")
                 print("Load module_matrix_model_elements_server...")
                 module_matrix_model_elements_server("mm_elements")
                 print("End load matrix inputs...")
                 module_matrix_life_cycle_diagram_server("matrix_life_cycle_diagram")
                 
                 
                 #-------------------------------------------------------
                 # DISABLE AND ENABLE
                 #-------------------------------------------------------
                 
               })
}