# model_matrix_overview.R
module_matrix_life_cycle_params_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$br(),

    shinydashboard::box(
      width = 12,
      title = "Review Checklist",
      collapsible = TRUE,
      collapsed = FALSE,

      tags$div(
        style = "background-color: #f0f7fb; border-left: 4px solid #3c8dbc; padding: 12px; margin-bottom: 15px;",
        tags$strong("Before running projections, verify the following:"),
        tags$ul(
          style = "margin-top: 8px; margin-bottom: 0;",
          tags$li(
            tags$strong("Lambda (\u03bb):"),
            " The dominant eigenvalue of the projection matrix should be close to 1.0 for a population near equilibrium.",
            " Values substantially below 1.0 suggest survival or fecundity parameters may be too low."
          ),
          tags$li(
            tags$strong("Generation time:"),
            " Should approximately match published estimates for your species.",
            " Large deviations may indicate issues with maturity schedule or stage durations."
          ),
          tags$li(
            tags$strong("Stable-stage distribution:"),
            " The proportion of individuals in each stage should align with expectations from the species' life history.",
            " For example, early life stages should typically contain more individuals than adult stages."
          ),
          tags$li(
            tags$strong("Density dependence:"),
            " Confirm that DD flags and K values are configured for the intended life stages.",
            " Stages without DD constraints will grow without bound."
          )
        ),
        tags$p(
          style = "margin-top: 10px; margin-bottom: 0; font-size: 12px; color: #666;",
          "These diagnostics are available in the ",
          tags$strong("Matrix Elements"),
          " tab after uploading your life cycle profile. See ",
          tags$a(
            href = "https://mattjbayly.github.io/CEMPRA_documentation/07_life_cycle_model.html#tips-on-building-a-new-life-cycle-profile-from-scratch",
            target = "_blank",
            "Chapter 7: Tips on Building a New Life Cycle Profile"
          ),
          " for guidance."
        )
      )
    ),

    # create a neat box for the inputs box
    shinydashboard::box(
      width = 12,
      fluidRow(
        column(6, downloadButton(ns("download_csv_lc"), "Download Life Cycle Parameters as a csv file")),

        column(6, downloadButton(ns("download_csv"), "Download Habitat Capacities as a csv file"))
      ),
    ),

    shinydashboard::box(
      width = 12,
      fluidRow(
        column(
          width = 12,
          tags$h4("Life Cycle Parameters File:"),
          tableOutput(ns("rv_preview"))
        )
      )

    ),



  )
}


module_matrix_life_cycle_params_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$rv_preview <- renderTable({
      session$userData$rv_life_stages$dat
    })
    
    
    output$download_csv <- downloadHandler(
      filename = function() { paste("Habitat_Capacities.csv", sep="") },
      content = function(file) { write.csv(session$userData$rv_hab_densities$dat, file, row.names=FALSE) }
    )
    
    output$download_csv_lc <- downloadHandler(
      filename = function() { paste("Life_Cycles_Profile.csv", sep="") },
      content = function(file) { write.csv(session$userData$rv_life_stages$dat, file, row.names=FALSE) }
    )
    
    
    
  })
}