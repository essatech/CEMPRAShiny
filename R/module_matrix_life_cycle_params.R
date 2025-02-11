# model_matrix_overview.R
module_matrix_life_cycle_params_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$br(),
    
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