#' Socio-Economic UI
module_socio_economic_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(shinydashboard::box(width = 12,
                              tags$h3("Socio-economic Evaluation"),
                              tags$div(tags$a(href = "https://mattjbayly.github.io/CEMPRA_documentation/07_life_cycle_model.html", "See guidance document help section: Socio-economic Evaluation", target =
                                                "_blank"))
  ),
  fluidRow(
    column(
      width = 8,
      shinydashboard::box(
        width = 12,
        tags$h3("Socio-economic Evaluation of Restoration Actions")
      ),
    ),
    column(
      width = 4,
      "content here..."
    )
  ))
}


#' Server Function
module_socio_economic_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 
                 print("module_socio_economic_server...")
                 
                 #-------------------------------------------------------
                 # DISABLE AND ENABLE
                 #-------------------------------------------------------
                 
               })
}