#' Display the life cycle diagram in the matrix population model
#'
module_matrix_life_cycle_diagram_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinydashboard::box(
      width = 12,
      tags$div(class = "lam_bb",
               actionButton(ns("open_lcd_modal"), "Life Cycle Diagram"))
      
    ))
  
}


#' Display the life cycle diagram in the matrix population model - Server Function
module_matrix_life_cycle_diagram_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #-------------------------------------------------------
    # Render DiagrammeR grViz output
    #-------------------------------------------------------
    
    # Render DiagrammeR grViz output
    output$diagramplot <- renderGrViz({
      
      print("Generating the life cycle diagram...")
      
      # Projection Matrix
      matA <- session$userData$rv_eigen_analysis$dat$pop_mod_mat$projection_matrix
      
      # Stage names
      stages <- session$userData$rv_eigen_analysis$dat$pop_mod_mat$life_histories$stage_names
      
      # anadromous (TRUE/FALSE)
      anadromous <- session$userData$rv_eigen_analysis$dat$pop_mod_mat$life_histories$anadromous
      
      # Stage names may be missing from non-anadromous inputs
      # Just use s sequence if that is the case
      
      if(length(stages) == 0 & !(anadromous)) {
        Nstage <- dim(matA)[1]
        Nstage <- as.numeric(Nstage)
        stages <- paste0("Stage_", seq(1:Nstage))
      }
      
      # Return empty plot if null with error message
      tryCatch({
        utility_plot_life_cycle_cempra(
          matA = matA,
          stages = stages,
          anadromous = anadromous,
          title = "Life Cycle Diagram"
        )
      }, error = function(e) {
        # Return a simple DiagrammeR graph with an error message
        grViz("
          digraph error {
            node [shape = plaintext]
            Error [label = 'Error with input data']
          }
          ")
      })
      
      
      
    })
    
    
    
    
    
    #-------------------------------------------------------
    # pop-up modal content
    #-------------------------------------------------------
    
    observeEvent(input$open_lcd_modal, {
      print("clicked open_lcd_modal...")
      
      showModal(modalDialog(
        title = "Life Cycle Diagram",
        size = "l",
        easyClose = TRUE,
        tagList(
          tags$p(
            "The following figure represents a dynamically generated lifecycle diagram based on the number of stages, transition rates, and data inputs. The structure of the diagram will differ depending on whether or not the anadromous life cycle profile check box is selected. For non-anadromous lifecycle profiles, stage transitions will be shown between each unique stage. For anadromous lifecycle profiles, blue circles indicate (Pb) pre-birth non-reproductive stages and red circles/text indicate reproductive (B) breeder stages classes. Please carefully review the diagram to ensure accuracy and consistency with expectations. Remember that egg (SE) and stage (S0) survivorship probabilities are rolled up into the fecundity term for a pre-birth-pulse census structure."
          ),
          tags$p(
            "Make sure there are no error messages on the previous page, otherwise the diagram will not render correctly."
          ),
          grVizOutput(ns("diagramplot"), height = "800px")
          
        )
      ))
    })
    
  })
}
