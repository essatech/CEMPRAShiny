# module_dd_cap.R

# UI Function
module_matrix_dd_cap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h4("Define Density Dependence Location Capacities"),
    fluidRow(
      column(
        width = 12,
        tags$p("This page displays the habitat density capacities stored in 
               session$userData$rv_hab_densities$dat. You can edit any cell except the first two columns 
               (which are locked). Edits will update the underlying data automatically.")
      )
    ),
    fluidRow(
      column(12, DT::dataTableOutput(ns("dd_cap_table")))
    ),
    fluidRow(
      column(12, downloadButton(ns("download_csv"), "Download Habitat Capacities as a csv file"))
    ),
    tags$br()
  )
}

# Server Function
module_matrix_dd_cap_server <- function(id, anadromous, Nstage) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Ensure session$userData$rv_hab_densities exists.
    if (is.null(session$userData$rv_hab_densities)) {
      session$userData$rv_hab_densities <- reactiveValues(
        # Example structure; adjust columns as needed.
        dat = data.frame(
          Region = character(0),    # locked column 1
          Location = character(0),  # locked column 2
          Capacity1 = numeric(0),   # editable column 3
          Capacity2 = numeric(0),   # editable column 4
          stringsAsFactors = FALSE
        )
      )
    }
    
    # Render the data table.
    output$dd_cap_table <- DT::renderDataTable({
      df <- session$userData$rv_hab_densities$dat
      
      # If no rows exist, show an empty table.
      if(nrow(df) == 0) {
        empty_df <- data.frame("No Data Available" = "")
        return(DT::datatable(empty_df,
                             options = list(dom = 't'),
                             editable = FALSE))
      }
      
      DT::datatable(
        df,
        selection = "none",
        editable = list(
          target = "cell",
          disable = list(columns = c(0, 1))  # disable editing of first two columns (0-indexed)
        ),
        rownames = FALSE,
        options = list(
          order = list(list(0, 'asc')),                         # Order by first column (0-indexed) ascending
          pageLength = 50,                                      # Default number of rows per page
          lengthMenu = list(c(10, 50, 100, 1000), c("10", "50", "100", "1,000"))
        )
      )
    }, server = FALSE)
    
    # Update reactive data when a cell is edited.
    observeEvent(input$dd_cap_table_cell_edit, {
      info <- input$dd_cap_table_cell_edit
      
      # Based on your observation, the row index returned is already 1-indexed.
      row <- info$row          # do NOT add +1 here
      col <- info$col + 1      # convert the 0-indexed column to 1-indexed
      value <- info$value
      
      df <- session$userData$rv_hab_densities$dat
      
      # Coerce the value to the column's data type.
      df[row, col] <- DT::coerceValue(value, df[[col]])
      
      # Update the reactive data frame.
      session$userData$rv_hab_densities$dat <- df
    })
    
    
    output$download_csv <- downloadHandler(
      filename = function() { paste("dd_cap_table-", Sys.Date(), ".csv", sep="") },
      content = function(file) { write.csv(session$userData$rv_hab_densities$dat, file, row.names=FALSE) }
    )
    
    
  })
}
