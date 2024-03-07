#' Table Module UI
#'
#' The UI portion of the module for displaying the main datatable
#'
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom DT DTOutput
#' @importFrom shinycssloaders withSpinner
#'
#' @param id The id for this module
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements

module_se_table_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        title = "Main Table Title Here",
        DTOutput(ns('main_table')),
        tags$br(),
        tags$br()
      )
    ),
    tags$script(src = "/js/main_table_module.js"),
    tags$script(paste0("main_table_module_js('", ns(''), "')"))
  )
}

#' Main Table Module Server
#'
#' The Server portion of the module for displaying the main datatable
#'
#' @importFrom shiny reactive reactiveVal observeEvent req callModule eventReactive
#' @importFrom DT renderDT datatable replaceData dataTableProxy
#' @importFrom dplyr tbl collect mutate arrange select filter pull
#' @importFrom purrr map_chr
#' @importFrom tibble tibble
#'
#' @param None
#'
#' @return None

module_se_table_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 
                 ns <- session$ns
                 
                 # Read in "mtcars" table from the database
                 r_table <- reactive({
                   
                   out <- session$userData$rv_se_inputs$socioeconomic_inputs
                   
                   if(!is.null(out)) {
                     # Get the the Location Implementation
                     se_tab <- SocioEconomicRun(socioeconomic_inputs = out)
                     mtab <- se_tab$stressor_reductions
                     mtab <- mtab[, c('id', 'loc_name', 'action',
                                      'n_units', 'measurement', 'linked_stressor')]
                     mtab <- mtab[order(mtab$action, mtab$loc_name), ]
                     mtab$id_ <- seq_len(nrow(mtab))
                     out <- mtab
                   }
                   
                   out
                   
                 })
                 
                 
                 main_table_prep <- reactiveVal(NULL)
                 
                 observeEvent(r_table(), {
                   out <- r_table()
                   
                   ids <- out$id_
                   
                   # Add on the action buttons html as new columns for each row
                   actions <- purrr::map_chr(ids, function(id_) {
                     paste0(
                       '<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
          <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Edit" id = ',
                       id_,
                       ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
        </div>'
                     )
                   })
                   
                   # Remove the `id_` and `is_deleted` columns. We don't want to show this column to the user
                   out <- out %>%
                     select(-id_)
                   
                   # Set the Action Buttons row to the first column of the `mtcars` table
                   out <- cbind(tibble(" " = actions),
                                out)
                   
                   if (is.null(main_table_prep())) {
                     # loading data into the table for the first time, so we render the entire table
                     # rather than using a DT proxy
                     main_table_prep(out)
                     
                   } else {
                     # table has already rendered, so use DT proxy to update the data in the
                     # table without rerendering the entire table
                     replaceData(main_table_proxy,
                                 out,
                                 resetPaging = FALSE,
                                 rownames = FALSE)
                     
                   }
                 })
                 
                 
                 # Render the full data table for the first time
                 # if not already created.
                 # Alternative is replaceData(main_table_proxy)
                 
                 output$main_table <- renderDT({
                   req(main_table_prep())
                   
                   out <- main_table_prep()
                   
                   datatable(
                     out,
                     rownames = FALSE,
                     colnames = c(
                       'Loc. ID',
                       'Loc. Name',
                       'Action',
                       'Amount',
                       'Units',
                       'Stressor Linkage'
                     ),
                     selection = "none",
                     class = "compact stripe row-border nowrap",
                     # Escape the HTML in all except 1st column (which has the buttons)
                     escape = -1,
                     extensions = c("Buttons"),
                     options = list(
                       scrollX = TRUE,
                       dom = 'Bftip',
                       buttons = list(
                         list(
                           extend = "excel",
                           text = "Download",
                           title = paste0("mtcars-", Sys.Date()),
                           exportOptions = list(columns = 1:(length(out) - 1))
                         )
                       ),
                       columnDefs = list(list(
                         targets = 0, orderable = FALSE
                       )),
                       drawCallback = JS(
                         "function(settings) {
          // removes any lingering tooltips
          $('.tooltip').remove()
        }"
                       ),
                       pageLength = 50
                     )
                   )
                   
                 })
                 
                 
                 
                 # Table has already been created.
                 # Just need to reload table content with proxy.
                 
                 main_table_proxy <-
                   DT::dataTableProxy('main_table')
                 
                 
                 # Source additional modules for add and edit.
                 # Add and edit use the same source module.
                 
                 # Add new entry
                 
                 
                 # Trigger the row to edit
                 row_to_edit <-
                   eventReactive(input$main_id_to_edit, {
                     r_table() %>%
                       filter(id_ == input$main_id_to_edit)
                   })
                 
                 # Edit existing entry
                 
                 module_se_table_edit_server(
                   "edit_row",
                   modal_title = "Edit Record",
                   row_to_edit = row_to_edit,
                   modal_trigger = reactive({
                     input$main_id_to_edit
                   })
                 )
                 
                 
                 
               })
}
