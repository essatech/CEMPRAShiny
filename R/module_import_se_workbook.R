#' module_import_se_workbook_ui
module_import_se_workbook_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fileInput(
      ns("up_se_workbook"),
      label = "Socio-economic Input Workbook (xlsx)",
      multiple = FALSE,
      accept = c(".xlsx")
    ),
    div(style = "color: #ffffff; background: #ff000059; border-radius: 5px; margin: 5px;",
        textOutput(ns(
          "upload_error_msg_se_workbook"
        )))
  )
}





#' module_import_se_workbook_server
module_import_se_workbook_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 
                 print("Calling module_import_se_workbook_server...")
                 
                 #--------------------------------------
                 # Upload Socio-Economic Input Workbook Data
                 observe({
                   
                   # Require the file
                   req(input$up_se_workbook)
                   
                   upload_ok <- FALSE
                   
                   # Run import function in a try catch
                   # to avoid app crashing on upload errors
                   tryCatch({
                     in_file <- input$up_se_workbook$datapath
                     
                     if (is.null(in_file)) {
                       return(NULL)
                     }
                     
                     print("Loading SE inputs...")
                     
                     # Import SE workbook
                     socioeconomic_inputs <-
                       CEMPRA::SocioEconomicWorkbook(filename = in_file)
                     
                     # Perform final checks on the imported data
                     
                     # Check that location IDs match
                     loc_ids <-
                       socioeconomic_inputs$`Location Implementation`$`Location ID`
                     loc_ids <- unique(loc_ids)
                     
                     loc_id_check <-
                       isolate({
                         session$userData$rv_stressor_magnitude$sm_dat[, 1]
                       })
                     loc_id_check <-
                       unique(as.data.frame(loc_id_check)[, 1])
                     bad_locations <- setdiff(loc_ids, loc_id_check)
                     
                     if (length(bad_locations) > 0) {
                       socioeconomic_inputs$import_pass <- FALSE
                       socioeconomic_inputs$error_state <-
                         paste0(
                           "Location IDs in the socio-economic workbook do not match the stressor magnitude file. The following location IDs are not found in the stressor magnitude file: ",
                           paste(bad_locations, collapse = ", ")
                         )
                     }
                     
                     # Check that stressors match
                     stress_ids <-
                       socioeconomic_inputs$`Stressor Reduction`$`Affected Stressor`
                     stress_ids <- unique(stress_ids)
                     
                     stress_check <-
                       isolate({
                         session$userData$rv_stressor_response$stressor_names
                       })
                     
                     bad_stressors <-
                       setdiff(stress_ids, stress_check)
                     
                     if (length(bad_stressors) > 0) {
                       socioeconomic_inputs$import_pass <- FALSE
                       socioeconomic_inputs$error_state <-
                         paste0(
                           "Stressor names in the socio-economic workbook do not match the stressor response file. The following stressor names are not found in the stressor response file: ",
                           paste(bad_stressors, collapse = ", ")
                         )
                     }
                     
                     
                     # If file is valid
                     if (socioeconomic_inputs$import_pass) {
                       # Update the socio-economic reactive values
                       session$userData$rv_se_inputs$socioeconomic_inputs <-
                         socioeconomic_inputs
                       
                       upload_ok <- TRUE
                       
                       output$upload_error_msg_se_workbook <-
                         renderText({
                           ""
                         })
                       
                     } else {
                       # If file upload is invalid. Set SE inputs to NULL
                       session$userData$rv_se_inputs$socioeconomic_inputs <-
                         NULL
                       
                       upload_ok <- FALSE
                       
                       output$upload_error_msg_se_workbook <-
                         renderText({
                           socioeconomic_inputs$error_state
                         })
                     }
                   },
                   error = function(e) {
                     # return a safeError if a parsing error occurs
                     print("Upload error...")
                     
                     output$upload_error_msg_sm <- renderText({
                       "Upload Error: Socio-Economic Input Workbook (xlsx) did not import correctly. Check data format and column names."
                     })
                   })
                 }) # end of socio-economic data upload
                 
                 
                 
               })
}
