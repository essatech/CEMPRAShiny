#' module_import_ui
#'
#' The UI portion of the import export model
#'
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom shinydashboard box
#'
#' @param id The id for this module
#'
#' @return a tagList containing UI elements
#'
module_import_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinydashboard::box(
      width = 10,
      tags$h3("Upload Data"),
      fluidRow(
        column(
          width = 12,
          tags$p(
            "Click the ‘browse…’ button to upload files. Note that uploaded a new data source will overwrite all local changes and you will need to re-run your results. Stressor response files, stressor magnitude files and watershed polygons must all be validated externally, or you will experience errors. Please see notes below to ensure proper data format. See sample data at the link below to help match "
          ),
          tags$a(
            href = "https://mattjbayly.github.io/CEMPRA_documentation/05_data_inputs.html",
            "See Guidance Document Help Section",
            target =
              "_blank"
          ),
          tags$br(),
          tags$a(
            href = "https://mattjbayly.github.io/CEMPRA_documentation/09_case_study_applications.html",
            "Download Example Datasets Here",
            target = "_blank"
          ),
          tags$p(""),
        )
      ),
      fluidRow(
        column(
          width = 5,
          shinydashboard::box(
            width = 12,
            accordion(
              id = "accordion1",
              accordionItem(title = "Stressor Response Workbook",
                            collapsed = TRUE,
                            tagList(tags$p("...."), ))
            ),
            fileInput(
              ns("up_sr_wb_dat"),
              label = "Stressor Response Workbook (xlsx)",
              multiple = FALSE,
              accept = c(".xlsx")
            ),
            div(style = "color: #ffffff; background: #ff000059; border-radius: 5px; margin: 5px;",
                textOutput(ns(
                  "upload_error_msg_sr"
                )))
          )
        ),
        column(
          width = 5,
          shinydashboard::box(
            width = 12,
            accordion(
              id = "accordion2",
              accordionItem(title = "Stressor Magnitude Workbook",
                            collapsed = TRUE,
                            tagList(tags$p("...."), ))
            ),
            fileInput(
              ns("up_sm_wb_dat"),
              label = "Stressor Magnitude Workbook (xlsx)",
              multiple = FALSE,
              accept = c(".xlsx")
            ),
            div(style = "color: #ffffff; background: #ff000059; border-radius: 5px; margin: 5px;",
                textOutput(ns(
                  "upload_error_msg_sm"
                )))
          )
        )
      ),
      fluidRow(column(
        width = 5,
        shinydashboard::box(
          width = 12,
          accordion(
            id = "accordion3",
            accordionItem(title = "Watershed GIS Polygons or Lines (Spatial) [.gpkg or .shp]",
                          collapsed = TRUE,
                          tagList(tags$p("...."), ))
          ),
          fileInput(
            ns("up_sheds"),
            label = "polygons as a single .gpkg file or .shp file with associated files (.cpg, .dbf, .prj, .shx)",
            multiple = TRUE,
            accept = c(".gpkg", ".shp", ".cpg", ".dbf", ".prj", ".shx")
          ),
          div(style = "color: #ffffff; background: #ff000059; border-radius: 5px; margin: 5px;",
              textOutput(ns(
                "upload_error_msg_sheds"
              )))
        )
      )),
      
      fluidRow(column(
        width = 12,
        tags$h3("Optional Additional Inputs")
      )),
      
      
      fluidRow(
        column(
          width = 5,
          shinydashboard::box(
            width = 12,
            accordion(
              id = "accordion4",
              accordionItem(title = "Population Model Vital Rates",
                            collapsed = TRUE,
                            tagList(tags$p("...."), ))
            ),
            div(
              "Upload life cycle profiles with for target species within the Population Model tab."
            ),
            div(style = "color: #ffffff; background: #ff000059; border-radius: 5px; margin: 5px;",
                textOutput(ns(
                  "upload_error_msg_vital"
                )))
          )
        ),
        column(
          width = 5,
          shinydashboard::box(
          width = 12,
          accordion(
            id = "accordion75",
            accordionItem(title = "Socio-economic Input Workbook",
                          collapsed = TRUE,
                          tagList(
                            tags$p(
                              "The socio-economic component ultimately attempts to provide a high-level cost-benefit analysis of restoration alternatives, and is designed to facilitate decision-making by quantifying the economic implications of competing restoration strategies"
                            )
                          ))
          ),
          module_import_se_workbook_ui(ns("module_import_se_workbook_main"))
        )
          
          
          
        )
        
        
        
      )
      
      
      
      
      
      
      
      
    )
  )
}





#' Import server function
#'
#' @param none
#'
#' @return None
#'
module_import_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 
                 print("Calling module_import_server")
                
                 # load module for se import
                 module_import_se_workbook_server("module_import_se_workbook_main")
                 
                 #--------------------------------------
                 # Stressor Resposne Workbook Data Download
                 observe({
                   # Require the file
                   req(input$up_sr_wb_dat)
                   
                   print("SR Workbook Data...")
                   
                   upload_ok <- FALSE
                   
                   # Run import function in a try catch
                   # to avoid app crashing on upload errors
                   tryCatch({
                     in_file <- input$up_sr_wb_dat
                     
                     if (is.null(in_file)) {
                       return(NULL)
                     }
                     
                     
                     # Ensure there are no spaces in file name
                     # file.rename(in_file$datapath, paste(in_file$datapath, ".xlsx", sep=""))
                     # Extract the stressor response relationships
                     sr_wb_dat <-
                       CEMPRA::StressorResponseWorkbook(filename = input$up_sr_wb_dat$datapath)
                     
                     # Update the session$userData$rv_stressor_response reactive object
                     start_time <- Sys.time()
                     
                     # Designate the stressor response object as a reactive value
                     session$userData$rv_stressor_response$main_sheet <-
                       sr_wb_dat$main_sheet
                     session$userData$rv_stressor_response$stressor_names <-
                       sr_wb_dat$stressor_names
                     session$userData$rv_stressor_response$pretty_names <-
                       sr_wb_dat$pretty_names
                     session$userData$rv_stressor_response$sr_dat <-
                       sr_wb_dat$sr_dat
                     session$userData$rv_stressor_response$active_layer <-
                       sr_wb_dat$stressor_names[1]
                     session$userData$rv_stressor_response$active_values_raw <-
                       NULL
                     session$userData$rv_stressor_response$active_values_response <-
                       NULL
                     session$userData$rv_stressor_response$active_refresh <-
                       start_time
                     session$userData$rv_stressor_response$hover_values <-
                       FALSE
                     session$userData$rv_stressor_response$interaction_names <-
                       names(sr_wb_dat$MInt)
                     session$userData$rv_stressor_response$interaction_values <-
                       sr_wb_dat$MInt
                     
                     
                     # Trigger redraw to clear selection
                     session$userData$rv_redraw$redraw <- 0
                     # Selected HUCs - Create an empty vector to hold all HUC click ids
                     session$userData$rv_clickedIds$ids <- vector()
                     # Selected HUCs Cumulative System Capacity - store temporary CSC for selected HUCs
                     session$userData$rv_clickedIds_csc$csc <- NA
                     session$userData$rv_clickedIds_csc$var_csc <-
                       NA
                     
                     # Joe model results holder - assume multiple runs
                     session$userData$rv_joe_model_results$sims <-
                       list()
                     
                     # Joe model scenario name holder - assume multiple runs
                     session$userData$rv_joe_model_sim_names$scenario_names <-
                       list()
                     
                     # Place holder for Joe Model estimated run times
                     session$userData$rv_joe_model_run_time$run_time_seconds <-
                       list()
                     
                     # Clear out any population model runs
                     session$userData$rv_pop_data_huc_ts$dat <-
                       list()
                     session$userData$rv_pop_data_huc_ts$run_counter <-
                       1
                     session$userData$rv_pop_data_huc_ts$update_ts_plots <-
                       FALSE
                     session$userData$rv_show_pop_main_plot$open <-
                       FALSE
                     
                     print("Triggering rv_sandbox_stressors$dat flush at with sr data...")
                     session$userData$rv_sandbox_stressors$dat <- list()
                     
                     # Clear out sample plot data
                     # session$userData$rv_show_sample_plot$open <- FALSE
                     session$userData$rv_pop_sample_plot_data$dat <-
                       list()
                     session$userData$rv_pop_sample_plot_data$run_counter <-
                       1
                     
                     # Hide system capacity button
                     addClass(id = "main_map-var_id",
                              class = "hide-this",
                              asis = TRUE)
                     
                     
                     upload_ok <- TRUE
                     
                     output$upload_error_msg_sr <- renderText({
                       ""
                     })
                   },
                   error = function(e) {
                     # return a safeError if a parsing error occurs
                     print("Upload error...")
                     
                     output$upload_error_msg_sr <- renderText({
                       "Upload Error: Stressor Response Workbook (xlsx) did not import correctly. Check data format, worksheet names and column names."
                     })
                   })
                 }) # end of Stressor Response Workbook Data Download
                 
                 
                 
                 #--------------------------------------
                 # Stressor Magnitude Workbook Data
                 observe({
                   
                   # Require the file
                   req(input$up_sm_wb_dat)
                   
                   upload_ok <- FALSE
                   
                   # Run import function in a try catch
                   # to avoid app crashing on upload errors
                   tryCatch({
                     in_file <- input$up_sm_wb_dat
                     
                     if (is.null(in_file)) {
                       return(NULL)
                     }
                     
                     print("Loading SM workbook...")
                     
                     # Extract the stressor response relationships
                     sm_wb_dat <-
                       CEMPRA::StressorMagnitudeWorkbook(
                         filename = input$up_sm_wb_dat$datapath,
                         scenario_worksheet = 1
                       ) # natural_unc
                     
                     # First reset the stressor response workbook
                     start_time <- Sys.time()
                     
                     session$userData$rv_stressor_response$active_layer <-
                       session$userData$rv_stressor_response$stressor_names[1]
                     session$userData$rv_stressor_response$active_values_raw <-
                       NULL
                     session$userData$rv_stressor_response$active_values_response <-
                       NULL
                     session$userData$rv_stressor_response$active_refresh <-
                       start_time
                     session$userData$rv_stressor_response$hover_values <-
                       FALSE
                     
                     # Update the stressor magnitude data
                     session$userData$rv_stressor_magnitude$sm_dat <-
                       sm_wb_dat
                     
                     # Trigger redraw to clear selection
                     session$userData$rv_redraw$redraw <- 0
                     
                     # Selected HUCs - Create an empty vector to hold all HUC click ids
                     session$userData$rv_clickedIds$ids <- vector()
                     
                     # Selected HUCs Cumulative System Capacity - store temporary CSC for selected HUCs
                     session$userData$rv_clickedIds_csc$csc <- NA
                     session$userData$rv_clickedIds_csc$var_csc <-
                       NA
                     
                     # Joe model results holder - assume multiple runs
                     session$userData$rv_joe_model_results$sims <-
                       list()
                     
                     # Joe model scenario name holder - assume multiple runs
                     session$userData$rv_joe_model_sim_names$scenario_names <-
                       list()
                     
                     # Place holder for Joe Model estimated run times
                     session$userData$rv_joe_model_run_time$run_time_seconds <-
                       list()
                     
                     # Clear out any population model runs
                     session$userData$rv_pop_data_huc_ts$dat <-
                       list()
                     session$userData$rv_pop_data_huc_ts$run_counter <-
                       1
                     session$userData$rv_pop_data_huc_ts$update_ts_plots <-
                       FALSE
                     session$userData$rv_show_pop_main_plot$open <-
                       FALSE
                     
                     print("Triggering rv_sandbox_stressors$dat flush at sm data...")
                     session$userData$rv_sandbox_stressors$dat <- list()
                     
                     # Clear out sample plot data
                     # session$userData$rv_show_sample_plot$open <- FALSE
                     session$userData$rv_pop_sample_plot_data$dat <-
                       list()
                     session$userData$rv_pop_sample_plot_data$run_counter <-
                       1
                     
                     # Hide system capacity button
                     addClass(id = "main_map-var_id",
                              class = "hide-this",
                              asis = TRUE)
                     
                     upload_ok <- TRUE
                     
                     output$upload_error_msg_sr <- renderText({
                       ""
                     })
                   },
                   error = function(e) {
                     # return a safeError if a parsing error occurs
                     print("Upload error...")
                     
                     output$upload_error_msg_sm <- renderText({
                       "Upload Error: Stressor Magnitude Workbook (xlsx) did not import correctly. Check data format and column names."
                     })
                   })
                 }) # end of Stressor Magnitude Workbook Data Upload
                 
                 
                 
                 #--------------------------------------
                 # Watershed Polygon Upload
                 observe({
                   
                   # Require the file
                   req(input$up_sheds)
                   
                   upload_ok <- FALSE
                   
                   print("Loading spatial data...")
                   
                   # Run import function in a try catch
                   # to avoid app crashing on upload errors
                   
                   tryCatch({
                     in_file <- input$up_sheds
                     
                     if (is.null(in_file)) {
                       return(NULL)
                     }
                     
                     if (nrow(in_file) > 1) {
                       # Shapefile load
                       infiles <-
                         in_file$datapath # get the location of files
                       dir <-
                         unique(dirname(infiles)) # get the directory
                       outfiles <-
                         file.path(dir, in_file$name) # create new path name
                       name <-
                         strsplit(in_file$name[1], "\\.")[[1]][1] # strip name
                       purrr::walk2(infiles, outfiles, ~ file.rename(.x, .y)) # rename files
                       hmdl <-
                         sf::read_sf(file.path(dir, paste0(name, ".shp"))) # read-in shapefile
                     } else {
                       # Geopackage load
                       # Load in the default watersheds geojson layer
                       hmdl <-
                         sf::st_read(input$up_sheds$datapath[1])
                     }
                     
                     print("File loading....")
                     
                     # Change rendering order
                     # we want small polygons in front and large polygons behind
                     print("Review order...")
                     hmdl$area <- sf::st_area(hmdl)
                     hmdl <- arrange(hmdl, desc(area))
                     
                     # Fix col names - if needed
                     cnames <- colnames(hmdl)
                     
                     # Force the addition of a HUC_ID column
                     if (!("HUC_ID" %in% cnames)) {
                       
                       # if("id" %in% tolower(colnames(cnames)) {
                       #
                       # } else {
                       #
                       # }
                       
                       use_id <-
                         which(grepl("id", tolower(cnames)))[1]
                       hmdl$HUC_ID <- hmdl[[use_id]]
                       
                     }
                     
                     if (!("NAME" %in% cnames)) {
                       print("NAME not in col names...")
                       use_name <-
                         which(grepl("name", tolower(cnames)))[1]
                       if(is.na(use_name)) {
                         hmdl$NAME <- "NO NAME"
                       } else {
                         hmdl$NAME <- hmdl[[use_name]] 
                       }
                     }
                     
                     hmdl$HUC_ID <- as.numeric(hmdl$HUC_ID)
                     
                     hmdl$uid <-
                       paste0(hmdl$HUC_ID, "|", hmdl$NAME)
                     
                     # Ensure the projection is 4326
                     if (st_crs(hmdl)$epsg != 4326) {
                       hmdl <- sf::st_transform(hmdl, 4326)
                     }
                     
                     # Drop any polygons with no ID or NA values for HUC_ID
                     # hmdl <- hmdl[!is.na(hmdl$HUC_ID), ]
                     
                     # Check if polygon IDs are matched in
                     # stressor magnitude file.
                     check_ids1 <- unique(hmdl$HUC_ID)
                     check_ids2 <- isolate({ session$userData$rv_stressor_magnitude$sm_dat$HUC_ID })
                     check_ids2 <- unique(check_ids2)
                     check_diff <- setdiff(check_ids1, check_ids2)
                     if(length(check_diff) > 0) {
                       check_diff <- paste(check_diff, collapse = ", ")
                       error_msg <- paste0("IDs missing from Stressor Magnitude: ", check_diff, "...") 
                       error_msg <- substr(error_msg, 1, 100)
                     } else {
                       error_msg <- ""
                     }
                     
                     
                     # Save default HUC to reactive values
                     session$userData$rv_HUC_geom$huc_geom <- hmdl
                     session$userData$rv_HUC_geom$leg_col <- NA
                     session$userData$rv_HUC_geom$leg_lab <- NA
                     session$userData$rv_HUC_geom$color_df <- NA
                     
                     # Initial load
                     bbox <- st_bbox(hmdl)
                     
                     # Determine geometry type and set
                     # Determine if running with lines or polygons
                     print("Determine if running with lines or polygons...")
                     geom_type <-
                       st_geometry_type(hmdl)
                     
                     print("Loading leaflet first time...")
                     
                     if (unique(geom_type)[1] %in% c("LINESTRING", "MULTILINESTRING")) {
                       session$userData$geom_type <- "lines"
                     } else {
                       session$userData$geom_type <- "polygons"
                     }
                     
                     
                     session$userData$rv_HUC_layer_load$data <- hmdl
                     session$userData$rv_HUC_layer_load$xmin <-
                       bbox$xmin
                     session$userData$rv_HUC_layer_load$ymin <-
                       bbox$ymin
                     session$userData$rv_HUC_layer_load$xmax <-
                       bbox$xmax
                     session$userData$rv_HUC_layer_load$ymax <-
                       bbox$ymax
                     session$userData$rv_HUC_layer_load$reload_map <-
                       TRUE
                     session$userData$rv_HUC_layer_load$add_polygons <-
                       FALSE
                     
                     # First reset the stressor response workbook
                     start_time <- Sys.time()
                     
                     session$userData$rv_stressor_response$active_layer <-
                       isolate(session$userData$rv_stressor_response$stressor_names[1])
                     session$userData$rv_stressor_response$active_values_raw <-
                       NULL
                     session$userData$rv_stressor_response$active_values_response <-
                       NULL
                     session$userData$rv_stressor_response$active_refresh <-
                       start_time
                     session$userData$rv_stressor_response$hover_values <-
                       FALSE
                     
                     
                     # Update the stressor magnitude data
                     session$userData$rv_stressor_magnitude$sm_dat <-
                       isolate(session$userData$rv_stressor_magnitude$sm_dat)
                     
                     # Trigger redraw to clear selection
                     session$userData$rv_redraw$redraw <- 0
                     
                     # Selected HUCs - Create an empty vector to hold all HUC click ids
                     session$userData$rv_clickedIds$ids <- vector()
                     
                     # Selected HUCs Cumulative System Capacity - store temporary CSC for selected HUCs
                     session$userData$rv_clickedIds_csc$csc <- NA
                     session$userData$rv_clickedIds_csc$var_csc <-
                       NA
                     
                     # Joe model results holder - assume multiple runs
                     session$userData$rv_joe_model_results$sims <-
                       list()
                     
                     # Joe model scenario name holder - assume multiple runs
                     session$userData$rv_joe_model_sim_names$scenario_names <-
                       list()
                     
                     # Place holder for Joe Model estimated run times
                     session$userData$rv_joe_model_run_time$run_time_seconds <-
                       list()
                     
                     # Clear out any population model runs
                     session$userData$rv_pop_data_huc_ts$dat <-
                       list()
                     session$userData$rv_pop_data_huc_ts$run_counter <-
                       1
                     session$userData$rv_pop_data_huc_ts$update_ts_plots <-
                       FALSE
                     session$userData$rv_show_pop_main_plot$open <-
                       FALSE
                     
                     print("Triggering rv_sandbox_stressors$dat flush at with spatial data...")
                     session$userData$rv_sandbox_stressors$dat <- list()
                     
                     # Clear out sample plot data
                     # session$userData$rv_show_sample_plot$open <- FALSE
                     session$userData$rv_pop_sample_plot_data$dat <-
                       list()
                     session$userData$rv_pop_sample_plot_data$run_counter <-
                       1
                     
                     # Hide system capacity button
                     addClass(id = "main_map-var_id",
                              class = "hide-this",
                              asis = TRUE)
                     
                     upload_ok <- TRUE
                     
                     output$upload_error_msg_sr <- renderText({
                       error_msg
                     })
                   },
                   error = function(e) {
                     # return a safeError if a parsing error occurs
                     print("Upload error...")
                     
                     output$upload_error_msg_sheds <-
                       renderText({
                         "Upload Error: HUC watersheds did not import correctly. Ensure data is uploaded as a gpkg file with latitude/longitude (EPSG:4326) projects and field attribute (column) names HUC_ID and NAME with the HUC_ID and values corresponding to those in the stressor magnitude file."
                       })
                   })
                 }) # end of Stressor Magnitude Workbook Data Download
                 
                 
                 
                 
                 #--------------------------------------
                 # # Upload vital rates
                 # observe({
                 #   # Require the file
                 #   req(input$up_vital)
                 #
                 #   upload_ok <- FALSE
                 #
                 #   # Run import function in a try catch
                 #   # to avoid app crashing on upload errors
                 #
                 #   tryCatch(
                 #     {
                 #       in_file <- input$up_vital
                 #
                 #       if (is.null(in_file)) {
                 #         return(NULL)
                 #       }
                 #
                 #       # Load in the default watersheds geojson layer
                 #       life_stages <-
                 #         read.csv(input$up_vital$datapath)
                 #
                 #       print("running JavaScript... updateAllInputs")
                 #       # Update all numeric inputs through javascript
                 #       js$updateAllInputs(rjson::toJSON(life_stages))
                 #
                 #       session$userData$rv_life_stages$dat <- life_stages
                 #
                 #       session$userData$rv_eigen_analysis$dat <- list()
                 #
                 #       session$userData$rv_ea_errors$possible_error_state <- FALSE
                 #       session$userData$rv_ea_errors$possible_error_msg <- ""
                 #
                 #       # session$userData$rv_show_sample_plot$open <- FALSE
                 #
                 #       session$userData$rv_pop_sample_plot_data$dat <- list()
                 #       session$userData$rv_pop_sample_plot_data$run_counter <- 1
                 #
                 #       # Sand box stressor values
                 #       session$userData$rv_sandbox_stressors$dat <- list()
                 #
                 #       session$userData$rv_pop_data_huc_ts$dat <- list()
                 #       session$userData$rv_pop_data_huc_ts$run_counter <- 1
                 #       session$userData$rv_pop_data_huc_ts$update_ts_plots <- FALSE
                 #
                 #       session$userData$rv_show_pop_main_plot$open <- FALSE
                 #
                 #       output$upload_error_msg_sheds <-
                 #         renderText({
                 #           ""
                 #         })
                 #     },
                 #     error = function(e) {
                 #       print("Upload error...")
                 #
                 #       output$upload_error_msg_sheds <-
                 #         renderText({
                 #           "Upload Error: Vital rate parameters did not import correctly. Please check file against the reference and reupload."
                 #         })
                 #     }
                 #   )
                 # }) # end vital rate import
                 
                 
                 
                 # #--------------------------------------
                 # # Upload Socio-Economic Input Workbook Data
                 # observe({
                 #   
                 #   # Require the file
                 #   req(input$up_se_workbook)
                 #   
                 #   upload_ok <- FALSE
                 #   
                 #   # Run import function in a try catch
                 #   # to avoid app crashing on upload errors
                 #   tryCatch({
                 #     in_file <- input$up_se_workbook$datapath
                 #     
                 #     if (is.null(in_file)) {
                 #       return(NULL)
                 #     }
                 #     
                 #     print("Loading SE inputs...")
                 #     
                 #     # Import SE workbook
                 #     socioeconomic_inputs <-
                 #       CEMPRA::SocioEconomicWorkbook(filename = in_file)
                 #     
                 #     # Perform final checks on the imported data
                 #     
                 #     # Check that location IDs match
                 #     loc_ids <- socioeconomic_inputs$`Location Implementation`$`Location ID`
                 #     loc_ids <- unique(loc_ids)
                 #     
                 #     loc_id_check <- isolate({ session$userData$rv_stressor_magnitude$sm_dat[, 1] })
                 #     loc_id_check <- unique(as.data.frame(loc_id_check)[, 1])
                 #     bad_locations <- setdiff(loc_ids, loc_id_check)
                 #     
                 #     if(length(bad_locations) > 0) {
                 #       socioeconomic_inputs$import_pass <- FALSE
                 #       socioeconomic_inputs$error_state <- paste0("Location IDs in the socio-economic workbook do not match the stressor magnitude file. The following location IDs are not found in the stressor magnitude file: ", paste(bad_locations, collapse = ", "))
                 #     }
                 #     
                 #     # Check that stressors match
                 #     stress_ids <- socioeconomic_inputs$`Stressor Reduction`$`Affected Stressor`
                 #     stress_ids <- unique(stress_ids)
                 #     
                 #     stress_check <- isolate({ session$userData$rv_stressor_response$stressor_names })
                 #     
                 #     bad_stressors <- setdiff(stress_ids, stress_check)
                 #     
                 #     if(length(bad_stressors) > 0) {
                 #       socioeconomic_inputs$import_pass <- FALSE
                 #       socioeconomic_inputs$error_state <- paste0("Stressor names in the socio-economic workbook do not match the stressor response file. The following stressor names are not found in the stressor response file: ", paste(bad_stressors, collapse = ", "))
                 #     }
                 #     
                 #     # If file is valid
                 #     if (socioeconomic_inputs$import_pass) {
                 #       
                 #       # Update the socio-economic reactive values
                 #       session$userData$rv_se_inputs$socioeconomic_inputs <-
                 #         socioeconomic_inputs
                 #       
                 #       upload_ok <- TRUE
                 #       
                 #       output$upload_error_msg_se_workbook <-
                 #         renderText({
                 #           ""
                 #         })
                 #       
                 #     } else {
                 #       # If file upload is invalid. Set SE inputs to NULL
                 #       session$userData$rv_se_inputs$socioeconomic_inputs <-
                 #         NULL
                 #       
                 #       upload_ok <- FALSE
                 #       
                 #       output$upload_error_msg_se_workbook <-
                 #         renderText({
                 #           socioeconomic_inputs$error_state
                 #         })
                 #     }
                 #   },
                 #   error = function(e) {
                 #     # return a safeError if a parsing error occurs
                 #     print("Upload error...")
                 #     
                 #     output$upload_error_msg_sm <- renderText({
                 #       "Upload Error: Socio-Economic Input Workbook (xlsx) did not import correctly. Check data format and column names."
                 #     })
                 #   })
                 # }) # end of socio-economic data upload
                 
                 
                 
               })
}
