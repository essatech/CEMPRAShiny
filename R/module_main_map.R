#' Main Map Module UI
#'
#' The UI portion of the main map module
#'
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom shinydashboard box
#'
#' @param id The id for this module
#'
#' @return a tagList containing UI elements
#'
module_main_map_ui <- function(id) {
  ns <- NS(id)
  
  tagList(shinydashboard::box(width = 12,
                              fluidRow(
                                column(
                                  width = 9,
                                  
                                  # Mouse-over text for basin name and HUC code
                                  htmlOutput(ns("txt_basin_name")),
                                  htmlOutput(ns("txt_huc_code")),
                                  
                                  # Main leaflet output with variable width and fixed height
                                  leafletOutput(ns("mainmap"), height = 550),
                                  
                                  
                                  fluidRow(column(width = 12,
                                                  module_huc_results_ui(
                                                    ns("huc_results")
                                                  ))),
                                ),
                                column(
                                  width = 3,
                                  style = "padding-left: 0; margin-left: -10px;",
                                  shinydashboard::box(
                                    width = 12,
                                    tags$div(
                                      class = "stack-box section-heading",
                                      style = "padding-left: 15px;",
                                      tags$b("Stressor-Response Relationships"),
                                      checkboxInput(
                                        ns("hover_values"),
                                        "Show raw values on mouse hover (slow)",
                                        FALSE
                                      ),
                                      module_all_sr_curves_ui(ns("all_sr_curves")),
                                    ),
                                    tags$div(
                                      class = "stack-box csc-box map-variable hide-this",
                                      id = ns("var_id"),
                                      shinydashboard::box(
                                        width = 12,
                                        # MJBA this hide this upon rendering
                                        background = "light-blue",
                                        tags$p("System Capacity", style = "float: left;"),
                                        # tags$p("[0%]", style = "float: right;"),
                                        tags$div(numericInput(
                                          ns("hiddenload"),
                                          label = "hidden", value = 0
                                        ), style = "display:none;")
                                      )
                                    ),
                                    
                                    # Create the stressor variable sidebar list
                                    htmlOutput(ns("stressor_variable_list")),
                                    
                                    
                                    tags$p("Select a stressor to display on the map or click the chart icon to edit details.", class = "small-helper-text"),
                                  ),
                                ),
                              )) # End of main box
          ) # End of main tagList
}


#' Main Map Module Server
#'
#' The Server portion of the Main Map module
#'
#' @importFrom shiny reactive reactiveVal observeEvent req callModule eventReactive
#' @importFrom dplyr tbl collect mutate arrange select filter pull
#' @importFrom shinyFeedback showFeedbackDanger hideFeedback showToast
#'
#' @param none
#'
#' @return None
#'
module_main_map_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 
                 # Create a list of stressor IDs that have already been registered
                 rv_registered_stressors <- reactiveValues(registered = NULL)
                 
                 
                 # Call sub module for HUC results
                 module_huc_results_server("huc_results")
                 module_all_sr_curves_server("all_sr_curves")
                 
                 # Toggle mouse-over huc values on and off
                 observeEvent(input$hover_values, {
                   session$userData$rv_stressor_response$hover_values <-
                     input$hover_values
                 })
                 
                 
                 
                 # --------------------------------------
                 # Main leaflet map reactive expression
                 # --------------------------------------
                 # Main leaflet map is called here, but only static elements
                 # are included that do not need updating with dynamic events.
                 # HUCs are called later with proxy since we will update then frequently.
                 output$mainmap <- renderLeaflet({
                   print("Running renderLeaflet()...")
                   mymap <- leaflet() %>%
                     addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo",
                                      options = providerTileOptions(noWrap = TRUE,
                                                                    opacity = 0.9)) %>%
                     addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
                     addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Grey", 
                                      options = providerTileOptions(noWrap = TRUE,
                                                                    opacity = 0.9)) %>%
                     addLayersControl(
                       baseGroups = c("Topo", "Imagery", "Grey"),
                       options = layersControlOptions(collapsed = FALSE)
                       )
                   
                   if(isolate(session$userData$geom_type) == "lines") {
                     line_weight <- 3
                     if(!(is.null(isolate(session$userData$rv_HUC_layer_load$data$WIDTH)))) {
                       line_weight <- isolate(session$userData$rv_HUC_layer_load$data$WIDTH)
                     }
                     mymap <- addPolylines(
                       map = mymap,
                       data = session$userData$rv_HUC_layer_load$data,
                       layerId = session$userData$rv_HUC_layer_load$data$uid,
                       color = "#3b9ab2",
                       weight = line_weight,
                       smoothFactor = 0.5,
                       opacity = 0.7,
                       highlightOptions = highlightOptions(
                         color = "#c2fffe",
                         weight = 4,
                         bringToFront = TRUE
                       )
                     )
                   } else {
                     mymap <- addPolygons(
                       map = mymap,
                       data = session$userData$rv_HUC_layer_load$data,
                       layerId = session$userData$rv_HUC_layer_load$data$uid,
                       color = "#444444",
                       weight = 1.2,
                       smoothFactor = 0.5,
                       opacity = 0.5,
                       fillOpacity = 0.5,
                       fillColor = "#d9d9d9",
                       highlightOptions = highlightOptions(
                         color = "white",
                         weight = 2,
                         bringToFront = TRUE
                       )
                     )
                   }
                   
                   mymap
                   # fitBounds(-119.04060, 52.37167, -114.78314, 54.76054)
                 })
                 
                 
                 # ---------------------------------------------------------
                 # Reactive expression to reload, repaint or redraw polygons
                 # ---------------------------------------------------------
                 r_huc_polygons <- reactive({
                   
                   print("r_huc_polygons() triggered ...")
                   
                   # If clear all selected - then trigger redraw.
                   session$userData$rv_redraw$redraw
                   
                   # Name of the variable to display
                   var_name <-
                     session$userData$rv_stressor_response$active_layer
                   
                   # HUC spatial geometry
                   huc_geom <- session$userData$rv_HUC_geom$huc_geom
                   
                   # Check if geometry is line or polygon
                   geom_type <-
                     st_geometry_type(session$userData$rv_HUC_layer_load$data)
                   if (unique(geom_type)[1] %in% c("LINESTRING", "MULTILINESTRING")) {
                     session$userData$geom_type <- "lines"
                   } else {
                     session$userData$geom_type <- "polygons"
                   }
                   
                   # Get values from magnitude table if normal variable
                   if (var_name != "system_capacity") {
                     # Data for HUCs (Stressor Magnitude)
                     sm_df <- session$userData$rv_stressor_magnitude$sm_dat
                     sm_df <- sm_df[sm_df$Stressor == var_name,]
                     
                     # Merge stressor magnitude values to sf object
                     huc_geom$values <- NA
                     huc_geom$values <-
                       sm_df$Mean[match(huc_geom$HUC_ID, sm_df$HUC_ID)]
                     
                     # Look at relationship from response curve
                     resp_curv <-
                       session$userData$rv_stressor_response$sr_dat[[var_name]]
                     
                     if (is.null(resp_curv)) {
                       # Special case for interaction matrix
                       print("Interaction matrix...")
                       sm_df <- session$userData$rv_stressor_magnitude$sm_dat
                       MInt_all <-
                         session$userData$rv_stressor_response$interaction_values
                       MInt <- MInt_all[[var_name]]
                       sm_df$dose <- sm_df$Mean
                       sm_df$HUC <- sm_df$HUC_ID
                       sm_df$simulation <- 1
                       sm_df$sys.cap <- NA
                       sm_df <-
                         sm_df[, c("HUC", "Stressor", "simulation", "dose", "sys.cap")]
                       sys_cap_resp <-
                         CEMPRA::interaction_matrix_sys_cap(
                           MInt = MInt,
                           sc.dose.df = sm_df,
                           adult_sys_cap = FALSE
                         )
                       sys_cap_resp <-
                         sys_cap_resp[which(sys_cap_resp$Stressor == var_name),]
                       
                       # Assign values to HUC
                       huc_geom$values <-
                         sys_cap_resp$sys.cap[match(huc_geom$HUC_ID, sys_cap_resp$HUC)]
                       huc_geom$values_sc <- huc_geom$values
                       
                       # Convert to percent for matrix surface
                       huc_geom$values_sc <- huc_geom$values_sc * 100
                       
                     } else {
                       # Normal stressor interpolation using curve
                       # Convert raw variable values to system capacity
                       
                       interp <- approx(
                         x = resp_curv$value,
                         y = resp_curv$mean_system_capacity,
                         xout = huc_geom$values,
                         rule = 2 # MJB change: points outside of range assume closest
                         # yleft = 0, yright = 100
                       )
                       huc_geom$values_sc <- interp$y
                     }
                     
                   } else {
                     # Otherwise look at system capacity from Joe mode
                     res <- session$userData$rv_joe_model_results$sims
                     res <-
                       res[[length(res)]] # Get the latest result (if multiple)
                     # Take the average by each watershed
                     df_vals <- res$ce.df %>%
                       dplyr::group_by(HUC) %>%
                       dplyr::summarise(values_sc = mean(CE))
                     # Add values as attribute to original object
                     huc_geom$values_sc <-
                       df_vals$values_sc[match(huc_geom$HUC_ID, df_vals$HUC)]
                     huc_geom$values_sc <- huc_geom$values_sc * 100
                     
                   }
                   
                   # Apply color ramp function
                   huc_geom$color_vec <- color_func(huc_geom$values_sc)
                   
                   # Update reference color dataframe rv to reset colors after selection
                   col_df <-
                     data.frame(id = huc_geom$HUC_ID, col = huc_geom$color_vec)
                   session$userData$rv_HUC_geom$color_df <- col_df
                   
                   return(huc_geom)
                 })
                 
                 
                 # --------------------------------------
                 # HUC polygon draw
                 # --------------------------------------
                 # Draw update or edit HUC polygons on leaflet map
                 # use an observe() function to capture changes and use
                 # leafletProxy() to only update the target layer.
                 
                 print("HUC polygon draw...")
                 
                 observe({
                   
                   print("Updating polygons with observer()...")
                   
                   if(isolate(session$userData$geom_type) == "lines") {
                     line_weight <- 3
                     if(!(is.null(isolate(session$userData$rv_HUC_layer_load$data$WIDTH)))) {
                       line_weight <- isolate(session$userData$rv_HUC_layer_load$data$WIDTH)
                     }
                     leafletProxy("mainmap") %>%
                       clearShapes() %>%
                       # Add or update HUC polygons on the map
                       addPolylines(
                         data = r_huc_polygons(),
                         layerId = r_huc_polygons()$uid,
                         color = r_huc_polygons()$color_vec,
                         weight = line_weight,
                         smoothFactor = 0.5,
                         opacity = 0.7,
                         highlightOptions = highlightOptions(
                           color = "#c2fffe",
                           weight = 4,
                           bringToFront = TRUE
                         )
                       ) %>%
                       # Delete any old pre-existing legend
                       clearControls() %>%
                       # Add new legend
                       addLegend(
                         "bottomright",
                         colors = leg_col,
                         labels = leg_lab,
                         title = session$userData$rv_stressor_response$active_layer,
                         opacity = 0.9
                       )
                   }
                   if(session$userData$geom_type == "polygons") {
                     leafletProxy("mainmap") %>%
                       clearShapes() %>%
                       # Add or update HUC polygons on the map
                       addPolygons(
                         data = r_huc_polygons(),
                         layerId = r_huc_polygons()$uid,
                         color = "#444444",
                         weight = 1.2,
                         smoothFactor = 0.5,
                         opacity = 0.5,
                         fillOpacity = 0.5,
                         fillColor = r_huc_polygons()$color_vec,
                         highlightOptions = highlightOptions(
                           color = "white",
                           weight = 2,
                           bringToFront = TRUE
                         )
                       ) %>%
                       # Delete any old pre-existing legend
                       clearControls() %>%
                       # Add new legend
                       addLegend(
                         "bottomright",
                         colors = leg_col,
                         labels = leg_lab,
                         title = session$userData$rv_stressor_response$active_layer,
                         opacity = 0.9
                       )
                   }
                   

                   
                   
                   # Add on selected HUCs (if any)
                   selected_hucs <- isolate(session$userData$rv_clickedIds$ids)
                   
                   if (length(selected_hucs) > 0) {
                     
                     print("Adding selected HUCs")
                     huc_geom_sel <- session$userData$rv_HUC_geom$huc_geom
                     
                     # Get subset of selected HUCs
                     huc_geom_sel <-
                       huc_geom_sel[which(huc_geom_sel$uid %in% selected_hucs),]
                     
                     # Update special ID
                     huc_geom_sel$uid <- paste0("select|", huc_geom_sel$uid)
                     
                     # Add selected HUCs to map
                     if(session$userData$geom_type == "lines") {
                       line_weight <- 3
                       if(!(is.null(isolate(session$userData$rv_HUC_layer_load$data$WIDTH)))) {
                         line_weight <- isolate(session$userData$rv_HUC_layer_load$data$WIDTH)
                       }
                       leafletProxy("mainmap") %>%
                         addPolylines(
                           data = huc_geom_sel,
                           layerId = huc_geom_sel$uid,
                           color = "#4dfff3",
                           weight = line_weight,
                           smoothFactor = 0.5,
                           opacity = 0.9,
                           #fillOpacity = 0.95,
                           #fillColor = "#4dfff3",
                           highlightOptions = highlightOptions(
                             color = "#c2fffe",
                             weight = 4,
                             bringToFront = TRUE
                           )
                         )
                     }
                     
                     if(session$userData$geom_type == "polygons") {
                       leafletProxy("mainmap") %>%
                         addPolygons(
                           data = huc_geom_sel,
                           layerId = huc_geom_sel$uid,
                           color = "#c2fffe",
                           weight = 1.2,
                           smoothFactor = 0.5,
                           opacity = 0.9,
                           fillOpacity = 0.95,
                           fillColor = "#4dfff3",
                           highlightOptions = highlightOptions(
                             color = "white",
                             weight = 2,
                             bringToFront = TRUE
                           )
                         )
                     }
                     

                   }
                 })
                 
                 
                 # ---------------------------------------------------------
                 # Mouse-Click on HUC events
                 # ---------------------------------------------------------
                 # Use a second observer to update the selected polygons...
                 # Mouse click events on a given HUC
                 observeEvent(input$mainmap_shape_click, {
                   # create object for clicked polygon
                   click <- input$mainmap_shape_click
                   # Get all the previous clicked polys
                   previous_click_ids <- session$userData$rv_clickedIds$ids
                   
                   # Get the HUC geom for the specific click
                   huc_geom <- session$userData$rv_HUC_geom$huc_geom
                   this_uid <- gsub("select\\|", "", click$id)
                   huc_geom_single <-
                     huc_geom[which(huc_geom$uid == this_uid),]
                   huc_geom_single$uid <-
                     paste0("select|", huc_geom_single$uid)
                   
                   if (this_uid %in% previous_click_ids) {
                     print("Remove from selected...")
                     # Polygon has already been clicked
                     # Need to remove it from the click vector
                     new_click_vec <-
                       previous_click_ids[previous_click_ids != this_uid]
                     session$userData$rv_clickedIds$ids <- new_click_vec
                     leafletProxy("mainmap") %>%
                       removeShape(huc_geom_single$uid)
                   } else {
                     print("Add to selected")
                     # User select a new polygon - nedd to update the color and add it to select list
                     # Need to add it from the click vector
                     session$userData$rv_clickedIds$ids <-
                       c(session$userData$rv_clickedIds$ids, this_uid)
                     
                     
                     if(session$userData$geom_type == "lines") {
                       line_weight <- 3
                       if(!(is.null(isolate(session$userData$rv_HUC_layer_load$data$WIDTH)))) {
                         line_weight <- isolate(session$userData$rv_HUC_layer_load$data$WIDTH)
                       }
                       leafletProxy("mainmap") %>%
                         addPolylines(
                           data = huc_geom_single,
                           layerId = huc_geom_single$uid,
                           color = "#4dfff3",
                           weight = line_weight,
                           smoothFactor = 0.5,
                           opacity = 0.9,
                           #fillOpacity = 0.95,
                           #fillColor = "#4dfff3",
                           highlightOptions = highlightOptions(
                             color = "#c2fffe",
                             weight = 4,
                             bringToFront = TRUE
                           )
                         )
                     }
                     
                     if(session$userData$geom_type == "polygons") {
                       leafletProxy("mainmap") %>%
                         addPolygons(
                           data = huc_geom_single,
                           layerId = huc_geom_single$uid,
                           color = "#c2fffe",
                           weight = 1.2,
                           smoothFactor = 0.5,
                           opacity = 0.9,
                           fillOpacity = 0.95,
                           fillColor = "#4dfff3",
                           highlightOptions = highlightOptions(
                             color = "white",
                             weight = 2,
                             bringToFront = TRUE
                           )
                         )
                     }
                     
                     

                     # End of deselect polygon and restor original color
                   }
                 })
                 
                 
                 
                 # ---------------------------------------------------------
                 # Define the stressor variables to plot div button side bar
                 # ---------------------------------------------------------
                 # 1) a single uiOutput that just rebuilds all of the boxes
                 output$stressor_variable_list <- renderUI({
                   req(session$userData$rv_stressor_response$stressor_names)
                   snames <- session$userData$rv_stressor_response$stressor_names
                   
                   # wrap them in a container so we know where they live
                   tags$div(id = ns("stressor_list_container"),
                            lapply(snames, function(s) {
                              # for each stressor name, call the UI function with its namespace
                              module_stressor_variable_ui(ns(s))
                            })
                   )
                 })
                 
                 # 2) separately, whenever the list of stressors changes, register exactly
                 #    one moduleServer() per new ID, **outside** of renderUI().
                 observeEvent(session$userData$rv_stressor_response$stressor_names, {
                   snames <- session$userData$rv_stressor_response$stressor_names
                   
                   lapply(seq_along(snames), function(i) {
                     # this will call module_stressor_variable_server() exactly once
                     # for each `snames[i]`, and tie it to the UI you just created above
                     module_stressor_variable_server(snames[i], stressor_index = i)
                   })
                 })
                 
                 
                 
                 # ---------------------------------------------------------
                 # Show the HUC Code and Basin Name Above Map
                 # ---------------------------------------------------------
                 output$txt_huc_code <- renderUI({
                   if (session$userData$rv_map_shape()) {
                     tags$p(session$userData$rv_map_location$huc_id, style = "float: right; color:#3b9ab2;")
                   } else {
                     tags$p("Location ID", style = "float: right; color:#3b9ab2;")
                   }
                 })
                 
                 output$txt_basin_name <- renderUI({
                   if (session$userData$rv_map_shape()) {
                     tags$p(session$userData$rv_map_location$huc_name, style = "float: left; color:#3b9ab2;")
                   } else {
                     tags$p("Location Name", style = "float: left; color:#3b9ab2;")
                   }
                 })
                 
                 
                 # ---------------------------------------------------------
                 # Mouse-over and mouse-out events
                 # ---------------------------------------------------------
                 # Observe mouseover events over leaflet map
                 # note the event concatenation 'object name' + '_click'; 'object name' + '_shape_mouseover'
                 observeEvent(input$mainmap_shape_mouseout, {
                   session$userData$rv_map_shape(FALSE)
                   session$userData$rv_stressor_response$active_values_raw <-
                     NULL
                 })
                 
                 observeEvent(input$mainmap_shape_mouseover, {
                   # User hovers mouse over a polygon (layer specific)
                   mainmap_shape_mouseover_info <-
                     input$mainmap_shape_mouseover
                   
                   if (!(is.null(mainmap_shape_mouseover_info))) {
                     # Parse the ID and HUC name
                     session$userData$rv_map_shape(TRUE)
                     poly_obj <-
                       mainmap_shape_mouseover_info$id # note leaflet id slot
                     parse_id <- strsplit(as.character(poly_obj), "\\|")[[1]]
                     huc_id <- parse_id[1]
                     huc_name <- parse_id[2]
                     session$userData$rv_map_location$huc_id <- huc_id
                     session$userData$rv_map_location$huc_name <- huc_name
                     
                     # Check box for user to toggle mouse-over display
                     # lag and time intensive so turned off by default
                     if (input$hover_values) {
                       # Get target values for each variable
                       # to prevent lag only run every half second
                       target_vals <-
                         session$userData$rv_stressor_magnitude$sm_dat %>%
                         dplyr::filter(HUC_ID == huc_id) %>%
                         dplyr::select("Stressor", "Mean")
                       
                       
                       
                       # Set active values
                       if (nrow(target_vals) > 0) {
                         session$userData$rv_stressor_response$active_values_raw <-
                           target_vals
                       } else {
                         # otherwise null
                         session$userData$rv_stressor_response$active_values_raw <-
                           NULL
                       }
                     } else {
                       session$userData$rv_stressor_response$active_values_raw <- NULL
                     }
                   }
                 })
                 
                 
                 # ------------------------------------------------------
                 # User clicks on System Capacity Map plot
                 # ------------------------------------------------------
                 # Listen to click events to change target variable selected
                 observe({
                   # ensure UI is loaded - do not run if not set
                   req(input$hiddenload)
                   # User clicks on ID
                   # Update reactive value for system capacity
                   updateActiveVar <- function() {
                     session$userData$rv_stressor_response$active_layer <-
                       "system_capacity"
                   }
                   # Use mouse click
                   my_id <- paste0("main_map-var_id")
                   onclick(my_id,
                           updateActiveVar(),
                           asis = TRUE)
                 })
                 
                 # ------------------------------------------
                 # Update selected class on layer panel
                 observe({
                   req(input$hiddenload)
                   req(session$userData$rv_stressor_response$active_layer)
                   print("Setting style...")
                   # print(active)
                   # Strip class away from any other selected
                   # q_code <- paste0("jQuery('.map-variable').removeClass('var-selected');")
                   # shinyjs::runjs(code = q_code)
                   
                   # Add class to system capacity
                   #  q_code <- paste0("jQuery('#main_map-var_id').addClass('var-selected');")
                   # shinyjs::runjs(code = q_code)
                 })
                 
               })
}
