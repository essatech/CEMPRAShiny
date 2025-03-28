#' CSC plots across scenarios ui
#'
#' @param id The id for this module
#'
#' @return a tagList containing UI elements
#'
module_scenario_csc_ui <- function(id) {
  ns <- NS(id)
  # Single action button to call modal
  actionButton(
    ns("open_scenario_csc"),
    tags$b("Scenario Results"),
    class = "chart-line clean-button",
    width = "100%"
  )
  
}



#' CSC plots across scenarios server
#'
#' Server and modal content for the Joe model server
#'
#' @param none
#'
#' @return None
#'
module_scenario_csc_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 
                 print("Calling module_scenario_csc_server...")
                 
                 #-------------------------------------------------------
                 # DISABLE AND ENABLE
                 #-------------------------------------------------------
                 # this module is disabled if the Joe Model results are empty
                 observe({
                   sims <- session$userData$rv_joe_model_results$sims
                   if (length(sims) > 0) {
                     shinyjs::enable("open_scenario_csc")
                   } else {
                     shinyjs::disable("open_scenario_csc")
                   }
                 })
                 
                 
                 
                 
                 #-------------------------------------------------------
                 # START OF INPUT MODAL UI
                 #-------------------------------------------------------
                 # Display the scenario tab
                 observeEvent(input$open_scenario_csc, {
                   print("Joe model form click to open ...")
                   # Gather a list of all the stessors to build the checkbox list
                   showModal(
                     modalDialog(
                       title = "Cumulative System Capacity Across Scenarios",
                       tagList(
                         shinydashboard::box(width = 12,
                                             fluidRow(
                                               column(
                                                 width = 12,
                                                 tags$p(
                                                   "Each time the Joe Model is run, a new “scenario” is created in this tab. A scenario can consist of changes to the stressor magnitude data, changes to the stressor-response data, or changes in the composition of variables included in the Joe Model run. For example, hypothetical “restoration action scenario” might include changes to the stressor magnitude variables at one or more locations to represent some possible intervention. A hypothetical future climate change scenario might increase the severity of temperature-related stressors. Other scenarios might include sensitivity tests that run the Joe Model under different assumptions about the relationship between the cumulative system capacity and one or more stressors. The idea is to either upload new input data or make changes in the app to create “new scenarios”. The Joe Model is then re-run for each scenario, and users review the overall change in the system capacity scores. Be sure to provide a unique name for each scenario for ease of reference. See the “Name of this Simulation” option in the Joe Model Run tab.", class = "small-helper-text"),
                                                 # Action button
                                                 fluidRow(column(
                                                   width = 12,
                                                   tagList(
                                                     actionButton(
                                                       ns("clear_button"),
                                                       "Clear data from previous scenarios for new comparison set"
                                                     )
                                                   )
                                                 )),
                                                 
                                                 tags$br(),
                                                 
                                                 # Weightings
                                                 fluidRow(
                                                   column(
                                                     width = 3,
                                                     selectInput(ns("weighting_type"), "Product or Weight:",
                                                                 c("Unweighted Mean (%)" = "Unweighted Mean (%)",
                                                                   "Weighted Mean (%)" = "Weighted Mean (%)",
                                                                   "Product (custom)" = "Product (custom)")),
                                                   ),
                                                   column(
                                                   width = 5,
                                                   selectInput(ns("location_weighting"), "Weight Location Scores By:",
                                                               c("Equal Weight Per Location" = "equal",
                                                                 "By Area (geometry)" = "area",
                                                                 "By Length (geometry)" = "length",
                                                                 "Estuary_Survival" = "Estuary_Survival",
                                                                 "Fines" = "Fines",
                                                                 "Fry_Capacity" = "Fry_Capacity",
                                                                 "Spawn_Capacity" = "Spawn_Capacity",
                                                                 "Spawn_Gravel" = "Spawn_Gravel",
                                                                 "Spawm_Temp_Eggs" = "Spawm_Temp_Eggs",
                                                                 "Spawm_Temp_Fry" = "Spawm_Temp_Fry",
                                                                 "Stream_Temp_Prespawn" = "Stream_Temp_Prespawn",
                                                                 "Wood_Abund_Fry" = "Wood_Abund_Fry")),
                                                   ),
                                                   column(
                                                     width = 4,
                                                     textInput(ns("custom_y_lab"), "Y-axis Label (New Units):", value = "Cumulative System Capacity (%)")
                                                   ),
                                                 ),
                                                 
                                                 fluidRow(column(plotlyOutput(
                                                   ns("scenario_boxplots")
                                                 ), width = 12)),
                                                 
                                                 fluidRow(column(
                                                   tags$p(
                                                     "The next set of plots shows each stressor's average system capacity score across each scenario. Only the mean value is shown (across locations) for convenience rather than the full distribution of each stressor. However, reviewing the plot below is useful for understanding the most critical and least critical stressors across scenarios.", class = "small-helper-text"),
                                                   width = 12
                                                 )),
                                                 fluidRow(column(plotlyOutput(
                                                   ns("scenario_stressor_dotplots")
                                                 ),
                                                 width = 12)),
                                               )
                                             )),
                         tags$p("Scenario Set")
                       ),
                       easyClose = TRUE,
                       size = 'l',
                       footer = NULL
                     )
                   )
                 }) # END OF INPUT MODAL UI
                 #-------------------------------------------------------
                 
                 
                 # ---------------------------------------------
                 # Generate plot for all life stages here (& lambda)...
                 # ---------------------------------------------
                 output$scenario_boxplots <- renderPlotly({
                   print("render scenario_boxplots...")
                   
                   jm_data <-
                     session$userData$rv_joe_model_results_scenarios$sims
                   
                   if (length(jm_data) == 0) {
                     print("Empty data...")
                     p <- ggplot(data.frame())
                     return(p)
                   }
                   
                   # Get data for current run (from above)
                   # Get the ce.df slot from element in the jm_data list
                   gather_ce <- function(x) {
                     t_dat <- x$ce.df
                     return(t_dat)
                   }
                   
                   all_dat <- lapply(jm_data, gather_ce)
                   all_dat <- do.call("rbind", all_dat)
                   all_dat$scenario_name <-
                     as.character(all_dat$scenario_name)
                   all_dat$CE <-
                     round(as.numeric(all_dat$CE * 100), 2)
                   
                   # Create the plot
                   p <-
                     ggplot(all_dat, aes(x = scenario_name, y = CE)) +
                     geom_boxplot() +
                     ggtitle("Cumulative System Capacity by Scenario") +
                     xlab("Scenario Names") + ylab("Cumulative System Capacity (%)")
                   
                   return(p)
                   
                 })
                 
                 
                 output$scenario_stressor_dotplots <- renderPlotly({
                   print("render scenario_stressor_dotplots...")
                   
                   jm_data <-
                     session$userData$rv_joe_model_results_scenarios$sims
                   
                   if (length(jm_data) == 0) {
                     print("Empty data...")
                     p <- ggplot(data.frame())
                     return(p)
                   }
                   
                   gather_ce <- function(x) {
                     t_dat <- x$sc.dose.df
                     return(t_dat)
                   }
                   
                   all_dat <- lapply(jm_data, gather_ce)
                   all_dat <- do.call("rbind", all_dat)
                   all_dat$scenario_name <-
                     as.character(all_dat$scenario_name)
                   all_dat$m.sys.cap <-
                     round(as.numeric(all_dat$m.sys.cap * 100), 2)
                   
                   p <-
                     ggplot(all_dat,
                            aes(x = scenario_name,
                                y = m.sys.cap,
                                color = Stressor)) +
                     geom_point() +
                     #geom_dotplot(binaxis = 'y', stackdir = 'center') +
                     ggtitle("System Capacity by Stressor by Scenario") +
                     xlab("Scenario Names") + ylab("Mean System Capacity (%)") +
                     theme(legend.position = "bottom")
                   
                   return(p)
                   
                 })
                 
                 
                 # ---------------------------------------------
                 # Clear scenario data...
                 # ---------------------------------------------
                 # Observe the action button
                 observeEvent(input$clear_button, {
                   # Joe model scenario name holder - assume multiple runs
                   session$userData$rv_joe_model_results_scenarios$sims <-
                     list()
                 })
                 
                 
                 
               })
}