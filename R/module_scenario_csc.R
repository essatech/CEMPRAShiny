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
                   
                   
                   # Populate select input with stressor options
                   updateSelectInput(
                     session,
                     "location_weighting",
                     choices = sort(unique(session$userData$rv_stressor_magnitude$sm_dat$Stressor))
                   )
                   
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
                                                               choices = NULL),
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
                   
                   if(length(jm_data) == 0) {
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
                   
                   # Add the MCMC batch replicate to the data
                   add_batch_id <- function(df) {
                     df %>%
                       group_by(HUC, scenario_name) %>%
                       mutate(batch_id = row_number()) %>%
                       ungroup()
                   }
                   
                   all_dat <- add_batch_id(all_dat)
                   
                   # Get options from select inputs
                   location_weighting <- input$location_weighting
                   # Product or weight
                   weighting_type <- input$weighting_type
                   # Y-axis label
                   y_lab <- input$custom_y_lab
                   
                   # ==========================================================
                   # If working with a simple unweighted mean
                   # ==========================================================
                   if(length(weighting_type) == 0) {
                     weighting_type <- "Unweighted Mean (%)"
                   }
                   if(is.na(weighting_type)) {
                     weighting_type <- "Unweighted Mean (%)"
                   }
                   if(weighting_type == "Unweighted Mean (%)") {
                     # session$userData$rv_stressor_response$stressor_names 
                     
                     if (length(jm_data) == 0) {
                       print("Empty data...")
                       p <- ggplot(data.frame())
                       return(p)
                     }
                     
                     all_dat$CE <-
                       round(as.numeric(all_dat$CE * 100), 2)
                     
                     # Group by location
                     m2 <- all_dat %>% group_by(scenario_name, batch_id) %>%
                       summarise(CE = mean(CE, na.rm = TRUE)) %>%
                       mutate(scenario_name = factor(scenario_name, levels = unique(scenario_name)))
                     
                     # Create the plot
                     p <-
                       ggplot(m2, aes(x = scenario_name, y = CE)) +
                       geom_boxplot() +
                       ggtitle("Cumulative System Capacity by Scenario") +
                       xlab("Scenario Names") + ylab("Cumulative System Capacity (%)")
                   }
                   
                   # ==========================================================
                   # If working with a weighted mean or product
                   # ==========================================================
                   if(weighting_type != "Unweighted Mean (%)") {
                     
                     print("alt weighting_type selected...")
                     
                     # Get the associated stressor magntiude data
                     sm_data <- session$userData$rv_stressor_magnitude$sm_dat
                     
                     # Determine if target metric is available
                     sm_data <- sm_data[sm_data$Stressor == location_weighting, ]
                     
                     if(nrow(sm_data) > 0) {
                       sm_data <- sm_data[, c("HUC_ID", "Mean")]
                       all_dat <- merge(all_dat, sm_data, by.x = "HUC", by.y = "HUC_ID", all.x = TRUE, all.y = FALSE)
                       # Drop any NA values
                       all_dat <- all_dat[!is.na(all_dat$Mean), ]
                       
                       # Calculate weighted mean and product
                       if(weighting_type == "Weighted Mean (%)") {
                         # Create a weighted mean for each combination of batch_id and scenario_name.
                         # use CE for mean and Mean for weights
                         m2 <- all_dat %>% group_by(scenario_name, batch_id) %>%
                           summarise(CE_adj = weighted.mean(CE, w = Mean, na.rm = TRUE)) %>%
                           mutate(scenario_name = factor(scenario_name, levels = unique(scenario_name)))
                       }
                       # and product
                       if(weighting_type == "Product (custom)") {
                         
                         all_dat$product_calc <- all_dat$CE * all_dat$Mean
                         
                         # Create a weighted mean for each combination of batch_id and scenario_name.
                         # use CE for mean and Mean for weights
                         m2 <- all_dat %>% group_by(scenario_name, batch_id) %>%
                           summarise(CE_adj = sum(product_calc, na.rm = TRUE)) %>%
                           mutate(scenario_name = factor(scenario_name, levels = unique(scenario_name)))
                         
                       }
                       
                       # Create the plot
                       p <-
                         ggplot(m2, aes(x = scenario_name, y = CE_adj)) +
                         geom_boxplot() +
                         ggtitle(y_lab) +
                         xlab("Scenario Names") + ylab(y_lab)
                       
                     } else {
                       # No matching stressor magntidue data
                       print("Empty data...")
                       p <- ggplot(data.frame())
                       return(p)
                     }
                   } # end of alt summary method
                   
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
                   
                   # Get options from select inputs
                   location_weighting <- input$location_weighting
                   # Product or weight
                   weighting_type <- input$weighting_type
                   # Y-axis label
                   y_lab <- input$custom_y_lab
                   
                   # ==========================================================
                   # If working with a simple unweighted mean
                   # ==========================================================
                   if(length(weighting_type) == 0) {
                     weighting_type <- "Unweighted Mean (%)"
                   }
                   if(is.na(weighting_type)) {
                     weighting_type <- "Unweighted Mean (%)"
                   }
                   if(weighting_type == "Unweighted Mean (%)") {
                     # session$userData$rv_stressor_response$stressor_names 
                     
                     if (length(jm_data) == 0) {
                       print("Empty data...")
                       p <- ggplot(data.frame())
                       return(p)
                     }
                     
                     all_dat$CE <-
                       round(as.numeric(all_dat$m.sys.cap * 100), 2)
                     
                     # Group by location
                     m2 <- all_dat %>% group_by(scenario_name, Stressor) %>%
                       summarise(CE = mean(CE, na.rm = TRUE)) %>%
                       mutate(scenario_name = factor(scenario_name, levels = unique(scenario_name)))
                     
                     # Create the plot
                     p <-
                       ggplot(m2, aes(x = scenario_name, y = CE)) +
                       geom_boxplot() +
                       ggtitle("Cumulative System Capacity by Scenario") +
                       xlab("Scenario Names") + ylab("Cumulative System Capacity (%)")
                     
                     p <-
                       ggplot(m2,
                              aes(x = scenario_name,
                                  y = CE,
                                  color = Stressor)) +
                       geom_point() +
                       geom_line() +
                       #geom_dotplot(binaxis = 'y', stackdir = 'center') +
                       ggtitle("System Capacity by Stressor by Scenario") +
                       xlab("Scenario Names") + ylab("Mean System Capacity (%)") +
                       theme(legend.position = "bottom")
                     
                   }
                   
                   # ==========================================================
                   # If working with a weighted mean or product
                   # ==========================================================
                   if(weighting_type != "Unweighted Mean (%)") {
                     
                     print("alt weighting_type selected...")
                     
                     # Get the associated stressor magntiude data
                     sm_data <- session$userData$rv_stressor_magnitude$sm_dat
                     
                     # Determine if target metric is available
                     sm_data <- sm_data[sm_data$Stressor == location_weighting, ]
                     
                     if(nrow(sm_data) > 0) {
                       sm_data <- sm_data[, c("HUC_ID", "Mean")]
                       all_dat <- merge(all_dat, sm_data, by.x = "HUC", by.y = "HUC_ID", all.x = TRUE, all.y = FALSE)
                       # Drop any NA values
                       all_dat <- all_dat[!is.na(all_dat$Mean), ]
                       
                       # Calculate weighted mean and product
                       if(weighting_type == "Weighted Mean (%)") {
                         # Create a weighted mean for each combination of batch_id and scenario_name.
                         # use CE for mean and Mean for weights
                        
                         all_dat$CE <- as.numeric(all_dat$m.sys.cap)
                         
                         m2 <- all_dat %>% group_by(scenario_name, Stressor) %>%
                           summarise(CE_adj = weighted.mean(CE, w = Mean, na.rm = TRUE)) %>%
                           mutate(scenario_name = factor(scenario_name, levels = unique(scenario_name)))
                       }
                       # and product
                       if(weighting_type == "Product (custom)") {
                         
                         all_dat$CE <- as.numeric(all_dat$m.sys.cap)
                         
                         all_dat$product_calc <- all_dat$CE * all_dat$Mean
                         
                         # Create a weighted mean for each combination of batch_id and scenario_name.
                         # use CE for mean and Mean for weights
                         m2 <- all_dat %>% group_by(scenario_name, Stressor) %>%
                           summarise(CE_adj = sum(product_calc, na.rm = TRUE)) %>%
                           mutate(scenario_name = factor(scenario_name, levels = unique(scenario_name)))
                         
                       }
                       
                       # Create the plot
                       p <-
                         ggplot(m2,
                                aes(x = scenario_name,
                                    y = CE_adj,
                                    color = Stressor)) +
                         geom_point() +
                         geom_line() +
                         #geom_dotplot(binaxis = 'y', stackdir = 'center') +
                         ggtitle(y_lab) +
                         xlab("Scenario Names") + ylab(y_lab) +
                         theme(legend.position = "bottom")
                       
                     } else {
                       # No matching stressor magntidue data
                       print("Empty data...")
                       p <- ggplot(data.frame())
                       return(p)
                     }
                   } # end of alt summary method
                   
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