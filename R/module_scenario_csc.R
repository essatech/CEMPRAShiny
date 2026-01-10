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

                   # Count scenarios for display
                   n_scenarios <- length(session$userData$rv_joe_model_results_scenarios$sims)

                   # Gather a list of all the stessors to build the checkbox list
                   showModal(
                     modalDialog(
                       title = NULL,
                       tagList(
                         # Custom header with title
                         tags$div(
                           class = "scenario-modal-header",
                           tags$h4("Scenario Comparison", class = "scenario-modal-title")
                         ),

                         # Brief intro text with expand option
                         tags$div(
                           class = "scenario-intro-section",
                           tags$p(
                             "Compare cumulative system capacity across different Joe Model runs. Each model run creates a new scenario for comparison.",
                             class = "scenario-intro-text"
                           ),
                           actionLink(ns("toggle_help"), "Learn more about scenarios...", class = "expand-help-link"),
                           tags$div(
                             id = ns("help_expanded"),
                             class = "help-expanded-content",
                             style = "display: none;",
                             tags$p(
                               "A scenario can consist of changes to the stressor magnitude data, changes to the stressor-response relationships, or changes in the composition of variables included in the Joe Model run.",
                               class = "help-detail-text"
                             ),
                             tags$p(
                               tags$b("Example scenarios:"),
                               class = "help-detail-text"
                             ),
                             tags$ul(
                               class = "help-detail-list",
                               tags$li("Restoration actions - modify stressor magnitudes at locations to represent interventions"),
                               tags$li("Climate change - increase severity of temperature-related stressors"),
                               tags$li("Sensitivity tests - run under different assumptions about stressor-response relationships")
                             ),
                             tags$p(
                               "Be sure to provide a unique name for each scenario in the 'Name of this Simulation' field when running the Joe Model.",
                               class = "help-detail-text",
                               style = "font-style: italic;"
                             )
                           )
                         ),

                         # Main comparison plot (always visible)
                         tags$div(
                           class = "scenario-main-plot",
                           plotlyOutput(ns("scenario_boxplots"), height = "350px")
                         ),

                         # Collapsible: Advanced Options
                         tags$div(
                           class = "collapsible-section",
                           actionLink(
                             ns("toggle_advanced"),
                             tagList(
                               tags$span(class = "collapse-icon", id = ns("icon_advanced"), icon("chevron-right")),
                               tags$span("Advanced Options", class = "collapsible-title"),
                               tags$span("Weighting & custom labels", class = "collapsible-subtitle")
                             ),
                             class = "collapsible-header-link"
                           ),
                           tags$div(
                             id = ns("advanced_options"),
                             class = "collapsible-content",
                             style = "display: none;",
                             tags$p("Use these options to weight location scores by a stressor variable (e.g., habitat area) or customize the output labels.", class = "collapsible-help-text"),
                             fluidRow(
                               column(
                                 width = 4,
                                 selectInput(ns("weighting_type"), "Aggregation Method:",
                                             c("Unweighted Mean (%)" = "Unweighted Mean (%)",
                                               "Weighted Mean (%)" = "Weighted Mean (%)",
                                               "Product (custom)" = "Product (custom)"))
                               ),
                               column(
                                 width = 4,
                                 selectInput(ns("location_weighting"), "Weight By Stressor:",
                                             choices = NULL)
                               ),
                               column(
                                 width = 4,
                                 textInput(ns("custom_y_lab"), "Y-axis Label:", value = "Cumulative System Capacity (%)")
                               )
                             )
                           )
                         ),

                         # Collapsible: Stressor Breakdown
                         tags$div(
                           class = "collapsible-section",
                           actionLink(
                             ns("toggle_stressor"),
                             tagList(
                               tags$span(class = "collapse-icon", id = ns("icon_stressor"), icon("chevron-right")),
                               tags$span("Stressor Breakdown", class = "collapsible-title"),
                               tags$span("View individual stressor contributions", class = "collapsible-subtitle")
                             ),
                             class = "collapsible-header-link"
                           ),
                           tags$div(
                             id = ns("stressor_breakdown"),
                             class = "collapsible-content",
                             style = "display: none;",
                             tags$p(
                               "This plot shows each stressor's average system capacity score across scenarios. Only the mean value is shown (across locations) for convenience. This view is useful for understanding which stressors are most limiting and how they change across scenarios.",
                               class = "collapsible-help-text"
                             ),
                             plotlyOutput(ns("scenario_stressor_dotplots"), height = "300px")
                           )
                         ),

                         # Footer with manage scenarios
                         tags$div(
                           class = "scenario-modal-footer",
                           tags$div(
                             class = "scenario-count",
                             icon("layer-group"),
                             tags$span(paste(n_scenarios, "scenario(s) saved"))
                           ),
                           actionButton(
                             ns("clear_button"),
                             tagList(icon("trash-can"), "Clear All"),
                             class = "btn-clear-scenarios"
                           )
                         )
                       ),
                       easyClose = TRUE,
                       size = 'l',
                       footer = NULL
                     )
                   )
                 }) # END OF INPUT MODAL UI
                 #-------------------------------------------------------


                 # Toggle help text
                 observeEvent(input$toggle_help, {
                   shinyjs::toggle("help_expanded")
                   # Update link text
                   if (input$toggle_help %% 2 == 1) {
                     shinyjs::html("toggle_help", "Show less")
                   } else {
                     shinyjs::html("toggle_help", "Learn more about scenarios...")
                   }
                 })

                 # Toggle advanced options
                 observeEvent(input$toggle_advanced, {
                   shinyjs::toggle("advanced_options")
                   shinyjs::toggleClass(id = "icon_advanced", class = "rotated")
                 })

                 # Toggle stressor breakdown
                 observeEvent(input$toggle_stressor, {
                   shinyjs::toggle("stressor_breakdown")
                   shinyjs::toggleClass(id = "icon_stressor", class = "rotated")
                 })


                 # ---------------------------------------------
                 # Generate plot for all life stages here (& lambda)...
                 # ---------------------------------------------
                 output$scenario_boxplots <- renderPlotly({

                   print("render scenario_boxplots...")

                   jm_data <-
                     session$userData$rv_joe_model_results_scenarios$sims

                   if(length(jm_data) == 0) {
                     print("Empty data...")
                     p <- ggplot(data.frame()) +
                       theme_minimal() +
                       labs(title = "No scenarios yet",
                            subtitle = "Run the Joe Model to create your first scenario")
                     return(ggplotly(p))
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
                       ggplot(m2, aes(x = scenario_name, y = CE, fill = scenario_name)) +
                       geom_boxplot(alpha = 0.7) +
                       theme_minimal() +
                       theme(legend.position = "none",
                             axis.text.x = element_text(angle = 45, hjust = 1)) +
                       labs(x = NULL, y = "Cumulative System Capacity (%)")
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
                         ggplot(m2, aes(x = scenario_name, y = CE_adj, fill = scenario_name)) +
                         geom_boxplot(alpha = 0.7) +
                         theme_minimal() +
                         theme(legend.position = "none",
                               axis.text.x = element_text(angle = 45, hjust = 1)) +
                         labs(x = NULL, y = y_lab)

                     } else {
                       # No matching stressor magntidue data
                       print("Empty data...")
                       p <- ggplot(data.frame())
                       return(p)
                     }
                   } # end of alt summary method

                   return(ggplotly(p))

                 })


                 output$scenario_stressor_dotplots <- renderPlotly({

                   print("render scenario_stressor_dotplots...")

                   jm_data <-
                     session$userData$rv_joe_model_results_scenarios$sims

                   if (length(jm_data) == 0) {
                     print("Empty data...")
                     p <- ggplot(data.frame()) +
                       theme_minimal() +
                       labs(title = "No data available")
                     return(ggplotly(p))
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
                       ggplot(m2,
                              aes(x = scenario_name,
                                  y = CE,
                                  color = Stressor,
                                  group = Stressor)) +
                       geom_point(size = 3) +
                       geom_line(alpha = 0.6) +
                       theme_minimal() +
                       theme(axis.text.x = element_text(angle = 45, hjust = 1),
                             legend.position = "bottom") +
                       labs(x = NULL, y = "Mean System Capacity (%)")

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
                                    color = Stressor,
                                    group = Stressor)) +
                         geom_point(size = 3) +
                         geom_line(alpha = 0.6) +
                         theme_minimal() +
                         theme(axis.text.x = element_text(angle = 45, hjust = 1),
                               legend.position = "bottom") +
                         labs(x = NULL, y = y_lab)

                     } else {
                       # No matching stressor magntidue data
                       print("Empty data...")
                       p <- ggplot(data.frame())
                       return(p)
                     }
                   } # end of alt summary method

                   return(ggplotly(p))

                 })


                 # ---------------------------------------------
                 # Clear scenario data...
                 # ---------------------------------------------
                 # Observe the action button
                 observeEvent(input$clear_button, {
                   # Joe model scenario name holder - assume multiple runs
                   session$userData$rv_joe_model_results_scenarios$sims <-
                     list()
                   # Close modal after clearing
                   removeModal()
                   showNotification("All scenarios cleared", type = "message")
                 })



               })
}
