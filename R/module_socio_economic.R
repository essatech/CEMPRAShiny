#' Socio-Economic UI
module_socio_economic_ui <- function(id) {
  ns <- NS(id)
  
  tagList(shinydashboard::box(
    width = 12,
    tags$h3("Socio-economic Evaluation"),
    tags$div(
      tags$a(
        href = "https://mattjbayly.github.io/CEMPRA_documentation/07_life_cycle_model.html",
        "See guidance document help section: Socio-economic Evaluation",
        target =
          "_blank"
      )
    )
  ),
  fluidRow(
    column(
      width = 9,
      shinydashboard::box(
        width = 12,
        tags$h3("Socio-economic Evaluation of Restoration Actions"),
        
        fluidRow(column(
          width = 6,
          accordion(
            id = "accordion5",
            accordionItem(title = "Step 1: Upload SE Input Workbook",
                          collapsed = TRUE,
                          tagList(
                            tags$p(
                              "The socio-economic component ultimately attempts to provide a high-level cost-benefit analysis of restoration alternatives, and is designed to facilitate decision-making by quantifying the economic implications of competing restoration strategies"
                            )
                          ))
          ),
          module_import_se_workbook_ui(ns("module_import_se_workbook_se")),
        ),
        column(
          width = 6,
          accordion(
            id = "accordion6",
            accordionItem(title = "Step 2: Run the Joe Model",
                          collapsed = TRUE,
                          tagList(
                            tags$p(
                              "The socio-economic component ultimately attempts to provide a high-level cost-benefit analysis of restoration alternatives, and is designed to facilitate decision-making by quantifying the economic implications of competing restoration strategies"
                            )
                          ))
          ),
          tags$b("Re-Run the Joe Model"),
          # Single action button to call modal
          module_joe_model_run_ui(ns("run_joe_model_se"))
        )),
        
        
        
        fluidRow(
          column(
            width = 12,
            
            tags$h3("Cost-Benefit Analysis"),
            
            
            tags$p(
              "Each time the Joe Model is run, a new “scenario” is created. A scenario can consist of changes to the stressor magnitude data, changes to the stressor-response data, and/or changes to the socio-economic inputs. Continue to upload new datasets and re-run the Joe Model multiple times to create several different restoration scenarios, then make comparisons between scenarios using the plots below for a simple socio-economic assessment.", class = "pm-ht"
            ),
            tags$p(
              "Ensure that each scenario is given a unique name in the Joe Model pop-up. If the name of the scenario is not updated, the scenario will be assigned the name of “Default” and data from the previous run will be overwritten.", class = "pm-ht"
            ),
            
            # Action button
            fluidRow(column(
              width = 12,
              tagList(
                actionButton(
                  ns("clear_button"),
                  "(Optionally) clear all cached data from previous scenarios to start from scratch"
                ),
                tags$br(),
              )
            )),
            
            tags$br(),
            tags$br(),
            
            # Weightings
            fluidRow(column(
              width = 12,
              
              tags$p(
                "The first group of boxplots shows the cumulative system capacity scores across locations. Each column of the boxplot represents a unique scenario. Click the clear button above to delete data and start from scratch (as needed). Use the dropdown select menu (below) to adjust how cumulative system capacity scores should be weighted across locations. The Joe Model assumes that all locations should be weighted equally (by default); however, weighting can be adjusted to other numeric attribute fields in the Locations GIS shapefile.", class = "pm-ht"
              ),
              
              selectInput(
                ns("location_weighting"),
                "Weight Location Scores By:",
                c(
                  "Equal Weight Per Location" = "equal",
                  "By Area (geometry)" = "area",
                  "By Length (geometry)" = "length",
                  "User Attribute: Rearing_area" = "area1",
                  "User Attribute: Spawning_area" = "area2",
                  "Gears" = "gear"
                )
              ),
            )),
            
            fluidRow(column(plotlyOutput(
              ns("scenario_boxplots")
            ), width = 12)),
            
            tags$br(),
            tags$br(),
            
            fluidRow(column(
              
              tags$p(
                "The next group of boxplots shows the total cost estimate to implement each scenario. The cost estimate is presented as a distribution of values in a boxplot to represent uncertainty and stochasticity. Each data point represents derived cost estimates from an individual batch replicate model run.", class = "pm-ht"
              ),
              
              
              plotlyOutput(
              ns("scenario_cost_boxplots")
            ), width = 12)),
            
            tags$br(),
            tags$br(),
            
            fluidRow(column(
              tags$p(
                "The following plot shows a simplified cost-benefit analysis with the total cost on the x-axis and the cumulative system capacity score on the y-axis. Each data point represents an individual batch replicate model run. It can be helpful to run a default baseline status quo scenario first and then click and drag in the plot area to zoom in for all subsequent scenarios – this allows us to see the relative change over a predefined baseline status quo baseline.", class = "pm-ht"
              ),
              
              plotlyOutput(
                ns("scenario_cost_benefit_plot")
              ),
              
              width = 12
            )),
            
            
            tags$br(),
            tags$br(),
            
            fluidRow(column(
              
              tags$h3("Diagnositics"),
              
              tags$p(
                "The next set of plots shows each stressor's average system capacity score across each scenario. Only the mean value is shown (across locations) for convenience rather than the full distribution of each stressor. However, reviewing the plot below is useful for understanding the most critical and least critical stressors across scenarios.", class = "pm-ht"
              ),
              width = 12
            )),
            fluidRow(column(plotlyOutput(
              ns("scenario_stressor_dotplots")
            ),
            width = 12)),
            
            tags$br(),
            tags$br(),
            
            fluidRow(column(
              
              tags$h3("Restoration Projects: Location Implementation"),
              
              tags$p(
                "Review individual line items (restoration projects) from the Location Implementation worksheet. Click the button beside each row to visualize relationships and modify the level of effort. After any edits are completed, re-run the Joe Model to update above plots.", class = "pm-ht"
              ),
              width = 12
            )),
            
            fluidRow(column(
              module_se_table_ui(ns("se_table")),
              # Call the table server function
            width = 12)),
            
            
            
          )
        )
        
        
        
        
        
      ),
    ),
    column(width = 3,
           
           shinydashboard::box(
             width = 12,
             
             tags$h4("Instructions"),
             tags$p("See YouTube Video tutorials for:"),
             tags$a("Video Tutorial 1: SE Module Overview", href = "https://www.youtube.com/watch?v="),
             tags$br(),
             tags$a("Video Tutorial 2: SE Data Inputs", href = "https://www.youtube.com/watch?v="),
             tags$br(),
             tags$br(),
             tags$b("Experiment with Example Datasets:"),
             tags$br(),
             tags$ul(
                     tags$li(tags$a(href = "/demo/se_stressor_response.xlsx", 
                                    download = "/demo/se_stressor_response.xlsx",
                                    "Demo Stressor-Response Input Workbook")),
                     tags$li(tags$a(href = "/demo/se_stressor_magnitude.xlsx", 
                                    download = "/demo/se_stressor_magnitude.xlsx",
                                    "Demo Stressor-Magnitude Input Workbook")),
                     tags$li(tags$a(href = "/demo/se_locations.gpkg", 
                                    download = "/demo/se_locations.gpkg",
                                    "Demo Locations GIS")),
                     tags$li(tags$a(href = "/demo/socio_economic_input.xlsx", 
                                    download = "/demo/socio_economic_input.xlsx",
                                    "Demo Socio-economic Input Workbook"))
             ),
             
             tags$br(),
             tags$br(),
             tags$b("The general workflow is as follows:"),
             tags$ol(class = "pm-ht",
               tags$li("Clear the cache of data from previous runs by clicking the grey clear data button (to the right)."),
               tags$li("Navigate to the Upload Data tab."),
               tags$li("Upload a stressor response file, a stressor magnitude file and a socio-economic input file (see examples above)."),
               tags$li("The Data Upload module checks input datasets for potential issues. Pay special attention to any red error messages (if they appear), read error message carefully, fix input workbooks, and re-upload until all the error messages disappear."),
               tags$li("Navigate back to the Socio-Economic tab and click the red button to run the Joe Model."),
               tags$li("Select stressors and iterations and then give the scenario a unique name like “default”, “baseline”, or “status-quo”, let the model run and review the results on this page."),
               tags$li("When ready, re-run the Joe Model, but this time assigns a new name for the scenario, such as “Restoration”, then click the checkbox “Run with Socio-Economic Inputs” and click the “Run the Joe Model” to run the model with the socio-economic inputs applied to the calculations."),
               tags$li("If the data inputs are set up appropriately,  the model will run and apply the restoration actions"),
               tags$li("Repeat this process as many times as necessary to create multiple restoration scenarios for comparison."),
               tags$li("Be creative and try uploading different stressor management files to simulate the effects of restoration with future climate change etc. It is also possible that there may be multiple restoration action workbooks to represent different restoration portfolios."),
               tags$li("Review the diagnostic plots to help inform restoration planning."),
               tags$li("Use the Restoration Projects: Location Implementation tabs to explore a specific project (see blue buttons on the table at the bottom of this page)."),
               tags$li("Export and save results as needed via screen shot or data download module.")
             )
           ))
  ))
}


#' Server Function
module_socio_economic_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 
                 print("module_socio_economic_server...")
                 
                 # Call the server function to run the Joe Model
                 # Run from within SE module
                 module_joe_model_run_server("run_joe_model_se") 
                 module_import_se_workbook_server("module_import_se_workbook_se")
                 
                 # ---------------------------------------------
                 # Clear scenario data...
                 # ---------------------------------------------
                 # Observe the action button
                 observeEvent(input$clear_button, {
                   # Joe model scenario name holder - assume multiple runs
                   session$userData$rv_joe_model_results_scenarios$sims <-
                     list()
                 })
                 
                 
                 # ---------------------------------------------
                 # Render scenario box plot for csc
                 # ---------------------------------------------
                 output$scenario_boxplots <- renderPlotly({
                   
                   print("render scneario boxplots...")
                   
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
                     # CSC Data
                     csc_dat <- x$ce.df
                     n_hucs <- length(unique(csc_dat$HUC))
                     csc_dat$replicate <- rep(1:(nrow(csc_dat)/n_hucs), n_hucs)
                     # Summarize by replicate
                     csc_sum <- csc_dat %>% 
                       group_by(scenario_name, replicate) %>% 
                       summarise(csc = mean(CE, na.rm = TRUE))
                     return(csc_sum)
                   }
                   
                   all_dat <- lapply(jm_data, gather_ce)
                   all_dat <- do.call("rbind", all_dat)
                   all_dat$scenario_name <-
                     as.character(all_dat$scenario_name)
                   all_dat$CE <-
                     round(as.numeric(all_dat$csc * 100), 2)
                   
                   # Create the plot
                   p <-
                     ggplot(all_dat, aes(x = scenario_name, y = CE)) +
                     geom_boxplot() +
                     ggtitle("Cumulative System Capacity by Scenario") +
                     xlab("Scenario Names") + ylab("Cumulative System Capacity (%)") +
                     theme_minimal()
                   
                   return(p)
                   
                 })
                 
                 
                 
                 
                 
                 # ---------------------------------------------
                 # Render scenario box plot for costs
                 # ---------------------------------------------
                 output$scenario_cost_boxplots <- renderPlotly({
                   
                   print("render cost boxplots...")
                   
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
                     scenario_name <- x$ce.df$scenario_name[1]
                     t_dat <- x$socioeconomic_inputs$cost_summary
                     if (is.null(t_dat)) {
                       t_dat <- data.frame(
                         action = NA,
                         id = NA,
                         total_cost = 0,
                         replicate = NA
                       )
                       #return(NULL)
                     }
                     t_dat$scenario_name <- scenario_name
                     return(t_dat)
                   }
                   
                   all_dat <- lapply(jm_data, gather_ce)
                   all_dat <- do.call("rbind", all_dat)
                   
                   # Joe Model has been run, but not yet with SE
                   if (length(all_dat) == 0) {
                     print("Empty data...")
                     p <- ggplot(data.frame())
                     return(p)
                   }
                   
                   # Need to group by scenario_name and replicate
                   # to summarize total costs
                   ad2 <- all_dat %>% 
                     group_by(scenario_name, replicate) %>% 
                     summarise(scenario_cost = sum(total_cost))
                   
                   ad2$scenario_name <-
                     as.character(ad2$scenario_name)
                   ad2$Cost <-
                     round(as.numeric(ad2$scenario_cost), 2)
                   
                   # Show in units of thousands or millions
                   # depending on magnitude of Cost values
                   if (max(ad2$Cost) >= 100000 & max(ad2$Cost) < 3000000) {
                     ad2$Cost <- ad2$Cost / 1000
                     mylab <- "Total Cost (Thousands $)"
                   } else if (max(ad2$Cost) >= 3000000) {
                     ad2$Cost <- ad2$Cost / 1000000
                     mylab <- "Total Cost (Millions $)"
                   } else {
                     mylab <- "Total Cost ($)"
                   }
                   
                   
                   # Create the plot
                   p <-
                     ggplot(ad2, aes(x = scenario_name, y = Cost)) +
                     geom_boxplot() +
                     scale_y_continuous(labels = scales::comma) +
                     ggtitle("Cost Summary by Scenario") +
                     xlab("Scenario Names") + ylab(mylab) +
                     theme_minimal()
                   
                   return(p)
                   
                 })
                 
                 
                 
                 
                 
                 # ---------------------------------------------
                 # Render Cost Benefit Scatter plot
                 # ---------------------------------------------
                 output$scenario_cost_benefit_plot <- renderPlotly({
                   
                   print("render cost benefit scatterplot...")
                   
                   jm_data <-
                     session$userData$rv_joe_model_results_scenarios$sims
                   
                   # No data, blank plot
                   if (length(jm_data) == 0) {
                     print("Empty data...")
                     p <- ggplot(data.frame())
                     return(p)
                   }
                   
                   # Get data for current run (from above)
                   # Get the ce.df slot from element in the jm_data list
                   gather_ce <- function(x) {
                     
                     scenario_name <- x$ce.df$scenario_name[1]
                     
                     # CSC Data
                     csc_dat <- x$ce.df
                     n_hucs <- length(unique(csc_dat$HUC))
                     csc_dat$replicate <- rep(1:(nrow(csc_dat)/n_hucs), n_hucs)
                     # Summarize by replicate
                     csc_sum <- csc_dat %>% 
                       group_by(replicate) %>% 
                       summarise(csc = mean(CE, na.rm = TRUE))
                     
                     t_dat <- x$socioeconomic_inputs$cost_summary
                     
                     if (is.null(t_dat)) {
                       csum2 <- data.frame(
                         replicate = NA,
                         csc = csc_sum$csc,
                         total_cost = 0,
                         scenario_name = scenario_name
                       )
                       return(csum2)
                     }
                     
                     # Summarize total cost by replicate
                     cost_sum <- t_dat %>% 
                       group_by(replicate) %>% 
                       summarise(total_cost = sum(total_cost, na.rm = TRUE))
                     
                     # Cost and CSC
                     ret_obj <- merge(csc_sum, cost_sum, by = "replicate", all.x = TRUE, all.y = FALSE)
                     ret_obj$scenario_name <- scenario_name
                     return(ret_obj)
                   }
                   
                   all_dat <- lapply(jm_data, gather_ce)
                   all_dat <- do.call("rbind", all_dat)
                   
                   # Joe Model has been run, but not yet with SE
                   if (length(all_dat) == 0) {
                     print("Empty data...")
                     p <- ggplot(data.frame())
                     return(p)
                   }
                   
                   all_dat$scenario_name <-
                     as.character(all_dat$scenario_name)
                   all_dat$Cost <-
                     round(as.numeric(all_dat$total_cost), 2)
                   
                   # Show in units of thousands or millions
                   # depending on magnitude of Cost values
                   if (max(all_dat$Cost) >= 100000 & max(all_dat$Cost) < 3000000) {
                     all_dat$Cost <- all_dat$Cost / 1000
                     mylab <- "Total Cost (Thousands $)"
                   } else if (max(all_dat$Cost) >= 3000000) {
                     all_dat$Cost <- all_dat$Cost / 1000000
                     mylab <- "Total Cost (Millions $)"
                   } else {
                     mylab <- "Total Cost ($)"
                   }
                   
                   all_dat$CSC <- round(as.numeric(all_dat$csc * 100), 2)
                   all_dat$csc <- NULL
                   
                   # Create the plot
                   p <-
                     ggplot(all_dat, aes(x = Cost, y = CSC, color = scenario_name)) +
                     geom_point() +
                     scale_x_continuous(labels = scales::comma) +
                     ggtitle("Simple Cost Benefit") +
                     labs(color='Scenario') + 
                     ylab("Cumulative System Capacity (%)") + xlab(mylab) +
                     theme_minimal()
                   
                   return(p)
                   
                 })
                 
                 
                 
                 # ---------------------------------------------
                 # Render scenario dot plot
                 # ---------------------------------------------
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
                     round(as.numeric(all_dat$m.sys.cap * 100), 3)
                   
                   p <-
                     ggplot(all_dat,
                            aes(x = scenario_name,
                                y = m.sys.cap,
                                color = Stressor,
                                group = Stressor)) +
                     geom_point() +
                     geom_line() +
                     #geom_dotplot(binaxis = 'y', stackdir = 'center') +
                     ggtitle("System Capacity by Stressor by Scenario") +
                     xlab("Scenario Names") + ylab("Mean System Capacity (%)") +
                     theme_minimal() +
                     theme(legend.position = "bottom")
                   
                   return(p)
                   
                 })
                 
                 
                 
                 # Load the dynamic table to visualize
                 # individual projects
                 module_se_table_server("se_table")
                 
                 
                 
                 
                 
               })
}