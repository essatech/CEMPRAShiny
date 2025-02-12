# model_matrix_overview.R
model_matrix_overview_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$br(),
    
    tags$h4("Overview"),
    
    tags$p(
      "Navigate through the following tab panels (blue text above) to adjust the survival, growth, reproduction, and density dependant constraints on the life cycle model. Life cycle model vital rate files can be edited here, exported (on the Download Data tab), and then re-uploaded in the file upload inputs (at the top of this page). Make sure the number of life stages is correct for your target species. Also, determine whether the life cycle should follow an anadromous life history (e.g., for salmon) or non-anadromous life history (e.g., for trout and other organisms that can reproduce more than once in their life cycle)."
    ),
    
    tags$h4("Scenario Results"),
    
    # Button to clear scenario data
    actionButton(ns("clearScenario"), "Clear scenario data"),
    
    # Wrap the tableOutput in a div with center alignment
    div(style = "text-align: center;",
        tableOutput(ns("scenarioSummary"))
    ),
    
    tags$br(),
    
    # The interactive plot output
    plotlyOutput(ns("scenarioPlot")),
    

    
    
    # tags$table(
    #   class = "table table-bordered",
    #   tags$thead(
    #     tags$tr(
    #       tags$th("Species/System"),
    #       tags$th("Life Cycle Profiles (csv)"),
    #       tags$th("Habitat Capacities (csv)"),
    #       tags$th("Stressor-Response (xlsx)"),
    #       tags$th("Stressor-Magnitude (xlsx)"),
    #       tags$th("Locations (gpkg)")
    #     )
    #   ),
    #   tags$tbody(
    #     # Row 1: Athabasca Rainbow Trout
    #     tags$tr(
    #       tags$td("Athabasca Rainbow Trout"),
    #       tags$td(a("Download", href = "./data/abrt/life cycles.csv", download = "life cycles.csv")),
    #       tags$td(a("Download", href = "./data/abrt/habitat_capacities.csv", download = "")),
    #       tags$td(a("Download", href = "./data/abrt/stressor_response_demo.xlsx", download = "")),
    #       tags$td(a("Download", href = "./data/abrt/stressor_magnitude_demo.xlsx", download = "stressor_magnitude_demo.xlsx")),
    #       tags$td(a("Download", href = "./data/abrt/watersheds.gpkg", download = ""))
    #     ),
    #     # Row 2: Boreal Whitefish
    #     tags$tr(
    #       tags$td("Boreal Whitefish"),
    #       tags$td(a("Download", href = "./data/bwfish/life cycles.csv", download = "")),
    #       tags$td(a("Download", href = "./data/bwfish/habitat_capacities.csv", download = "")),
    #       tags$td(a("Download", href = "./data/bwfish/stressor_response_demo.xlsx", download = "")),
    #       tags$td(a("Download", href = "./data/bwfish/stressor_magnitude_demo.xlsx", download = "")),
    #       tags$td(a("Download", href = "./data/bwfish/watersheds.gpkg", download = ""))
    #     ),
    #     # Row 3: Snake River Salmon
    #     tags$tr(
    #       tags$td("Snake River Salmon"),
    #       tags$td(a("Download", href = "./data/snake/life cycles.csv", download = "")),
    #       tags$td(a("Download", href = "./data/snake/habitat_capacities.csv", download = "")),
    #       tags$td(a("Download", href = "./data/snake/stressor_response_demo.xlsx", download = "")),
    #       tags$td(a("Download", href = "./data/snake/stressor_magnitude_demo.xlsx", download = "")),
    #       tags$td(a("Download", href = "./data/snake/watersheds.gpkg", download = ""))
    #     ),
    #     # Row 4: Rocky Mountain Trout
    #     tags$tr(
    #       tags$td("Rocky Mountain Trout"),
    #       tags$td(a("Download", href = "./data/rmt/life cycles.csv", download = "")),
    #       tags$td(a("Download", href = "./data/rmt/habitat_capacities.csv", download = "")),
    #       tags$td(a("Download", href = "./data/rmt/stressor_response_demo.xlsx", download = "")),
    #       tags$td(a("Download", href = "./data/rmt/stressor_magnitude_demo.xlsx", download = "")),
    #       tags$td(a("Download", href = "./data/rmt/watersheds.gpkg", download = ""))
    #     ),
    #     # Row 5: Pacific Lamprey
    #     tags$tr(
    #       tags$td("Pacific Lamprey"),
    #       tags$td(a("Download", href = "./data/plat/life cycles.csv", download = "")),
    #       tags$td(a("Download", href = "./data/plat/habitat_capacities.csv", download = "")),
    #       tags$td(a("Download", href = "./data/plat/stressor_response_demo.xlsx", download = "")),
    #       tags$td(a("Download", href = "./data/plat/stressor_magnitude_demo.xlsx", download = "")),
    #       tags$td(a("Download", href = "./data/plat/watersheds.gpkg", download = ""))
    #     )
    #   )
    # ),
    
    
    
    
    
  )
}


model_matrix_overview_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # output$rv_preview <- renderTable({
    #   session$userData$rv_life_stages$dat
    # })
    
    
    
    # Render the interactive violin plot.
    output$scenarioPlot <- renderPlotly({
      # Retrieve scenario data stored in session$userData$rv_pop_mod_scenarios$dat
      scenario_data <- session$userData$rv_pop_mod_scenarios$dat
      if (is.null(scenario_data) || nrow(scenario_data) == 0) {
        p <- ggplot() +
          annotate(
            "text",
            x = 0.5,
            y = 0.5,
            label = "No scenario data",
            size = 6,
            hjust = 0.5
          ) +
          theme_void()
        return(ggplotly(p))
      }
      
      scenario_data <- as.data.frame(scenario_data)
      
      # If "01_Baseline" is present, force it to be the first factor level.
      if ("01_Baseline" %in% scenario_data$scenario_name) {
        baseline_median <- median(scenario_data$N[scenario_data$scenario_name == "01_Baseline"], na.rm = TRUE)
        new_levels <- c("01_Baseline", sort(setdiff(
          unique(scenario_data$scenario_name), "01_Baseline"
        )))
        scenario_data$scenario_name <- factor(scenario_data$scenario_name, levels = new_levels)
      } else {
        scenario_data$scenario_name <- factor(scenario_data$scenario_name)
      }
      
      # Create the violin plot.
      # The geom_boxplot uses outlier.shape = NA so that outlier markers are not displayed.
      p <- ggplot(scenario_data, aes(x = scenario_name, y = N)) +
        
        # geom_violin(trim = FALSE,
        #             fill = "skyblue",
        #             color = "black") +
        
        geom_boxplot(
          width = 0.1,
          fill = "white",
          outlier.shape = NA
        ) +
        labs(x = "Scenario", y = "N") +
        theme_minimal()
      
      # If "01_Baseline" exists, add a horizontal dotted line at its median.
      if ("01_Baseline" %in% scenario_data$scenario_name) {
        p <- p + geom_hline(
          yintercept = baseline_median,
          linetype = "dotted",
          color = "red"
        )
      }
      
      ggplotly(p)
    })
    
    # Render the summary table below the plot.
    output$scenarioSummary <- renderTable({
      # Retrieve scenario data
      scenario_data <- session$userData$rv_pop_mod_scenarios$dat
      
      if (is.null(scenario_data) || nrow(scenario_data) == 0) {
        return(NULL)
      }
      
      scenario_data <- as.data.frame(scenario_data)
      
      # Compute summary statistics for each scenario after excluding outliers.
      # Outliers are defined as values outside [Q1 - 1.5*IQR, Q3 + 1.5*IQR].
      # We then compute the median, 10th, and 90th percentiles of the filtered data.
      scenario_summary <- scenario_data %>%
        group_by(scenario_name) %>%
        summarize(
          Median = round(median(N, na.rm = TRUE), 0),
          Mean = round(mean(N, na.rm = TRUE), 0),
          p10 = round(quantile(N, 0.10, na.rm = TRUE), 0),
          p90 = round(quantile(N, 0.90, na.rm = TRUE), 0),
          Min = round(min(N, 0.10, na.rm = TRUE), 0),
          Max = round(max(N, 0.90, na.rm = TRUE), 0))
      
      
      # Optionally, force "01_Baseline" to appear first.
      if ("01_Baseline" %in% scenario_summary$scenario_name) {
        new_levels <- c("01_Baseline", sort(
          setdiff(scenario_summary$scenario_name, "01_Baseline")
        ))
        scenario_summary$scenario_name <- factor(scenario_summary$scenario_name, levels = new_levels)
        scenario_summary <- arrange(scenario_summary, scenario_name)
      }
      
      scenario_summary
    }, digits = 0)
    
    
    
    # Observer to clear the scenario data when the button is pressed.
    observeEvent(input$clearScenario, {
      # Reset the data frame; you can also choose to set it to an empty data.frame.
      session$userData$rv_pop_mod_scenarios$dat <- data.frame(
        scenario_name = character(),
        N = numeric(),
        stringsAsFactors = FALSE
      )
      showNotification("Scenario data cleared.", type = "message")
    })
    
    
    
  })
}