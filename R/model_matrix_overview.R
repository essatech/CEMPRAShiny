# model_matrix_overview.R
model_matrix_overview_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$br(),

    # Scenario Results Section
    tags$h4("Compare Scenario Results"),

    tags$p(
      "Use this tab to compare population projections across different scenarios.
      Each scenario represents a unique combination of parameter settings and stressor magnitudes.",
      class = "pm-ht"
    ),

    tags$div(
      class = "alert alert-secondary",
      style = "margin-top: 15px;",
      tags$strong("How to use:"),
      tags$ol(
        style = "margin-bottom: 0; padding-left: 20px;",
        tags$li("Go to the ", tags$strong("Run Population Model"), " tab and configure your projection settings."),
        tags$li("Click ", tags$strong("Run Population Projection"), " to generate results."),
        tags$li("In the time series modal, click ", tags$strong("Save Scenario"), " to store results."),
        tags$li("Repeat with different parameter configurations to build comparison scenarios."),
        tags$li("Return here to view side-by-side comparisons of all saved scenarios.")
      )
    ),

    tags$p(
      "The first scenario saved is automatically named '01_Baseline' and serves as the reference point
      (shown with a red dotted line on the plot). Subsequent scenarios are numbered sequentially.",
      class = "pm-ht",
      style = "margin-top: 10px;"
    ),

    # Button to clear scenario data
    tags$div(
      style = "margin-top: 15px;",
      actionButton(ns("clearScenario"), "Clear scenario data", class = "btn-warning")
    ),

    tags$hr(),

    tags$h5("Summary Statistics", style = "text-align: center; margin-top: 20px;"),

    # Wrap the tableOutput in a div with center alignment
    div(style = "display: flex; justify-content: center; margin-top: 15px;",
        tableOutput(ns("scenarioSummary"))
    ),

    # P10/P90 explanation
    tags$div(
      style = "text-align: center; margin-top: 10px; margin-bottom: 20px;",
      tags$small(
        class = "pm-ht",
        tags$strong("P10"), " = 10th percentile (90% of simulations exceeded this value); ",
        tags$strong("P90"), " = 90th percentile (10% of simulations exceeded this value). ",
        "These values represent the range within which 80% of simulation results fall."
      )
    ),

    tags$hr(),

    tags$h5("Scenario Comparison Plot", style = "text-align: center; margin-top: 20px;"),

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
            label = "No scenario data saved yet.\nRun projections from the 'Run Model' tab\nand save scenarios to compare.",
            size = 5,
            hjust = 0.5,
            color = "gray50"
          ) +
          xlim(0, 1) +
          ylim(0, 1) +
          theme_void() +
          theme(
            panel.border = element_rect(color = "gray80", fill = NA, linewidth = 1)
          )
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
      
      # Create the boxplot with colored scenarios.
      # The geom_boxplot uses outlier.shape = NA so that outlier markers are not displayed.
      p <- ggplot(scenario_data, aes(x = scenario_name, y = N, fill = scenario_name)) +

        geom_boxplot(
          width = 0.5,
          outlier.shape = NA,
          alpha = 0.7
        ) +
        labs(x = "Scenario", y = "N Individuals") +
        scale_fill_brewer(palette = "Set2") +
        theme_bw() +
        theme(
          panel.grid.major = element_line(color = "gray80", linewidth = 0.5),
          panel.grid.minor = element_line(color = "gray90", linewidth = 0.25),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none"
        )

      # If "01_Baseline" exists, add a horizontal dotted line at its median.
      if ("01_Baseline" %in% scenario_data$scenario_name) {
        p <- p + geom_hline(
          yintercept = baseline_median,
          linetype = "dotted",
          color = "red",
          linewidth = 1
        )
      }

      ggplotly(p) %>%
        layout(
          yaxis = list(
            gridcolor = "gray80",
            gridwidth = 1,
            showgrid = TRUE
          ),
          xaxis = list(
            gridcolor = "gray80",
            gridwidth = 1,
            showgrid = TRUE
          )
        )
    })
    
    # Render the summary table below the plot.
    output$scenarioSummary <- renderTable({
      # Retrieve scenario data
      scenario_data <- session$userData$rv_pop_mod_scenarios$dat
      
      if (is.null(scenario_data) || nrow(scenario_data) == 0) {
        return(NULL)
      }
      
      scenario_data <- as.data.frame(scenario_data)
      
      # Compute summary statistics for each scenario.
      # P10 and P90 represent the 10th and 90th percentiles of simulation results.
      scenario_summary <- scenario_data %>%
        group_by(scenario_name) %>%
        summarize(
          Median = round(median(N, na.rm = TRUE), 0),
          Mean = round(mean(N, na.rm = TRUE), 0),
          P10 = round(quantile(N, 0.10, na.rm = TRUE), 0),
          P90 = round(quantile(N, 0.90, na.rm = TRUE), 0),
          Min = round(min(N, na.rm = TRUE), 0),
          Max = round(max(N, na.rm = TRUE), 0)) %>%
        rename(Scenario = scenario_name)
      
      
      # Optionally, force "01_Baseline" to appear first.
      if ("01_Baseline" %in% scenario_summary$Scenario) {
        new_levels <- c("01_Baseline", sort(
          setdiff(scenario_summary$Scenario, "01_Baseline")
        ))
        scenario_summary$Scenario <- factor(scenario_summary$Scenario, levels = new_levels)
        scenario_summary <- arrange(scenario_summary, Scenario)
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