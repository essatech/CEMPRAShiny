# module_dd_cap.R

# UI Function
module_matrix_dd_cap_ui <- function(id) {

  ns <- NS(id)
  tagList(
    tags$h4("Define Density Dependence Location Capacities"),

    tags$p(
      "This table specifies the ", tags$strong("maximum carrying capacity (K)"), " for each life stage
      at each location. When density-dependent bottlenecks are defined above (e.g., ", tags$code("bh_stage_1"), "),
      the model uses these K values to constrain population growth via Beverton-Holt or Hockey-Stick functions.",
      class = "pm-ht"
    ),

    tags$p(
      "You can edit capacity values directly in the table below (columns 3+), or upload a habitat capacities
      CSV file using the file uploader at the top of this page. The first two columns (HUC_ID and NAME) are locked.",
      class = "pm-ht"
    ),

    tags$details(
      tags$summary(
        style = "cursor: pointer; color: #337ab7; font-weight: bold; margin-top: 10px; margin-bottom: 10px;",
        "Learn more about creating the habitat capacities file..."
      ),
      tags$div(
        style = "padding: 10px; background-color: #f9f9f9; border-radius: 4px; margin-top: 10px; margin-bottom: 15px;",

        tags$h5("File Structure"),
        tags$p(
          "The habitat capacities file is a CSV with location identifiers in the first two columns and
          stage-specific carrying capacities in subsequent columns. Each row represents a location (e.g., a stream reach)."
        ),

        tags$h5("Required Columns", style = "margin-top: 15px;"),
        tags$table(
          class = "table table-sm table-bordered",
          style = "font-size: 12px; margin-bottom: 15px;",
          tags$thead(
            tags$tr(
              tags$th("Column Name", style = "width: 30%;"),
              tags$th("Description")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td(tags$code("HUC_ID")),
              tags$td("Unique location identifier (must match IDs in stressor magnitude data)")
            ),
            tags$tr(
              tags$td(tags$code("NAME")),
              tags$td("Human-readable location name (e.g., 'Reach 1', 'Upper Creek')")
            )
          )
        ),

        tags$h5("Capacity Columns (Non-Anadromous)", style = "margin-top: 15px;"),
        tags$table(
          class = "table table-sm table-bordered",
          style = "font-size: 12px; margin-bottom: 15px;",
          tags$thead(
            tags$tr(
              tags$th("Column Name", style = "width: 30%;"),
              tags$th("Description")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td(tags$code("k_stage_0_mean")),
              tags$td("Fry carrying capacity (max Age-0 individuals)")
            ),
            tags$tr(
              tags$td(tags$code("k_stage_1_mean")),
              tags$td("Stage 1 capacity (e.g., parr)")
            ),
            tags$tr(
              tags$td(tags$code("k_stage_2_mean"), ", ", tags$code("k_stage_3_mean"), ", ..."),
              tags$td("Subsequent stage capacities as needed")
            ),
            tags$tr(
              tags$td(tags$code("k_spawners_mean")),
              tags$td("Total spawner capacity (pooled across mature age classes)")
            )
          )
        ),

        tags$h5("Capacity Columns (Anadromous)", style = "margin-top: 15px;"),
        tags$p("For anadromous species, use 'Pb' (pre-breeder) or 'B' (breeder) suffixes:"),
        tags$table(
          class = "table table-sm table-bordered",
          style = "font-size: 12px; margin-bottom: 15px;",
          tags$thead(
            tags$tr(
              tags$th("Column Name", style = "width: 30%;"),
              tags$th("Description")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td(tags$code("k_stage_0_mean")),
              tags$td("Fry carrying capacity (no Pb/B suffix needed for fry)")
            ),
            tags$tr(
              tags$td(tags$code("k_stage_Pb_1_mean")),
              tags$td("Pre-breeder stage 1 capacity (e.g., parr/smolt)")
            ),
            tags$tr(
              tags$td(tags$code("k_stage_Pb_2_mean"), ", ..."),
              tags$td("Pre-breeder stages for older juvenile/sub-adult classes")
            ),
            tags$tr(
              tags$td(tags$code("k_spawners_mean")),
              tags$td("Total spawner capacity (pooled across spawner age classes)")
            )
          )
        ),

        tags$div(
          style = "background-color: #d1ecf1; border: 1px solid #bee5eb; border-radius: 4px; padding: 10px; margin-top: 15px;",
          tags$strong("Tip: "),
          "Only include columns for life stages with density-dependent constraints defined in the
          bottlenecks section above. Columns for stages without corresponding ",
          tags$code("bh_"), " or ", tags$code("hs_"), " flags will be ignored by the model."
        ),

        tags$p(
          style = "margin-top: 15px;",
          tags$a(href = "https://mattjbayly.github.io/CEMPRA_documentation/07_life_cycle_model.html#location-and-stage-specific-carrying-capacities",
                 target = "_blank", "See full documentation with examples and estimation methods...")
        )
      )
    ),

    fluidRow(
      column(
        width = 12,
        tags$p(
          style = "color: #6c757d; font-size: 12px; margin-top: 10px;",
          tags$em("Note: Edits made in the table below will update the underlying data automatically.")
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        tags$div(
          style = "overflow-x: auto; max-width: 100%;",
          rHandsontableOutput(ns("dd_cap_table"))
        )
      )
    ),
    fluidRow(
      column(12, downloadButton(ns("download_csv"), "Download Habitat Capacities as a csv file"))
    ),
    tags$br()
  )
}

# Server Function
module_matrix_dd_cap_server <- function(id, anadromous, Nstage) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Ensure session$userData$rv_hab_densities exists.
    if (is.null(session$userData$rv_hab_densities)) {
      session$userData$rv_hab_densities <- reactiveValues(
        # Example structure; adjust columns as needed.
        dat = data.frame(
          HUC_ID = character(0),    # locked column 1
          NAME = character(0),      # locked column 2
          stringsAsFactors = FALSE
        )
      )
    }

    # Render the rHandsontable.
    output$dd_cap_table <- renderRHandsontable({
      df <- session$userData$rv_hab_densities$dat

      # If no rows exist, show an empty placeholder table.
      if (is.null(df) || nrow(df) == 0) {
        empty_df <- data.frame(
          HUC_ID = character(0),
          NAME = character(0),
          stringsAsFactors = FALSE
        )
        return(
          rhandsontable(empty_df, rowHeaders = FALSE, stretchH = "none") %>%
            hot_cols(colWidths = 150)
        )
      }

      # Determine number of columns
      ncols <- ncol(df)

      # Create the rhandsontable with horizontal scrolling
      hot <- rhandsontable(
        df,
        rowHeaders = FALSE,
        stretchH = "none",
        overflow = "visible",
        height = min(600, 30 + nrow(df) * 25)  # Dynamic height based on rows
      )

      # Set column widths - first two columns narrower, K columns wider
      col_widths <- c(100, 150, rep(120, max(0, ncols - 2)))
      hot <- hot %>% hot_cols(colWidths = col_widths)

      # Lock the first two columns (HUC_ID and NAME) - make them read-only
      if (ncols >= 1) {
        hot <- hot %>% hot_col(col = 1, readOnly = TRUE)
      }
      if (ncols >= 2) {
        hot <- hot %>% hot_col(col = 2, readOnly = TRUE)
      }

      # Set numeric format for K columns (columns 3+)
      if (ncols > 2) {
        for (i in 3:ncols) {
          hot <- hot %>% hot_col(col = i, format = "0,0")
        }
      }

      hot
    })

    # Update reactive data when the table is edited.
    observeEvent(input$dd_cap_table, {
      if (!is.null(input$dd_cap_table)) {
        new_data <- hot_to_r(input$dd_cap_table)
        # Only update if data actually changed to avoid infinite loops
        if (!identical(session$userData$rv_hab_densities$dat, new_data)) {
          session$userData$rv_hab_densities$dat <- new_data
        }
      }
    })
    
    
    output$download_csv <- downloadHandler(
      filename = function() { paste("dd_cap_table-", Sys.Date(), ".csv", sep="") },
      content = function(file) { write.csv(session$userData$rv_hab_densities$dat, file, row.names=FALSE) }
    )
    
    
  })
}
