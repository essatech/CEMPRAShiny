#' Matrix Model LTRE UI
module_matrix_model_ltre_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$b("Sensitivity Analysis", style = "text-align: center;"),
    tags$div(
      class = "lam_bb",
      tags$p(
        "Run a global sensitivity analysis using Monte Carlo sampling and PRCC-based parameter rankings to identify which demographic parameters most influence population growth rate (lambda)."
      ),
      actionButton(ns("open_ltre_modal"), "LTRE Range Analysis")
    )
  )
}


#' Matrix Model LTRE Server Component
module_matrix_model_ltre_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    print("module_matrix_model_ltre_server...")

    # Reactive state
    ltre_table_data   <- reactiveVal(NULL)   # Visible/editable rows only
    ltre_hidden_data  <- reactiveVal(NULL)   # Hidden structural rows (Nstage, k, etc.)
    ltre_result       <- reactiveVal(NULL)
    stored_n_samples  <- reactiveVal(1000)

    # ----------------------------------------------------------
    # Helper: build the modal dialog
    # ----------------------------------------------------------
    build_modal <- function() {

      results_ui <- NULL
      res <- ltre_result()

      if (!is.null(res)) {
        results_ui <- tagList(
          tags$hr(),
          tags$h4("Lambda Distribution"),
          textOutput(ns("ltre_lambda_summary")),
          plotOutput(ns("ltre_lambda_hist"), height = "350px"),
          tags$h4("Parameter Sensitivity Rankings"),
          tags$p(
            "The bar chart below ranks parameters by their Partial Rank Correlation",
            "Coefficient (PRCC) with lambda. PRCC measures the strength of the monotonic",
            "relationship between each parameter and lambda after removing the linear",
            "effects of all other parameters, making it a robust measure of global sensitivity.",
            "Parameters with higher absolute PRCC values have a stronger independent influence",
            "on population growth rate. Blue bars indicate statistically significant correlations",
            "(p < 0.01); coral bars indicate non-significant correlations, which may still be",
            "meaningful but should be interpreted with caution."
          ),
          plotOutput(ns("ltre_prcc_barplot"), height = "450px"),
          tags$h4("Top 10 Parameters"),
          DT::DTOutput(ns("ltre_prcc_table"))
        )
      }

      modalDialog(
        title = "LTRE Range Analysis",
        size = "l",
        easyClose = TRUE,
        footer = NULL,

        tagList(
          tags$p(
            "A Life Table Response Experiment (LTRE) identifies which demographic parameters",
            "have the greatest influence on population growth rate (lambda). This analysis uses",
            "Monte Carlo sampling from user-defined distributions and Partial Rank Correlation",
            "Coefficients (PRCC) for global sensitivity analysis."
          ),
          tags$p(
            "Edit the table below to specify which parameters to test. Set",
            tags$b("Test_Parameter"), "to TRUE for parameters you want to include in the",
            "analysis, then specify a Distribution type (normal, beta, or pert), the SD",
            "(for normal/beta), and the Low_Limit and Up_Limit bounds for sampling."
          ),
          tags$p(
            "Parameters left as FALSE will be held at their current Value. At least one",
            "parameter must be set to TRUE before running the analysis."
          ),
          tags$p(
            "For more advanced usage and scripting, this analysis can also be run directly",
            "in R using", tags$code("CEMPRA::pop_model_ltre_range()"), ".",
            "See the function documentation for details on customizing distributions,",
            "sample sizes, and working with the full results object."
          ),

          rhandsontable::rHandsontableOutput(ns("ltre_params_table")),

          tags$br(),

          fluidRow(
            column(4, numericInput(ns("n_samples"), "Monte Carlo Samples",
                                   value = isolate(stored_n_samples()),
                                   min = 100, max = 10000, step = 100)),
            column(4, actionButton(ns("run_ltre"), "Run Analysis",
                                   class = "btn-primary", icon = icon("play"))),
            column(4, uiOutput(ns("run_ltre_msg")))
          ),

          results_ui
        )
      )
    }

    # ----------------------------------------------------------
    # Step 1: Open Modal
    # ----------------------------------------------------------
    observeEvent(input$open_ltre_modal, {

      dat <- session$userData$rv_life_stages$dat

      # Add extended columns to full data
      ext_df <- dat
      ext_df$Test_Parameter <- FALSE
      ext_df$Distribution   <- "normal"
      ext_df$SD             <- NA_real_
      ext_df$Low_Limit      <- NA_real_
      ext_df$Up_Limit       <- NA_real_
      ext_df$Notes          <- ""

      # Split into hidden structural rows and visible editable rows
      exclude <- ext_df$Name %in% c("Nstage", "anadromous", "k") |
        grepl("^hs_", ext_df$Name) |
        grepl("^bh_", ext_df$Name)
      ltre_hidden_data(ext_df[exclude, , drop = FALSE])

      visible_df <- ext_df[!exclude, , drop = FALSE]

      # Truncate Parameters display to 25 characters
      visible_df$Parameters <- ifelse(
        nchar(as.character(visible_df$Parameters)) > 25,
        paste0(substr(as.character(visible_df$Parameters), 1, 22), "..."),
        as.character(visible_df$Parameters)
      )

      ltre_table_data(visible_df)
      ltre_result(NULL)

      showModal(build_modal())
    })

    # ----------------------------------------------------------
    # Step 2: Editable Table
    # ----------------------------------------------------------
    output$ltre_params_table <- rhandsontable::renderRHandsontable({
      df <- ltre_table_data()
      req(df)

      # Subset to display columns
      display_df <- df[, c("Parameters", "Name", "Value",
                            "Test_Parameter", "Distribution",
                            "SD", "Low_Limit", "Up_Limit"), drop = FALSE]

      tbl_height <- min(500, 30 + nrow(display_df) * 25)

      rhandsontable::rhandsontable(display_df, height = tbl_height,
                                    stretchH = "all", rowHeaders = NULL) |>
        rhandsontable::hot_col("Parameters", readOnly = TRUE) |>
        rhandsontable::hot_col("Name", readOnly = TRUE) |>
        rhandsontable::hot_col("Value", type = "numeric") |>
        rhandsontable::hot_col("Test_Parameter", type = "checkbox") |>
        rhandsontable::hot_col("Distribution", type = "dropdown",
                               source = c("normal", "beta", "pert")) |>
        rhandsontable::hot_col("SD", type = "numeric") |>
        rhandsontable::hot_col("Low_Limit", type = "numeric") |>
        rhandsontable::hot_col("Up_Limit", type = "numeric")
    })

    # Sync edits back
    observeEvent(input$ltre_params_table, {
      updated <- rhandsontable::hot_to_r(input$ltre_params_table)
      # Preserve Notes column from the full data
      full <- ltre_table_data()
      if (!is.null(full) && "Notes" %in% names(full)) {
        updated$Notes <- full$Notes
      } else {
        updated$Notes <- ""
      }
      ltre_table_data(updated)
    })

    # ----------------------------------------------------------
    # Step 3: Validation and Run Button State
    # ----------------------------------------------------------
    can_run <- reactive({
      df <- ltre_table_data()
      if (is.null(df)) return(FALSE)

      test_rows <- which(df$Test_Parameter == TRUE)
      if (length(test_rows) == 0) return(FALSE)

      for (i in test_rows) {
        val <- as.numeric(df$Value[i])
        low <- as.numeric(df$Low_Limit[i])
        high <- as.numeric(df$Up_Limit[i])
        sd_val <- as.numeric(df$SD[i])
        dist <- tolower(trimws(as.character(df$Distribution[i])))

        if (is.na(val)) return(FALSE)
        if (is.na(low) || is.na(high)) return(FALSE)
        if (low >= high) return(FALSE)

        if (dist %in% c("normal", "beta")) {
          if (is.na(sd_val) || sd_val <= 0) return(FALSE)
        }
      }
      TRUE
    })

    observe({
      shinyjs::toggleState("run_ltre", condition = can_run())
    })

    # Validation message explaining why Run is disabled
    output$run_ltre_msg <- renderUI({
      df <- ltre_table_data()
      if (is.null(df)) {
        return(tags$span("Loading table...", style = "color: grey; font-style: italic; line-height: 34px;"))
      }

      test_rows <- which(df$Test_Parameter == TRUE)
      if (length(test_rows) == 0) {
        return(tags$span("Set at least one parameter's Test_Parameter to TRUE.",
                         style = "color: #c0392b; font-style: italic; line-height: 34px;"))
      }

      # Check each selected row for issues
      issues <- character(0)
      for (i in test_rows) {
        nm <- df$Name[i]
        val <- as.numeric(df$Value[i])
        low <- as.numeric(df$Low_Limit[i])
        high <- as.numeric(df$Up_Limit[i])
        sd_val <- as.numeric(df$SD[i])
        dist <- tolower(trimws(as.character(df$Distribution[i])))

        if (is.na(val)) {
          issues <- c(issues, paste0(nm, ": Value is missing."))
        } else if (is.na(low) || is.na(high)) {
          issues <- c(issues, paste0(nm, ": Low_Limit and Up_Limit are required."))
        } else if (low >= high) {
          issues <- c(issues, paste0(nm, ": Low_Limit must be less than Up_Limit."))
        } else if (dist %in% c("normal", "beta") && (is.na(sd_val) || sd_val <= 0)) {
          issues <- c(issues, paste0(nm, ": SD must be > 0 for ", dist, " distribution."))
        }
      }

      if (length(issues) > 0) {
        return(tags$span(issues[1],
                         style = "color: #c0392b; font-style: italic; line-height: 34px;"))
      }

      # All good
      return(NULL)
    })

    # ----------------------------------------------------------
    # Step 4: Run Analysis
    # ----------------------------------------------------------
    observeEvent(input$run_ltre, {

      shinybusy::show_modal_spinner(
        spin = "hollow-dots",
        color = "#0073b7",
        text = "Running LTRE Range Analysis... This may take a moment."
      )

      # Reconstruct full data frame: visible (user-edited) + hidden structural rows
      input_df <- rbind(ltre_hidden_data(), ltre_table_data())

      # Store n_samples
      stored_n_samples(input$n_samples)

      result <- tryCatch({
        CEMPRA::pop_model_ltre_range(
          life_cycle_params = input_df,
          n_samples = stored_n_samples(),
          seed = 42
        )
      }, error = function(e) {
        shinybusy::remove_modal_spinner()
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
        return(NULL)
      })

      shinybusy::remove_modal_spinner()

      if (!is.null(result)) {
        ltre_result(result)
        session$userData$rv_pop_model_ltre$ltre <- result
        session$userData$rv_show_ltre_plot$open <- TRUE
      }
    })

    # ----------------------------------------------------------
    # Step 5: Show Results Modal
    # ----------------------------------------------------------
    observeEvent(session$userData$rv_show_ltre_plot$open, {
      req(session$userData$rv_show_ltre_plot$open)
      showModal(build_modal())
    }, ignoreInit = TRUE)

    # ----------------------------------------------------------
    # Step 6: Render Results
    # ----------------------------------------------------------

    # Lambda summary text
    output$ltre_lambda_summary <- renderText({
      res <- ltre_result()
      req(res)
      s <- res$lambda_summary
      paste0(
        "Median lambda: ", round(s$median, 4),
        " | Pr(lambda > 1): ", round(s$prob_lambda_gt_1 * 100, 1), "%",
        " | Pr(lambda < 1): ", round(s$prob_lambda_lt_1 * 100, 1), "%",
        " | Valid samples: ", s$n_valid, " of ", s$n_valid + s$n_failed
      )
    })

    # Lambda histogram
    output$ltre_lambda_hist <- renderPlot({
      res <- ltre_result()
      req(res)

      lambda_vals <- res$samples$lambda[!is.na(res$samples$lambda)]
      med_val <- res$lambda_summary$median
      pct_below <- round(res$lambda_summary$prob_lambda_lt_1 * 100, 1)

      hist(lambda_vals, breaks = 40, col = "steelblue", border = "white",
           main = "Distribution of Lambda from Monte Carlo Samples",
           xlab = "Lambda (population growth rate)",
           sub = paste0(pct_below, "% of samples with lambda < 1"))

      abline(v = 1, col = "black", lty = 1, lwd = 2)
      abline(v = med_val, col = "red", lty = 2, lwd = 2)

      legend("topright",
             legend = c("Lambda = 1", paste0("Median = ", round(med_val, 4))),
             col = c("black", "red"), lty = c(1, 2), lwd = 2, bg = "white")
    })

    # PRCC bar chart
    output$ltre_prcc_barplot <- renderPlot({
      res <- ltre_result()
      req(res)

      prcc_data <- res$prcc_rankings
      prcc_data$parameter <- factor(prcc_data$parameter,
                                     levels = rev(prcc_data$parameter))
      prcc_data$significant <- prcc_data$p_value < 0.01

      ggplot2::ggplot(prcc_data,
                      ggplot2::aes(x = .data$abs_prcc,
                                   y = .data$parameter,
                                   fill = .data$significant)) +
        ggplot2::geom_col(width = 0.7) +
        ggplot2::scale_fill_manual(
          values = c("TRUE" = "steelblue", "FALSE" = "coral"),
          labels = c("TRUE" = "(p < 0.01)", "FALSE" = "(p > 0.01)"),
          name = "Statistical Significance"
        ) +
        ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
        ggplot2::labs(
          x = "Absolute PRCC with Lambda",
          y = "Model Parameter",
          title = "Parameter Sensitivity Rankings",
          subtitle = paste0("Based on ", res$metadata$n_samples, " Monte Carlo samples")
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          legend.position = "bottom"
        )
    })

    # PRCC table
    output$ltre_prcc_table <- DT::renderDT({
      res <- ltre_result()
      req(res)

      top10 <- utils::head(res$prcc_rankings, 10)
      top10$prcc <- round(top10$prcc, 3)
      top10$abs_prcc <- round(top10$abs_prcc, 3)
      top10$p_value <- round(top10$p_value, 4)

      DT::datatable(
        top10,
        selection = "none",
        rownames = FALSE,
        colnames = c("Parameter", "PRCC", "p-value", "Abs. PRCC"),
        options = list(pageLength = 10, dom = "t", ordering = FALSE)
      )
    })

  })
}
