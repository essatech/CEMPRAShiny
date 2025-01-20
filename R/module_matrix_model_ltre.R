#' Matrix Model LTRE UI
module_matrix_model_ltre_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$b("Sensitivity Tests", style = "text-align: center;"),
    tags$div(
      class = "lam_bb",
      tags$p(
        "Run a sensitivity tests by increasing and decreasing parameters by a fraction",
      ),
      
      fluidRow(
        column(
          width = 6,
          numericInput(ns("test_n_adjust"), label = "adjust params by fraction", value = 0.05)
        ),
        column(width = 6, numericInput(
          ns("test_n_replicates"),
          label = "n replicates",
          value = 10
        ))
      ),
      
      actionButton(ns("ltre_projection"), "Run Sensitivity Test", class = "btn lam_bb")
    )
  )
}


#' Matrix Model LTRE Server Component
module_matrix_model_ltre_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    print("module_matrix_model_ltre_server...")
    
    #-------------------------------------------------------
    # Run parameter sensitivity test...
    #-------------------------------------------------------
    observeEvent(input$ltre_projection, {
      # Show a loading spinner to the user
      show_modal_spinner(
        spin = "hollow-dots",
        color = "#0073b7",
        text = paste0("Running parameter sensitivity test...")
      )
      
      # -----------------------------------
      # Gather inputs in isolate mode
      isolate({
        # Modal is hidden or reset
        session$userData$rv_show_ltre_plot$open <- FALSE
        
        test_n_adjust <- input$test_n_adjust
        test_n_replicates <- input$test_n_replicates
        
        dat <- session$userData$rv_life_stages$dat
        
        # Gather the environmental stressors (if any)
        CE_df <- session$userData$rv_sandbox_stressors$dat
        
        dose <- NA
        sr_wb_dat <- NA
        HUC_ID <- NA
        
        if(!(is.data.frame(CE_df))) {
          if(class(CE_df) == "list") {
            if(length(CE_df) > 1) {
              if(names(CE_df)[1] == "Mean") {
                print("data preview error")
                CE_df <- data.frame()
                browser()
              }
            }
          }
        }
        

        
        
        
        if (length(CE_df) == 0) {
          CE_df <- data.frame()
        } else {
          # ---------------------------------
          # See if any enviro stressors present
          CE_df <- CE_df[CE_df$check_on, ]
        }
        
        
        if (nrow(CE_df) >= 1) {
          # Thin down stressors to target...
          sr <- list()
          sr$main_sheet <- session$userData$rv_stressor_response$main_sheet
          sr$stressor_names <- session$userData$rv_stressor_response$stressor_names
          sr$sr_dat <- session$userData$rv_stressor_response$sr_dat
          # Thin down...
          sr$main_sheet <- sr$main_sheet[which(sr$main_sheet$Stressors %in% CE_df$Stressors), ]
          sr$stressor_names <- sr$stressor_names[sr$stressor_names %in% CE_df$Stressors]
          sr$sr_dat <- sr$sr_dat[which(names(sr$sr_dat) %in%  CE_df$Stressors)]
          
          # Stressor Magnitude...
          # Make up dummy data for a sample watershed
          smw_sample <- data.frame(
            HUC_ID = CE_df$HUC_ID,
            NAME = CE_df$NAME,
            Stressor = CE_df$Stressors,
            Stressor_cat = CE_df$Stressor_cat,
            Mean = CE_df$Mean,
            SD = CE_df$SD,
            Distribution = CE_df$Distribution,
            Low_Limit = CE_df$Low_Limit,
            Up_Limit = CE_df$Up_Limit
          )
          
          # Set CE layers
          dose <- smw_sample
          sr_wb_dat <- sr
          
          HUC_ID <- unique(CE_df$HUC_ID)[1]
          if (length(HUC_ID) == 0) {
            HUC_ID <- unique(CE_df$HUC)[1]
          }
          
          
        }
        
      })
      # end of isolate
      # -------------------------------------------
      
      # semi-sanitize inputs
      test_n_adjust <- ifelse(is.na(test_n_adjust), 0, test_n_adjust)
      test_n_adjust <- ifelse(test_n_adjust < 0, 0, test_n_adjust)
      
      test_n_replicates <- ifelse(is.na(test_n_replicates), 1, test_n_replicates)
      test_n_replicates <- ifelse(test_n_replicates < 1, 1, test_n_replicates)
      
      # Gather population model inputs
      life_cycle_params <- isolate({
        session$userData$rv_life_stages$dat
      })
      
      # Run the sensitivity test
      ltre <- CEMPRA::pop_model_ltre(
        step_size = test_n_adjust,
        # User input
        dose = dose,
        sr_wb_dat = sr_wb_dat,
        life_cycle_params = life_cycle_params,
        HUC_ID = HUC_ID,
        n_reps = test_n_replicates,
        # User input
        stressors = NA,
        # Should already be filtered
        habitat_dd_k = NULL
      )
      
      # Gather the outputs and store in user data
      session$userData$rv_pop_model_ltre$ltre <- ltre
      
      # Open the modal
      session$userData$rv_show_ltre_plot$open <- TRUE
      
      
    })
    
    
    #----------------------------------------------------------
    # Create content (tables and figures) to show in the modal
    #----------------------------------------------------------
    
    # Render the data table
    output$ltre_summary_ce <- renderDT({
      # Example dataset (iris)
      ltre <- session$userData$rv_pop_model_ltre$ltre
      
      df <- ltre$ltre_summary_ce
      df$Parameter <- df$this_param
      df$Value <- df$value_default
      df$Change <- paste0(df$direction, ": ", round(df$value_adj, 3))
      df$Lambda <- round(df$lambda, 3)
      df$PctChange <- round(df$lambda_change, 3)
      df <- df[, c("Parameter", "Value", "Change", "Lambda", "PctChange")]
      
      datatable(
        df,
        selection = 'none',
        rownames = FALSE,
        colnames = c(
          "Parameter",
          "Initial Value",
          "Changed Value",
          "Resulting Lambda",
          "% Change"
        ),
        options = list(pageLength = 10, scrollX = TRUE)
      ) %>%
        formatPercentage("PctChange", 2)
      
      
    })
    
    
    # Render the data table
    output$ltre_summary_baseline <- renderDT({
      # Example dataset (iris)
      ltre <- session$userData$rv_pop_model_ltre$ltre
      
      df <- ltre$ltre_summary_baseline
      df$Parameter <- df$this_param
      df$Value <- df$value_default
      df$Change <- paste0(df$direction, " to: ", round(df$value_adj, 3))
      df$Lambda <- round(df$lambda, 3)
      df$PctChange <- round(df$lambda_change, 3)
      df <- df[, c("Parameter", "Value", "Change", "Lambda", "PctChange")]
      
      datatable(
        df,
        selection = 'none',
        rownames = FALSE,
        colnames = c(
          "Parameter",
          "Initial Value",
          "Changed Value",
          "Resulting Lambda",
          "% Change"
        ),
        options = list(pageLength = 10, scrollX = TRUE)
      ) %>%
        formatPercentage("PctChange", 2)
      
      
    })
    
    
    
    # Render the data table
    output$ltre_text_baseline <- renderText({
      ltre <- session$userData$rv_pop_model_ltre$ltre
      mlambda <- round(ltre$mean_baseline_lambda, 3)
      paste0(
        "Under the baseline scenario (omitting all cumulative effects stressors and stressor-response relationships) the mean lambda value from stochastic simulations is " ,
        mlambda,
        "."
      )
    })
    
    output$ltre_text_ce <- renderText({
      ltre <- session$userData$rv_pop_model_ltre$ltre
      mlambda <- round(ltre$mean_ce_lambda, 3)
      paste0(
        "Under the cumulative effects scenario (with selected stressors and stressor-response relationships) the mean lambda value for the target location from stochastic simulations is  " ,
        mlambda,
        "."
      )
    })
    
    
    
    # Update plot parameter selection
    observe({
      ltre <- session$userData$rv_pop_model_ltre$ltre
      df <- ltre$ltre_summary_baseline
      parameters <- sort(unique(df$this_param))
      
      updateSelectInput(session,
                        "plot_param",
                        choices = parameters,
                        selected = parameters[1])
    })
    
    
    plot_data <- reactive({
      req(input$plot_param) # Ensure the input is available
      dat <- session$userData$rv_pop_model_ltre$ltre$all_dat_stochastic
      dat <- dat[dat$this_param == input$plot_param, ]
      dat
    })
    
    
    output$histogram_plots <- renderPlotly({
      print("Plotting adults...")
      req(plot_data()) # Ensure plot_data() is ready
      
      dat <- plot_data()
      
      dat$ce <- ifelse(dat$ce == "ce", "CE: With Stressors", dat$ce)
      dat$ce <- ifelse(dat$ce == "baseline", "Baseline: Without Stressors", dat$ce)
      
      ggplot(dat, aes(x = lambda, fill = interaction(ce, direction))) +
        # Overlapping histograms
        geom_density(
          alpha    = 0.5,            # semi-transparent fill
          position = "identity",     # allow overlaps
        ) +
        # Facet by 'this_param' vertically (one row per param)
        facet_wrap(~ ce, ncol = 1, strip.position = "top") +
        scale_fill_manual(
          values = c(
            "CE: With Stressors.increase"        = "lightblue",
            "CE: With Stressors.decrease"        = "lightpink",
            "Baseline: Without Stressors.increase"  = "lightblue",
            "Baseline: Without Stressors.decrease"  = "lightpink"
          ),
          name = "Scenario"
        ) +
        labs(
          x = "Lambda Value",
          y = "Relative Frequency"
        ) +
        # General theme
        theme_bw(base_size = 14) +
        theme(legend.position="none")
      
      
    })
    
    
    
    #-------------------------------------------------------
    # Open the sensitivity test modal
    #-------------------------------------------------------
    observeEvent(session$userData$rv_show_ltre_plot$open, {
      # Determine the number of stages
      n_stage <- session$userData$rv_life_stages$dat$Value[session$userData$rv_life_stages$dat$Name == "Nstage"]
      
      
      # Show sensitivity test results
      showModal(
        modalDialog(
          title = "Parameter Sensitivity Tests",
          
          tagList(
            tags$p(
              "The following sensitivity test is similar to a life table response experiment (LTRE) for matrix population models. In the following test, each parameter is increased and decreased by a fixed fraction (defined by the user input):"
            ),
            tags$p(
              "increased parameter value = initial value + (initial value * fraction)"
            ),
            tags$p(
              "decreased parameter value = initial value - (initial value * fraction)"
            ),
            tags$p(
              "The projection matrices are then regenerated for each change in each parameter and compared to the initial default value (with the original unmodified values). The intrinsic productivity (lambda) is then calculated from each projection matrix to evaluate the sensitivity between lambda and input parameters. In other words, this sensitivity test is used to determine the relative influence of each parameter on the change in intrinsic productivity."
            ),
            
            tags$p(
              "The first table shows the results from the sensitivity test using baseline vital rates (omitting all stressors and stressor-response relationships). The second table shows us the result of the same sensitivity test after applying (selected) stressors and stressor response relationships to the vital rates as initial modifiers. Pay close attention to how the relative ranking of each parameter changes between these tables or ignore the second table if you are running the test without stressors."
            ),
            tags$p(
              "Resulting lambda values and percent change values are reported as the geometric mean from batch replicates. Batch replicates allow us to incorporate stochasticity in the vital rates, stressor magnitude levels, and stressor-response values. If the ranking order does not make sense, stochasticity may be affecting the results. To resolve this issue either increase the number of batch replicates and/or decrease stochasticity in the vital rates, stressors, and stressor-response relationships and try re-running the sensitivity test."
            ),
            
            tags$p(
              "The sensitivity tests can be creatively leveraged for comparative demography. Users may switch between locations, scenarios, toggle stressors on/off, etc. to understand important context dependence. However, be aware that the sensitivity test only explores changes to intrinsic population productivity (lambda). The sensitivity test does not incorporate habitat availability and habitat capacity constraints (density dependence), which may be an important bottleneck to population recovery. Also, remember that a fixed increase of X% for one parameter (e.g., egg survivorship) may be possible to achieve in the real world, but the same fixed increase (of X%) may be impossible or meaningless to pursue for another vital rate (e.g., sex ratio). See existing literature on LTREs for avenues to pursue this further."
            ),
            
            
            tags$br(),
            
            tags$h4(
              "Baseline Sensitivity Test Results: Exluding all Stressors and Stressor-Response Relationships"
            ),
            
            textOutput(ns("ltre_text_baseline")),
            
            DTOutput(ns("ltre_summary_baseline")),
            
            tags$br(),
            
            tags$h4(
              "CE Sensitivity Test Results: Including Stressors and Stressor-Response Relationships"
            ),
            
            textOutput(ns("ltre_text_ce")),
            
            DTOutput(ns("ltre_summary_ce")),
            
            #
            tags$br(),
            
            tags$p(
              "Explore stochastic distributions of lambda estimates. Use the select box to switch between parameters. Pink shows the distribution of lambda values where the parameter was decreased by a fraction. Blue shows the distribution of lambda values where the parameter was increased by a fraction."
            ),
            
            selectInput(ns("plot_param"), "Choose a parameter to plot:", choices = NULL),
            
            plotlyOutput(ns("histogram_plots"))
            
          ),
          easyClose = TRUE,
          size = "l",
          footer = NULL
        ),
        
      )
    }, ignoreInit = TRUE)
  
  
  
  
  })
}