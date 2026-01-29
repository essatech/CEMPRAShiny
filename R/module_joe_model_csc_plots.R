#' Joe Model Cumulative System Capacity Plots UI
#'
#' Define parameters and run the Joe Model
#' 
#' @param id The id for this module
#'
#' @return a tagList containing UI elements
#'
module_joe_model_csc_plots_ui <- function(id) {
  
  ns <- NS(id)
  # Single action button to call modal
  actionButton(ns("open_joe_modal_csc_plots_all"),
                  tags$b("Across all locations"),
                  class="chart-line clean-button",
                  width = "100%")

}



#' Joe Model Cumulative System Capacity Plots Server
#'
#' Server and modal content for the Joe model server
#'
#' @param none
#'
#' @return None
#'
module_joe_model_csc_plots_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      print("Calling module_joe_model_csc_plots_server")
      
      #-------------------------------------------------------
      # DISABLE AND ENABLE 
      #------------------------------------------------------- 
      # this module is disabled if the Joe Model results are empty
      observe({
        sims <- session$userData$rv_joe_model_results$sims
        if(length(sims) > 0) {
          shinyjs::enable("open_joe_modal_csc_plots_all")
        } else {
          shinyjs::disable("open_joe_modal_csc_plots_all")
        }
      })




      #-------------------------------------------------------
      # START OF INPUT MODAL UI
      #-------------------------------------------------------      
      # Display the CSC plots for all watersheds
      observeEvent(input$open_joe_modal_csc_plots_all, {
        print("Joe model form click to open ...")
        # Gather a list of all the stessors to build the checkbox list
        showModal(modalDialog(
          title = "Cumulative System Capacity Plots",
          tagList(
              shinydashboard::box(
                width = 12,
                fluidRow(
                  column(width = 12,
                         tags$p("This section provides an overview of the Joe Model results for the entire study area. The following table contains summary statistics for the cumulative system capacity (SC) across each simulation (batch replicate) and locations. The first column describes summary statistics across the batch replicates (i.e., take the mean across locations for each batch replicate then calculate summary statistics across the batch replicates). The second column describes summary statistics across all locations and batch replicates. A histogram is included (below) to visualize system capacity across all HUCs and batch replicates. Detailed plots can be generated for each watershed but they take longer to render (click the button to generate rendering)."),
                         )
                ),
                fluidRow(
                  column(
                    DT::dataTableOutput(ns("csc_tables")),
                    width = 6),
                  column(
                    column(plotOutput(ns("csc_hist")), width = 10),
                    width = 6)
                )
              ),
            fluidRow(
              shinydashboard::box(
                width = 12,
                fluidRow(
                  column(width = 12,
                         tags$p("Histograms of cumulative system capacity can also be generated individually for each HUC, however the rendering process is slow. Click the button below to generate cumulative system capacity plots across all selected HUCs individually. Colours in the graph represent percentile breaks."),
                  )
                ),
                actionButton(ns("csc_show_all_plots"), "display individual plots for all HUCs (slow rendering)"),
                plotOutput(ns("csc_plot_panel")),
              )
            ),
          ),
          easyClose = TRUE,
          size = 'l',
          footer = NULL
        ))
      }) # END OF INPUT MODAL UI
      #-------------------------------------------------------


      
      
      #-------------------------------------------------------
      # Generate CSC Joe Model Summary Tables
      #-------------------------------------------------------
      output$csc_tables <- renderDataTable({
        
        # Build summary table of Joe Model results
        # Get the most recent result set
        simulation_index <- length(session$userData$rv_joe_model_results$sims)
        # Get the Joe model results object 
        jmr <- session$userData$rv_joe_model_results$sims[[simulation_index]]
        
        # Summary across simulations
        # Look at system wide CE scores
        sim_scores <- jmr$ce.df %>% group_by(simulation) %>%
          summarise(
            CE_mean = mean(CE, na.rm = TRUE)
          )
        
        s_obj_sim <- summary(sim_scores$CE_mean * 100, na.rm = TRUE)
        
        # Summary across HUCs
        # Look at system wide CE scores
        h_scores <- jmr$ce.df %>% group_by(HUC) %>%
          summarise(
            CE_mean = mean(CE, na.rm = TRUE)
          )
        
        s_obj_huc <- summary(h_scores$CE_mean * 100, na.rm = TRUE)
        
        df_csc_res <- data.frame(sims = as.matrix(s_obj_sim)[,1], hucs = as.matrix(s_obj_huc)[,1])
        df_csc_res <- round(df_csc_res, 1)
        

        # Build the JS DT Data Table Object
        my_dt <- DT::datatable(
          df_csc_res,
          editable =  FALSE,
          caption = "Mean system capacity summary tables across all simulations for the entire system (Global Mean System Capacity Across Simulations) and across individual locations",  
          colnames = c('Global Mean SC (per simulation, %)' = 'sims', 'Mean SC Across Locations (%)' = 'hucs'),
          filter = "none",
          selection = "single",
          rownames = TRUE,
          class = "cell-border stripe",
          options = list(
            pageLength = 500,
            info = FALSE,
            dom = 't',
            ordering = FALSE,
            columnDefs = list(list(
              className = 'dt-left', targets = "_all"
            ))
          )
        )

      })
      
      
      
      #-------------------------------------------------------
      # Historgram for CSC per HUC
      #-------------------------------------------------------
      output$csc_hist <- renderPlot({
        
        # Build summary table of Joe Model results
        # Get the most recent result set
        simulation_index <- length(session$userData$rv_joe_model_results$sims)
        # Get the Joe model results object 
        jmr <- session$userData$rv_joe_model_results$sims[[simulation_index]]
        
        # Summary across HUCs
        # Look at system wide CE scores
        h_scores <- jmr$ce.df %>% group_by(HUC) %>%
          summarise(
            CE_mean = mean(CE, na.rm = TRUE)
          )
        
        hist(h_scores$CE_mean * 100, xlab = "mean sys. capacity per location (%)", main = "Across Locations")
      
      })
      
      
      #-------------------------------------------------------
      # Generate CSC Joe Model Plot Panel
      #-------------------------------------------------------
      
      # Set trigger to load all plots..
      pp <- eventReactive(input$csc_show_all_plots, {
        
        # Build summary table of Joe Model results
        # Get the most recent result set
        simulation_index <- length(session$userData$rv_joe_model_results$sims)
        # Get the Joe model results object 
        jmr <- session$userData$rv_joe_model_results$sims[[simulation_index]]
        
        
        # Get the Joe Model result object
        plot_df <- jmr$ce.df
        plot_df$HUC <- as.character(plot_df$HUC)

        # Get NAME data from stressor magnitude to create better labels
        sm_dat <- session$userData$rv_stressor_magnitude$sm_dat
        if (!is.null(sm_dat) && "NAME" %in% colnames(sm_dat) && "HUC_ID" %in% colnames(sm_dat)) {
          # Get unique HUC_ID to NAME mapping
          name_lookup <- sm_dat %>%
            dplyr::select(HUC_ID, NAME) %>%
            dplyr::distinct(HUC_ID, .keep_all = TRUE) %>%
            dplyr::mutate(HUC_ID = as.character(HUC_ID))

          # Create facet label: truncate NAME if > 10 chars, format as "NAME, ID:HUC_ID"
          name_lookup <- name_lookup %>%
            dplyr::mutate(
              NAME_trunc = ifelse(nchar(NAME) > 10, paste0(substr(NAME, 1, 10), "..."), NAME),
              facet_label = paste0(NAME_trunc, ", ID:", HUC_ID)
            )

          # Join to plot_df
          plot_df <- plot_df %>%
            dplyr::left_join(name_lookup, by = c("HUC" = "HUC_ID"))

          # For any HUCs without NAME, use just the HUC ID
          plot_df <- plot_df %>%
            dplyr::mutate(facet_label = ifelse(is.na(facet_label), paste0("ID:", HUC), facet_label))

          # Order by NAME then HUC_ID for facet display
          plot_df <- plot_df %>%
            dplyr::arrange(NAME, as.numeric(HUC))

          # Convert facet_label to factor with ordered levels
          label_order <- plot_df %>%
            dplyr::distinct(facet_label, NAME, HUC) %>%
            dplyr::arrange(NAME, as.numeric(HUC)) %>%
            dplyr::pull(facet_label)
          plot_df$facet_label <- factor(plot_df$facet_label, levels = unique(label_order))
        } else {
          # Fallback: use HUC as facet label
          plot_df$facet_label <- paste0("ID:", plot_df$HUC)
          plot_df$facet_label <- factor(plot_df$facet_label,
            levels = unique(plot_df$facet_label[order(as.numeric(plot_df$HUC))]))
        }

        # Median by plot
        plot_df_median <- plot_df %>% group_by(facet_label) %>% summarise(median = median(CE, na.rm = TRUE))
        plot_df_mean <- plot_df %>% group_by(facet_label) %>% summarise(mean = mean(CE))
        
        # Set Andy theme
        andy_theme <- theme(
          axis.text.y   = element_text(size = 12),
          axis.text.x   = element_text(size = 12),
          axis.title.y  = element_text(size = 14),
          axis.title.x  = element_text(size = 14),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(
            colour = "black",
            fill = NA,
            size = 0.5
          )
        )
        
        
        # Generate big panel plot
        big_plot <- ggplot(data = plot_df) +
          geom_freqpoly(aes(x = CE, y = ..ndensity..),
                        size = 1,
                        binwidth = 0.01) +
          geom_vline(
            data = plot_df_median,
            mapping = aes(xintercept = median, color = "median"),
            linetype = "dashed"
          ) +
          geom_vline(
            data = plot_df_mean,
            mapping = aes(xintercept = mean, color = "mean"),
            linetype = "dashed"
          ) +
          scale_color_manual(name = "statistics",
                             values = c(median = "blue", mean = "red")) +
          scale_x_continuous(limits = c(0, 1)) +
          facet_wrap( ~ facet_label, ncol = 5) +
          geom_rect(
            data = data.frame(
              xmin = 0,
              xmax = 0.2,
              ymin = 0,
              ymax = Inf
            ),
            aes(
              xmin = xmin,
              xmax = xmax,
              ymin = ymin,
              ymax = ymax
            ),
            fill = "red",
            alpha = 0.2
          ) +
          geom_rect(
            data = data.frame(
              xmin = 0.2,
              xmax = 0.5,
              ymin = 0,
              ymax = Inf
            ),
            aes(
              xmin = xmin,
              xmax = xmax,
              ymin = ymin,
              ymax = ymax
            ),
            fill = "orange",
            alpha = 0.2
          ) +
          geom_rect(
            data = data.frame(
              xmin = 0.5,
              xmax = 0.7,
              ymin = 0,
              ymax = Inf
            ),
            aes(
              xmin = xmin,
              xmax = xmax,
              ymin = ymin,
              ymax = ymax
            ),
            fill = "yellow",
            alpha = 0.2
          ) +
          geom_rect(
            data = data.frame(
              xmin = 0.7,
              xmax = 1,
              ymin = 0,
              ymax = Inf
            ),
            aes(
              xmin = xmin,
              xmax = xmax,
              ymin = ymin,
              ymax = ymax
            ),
            fill = "light green",
            alpha = 0.2
          ) +
          xlab("Cumulative system capacity") +
          ylab("Scaled probability") +
          andy_theme
        
        
        # Hide trigger button after pressed
        shinyjs::hide("csc_show_all_plots")

        return(big_plot)

      })
      
      
      # Make plot height dynamic
      heightSize <- reactive({
        print("Adjust csc plot height size...")
        # Get the Joe model results object 
        simulation_index <- length(session$userData$rv_joe_model_results$sims)
        jmr <- session$userData$rv_joe_model_results$sims[[simulation_index]]
        n_hucs <- unique(jmr$ce.df$HUC)
        my_df_rows <- length(n_hucs) / 5 # 5 columns
        plot_height <- 60 + as.integer(120 * my_df_rows)
        print(plot_height)
        
        return(plot_height)

      })
      
      # Generate plots for the latest 
      output$csc_plot_panel <- renderPlot({

        print("renderPlot...")
        if(input$csc_show_all_plots == 0) {
          return(NULL)
        } else {
          # Wait for button click to render plot
          pp()
        }
        
      }, height = heightSize)
      
      

      
   
    }
  )
}