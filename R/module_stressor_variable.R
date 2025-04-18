#' stressor_variable UI
#'
#' The UI portion of the main map module
#'
#' @param id The id for this module
#'
#' @return a tagList containing UI elements
#'
module_stressor_variable_ui <- function(id) {
  ns <- NS(id)

  tags$div(
    class = "stack-box map-variable", id = ns("var_id"),
    shinydashboard::box(
      width = 12,
      background = "light-blue",
      class = "stack-box-border",
      htmlOutput(ns("variable_label")),
      htmlOutput(ns("variable_val_raw")),
      tags$div(actionButton(ns("response_plot"), icon("chart-line"), class = "response-button"),
        style = "float: right; display: inline-block;"
      ),
      tags$p("", style = "float: right;"),
      tags$div(numericInput(ns("hiddenload"), label = "hidden", value = 0), style = "display:none;")
    ) # end of box
  ) # end of div
}


#' stressor_variable Server
#'
#' The Server portion of the stressor_variable module
#'
#' @param mlabel Character. Label of the variable
#'
#'
#' @return None
#'
module_stressor_variable_server <- function(id, stressor_index = NA) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      
      # Set the label
      output$variable_label <- renderUI({
        # print("Variable Label")
        label <- session$userData$rv_stressor_response$pretty_names[stressor_index]
        # Adjust if matrix interaction term
        if (stressor_index > length(session$userData$rv_stressor_response$pretty_names)) {
          label <- session$userData$rv_stressor_response$interaction_names[stressor_index - length(session$userData$rv_stressor_response$pretty_names)]
        }
        label <- paste0(label, "  ")
        tags$p(label, style = "float: left;")
      })

      # Change mouse-over raw value
      output$variable_val_raw <- renderUI({
        sname <- session$userData$rv_stressor_response$stressor_names[stressor_index]

        # Adjust if matrix interaction term
        if (stressor_index > length(session$userData$rv_stressor_response$stressor_names)) {
          sname <- session$userData$rv_stressor_response$interaction_names[stressor_index - length(session$userData$rv_stressor_response$pretty_names)]
        }

        if (is.null(session$userData$rv_stressor_response$active_values_raw)) {
          tags$p(" ", style = "float: left; display:inline;")
        } else {
          raw_vals <- session$userData$rv_stressor_response$active_values_raw$Mean
          names <- session$userData$rv_stressor_response$active_values_raw$Stressor
          target_val <- raw_vals[which(names == sname)]
          target_val <- ifelse(is.na(target_val), " ", paste0(" ", target_val))
          tags$p(target_val, style = "float: left; display:inline; padding-left: 8px; font-size: 90%;")
        }
      })


      # ------------------------------------------------------
      # Set selected class as selected variable
      # Should it be styled as selected or light blue un-selected
      # ------------------------------------------------------
      observe({
        req(input$hiddenload)
        # print("Selected Variable")
        
        # MJB added here June 6 2023
        
        # Set the stressor response object as a reactive value
        if (!(is.na(stressor_index))) {
          
          active <- session$userData$rv_stressor_response$active_layer
          current <- session$userData$rv_stressor_response$stressor_names[stressor_index]


          # Fix name if selecting an interaction matrix
          if (is.na(current)) {
            # Assume interaction matrix
            current <- session$userData$rv_stressor_response$interaction_names[stressor_index - length(session$userData$rv_stressor_response$pretty_names)]
          }
          
         
         if(is.null(current)) {
           current <- NA # MJB added - fix exception for null
         }
          
          if (!(is.na(current)) & !(is.na(active))) {
            
            # Special case for system_capacity
            if (active == "system_capacity") {
              q_code <- paste0("jQuery('#main_map-var_id').addClass('var-selected');")
              shinyjs::runjs(code = q_code)
            }
            if (active != "system_capacity") {
              q_code <- paste0("jQuery('#main_map-var_id').removeClass('var-selected');")
              shinyjs::runjs(code = q_code)
            }
            
            current_id <- current
            
            # JS can not have space in IDs
            current_id <- gsub(" ", "__", current_id, fixed = TRUE)
            active_id <- gsub(" ", "__", active, fixed = TRUE)
            
            # deal with space and jQuery ID
            if(grepl(" ", current_id)) {
              current_id <- gsub(" ", "\\\ ", current_id, fixed = TRUE)
            }
            
            if (active_id == current) {
              # print("Adding class")
              q_code <- paste0("jQuery('#main_map-", current_id, "-var_id').addClass('var-selected');")
              shinyjs::runjs(code = q_code)
            } else {
              q_code <- paste0("jQuery('#main_map-", current_id, "-var_id').removeClass('var-selected');")
              shinyjs::runjs(code = q_code)
            }
          }
        }
      })


      # ---------------------------------------------------------
      # Listen to click events to change target variable selected
      observe({
        # ensure UI is loaded - do not run if not set
        req(input$hiddenload)
        # User clicks on ID
        current <- session$userData$rv_stressor_response$stressor_names[stressor_index]

        # If interaction matrix
        if (is.na(current)) {
          # Assume interaction matrix
          current <- session$userData$rv_stressor_response$interaction_names[stressor_index - length(session$userData$rv_stressor_response$pretty_names)]
        }

        # Update reactive value of target variable selected
        updateActiveVar <- function(current) {
          print(paste0("User click.. update to layer... ", current))
          # Set css color of system capacity button to light blue
          
          session$userData$rv_stressor_response$active_layer <- current
        }
        # Use mouse click
        my_id <- paste0("main_map-", current, "-var_id")
        onclick(my_id,
          updateActiveVar(current),
          asis = TRUE
        )
      })






      #-------------------------------------------------------
      # Stressor response modal dialog
      #-------------------------------------------------------
      # Open the stressor response dialog box
      observeEvent(input$response_plot, {
        
        # Dont load until button clicked
        req(input$hiddenload)

        this_var <- session$userData$rv_stressor_response$pretty_names[stressor_index]
        # print(paste0("Stressor response modal is open for ... ", this_var))

        # Increment modal refresh counter
        print("----SR MODAL OPEN ------")
        print(stressor_index)
        
        # -------------------------------------------------------
        # If interaction matrix - show but not editable
        if (is.na(this_var)) {
          # Assume interaction matrix
          this_var <- session$userData$rv_stressor_response$interaction_names[stressor_index - length(session$userData$rv_stressor_response$pretty_names)]

          showModal(modalDialog(
            title = paste0("Stressor-Response Relationship: ", this_var),
            tagList(
              tags$p("Custom 2-way matrix interaction surface: The 2-factor interaction matrices are defined in the Stressor-Response Excel workbook. Customizable 2-factor interaction matrices may be (optionally) included by users to specify non-additive interactions between stressor variables (e.g., antagonistic, synergistic, etc.). If included, these matrices define the mean system capacity at different combination levels between two stressors. This can be especially important to capture conditional effects, attenuating or exacerbating factors, and/or compound variance and uncertainties. The 2-factor interaction matrices can also be a convenient mechanism to explore hypothetical and experimental scenarios."),
              tags$b("Mean System Capacity:"),
              fluidRow(
                shinydashboard::box(
                  width = 12,
                  tags$p(textOutput(ns("text_interaction_matrix"))),
                  DT::dataTableOutput(ns('interaction_matrix_main'))
                )
              ),
              tags$b("SD:"),
              fluidRow(
                shinydashboard::box(
                  width = 12,
                  tags$p(textOutput(ns("text_interaction_matrix"))),
                  DT::dataTableOutput(ns('interaction_matrix_sd'))
                )
              ),
              tags$b("Lower Limit:"),
              fluidRow(
                shinydashboard::box(
                  width = 12,
                  tags$p(textOutput(ns("text_interaction_matrix"))),
                  DT::dataTableOutput(ns('interaction_matrix_ll'))
                )
              ),
              tags$b("Upper Limit:"),
              fluidRow(
                shinydashboard::box(
                  width = 12,
                  tags$p(textOutput(ns("text_interaction_matrix"))),
                  DT::dataTableOutput(ns('interaction_matrix_ul'))
                )
              )
            ),
            easyClose = TRUE,
            size = "l",
            footer = NULL
          ))

          # end of matrix interaction surface
        } else {

          # -------------------------------------------------------
          # Normal variable - not a matrix interaction surface
          # Currently selected variable name
          # note can not use this_var <- session$userData$rv_stressor_response$active_layer

          this_var_pretty <- gsub("_", " ", this_var)

          # Main sheet attributes
          this_main <- session$userData$rv_stressor_response$main_sheet

          showModal(modalDialog(
            title = paste0("Stressor-Response Relationship: ", this_var_pretty),
            tagList(
              
              fluidRow(shinydashboard::box(
                width = 12,
                
                tags$p("Adjust how the stressor-response relationship is linked to the focal species/systems. The following inputs are sourced from the Main worksheet of the Stressor-Response workbook. Adjust the following dropdowns to control min/max interactions, change the functional form, the model endpoint (Population Model vs Joe Model), and vital rate linkages (if associated with the Population Model).", class = "small-helper-text"),
                
                
                fluidRow(
                  column(
                    width = 3,
                    selectInput(
                      ns("s_Interaction"),
                      "Interactions:",
                      c(
                        "NA" = NA,
                        "Minimum" = "Minimum",
                        "Maximum" = "Maximum"
                      )
                    ),
                    bsTooltip(
                      id = ns("s_Interaction"),
                      title = "Choose whether to use the minimum or maximum interaction rule",
                      placement = "top",
                      trigger = "hover"
                    ),
                    
                    class = "grouped-box-1"
                  ),
                  column(
                    width = 3,
                    selectInput(
                      ns("s_Linked"),
                      "Interaction (Linked) Groups:",
                      c(
                        "NA" = NA,
                        "A" = "A",
                        "B" = "B",
                        "C" = "C",
                        "D" = "D",
                        "E" = "E"
                      )
                    ),
                    class = "grouped-box-1"
                  ),
                  
                  column(
                    width = 3,
                    selectInput(
                      ns("s_Function"),
                      "Function Type:",
                      c("continuous" = "continuous", "step" = "step")
                    ),
                    class = "grouped-box-2"
                  ),
                  
                  column(
                    width = 3,
                    selectInput(
                      ns("s_Stress_Scale"),
                      "Raw Stressor Scale:",
                      c("linear" = "linear", "log" = "log")
                    ),
                    class = "grouped-box-2"
                  )
                ),
                
                fluidRow(
                  column(
                    width = 3,
                    selectInput(
                      ns("s_Model"),
                      "Model Endpoint:",
                      c(
                        "All" = "All",
                        "Joe Model" = "Joe Model",
                        "Population Model" = "Population Model"
                      )
                    ),
                    class = "grouped-box-4"
                  ),
                  column(
                    width = 3,
                    selectInput(
                      ns("s_Life_stages"),
                      "Life Stages (Pop. Only):",
                      c("..." = "...", "other..." = "other...")
                    ),
                    class = "grouped-box-3"
                  ),
                  column(
                    width = 3,
                    selectInput(
                      ns("s_Parameters"),
                      "Vital Rate Parameters (Pop. Only):",
                      c(
                        "survival" = "survival",
                        "capacity" = "capacity",
                        "fecundity" = "fecundity"
                      )
                    ),
                    class = "grouped-box-3"
                  ),
                  column(
                    width = 3,
                    textInput(ns("s_Units"), "Raw Stressor Units:", value = "units"),
                    class = "grouped-box-4"
                  )
                ),
                
              )), 
              
              
              
              
              fluidRow(
                shinydashboard::box(
                  width = 12,
                  
                  tags$p("Use the table below to edit and adjust the stressor-response (dose-response) relationship. Click on cells in the table to adjust values. The graph shows the dose:response relationship between the raw stressor values (x-axis) and the stressor-response score (or mean system capacity, y-axis). The red line shows the mean value, and the shading represents uncertainty or stochasticity in the relationship. The red shading represents one standard deviation, and the grey shading represents the upper and lower bounds of min and max values. Click and drag within the graph window to zoom in on particular trends; double-click the graph to zoom out to full view.", class = "small-helper-text"),
                  
                  dygraphOutput(ns("dose_response_plot"))
                )
              ),
              fluidRow(
                shinydashboard::box(
                  width = 12,
                  
                  navset_card_underline(
                    nav_panel(
                      "Stressor-Response Input Data",
                      
                      tagList(
                        tags$p(
                          "Edit the underlying Stressor-Response Relationship. Double-click a cell to edit its value. Remember the SD, Lower Limit, and Upper Limit define stochasticity and uncertainty of the response score for a given level of a stressor. Do not forget to click the green save button at the bottom before closing the tab.",
                          class = "small-helper-text"
                        ),
                        DTOutput(ns("stressor_response_dt"))
                      )
                    ),
                    
                    nav_panel("Raw Distribution", tagList(fluidRow(
                      column(
                        width = 12,
                        tags$b("Distribution of Raw Stressor Values:"),
                        tags$p(textOutput(ns("text_preview"))),
                        plotOutput(ns("hist_vals_plot")),
                      )
                      
                    ))), 
                    
                    nav_panel("Correlated Stressors", tagList("...")),
                    nav_panel("Location SR Scores", tagList("..."))
                    
                  ),
                  
                  actionButton(
                    ns("close_sr_modal"),
                    label = tagList(icon("save"), "Save SR Function Updates and Close Module"),
                    style = "margin: 15px; color: white;",
                    class = "btn btn-success"
                  )
                )
              )
            ),
            easyClose = TRUE,
            size = "l",
            footer = NULL
          ))
        }
      })
      
      # main_map-BEC Unit Score runoff-var_id
      # #main_map-MWAT Max. Wekly Avg. Strm Tmp-var_id


      #-------------------------------------------------------
      # Close stressor response modal with custom button
      #-------------------------------------------------------
      observeEvent(input$close_sr_modal, {
        print("sr modal closed")
        # Trigger reloaf of data
        session$userData$rv_srdt$reload <- 2
        print(session$userData$rv_srdt$reload)
        removeModal()
      })



      #-------------------------------------------------------
      # Stressor response modal dialog
      #-------------------------------------------------------
      # Add table data as data table
      # This is the doese response relationship for each stressor
      output$stressor_response_dt <- renderDT({
        
        names(session$userData$rv_stressor_response)
        session$userData$rv_stressor_response$interaction_names
        session$userData$rv_stressor_response$active_layer
        session$userData$rv_stressor_response$main_sheet
        
        # Get the associated data from the main sheet
        m_sheet <- session$userData$rv_stressor_response$main_sheet[session$userData$rv_stressor_response$main_sheet$Stressors == session$userData$rv_stressor_response$active_layer, ]
        
        # Do not run on app load
        req(session$userData$rv_stressor_response$active_layer)
        
        # Trigger reloaf of data
        print("----SR MODAL DATA ------")
        session$userData$rv_srdt$reload <- 1
        print(session$userData$rv_srdt$reload)
        
        # Get all SR data
        sr_data <- isolate(session$userData$rv_stressor_response$sr_dat)
        
        # Nov 9 2024 update MJB isolate causing issue with refresh
        # sr_data <- session$userData$rv_stressor_response$sr_dat
        
        # Filter for target layer
        this_var <- session$userData$rv_stressor_response$active_layer # e.g., temperature
        table_vals <- sr_data[[this_var]] # e.g., temperature

        # Build the JS DT Data Table Object
        DT::datatable(
          table_vals,
          # The Stressor column is not editable
          editable = TRUE, # list(target = "cell", disable = list(columns = c(1))),
          colnames = c(
            "Raw Stressor Value" = "value", "Stressor-Response Score (0-100)" = "mean_system_capacity",
            "SD of Resp. (0-100)" = "sd", "Lower Limit of Resp. (0)" = "lwr", "Upper Limit of Resp. (100)" = "upr"
          ),
          filter = "none",
          selection = "none",
          rownames = FALSE,
          class = "cell-border stripe",
          options = list(
            pageLength = 500,
            info = FALSE,
            dom = "t",
            ordering = FALSE,
            columnDefs = list(list(
              className = "dt-left", targets = "_all"
            ))
          )
        )
      })

      # Create a proxy for the above table
      dt_proxy <- DT::dataTableProxy("main_map-Foot_flow-stressor_response_dt")
      
      
      #-------------------------------------------------------
      # Summary histogram of values
      #-------------------------------------------------------
      output$hist_vals_plot <- renderPlot({
        
        # Do not run on app load
        req(session$userData$rv_stressor_response$active_layer)
        
        print("Generating barplot...")
        
        # Get all SR data - and keep updating the plot as values are moved
        sr_data <- session$userData$rv_stressor_response$sr_dat
        # Filter for target layer
        this_var <- session$userData$rv_stressor_response$active_layer # e.g., temperature
        table_vals <- sr_data[[this_var]] # e.g., temperature
        
        # Get all stressor values
        # head(session$userData$rv_stressor_magnitude$sm_dat)
        
        sm_dat <- isolate(session$userData$rv_stressor_magnitude$sm_dat)
        sm_dat <- sm_dat[which(sm_dat$Stressor == this_var), ]
        
        hist(sm_dat$Mean, main = NA, xlab = this_var)
        for(ll in 1:length(table_vals$value)) {
          abline(v = table_vals$value[ll], col = "purple", lty = 2, lwd = 2)
        }
        
      })


      #-------------------------------------------------------
      # Populate summary statistics for stressor in modal
      #-------------------------------------------------------
      # Display mean min and max
      output$text_preview <- renderText({
        #this_var <- isolate(session$userData$rv_stressor_response$active_layer) # e.g., temperature # OLD
        this_var <- session$userData$rv_stressor_response$active_layer # New Nov 9th 2024
        # Stressor magnitude data
        sm_df <- session$userData$rv_stressor_magnitude$sm_dat
        # Subset to targer variable
        sm_sub <- sm_df[which(sm_df$Stressor == this_var), ]
        my_mean <- round(mean(sm_sub$Mean, na.rm = TRUE), 2)
        my_median <- round(median(sm_sub$Mean, na.rm = TRUE), 2)
        my_min <- min(sm_sub$Mean, na.rm = TRUE)
        my_max <- max(sm_sub$Mean, na.rm = TRUE)
        
        data_rng_txt <- paste0("Summary of raw stressor values, Mean: ", my_mean, ", Median: ", my_median, " (Min: ", my_min, ", Max: ", my_max, ")")
        return(data_rng_txt)
      })



      #------------------------------------------------------------------------
      # Update a data value cell
      #------------------------------------------------------------------------
      # When there is an edit to a cell
      # update the stessor response reactive values
      observeEvent(input$stressor_response_dt_cell_edit, {
        
        # Get new value of edited cell
        info <- input$stressor_response_dt_cell_edit

        # Index list of stressor names
        this_var <- isolate(session$userData$rv_stressor_response$active_layer) # e.g., temperature

        # print(paste0("Edit value for ... ", this_var))

        # HUCs currently selected
        selected_raw <- session$userData$rv_clickedIds$ids
        # Fix format
        getID <- function(x) {
           strsplit(x, "\\|")[[1]][1]
        }
        selected_ids <- lapply(selected_raw, getID) %>% unlist()

        info$value <- as.numeric(info$value)
        info$value <- ifelse(is.na(info$value), 0, info$value)

        i <- as.numeric(info$row)
        j <- as.numeric(info$col)
        j <- j + 1 # Weird issue with row names missing
        k <- as.numeric(info$value)

        # Ensure value is ok
        if (j > 1) {
          # Keep system capacity within reasonable bounds
          k <- ifelse(k < 0, 0, k)
          k <- ifelse(k > 100, 100, k)
        }

        var <- c("value", "mean_system_capacity", "sd", "lwr", "upr")
        
        # Ensure that lower and upper bounds are never greater than or less than msc
        srow_vals <- isolate(session$userData$rv_stressor_response$sr_dat[[this_var]][i,])
        # Current mean system capacity
        c_msc <- srow_vals$mean_system_capacity
        
        if (j == 4) { # lwr
          # Lower bounds must always be less than msc
          k <- ifelse(k > c_msc, c_msc, k)
        }
        if (j == 5) { # upr
          # Upper bounds must always be greater than msc
          k <- ifelse(k < c_msc, c_msc, k)
        }
        
        # Update the table without reloading completely
        updated_data_tmp <- isolate(session$userData$rv_stressor_response$sr_dat[[this_var]])
        updated_data_tmp[i, j] <- k
        replaceData(dt_proxy, updated_data_tmp, resetPaging = FALSE) 
        # Set resetPaging to FALSE to keep the current paging

        # Update stressor response value
        session$userData$rv_stressor_response$sr_dat[[this_var]][i, j] <- k
        # Update the DT data table so the user knows what they have just done

        # Also invalidate the cumulative system capacity score in the stressor magnitude table
        print("click csc invalidated...")
        session$userData$rv_clickedIds_csc$csc <- NA
        

        
      })



      #------------------------------------------------------------------------
      # renderDygraph charts data visualization
      #------------------------------------------------------------------------
      output$dose_response_plot <- renderDygraph({

        # Filter for target layer
        this_var <- session$userData$rv_stressor_response$active_layer # e.g., temperature

        # print(paste0(this_var, " this_var in renderDygraph()"))
        # Get all SR data
        table_vals <- session$userData$rv_stressor_response$sr_dat[[this_var]]
        table_vals$lwr_sd <- table_vals$mean_system_capacity - table_vals$sd
        table_vals$upr_sd <- table_vals$mean_system_capacity + table_vals$sd
        table_vals <- table_vals[order(table_vals$mean_system_capacity), ]
        table_vals <- table_vals[order(table_vals$value), ]

        # Fix lower and upper sd bounds to be within range of limits
        table_vals$lwr_sd <- ifelse(table_vals$lwr_sd < table_vals$lwr, table_vals$lwr, table_vals$lwr_sd)
        table_vals$upr_sd <- ifelse(table_vals$upr_sd > table_vals$upr, table_vals$upr, table_vals$upr_sd)

        # Ensure no bad values
        table_vals <- table_vals[, c("value", "mean_system_capacity", "lwr", "upr", "lwr_sd", "upr_sd")]

        # Pretty label for plot
        pretty_lab <- gsub("_", " ", paste0("Stressor-Response Curve for ", this_var))

        # X-axis mouse-over formatting
        myvFormatter <- "function formatValue(v) {
              var prefix = 'Raw Stressor: ';
              return prefix + String(v);
        }"
        
        # Get additional stressor attributes
        
        # Create the x-axis label for the plot
        m_ms <- session$userData$rv_stressor_response$main_sheet
        m_ms <- m_ms[m_ms$Stressors == this_var, ]
        m_units <- m_ms$Units
        m_units <- ifelse(length(m_units) == 0, "", m_units)
        m_units <- ifelse(is.na(m_units), "", m_units)
        m_name <- gsub("_", " ", this_var)
        if(m_units != "") {
          m_name <- paste0(m_name, " (", m_units, ")")
        }
        xlab_plot <- paste0("Raw Stressor Magnitude Values: ", m_name)

        # Create the y-axis label for the plot
        m_ls <- m_ms$Life_stages
        m_ls <- ifelse(length(m_ls) == 0, "", m_ls)
        m_ls <- ifelse(is.na(m_ls), "", m_ls)
        m_param <- m_ms$Parameters
        m_param <- ifelse(length(m_param) == 0, "", m_param)
        m_param <- ifelse(is.na(m_param), "", m_param)
        m_name <- ": ("
        if(m_ls != "") {
          m_name <- paste0(m_name, "", m_ls)
        }
        if(m_param != "") {
          if(m_ls != "") {
            m_name <- paste0(m_name, " ", m_param)
          } else {
            m_name <- paste0(m_name, "", m_param)
          }
        }
        m_name <- paste0(m_name, ")")
        if(m_name == ": ()") {
          m_name <- ""
        }
        ylab_plot <- paste0("Stressor Response Score (%)", m_name)
        
        # Start and return the dygraph plot
        dygraph(table_vals, main = pretty_lab) %>%
          dyAxis("x", label = xlab_plot, valueFormatter = JS(myvFormatter)) %>%
          dyAxis("y", label = ylab_plot) %>%
          dySeries(c("lwr", "mean_system_capacity", "upr"), label = "msc", color = "grey") %>%
          dySeries(c("lwr_sd", "mean_system_capacity", "upr_sd"), label = "Response ", color = "red")
        
      })
      
      
      



      #------------------------------------------------------------------------
      #------------------------------------------------------------------------
      # render 2-factor interaction matrix table
      #------------------------------------------------------------------------
      #------------------------------------------------------------------------
      
      output$interaction_matrix_main <- DT::renderDataTable({
          print("Rendering matrix intraction table...")
          this_var <- session$userData$rv_stressor_response$interaction_names[stressor_index - length(session$userData$rv_stressor_response$stressor_names)]
          mat_data <- session$userData$rv_stressor_response$interaction_values[[this_var]]
          mtable <- mat_data$mat_msc[, 2:ncol(mat_data$mat_msc)]
          rnms <- unlist(mat_data$mat_msc[, 1])
          rnms <- as.numeric(rnms)
          mtable <- data.frame(mtable)
          rownames(mtable) <- rnms
          colnames_cust <- gsub("X", "", colnames(mtable))
          datatable(mtable,
          colnames = colnames_cust,
          )
      })

      output$interaction_matrix_sd <- DT::renderDataTable({
          print("Rendering matrix intraction table...")
          this_var <- session$userData$rv_stressor_response$interaction_names[stressor_index - length(session$userData$rv_stressor_response$stressor_names)]
          mat_data <- session$userData$rv_stressor_response$interaction_values[[this_var]]
          mtable <- mat_data$mat_sd[, 2:ncol(mat_data$mat_sd)]
          rnms <- unlist(mat_data$mat_sd[, 1])
          rnms <- as.numeric(rnms)
          mtable <- data.frame(mtable)
          rownames(mtable) <- rnms
          colnames_cust <- gsub("X", "", colnames(mtable))
          datatable(mtable,
          colnames = colnames_cust,
          )
      })

      output$interaction_matrix_ll <- DT::renderDataTable({
          print("Rendering matrix intraction table...")
          this_var <- session$userData$rv_stressor_response$interaction_names[stressor_index - length(session$userData$rv_stressor_response$stressor_names)]
          mat_data <- session$userData$rv_stressor_response$interaction_values[[this_var]]
          mtable <- mat_data$mat_ll[, 2:ncol(mat_data$mat_ll)]
          rnms <- unlist(mat_data$mat_ll[, 1])
          rnms <- as.numeric(rnms)
          mtable <- data.frame(mtable)
          rownames(mtable) <- rnms
          colnames_cust <- gsub("X", "", colnames(mtable))
          datatable(mtable,
          colnames = colnames_cust,
          )
      })

      output$interaction_matrix_ul <- DT::renderDataTable({
          print("Rendering matrix intraction table...")
          this_var <- session$userData$rv_stressor_response$interaction_names[stressor_index - length(session$userData$rv_stressor_response$stressor_names)]
          mat_data <- session$userData$rv_stressor_response$interaction_values[[this_var]]
          mtable <- mat_data$mat_ul[, 2:ncol(mat_data$mat_ul)]
          rnms <- unlist(mat_data$mat_ul[, 1])
          rnms <- as.numeric(rnms)
          mtable <- data.frame(mtable)
          rownames(mtable) <- rnms
          colnames_cust <- gsub("X", "", colnames(mtable))
          datatable(mtable,
          colnames = colnames_cust,
          )
      })

      # Interaction matrix text
      output$text_interaction_matrix <- renderText({
          this_var <- session$userData$rv_stressor_response$interaction_names[stressor_index - length(session$userData$rv_stressor_response$stressor_names)]
          mat_data <- session$userData$rv_stressor_response$interaction_values[[this_var]]
          mcol <- mat_data$Columns
          mrow <- mat_data$Rows
          paste0("Interpolation matrix columns are stressor ", mcol, " and rows are stressor ", mrow, ".")
      })



      # Finally return the stressor name
      return(stressor_index)
    }
  )
}
