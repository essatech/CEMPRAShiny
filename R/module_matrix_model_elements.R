#' Matrix Model Elements UI
#'
#' The UI portion of the matrix model
#'
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom shinydashboard box
#'
#' @param id The id for this module
#'
#' @return a tagList containing UI elements
#'
module_matrix_model_elements_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinydashboard::box(
      width = 12,
      
      tags$b("Density-Independent Components", style = "text-align: center;"),
      
      tags$table(class = "lam_v", style = "width: 100%;",
                 uiOutput(ns(
                   "dens_independent_comp"
                 ))),
      
      tags$br(),
      
      tags$div(class = "lam_bb",
               actionButton(
                 ns("model_component"), "Symbolic Matrix Representation"
               )),
      
      tags$div(class = "lam_bb",
               actionButton(ns("eigen_analysis"), "Projection Matrix & Eigen Analysis"))
      
    ),

    shinydashboard::box(
      width = 12,

      module_matrix_model_ltre_ui(ns("matrix_model_ltre"))

    )

  )
  
}


#' Matrix Model Elements SERVER
#'
#' @param none
#'
#' @return None
#'
module_matrix_model_elements_server <- function(id) {
  
  moduleServer(id,
               function(input, output, session) {
                 
                 ns <- session$ns
                 
                 print("matrix model elements server")
                 module_matrix_model_ltre_server("matrix_model_ltre")

                 #-------------------------------------------------------
                 # Density-Independent Components
                 #-------------------------------------------------------
                 observe({
                   
                   print("pop mod dens-indepent matrix elements...")
                   
                   # Reset assume we are clear of errors ...
                   isolate({
                     session$userData$rv_ea_errors$possible_error_state <- FALSE
                     session$userData$rv_ea_errors$possible_error_msg <- ""
                   })
                   
                   # Make sure we actually have the data
                   req(nrow(session$userData$rv_life_stages$dat) > 5)
                   
                   # Nstage must be specified before calculations can proceed
                   req(session$userData$rv_life_stages$dat$Value[session$userData$rv_life_stages$dat$Name == "Nstage"] > 0)
                   
                   # Gather population model inputs
                   dat <- session$userData$rv_life_stages$dat
                   
                   # Clean inputs and set to n-stage limit
                   dat <- CEMPRA::pop_model_dat_clean(dat)
                   
                   # Fix eps for non-anadromous fish
                   print("pop mod setup started...")
                   
                   # Fix vectors to match Nstage
                   n_stage <- as.numeric(dat$Value[dat$Name == "Nstage"])
                   
                   # Setup objects for population model
                   #pop_mod_setup <-
                   #  CEMPRA::pop_model_setup(life_cycles = dat)
                   
                   pop_mod_setup <- tryCatch({
                     # Your code here
                     CEMPRA::pop_model_setup(life_cycles = dat)
                   }, error = function(e) {
                     # Return error mode
                     ret <- list()
                     ret$possible_error_state <- "Loading..."
                     ret
                   })
                   
                   
                   if (pop_mod_setup$possible_error_state != "All Good") {
                     
                     print("Bad error settings")
                     session$userData$rv_ea_errors$possible_error_state <- TRUE
                     session$userData$rv_ea_errors$possible_error_msg <-
                       pop_mod_setup$possible_error_state
                     
                   } else {
                     
                     print("Parameters ok...")
                     
                     # pop_mod_setup$life_pars
                     # write.csv(pop_mod_setup$life_pars, file = "TEST.csv", row.names = FALSE)
                     # Build matrix elements for population model
                     pop_mod_mat <-
                       CEMPRA::pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
                     
                     # Preview density-independent transition projection_matrix
                     A <- pop_mod_mat$projection_matrix
                     
                     # Assign nicknames for each stage
                     # snames <-
                     #  c("egg_yoy", "juv", "subadult", "adult")
                     # rownames(A) <- colnames(A) <- snames
                     # print("Check input matrix...")
                     
                     # Simple density-independent lambda estimate
                     lambda <- popbio::lambda(A)
                     # Simple Eigen analysis
                     ea <- popbio::eigen.analysis(A)
                     
                     lambda <- round(ea$lambda1, 3)
                     damping_ratio <- round(ea$damping.ratio, 2)
                     gen_time <-
                       round(popbio::generation.time(A), 1)
                     net_repo_rate <-
                       round(popbio::net.reproductive.rate(A), 2)
                     
                     
                     # Check out symbolic matrix representations
                     # For density
                     ds <- pop_mod_setup$density_stage_symbolic
                     ds_m <-
                       matrix(as.character(ds), nrow = nrow(A), ncol = ncol(A))
                     ds_m <- t(ds_m)
                     colnames(ds_m) <- paste("s", 1:ncol(A), sep = "")
                     rownames(ds_m) <- paste("s", 1:nrow(A), sep = "")
                     
                     # For life stages
                     lss <- pop_mod_setup$life_stages_symbolic
                     lss_m <-
                       matrix(as.character(lss), nrow = nrow(A), ncol = ncol(A))
                     lss_m <- t(lss_m)
                     colnames(lss_m) <- paste("s", 1:ncol(A), sep = "")
                     rownames(lss_m) <- paste("s", 1:nrow(A), sep = "")
                     
                     
                     # Add objects to list
                     session$userData$rv_eigen_analysis$dat$lambda <- lambda
                     session$userData$rv_eigen_analysis$dat$damping_ratio <-
                       damping_ratio
                     session$userData$rv_eigen_analysis$dat$gen_time <- gen_time
                     session$userData$rv_eigen_analysis$dat$net_repo_rate <-
                       net_repo_rate
                     session$userData$rv_eigen_analysis$dat$ea <- ea
                     session$userData$rv_eigen_analysis$dat$pop_mod_mat <-
                       pop_mod_mat
                     session$userData$rv_eigen_analysis$dat$lss_m <- lss_m
                     session$userData$rv_eigen_analysis$dat$ds_m <- ds_m
                     
                   }
                   
                   
                 })
                 
                 
                 # Calculate density-independent matrix elements...
                 output$dens_independent_comp <- renderUI({
                   
                   print("Building DI comp...")
                   
                   if (session$userData$rv_ea_errors$possible_error_state) {
                     # error state - return error message
                     tl <-
                       tags$tr(tags$td(session$userData$rv_ea_errors$possible_error_msg), class = "pm-bad-inputs")
                     return(tl)
                   } else {
                     tl <- tagList(
                       tags$tr(
                         tags$td("Lambda: ", style = "vertical-align: middle;"),
                         tags$td(session$userData$rv_eigen_analysis$dat$lambda, class = "pm-cell-values")
                       ),
                       tags$tr(
                         tags$td("Damping Ratio: ", style = "vertical-align: middle;"),
                         tags$td(session$userData$rv_eigen_analysis$dat$damping_ratio, class = "pm-cell-values")
                       ),
                       tags$tr(
                         tags$td("Generation Time: ", style = "vertical-align: middle;"),
                         tags$td(session$userData$rv_eigen_analysis$dat$gen_time, class = "pm-cell-values")
                       ),
                       tags$tr(
                         tags$td("Net Reproductive Rate: ", style = "vertical-align: middle;"),
                         tags$td(session$userData$rv_eigen_analysis$dat$net_repo_rate, class = "pm-cell-values"),
                       )
                     )
                     return(tl)
                   }
                 })
                 
                 
                 
                 
                 
                 # Symbolic Transition matrix data table
                 output$dt_lss_m <- renderDataTable({
                   
                   print("Building DT1...")
                   
                   # Get the transition matrix
                   A <- (session$userData$rv_eigen_analysis$dat$lss_m)
                   # Add names to column
                   #mnames <-
                   #   c("Stage 1", "Stage 2", "Stage 3", "Stage 4")
                   mnames <- paste("Stage ", 1:ncol(A), sep = "")
                   
                   # Check if population is being run in anadromous mode
                   anadrmous <- session$userData$rv_eigen_analysis$dat$pop_mod_mat$anadrmous
                   
                   if(anadrmous) {
                     # Update stage names
                     mnames <- session$userData$rv_eigen_analysis$dat$pop_mod_mat$life_histories$stage_names
                     mnames <- gsub("_", " ", mnames)
                     mnames <- gsub("stage", "Stage", mnames)
                   }
                   
                   colnames(A) <- mnames
                   rownames(A) <- mnames
                   # Build the JS DT Data Table Object
                   DT::datatable(
                     A,
                     editable =  FALSE,
                     caption = "Symbolic Representation of the Transition matrix (B)",
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
                 
                 
                 # Symbolic Transition matrix data table
                 output$dt_ds_m <- renderDataTable({
                   
                   print("Building DT2...")
                   
                   # Get the transition matrix
                   A <- (session$userData$rv_eigen_analysis$dat$ds_m)
                   
                   # Add names to column
                   mnames <- paste("Stage ", 1:ncol(A), sep = "")
                   
                   # Check if population is being run in anadromous mode
                   anadrmous <- session$userData$rv_eigen_analysis$dat$pop_mod_mat$anadrmous
                   
                   if(anadrmous) {
                     # Update stage names
                     mnames <- session$userData$rv_eigen_analysis$dat$pop_mod_mat$life_histories$stage_names
                     mnames <- gsub("_", " ", mnames)
                     mnames <- gsub("stage", "Stage", mnames)
                   }
                   
                   
                   colnames(A) <- mnames
                   rownames(A) <- mnames
                   # Build the JS DT Data Table Object
                   DT::datatable(
                     A,
                     editable =  FALSE,
                     caption = "Symbolic Representation of the Density-Dependence Matrix (D)",
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
                 
                 
                 
                 # Transition matrix data table
                 output$dt_transition_matrix <- renderDataTable({
                   
                   print("Building DT3...")
                   
                   # Get the transition matrix
                   A <-
                     round(session$userData$rv_eigen_analysis$dat$pop_mod_mat$projection_matrix,
                           3)
                   # Add names to column
                   mnames <- paste("Stage", 1:ncol(A))
                   
                   # Check if population is being run in anadromous mode
                   anadrmous <- session$userData$rv_eigen_analysis$dat$pop_mod_mat$anadrmous
                   
                   if(anadrmous) {
                     # Update stage names
                     mnames <- session$userData$rv_eigen_analysis$dat$pop_mod_mat$life_histories$stage_names
                     mnames <- gsub("_", " ", mnames)
                     mnames <- gsub("stage", "Stage", mnames)
                   }
                   
                   colnames(A) <- mnames
                   rownames(A) <- mnames
                   # Build the JS DT Data Table Object
                   DT::datatable(
                     A,
                     editable =  FALSE,
                     caption = "Transition matrix",
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
                 
                 # Sensitivities matrix data table
                 output$dt_sensitivities_matrix <- renderDataTable({
                   
                   print("Building DT4...")
                   
                   A2 <-
                     round(session$userData$rv_eigen_analysis$dat$ea$sensitivities, 3)
                   
                   mnames <- paste("Stage", 1:ncol(A2))
                   
                   # Check if population is being run in anadromous mode
                   anadrmous <- session$userData$rv_eigen_analysis$dat$pop_mod_mat$anadrmous
                   
                   if(anadrmous) {
                     # Update stage names
                     mnames <- session$userData$rv_eigen_analysis$dat$pop_mod_mat$life_histories$stage_names
                     mnames <- gsub("_", " ", mnames)
                     mnames <- gsub("stage", "Stage", mnames)
                   }
                   
                   
                   colnames(A2) <- mnames
                   rownames(A2) <- mnames
                   
                   DT::datatable(
                     A2,
                     editable =  FALSE,
                     caption = "Sensitivity Matrix",
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
                 
                 # elasticities matrix data table
                 output$dt_elasticities_matrix <- renderDataTable({
                   
                   print("Building DT5...")
                   
                   A3 <-
                     round(session$userData$rv_eigen_analysis$dat$ea$elasticities, 3)
                   
                   mnames <- paste("Stage", 1:ncol(A3))
                   
                   # Check if population is being run in anadromous mode
                   anadrmous <- session$userData$rv_eigen_analysis$dat$pop_mod_mat$anadrmous
                   
                   if(anadrmous) {
                     # Update stage names
                     mnames <- session$userData$rv_eigen_analysis$dat$pop_mod_mat$life_histories$stage_names
                     mnames <- gsub("_", " ", mnames)
                     mnames <- gsub("stage", "Stage", mnames)
                   }
                   
                   
                   colnames(A3) <- mnames
                   rownames(A3) <- mnames
                   
                   DT::datatable(
                     A3,
                     editable =  FALSE,
                     caption = "Elasticities Matrix",
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
                 
                 # other matrix data table
                 output$dt_stablestage_matrix <- renderDataTable({
                   
                   print("Building DT6...")
                   
                   repro <-
                     round(session$userData$rv_eigen_analysis$dat$ea$repro.value, 2)
                   ss <-
                     round(session$userData$rv_eigen_analysis$dat$ea$stable.stage, 2)
                   repro <- data.frame(t(repro))
                   ss <- data.frame(t(ss))
                   repro_ss <- rbind(repro, ss)
                   
                   mnames <- paste("Stage", 1:length(ss))
                   
                   # Check if population is being run in anadromous mode
                   anadrmous <- session$userData$rv_eigen_analysis$dat$pop_mod_mat$anadrmous
                   
                   if (anadrmous) {
                     # Update stage names
                     mnames <- session$userData$rv_eigen_analysis$dat$pop_mod_mat$life_histories$stage_names
                     mnames <- gsub("_", " ", mnames)
                     mnames <- gsub("stage", "Stage", mnames)
                   }
                   
                   colnames(repro_ss) <-
                     mnames
                   rownames(repro_ss) <-
                     c("Repro. Value", "Stable Stage")
                   
                   DT::datatable(
                     repro_ss,
                     editable =  FALSE,
                     caption = "Reproductive Values & Stable Stage Distribution (0 - 1)",
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

                 output$lambda_txt <-
                   renderText({
                     paste0("Lambda: ", session$userData$rv_eigen_analysis$dat$lambda)
                   })
                 
                 
                 
                 #-------------------------------------------------------
                 # Density-Independent Modal
                 #-------------------------------------------------------
                 observeEvent(input$eigen_analysis, {
                   print("Eigen Analysis...")
                   
                   showModal(
                     modalDialog(
                       title = "Full Matrix Eigen Analysis",
                       tagList(
                         tags$p(
                           "The following tables represent outputs from an eigen analysis of the stage-structured matrix model. Note that these values are only relevant for density-independent growth conditions and will be misleading if not interpreted alongside density-dependent constraints."
                         ),
                         
                         tags$p(textOutput(ns("lambda_txt"))),
                         tags$p(
                           "The Lambda value represents the intrinsic population growth rate (at stable stage & equilibrium conditions). Lambda values greater than 1.0 indicate the population will increase and lambda values less than one indicate the population will decrease."
                         ),
                         
                         
                         tags$p(
                           "The following table shows the transition matrix for density-independent growth. The values represented here are adjusted for survival, growth, reproduction & the sex ratio, but they do not consider density-dependent constraints on population growth."
                         ),
                         DT::dataTableOutput(ns("dt_transition_matrix")),
                         tags$br(),
                         
                         tags$p(
                           "The next table shows the sensitivities matrix. What effect does an absolute change in a vital rate have on lambda? For example, if we change first-year survival by 0.001, how much will that affect the population growth rate?"
                         ),
                         DT::dataTableOutput(ns("dt_sensitivities_matrix")),
                         tags$br(),
                         
                         tags$p(
                           "The next table shows the elasticities matrix. What effect does a proportional change in vital rate have on population growth. For example, if we change first-year survival by 1%, how much will that affect population growth?"
                         ),
                         DT::dataTableOutput(ns("dt_elasticities_matrix")),
                         tags$br(),
                         
                         tags$p(
                           "The next table shows the reproductive value and stable stage distribution for each life stage. The reproductive value shows the value of a given stage as a seed for population growth (the first age class has a reproductive value of 1.0 by definition). The stable stage distribution represents the proportion of the population in each stage under hypothetical non-stochastic density-independent growth conditions (e.g., what proportion of the total population are juvenile, adults etc.?)."
                         ),
                         DT::dataTableOutput(ns("dt_stablestage_matrix")),
                         tags$br(),
                         
                       ),
                       easyClose = TRUE,
                       size = "l",
                       footer = NULL
                     )
                   )
                 })
                 
                 
                 
                 
                 #-------------------------------------------------------
                 # Density-Dependent Modal
                 #-------------------------------------------------------
                 observeEvent(input$model_component, {
                   print("Model Component...")
                   
                   showModal(
                     modalDialog(
                       title = "Model Component",
                       tagList(
                         tags$p(
                           "Under density-dependent growth conditions the projection matrix (A) is the product of the transition matrix (B), consisting of the life history charactersitcis, and the density-dependce matrix (D). A = B*D."
                         ),
                         DT::dataTableOutput(ns("dt_lss_m")),
                         DT::dataTableOutput(ns("dt_ds_m")),
                       ),
                       easyClose = TRUE,
                       size = "l",
                       footer = NULL
                     ),
                   )
                 })

               })
}