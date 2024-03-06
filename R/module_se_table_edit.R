#' SE Project Details
module_se_table_edit_server <-
  function(id,
           modal_title,
           row_to_edit,
           modal_trigger) {
    moduleServer(id,
                 function(input, output, session) {
                   
                   ns <- session$ns
                   
                   # modal_trigger() originates from input$add_row
                   # input$car_id_to_edit in table module
                   
                   observeEvent(modal_trigger(), {
                     
                     hold <- row_to_edit()
                     
                     
                     output$restoration_effort <- renderPlot({
                       
                       par(mfrow = c(2, 1))
                       
                       # Get stressor response function (isolate)
                       se_dat <- isolate({ session$userData$rv_se_inputs$socioeconomic_inputs })
                       
                       # Re-run the basic SE model
                       se_dat <- SocioEconomicRun(socioeconomic_inputs = se_dat)
                       
                       
                       # Get the stressor response curve
                       sr_dat <- isolate({ session$userData$rv_stressor_response$sr_dat[[hold$linked_stressor]] })
                       sr_dat <- as.data.frame(sr_dat)
                       
                       # Get the stressor magnitude data
                       sm_dat <- isolate({ session$userData$rv_stressor_magnitude$sm_dat })
                       sm_dat <- sm_dat[sm_dat$HUC_ID == hold$id & sm_dat$Stressor == hold$linked_stressor, ]
                       
                       # browser()
                       
                       # Start primary stressor response plot
                       plot(sr_dat[, 1], sr_dat[, 2], type = 'l', las = 1,
                            main = "CHANGE IN RESPONSE",
                            xlab = paste0("Stressor: ", sm_dat$Stressor[1]),
                            ylab = "Response (CSC %)")
                       grid()
                       
                       # Use linear interpolation approx to determine unknown
                       # y value for vector of x values
                       sm_dat$Response <- approx(sr_dat[, 1], sr_dat[, 2], xout = sm_dat$Mean, rule = 2)$y
                       
                       points(sm_dat$Mean, sm_dat$Response,
                              col = "red", pch = 13, cex = 2, lwd = 2)
                       
                       # Show with stressor reduction
                       rdu <- se_dat$stressor_reductions
                       rdu <- rdu[rdu$id == hold$id & rdu$action == hold$action & rdu$linked_stressor == hold$linked_stressor, ]
                       
                       m_change <- rdu$stressor_reductions[1]
                       
                       # Recalculate stressor response score
                       v_adj <- sm_dat$Mean + m_change
                       sm_dat$Response2 <- approx(sr_dat[, 1], sr_dat[, 2], xout = v_adj, rule = 2)$y
                       
                       # Show on plot
                       points(sm_dat$Mean, sm_dat$Response2,
                              col = "darkgreen", pch = 8, cex = 2, lwd = 2)
                       
                       
                       # Find stressor reduction curve
                       sr_curve <- rdu$sr_curve_id[1]
                       
                       rdc <- se_dat$sr_curvs[[sr_curve]]
                       rdc <- as.data.frame(rdc)
                       
                       # Plot out the stressor reduction curve
                       plot(rdc[, 1], rdc[, 2], type = 'l', las = 1,
                            main = "RESTORATION EFFORT",
                            xlab = rdu$scaled_units[1], lwd = 1.5,
                            ylab = paste0("Change in: ", rdu$linked_stressor[1]))
                       grid()
                       
                       # Point of zero action
                       points(rdc[1, 1], rdc[1, 2],
                              col = "red", pch = 13, cex = 2, lwd = 2)
                       
                       
                       # Show on plot
                       points(rdu$rest_density, rdu$stressor_reductions,
                              col = "darkgreen", pch = 8, cex = 2, lwd = 2)
                       
                     })
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     showModal(modalDialog(
                       tagList(
                         fluidRow(column(width = 12,
                                         tags$p(hold$action),
                                         tags$b(paste0(
                                           hold$id, ": ", hold$loc_name
                                         )))),
                         
                         fluidRow(column(
                           width = 6,
                           numericInput(
                             ns('n_units'),
                             'Amount (units)',
                             value = ifelse(is.null(hold), "", hold$n_units),
                             min = 0,
                             step = 0.1
                           )
                         ),
                         column(width = 6, tags$p(hold$measurement))),
                         
                         
                         fluidRow(column(
                           width = 12,
                           plotOutput(ns("restoration_effort"), height = "600px")
                         )),
                         
                       ),
                       title = hold$action,
                       size = 'l',
                       footer = list(
                         modalButton('Cancel'),
                         actionButton(
                           ns('submit'),
                           'Save',
                           class = "btn btn-primary",
                           style = "color: white"
                         )
                       )
                     )) # end of module UI
                     
                     # Observe event for "Model" text input in Add/Edit Car Modal
                     # `shinyFeedback`
                     observeEvent(input$n_units, {
                       
                       print("Update plots here...")
                       
                     })
                   }) # end of modal_trigger()
                   
                   
                   
                   edit_row_dat <- reactive({
                     
                     hold <- row_to_edit()
                     
                     print("edit_row_dat...")
                     browser()
                     
                     out <- list(
                       "id_" = if (is.null(hold))
                         uuid::UUIDgenerate()
                       else
                         hold$id_,
                       "model" = input$model,
                       "mpg" = input$mpg,
                       "cyl" = input$cyl,
                       "disp" = input$disp,
                       "hp" = input$hp,
                       "drat" = input$drat,
                       "wt" = input$wt,
                       "qsec" = input$qsec,
                       "vs" = input$vs,
                       "am" = input$am,
                       "gear" = input$gear,
                       "carb" = input$carb
                     )
                     
                     out
                   })
                   
                   validate_edit <- eventReactive(input$submit, {
                     dat <- edit_row_dat()
                     
                     # Logic to validate inputs...
                     
                     dat
                   })
                   
                   observeEvent(validate_edit(), {
                     
                     removeModal()
                     
                     dat <- validate_edit()
                     
                     print("Update reactive...")
                     browser()
                     
                     tryCatch({
                       
                       # creating a new car
                       uid <- uuid::UUIDgenerate()
                       
                       dbExecute(
                         conn,
                         "INSERT INTO mtcars (uid, id_, model, mpg, cyl, disp, hp, drat, wt, qsec, vs, am,
        gear, carb, created_at, created_by, modified_at, modified_by, is_deleted) VALUES
        ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19)",
                         params = c(list(uid),
                                    unname(dat))
                       )
                       
                       session$userData$db_trigger(session$userData$db_trigger() + 1)
                       
                       showToast("success", paste0(modal_title, " Success"))
                       
                     }, error = function(error) {
                       
                       msg <- "Error Adding Car"
                       
                       # print `msg` so that we can find it in the logs
                       print(msg)
                       
                       # print the actual error to log it
                       print(error)
                       
                       # show error `msg` to user.  User can then tell us about error and we can
                       # quickly identify where it cam from based on the value in `msg`
                       showToast("error", msg)
                       
                     })
                   })
                   
                 })
  }
