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
                     
                     
                     
                     # -------------------------------------------------------
                     # Create reactive object in module to hold socio-economic
                     # calculations
                     # -------------------------------------------------------
                     
                     se_crs <- reactive({
                       # Get stressor response function (isolate)
                       se_dat <-
                         isolate({
                           session$userData$rv_se_inputs$socioeconomic_inputs
                         })
                       # turn off stochasticity
                       se_dat$`Location Implementation`$`SD Number of Units` <-
                         0
                       se_dat$`Management Actions`$`SD of Cost per Unit` <-
                         0
                       
                       # Get the stressor response curve
                       sr_dat <-
                         isolate({
                           session$userData$rv_stressor_response$sr_dat[[hold$linked_stressor]]
                         })
                       sr_dat <- as.data.frame(sr_dat)
                       
                       # Get the stressor magnitude data
                       sm_dat <-
                         isolate({
                           session$userData$rv_stressor_magnitude$sm_dat
                         })
                       sm_dat <-
                         sm_dat[sm_dat$HUC_ID == hold$id &
                                  sm_dat$Stressor == hold$linked_stressor,]
                       
                       
                       # Source input and update plots
                       n_units_input <- input$n_units
                       
                       if (is.null(n_units_input)) {
                         n_units_input <- 0
                       }
                       if (is.na(n_units_input)) {
                         n_units_input <- 0
                       }
                       
                       # Filter for only target project
                       tmp <- se_dat$`Location Implementation`
                       tmp <-
                         tmp[tmp$`Location ID` == hold$id &
                               tmp$`Management Action Name` == hold$action,]
                       tmp$`Mean Number of Units Restored` <-
                         n_units_input
                       
                       # Overwrite original
                       se_dat$`Location Implementation` <- tmp
                       
                       # Re-run the basic SE model
                       se_dat$stressor_reductions <- NULL
                       se_dat$cost_summary <- NULL
                       
                       
                       se_dat <-
                         SocioEconomicRun(socioeconomic_inputs = se_dat,
                                          deterministic = TRUE)
                       
                       
                       
                       # Show with stressor reduction
                       rdu <- se_dat$stressor_reductions
                       rdu <-
                         rdu[rdu$id == hold$id &
                               rdu$action == hold$action &
                               rdu$linked_stressor == hold$linked_stressor, ]
                       
                       # rest_density - 8
                       rdu <- rdu[1, ]
                       
                       # Store in list object
                       se_crs <- list(
                         se_dat = se_dat,
                         sr_dat = sr_dat,
                         sm_dat = sm_dat,
                         rdu = rdu
                       )
                       
                       return(se_crs)
                       
                     })
                     
                     
                     
                     # -------------------------------------------------------
                     # Render dynamic html to display
                     # -------------------------------------------------------
                     output$samplecalculations <- renderUI({
                       # Extract data objects to display
                       se_crs <- se_crs()
                       se_dat <- se_crs$se_dat
                       sr_dat <- se_crs$sr_dat
                       sm_dat <- se_crs$sm_dat
                       rdu <- se_crs$rdu
                       
                       # Find stressor reduction curve
                       sr_curve <- rdu$sr_curve_id[1]
                       
                       # Get the stressor reduction curve
                       rdc <- se_dat$sr_curvs[[sr_curve]]
                       rdc <- as.data.frame(rdc)
                       
                       # Calculate change in stressor magnitude
                       
                       rdu$stressor_reductions_qa <-
                         approx(rdc[, 1],
                                rdc[, 2],
                                xout = rdu$rest_density[1],
                                rule = 2)$y
                       
                       
                       # Calculate change in stressor response
                       v_adj <-
                         sm_dat$Mean[1] + rdu$stressor_reductions
                       
                       sm_dat$Response1 <-
                         approx(sr_dat[, 1],
                                sr_dat[, 2],
                                xout = sm_dat$Mean[1],
                                rule = 2)$y
                       
                       sm_dat$Response2 <-
                         approx(sr_dat[, 1], sr_dat[, 2], xout = v_adj, rule = 2)$y
                       
                       # Clean cost display
                       rdu$total_cost_clean <-
                         round(rdu$total_cost, 0)
                       
                       # Calculate cost-effectiveness of action
                       change_in_response <-
                         sm_dat$Response2 - sm_dat$Response1
                       if (change_in_response == 0) {
                         mchng <- 0
                         change_in_response <- "No Change"
                       } else {
                         mchng <- round(rdu$total_cost / change_in_response, 3)
                         change_in_response <- ""
                       }
                       
                       if (mchng <= 1000) {
                         change_in_response <- round(mchng, 0)
                         change_in_response <-
                           paste0("$", change_in_response, "/per % gain")
                       }
                       if (mchng > 1000 & mchng <= 1000000) {
                         change_in_response <- round(mchng / 1000, 1)
                         change_in_response <-
                           paste0("$", change_in_response, "k/per % gain")
                       }
                       if (mchng > 1000000) {
                         change_in_response <- round(mchng / 1000000, 2)
                         change_in_response <-
                           paste0("$", change_in_response, "million/per % gain")
                       }
                       
                       if (rdu$total_cost > 1000 &
                           rdu$total_cost <= 1000000) {
                         rdu$total_cost_clean <- round(rdu$total_cost_clean / 1000, 1)
                         rdu$total_cost_clean <-
                           paste0(as.character(rdu$total_cost_clean), " thousand")
                       }
                       if (rdu$total_cost > 1000000) {
                         rdu$total_cost_clean <- round(rdu$total_cost_clean / 1000000, 1)
                         rdu$total_cost_clean <-
                           paste0(as.character(rdu$total_cost_clean), " million")
                       }
                       
                       
                       
                       #-----------------------------------
                       # Target values for display
                       #-----------------------------------
                       
                       # rdu$id # Location ID
                       # rdu$loc_name # Location Name
                       # Create an un-ordered list as an HTML object
                       
                       tagList(fluidRow(
                         column(
                           tags$b("Restoration Action Cost: "),
                           tags$ul(
                             tags$li(
                               paste0(rdu$action, " (", rdu$measurement, "): ", rdu$n_units)
                             ),
                             tags$li(
                               paste0(
                                 "Cost per Unit: $",
                                 rdu$cost_units,
                                 " * n-units: ",
                                 rdu$n_units,
                                 " * Cost Multiplier: ",
                                 rdu$cost_multiplier
                               )
                             ),
                             tags$li(
                               paste0(
                                 "Bulk Discount Modifiers... ",
                                 rdu$bulk_multi_1,
                                 ", ",
                                 rdu$bulk_multi_2
                               )
                             ),
                             tags$li(paste0(
                               "Total Cost: $", rdu$total_cost_clean
                             ))
                           ),
                           
                           tags$b("Convert Restoration Action to Density: "),
                           tags$li(
                             paste0(rdu$action, " (", rdu$measurement, "): ", rdu$n_units)
                           ),
                           tags$ul(tags$li(
                             paste0("Location Size: ", rdu$size_var, ": ", rdu$size_vals)
                           ),
                           
                           # Restoration Density
                           tags$li(
                             paste0(
                               rdu$n_units,
                               " / ",
                               rdu$size_vals,
                               " = ",
                               round(rdu$rest_density, 3)
                             )
                           ),
                           tags$li(paste0(
                             paste0(round(rdu$rest_density, 3), ": ", rdu$scaled_units)
                           ))),
                           width = 6
                         ),
                         column(
                           tags$b("Restoration Action Effectivness: "),
                           tags$ul(
                             tags$li(
                               paste0("Stressor Reduction Curve ID: ", rdu$sr_curve_id)
                             ),
                             tags$li(paste0(
                               "Stressor Linkage: ", rdu$linked_stressor
                             )),
                             tags$li(paste0(
                               "Initial Stressor Magnitude: ", sm_dat$Mean[1]
                             )),
                             tags$li(paste0(
                               "Change in Stressor Magnitude: ",
                               round(rdu$stressor_reductions, 3)
                             )),
                             tags$li(paste0(
                               "(",
                               sm_dat$Mean[1],
                               " + ",
                               round(rdu$stressor_reductions, 3),
                               ") = ",
                               round(sm_dat$Mean[1] + rdu$stressor_reductions, 3)
                             )),
                             tags$li(paste0(
                               "New Stressor Magnitude: ", round(v_adj, 3)
                             ))
                           ),
                           
                           tags$b("Change in Stressor Response: "),
                           tags$ul(tags$li(
                             paste0("Initial Stressor Response: ", sm_dat$Response1)
                           ),
                           tags$li(
                             paste0("New Stressor Response: ", round(sm_dat$Response2, 3))
                           ),
                           tags$li(
                             paste0("Cost Effectivness: ", change_in_response)
                           )),
                           width = 6
                         )
                       ), ) # end of tag list
                       
                     })
                     
                     
                     
                     # -------------------------------------------------------
                     # Create Plots
                     # -------------------------------------------------------
                     output$restoration_effort <- renderPlot({
                       # Extract data objects to display
                       se_crs <- se_crs()
                       se_dat <- se_crs$se_dat
                       sr_dat <- se_crs$sr_dat
                       sm_dat <- se_crs$sm_dat
                       rdu <- se_crs$rdu
                       
                       # Find stressor reduction curve
                       sr_curve <- rdu$sr_curve_id[1]
                       
                       # Get the stressor reduction curve
                       rdc <- se_dat$sr_curvs[[sr_curve]]
                       rdc <- as.data.frame(rdc)
                       
                       
                       par(mfrow = c(1, 2))
                       
                       # .....................................
                       # Plot out the stressor reduction curve
                       
                       plot(
                         rdc[, 1],
                         rdc[, 2],
                         type = 'l',
                         las = 1,
                         main = "RESTORATION EFFORT",
                         xlab = rdu$scaled_units[1],
                         lwd = 1.5,
                         ylab = paste0("Change in: ", rdu$linked_stressor[1])
                       )
                       grid()
                       
                       
                       # rdc[, 3] <- 0.1
                       
                       # Create polygon with SD values
                       xseq <- c(rdc[, 1], rev(rdc[, 1]), rdc[1, 1])
                       yseq <-
                         c(rdc[, 2] + rdc[, 3], rev(rdc[, 2] - rdc[, 3]), rdc[1, 2] + rdc[1, 3])
                       
                       polygon(xseq, yseq, col = "#f0f0f0", border = NA)
                       grid()
                       
                       # Redraw mean effect
                       points(rdc[, 1], rdc[, 2], type = 'l', lwd = 1.5)
                       
                       # Add box back on plot
                       graphics::box()
                       
                       # Point of zero action
                       points(
                         rdc[1, 1],
                         rdc[1, 2],
                         col = "red",
                         pch = 13,
                         cex = 2,
                         lwd = 2
                       )
                       
                       
                       
                       xval <- rdu$rest_density
                       pch_use <- 8
                       if (xval < min(rdc[, 1])) {
                         xval <- min(rdc[, 1])
                         pch_use <- 6
                       }
                       if (xval > max(rdc[, 1])) {
                         xval <- max(rdc[, 1])
                         pch_use <- 2
                       }
                       
                       
                       # Show on plot
                       points(
                         xval,
                         rdu$stressor_reductions,
                         col = "darkgreen",
                         pch = pch_use,
                         cex = 2,
                         lwd = 2
                       )
                       
                       # --------------------------------------------------
                       # Start primary stressor response plot
                       # --------------------------------------------------
                       
                       plot(
                         sr_dat[, 1],
                         sr_dat[, 2],
                         type = 'l',
                         las = 1,
                         main = "CHANGE IN STRESSOR-RESPONSE",
                         xlab = paste0("Stressor: ", sm_dat$Stressor[1]),
                         ylab = "Response (CSC %)"
                       )
                       
                       # Create polygon with SD values
                       xseq <-
                         c(sr_dat[, 1], rev(sr_dat[, 1]), sr_dat[1, 1])
                       yseq <-
                         c(sr_dat[, 2] + sr_dat[, 3],
                           rev(sr_dat[, 2] - sr_dat[, 3]),
                           sr_dat[1, 2] + sr_dat[1, 3])
                       
                       polygon(xseq, yseq, col = "#e3e3e3", border = NA)
                       grid()
                       
                       graphics::box()
                       
                       # Redraw mean effect
                       points(sr_dat[, 1], sr_dat[, 2], type = 'l', lwd = 1.5)
                       
                       # Use linear interpolation approx to determine unknown
                       # y value for vector of x values
                       sm_dat$Response <-
                         approx(sr_dat[, 1],
                                sr_dat[, 2],
                                xout = sm_dat$Mean,
                                rule = 2)$y
                       
                       points(
                         sm_dat$Mean,
                         sm_dat$Response,
                         col = "red",
                         pch = 13,
                         cex = 2,
                         lwd = 2
                       )
                       
                       m_change <- rdu$stressor_reductions[1]
                       
                       # Recalculate stressor response score
                       v_adj <- sm_dat$Mean + m_change
                       sm_dat$Response2 <-
                         approx(sr_dat[, 1], sr_dat[, 2], xout = v_adj, rule = 2)$y
                       
                       
                       xval <- v_adj
                       pch_use <- 8
                       if (xval < min(sr_dat[, 1])) {
                         xval <- min(sr_dat[, 1])
                         pch_use <- 6
                       }
                       if (xval > max(sr_dat[, 1])) {
                         xval <- max(sr_dat[, 1])
                         pch_use <- 2
                       }
                       
                       
                       # Show on plot
                       points(
                         xval,
                         sm_dat$Response2,
                         col = "darkgreen",
                         pch = pch_use,
                         cex = 2,
                         lwd = 2
                       )
                       
                     })
                     
                     
                     
                     
                     
                     
                     # -------------------------------------------------------
                     # Create Cost-Benefit Plot
                     # -------------------------------------------------------
                     output$cost_benefit_plot <- renderPlot({
                       # Extract data objects to display
                       se_crs <- se_crs()
                       se_dat <- se_crs$se_dat
                       sr_dat <- se_crs$sr_dat
                       sm_dat <- se_crs$sm_dat
                       rdu <- se_crs$rdu
                       
                       # Find stressor reduction curve
                       sr_curve <- rdu$sr_curve_id[1]
                       
                       # Get the stressor reduction curve
                       rdc <- se_dat$sr_curvs[[sr_curve]]
                       rdc <- as.data.frame(rdc)
                       
                       # ......................................
                       # Calculate the total cost
                       # and benefit for each step
                       
                       mseq <- rdc[, 1]
                       vseq <-
                         seq(rdc[1, 1], rdc[nrow(rdc), 1], length.out = 20)
                       
                       build_list <- list()
                       
                       for (i in 1:length(vseq)) {
                         this_imp <- vseq[i]
                         
                         mstep <- se_dat$stressor_reductions
                         mstep <-
                           mstep[mstep$linked_stressor == hold$linked_stressor &
                                   mstep$id == hold$id &
                                   mstep$action == hold$action,]
                         mstep <- mstep[1,]
                         
                         # Convert imp to raw units
                         # Multiply by size column
                         n_units <- this_imp * mstep$size_vals
                         
                         # Get unit cost
                         n_cost <- mstep$cost_units
                         # Apply cost multiplier
                         n_cost <- n_cost * mstep$cost_multiplier
                         # Apply bulk discounts
                         if (n_units > mstep$bulk_thresh_1 &
                             n_units < mstep$bulk_thresh_2) {
                           n_cost <- n_cost * mstep$bulk_multi_1
                         }
                         if (n_units > mstep$bulk_thresh_2) {
                           n_cost <- n_cost * mstep$bulk_multi_2
                         }
                         # Calculate cost
                         total_cost <- round(n_units * n_cost, 2)
                         
                         # Calculate benefit
                         stressor_reduct <-
                           approx(rdc[, 1],
                                  rdc[, 2],
                                  xout = this_imp,
                                  rule = 2)$y
                         
                         # Apply effectivness correction
                         stressor_reduct <-
                           stressor_reduct * mstep$effect_multiplier
                         
                         sm1 <- sm_dat$Mean
                         sm2 <- sm_dat$Mean + stressor_reduct
                         
                         stressor_response1 <-
                           approx(sr_dat[, 1],
                                  sr_dat[, 2],
                                  xout = sm1,
                                  rule = 2)$y
                         
                         stressor_response2 <-
                           approx(sr_dat[, 1],
                                  sr_dat[, 2],
                                  xout = sm2,
                                  rule = 2)$y
                         
                         add_row <- data.frame(
                           rest_dens = this_imp,
                           n_units = n_units,
                           n_cost = n_cost,
                           total_cost = total_cost,
                           sm1 = sm1,
                           sm2 = sm2,
                           sr1 = stressor_response1,
                           sr2 = stressor_response2
                         )
                         
                         build_list[[i]] <- add_row
                         
                       }
                       
                       srben <- do.call("rbind", build_list)
                       
                       # print("Plot this...")
                       #browser()
                       
                       # Adjust total cost
                       srben$tcadj <- 0
                       mylab = "Total Cost ($)"
                       
                       if (max(srben$total_cost) > 1000 &
                           max(srben$total_cost) < 1000000) {
                         srben$tcadj <- srben$total_cost / 1000
                         mylab = "Cost ($ thousands)"
                       }
                       if (max(srben$total_cost) >= 1000000) {
                         srben$tcadj <- srben$total_cost / 1000000
                         mylab = "Cost ($ millions)"
                       }
                       
                       
                       
                       par(mfrow = c(1, 1),
                           mar = c(5.1, 4.1, 4.1, 4.1))
                       
                       plot(
                         srben$n_units,
                         srben$tcadj,
                         type = 'o',
                         las = 1,
                         main = "COST-BENEFIT",
                         xlab = paste0(
                           "Level of Action: ",
                           rdu$action[1],
                           " (",
                           rdu$measurement[1],
                           ")"
                         ),
                         lwd = 1.5,
                         col = "blue",
                         ylab = mylab,
                         axes = FALSE
                       )
                       
                       abline(v = input$n_units, lty = 2, col = "darkgreen", lwd = 1.5)
                       
                       axis(side = 2, col = "blue")
                       axis(
                         side = 1,
                         col.axis = "black",
                         col.ticks = "black"
                       )
                       title(
                         xlab = paste0(
                           "Level of Action: ",
                           rdu$action[1],
                           " (",
                           rdu$measurement[1],
                           ")"
                         ),
                         col.lab = "black"
                       )
                       title(ylab = mylab,
                             col.lab = "blue")
                       
                       par(new = TRUE)
                       
                       plot(
                         srben$n_units,
                         srben$sr2,
                         type = "o",
                         col = "purple",
                         ylim = range(srben$sr2),
                         ylab = "",
                         xlab = "",
                         axes = FALSE
                       )
                       
                       # Add the right y-axis
                       axis(side = 4,
                            at = pretty(range(srben$sr2)),
                            col = "purple")
                       mtext(
                         "Response (% CSC)",
                         side = 4,
                         line = 2,
                         col = "purple"
                       )
                       
                       legend(
                         "bottomright",
                         legend = c("Cost", "Benefit"),
                         col = c("blue", "purple"),
                         lwd = 1
                       )
                       
                       
                     })
                     
                     
                     
                     
                     
                     
                     showModal(
                       modalDialog(
                         tagList(
                           fluidRow(column(
                             width = 12,
                             tags$h3(paste0(
                               "Location: ", hold$id, ": ", hold$loc_name
                             )),
                             tags$p(paste0("Restoration Action: ", hold$action))
                           )),
                           
                           
                           fluidRow(
                             column(
                               width = 4,
                               numericInput(
                                 ns('n_units'),
                                 'Implementation Amount (units)',
                                 value = ifelse(is.null(hold), "", hold$n_units),
                                 min = 0,
                                 step = 0.1
                               )
                             ),
                             column(width = 8,
                                    tags$p(
                                      paste0(
                                        "Adjust the level of the restoration action to be implemented at this location, represented as (",
                                        hold$measurement,
                                        ") of ",
                                        hold$action
                                      )
                                    ))
                           ),
                           
                           fluidRow(column(
                             width = 12,
                             tags$b("Sample Calculations: "),
                             tags$p(
                               "The following section contains dynamic text to demonstrate and explore the calculations underlying the socio-economic module. Adjust the implementation input box (above) to see how values update. Restoration actions are first converted from raw absolute units to location-based densities. Restoration actions (represented as densities) are then used to calculate reductions in stressor levels (plot to left) and finally used to reduce stressor levels (plot to right). The cost-effectiveness shows the cumulative system capacity (response) score change per dollar spent.",
                               class = "pm-ht"
                             ),
                             uiOutput(ns("samplecalculations"))
                           )),
                           
                           fluidRow(column(
                             width = 12,
                             tags$p(
                               "Plots show reductions in stressor levels for increased restoration action (left) and the resulting change in the stressor response relationship from the restoration action. The red marker shows the default status quo condition with (no action), and the green marker shows the effect of the restoration action. Green triangle indicates data out of range.",
                               class = "pm-ht"
                             ),
                             plotOutput(ns("restoration_effort"), height = "500px")
                           )),
                           
                           fluidRow(column(
                             width = 12,
                             tags$p(
                               "Finally, we introduce a simple cost-benefit plot, where the level of restoration action (to be potentially implemented) at the target location is plotted on the x-axis. The total derived cost is plotted on the y-axis (blue line: right side), followed by the response score from the stressor-response relationship (purple line: left side). This plot aims to look for apparent trade-offs and inflection points.",
                               class = "pm-ht"
                             ),
                             plotOutput(ns("cost_benefit_plot"), height = "400px"),
                             tags$p(
                               "Remember that these plots only show simple deterministic scores without accounting for stochasticity or uncertainty. This page should, therefore, be used for calibration and exploratory purposes.",
                               class = "pm-ht"
                             ),
                           )),
                           
                           
                         ),
                         title = hold$action,
                         size = 'l',
                         footer = list(
                           modalButton('Cancel'),
                           actionButton(
                             ns('submit'),
                             'Save and Update Implementation Level',
                             class = "btn btn-primary",
                             style = "color: white"
                           )
                         ),
                         easyClose = TRUE
                       )
                     ) # end of module UI
                     
                     # Observe event for "Model" text input in Add/Edit Car Modal
                     # `shinyFeedback`
                     
                   }) # end of modal_trigger()
                   
                   
                   
                   edit_row_dat <- reactive({
                     hold <- row_to_edit()
                     print("edit_row_dat...")
                     # Get new restoration level
                     hold$n_units <- input$n_units
                     hold
                   })
                   
                   validate_edit <- eventReactive(input$submit, {
                     dat <- edit_row_dat()
                     # Logic to validate inputs...
                     dat
                   })
                   
                   observeEvent(validate_edit(), {
                     removeModal()
                     
                     dat <- validate_edit()
                     print("start tryCatch...")
                     
                     tryCatch({
                       
                       # update parent reactive object for se workbook
                       tmp <- isolate({ session$userData$rv_se_inputs$socioeconomic_inputs$`Location Implementation` })
                       
                       # Get row index
                       row_index <- which(tmp$`Location ID` == dat$id & tmp$`Management Action Name` == dat$action)
                       
                       # Update reactive object
                       session$userData$rv_se_inputs$socioeconomic_inputs$`Location Implementation`$`Mean Number of Units Restored`[row_index] <- dat$n_units
                       
                       showToast("success", paste0(modal_title, " Success"))
                       
                     }, error = function(error) {
                       
                       msg <- "Error updating implimentation level..."
                       
                       # print `msg` so that we can find it in the logs
                       print(msg)
                       
                       # show error `msg` to user.  User can then tell us about error and we can
                       # quickly identify where it cam from based on the value in `msg`
                       showToast("error", msg)
                       
                     })
                   })
                   
                 })
  }
