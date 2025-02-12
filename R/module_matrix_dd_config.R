# module_dd_config.R

#' Density Dependence Function Configuration UI
#'
#' @param id Module ID.
#'
#' @return A tagList containing UI elements.
module_matrix_dd_config_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    tags$h4("Define Density Dependence Bottlenecks: with life stage-specific carrying capacity"),
    shinyjs::useShinyjs(),  # Include shinyjs
    fluidRow(
      column(
        width = 12,
        tags$p("This page shows the reactive density dependence data stored in session$userData$rv_life_stages$dat.
               Modify the rows below to update density-dependent settings. Only rows whose Name begins with 'bh_' or 'hs_' and have Value == 1
               are considered density dependence entries.")
      )
    ),
    fluidRow(
      column(
        width = 12,
        actionButton(ns("add_dd"), "Add Bottleneck Row"),
        actionButton(ns("remove_dd"), "Remove Bottleneck Row")
      )
    ),
    fluidRow(
      column(width = 6, tags$p("DD Function Type:")),
      column(width = 6, tags$p("Target Life Stage:"))
    ),
    # Dynamic UI for the density dependence rows.
    uiOutput(ns("dd_configurations_ui")),
    # The Apply Updates button.
    fluidRow(
      column(
        width = 12,
        actionButton(ns("apply_updates"), "Apply Density-Dependant Constraints", style="color: white; background-color: green;")
      )
    ),
    fluidRow(
      column(
        width = 12,
        span(textOutput(ns("concatenated_summary")), style="color:darkgreen; font-weight:bold; font-size:14;")
      )
    )
  )
}

#' Density Dependence Function Configuration Server Module
#'
#' @param id Module ID.
#' @param anadromous A reactive expression that returns a logical value indicating whether the life history is anadromous.
#' @param Nstage A reactive expression that returns the number of stages.
#'
#' @return None. (Side effects: sets up the dynamic UI and data updating.)
module_matrix_dd_config_server <- function(id, anadromous, Nstage) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --------------------------------------------------------------------------
    # 0. Initialize reactive data if needed.
    # --------------------------------------------------------------------------
    if (is.null(session$userData$rv_life_stages)) {
      session$userData$rv_life_stages <- reactiveValues(
        dat = data.frame(
          Parameters = character(0),
          Name = character(0),
          Value = numeric(0),
          stringsAsFactors = FALSE
        )
      )
    }
    
    # A flag to mark when the UI has been initialized from the reactive data.
    rv_initialized <- reactiveVal(FALSE)
    
    # --------------------------------------------------------------------------
    # 1. Reactive value to track the number of dynamic rows.
    # --------------------------------------------------------------------------
    rv_dd_count <- reactiveVal(0)
    
    observeEvent(input$add_dd, {
      rv_dd_count(rv_dd_count() + 1)
    })
    observeEvent(input$remove_dd, {
      newCount <- rv_dd_count() - 1
      if (newCount < 0) newCount <- 0
      rv_dd_count(newCount)
    })
    
    # --------------------------------------------------------------------------
    # 2. Render the dynamic UI rows based on rv_dd_count().
    # --------------------------------------------------------------------------
    output$dd_configurations_ui <- renderUI({
      n <- rv_dd_count()
      if (n == 0) return(NULL)
      tagList(
        lapply(seq_len(n), function(i) {
          fluidRow(
            column(
              width = 6,
              selectInput(
                ns(paste0("dd_function_type_", i)),
                label = NULL,
                choices = c("Select Function" = "Select Function",
                            "Beverton-Holt (bh_)" = "bh_",
                            "Hockey Stick (hs_)" = "hs_")
              )
            ),
            column(
              width = 6,
              selectInput(
                ns(paste0("on_life_stage_", i)),
                label = NULL,
                choices = if (isTRUE(anadromous()))
                  c("Select Life Stage", "stage_0",
                    paste0("stage_pb_", seq_len(as.numeric(Nstage()))),
                    paste0("stage_b_", seq_len(as.numeric(Nstage()))),
                    "spawners")
                else
                  c("Select Life Stage", "stage_0",
                    paste0("stage_", seq_len(as.numeric(Nstage()))))
              )
            )
          )
        })
      )
    })
    
    # Prevent suspending the dynamic UI when the tab is hidden.
    outputOptions(output, "dd_configurations_ui", suspendWhenHidden = FALSE)
    
    # --------------------------------------------------------------------------
    # 3. Render a concatenated summary of the valid density-dependent inputs.
    #     (Only the values stored in session$userData$rv_life_stages$dat that have a Name
    #      starting with "bh_" or "hs_" and Value == 1 will be shown.)
    # --------------------------------------------------------------------------
    output$concatenated_summary <- renderText({
      dat <- session$userData$rv_life_stages$dat
      if (is.null(dat) || nrow(dat) == 0) return("No DD configurations defined.")
      validRecords <- dat[grepl("^(bh_|hs_)", dat$Name) & (dat$Value == 1), , drop = FALSE]
      if (nrow(validRecords) == 0) return("No DD configurations defined.")
      paste(validRecords$Name, collapse = ", ")
    })
    
    # --------------------------------------------------------------------------
    # 4. Observer: Listen for changes in the reactive data and update the UI.
    # --------------------------------------------------------------------------
    observeEvent(session$userData$rv_life_stages$dat, {
      dat <- session$userData$rv_life_stages$dat
      if (is.null(dat)) return()
      # Filter for density-dependent records (Name starts with "bh_" or "hs_" and Value == 1).
      validRecords <- dat[grepl("^(bh_|hs_)", dat$Name) & (dat$Value == 1), , drop = FALSE]
      nValid <- nrow(validRecords)
      rv_dd_count(nValid)
      
      session$onFlushed(function() {
        if (nValid > 0) {
          for (i in seq_len(nValid)) {
            record <- validRecords[i, ]
            dd_val   <- substr(record$Name, 1, 3)
            life_val <- substr(record$Name, 4, nchar(record$Name))
            updateSelectInput(session, paste0("dd_function_type_", i),
                              selected = dd_val)
            updateSelectInput(session, paste0("on_life_stage_", i),
                              selected = life_val)
          }
        }
        rv_initialized(TRUE)
      }, once = TRUE)
    }, ignoreNULL = FALSE)
    
    # --------------------------------------------------------------------------
    # 5. Observer: Toggle the Apply Updates button using shinyjs::toggleState.
    #     The button is enabled only if:
    #       - There are no dynamic rows, OR
    #       - All dynamic rows have inputs that are not set to the defaults.
    # --------------------------------------------------------------------------
    observe({
      n <- rv_dd_count()
      # If there are no rows, enable the button so that clicking Apply Updates
      # will simply remove any existing DD records.
      if (n == 0) {
        shinyjs::enable("apply_updates")
        return()
      }
      valid <- TRUE
      for (i in seq_len(n)) {
        dd_val   <- input[[paste0("dd_function_type_", i)]]
        life_val <- input[[paste0("on_life_stage_", i)]]
        if (is.null(dd_val) || dd_val == "Select Function") valid <- FALSE
        if (is.null(life_val) || life_val == "Select Life Stage") valid <- FALSE
      }
      if (valid) {
        shinyjs::enable("apply_updates")
      } else {
        shinyjs::disable("apply_updates")
      }
    })
    
    # --------------------------------------------------------------------------
    # 6. Observer: Update session$userData$rv_life_stages$dat only when the user clicks Apply Updates.
    #     If there are no dynamic rows, it will remove any density-dependent records.
    # --------------------------------------------------------------------------
    observeEvent(input$apply_updates, {
      if (!rv_initialized()) return()
      n <- rv_dd_count()
      if (n == 0) {
        isolate({
          dat <- session$userData$rv_life_stages$dat
          if (!is.null(dat)) {
            dat <- dat[!grepl("^(bh_|hs_)", dat$Name), , drop = FALSE]
            session$userData$rv_life_stages$dat <- dat
          }
        })
        return()
      }
      for (i in seq_len(n)) {
        req(input[[paste0("dd_function_type_", i)]])
        req(input[[paste0("on_life_stage_", i)]])
      }
      newRecords <- data.frame(
        Parameters = rep("density_dependence", n),
        Name = sapply(seq_len(n), function(i) {
          dd_val   <- input[[paste0("dd_function_type_", i)]]
          life_val <- input[[paste0("on_life_stage_", i)]]
          paste0(dd_val, life_val)
        }),
        Value = rep(1, n),
        stringsAsFactors = FALSE
      )
      isolate({
        dat <- session$userData$rv_life_stages$dat
        if (is.null(dat)) {
          dat <- newRecords
        } else {
          dat <- dat[!grepl("^(bh_|hs_)", dat$Name), , drop = FALSE]
          # Ensure no duplicated rows in newRecords
          newRecords <- newRecords[!duplicated(newRecords$Name), , drop = FALSE]
          dat <- rbind(dat, newRecords)
        }
        session$userData$rv_life_stages$dat <- dat
      })
    })
    
  })
}
