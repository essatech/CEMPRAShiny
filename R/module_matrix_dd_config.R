# module_dd_config.R

#' Density Dependence Function Configuration UI
#'
#' @param id Module ID.
#'
#' @return A tagList containing UI elements.
module_matrix_dd_config_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),

    tags$h4("Define Density Dependence Bottlenecks: with life stage-specific carrying capacity"),

    fluidRow(
      column(
        width = 12,
        tags$p(
          "Configure which life stages have density-dependent constraints.
          Select a function type (Beverton-Holt or Hockey-Stick) and the target life stage for each bottleneck.",
          class = "pm-ht"
        )
      )
    ),

    # Header row for the table
    fluidRow(
      column(width = 5, tags$strong("DD Function Type")),
      column(width = 5, tags$strong("Target Life Stage")),
      column(width = 2, tags$strong(""))
    ),

    tags$hr(style = "margin: 5px 0;"),

    # Dynamic UI for the density dependence rows
    uiOutput(ns("dd_rows_ui")),

    # Add row button
    fluidRow(
      column(
        width = 12,
        style = "margin-top: 10px;",
        actionButton(
          ns("add_row"),
          label = tagList(shiny::icon("plus"), " Add Bottleneck"),
          class = "btn-sm btn-default"
        )
      )
    ),

    tags$hr(style = "margin: 15px 0;"),

    # Apply button with dynamic styling
    fluidRow(
      column(
        width = 12,
        # CSS for flashing animation (yellow to orange for visibility)
        tags$style(HTML(paste0("
          @keyframes flash-warning {
            0%, 100% { background-color: #ffcc00; border-color: #e6b800; box-shadow: 0 0 8px #ffcc00; }
            50% { background-color: #ff8c00; border-color: #e67300; box-shadow: 0 0 12px #ff8c00; }
          }
          #", ns("apply_updates"), ".pending-changes {
            animation: flash-warning 1s ease-in-out infinite;
            color: #000 !important;
            font-weight: bold;
          }
          #", ns("apply_updates"), ".no-changes {
            background-color: #5cb85c;
            border-color: #4cae4c;
            color: white;
            box-shadow: none;
          }
        "))),
        actionButton(
          ns("apply_updates"),
          label = "Apply Density-Dependence Constraints",
          class = "btn no-changes",
          style = "font-weight: bold;"
        ),
        tags$span(
          id = ns("pending_indicator"),
          style = "margin-left: 10px; color: #f0ad4e; font-weight: bold; display: none;",
          shiny::icon("exclamation-triangle"),
          " Unsaved changes"
        )
      )
    ),

    # Summary of applied configurations
    fluidRow(
      column(
        width = 12,
        style = "margin-top: 10px;",
        tags$div(
          id = ns("summary_box"),
          style = "background-color: #dff0d8; border: 1px solid #d6e9c6; border-radius: 4px; padding: 10px;",
          tags$strong("Current DD Configurations: ", style = "color: #3c763d;"),
          textOutput(ns("concatenated_summary"), inline = TRUE)
        )
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

    # --------------------------------------------------------------------------
    # 1. Reactive values for managing rows
    # --------------------------------------------------------------------------

    # List of row configurations: each element is list(id, dd_type, life_stage)
    rv_rows <- reactiveVal(list())

    # Counter for generating unique row IDs
    rv_row_counter <- reactiveVal(0)

    # Track if there are pending (unsaved) changes
    rv_has_pending_changes <- reactiveVal(FALSE)

    # Flag to prevent circular updates when Apply is clicked
    # FALSE = allow data load processing, TRUE = block (during Apply)
    rv_applying <- reactiveVal(FALSE)

    # Flag to suppress input change detection during data load
    rv_loading_data <- reactiveVal(FALSE)

    # Track the last loaded data signature to detect external changes
    rv_last_data_signature <- reactiveVal("")

    # --------------------------------------------------------------------------
    # 2. Helper function to generate life stage choices
    # --------------------------------------------------------------------------
    get_life_stage_choices <- function() {
      if (isTRUE(anadromous())) {
        c(
          "Select Life Stage" = "",
          "Fry (stage_0)" = "stage_0",
          setNames(
            paste0("stage_pb_", seq_len(as.numeric(Nstage()))),
            paste0("Pre-breeder Age ", seq_len(as.numeric(Nstage())), " (stage_pb_", seq_len(as.numeric(Nstage())), ")")
          ),
          setNames(
            paste0("stage_b_", seq_len(as.numeric(Nstage()))),
            paste0("Breeder Age ", seq_len(as.numeric(Nstage())), " (stage_b_", seq_len(as.numeric(Nstage())), ")")
          ),
          "Total Spawners (spawners)" = "spawners"
        )
      } else {
        c(
          "Select Life Stage" = "",
          "Fry (stage_0)" = "stage_0",
          setNames(
            paste0("stage_", seq_len(as.numeric(Nstage()))),
            paste0("Stage ", seq_len(as.numeric(Nstage())), " (stage_", seq_len(as.numeric(Nstage())), ")")
          )
        )
      }
    }

    # --------------------------------------------------------------------------
    # 3. Initialize rows from existing data (e.g., when CSV is loaded)
    # --------------------------------------------------------------------------
    observeEvent(session$userData$rv_life_stages$dat, {
      # Skip if we're in the middle of applying changes (circular update)
      if (rv_applying()) return()

      dat <- session$userData$rv_life_stages$dat
      if (is.null(dat) || nrow(dat) == 0) {
        return()
      }

      # Create a signature of the DD data to detect changes
      dd_records <- dat[grepl("^(bh_|hs_)", dat$Name), , drop = FALSE]
      new_signature <- paste(sort(paste(dd_records$Name, dd_records$Value)), collapse = "|")

      # Skip if data hasn't actually changed
      if (new_signature == rv_last_data_signature()) {
        return()
      }

      # Mark that we're loading data to suppress input change detection
      rv_loading_data(TRUE)

      # Filter for density-dependent records
      validRecords <- dat[grepl("^(bh_|hs_)", dat$Name) & (dat$Value == 1), , drop = FALSE]

      if (nrow(validRecords) == 0) {
        rv_rows(list())
        rv_has_pending_changes(FALSE)
        rv_last_data_signature(new_signature)
        # Delay turning off loading flag to allow UI to render
        shinyjs::delay(300, {
          rv_loading_data(FALSE)
          # Ensure button shows correct state
          shinyjs::removeClass("apply_updates", "pending-changes")
          shinyjs::addClass("apply_updates", "no-changes")
          shinyjs::hide("pending_indicator")
        })
        return()
      }

      # Parse existing records into row list
      new_rows <- list()
      counter <- rv_row_counter()

      for (i in seq_len(nrow(validRecords))) {
        record <- validRecords[i, ]
        dd_type <- substr(record$Name, 1, 3)  # "bh_" or "hs_"
        life_stage <- substr(record$Name, 4, nchar(record$Name))

        counter <- counter + 1
        new_rows[[length(new_rows) + 1]] <- list(
          id = counter,
          dd_type = dd_type,
          life_stage = life_stage
        )
      }

      rv_row_counter(counter)
      rv_rows(new_rows)
      rv_has_pending_changes(FALSE)
      rv_last_data_signature(new_signature)

      # Delay turning off loading flag to allow UI to render first
      shinyjs::delay(300, {
        rv_loading_data(FALSE)
        # Ensure button shows correct state after data load
        shinyjs::removeClass("apply_updates", "pending-changes")
        shinyjs::addClass("apply_updates", "no-changes")
        shinyjs::hide("pending_indicator")
      })
    }, ignoreNULL = FALSE, priority = 100)

    # --------------------------------------------------------------------------
    # 4. Render dynamic rows UI
    # --------------------------------------------------------------------------
    output$dd_rows_ui <- renderUI({
      rows <- rv_rows()

      if (length(rows) == 0) {
        return(
          tags$div(
            style = "padding: 20px; text-align: center; color: #888; background-color: #f9f9f9; border-radius: 4px;",
            tags$em("No density-dependent bottlenecks defined. Click 'Add Bottleneck' to add one.")
          )
        )
      }

      life_stage_choices <- get_life_stage_choices()

      tagList(
        lapply(rows, function(row) {
          row_id <- row$id
          fluidRow(
            id = ns(paste0("row_container_", row_id)),
            style = "margin-bottom: 8px; padding: 5px; background-color: #fafafa; border-radius: 4px;",
            column(
              width = 5,
              selectInput(
                ns(paste0("dd_type_", row_id)),
                label = NULL,
                choices = c(
                  "Select Function" = "",
                  "Beverton-Holt" = "bh_",
                  "Hockey-Stick" = "hs_"
                ),
                selected = row$dd_type
              )
            ),
            column(
              width = 5,
              selectInput(
                ns(paste0("life_stage_", row_id)),
                label = NULL,
                choices = life_stage_choices,
                selected = row$life_stage
              )
            ),
            column(
              width = 2,
              style = "padding-top: 5px;",
              actionButton(
                ns(paste0("delete_", row_id)),
                label = "Remove",
                icon = NULL,
                class = "btn-sm btn-default",
                style = "color: #555; background-color: #e0e0e0; border-color: #ccc;",
                title = "Remove this bottleneck"
              )
            )
          )
        })
      )
    })

    # Prevent suspending when hidden
    outputOptions(output, "dd_rows_ui", suspendWhenHidden = FALSE)

    # --------------------------------------------------------------------------
    # 5. Add row handler
    # --------------------------------------------------------------------------
    observeEvent(input$add_row, {
      counter <- rv_row_counter() + 1
      rv_row_counter(counter)

      rows <- rv_rows()
      rows[[length(rows) + 1]] <- list(
        id = counter,
        dd_type = "",
        life_stage = ""
      )
      rv_rows(rows)
      rv_has_pending_changes(TRUE)
    })

    # --------------------------------------------------------------------------
    # 6. Delete row handlers and input change handlers (dynamic observers)
    # --------------------------------------------------------------------------
    # Track which rows we've set up observers for
    rv_observed_rows <- reactiveVal(character(0))

    observe({
      rows <- rv_rows()
      current_ids <- if (length(rows) > 0) sapply(rows, function(r) r$id) else integer(0)
      observed <- as.integer(rv_observed_rows())

      # Set up observers for new rows
      new_ids <- setdiff(current_ids, observed)

      for (row_id in new_ids) {
        local({
          local_id <- row_id

          # Observer for delete button
          observeEvent(input[[paste0("delete_", local_id)]], {
            current_rows <- rv_rows()
            # Remove the row with this ID
            current_rows <- Filter(function(r) r$id != local_id, current_rows)
            rv_rows(current_rows)
            rv_has_pending_changes(TRUE)
          }, ignoreInit = TRUE)

          # Observer for dd_type dropdown changes
          observeEvent(input[[paste0("dd_type_", local_id)]], {
            if (!rv_loading_data()) {
              rv_has_pending_changes(TRUE)
            }
          }, ignoreInit = TRUE)

          # Observer for life_stage dropdown changes
          observeEvent(input[[paste0("life_stage_", local_id)]], {
            if (!rv_loading_data()) {
              rv_has_pending_changes(TRUE)
            }
          }, ignoreInit = TRUE)
        })
      }

      rv_observed_rows(as.character(current_ids))
    })

    # --------------------------------------------------------------------------
    # 8. Update Apply button styling based on pending changes
    # --------------------------------------------------------------------------
    observe({
      has_changes <- rv_has_pending_changes()

      if (has_changes) {
        shinyjs::removeClass("apply_updates", "no-changes")
        shinyjs::addClass("apply_updates", "pending-changes")
        shinyjs::show("pending_indicator")
      } else {
        shinyjs::removeClass("apply_updates", "pending-changes")
        shinyjs::addClass("apply_updates", "no-changes")
        shinyjs::hide("pending_indicator")
      }
    })

    # --------------------------------------------------------------------------
    # 9. Validate inputs and enable/disable Apply button
    # --------------------------------------------------------------------------
    observe({
      rows <- rv_rows()

      # If no rows, enable (allows clearing all DD settings)
      if (length(rows) == 0) {
        shinyjs::enable("apply_updates")
        return()
      }

      # Check all rows have valid selections
      all_valid <- TRUE
      for (row in rows) {
        dd_val <- input[[paste0("dd_type_", row$id)]]
        life_val <- input[[paste0("life_stage_", row$id)]]

        if (is.null(dd_val) || dd_val == "") all_valid <- FALSE
        if (is.null(life_val) || life_val == "") all_valid <- FALSE
      }

      if (all_valid) {
        shinyjs::enable("apply_updates")
      } else {
        shinyjs::disable("apply_updates")
      }
    })

    # --------------------------------------------------------------------------
    # 10. Apply Updates handler
    # --------------------------------------------------------------------------
    observeEvent(input$apply_updates, {
      rows <- rv_rows()

      # Set applying flag to prevent the data load observer from re-processing
      rv_applying(TRUE)

      if (length(rows) == 0) {
        # Remove all DD records
        dat <- session$userData$rv_life_stages$dat
        if (!is.null(dat)) {
          dat <- dat[!grepl("^(bh_|hs_)", dat$Name), , drop = FALSE]
          session$userData$rv_life_stages$dat <- dat

          # Update signature to match new data
          dd_records <- dat[grepl("^(bh_|hs_)", dat$Name), , drop = FALSE]
          new_sig <- paste(sort(paste(dd_records$Name, dd_records$Value)), collapse = "|")
          rv_last_data_signature(new_sig)
        }
        rv_has_pending_changes(FALSE)
        shinyjs::delay(100, rv_applying(FALSE))
        return()
      }

      # Collect current values from inputs
      new_records_list <- list()
      for (row in rows) {
        dd_val <- input[[paste0("dd_type_", row$id)]]
        life_val <- input[[paste0("life_stage_", row$id)]]

        if (!is.null(dd_val) && dd_val != "" &&
            !is.null(life_val) && life_val != "") {
          new_records_list[[length(new_records_list) + 1]] <- list(
            dd_type = dd_val,
            life_stage = life_val
          )
        }
      }

      if (length(new_records_list) == 0) {
        # No valid records, remove all DD
        dat <- session$userData$rv_life_stages$dat
        if (!is.null(dat)) {
          dat <- dat[!grepl("^(bh_|hs_)", dat$Name), , drop = FALSE]
          session$userData$rv_life_stages$dat <- dat

          # Update signature
          dd_records <- dat[grepl("^(bh_|hs_)", dat$Name), , drop = FALSE]
          new_sig <- paste(sort(paste(dd_records$Name, dd_records$Value)), collapse = "|")
          rv_last_data_signature(new_sig)
        }
        rv_has_pending_changes(FALSE)
        shinyjs::delay(100, rv_applying(FALSE))
        return()
      }

      # Create new records data frame
      newRecords <- data.frame(
        Parameters = rep("density_dependence", length(new_records_list)),
        Name = sapply(new_records_list, function(r) paste0(r$dd_type, r$life_stage)),
        Value = rep(1, length(new_records_list)),
        stringsAsFactors = FALSE
      )

      # Remove duplicates
      newRecords <- newRecords[!duplicated(newRecords$Name), , drop = FALSE]

      dat <- session$userData$rv_life_stages$dat
      if (is.null(dat)) {
        dat <- newRecords
      } else {
        # Remove existing DD records and add new ones
        dat <- dat[!grepl("^(bh_|hs_)", dat$Name), , drop = FALSE]
        dat <- rbind(dat, newRecords)
      }
      session$userData$rv_life_stages$dat <- dat

      # Update signature to match the data we just applied
      dd_records <- dat[grepl("^(bh_|hs_)", dat$Name), , drop = FALSE]
      new_sig <- paste(sort(paste(dd_records$Name, dd_records$Value)), collapse = "|")
      rv_last_data_signature(new_sig)

      # Update internal row state with applied values
      rv_loading_data(TRUE)  # Suppress input change detection during row update
      updated_rows <- list()
      counter <- rv_row_counter()
      for (i in seq_along(new_records_list)) {
        counter <- counter + 1
        updated_rows[[i]] <- list(
          id = counter,
          dd_type = new_records_list[[i]]$dd_type,
          life_stage = new_records_list[[i]]$life_stage
        )
      }
      rv_row_counter(counter)
      rv_rows(updated_rows)

      rv_has_pending_changes(FALSE)

      # Delay turning off flags to allow UI to update first
      shinyjs::delay(200, {
        rv_loading_data(FALSE)
        rv_applying(FALSE)
      })
    })

    # --------------------------------------------------------------------------
    # 11. Render summary of applied configurations
    # --------------------------------------------------------------------------
    output$concatenated_summary <- renderText({
      dat <- session$userData$rv_life_stages$dat
      if (is.null(dat) || nrow(dat) == 0) return("None")

      validRecords <- dat[grepl("^(bh_|hs_)", dat$Name) & (dat$Value == 1), , drop = FALSE]
      if (nrow(validRecords) == 0) return("None")

      paste(validRecords$Name, collapse = ", ")
    })

  })
}
