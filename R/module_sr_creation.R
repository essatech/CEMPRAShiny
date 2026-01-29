# ===========================================
# Custom Stressor-response creation function
# ===========================================

module_sr_creation_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),

    # CSS for styling - SCOPED to .sr-creation-module only
    tags$head(tags$style(HTML("
      /* All styles scoped to .sr-creation-module to avoid affecting other modules */

      /* Progress indicator styles */
      .sr-creation-module .sr-progress-container {
        display: flex;
        justify-content: space-between;
        margin-bottom: 20px;
        padding: 15px 20px;
        background: linear-gradient(to right, #f8f9fa, #ffffff);
        border-radius: 8px;
        border: 1px solid #dee2e6;
      }
      .sr-creation-module .sr-progress-step {
        display: flex;
        align-items: center;
        flex: 1;
        position: relative;
      }
      .sr-creation-module .sr-progress-step:not(:last-child)::after {
        content: '';
        flex: 1;
        height: 2px;
        background-color: #dee2e6;
        margin: 0 10px;
      }
      .sr-creation-module .sr-progress-step.completed:not(:last-child)::after {
        background-color: #28a745;
      }
      .sr-creation-module .sr-step-circle {
        width: 36px;
        height: 36px;
        border-radius: 50%;
        display: flex;
        align-items: center;
        justify-content: center;
        font-weight: bold;
        font-size: 14px;
        border: 2px solid #dee2e6;
        background-color: white;
        color: #6c757d;
        flex-shrink: 0;
      }
      .sr-creation-module .sr-step-circle.active {
        border-color: #3c8dbc;
        background-color: #3c8dbc;
        color: white;
      }
      .sr-creation-module .sr-step-circle.completed {
        border-color: #28a745;
        background-color: #28a745;
        color: white;
      }
      .sr-creation-module .sr-step-label {
        margin-left: 8px;
        font-size: 12px;
        color: #6c757d;
        white-space: nowrap;
      }
      .sr-creation-module .sr-step-label.active {
        color: #3c8dbc;
        font-weight: 600;
      }
      .sr-creation-module .sr-step-label.completed {
        color: #28a745;
      }

      /* Validation checkmarks */
      .sr-creation-module .sr-validation-status {
        display: inline-flex;
        align-items: center;
        margin-left: 10px;
        font-size: 13px;
      }
      .sr-creation-module .sr-validation-status.valid {
        color: #28a745;
      }
      .sr-creation-module .sr-validation-status.invalid {
        color: #dc3545;
      }
      .sr-creation-module .sr-validation-status.pending {
        color: #6c757d;
      }

      /* Well panel styling */
      .sr-creation-module .sr-settings-well {
        background-color: #f8f9fa;
        border: 1px solid #e9ecef;
        border-radius: 6px;
        padding: 15px;
        margin-bottom: 15px;
      }
      .sr-creation-module .sr-settings-well-header {
        font-weight: 600;
        color: #495057;
        margin-bottom: 12px;
        padding-bottom: 8px;
        border-bottom: 1px solid #dee2e6;
        display: flex;
        align-items: center;
      }
      .sr-creation-module .sr-settings-well-header .fa,
      .sr-creation-module .sr-settings-well-header .fas,
      .sr-creation-module .sr-settings-well-header .far {
        margin-right: 8px;
        color: #6c757d;
      }

      /* Tab styling enhancements - scoped */
      .sr-creation-module .nav-tabs > li > a {
        font-weight: 500;
      }
      .sr-creation-module .nav-tabs > li.active > a {
        border-top: 3px solid #3c8dbc;
      }

      /* Help expander styling */
      .sr-creation-module .sr-help-expander {
        background-color: #fff3cd;
        border: 1px solid #ffc107;
        border-radius: 4px;
        margin-bottom: 15px;
      }
      .sr-creation-module .sr-help-expander .panel-heading {
        background-color: transparent;
        border: none;
        padding: 8px 12px;
      }
      .sr-creation-module .sr-help-expander .panel-body {
        padding: 12px;
        font-size: 13px;
        line-height: 1.5;
      }

      /* Step content area */
      .sr-creation-module .sr-step-content {
        min-height: 400px;
      }
    "))),

    # Wrapper div with scoping class
    div(
      class = "sr-creation-module",

    shinydashboard::box(
      width = 12,

      # Header
      tags$h3("Formula Builder: Create Novel Stressors and Stressor-Response Relationships"),

      tags$p(
        "Create novel stressors from existing data using custom formulas, then define stressor-response relationships.",
        class = "pm-ht"
      ),

      # Progress Indicator
      uiOutput(ns("progress_indicator")),

      tags$hr(),

      # Tabset Panel for Steps
      tabsetPanel(
        id = ns("step_tabs"),
        type = "tabs",

        # =====================
        # STEP 1: Formula Builder
        # =====================
        tabPanel(
          title = tagList(icon("calculator"), " Step 1: Build Formula"),
          value = "step1",
          class = "sr-step-content",

          tags$br(),

          fluidRow(
            column(
              width = 8,

              # Help expander
              div(
                class = "panel-group sr-help-expander",
                div(
                  class = "panel panel-default",
                  style = "margin-bottom: 0; border: none; box-shadow: none;",
                  div(
                    class = "panel-heading",
                    a(
                      icon("info-circle"), " How to use the Formula Builder",
                      `data-toggle` = "collapse",
                      href = paste0("#", ns("help_step1")),
                      style = "text-decoration: none; color: #856404;"
                    )
                  ),
                  div(
                    id = ns("help_step1"),
                    class = "panel-collapse collapse",
                    div(
                      class = "panel-body",
                      tags$p("The formula builder takes existing variables (stressors) from your stressor magnitude data and combines them using custom mathematical expressions to generate new variables."),
                      tags$p(tags$b("Supported operators:"), " Addition (+), Subtraction (-), Multiplication (*), Division (/), Exponents (^), Parentheses (), log(), log10(), sqrt()"),
                      tags$p(tags$b("Tips:"), " Use exact variable names as shown below. Ensure matching parentheses. The formula is applied to all locations."),
                      tags$p(tags$b("Applications:"), " Model stressor interactions, apply climate scenarios (e.g., Temperature+2), implement regression equations from literature, calculate habitat capacity from area metrics.")
                    )
                  )
                )
              ),

              # Available Variables
              wellPanel(
                class = "sr-settings-well",
                div(class = "sr-settings-well-header", icon("list"), "Available Variables"),
                tags$p("These stressor variables can be used in your formula:", class = "small text-muted"),
                div(textOutput(ns("stressors_to_choose_from")), style = "color: #6f42c1; font-family: monospace;")
              ),

              # Formula Examples expander
              div(
                class = "panel-group",
                style = "margin-bottom: 15px;",
                div(
                  class = "panel panel-default",
                  div(
                    class = "panel-heading",
                    style = "padding: 8px 12px; background-color: #e7f3ff; border-color: #b8daff;",
                    a(
                      icon("lightbulb"), " Show formula examples",
                      `data-toggle` = "collapse",
                      href = paste0("#", ns("formula_examples")),
                      style = "text-decoration: none; color: #004085;"
                    )
                  ),
                  div(
                    id = ns("formula_examples"),
                    class = "panel-collapse collapse",
                    div(
                      class = "panel-body",
                      style = "font-size: 13px;",
                      tags$ul(
                        style = "margin-bottom: 0;",
                        tags$li(tags$b("Habitat capacity estimate:"), tags$br(),
                          tags$code("(stream_area_pool*0.16)+(stream_area_glide*0.04)+(stream_area_riffle*0.01)"), tags$br(),
                          tags$small(class = "text-muted", "Calculates carrying capacity from habitat unit areas and density estimates")),
                        tags$li(style = "margin-top: 10px;", tags$b("Climate scenario adjustment:"), tags$br(),
                          tags$code("Temperature + 2"), " or ", tags$code("Fines_pct * 0.8"), tags$br(),
                          tags$small(class = "text-muted", "Apply fixed increases/decreases to simulate scenarios")),
                        tags$li(style = "margin-top: 10px;", tags$b("Stressor interaction (Wenger et al., 2011):"), tags$br(),
                          tags$code("(-0.59*Temp)+((-0.88*Temp)^2)+(-0.20*PeakFlows)"), tags$br(),
                          tags$small(class = "text-muted", "Model interactive effects between temperature and flow")),
                        tags$li(style = "margin-top: 10px;", tags$b("Regression equation (Moore et al., 2013):"), tags$br(),
                          tags$code("7.91+(0.484*July_Temp)+(1.18*log10(Drainage_Area_km2+0.001))+(-0.00306*Median_Elev_m)"), tags$br(),
                          tags$small(class = "text-muted", "Predict MWAT from basin-level GIS variables"))
                      )
                    )
                  )
                )
              ),

              # Formula Input
              wellPanel(
                class = "sr-settings-well",
                div(class = "sr-settings-well-header", icon("edit"), "Enter Formula"),
                textAreaInput(
                  ns("formula"),
                  label = NULL,
                  value = "",
                  rows = 3,
                  placeholder = "Enter your formula here, e.g., Temperature + 2 or log(Stressor1 * Stressor2)"
                ),
                uiOutput(ns("var_buttons")),

                tags$hr(),

                # Run button
                actionButton(
                  ns("eval_formula"),
                  "Run Formula and Generate New Data",
                  icon = icon("play"),
                  class = "btn btn-success btn-lg",
                  style = "color: white; width: 100%;"
                ),

                tags$br(), tags$br(),

                # Validation status
                uiOutput(ns("step1_validation")),

                # Formula result output
                verbatimTextOutput(ns("formula_result"))
              ),

              # Display Options (expanded) - only decimals here, color ramp moved to Step 2
              wellPanel(
                class = "sr-settings-well",
                div(class = "sr-settings-well-header", icon("cog"), "Display Options"),
                numericInput(
                  ns("r_decimals"),
                  label = "Round to (n) Decimals",
                  value = 2,
                  min = 0,
                  max = 100
                )
              )

            ),
            column(
              width = 4,
              wellPanel(
                class = "sr-settings-well",
                div(class = "sr-settings-well-header", icon("chart-bar"), "Distribution Preview of Raw Values"),
                plotOutput(ns("hist_plot"), height = "200px")
              ),
              wellPanel(
                class = "sr-settings-well",
                div(class = "sr-settings-well-header", icon("calculator"), "Summary Statistics of Raw Values"),
                uiOutput(ns("basic_stats_step1"))
              )
            )
          )
        ),

        # =====================
        # STEP 2: Evaluate Results
        # =====================
        tabPanel(
          title = tagList(icon("map"), " Step 2: Evaluate Results"),
          value = "step2",
          class = "sr-step-content",

          tags$br(),

          # Help expander
          div(
            class = "panel-group sr-help-expander",
            div(
              class = "panel panel-default",
              style = "margin-bottom: 0; border: none; box-shadow: none;",
              div(
                class = "panel-heading",
                a(
                  icon("info-circle"), " About evaluating stressor values",
                  `data-toggle` = "collapse",
                  href = paste0("#", ns("help_step2")),
                  style = "text-decoration: none; color: #856404;"
                )
              ),
              div(
                id = ns("help_step2"),
                class = "panel-collapse collapse",
                div(
                  class = "panel-body",
                  tags$p("After running the formula, the new stressor values are temporarily cached for review. Use the interactive map and summary statistics to verify the results make sense."),
                  tags$p(tags$b("What to check:"), " Look for unexpected NA values, outliers, or patterns that don't match your expectations. The formula can be re-run from Step 1 if adjustments are needed."),
                  tags$p(tags$b("Tip:"), " Each time you run the formula, the previous data is overwritten. Download the data if you want to save intermediate results.")
                )
              )
            )
          ),

          # Validation status
          uiOutput(ns("step2_validation")),

          fluidRow(
            column(
              width = 8,
              # Color ramp selector above the map
              wellPanel(
                class = "sr-settings-well",
                style = "padding: 10px;",
                fluidRow(
                  column(
                    width = 6,
                    selectInput(
                      ns("color_ramp"),
                      label = "Map Color Ramp",
                      choices = c("Red High, Blue Low", "Blue High, Red Low"),
                      selected = "Red High, Blue Low"
                    )
                  ),
                  column(
                    width = 6,
                    style = "padding-top: 25px;",
                    uiOutput(ns("download_btn"))
                  )
                )
              ),
              # Map
              module_sr_creation_map_ui(ns("module_sr_creation_map"))
            ),
            column(
              width = 4,
              wellPanel(
                class = "sr-settings-well",
                div(class = "sr-settings-well-header", icon("chart-bar"), "Distribution Preview of Raw Values"),
                plotOutput(ns("hist_plot_step2"), height = "180px")
              ),
              wellPanel(
                class = "sr-settings-well",
                div(class = "sr-settings-well-header", icon("calculator"), "Summary Statistics of Raw Values"),
                uiOutput(ns("basic_stats_step2"))
              ),
              wellPanel(
                class = "sr-settings-well",
                div(class = "sr-settings-well-header", icon("chart-pie"), "Percentile Breakpoints"),
                uiOutput(ns("s_summary_stats")),
                tags$small(class = "text-muted", "Color swatches match map legend")
              )
            )
          )
        ),

        # =====================
        # STEP 3: Define SR Relationship
        # =====================
        tabPanel(
          title = tagList(icon("chart-line"), " Step 3: Define Relationship"),
          value = "step3",
          class = "sr-step-content",

          tags$br(),

          # Help expander
          div(
            class = "panel-group sr-help-expander",
            div(
              class = "panel panel-default",
              style = "margin-bottom: 0; border: none; box-shadow: none;",
              div(
                class = "panel-heading",
                a(
                  icon("info-circle"), " About stressor-response relationships",
                  `data-toggle` = "collapse",
                  href = paste0("#", ns("help_step3")),
                  style = "text-decoration: none; color: #856404;"
                )
              ),
              div(
                id = ns("help_step3"),
                class = "panel-collapse collapse",
                div(
                  class = "panel-body",
                  tags$p("A stressor-response relationship defines how raw stressor values (e.g., temperature in degrees) translate to a system capacity score (0-100%). This curve is used by the Joe Model and Population Model to evaluate habitat quality."),
                  tags$p(tags$b("Copy existing:"), " Use this option if your new stressor is a variant of an existing one (e.g., a climate scenario). All settings and the SR curve will be copied."),
                  tags$p(tags$b("Create new:"), " Build a custom relationship by defining the curve points in the table below. Each row specifies a raw stressor value and its corresponding response score."),
                  tags$p(tags$b("Identity function:"), " If your formula already outputs habitat capacity (0-100), use raw values 0 and 100 with corresponding scores of 0 and 100 (a 1:1 line).")
                )
              )
            )
          ),

          fluidRow(
            column(
              width = 8,

              # Create New vs Copy
              wellPanel(
                class = "sr-settings-well",
                div(class = "sr-settings-well-header", icon("copy"), "Source"),
                radioButtons(
                  ns("sr_radio"),
                  NULL,
                  c(
                    "Create New Relationship" = "new",
                    "Copy from Existing Stressor" = "copy"
                  ),
                  selected = "new"
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'copy'", ns("sr_radio")),
                  selectInput(
                    ns("copy_from_stressor"),
                    "Select Stressor to Copy From:",
                    choices = NULL
                  )
                )
              ),

              # Interaction Settings
              wellPanel(
                class = "sr-settings-well",
                div(class = "sr-settings-well-header", icon("link"), "Interaction Settings"),
                tags$p(class = "small text-muted", "Configure if this stressor should be grouped with correlated stressors."),
                # Interaction help expander
                div(
                  class = "panel-group",
                  style = "margin-bottom: 10px;",
                  div(
                    class = "panel panel-default",
                    div(
                      class = "panel-heading",
                      style = "padding: 5px 10px; background-color: #f8f9fa;",
                      a(
                        icon("question-circle"), " What are interaction groups?",
                        `data-toggle` = "collapse",
                        href = paste0("#", ns("help_interactions")),
                        style = "text-decoration: none; font-size: 12px; color: #6c757d;"
                      )
                    ),
                    div(
                      id = ns("help_interactions"),
                      class = "panel-collapse collapse",
                      div(
                        class = "panel-body",
                        style = "font-size: 12px;",
                        tags$p("Some stressors are highly correlated (e.g., road density and stream crossing density). Including both would double-count their effects."),
                        tags$p("Use 'Minimum' interaction to group them - only the stressor with the lowest response score will be used in the model."),
                        tags$p("All stressors in a group must have the same interaction type and linked group name (e.g., 'roads').")
                      )
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 6,
                    selectInput(
                      ns("s_Interaction"),
                      "Interaction Type:",
                      c("None" = NA, "Minimum" = "Minimum")
                    )
                  ),
                  column(
                    width = 6,
                    textInput(ns("s_Linked"), "Linked Group Name:", value = NA, placeholder = "e.g., roads")
                  )
                )
              ),

              # Curve Settings
              wellPanel(
                class = "sr-settings-well",
                div(class = "sr-settings-well-header", icon("bezier-curve"), "Curve Settings"),
                fluidRow(
                  column(
                    width = 4,
                    selectInput(
                      ns("s_Function"),
                      "Function Type:",
                      c("continuous" = "continuous", "step" = "step")
                    ),
                    tags$small(class = "text-muted", "'continuous' for smooth curves, 'step' for discrete levels")
                  ),
                  column(
                    width = 4,
                    selectInput(
                      ns("s_Stress_Scale"),
                      "Interpolation Scale:",
                      c("linear" = "linear", "log" = "log")
                    ),
                    tags$small(class = "text-muted", "How to interpolate between defined points")
                  ),
                  column(
                    width = 4,
                    textInput(ns("s_Units"), "Raw Stressor Units:", value = "units", placeholder = "e.g., Celsius"),
                    tags$small(class = "text-muted", "Units for documentation")
                  )
                )
              ),

              # Model Settings
              wellPanel(
                class = "sr-settings-well",
                div(class = "sr-settings-well-header", icon("cogs"), "Model Settings"),
                # Model settings help expander
                div(
                  class = "panel-group",
                  style = "margin-bottom: 10px;",
                  div(
                    class = "panel panel-default",
                    div(
                      class = "panel-heading",
                      style = "padding: 5px 10px; background-color: #f8f9fa;",
                      a(
                        icon("question-circle"), " Population Model settings explained",
                        `data-toggle` = "collapse",
                        href = paste0("#", ns("help_model")),
                        style = "text-decoration: none; font-size: 12px; color: #6c757d;"
                      )
                    ),
                    div(
                      id = ns("help_model"),
                      class = "panel-collapse collapse",
                      div(
                        class = "panel-body",
                        style = "font-size: 12px;",
                        tags$p("The Population Model requires additional settings to link stressors to specific life stages and vital rates."),
                        tags$p(tags$b("Life Stages:"), " Codes like SE (egg survival), S0 (age-0), surv_1 (yearling survival), eps_4 (age-4 fecundity). See documentation for full list."),
                        tags$p(tags$b("Vital Rate:"), " Which demographic parameter is affected - survival, capacity (carrying capacity), or fecundity (reproduction).")
                      )
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 4,
                    selectInput(
                      ns("s_Model"),
                      "Model Endpoint:",
                      c("All" = "All", "Joe Model" = "Joe Model", "Population Model" = "Population Model")
                    )
                  ),
                  column(
                    width = 4,
                    textInput(ns("s_Life_stages"), "Life Stages (Pop. Model):", value = NA, placeholder = "e.g., SE, S0")
                  ),
                  column(
                    width = 4,
                    selectInput(
                      ns("s_Parameters"),
                      "Vital Rate (Pop. Model):",
                      c("survival" = "survival", "capacity" = "capacity", "fecundity" = "fecundity")
                    )
                  )
                )
              ),

              # SR Table
              wellPanel(
                class = "sr-settings-well",
                div(class = "sr-settings-well-header", icon("table"), "Stressor-Response Curve Data"),
                tags$p(class = "small text-muted", "Define the relationship between raw stressor values and response scores. Each row is a point on the curve."),
                fluidRow(
                  column(width = 6,
                    actionButton(ns("add_row"), "Add Row", icon = icon("plus"), class = "btn-sm"),
                    actionButton(ns("delete_row"), "Delete Row", icon = icon("minus"), class = "btn-sm btn-warning")
                  ),
                  column(width = 6,
                    uiOutput(ns("step3_validation"))
                  )
                ),
                tags$br(),
                rHandsontableOutput(ns("hot"))
              )

            ),
            column(
              width = 4,
              wellPanel(
                class = "sr-settings-well",
                div(class = "sr-settings-well-header", icon("chart-bar"), "Distribution Preview of Raw Values"),
                plotOutput(ns("hist_plot_step3"), height = "150px")
              ),
              wellPanel(
                class = "sr-settings-well",
                div(class = "sr-settings-well-header", icon("calculator"), "Summary Statistics of Raw Values"),
                uiOutput(ns("basic_stats_step3"))
              ),
              wellPanel(
                class = "sr-settings-well",
                div(class = "sr-settings-well-header", icon("chart-area"), "Stressor-Response Curve Preview"),
                dygraphOutput(ns("sr_preview_plot"), height = "280px")
              )
            )
          )
        ),

        # =====================
        # STEP 4: Save
        # =====================
        tabPanel(
          title = tagList(icon("save"), " Step 4: Save"),
          value = "step4",
          class = "sr-step-content",

          tags$br(),

          # Help expander
          div(
            class = "panel-group sr-help-expander",
            div(
              class = "panel panel-default",
              style = "margin-bottom: 0; border: none; box-shadow: none;",
              div(
                class = "panel-heading",
                a(
                  icon("info-circle"), " About saving your stressor",
                  `data-toggle` = "collapse",
                  href = paste0("#", ns("help_step4")),
                  style = "text-decoration: none; color: #856404;"
                )
              ),
              div(
                id = ns("help_step4"),
                class = "panel-collapse collapse",
                div(
                  class = "panel-body",
                  tags$p("Saving will add your new stressor and its stressor-response relationship to the active model datasets. The stressor will then be available for use in the Joe Model, Population Model, and other components."),
                  tags$p(tags$b("Naming:"), " Use a clear, descriptive name. Only letters, numbers, and underscores are allowed. The name must be unique."),
                  tags$p(tags$b("After saving:"), " The application will automatically update to include the new stressor. You can view it in the Stressor-Response Relationships section.")
                )
              )
            )
          ),

          fluidRow(
            column(
              width = 6,
              wellPanel(
                class = "sr-settings-well",
                div(class = "sr-settings-well-header", icon("tag"), "Name Your Stressor"),
                textInput(
                  ns("s_New_Stressor_Name"),
                  label = NULL,
                  value = "New_Stressor",
                  placeholder = "Enter stressor name (no spaces)"
                ),
                tags$small(class = "text-muted", "Use only letters, numbers, and underscores. Must be unique.")
              )
            ),
            column(
              width = 6,
              wellPanel(
                class = "sr-settings-well",
                div(class = "sr-settings-well-header", icon("clipboard-check"), "Completion Status"),
                uiOutput(ns("completion_checklist"))
              )
            )
          ),

          fluidRow(
            column(
              width = 12,
              wellPanel(
                style = "background-color: #e8f5e9; border-color: #c8e6c9;",
                shinyjs::disabled(
                  actionButton(
                    ns("save_data"),
                    "Save Stressor to Model",
                    icon = icon("save"),
                    class = "btn btn-success btn-lg",
                    style = "width: 100%; font-size: 18px; padding: 15px;"
                  )
                ),
                tags$br(), tags$br(),
                tags$p(
                  id = ns("save_help"),
                  class = "text-center text-muted",
                  "Complete all steps above to enable the save button."
                )
              )
            )
          )
        )
      )
    )
    ) # Close wrapper div with sr-creation-module class
  )
}


# ==========================
# SERVER FUNCTION
# ==========================

module_sr_creation_server <- function(id, df, allowed_vars = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Create placeholder reactive dataframe for the formula evaluation
    rv_formula_df <- reactiveValues(dat = NULL)
    
    # Show list of stressors to choose from
    output$stressors_to_choose_from <- renderText({
      print("building stressors_to_choose_from...")
      stcf <- unique(session$userData$rv_stressor_magnitude$sm_dat$Stressor)
      stcf <- stcf[!(is.na(stcf) | stcf == "")]
      stcf <- sort(stcf)
      stcf <- paste(stcf, collapse = ", ")
      stcf
    })

    # ======================================================
    # Validation reactive values
    # ======================================================
    validation <- reactiveValues(
      step1_complete = FALSE,
      step2_complete = FALSE,
      step3_complete = FALSE,
      step4_complete = FALSE
    )

    # ======================================================
    # Progress Indicator
    # ======================================================
    output$progress_indicator <- renderUI({
      # Determine step statuses
      step1_status <- if (validation$step1_complete) "completed" else if (input$step_tabs == "step1") "active" else ""
      step2_status <- if (validation$step2_complete) "completed" else if (input$step_tabs == "step2") "active" else ""
      step3_status <- if (validation$step3_complete) "completed" else if (input$step_tabs == "step3") "active" else ""
      step4_status <- if (input$step_tabs == "step4") "active" else ""

      div(
        class = "sr-progress-container",
        # Step 1
        div(
          class = paste("sr-progress-step", step1_status),
          div(class = paste("sr-step-circle", step1_status),
            if (validation$step1_complete) icon("check") else "1"
          ),
          span(class = paste("sr-step-label", step1_status), "Build Formula")
        ),
        # Step 2
        div(
          class = paste("sr-progress-step", step2_status),
          div(class = paste("sr-step-circle", step2_status),
            if (validation$step2_complete) icon("check") else "2"
          ),
          span(class = paste("sr-step-label", step2_status), "Evaluate")
        ),
        # Step 3
        div(
          class = paste("sr-progress-step", step3_status),
          div(class = paste("sr-step-circle", step3_status),
            if (validation$step3_complete) icon("check") else "3"
          ),
          span(class = paste("sr-step-label", step3_status), "Define SR")
        ),
        # Step 4
        div(
          class = paste("sr-progress-step", step4_status),
          div(class = paste("sr-step-circle", step4_status), "4"),
          span(class = paste("sr-step-label", step4_status), "Save")
        )
      )
    })

    # ======================================================
    # Step 1 Validation Status
    # ======================================================
    output$step1_validation <- renderUI({
      if (validation$step1_complete) {
        div(class = "sr-validation-status valid",
          icon("check-circle"), " Formula executed successfully"
        )
      } else {
        div(class = "sr-validation-status pending",
          icon("clock"), " Run formula to continue"
        )
      }
    })

    # ======================================================
    # Step 2 Validation Status
    # ======================================================
    output$step2_validation <- renderUI({
      if (!validation$step1_complete) {
        div(class = "sr-validation-status invalid",
          icon("exclamation-triangle"), " Complete Step 1 first"
        )
      } else if (validation$step2_complete) {
        div(class = "sr-validation-status valid",
          icon("check-circle"), " Data ready for use"
        )
      } else {
        div(class = "sr-validation-status pending",
          icon("info-circle"), " Review the results below"
        )
      }
    })

    # ======================================================
    # Step 3 Validation Status
    # ======================================================
    output$step3_validation <- renderUI({
      sr_result <- process_sr_table(sr_values())
      if (sr_result$valid) {
        div(class = "sr-validation-status valid", style = "text-align: right;",
          icon("check-circle"), " Table valid"
        )
      } else {
        div(class = "sr-validation-status invalid", style = "text-align: right;",
          icon("exclamation-circle"), " Fill in raw stressor values"
        )
      }
    })

    # ======================================================
    # Completion Checklist (Step 4)
    # ======================================================
    output$completion_checklist <- renderUI({
      # Check each step
      formula_ok <- validation$step1_complete
      data_ok <- validation$step2_complete
      sr_ok <- validation$step3_complete
      name_ok <- !is.null(input$s_New_Stressor_Name) && nchar(trimws(input$s_New_Stressor_Name)) >= 2

      # Check name uniqueness
      existing_names <- session$userData$rv_stressor_response$stressor_names
      name_unique <- !(tolower(trimws(input$s_New_Stressor_Name)) %in% tolower(existing_names))

      tags$ul(
        style = "list-style: none; padding-left: 0; margin: 0;",
        tags$li(
          style = paste("color:", if (formula_ok) "#28a745" else "#dc3545"),
          if (formula_ok) icon("check-circle") else icon("times-circle"),
          " Step 1: Formula executed"
        ),
        tags$li(
          style = paste("color:", if (data_ok) "#28a745" else "#dc3545"),
          if (data_ok) icon("check-circle") else icon("times-circle"),
          " Step 2: Data evaluated"
        ),
        tags$li(
          style = paste("color:", if (sr_ok) "#28a745" else "#dc3545"),
          if (sr_ok) icon("check-circle") else icon("times-circle"),
          " Step 3: SR relationship defined"
        ),
        tags$li(
          style = paste("color:", if (name_ok) "#28a745" else "#dc3545"),
          if (name_ok) icon("check-circle") else icon("times-circle"),
          " Stressor name provided"
        ),
        if (!name_unique && name_ok) tags$li(
          style = "color: #dc3545",
          icon("exclamation-triangle"),
          " Name already exists - choose unique name"
        )
      )
    })

    # ======================================================
    # Call the submodule and pass the reactive values
    # ======================================================
    
    module_sr_creation_map_server("module_sr_creation_map",
                                  rv_formula_df,
                                  reactive(input$color_ramp))
    
    # Create variable insert widget using available stressor names
    output$var_buttons <- renderUI({
      # Get available stressor variable names
      stressor_vars <- unique(session$userData$rv_stressor_magnitude$sm_dat$Stressor)
      stressor_vars <- stressor_vars[!(is.na(stressor_vars) | stressor_vars == "")]
      stressor_vars <- sort(stressor_vars)

      if (length(stressor_vars) == 0) {
        return(NULL)
      }

      fluidRow(
        column(
          width = 8,
          selectInput(ns("var_select"), "Insert Variable into Formula:", choices = stressor_vars)
        ),
        column(
          width = 4,
          style = "margin-top: 25px;",
          actionButton(ns("insert_var"), "Insert", icon = icon("plus"), class = "btn-info")
        )
      )
    })
    
    # When the "Insert" button is clicked, append the selected variable name to the formula.
    observeEvent(input$insert_var, {
      var_to_insert <- input$var_select
      new_formula <- paste0(input$formula, " ", var_to_insert)
      updateTextAreaInput(session, "formula", value = new_formula)
    })


    # Sanitize formula input - allows only safe arithmetic characters
    sanitize_formula <- function(formula_str) {
      # Allow letters, digits, whitespace, and these symbols: + * / ^ ( ) . , _ and any punctuation dash
      if (grepl("[^0-9a-zA-Z\\+\\*/\\^\\(\\)\\.\\s,_\\p{Pd}]",
                formula_str,
                perl = TRUE)) {
        return(NULL)
      }
      return(formula_str)
    }

    # ----------------------------------------------------------------------
    # Helper function to process and validate SR table data
    # Returns a list with: valid (boolean), table_vals (processed data)
    # ----------------------------------------------------------------------
    process_sr_table <- function(mvals) {
      result <- list(valid = FALSE, table_vals = NULL)

      if (nrow(mvals) == 0) {
        return(result)
      }

      # Get all SR data
      table_vals <- mvals
      colnames(table_vals) <- c("value", "mean_system_capacity", "sd", "lwr", "upr")

      # Convert to numeric
      table_vals$value <- as.numeric(as.character(table_vals$value))
      table_vals$mean_system_capacity <- as.numeric(as.character(table_vals$mean_system_capacity))
      table_vals$sd <- as.numeric(as.character(table_vals$sd))
      table_vals$lwr <- as.numeric(as.character(table_vals$lwr))
      table_vals$upr <- as.numeric(as.character(table_vals$upr))

      # Calculate SD bounds before replacing NAs
      table_vals$lwr_sd <- table_vals$mean_system_capacity - table_vals$sd
      table_vals$upr_sd <- table_vals$mean_system_capacity + table_vals$sd

      # Replace missing data with appropriate defaults
      table_vals$mean_system_capacity <- ifelse(
        is.na(table_vals$mean_system_capacity), 100, table_vals$mean_system_capacity
      )
      table_vals$sd <- ifelse(is.na(table_vals$sd), 0, table_vals$sd)
      table_vals$lwr <- ifelse(is.na(table_vals$lwr), 0, table_vals$lwr)
      table_vals$upr <- ifelse(is.na(table_vals$upr), 100, table_vals$upr)

      # Only keep rows with valid numeric stressor values
      table_vals <- table_vals[!is.na(table_vals$value), ]

      if (nrow(table_vals) == 0) {
        return(result)
      }

      # Fix SD bounds to be within range of limits
      table_vals$lwr_sd <- ifelse(table_vals$lwr_sd < table_vals$lwr, table_vals$lwr, table_vals$lwr_sd)
      table_vals$upr_sd <- ifelse(table_vals$upr_sd > table_vals$upr, table_vals$upr, table_vals$upr_sd)

      # Sort by value
      table_vals <- table_vals[order(table_vals$value), ]

      result$valid <- TRUE
      result$table_vals <- table_vals
      return(result)
    }

    # ----------------------------------------------------------------------
    # Helper function to validate stressor name
    # Returns a list with: valid (boolean), name (cleaned name), error (message if invalid)
    # ----------------------------------------------------------------------
    validate_stressor_name <- function(name, existing_names) {
      result <- list(valid = FALSE, name = NULL, error = NULL)

      # Trim whitespace and replace spaces with underscores
      name <- trimws(name)
      name <- gsub(" ", "_", name)

      # Check if empty
      if (is.null(name) || name == "" || nchar(name) == 0) {
        result$error <- "Stressor name cannot be empty."
        return(result)
      }

      # Remove special characters (keep only alphanumeric and underscore)
      clean_name <- gsub("[^a-zA-Z0-9_]", "", name)

      if (clean_name != name) {
        result$error <- paste0("Stressor name contains invalid characters. Cleaned name: '", clean_name, "'")
        name <- clean_name
      }

      # Check for duplicate names
      if (tolower(name) %in% tolower(existing_names)) {
        result$error <- paste0("A stressor named '", name, "' already exists. Please choose a unique name.")
        return(result)
      }

      # Check minimum length
      if (nchar(name) < 2) {
        result$error <- "Stressor name must be at least 2 characters long."
        return(result)
      }

      result$valid <- TRUE
      result$name <- name
      return(result)
    }

    # ----------------------------------------------------------------------
    # When "Evaluate Formula" is clicked, parse and evaluate the expression
    # ----------------------------------------------------------------------
    observeEvent(input$eval_formula, {
      formula_str <- input$formula
      
      # formula_str <- "log(1.3* (Fines/(8.1*Fry_Capacity))^2)"
      # formula_str <- "1.3*(Ficnes/(8.1*Fry_Capacity))^2"
      # print("Check this...")
      # browser()
      
      formula_str <- gsub(" ", "", formula_str)
      
      safe_formula <- sanitize_formula(formula_str)
      
      smd <- session$userData$rv_stressor_magnitude$sm_dat
      smd <- smd[, c("HUC_ID", "NAME", "Stressor", "Mean")]
      # Convert smd from long to wide format but keep HUC_ID
      smd2 <- spread(smd, key = "Stressor", value = "Mean")
      
      if (is.null(safe_formula)) {
        output$formula_result <- renderText("Error: Formula contains invalid characters!")
        return()
      }
      
      return_message <- "Formula Evaluated Successfully"
      error_occurred <- FALSE

      # Try to parse and evaluate the expression.
      # We evaluate in the environment of the provided dataframe `df`.
      result <- tryCatch({
        expr <- parse(text = safe_formula)
        # Because the columns in df are vectorized, the expression is computed for every row.
        eval(expr, envir = smd2)
      }, error = function(e) {
        error_occurred <<- TRUE
        return_message <<- paste("Error in evaluating formula:", e$message)
        return(NULL)
      })

      smd_out <- smd2[, c("HUC_ID", "NAME")]

      if (length(result) > 0 && !error_occurred) {
        # Check if result contains any valid (non-NA) values
        if (all(is.na(result))) {
          return_message <- "Warning: Formula produced all NA values. Check that referenced stressors exist."
          smd_out$val <- NA
        } else {
          smd_out$val <- result
        }
      } else {
        if (!error_occurred) {
          return_message <- paste(
            "Error in evaluating formula. Use round brackets and * symbol for multiplication..."
          )
        }
        smd_out$val <- NA
      }
      
      # browser()
      
      # Update reactive object
      rv_formula_df$dat <- smd_out
      
      
      output$formula_result <- renderPrint(return_message)
    })
    
    # --------------------------------------------------------------
    # Download formula data button for download_data
    # --------------------------------------------------------------
    
    output$download_btn <- renderUI({
      if (is.null(rv_formula_df$dat)) {
        return(NULL)  # Don't show the button if no data
      } else {
        downloadButton(ns("download_data"), "Download Data")
      }
    })
    
    output$download_data <- downloadHandler(
      filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        paste0("formula_data_", timestamp, ".xlsx")
      },
      content = function(file) {
        
        # Gather data
        mydata <- rv_formula_df$dat
        
        # Round to set decimals
        n_round <- isolate(input$r_decimals)
        
        mydata$val <- round(mydata$val, n_round)
        
        mydata$Stressor <- "Custom"
        mydata$Stressor_cat <- "Custom"
        mydata$Mean <- mydata$val
        mydata$SD <- 0
        mydata$Distribution <- "normal"
        mydata$Low_Limit <- mydata$val
        mydata$Up_Limit <- mydata$val
        mydata$Comments <- NA
        
        mydata <- mydata[, c("HUC_ID", "NAME", "Stressor", "Stressor_cat",
                             "Mean", "SD", "Distribution", "Low_Limit",
                             "Up_Limit", "Comments")]
        
        write_xlsx(mydata, path = file)
        
      }
    )
    
    
    # --------------------------------------------------------------
    # Summary Statistics
    # --------------------------------------------------------------
    
    # fill the uiOutput for s_summary_stats - now only percentile breakpoints
    output$s_summary_stats <- renderUI({
      print("building s_summary_stats (percentiles)...")
      req(rv_formula_df$dat$val)

      vals <- rv_formula_df$dat$val
      n_decimals <- input$r_decimals
      if (is.null(n_decimals) || is.na(n_decimals)) n_decimals <- 2

      # Calculate percentile breakpoints
      mpercentiles <- quantile(vals, probs = seq(0, 1, 0.1), na.rm = TRUE)
      mpercentiles <- round(mpercentiles, n_decimals)

      # Define the color vector (for P10-P90)
      i_colors <- c(
        '#3288bd',
        '#66c2a5',
        '#abdda4',
        '#e6f598',
        '#ffffbf',
        '#fee08b',
        '#fdae61',
        '#f46d43',
        '#d53e4f'
      )

      # Reverse colors if needed
      if (input$color_ramp == "Blue High, Red Low") {
        m_colors <- rev(i_colors)
      } else {
        m_colors <- i_colors
      }

      # Render percentile list with color swatches
      tags$ul(
        style = "list-style: none; padding-left: 0; margin: 0; font-size: 12px;",
          tags$li(
            tags$span(
              style = sprintf(
                "display:inline-block; width:12px; height:12px; background-color:%s; margin-right:5px;",
                m_colors[1]
              )
            ),
            paste("P10:", mpercentiles[2])
          ),
          tags$li(
            tags$span(
              style = sprintf(
                "display:inline-block; width:12px; height:12px; background-color:%s; margin-right:5px;",
                m_colors[2]
              )
            ),
            paste("P20:", mpercentiles[3])
          ),
          tags$li(
            tags$span(
              style = sprintf(
                "display:inline-block; width:12px; height:12px; background-color:%s; margin-right:5px;",
                m_colors[3]
              )
            ),
            paste("P30:", mpercentiles[4])
          ),
          tags$li(
            tags$span(
              style = sprintf(
                "display:inline-block; width:12px; height:12px; background-color:%s; margin-right:5px;",
                m_colors[4]
              )
            ),
            paste("P40:", mpercentiles[5])
          ),
          tags$li(
            tags$span(
              style = sprintf(
                "display:inline-block; width:12px; height:12px; background-color:%s; margin-right:5px;",
                m_colors[5]
              )
            ),
            paste("P50:", mpercentiles[6])
          ),
          tags$li(
            tags$span(
              style = sprintf(
                "display:inline-block; width:12px; height:12px; background-color:%s; margin-right:5px;",
                m_colors[6]
              )
            ),
            paste("P60:", mpercentiles[7])
          ),
          tags$li(
            tags$span(
              style = sprintf(
                "display:inline-block; width:12px; height:12px; background-color:%s; margin-right:5px;",
                m_colors[7]
              )
            ),
            paste("P70:", mpercentiles[8])
          ),
          tags$li(
            tags$span(
              style = sprintf(
                "display:inline-block; width:12px; height:12px; background-color:%s; margin-right:5px;",
                m_colors[8]
              )
            ),
            paste("P80:", mpercentiles[9])
          ),
          tags$li(
            tags$span(
              style = sprintf(
                "display:inline-block; width:12px; height:12px; background-color:%s; margin-right:5px;",
                m_colors[9]
              )
            ),
            paste("P90:", mpercentiles[10])
          )
        )
    })
    
    
    
    
    
    
    
    output$hist_plot <- renderPlot({
      print("summary hist step1...")
      req(rv_formula_df$dat$val)
      hist(
        rv_formula_df$dat$val,
        main = NA,
        xlab = "Raw Stressor Values",
        ylab = "Frequency",
        col = "grey",
        border = "black"
      )
    })

    output$hist_plot_step2 <- renderPlot({
      print("summary hist step2...")
      req(rv_formula_df$dat$val)
      hist(
        rv_formula_df$dat$val,
        main = NA,
        xlab = "Raw Stressor Values",
        ylab = "Frequency",
        col = "grey",
        border = "black"
      )
    })

    output$hist_plot_step3 <- renderPlot({
      print("summary hist step3...")
      req(rv_formula_df$dat$val)
      hist(
        rv_formula_df$dat$val,
        main = NA,
        xlab = "Raw Stressor Values",
        ylab = "Frequency",
        col = "grey",
        border = "black"
      )
    })

    # --------------------------------------------------------------
    # Basic Summary Statistics (for Steps 1, 2, 3)
    # --------------------------------------------------------------
    render_basic_stats <- function() {
      req(rv_formula_df$dat$val)
      vals <- rv_formula_df$dat$val
      n_decimals <- input$r_decimals
      if (is.null(n_decimals) || is.na(n_decimals)) n_decimals <- 2

      mmin <- round(min(vals, na.rm = TRUE), n_decimals)
      mmax <- round(max(vals, na.rm = TRUE), n_decimals)
      mmean <- round(mean(vals, na.rm = TRUE), n_decimals)
      mmedian <- round(median(vals, na.rm = TRUE), n_decimals)
      mstd <- round(sd(vals, na.rm = TRUE), n_decimals)
      n_valid <- sum(!is.na(vals))

      tags$ul(
        style = "list-style: none; padding-left: 0; margin: 0;",
        tags$li(tags$b("Min: "), mmin),
        tags$li(tags$b("Max: "), mmax),
        tags$li(tags$b("Mean: "), mmean),
        tags$li(tags$b("Median: "), mmedian),
        tags$li(tags$b("Std Dev: "), mstd),
        tags$li(tags$b("N: "), n_valid, " locations")
      )
    }

    output$basic_stats_step1 <- renderUI({ render_basic_stats() })
    output$basic_stats_step2 <- renderUI({ render_basic_stats() })
    output$basic_stats_step3 <- renderUI({ render_basic_stats() })


    #==============================================
    # Stressor Response Table
    #==============================================
    # Initialize with 3 rows as an example.
    sr_values <- reactiveVal(
      data.frame(
        `Raw Stressor Values` = rep("", 3),
        `Stressor-Response Score (0 to 100)` = c("0", "50", "100"),
        `SD` = rep("0", 3),
        `Lower Limit (0 to 100)` = rep("0", 3),
        `Upper Limit (0 to 100)` = rep("100", 3),
        stringsAsFactors = FALSE
      )
    )

    # ======================================================
    # Update validation states (must be after sr_values is defined)
    # ======================================================
    observe({
      # Step 1: Formula has been run and produced valid results
      formula_valid <- !is.null(rv_formula_df$dat) &&
                       !is.null(rv_formula_df$dat$val) &&
                       !all(is.na(rv_formula_df$dat$val))
      validation$step1_complete <- formula_valid

      # Step 2: Same as step 1 for now (data exists)
      validation$step2_complete <- formula_valid

      # Step 3: SR table is valid
      sr_result <- process_sr_table(sr_values())
      validation$step3_complete <- sr_result$valid
    })

    output$hot <- renderRHandsontable({
      rhandsontable(sr_values(),
                    rowHeaders = TRUE,
                    stretchH = "all",
                    selectCallback = TRUE)
    })
    
    
    #------------------------------------------------------------------------
    # renderDygraph charts data visualization
    #------------------------------------------------------------------------
    output$sr_preview_plot <- renderDygraph({
      # Process SR table data using helper function
      sr_result <- process_sr_table(sr_values())

      if (!sr_result$valid) {
        return(NULL)
      }

      table_vals <- sr_result$table_vals
      table_vals <- table_vals[, c("value", "mean_system_capacity", "lwr", "upr", "lwr_sd", "upr_sd")]

      # X-axis mouse-over formatting
      myvFormatter <- "function formatValue(v) {
              var prefix = 'Raw Stressor: ';
              return prefix + String(v);
        }"

      # Start and return the dygraph plot
      dygraph(table_vals, main = "Stressor-Response Preview") %>%
        dyAxis("x", label = "Raw Stressor Values", valueFormatter = JS(myvFormatter)) %>%
        dyAxis("y", label = "Response Score") %>%
        dySeries(c("lwr", "mean_system_capacity", "upr"), label = "msc", color = "grey") %>%
        dySeries(c("lwr_sd", "mean_system_capacity", "upr_sd"), label = "Response ", color = "red")
    })
    
    
    
    
    
    # When the table is edited, update the reactive value.
    observeEvent(input$hot, {
      newData <- hot_to_r(input$hot)
      sr_values(newData)
    })
    
    # Add a new row when requested.
    observeEvent(input$add_row, {
      newData <- sr_values()
      newRow <- data.frame(
        `Raw Stressor Values` = "",
        `Stressor-Response Score (0 to 100)` = "100",
        `SD` = "0",
        `Lower Limit (0 to 100)` = "0",
        `Upper Limit (0 to 100)` = "100",
        stringsAsFactors = FALSE
      )
      sr_values(rbind(newData, newRow))
    })
    
    # Delete the currently selected row(s) from the handsontable
    observeEvent(input$delete_row, {
      newData <- sr_values()
      if (nrow(newData) == 0) {
        return()
      }

      # Get selected rows from handsontable (input$hot_select contains selection info)
      selection <- input$hot_select
      rows_to_delete <- NULL

      if (!is.null(selection) && !is.null(selection$select)) {
        # selection$select contains: list(r = start_row, c = start_col, r2 = end_row, c2 = end_col)
        # Rows are 0-indexed in handsontable, so add 1 for R
        start_row <- selection$select$r + 1
        end_row <- selection$select$r2 + 1
        rows_to_delete <- start_row:end_row
      }

      if (is.null(rows_to_delete) || length(rows_to_delete) == 0) {
        # No selection - delete the last row as fallback
        rows_to_delete <- nrow(newData)
        showNotification("No row selected. Deleting last row.", type = "warning", duration = 3)
      }

      # Ensure we don't delete all rows - keep at least one
      if (length(rows_to_delete) >= nrow(newData)) {
        showNotification("Cannot delete all rows. At least one row must remain.", type = "error", duration = 3)
        return()
      }

      # Delete selected rows
      sr_values(newData[-rows_to_delete, , drop = FALSE])
    })


    #------------------------------------------------------------------------
    # Copy Existing Stressor-Response: Populate dropdown choices
    #------------------------------------------------------------------------
    observe({
      stressor_choices <- session$userData$rv_stressor_response$stressor_names
      pretty_names <- session$userData$rv_stressor_response$pretty_names
      if (length(stressor_choices) > 0) {
        names(stressor_choices) <- pretty_names
      }
      updateSelectInput(session, "copy_from_stressor", choices = stressor_choices)
    })


    #------------------------------------------------------------------------
    # Copy Existing Stressor-Response: Populate form when stressor is selected
    #------------------------------------------------------------------------
    observeEvent(input$copy_from_stressor, {
      req(input$sr_radio == "copy")
      req(input$copy_from_stressor)

      selected_stressor <- input$copy_from_stressor

      # 1. Get metadata from main_sheet
      main_sheet <- session$userData$rv_stressor_response$main_sheet
      stressor_row <- main_sheet[main_sheet$Stressors == selected_stressor, ]

      if (nrow(stressor_row) > 0) {
        # Update form inputs with copied values
        updateSelectInput(session, "s_Interaction",
          selected = if(!is.na(stressor_row$Interaction[1])) stressor_row$Interaction[1] else "NA")
        updateTextInput(session, "s_Linked",
          value = if(!is.na(stressor_row$Linked[1])) as.character(stressor_row$Linked[1]) else "NA")
        updateSelectInput(session, "s_Function",
          selected = if(!is.na(stressor_row$Function[1])) stressor_row$Function[1] else "continuous")
        updateSelectInput(session, "s_Stress_Scale",
          selected = if(!is.na(stressor_row$Stress_Scale[1])) stressor_row$Stress_Scale[1] else "linear")
        updateSelectInput(session, "s_Model",
          selected = if(!is.na(stressor_row$Model[1])) stressor_row$Model[1] else "All")
        updateTextInput(session, "s_Life_stages",
          value = if(!is.na(stressor_row$Life_stages[1])) as.character(stressor_row$Life_stages[1]) else "NA")
        updateSelectInput(session, "s_Parameters",
          selected = if(!is.na(stressor_row$Parameters[1])) stressor_row$Parameters[1] else "survival")
        updateTextInput(session, "s_Units",
          value = if(!is.na(stressor_row$Units[1])) as.character(stressor_row$Units[1]) else "units")
      }

      # 2. Get SR table data from sr_dat
      sr_data <- session$userData$rv_stressor_response$sr_dat[[selected_stressor]]

      if (!is.null(sr_data) && nrow(sr_data) > 0) {
        # Convert to the format expected by sr_values()
        new_table <- data.frame(
          `Raw Stressor Values` = as.character(sr_data$value),
          `Stressor-Response Score (0 to 100)` = as.character(sr_data$mean_system_capacity),
          `SD` = as.character(sr_data$sd),
          `Lower Limit (0 to 100)` = as.character(sr_data$lwr),
          `Upper Limit (0 to 100)` = as.character(sr_data$upr),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )

        # Update the reactive value - this will re-render the rhandsontable
        sr_values(new_table)
      }
    }, ignoreInit = TRUE)


    #------------------------------------------------------------------------
    # Save data to new SR relationship
    #------------------------------------------------------------------------
    observeEvent(input$save_data, {

      print("Save new SR function...")

      # Process SR table data using helper function
      sr_result <- process_sr_table(sr_values())

      if (!sr_result$valid) {
        showNotification("Error: Stressor-Response table is incomplete. Please fill in all Raw Stressor Values.",
                         type = "error", duration = 5)
        return()
      }

      table_vals <- sr_result$table_vals

      # Validate formula-generated stressor values (if formula builder was used)
      if (is.null(rv_formula_df$dat)) {
        showNotification("Error: No formula data found. Please run the formula first.",
                         type = "error", duration = 5)
        return()
      }

      if (is.null(rv_formula_df$dat$val) || all(is.na(rv_formula_df$dat$val))) {
        showNotification("Error: Formula stressor values are all NA. Please check your formula and run it again.",
                         type = "error", duration = 8)
        return()
      }

      # Validate stressor name
      existing_names <- session$userData$rv_stressor_response$stressor_names
      name_result <- validate_stressor_name(input$s_New_Stressor_Name, existing_names)

      if (!name_result$valid) {
        showNotification(paste("Error:", name_result$error), type = "error", duration = 5)
        return()
      }

      stressor_name <- name_result$name

      # Build new row for main worksheet
      new_row <- data.frame(
        Stressors = stressor_name,
        Stressor_cat = stressor_name,
        Interaction = if (is.null(input$s_Interaction) || input$s_Interaction == "NA") NA else input$s_Interaction,
        Linked = if (is.null(input$s_Linked) || input$s_Linked == "" || input$s_Linked == "NA") NA else input$s_Linked,
        Stress_Scale = input$s_Stress_Scale,
        Function = input$s_Function,
        Life_stages = if (is.null(input$s_Life_stages) || input$s_Life_stages == "" || input$s_Life_stages == "NA") NA else input$s_Life_stages,
        Parameters = input$s_Parameters,
        Units = input$s_Units,
        Model = input$s_Model,
        stringsAsFactors = FALSE
      )

      print("flush out old data...")

      # Update refresh time
      session$userData$rv_stressor_response$active_refresh <- Sys.time()

      # Gather input for main worksheet - ensure column compatibility
      main_sheet_cols <- colnames(session$userData$rv_stressor_response$main_sheet)
      # Add any missing columns to new_row with NA values
      for (col in main_sheet_cols) {
        if (!(col %in% colnames(new_row))) {
          new_row[[col]] <- NA
        }
      }
      # Select only columns that exist in main_sheet, in the same order
      new_row <- new_row[, main_sheet_cols, drop = FALSE]
      session$userData$rv_stressor_response$main_sheet <- rbind(session$userData$rv_stressor_response$main_sheet, new_row)

      # Add stressor name
      session$userData$rv_stressor_response$stressor_names <- c(session$userData$rv_stressor_response$stressor_names, stressor_name)

      # Add pretty name
      session$userData$rv_stressor_response$pretty_names <- c(session$userData$rv_stressor_response$pretty_names, gsub("_", " ", stressor_name))

      # Add the SR data table - make sure it is added as a tibble
      session$userData$rv_stressor_response$sr_dat[[stressor_name]] <- dplyr::tibble(table_vals[, c("value", "mean_system_capacity", "sd", "lwr", "upr")])

      # Add on data to stressor-magnitude table
      bnsm <- rv_formula_df$dat

      # Ensure HUC_ID type matches existing sm_dat
      existing_sm <- session$userData$rv_stressor_magnitude$sm_dat
      if (is.numeric(existing_sm$HUC_ID)) {
        bnsm$HUC_ID <- as.numeric(bnsm$HUC_ID)
      } else {
        bnsm$HUC_ID <- as.character(bnsm$HUC_ID)
      }

      # Set stressor columns
      bnsm$Stressor <- stressor_name
      bnsm$Stressor_cat <- stressor_name

      # Set numeric columns with proper types
      bnsm$Mean <- as.numeric(bnsm$val)
      bnsm$SD <- as.numeric(0)
      bnsm$Distribution <- "normal"
      bnsm$Low_Limit <- as.numeric(min(bnsm$val, na.rm = TRUE))
      bnsm$Up_Limit <- as.numeric(max(bnsm$val, na.rm = TRUE))
      bnsm$Comments <- NA

      # Remove the temporary 'val' column
      bnsm$val <- NULL

      # Ensure column compatibility with sm_dat - add any missing columns with NA
      sm_cols <- colnames(existing_sm)
      for (col in sm_cols) {
        if (!(col %in% colnames(bnsm))) {
          bnsm[[col]] <- NA
        }
      }
      # Select only sm_dat columns in correct order
      bnsm <- bnsm[, sm_cols, drop = FALSE]

      # Debug: print info about what we're adding
      print(paste("Adding", nrow(bnsm), "rows for stressor:", stressor_name))
      print(paste("Mean values range:", min(bnsm$Mean, na.rm = TRUE), "to", max(bnsm$Mean, na.rm = TRUE)))
      print(paste("HUC_ID type in bnsm:", class(bnsm$HUC_ID)))
      print(paste("HUC_ID sample from bnsm:", paste(head(bnsm$HUC_ID, 5), collapse = ", ")))

      # Check existing sm_dat HUC_IDs
      print(paste("HUC_ID type in existing sm_dat:", class(existing_sm$HUC_ID)))
      print(paste("HUC_ID sample from existing sm_dat:", paste(head(unique(existing_sm$HUC_ID), 5), collapse = ", ")))

      # Check spatial layer HUC_IDs
      if (!is.null(session$userData$rv_HUC_geom$huc_geom)) {
        spatial_hucs <- session$userData$rv_HUC_geom$huc_geom$HUC_ID
        print(paste("HUC_ID type in spatial layer:", class(spatial_hucs)))
        print(paste("HUC_ID sample from spatial layer:", paste(head(unique(spatial_hucs), 5), collapse = ", ")))

        # Check if bnsm HUC_IDs match spatial HUC_IDs
        matches <- sum(bnsm$HUC_ID %in% spatial_hucs)
        print(paste("Number of bnsm HUC_IDs matching spatial layer:", matches, "out of", nrow(bnsm)))
      }

      # Merge onto master sm data
      session$userData$rv_stressor_magnitude$sm_dat <- rbind(
        existing_sm,
        bnsm
      )

      # Trigger refresh for stressor magnitude data
      session$userData$rv_stressor_magnitude$active_refresh <- Sys.time()

      # Also trigger a redraw of the map
      session$userData$rv_redraw$redraw <- runif(1)

      # When save is successful, render a green success message
      showNotification(paste0("New Stressor '", stressor_name, "' and Stressor-Response relationship was created!"),
                       type = "default",
                       duration = 5)

      # Reset the stressor name input to avoid accidental duplicates
      updateTextInput(session, "s_New_Stressor_Name", value = "New_Stressor")

    })

    #------------------------------------------------------------------------
    # Observer to enable/disable save button based on SR table validity
    #------------------------------------------------------------------------
    observe({
      # Check if SR table is valid
      sr_result <- process_sr_table(sr_values())
      sr_valid <- sr_result$valid

      # Check if formula data exists and is valid
      formula_valid <- !is.null(rv_formula_df$dat) &&
                       !is.null(rv_formula_df$dat$val) &&
                       !all(is.na(rv_formula_df$dat$val))

      # Check if stressor name is non-empty
      name_valid <- !is.null(input$s_New_Stressor_Name) &&
                    nchar(trimws(input$s_New_Stressor_Name)) >= 2

      # Enable or disable the save button
      if (sr_valid && formula_valid && name_valid) {
        shinyjs::enable("save_data")
      } else {
        shinyjs::disable("save_data")
      }
    })


  })
}
