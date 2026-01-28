module_matrix_model_inputs_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Tabbed Panels for the various model inputs
    tabsetPanel(
      ## Upload Data Tab (first tab)
      tabPanel(
        "Upload Data",
        tags$br(),

        tags$p(
          "Upload your life cycle profile and habitat capacities files here. These files define the vital rates
          and density-dependent constraints for your population model. Changes made here will update parameters
          across all other tabs.",
          class = "pm-ht"
        ),

        shinydashboard::box(
          width = 12,
          title = "Life Cycle Profile",
          collapsible = TRUE,
          collapsed = FALSE,

          fluidRow(
            column(
              width = 6,
              tags$p("Load Life Cycles Profile File: Vital Rates"),
              fileInput(
                ns("up_vital2"),
                label = "life cycles.csv",
                multiple = FALSE,
                accept = c(".csv")
              )
            ),
            column(
              width = 6,
              tags$p(
                tags$strong("Nstage"), " - Number of life stages in your model (1-10).",
                class = "pm-ht"
              ),
              numericInput(ns("Nstage"), label = "Nstage (# Stages)", value = life_stages$Value[life_stages$Name == "Nstage"])
            )
          ),

          fluidRow(
            column(
              width = 12,
              checkboxInput(
                ns("anadromous"),
                label = "Does your species follow an anadromous life history (Yes/checked = semelparous for salmon; No/unchecked = iteroparous for trout etc.)",
                value = life_stages$Value[life_stages$Name == "anadromous"]
              )
            )
          ),

          div(style = "color: #ffffff; background: #ff000059; border-radius: 5px; margin: 5px;", textOutput(ns(
            "upload_error_msg_vitals"
          )))
        ),

        shinydashboard::box(
          width = 12,
          title = "Habitat Capacities",
          collapsible = TRUE,
          collapsed = FALSE,

          tags$p(
            "Upload location-specific carrying capacities for density-dependent constraints.
            This file is optional but recommended if you have habitat data.",
            class = "pm-ht"
          ),

          fluidRow(
            column(
              width = 6,
              fileInput(
                ns("up_habitat_capacities"),
                label = "habitat_capacities.csv",
                multiple = FALSE,
                accept = c(".csv")
              )
            )
          )
        ),

        tags$hr(style = "margin: 25px 0; border-top: 2px solid #3c8dbc;"),

        tags$h4("Stressor Data for Population Model", style = "color: #3c8dbc; margin-bottom: 15px;"),

        tags$p(
          "The Population Model uses stressor-response relationships to modify vital rates based on environmental conditions.
          Upload stressor data here to apply cumulative effects to your population projections.",
          class = "pm-ht"
        ),

        shinydashboard::box(
          width = 12,
          title = "Stressor Response Workbook",
          collapsible = TRUE,
          collapsed = FALSE,

          tags$p(
            "Defines how environmental stressors affect survival, capacity, and fecundity at each life stage.
            The Population Model uses these relationships to adjust vital rates.",
            class = "pm-ht"
          ),

          tags$details(
            tags$summary(
              style = "cursor: pointer; color: #337ab7; font-weight: bold; margin-bottom: 10px;",
              "View format requirements..."
            ),
            tags$div(
              style = "padding: 10px; background-color: #f9f9f9; border-radius: 4px; margin-bottom: 15px;",
              tags$p(tags$b("Format:"), " Excel workbook (.xlsx)"),
              tags$p(tags$b("Required worksheets:")),
              tags$ul(
                tags$li(tags$b("Main"), " - Index with columns: ", tags$code("Stressors"), ", ", tags$code("Life_stages"), " (e.g., stage_0, stage_1, adult), ", tags$code("Parameters"), " (survival, capacity, or fecundity)"),
                tags$li(tags$b("Individual stressor worksheets"), " - Dose-response data with columns: ", tags$code("value"), ", ", tags$code("mean_system_capacity"), ", ", tags$code("sd"), ", ", tags$code("lwr"), ", ", tags$code("upr"))
              ),
              tags$p(
                tags$a(href = "https://mattjbayly.github.io/CEMPRA_documentation/05_data_inputs.html#stressor-response-workbook",
                       target = "_blank", icon("external-link-alt"), " Full format specification")
              )
            )
          ),

          fluidRow(
            column(
              width = 6,
              fileInput(
                ns("up_sr_wb_pop"),
                label = "Stressor Response Workbook (xlsx)",
                multiple = FALSE,
                accept = c(".xlsx")
              )
            ),
            column(
              width = 6,
              div(style = "color: #ffffff; background: #ff000059; border-radius: 5px; margin: 5px; padding: 5px;",
                  textOutput(ns("upload_error_msg_sr_pop")))
            )
          )
        ),

        shinydashboard::box(
          width = 12,
          title = "Stressor Magnitude Workbook",
          collapsible = TRUE,
          collapsed = FALSE,

          tags$p(
            "Contains location-specific stressor values that determine how environmental conditions
            modify population vital rates at each site.",
            class = "pm-ht"
          ),

          tags$details(
            tags$summary(
              style = "cursor: pointer; color: #337ab7; font-weight: bold; margin-bottom: 10px;",
              "View format requirements..."
            ),
            tags$div(
              style = "padding: 10px; background-color: #f9f9f9; border-radius: 4px; margin-bottom: 15px;",
              tags$p(tags$b("Format:"), " Excel workbook (.xlsx)"),
              tags$p(tags$b("Required columns:")),
              tags$ul(
                tags$li(tags$code("HUC_ID"), " - Location identifier (must match spatial data and habitat capacities)"),
                tags$li(tags$code("NAME"), " - Location name"),
                tags$li(tags$code("Stressor"), " - Stressor name (must match Stressor Response Workbook)"),
                tags$li(tags$code("Mean"), ", ", tags$code("SD"), ", ", tags$code("Distribution"), ", ", tags$code("Low_Limit"), ", ", tags$code("Up_Limit"))
              ),
              tags$p(tags$em("Tip: Use different worksheets for baseline vs. scenario conditions.")),
              tags$p(
                tags$a(href = "https://mattjbayly.github.io/CEMPRA_documentation/05_data_inputs.html#stressor-magnitude-workbook",
                       target = "_blank", icon("external-link-alt"), " Full format specification")
              )
            )
          ),

          fluidRow(
            column(
              width = 6,
              fileInput(
                ns("up_sm_wb_pop"),
                label = "Stressor Magnitude Workbook (xlsx)",
                multiple = FALSE,
                accept = c(".xlsx")
              )
            ),
            column(
              width = 6,
              div(style = "color: #ffffff; background: #ff000059; border-radius: 5px; margin: 5px; padding: 5px;",
                  textOutput(ns("upload_error_msg_sm_pop")))
            )
          )
        ),

        tags$br()
      ),

      ## Survival Parameters Tab
      tabPanel(
        "Survival",

        tags$br(),
        tags$p(
          "Define the individual survivorship probabilities for each stage class evaluated on annual time steps in the simulation.
          These represent density-independent survival rates; density-dependent constraints are configured separately in the Density Dependence tab.",
          class = "pm-ht"
        ),

        # Conditional explanatory text for non-anadromous mode
        conditionalPanel(
          condition = sprintf("input['%s'] == false", ns("anadromous")),
          tags$div(
            style = "background-color: #f0f7fb; border-left: 4px solid #3c8dbc; padding: 10px; margin-bottom: 15px;",
            tags$strong("Non-Anadromous Mode: "),
            tags$span(
              "SE (egg survival) and S0 (fry survival) govern early life transitions incorporated into the fecundity term.
              surv_1 through surv_N represent annual stage-to-stage transition probabilities. Individuals may spend
              multiple years in each stage (configured in the Growth tab), with survival applied each year."
            )
          )
        ),

        # Conditional explanatory text for anadromous mode
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("anadromous")),
          tags$div(
            style = "background-color: #fef9e7; border-left: 4px solid #f39c12; padding: 10px; margin-bottom: 15px;",
            tags$strong("Anadromous Mode: "),
            tags$span(
              "SE (egg-to-fry survival) and S0 (fry to age-1) govern transitions from age-0 to age-1. surv_1 typically represents survival from age-1 to age-2. Subsequent surv_X values govern pre-breeder (Pb) marine survival between age classes. For salmon,
              we recommend an age-based model where each stage equals one year (all year_X = 1 in Growth tab)."
            )
          )
        ),

        # Survival probabilities
        fluidRow(
          column(
            width = 4,
            numericInput(ns("SE"), label = "SE (Egg Survival)", value = life_stages$Value[life_stages$Name == "SE"])
          ),
          column(
            width = 4,
            numericInput(ns("S0"), label = "S0 (YOY Survival)", value = life_stages$Value[life_stages$Name == "S0"])
          ),
          column(
            width = 4,
            numericInput(ns("surv_1"), label = "surv_1 (Stage 1 Survival)", value = life_stages$Value[life_stages$Name == "surv_1"])
          )
        ),
        fluidRow(
          column(
            width = 4,
            numericInput(ns("surv_2"), label = "surv_2 (Stage 2 Survival)", value = life_stages$Value[life_stages$Name == "surv_2"])
          ),
          column(
            width = 4,
            numericInput(ns("surv_3"), label = "surv_3 (Stage 3 Survival)", value = life_stages$Value[life_stages$Name == "surv_3"])
          ),
          column(
            width = 4,
            numericInput(ns("surv_4"), label = "surv_4 (Stage 4 Survival)", value = life_stages$Value[life_stages$Name == "surv_4"])
          )
        ),
        fluidRow(
          column(
            width = 4,
            numericInput(ns("surv_5"), label = "surv_5 (Stage 5 Survival)", value = life_stages$Value[life_stages$Name == "surv_5"])
          ),
          column(
            width = 4,
            numericInput(ns("surv_6"), label = "surv_6 (Stage 6 Survival)", value = life_stages$Value[life_stages$Name == "surv_6"])
          ),
          column(
            width = 4,
            numericInput(ns("surv_7"), label = "surv_7 (Stage 7 Survival)", value = life_stages$Value[life_stages$Name == "surv_7"])
          )
        ),
        fluidRow(
          column(
            width = 4,
            numericInput(ns("surv_8"), label = "surv_8 (Stage 8 Survival)", value = life_stages$Value[life_stages$Name == "surv_8"])
          ),
          column(
            width = 4,
            numericInput(ns("surv_9"), label = "surv_9 (Stage 9 Survival)", value = life_stages$Value[life_stages$Name == "surv_9"])
          ),
          column(
            width = 4,
            numericInput(ns("surv_10"), label = "surv_10 (Stage 10 Survival)", value = life_stages$Value[life_stages$Name == "surv_10"])
          )
        ),
        # Anadromous-only survival parameters (spawner migration and pre-spawn)
        # Hidden when running in non-anadromous mode
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("anadromous")),

          tags$h5("Spawning Migration Survival (Anadromous Only)"),
          tags$p(
            "Control survival during migration to spawning grounds. This typically represents mortality
            prior to when spawner enumeration can occur (e.g., in-river migration mortality).",
            class = "pm-ht", style = "font-size: 12px; color: #666;"
          ),
          fluidRow(
            column(
              width = 3,
              numericInput(ns("smig_1"), label = "smig_1 (Mig. Surv. Age-1)", value = life_stages$Value[life_stages$Name == "smig_1"])
            ),
            column(
              width = 3,
              numericInput(ns("smig_2"), label = "smig_2 (Mig. Surv. Age-2)", value = life_stages$Value[life_stages$Name == "smig_2"])
            ),
            column(
              width = 3,
              numericInput(ns("smig_3"), label = "smig_3 (Mig. Surv. Age-3)", value = life_stages$Value[life_stages$Name == "smig_3"])
            ),
            column(
              width = 3,
              numericInput(ns("smig_4"), label = "smig_4 (Mig. Surv. Age-4)", value = life_stages$Value[life_stages$Name == "smig_4"])
            )
          ),
          fluidRow(
            column(
              width = 3,
              numericInput(ns("smig_5"), label = "smig_5 (Mig. Surv. Age-5)", value = life_stages$Value[life_stages$Name == "smig_5"])
            ),
            column(
              width = 3,
              numericInput(ns("smig_6"), label = "smig_6 (Mig. Surv. Age-6)", value = life_stages$Value[life_stages$Name == "smig_6"])
            ),
            column(
              width = 3,
              numericInput(ns("smig_7"), label = "smig_7 (Mig. Surv. Age-7)", value = life_stages$Value[life_stages$Name == "smig_7"])
            ),
            column(
              width = 3,
              numericInput(ns("smig_8"), label = "smig_8 (Mig. Surv. Age-8)", value = life_stages$Value[life_stages$Name == "smig_8"])
            )
          ),
          fluidRow(
            column(
              width = 3,
              numericInput(ns("smig_9"), label = "smig_9 (Mig. Surv. Age-9)", value = life_stages$Value[life_stages$Name == "smig_9"])
            ),
            column(
              width = 3,
              numericInput(ns("smig_10"), label = "smig_10 (Mig. Surv. Age-10)", value = life_stages$Value[life_stages$Name == "smig_10"])
            )
          ),

          tags$h5("Pre-spawn Survival (Anadromous Only)"),
          tags$p(
            "Represent mortality after spawning enumeration occurs but before successful reproduction
            (e.g., prespawn mortality, effective spawners). Set to 1.0 if unknown.",
            class = "pm-ht", style = "font-size: 12px; color: #666;"
          ),
          fluidRow(
            column(
              width = 3,
              numericInput(ns("u_1"), label = "u_1 (Pre. Spwn. Surv. Age-1)", value = life_stages$Value[life_stages$Name == "u_1"])
            ),
            column(
              width = 3,
              numericInput(ns("u_2"), label = "u_2 (Pre. Spwn. Surv. Age-2)", value = life_stages$Value[life_stages$Name == "u_2"])
            ),
            column(
              width = 3,
              numericInput(ns("u_3"), label = "u_3 (Pre. Spwn. Surv. Age-3)", value = life_stages$Value[life_stages$Name == "u_3"])
            ),
            column(
              width = 3,
              numericInput(ns("u_4"), label = "u_4 (Pre. Spwn. Surv. Age-4)", value = life_stages$Value[life_stages$Name == "u_4"])
            )
          ),
          fluidRow(
            column(
              width = 3,
              numericInput(ns("u_5"), label = "u_5 (Pre. Spwn. Surv. Age-5)", value = life_stages$Value[life_stages$Name == "u_5"])
            ),
            column(
              width = 3,
              numericInput(ns("u_6"), label = "u_6 (Pre. Spwn. Surv. Age-6)", value = life_stages$Value[life_stages$Name == "u_6"])
            ),
            column(
              width = 3,
              numericInput(ns("u_7"), label = "u_7 (Pre. Spwn. Surv. Age-7)", value = life_stages$Value[life_stages$Name == "u_7"])
            ),
            column(
              width = 3,
              numericInput(ns("u_8"), label = "u_8 (Pre. Spwn. Surv. Age-8)", value = life_stages$Value[life_stages$Name == "u_8"])
            )
          ),
          fluidRow(
            column(
              width = 3,
              numericInput(ns("u_9"), label = "u_9 (Pre. Spwn. Surv. Age-9)", value = life_stages$Value[life_stages$Name == "u_9"])
            ),
            column(
              width = 3,
              numericInput(ns("u_10"), label = "u_10 (Pre. Spwn. Surv. Age-10)", value = life_stages$Value[life_stages$Name == "u_10"])
            )
          )
        ),  # end conditionalPanel for anadromous-only parameters

        tags$h5("Stochastic Survival Parameters"),
        tags$p(
          "These parameters control interannual variability in survival rates across simulations.
          Higher values increase volatility in population projections, useful for assessing population
          viability and extinction risk under environmental uncertainty.",
          class = "pm-ht", style = "font-size: 12px; color: #666;"
        ),
        fluidRow(
          column(
            width = 4,
            numericInput(ns("M.cv"), label = "M.cv (Coefficient of variation in stage-specific mortality)", value = life_stages$Value[life_stages$Name == "M.cv"]),
            tags$small(
              style = "color: #888; display: block; margin-top: -10px;",
              "Controls year-to-year variability in survival rates (beta distribution). Typical values: 0.05-0.20.
              Higher values = more variable survival between years."
            )
          ),
          column(
            width = 4,
            numericInput(ns("M.rho"), label = "M.rho (Correlation in mortality through time)", value = life_stages$Value[life_stages$Name == "M.rho"]),
            tags$small(
              style = "color: #888; display: block; margin-top: -10px;",
              "Correlation in good/bad years across stage classes (0-1). Low values = stages compensate independently.
              High values = all cohorts experience good/bad years simultaneously, increasing volatility."
            )
          ),
          column(
            width = 4,
            numericInput(ns("p.cat"), label = "p.cat (Probability of catastrophic event per generation)", value = life_stages$Value[life_stages$Name == "p.cat"]),
            tags$small(
              style = "color: #888; display: block; margin-top: -10px;",
              "Annual probability of a catastrophic mortality event (e.g., disease, drought). Scaled to generation time.
              Set to 0 to disable. Typical values: 0-0.05."
            )
          )
        ),
        tags$h5("Sex Ratio"),
        fluidRow(column(
          width = 6,
          numericInput(ns("SR"), label = "SR - Sex ratio (portion female at birth)", value = life_stages$Value[life_stages$Name == "SR"])
        ))
      ),
      # end Survival Parameters tab
      
      ## Growth Parameters Tab
      tabPanel(
        "Growth",
        tags$br(),
        tags$p(
          "Growth is represented as time (years) spent in each stage. This approach avoids defining size attributes
          and allows flexibility when parameterizing different species. Stage-to-stage transition probabilities
          account for the number of years spent in each stage.",
          class = "pm-ht"
        ),

        # Conditional explanatory text for non-anadromous mode
        conditionalPanel(
          condition = sprintf("input['%s'] == false", ns("anadromous")),
          tags$div(
            style = "background-color: #f0f7fb; border-left: 4px solid #3c8dbc; padding: 10px; margin-bottom: 15px;",
            tags$strong("Non-Anadromous Mode: "),
            tags$span(
              "Individuals can spend multiple years in each stage (e.g., year_4 = 5 means adults can remain
              in stage 4 for up to 5 years). This creates a stage-structured matrix where some individuals
              advance to the next stage while others remain. Setting all year_X = 1 creates an age-based Leslie matrix."
            )
          )
        ),

        # Conditional explanatory text for anadromous mode
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("anadromous")),
          tags$div(
            style = "background-color: #fef9e7; border-left: 4px solid #f39c12; padding: 10px; margin-bottom: 15px;",
            tags$strong("Anadromous Mode: "),
            tags$span(
              "For salmon and other anadromous species, we strongly recommend setting all year_X values to 1,
              creating an age-based Leslie matrix where each stage represents exactly one year of age.
              This simplifies interpretation and avoids confusion with pre-breeder (Pb) and breeder (B) pathways."
            )
          )
        ),

        fluidRow(
          column(
            width = 3,
            numericInput(ns("year_1"), label = "year_1 (Years as Stage 1)", value = life_stages$Value[life_stages$Name == "year_1"])
          ),
          column(
            width = 3,
            numericInput(ns("year_2"), label = "year_2 (Years as Stage 2)", value = life_stages$Value[life_stages$Name == "year_2"])
          ),
          column(
            width = 3,
            numericInput(ns("year_3"), label = "year_3 (Years as Stage 3)", value = life_stages$Value[life_stages$Name == "year_3"])
          ),
          column(
            width = 3,
            numericInput(ns("year_4"), label = "year_4 (Years as Stage 4)", value = life_stages$Value[life_stages$Name == "year_4"])
          )
        ),
        fluidRow(
          column(
            width = 3,
            numericInput(ns("year_5"), label = "year_5 (Years as Stage 5)", value = life_stages$Value[life_stages$Name == "year_5"])
          ),
          column(
            width = 3,
            numericInput(ns("year_6"), label = "year_6 (Years as Stage 6)", value = life_stages$Value[life_stages$Name == "year_6"])
          ),
          column(
            width = 3,
            numericInput(ns("year_7"), label = "year_7 (Years as Stage 7)", value = life_stages$Value[life_stages$Name == "year_7"])
          ),
          column(
            width = 3,
            numericInput(ns("year_8"), label = "year_8 (Years as Stage 8)", value = life_stages$Value[life_stages$Name == "year_8"])
          )
        ),
        fluidRow(
          column(
            width = 3,
            numericInput(ns("year_9"), label = "year_9 (Years as Stage 9)", value = life_stages$Value[life_stages$Name == "year_9"])
          ),
          column(
            width = 3,
            numericInput(ns("year_10"), label = "year_10 (Years as Stage 10)", value = life_stages$Value[life_stages$Name == "year_10"])
          )
        )
      ),
      # end Growth Parameters tab
      
      ## Reproduction Parameters Tab
      tabPanel(
        "Reproduction",
        tags$br(),
        tags$p(
          "Configure reproduction parameters including fecundity (eggs per female), maturity schedules,
          and sex ratio. The fecundity element of the matrix incorporates egg survival (SE) and fry survival (S0)
          from the Survival tab.",
          class = "pm-ht"
        ),

        # Conditional explanatory text for non-anadromous mode
        conditionalPanel(
          condition = sprintf("input['%s'] == false", ns("anadromous")),
          tags$div(
            style = "background-color: #f0f7fb; border-left: 4px solid #3c8dbc; padding: 10px; margin-bottom: 15px;",
            tags$strong("Non-Anadromous Mode: "),
            tags$span(
              "For iteroparous species (trout, char, etc.), mature individuals can reproduce multiple times.
              Set mat_X to define the proportion mature at each stage (e.g., mat_4 = 1 means 100% of stage-4
              individuals are mature). Use a single eps value for mean eggs per female, or specify eps_X for
              stage-specific fecundity if larger/older fish produce more eggs."
            )
          )
        ),

        # Conditional explanatory text for anadromous mode
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("anadromous")),
          tags$div(
            style = "background-color: #fef9e7; border-left: 4px solid #f39c12; padding: 10px; margin-bottom: 15px;",
            tags$strong("Anadromous Mode: "),
            tags$span(
              "For semelparous salmon, mat_X defines the probability of returning to spawn at each age
              (e.g., mat_3 = 0.15, mat_4 = 0.70, mat_5 = 1.0). The oldest age class should have mat = 1.0.
              Use age-specific fecundity (eps_3, eps_4, eps_5) since older/larger fish typically produce more eggs.
              Configure spawner migration (smig_X) and prespawn survival (u_X) in Review Inputs if needed."
            )
          )
        ),

        fluidRow(
          column(
            width = 4,
            numericInput(ns("events"), label = "events (Spawn Events per Female)", value = life_stages$Value[life_stages$Name == "events"]),
            tags$small(
              style = "color: #888; display: block; margin-top: -10px;",
              "Number of spawning events per female per year. Almost always set to 1. Keep at 1 even for systems
              with multiple life history variants (e.g., Spring & Fall Chinook) - use separate profiles instead."
            )
          ),
          column(
            width = 4,
            numericInput(ns("eps_sd"), label = "eps_sd (SD variation in Eggs per Female)", value = life_stages$Value[life_stages$Name == "eps_sd"]),
            tags$small(
              style = "color: #888; display: block; margin-top: -10px;",
              "Standard deviation in eggs-per-spawner across years and replicates. Controls fecundity variability.
              Higher values create more variable recruitment. Density-dependent constraints may attenuate effects."
            )
          )
        ),

        tags$hr(style = "margin-top: 25px; margin-bottom: 20px; border-top: 1px solid #ddd;"),

        tags$h5("Eggs per Female Spawner (Fecundity)"),
        tags$p(
          "Eggs per female spawner (eps) per mature individual for each stage/age class. Values represent
          the average number of eggs produced by a spawning female.",
          class = "pm-ht"
        ),

        # Conditional explanatory text for eps - non-anadromous
        conditionalPanel(
          condition = sprintf("input['%s'] == false", ns("anadromous")),
          tags$div(
            style = "background-color: #f0f7fb; border-left: 4px solid #3c8dbc; padding: 10px; margin-bottom: 15px;",
            tags$strong("Non-Anadromous: "),
            tags$span(
              "Typically only one eps value is needed (e.g., eps_4 if stage 4 is the first mature stage).
              Set eps to 0 for immature stages. If fecundity increases with age/size, you can specify
              different values for each mature stage."
            )
          )
        ),

        # Conditional explanatory text for eps - anadromous
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("anadromous")),
          tags$div(
            style = "background-color: #fef9e7; border-left: 4px solid #f39c12; padding: 10px; margin-bottom: 15px;",
            tags$strong("Anadromous: "),
            tags$span(
              "Use age-specific fecundity values (eps_3, eps_4, eps_5, etc.) since older/larger salmon
              typically produce more eggs. Set eps to 0 for ages that don't spawn. Fecundity is multiplied
              by the maturity schedule (mat_X) to determine reproductive contribution."
            )
          )
        ),

        fluidRow(
          column(
            width = 3,
            numericInput(ns("eps_1"), label = "eps_1 (Stage 1)", value = life_stages$Value[life_stages$Name == "eps_1"])
          ),
          column(
            width = 3,
            numericInput(ns("eps_2"), label = "eps_2 (Stage 2)", value = life_stages$Value[life_stages$Name == "eps_2"])
          ),
          column(
            width = 3,
            numericInput(ns("eps_3"), label = "eps_3 (Stage 3)", value = life_stages$Value[life_stages$Name == "eps_3"])
          ),
          column(
            width = 3,
            numericInput(ns("eps_4"), label = "eps_4 (Stage 4)", value = life_stages$Value[life_stages$Name == "eps_4"])
          )
        ),
        fluidRow(
          column(
            width = 3,
            numericInput(ns("eps_5"), label = "eps_5 (Stage 5)", value = life_stages$Value[life_stages$Name == "eps_5"])
          ),
          column(
            width = 3,
            numericInput(ns("eps_6"), label = "eps_6 (Stage 6)", value = life_stages$Value[life_stages$Name == "eps_6"])
          ),
          column(
            width = 3,
            numericInput(ns("eps_7"), label = "eps_7 (Stage 7)", value = life_stages$Value[life_stages$Name == "eps_7"])
          ),
          column(
            width = 3,
            numericInput(ns("eps_8"), label = "eps_8 (Stage 8)", value = life_stages$Value[life_stages$Name == "eps_8"])
          )
        ),
        fluidRow(
          column(
            width = 3,
            numericInput(ns("eps_9"), label = "eps_9 (Stage 9)", value = life_stages$Value[life_stages$Name == "eps_9"])
          ),
          column(
            width = 3,
            numericInput(ns("eps_10"), label = "eps_10 (Stage 10)", value = life_stages$Value[life_stages$Name == "eps_10"])
          )
        ),
        tags$h5("Fecundity Variability and Spawning Interval"),
        tags$p(
          "These parameters control temporal correlation in fecundity and the spawning schedule.",
          class = "pm-ht", style = "font-size: 12px; color: #666;"
        ),
        fluidRow(
          column(
            width = 6,
            numericInput(ns("egg_rho"), label = "egg_rho (Correlation in egg fecundity through time)", value = life_stages$Value[life_stages$Name == "egg_rho"]),
            tags$small(
              style = "color: #888; display: block; margin-top: -10px;",
              "Correlation in fecundity between age classes within a year (0-1). If multiple mature stages contribute
              to spawning, this controls whether good/bad years are synchronized across age classes. Low values allow
              cohorts to compensate for each other; high values increase population volatility."
            )
          ),
          column(
            width = 6,
            numericInput(ns("int"), label = "int (Spawning Interval - years)", value = life_stages$Value[life_stages$Name == "int"]),
            tags$small(
              style = "color: #888; display: block; margin-top: -10px;",
              "Spawning interval in years. Almost always set to 1 (annual spawning). Values >1 indicate that mature
              individuals skip years between spawning events. Proceed with caution if using values other than 1."
            )
          )
        ),

        tags$hr(style = "margin-top: 25px; margin-bottom: 20px; border-top: 1px solid #ddd;"),

        tags$h5("Maturity Schedule"),
        tags$p(
          "The probability (proportion 0-1) that an individual is sexually mature at each stage/age class.
          This determines which stages contribute to reproduction in the population matrix.",
          class = "pm-ht"
        ),

        # Conditional explanatory text for maturity - non-anadromous
        conditionalPanel(
          condition = sprintf("input['%s'] == false", ns("anadromous")),
          tags$div(
            style = "background-color: #f0f7fb; border-left: 4px solid #3c8dbc; padding: 10px; margin-bottom: 15px;",
            tags$strong("Non-Anadromous: "),
            tags$span(
              "Set mat_X = 0 for juvenile stages and mat_X = 1 for fully mature stages. Intermediate values
              (e.g., mat_3 = 0.5) indicate partial maturity where only a fraction of individuals in that stage
              reproduce. For iteroparous species, mature individuals can spawn in multiple consecutive years."
            ),
            tags$div(
              style = "margin-top: 8px; font-style: italic; color: #666;",
              "Example (5-stage trout): mat_1 = 0, mat_2 = 0, mat_3 = 0, mat_4 = 0.5, mat_5 = 1.0"
            )
          )
        ),

        # Conditional explanatory text for maturity - anadromous
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("anadromous")),
          tags$div(
            style = "background-color: #fef9e7; border-left: 4px solid #f39c12; padding: 10px; margin-bottom: 15px;",
            tags$strong("Anadromous: "),
            tags$span(
              "For salmon, mat_X represents the probability of returning to spawn at each age. Values should
              sum conceptually to account for all fish eventually spawning. The oldest mature age class should
              have mat = 1.0 (all remaining fish spawn). Earlier ages have lower values representing the
              proportion that return early vs. stay at sea."
            ),
            tags$div(
              style = "margin-top: 8px; font-style: italic; color: #666;",
              "Example (Chinook, ages 3-5 spawn): mat_1 = 0, mat_2 = 0, mat_3 = 0.15, mat_4 = 0.70, mat_5 = 1.0"
            )
          )
        ),

        fluidRow(
          column(
            width = 3,
            numericInput(ns("mat_1"), label = "mat_1 (Maturity as Stage 1)", value = life_stages$Value[life_stages$Name == "mat_1"])
          ),
          column(
            width = 3,
            numericInput(ns("mat_2"), label = "mat_2 (Maturity as Stage 2)", value = life_stages$Value[life_stages$Name == "mat_2"])
          ),
          column(
            width = 3,
            numericInput(ns("mat_3"), label = "mat_3 (Maturity as Stage 3)", value = life_stages$Value[life_stages$Name == "mat_3"])
          ),
          column(
            width = 3,
            numericInput(ns("mat_4"), label = "mat_4 (Maturity as Stage 4)", value = life_stages$Value[life_stages$Name == "mat_4"])
          )
        ),
        fluidRow(
          column(
            width = 3,
            numericInput(ns("mat_5"), label = "mat_5 (Maturity as Stage 5)", value = life_stages$Value[life_stages$Name == "mat_5"])
          ),
          column(
            width = 3,
            numericInput(ns("mat_6"), label = "mat_6 (Maturity as Stage 6)", value = life_stages$Value[life_stages$Name == "mat_6"])
          ),
          column(
            width = 3,
            numericInput(ns("mat_7"), label = "mat_7 (Maturity as Stage 7)", value = life_stages$Value[life_stages$Name == "mat_7"])
          ),
          column(
            width = 3,
            numericInput(ns("mat_8"), label = "mat_8 (Maturity as Stage 8)", value = life_stages$Value[life_stages$Name == "mat_8"])
          )
        ),
        fluidRow(
          column(
            width = 3,
            numericInput(ns("mat_9"), label = "mat_9 (Maturity as Stage 9)", value = life_stages$Value[life_stages$Name == "mat_9"])
          ),
          column(
            width = 3,
            numericInput(ns("mat_10"), label = "mat_10 (Maturity as Stage 10)", value = life_stages$Value[life_stages$Name == "mat_10"])
          )
        )
      ),
      # end Reproduction Parameters tab
      
      ## Density Dependence Tab
      tabPanel(
        "Density Dependence",
        tags$br(),

        # Dynamic warning alerts for DD configuration issues
        uiOutput(ns("dd_config_warnings")),

        # Overview section
        shinydashboard::box(
          width = 12,
          title = "About Density Dependence",
          collapsible = TRUE,
          collapsed = FALSE,

          tags$p(
            "It is rare for natural populations to grow in perpetuity without any constraints on growth, survival, and reproduction.
            The life cycle model includes mechanisms to constrain population growth or limit high densities through ",
            tags$strong("density-dependent bottlenecks"), ".",
            class = "pm-ht"
          ),

          tags$p(
            "The CEMPRA tool has two mechanisms to incorporate density-dependent growth constraints:",
            class = "pm-ht"
          ),

          tags$ol(
            tags$li(
              tags$strong("Location and Stage-Specific Carrying Capacities (Recommended):"),
              " Use this approach when you have habitat data and can estimate maximum densities for specific life stages
              (e.g., 'Location X can produce up to 1,200 parr'). This provides an intuitive workflow with Beverton-Holt
              or Hockey-Stick functions."
            ),
            tags$li(
              tags$strong("Compensation Ratios:"),
              " An alternative approach that allows evaluation without specifying location-specific habitat availability.
              Uses adult carrying capacity and back-calculates stage-specific K values via stable-stage distribution.
              Recommended only for users familiar with compensation ratios in population ecology."
            )
          ),

          tags$details(
            tags$summary(
              style = "cursor: pointer; color: #337ab7; font-weight: bold; margin-top: 10px;",
              "Learn more about density-dependent growth..."
            ),
            tags$div(
              style = "padding: 10px; background-color: #f9f9f9; border-radius: 4px; margin-top: 10px;",
              tags$p(
                "Both mechanisms utilize the ", tags$strong("Beverton-Holt function"), " or a strict ",
                tags$strong("Hockey-Stick"), " threshold to constrain transitions at key demographic bottlenecks."
              ),
              tags$p(
                tags$strong("Beverton-Holt Function:"), " Calculates expected individuals in the next time step as a
                function of current abundance, carrying capacity (K), and baseline survival (S). As population approaches K,
                recruitment flattens asymptotically."
              ),
              tags$p(
                tags$strong("Hockey-Stick:"), " A simpler hard-cap approach where abundance cannot exceed K, regardless of productivity."
              ),
              tags$p(
                "For more details, see ",
                tags$a(href = "https://mattjbayly.github.io/CEMPRA_documentation/07_life_cycle_model.html#density-dependent-constraints-on-growth",
                       target = "_blank", "Chapter 7: Density-Dependent Constraints"),
                " in the CEMPRA documentation."
              )
            )
          )
        ),

        # Location-Specific Carrying Capacities Section
        shinydashboard::box(
          width = 12,

          tags$h3(
            "Define Density Dependence with Location-Specific Carrying Capacities"
          ),

          tags$p(
            "This is the ", tags$strong("recommended approach"), " for most applications. Define which life stages have
            density-dependent bottlenecks and specify location-specific carrying capacities for each stage.",
            class = "pm-ht"
          ),

          tags$details(
            tags$summary(
              style = "cursor: pointer; color: #337ab7; font-weight: bold;",
              "Learn more about setting up location-specific capacities..."
            ),
            tags$div(
              style = "padding: 10px; background-color: #f9f9f9; border-radius: 4px; margin-top: 10px;",

              tags$h5("Step 1: Define Density-Dependent Bottlenecks"),
              tags$p("Use the controls below to specify which life stages have density-dependent constraints and which function to use:"),

              tags$table(
                class = "table table-sm table-bordered",
                style = "font-size: 12px; margin-bottom: 15px;",
                tags$thead(
                  tags$tr(
                    tags$th("Name Tag", style = "width: 25%;"),
                    tags$th("Mechanism", style = "width: 20%;"),
                    tags$th("Description")
                  )
                ),
                tags$tbody(
                  tags$tr(tags$td("bh_stage_0"), tags$td("Beverton-Holt"), tags$td("Egg-to-fry transition limiting max fry")),
                  tags$tr(tags$td("hs_stage_0"), tags$td("Hockey-Stick"), tags$td("Hard cap on fry abundance")),
                  tags$tr(tags$td("bh_stage_1, bh_stage_2, ..."), tags$td("Beverton-Holt"), tags$td("Stage transitions with BH constraint")),
                  tags$tr(tags$td("hs_stage_1, hs_stage_2, ..."), tags$td("Hockey-Stick"), tags$td("Hard cap at each stage")),
                  tags$tr(tags$td("bh_spawners"), tags$td("Beverton-Holt"), tags$td("Total spawner capacity constraint")),
                  tags$tr(tags$td("hs_spawners"), tags$td("Hockey-Stick"), tags$td("Hard cap on total spawners"))
                )
              ),

              tags$p(
                tags$em("For anadromous species:"),
                " Use 'pb' (pre-breeder) or 'b' (breeder) suffixes, e.g., bh_stage_pb_1, bh_stage_b_3, etc."
              ),

              tags$h5("Step 2: Define Location-Specific K Values", style = "margin-top: 15px;"),
              tags$p("The habitat capacities table below specifies maximum individuals per stage per location. Required columns:"),
              tags$ul(
                tags$li(tags$code("HUC_ID"), " / ", tags$code("NAME"), ": Location identifiers"),
                tags$li(tags$code("k_stage_0_mean"), ": Fry carrying capacity"),
                tags$li(tags$code("k_stage_1_mean"), ", ", tags$code("k_stage_2_mean"), ", ...: Stage-specific capacities"),
                tags$li(tags$code("k_spawners_mean"), ": Total spawner capacity (anadromous)")
              ),

              tags$p(
                tags$a(href = "https://mattjbayly.github.io/CEMPRA_documentation/07_life_cycle_model.html#location-and-stage-specific-carrying-capacities",
                       target = "_blank", "See full documentation with examples...")
              )
            )
          ),

          tags$br(),

          module_matrix_dd_config_ui(ns("dd_config")),

          tags$br(),

          module_matrix_dd_cap_ui(ns("dd_cap"))

        ),
        
        tags$br(),
        
        
        shinydashboard::box(
          width = 12,

          tags$h3("(Optional) Define Density Dependence with Compensation Ratios"),

          tags$p(
            tags$em("Note: This approach is only recommended for advanced users familiar with compensation ratios in population ecology.
            For most applications, use the Location-Specific Carrying Capacities approach above."),
            style = "color: #856404; background-color: #fff3cd; padding: 10px; border-radius: 4px; margin-bottom: 15px;"
          ),

          tags$details(
            tags$summary(
              style = "cursor: pointer; color: #337ab7; font-weight: bold;",
              "Learn more about compensation ratios..."
            ),
            tags$div(
              style = "padding: 10px; background-color: #f9f9f9; border-radius: 4px; margin-top: 10px; margin-bottom: 15px;",

              tags$p(
                "Compensation ratios (CR) express density-dependent survival as a multiplier that varies with population density.
                Each life stage can have its own compensation ratio, which determines how strongly survival decreases as density increases."
              ),

              tags$p(
                tags$strong("How it works:"), " The formula is: ",
                tags$code("S_dd = S * (CR / (1 + (CR - 1) * (N / K)))"),
                " where S is baseline survival, CR is the compensation ratio, N is current abundance, and K is carrying capacity."
              ),

              tags$ul(
                tags$li(tags$strong("CR = 1:"), " No density dependence (survival is constant)"),
                tags$li(tags$strong("CR > 1:"), " Density-dependent survival that decreases as N approaches K"),
                tags$li(tags$strong("Higher CR:"), " Stronger density-dependent effect")
              ),

              tags$p(
                tags$strong("Important:"), " When using this approach, the fry survival rate (s0.1.det) will be automatically
                optimized to achieve population equilibrium (lambda = 1.0) at carrying capacity K. Stage-specific K values
                are back-calculated from the adult K using the stable-stage distribution."
              ),

              tags$p(
                tags$a(href = "https://mattjbayly.github.io/CEMPRA_documentation/07_life_cycle_model.html#compensation-ratios",
                       target = "_blank", "See full documentation on compensation ratios...")
              )
            )
          ),

          tags$p(
            "Set the adult carrying capacity (K) to define the mean equilibrium abundance of adults in the simulation.
            Carrying capacities for other life stages are derived from K using the stable-stage distribution.",
            class = "pm-ht"
          ),
          fluidRow(column(
            width = 12,
            numericInput(ns("k"), label = "(K) - Adult Carrying Capacity (for compensation ratios)", value = life_stages$Value[life_stages$Name == "k"])
          )),

          tags$br(),

          tags$p(
            "Specify compensation ratios for each life stage. Values greater than 1 activate density dependence for that stage.
            Leave at 1 for stages without density-dependent survival.",
            class = "pm-ht"
          ),
          fluidRow(
            column(
              width = 4,
              numericInput(ns("cr_E"), label = "cr_E (Egg Survival CR)", value = life_stages$Value[life_stages$Name == "cr_E"])
            ),
            column(
              width = 4,
              numericInput(ns("cr_0"), label = "cr_0 (Stage 0 Survival CR)", value = life_stages$Value[life_stages$Name == "cr_0"])
            ),
            column(
              width = 4,
              numericInput(ns("cr_1"), label = "cr_1 (Stage 1 Survival CR)", value = life_stages$Value[life_stages$Name == "cr_1"])
            )
          ),
          fluidRow(
            column(
              width = 4,
              numericInput(ns("cr_2"), label = "cr_2 (Stage 2 Survival CR)", value = life_stages$Value[life_stages$Name == "cr_2"])
            ),
            column(
              width = 4,
              numericInput(ns("cr_3"), label = "cr_3 (Stage 3 Survival CR)", value = life_stages$Value[life_stages$Name == "cr_3"])
            ),
            column(
              width = 4,
              numericInput(ns("cr_4"), label = "cr_4 (Stage 4 Survival CR)", value = life_stages$Value[life_stages$Name == "cr_4"])
            )
          ),
          fluidRow(
            column(
              width = 4,
              numericInput(ns("cr_5"), label = "cr_5 (Stage 5 Survival CR)", value = life_stages$Value[life_stages$Name == "cr_5"])
            ),
            column(
              width = 4,
              numericInput(ns("cr_6"), label = "cr_6 (Stage 6 Survival CR)", value = life_stages$Value[life_stages$Name == "cr_6"])
            ),
            column(
              width = 4,
              numericInput(ns("cr_7"), label = "cr_7 (Stage 7 Survival CR)", value = life_stages$Value[life_stages$Name == "cr_7"])
            )
          ),
          fluidRow(
            column(
              width = 4,
              numericInput(ns("cr_8"), label = "cr_8 (Stage 8 Survival CR)", value = life_stages$Value[life_stages$Name == "cr_8"])
            ),
            column(
              width = 4,
              numericInput(ns("cr_9"), label = "cr_9 (Stage 9 Survival CR)", value = life_stages$Value[life_stages$Name == "cr_9"])
            ),
            column(
              width = 4,
              numericInput(ns("cr_10"), label = "cr_10 (Stage 10 Survival CR)", value = life_stages$Value[life_stages$Name == "cr_10"])
            )
          ),

          tags$hr(style = "margin: 20px 0;"),

          tags$div(
            class = "lam_bb",
            actionButton(
              ns("compensation_ratios"),
              "Compensation Ratios (explainer)",
              icon = icon("info-circle")
            )
          )

        )

      ),
      # end Density Dependence tab
      
      tabPanel("Review Inputs", module_matrix_life_cycle_params_ui(
        ns("module_matrix_life_cycle_params")
      )),

      ## Run Model Tab (styled red)
      tabPanel(
        title = tags$span("Run Population Model", style = "color: #d9534f; font-weight: bold;"),
        tags$br(),

        tags$p(
          "Run population projections with your configured life cycle parameters and density-dependent constraints.
          Select a location to load stressor values, adjust parameters as needed, then run the projection.",
          class = "pm-ht"
        ),

        # Population Projection Preview content (moved from sidebar)
        module_matrix_model_preview_ui(ns("mm_preview"))
      ),

      ## Results Tab (moved to end, styled red)
      tabPanel(
        title = tags$span("Compare Scenario Results", style = "color: #d9534f; font-weight: bold;"),
        model_matrix_overview_ui(ns("model_matrix_overview"))
      )

    )  # end tabsetPanel
  )
}


module_matrix_model_inputs_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Add form validators (front-end only)
    btwn_01 <- function(value) {
      if (is.null(value)) {
        return("Cannot be blank")
      }
      if (is.na(value)) {
        return("Cannot be blank")
      }
      if (class(value) != "numeric" &
          class(value) != "integer") {
        return("Must be a number")
      }
      if (value > 1) {
        return("Value must be between 0 and 1")
      }
      if (value < 0) {
        return("Value must be between 0 and 1")
      }
    }
    
    # Add form validators (front-end only)
    btwn_1_and_10 <- function(value) {
      if (is.null(value)) {
        return("Cannot be blank")
      }
      if (is.na(value)) {
        return("Cannot be blank")
      }
      if (class(value) != "numeric" &
          class(value) != "integer") {
        return("Must be a number")
      }
      if (value > 10) {
        return("Value must be between 1 and 10")
      }
      if (value < 1) {
        return("Value must be between 1 and 10")
      }
    }
    
    simp_num <- function(value) {
      if (is.null(value)) {
        return("Cannot be blank")
      }
      if (is.na(value)) {
        return("Cannot be blank")
      }
      if (class(value) != "numeric" &
          class(value) != "integer") {
        return("Must be a number")
      }
    }
    
    gtr_0 <- function(value) {
      if (is.null(value)) {
        return("Cannot be blank")
      }
      if (is.na(value)) {
        return("Cannot be blank")
      }
      if (class(value) != "numeric" &
          class(value) != "integer") {
        return("Must be a number")
      }
      if (value <= 0) {
        return("Value must be between greater than 0")
      }
    }
    
    
    
    iv <- InputValidator$new()
    iv$add_rule("Nstage", btwn_1_and_10)
    iv$add_rule("k", simp_num)
    iv$add_rule("events", simp_num)
    # iv$add_rule("eps", simp_num)
    iv$add_rule("int", simp_num)
    iv$add_rule("SE", btwn_01)
    iv$add_rule("S0", btwn_01)
    iv$add_rule("SR", btwn_01)
    iv$add_rule("surv_1", btwn_01)
    #iv$add_rule("surv_2", btwn_01)
    #iv$add_rule("surv_3", btwn_01)
    #iv$add_rule("surv_4", btwn_01)
    iv$add_rule("year_1", gtr_0)
    #iv$add_rule("year_2", gtr_0)
    #iv$add_rule("year_3", gtr_0)
    #iv$add_rule("year_4", gtr_0)
    iv$add_rule("cr_E", simp_num)
    iv$add_rule("cr_0", simp_num)
    iv$add_rule("cr_1", simp_num)
    #iv$add_rule("cr_2", simp_num)
    #iv$add_rule("cr_3", simp_num)
    #iv$add_rule("cr_4", simp_num)
    iv$add_rule("mat_1", btwn_01)
    #iv$add_rule("mat_2", btwn_01)
    #iv$add_rule("mat_3", btwn_01)
    #iv$add_rule("mat_4", btwn_01)
    iv$add_rule("eps_sd", simp_num)
    iv$add_rule("egg_rho", simp_num)
    iv$add_rule("M.cv", simp_num)
    iv$add_rule("M.rho", simp_num)
    iv$enable()
    
    
    # Create reactive expressions for anadromous and Nstage so they can be passed to the module.
    anadromous_reactive <- reactive({
      input$anadromous
    })
    Nstage_reactive <- reactive({
      input$Nstage
    })
    
    # Call the DD configuration module server.
    module_matrix_dd_config_server("dd_config", anadromous = anadromous_reactive, Nstage = Nstage_reactive)
    
    module_matrix_life_cycle_params_server("module_matrix_life_cycle_params")
    
    
    # Call the DD cap module
    module_matrix_dd_cap_server("dd_cap", anadromous = anadromous_reactive, Nstage = Nstage_reactive)

    model_matrix_overview_server("model_matrix_overview")

    # Call the population projection preview module server
    print("Load module_matrix_model_preview_server...")
    module_matrix_model_preview_server("mm_preview")


    #--------------------------------------
    # DD Configuration Warnings (explain_dd_settings)
    #--------------------------------------

    # Reactive to run explain_dd_settings() when inputs change
    dd_explanation <- reactive({
      # Get life cycle data
      life_cycles <- session$userData$rv_life_stages$dat

      # Get habitat capacity data
      habitat_dd_k <- session$userData$rv_hab_densities$dat

      # Return NULL if no life cycle data
      if (is.null(life_cycles) || nrow(life_cycles) == 0) {
        return(NULL)
      }

      # Prepare habitat_dd_k - check if it has data
      if (!is.null(habitat_dd_k) && nrow(habitat_dd_k) > 0) {
        # Use first HUC_ID for explanation (representative)
        # Find the HUC_ID column (could be named differently) - case insensitive
        col_names <- names(habitat_dd_k)
        huc_col_idx <- grep("^huc_id$|^huc$|^id$|^location$",
                            tolower(col_names))[1]
        if (!is.na(huc_col_idx)) {
          huc_col <- col_names[huc_col_idx]  # Get actual column name with original case
          huc_id <- as.character(habitat_dd_k[[huc_col]][1])
        } else {
          huc_id <- NULL
          habitat_dd_k <- NULL
        }
      } else {
        habitat_dd_k <- NULL
        huc_id <- NULL
      }

      # Run explain_dd_settings (suppress console output)
      tryCatch({
        result <- CEMPRA::explain_dd_settings(
          life_cycles = life_cycles,
          habitat_dd_k = habitat_dd_k,
          HUC_ID = huc_id,
          verbose = FALSE
        )
        return(result)
      }, error = function(e) {
        # Return NULL on error to avoid crashing
        return(NULL)
      })
    })

    # Render DD configuration warnings
    output$dd_config_warnings <- renderUI({
      explanation <- dd_explanation()

      if (is.null(explanation)) {
        return(NULL)
      }

      # Collect all non-NULL notes and warnings
      alerts <- list()

      # Check s0_details$note
      if (!is.null(explanation$s0_details$note) &&
          nchar(explanation$s0_details$note) > 0) {
        alerts$s0 <- list(
          title = "S0 Optimization Note",
          message = explanation$s0_details$note,
          type = "info"
        )
      }

      # Check bh_dd_stages$note
      if (!is.null(explanation$bh_dd_stages$note) &&
          nchar(explanation$bh_dd_stages$note) > 0) {
        alerts$bh_dd <- list(
          title = "Density-Dependence Configuration Issue",
          message = explanation$bh_dd_stages$note,
          type = "warning"
        )
      }

      # Check k_values$note
      if (!is.null(explanation$k_values$note) &&
          nchar(explanation$k_values$note) > 0) {
        alerts$k_values <- list(
          title = "Habitat Capacity Configuration Issue",
          message = explanation$k_values$note,
          type = "warning"
        )
      }

      # Check warnings
      if (length(explanation$warnings) > 0 &&
          !all(sapply(explanation$warnings, function(x) is.null(x) || nchar(x) == 0))) {
        alerts$warnings <- list(
          title = "Configuration Warnings",
          message = paste(explanation$warnings, collapse = " "),
          type = "warning"
        )
      }

      # If no alerts, return NULL
      if (length(alerts) == 0) {
        return(NULL)
      }

      # Create alert boxes
      alert_ui <- lapply(alerts, function(alert) {
        # Determine color based on type
        bg_color <- if (alert$type == "warning") "#fff3cd" else "#cff4fc"
        border_color <- if (alert$type == "warning") "#ffc107" else "#0dcaf0"
        text_color <- if (alert$type == "warning") "#856404" else "#055160"
        icon_name <- if (alert$type == "warning") "exclamation-triangle" else "info-circle"

        tags$div(
          class = "alert",
          style = paste0(
            "background-color: ", bg_color, "; ",
            "border: 1px solid ", border_color, "; ",
            "border-left: 4px solid ", border_color, "; ",
            "border-radius: 4px; ",
            "padding: 12px 15px; ",
            "margin-bottom: 15px; ",
            "color: ", text_color, ";"
          ),
          tags$div(
            style = "display: flex; align-items: flex-start;",
            tags$span(
              shiny::icon(icon_name),
              style = "margin-right: 10px; margin-top: 2px;"
            ),
            tags$div(
              tags$strong(alert$title, style = "display: block; margin-bottom: 5px;"),
              tags$span(alert$message, style = "font-size: 13px;")
            )
          )
        )
      })

      # Return all alerts wrapped in a div
      tags$div(
        style = "margin-bottom: 15px;",
        alert_ui
      )
    })


    #--------------------------------------
    # Upload vital rates from csv
    #--------------------------------------
    observe({
      # Require the file
      print("Upload vital rate from csv...")
      
      req(input$up_vital2)
      
      upload_ok <- FALSE
      
      # Run import function in a try catch
      # to avoid app crashing on upload errors
      
      print("import life cycles csv...")
      
      tryCatch({
        in_file <- input$up_vital2
        
        if (is.null(in_file)) {
          return(NULL)
        }
        
        # Load in the default watersheds geojson layer
        life_stages <-
          read.csv(input$up_vital2$datapath)
        
        print("Uploading file from csv...")
        life_stages <- CEMPRA::pop_model_dat_clean(dat = life_stages, nstage_fill = 10)
        
        # Determine if anadromous
        anadromous <- life_stages$Value[life_stages$Name == "anadromous"]
        if (anadromous == 1) {
          anadromous <- TRUE
        } else {
          anadromous <- FALSE
        }
        
        
        # print("running JavaScript... updateAllInputs")
        # Update all numeric inputs through javascript
        # js$updateAllInputs(rjson::toJSON(life_stages))
        
        # Update via the UI
        isolate({
          # Update data object in isolate...
          # but trigger events through updateNumericInput()
          # (below)
          session$userData$rv_life_stages$dat <-
            life_stages
        })
        
        
        session$userData$rv_eigen_analysis$dat <-
          list()
        
        session$userData$rv_ea_errors$possible_error_state <-
          FALSE
        
        session$userData$rv_ea_errors$possible_error_msg <-
          ""
        
        # session$userData$rv_show_sample_plot$open <- FALSE
        
        session$userData$rv_pop_sample_plot_data$dat <-
          list()
        session$userData$rv_pop_sample_plot_data$run_counter <-
          1
        
        # Sand box stressor values
        #session$userData$rv_sandbox_stressors$dat <-
        # list()
        
        session$userData$rv_pop_data_huc_ts$dat <-
          list()
        session$userData$rv_pop_data_huc_ts$run_counter <-
          1
        session$userData$rv_pop_data_huc_ts$update_ts_plots <-
          FALSE
        
        session$userData$rv_show_pop_main_plot$open <-
          FALSE
        
        # Numer of stage uploaded in csv
        n_stage <-
          life_stages$Value[life_stages$Name == "Nstage"]
        n_stage <- as.numeric(n_stage)
        
        if (n_stage < 1 | n_stage > 10) {
          output$upload_error_msg_sheds <-
            renderText({
              "Number of stages (Nstage) must be between 1 and 10"
            })
        } else {
          output$upload_error_msg_sheds <-
            renderText({
              ""
            })
        }
        
        
        # Upload all numeric input values on page
        print("Updating numeric inputs...")
        
        updateNumericInput(session, "Nstage", value = life_stages$Value[life_stages$Name == "Nstage"])
        Sys.sleep(0.1) # Allow time for the Nstage to update...
        
        # Turn on/off anadromous mode
        anadromous <- life_stages$Value[life_stages$Name == "anadromous"]
        anadromous <- as.logical(anadromous)
        anadromous <- ifelse(is.na(anadromous), FALSE, anadromous)
        updateCheckboxInput(session, "anadromous", value = anadromous)
        Sys.sleep(0.1) # Allow time for the Nstage to update...
        
        updateNumericInput(session, "k", value = life_stages$Value[life_stages$Name == "k"])
        updateNumericInput(session, "events", value = life_stages$Value[life_stages$Name == "events"])
        # updateNumericInput(session, "eps", value = life_stages$Value[life_stages$Name == "eps"])
        updateNumericInput(session, "int", value = life_stages$Value[life_stages$Name == "int"])
        updateNumericInput(session, "SE", value = life_stages$Value[life_stages$Name == "SE"])
        updateNumericInput(session, "S0", value = life_stages$Value[life_stages$Name == "S0"])
        updateNumericInput(session, "SR", value = life_stages$Value[life_stages$Name == "SR"])
        updateNumericInput(session, "eps_sd", value = life_stages$Value[life_stages$Name == "eps_sd"])
        updateNumericInput(session, "egg_rho", value = life_stages$Value[life_stages$Name == "egg_rho"])
        updateNumericInput(session, "M.cv", value = life_stages$Value[life_stages$Name == "M.cv"])
        updateNumericInput(session, "M.rho", value = life_stages$Value[life_stages$Name == "M.rho"])
        updateNumericInput(session, "p.cat", value = life_stages$Value[life_stages$Name == "p.cat"])
        
        updateNumericInput(session, "cr_E", value = life_stages$Value[life_stages$Name == "cr_E"])
        updateNumericInput(session, "cr_0", value = life_stages$Value[life_stages$Name == "cr_0"])
        
        updateNumericInput(session, "surv_1", value = life_stages$Value[life_stages$Name == "surv_1"])
        updateNumericInput(session, "surv_2", value = life_stages$Value[life_stages$Name == "surv_2"])
        updateNumericInput(session, "surv_3", value = life_stages$Value[life_stages$Name == "surv_3"])
        updateNumericInput(session, "surv_4", value = life_stages$Value[life_stages$Name == "surv_4"])
        updateNumericInput(session, "surv_5", value = life_stages$Value[life_stages$Name == "surv_5"])
        updateNumericInput(session, "surv_6", value = life_stages$Value[life_stages$Name == "surv_6"])
        updateNumericInput(session, "surv_7", value = life_stages$Value[life_stages$Name == "surv_7"])
        updateNumericInput(session, "surv_8", value = life_stages$Value[life_stages$Name == "surv_8"])
        updateNumericInput(session, "surv_9", value = life_stages$Value[life_stages$Name == "surv_9"])
        updateNumericInput(session, "surv_10", value = life_stages$Value[life_stages$Name == "surv_10"])
        
        updateNumericInput(session, "year_1", value = life_stages$Value[life_stages$Name == "year_1"])
        updateNumericInput(session, "year_2", value = life_stages$Value[life_stages$Name == "year_2"])
        updateNumericInput(session, "year_3", value = life_stages$Value[life_stages$Name == "year_3"])
        updateNumericInput(session, "year_4", value = life_stages$Value[life_stages$Name == "year_4"])
        updateNumericInput(session, "year_5", value = life_stages$Value[life_stages$Name == "year_5"])
        updateNumericInput(session, "year_6", value = life_stages$Value[life_stages$Name == "year_6"])
        updateNumericInput(session, "year_7", value = life_stages$Value[life_stages$Name == "year_7"])
        updateNumericInput(session, "year_8", value = life_stages$Value[life_stages$Name == "year_8"])
        updateNumericInput(session, "year_9", value = life_stages$Value[life_stages$Name == "year_9"])
        updateNumericInput(session, "year_10", value = life_stages$Value[life_stages$Name == "year_10"])
        
        updateNumericInput(session, "cr_1", value = life_stages$Value[life_stages$Name == "cr_1"])
        updateNumericInput(session, "cr_2", value = life_stages$Value[life_stages$Name == "cr_2"])
        updateNumericInput(session, "cr_3", value = life_stages$Value[life_stages$Name == "cr_3"])
        updateNumericInput(session, "cr_4", value = life_stages$Value[life_stages$Name == "cr_4"])
        updateNumericInput(session, "cr_5", value = life_stages$Value[life_stages$Name == "cr_5"])
        updateNumericInput(session, "cr_6", value = life_stages$Value[life_stages$Name == "cr_6"])
        updateNumericInput(session, "cr_7", value = life_stages$Value[life_stages$Name == "cr_7"])
        updateNumericInput(session, "cr_8", value = life_stages$Value[life_stages$Name == "cr_8"])
        updateNumericInput(session, "cr_9", value = life_stages$Value[life_stages$Name == "cr_9"])
        updateNumericInput(session, "cr_10", value = life_stages$Value[life_stages$Name == "cr_10"])
        
        updateNumericInput(session, "mat_1", value = life_stages$Value[life_stages$Name == "mat_1"])
        updateNumericInput(session, "mat_2", value = life_stages$Value[life_stages$Name == "mat_2"])
        updateNumericInput(session, "mat_3", value = life_stages$Value[life_stages$Name == "mat_3"])
        updateNumericInput(session, "mat_4", value = life_stages$Value[life_stages$Name == "mat_4"])
        updateNumericInput(session, "mat_5", value = life_stages$Value[life_stages$Name == "mat_5"])
        updateNumericInput(session, "mat_6", value = life_stages$Value[life_stages$Name == "mat_6"])
        updateNumericInput(session, "mat_7", value = life_stages$Value[life_stages$Name == "mat_7"])
        updateNumericInput(session, "mat_8", value = life_stages$Value[life_stages$Name == "mat_8"])
        updateNumericInput(session, "mat_9", value = life_stages$Value[life_stages$Name == "mat_9"])
        updateNumericInput(session, "mat_10", value = life_stages$Value[life_stages$Name == "mat_10"])
        
        updateNumericInput(session, "smig_1", value = life_stages$Value[life_stages$Name == "smig_1"])
        updateNumericInput(session, "smig_2", value = life_stages$Value[life_stages$Name == "smig_2"])
        updateNumericInput(session, "smig_3", value = life_stages$Value[life_stages$Name == "smig_3"])
        updateNumericInput(session, "smig_4", value = life_stages$Value[life_stages$Name == "smig_4"])
        updateNumericInput(session, "smig_5", value = life_stages$Value[life_stages$Name == "smig_5"])
        updateNumericInput(session, "smig_6", value = life_stages$Value[life_stages$Name == "smig_6"])
        updateNumericInput(session, "smig_7", value = life_stages$Value[life_stages$Name == "smig_7"])
        updateNumericInput(session, "smig_8", value = life_stages$Value[life_stages$Name == "smig_8"])
        updateNumericInput(session, "smig_9", value = life_stages$Value[life_stages$Name == "smig_9"])
        updateNumericInput(session, "smig_10", value = life_stages$Value[life_stages$Name == "smig_10"])
        
        updateNumericInput(session, "u_1", value = life_stages$Value[life_stages$Name == "u_1"])
        updateNumericInput(session, "u_2", value = life_stages$Value[life_stages$Name == "u_2"])
        updateNumericInput(session, "u_3", value = life_stages$Value[life_stages$Name == "u_3"])
        updateNumericInput(session, "u_4", value = life_stages$Value[life_stages$Name == "u_4"])
        updateNumericInput(session, "u_5", value = life_stages$Value[life_stages$Name == "u_5"])
        updateNumericInput(session, "u_6", value = life_stages$Value[life_stages$Name == "u_6"])
        updateNumericInput(session, "u_7", value = life_stages$Value[life_stages$Name == "u_7"])
        updateNumericInput(session, "u_8", value = life_stages$Value[life_stages$Name == "u_8"])
        updateNumericInput(session, "u_9", value = life_stages$Value[life_stages$Name == "u_9"])
        updateNumericInput(session, "u_10", value = life_stages$Value[life_stages$Name == "u_10"])
        
        updateNumericInput(session, "eps_1", value = life_stages$Value[life_stages$Name == "eps_1"])
        updateNumericInput(session, "eps_2", value = life_stages$Value[life_stages$Name == "eps_2"])
        updateNumericInput(session, "eps_3", value = life_stages$Value[life_stages$Name == "eps_3"])
        updateNumericInput(session, "eps_4", value = life_stages$Value[life_stages$Name == "eps_4"])
        updateNumericInput(session, "eps_5", value = life_stages$Value[life_stages$Name == "eps_5"])
        updateNumericInput(session, "eps_6", value = life_stages$Value[life_stages$Name == "eps_6"])
        updateNumericInput(session, "eps_7", value = life_stages$Value[life_stages$Name == "eps_7"])
        updateNumericInput(session, "eps_8", value = life_stages$Value[life_stages$Name == "eps_8"])
        updateNumericInput(session, "eps_9", value = life_stages$Value[life_stages$Name == "eps_9"])
        updateNumericInput(session, "eps_10", value = life_stages$Value[life_stages$Name == "eps_10"])
        
      }, error = function(e) {
        print("Upload error...")
        
        output$upload_error_msg_vitals <-
          renderText({
            "Upload Error: Vital rate parameters did not import correctly. Please check file against the reference and reupload."
          })
      })
    }) # end vital rate import
    
    
    
    #--------------------------------------
    # Upload habitat capacities csv
    #--------------------------------------
    observe({
      # Require the file
      print("Upload habitat densities from csv...")
      req(input$up_habitat_capacities)
      # Run import function in a try catch
      # to avoid app crashing on upload errors
      print("import habitat densities from csv...")
      
      tryCatch({
        in_file <- input$up_habitat_capacities
        if (is.null(in_file)) {
          return(NULL)
        }
        # Load in the default watersheds geojson layer
        hab_densities <-
          read.csv(input$up_habitat_capacities$datapath)
        
        hab_densities <- CEMPRA::pop_model_hab_dens_clean(hab_dens = hab_densities)
        
        # Update via the UI
        isolate({
          # Habitat densities for population model
          session$userData$rv_hab_densities$dat <- hab_densities
        })
      }, error = function(e) {
        print("Upload error...")
        output$upload_error_msg_vitals <-
          renderText({
            "Upload Error: Habitat capacities did not import correctly. Please check file against the reference and reupload."
          })
      })
    }) # end habitat capacities import


    #--------------------------------------
    # Upload Stressor Response Workbook (from Pop Model page)
    #--------------------------------------
    observe({
      # Require the file
      req(input$up_sr_wb_pop)

      print("Upload Stressor Response Workbook from Population Model page...")

      # Run import function in a try catch
      # to avoid app crashing on upload errors
      tryCatch({
        in_file <- input$up_sr_wb_pop

        if (is.null(in_file)) {
          return(NULL)
        }

        # Extract the stressor response relationships
        sr_wb_dat <-
          CEMPRA::StressorResponseWorkbook(filename = input$up_sr_wb_pop$datapath)

        # Update the session$userData$rv_stressor_response reactive object
        start_time <- Sys.time()

        # Designate the stressor response object as a reactive value
        session$userData$rv_stressor_response$main_sheet <-
          sr_wb_dat$main_sheet
        session$userData$rv_stressor_response$stressor_names <-
          sr_wb_dat$stressor_names
        session$userData$rv_stressor_response$pretty_names <-
          sr_wb_dat$pretty_names
        session$userData$rv_stressor_response$sr_dat <-
          sr_wb_dat$sr_dat
        session$userData$rv_stressor_response$active_layer <-
          sr_wb_dat$stressor_names[1]
        session$userData$rv_stressor_response$active_values_raw <-
          NULL
        session$userData$rv_stressor_response$active_values_response <-
          NULL
        session$userData$rv_stressor_response$active_refresh <-
          start_time
        session$userData$rv_stressor_response$hover_values <-
          FALSE
        session$userData$rv_stressor_response$interaction_names <-
          names(sr_wb_dat$MInt)
        session$userData$rv_stressor_response$interaction_values <-
          sr_wb_dat$MInt

        # Trigger map redraw to clear selection
        session$userData$rv_redraw$redraw <- 0

        # Clear selected HUCs
        session$userData$rv_clickedIds$ids <- vector()
        session$userData$rv_clickedIds_csc$csc <- NA
        session$userData$rv_clickedIds_csc$var_csc <- NA

        # Clear Joe model results (since input data changed)
        session$userData$rv_joe_model_results$sims <- list()
        session$userData$rv_joe_model_sim_names$scenario_names <- list()
        session$userData$rv_joe_model_run_time$run_time_seconds <- list()

        # Clear out any previous population model runs since data changed
        session$userData$rv_pop_data_huc_ts$dat <- list()
        session$userData$rv_pop_data_huc_ts$run_counter <- 1
        session$userData$rv_pop_data_huc_ts$update_ts_plots <- FALSE
        session$userData$rv_show_pop_main_plot$open <- FALSE

        # Clear sample plot data
        session$userData$rv_pop_sample_plot_data$dat <- list()
        session$userData$rv_pop_sample_plot_data$run_counter <- 1

        # Clear sandbox stressors
        print("Triggering rv_sandbox_stressors$dat flush with sr data from pop model...")
        session$userData$rv_sandbox_stressors$dat <- list()

        # Clear error message on success
        output$upload_error_msg_sr_pop <- renderText({ "" })

      }, error = function(e) {
        print("Upload error (SR from Pop Model)...")
        output$upload_error_msg_sr_pop <- renderText({
          "Upload Error: Stressor Response Workbook (xlsx) did not import correctly. Check data format, worksheet names and column names."
        })
      })
    }) # end Stressor Response Workbook upload (Pop Model page)


    #--------------------------------------
    # Upload Stressor Magnitude Workbook (from Pop Model page)
    #--------------------------------------
    observe({
      # Require the file
      req(input$up_sm_wb_pop)

      print("Upload Stressor Magnitude Workbook from Population Model page...")

      # Run import function in a try catch
      # to avoid app crashing on upload errors
      tryCatch({
        in_file <- input$up_sm_wb_pop

        if (is.null(in_file)) {
          return(NULL)
        }

        # Extract the stressor magnitude data
        sm_wb_dat <-
          CEMPRA::StressorMagnitudeWorkbook(
            filename = input$up_sm_wb_pop$datapath,
            scenario_worksheet = 1
          )

        start_time <- Sys.time()

        # Reset active layer in stressor response
        session$userData$rv_stressor_response$active_layer <-
          session$userData$rv_stressor_response$stressor_names[1]
        session$userData$rv_stressor_response$active_values_raw <- NULL
        session$userData$rv_stressor_response$active_values_response <- NULL
        session$userData$rv_stressor_response$active_refresh <- start_time
        session$userData$rv_stressor_response$hover_values <- FALSE

        # Update the stressor magnitude data
        session$userData$rv_stressor_magnitude$sm_dat <- sm_wb_dat

        # Trigger map redraw to clear selection
        session$userData$rv_redraw$redraw <- 0

        # Clear selected HUCs
        session$userData$rv_clickedIds$ids <- vector()
        session$userData$rv_clickedIds_csc$csc <- NA
        session$userData$rv_clickedIds_csc$var_csc <- NA

        # Clear Joe model results (since input data changed)
        session$userData$rv_joe_model_results$sims <- list()
        session$userData$rv_joe_model_sim_names$scenario_names <- list()
        session$userData$rv_joe_model_run_time$run_time_seconds <- list()

        # Clear out any previous population model runs since data changed
        session$userData$rv_pop_data_huc_ts$dat <- list()
        session$userData$rv_pop_data_huc_ts$run_counter <- 1
        session$userData$rv_pop_data_huc_ts$update_ts_plots <- FALSE
        session$userData$rv_show_pop_main_plot$open <- FALSE

        # Clear sample plot data
        session$userData$rv_pop_sample_plot_data$dat <- list()
        session$userData$rv_pop_sample_plot_data$run_counter <- 1

        # Clear sandbox stressors
        print("Triggering rv_sandbox_stressors$dat flush with sm data from pop model...")
        session$userData$rv_sandbox_stressors$dat <- list()

        # Clear error message on success
        output$upload_error_msg_sm_pop <- renderText({ "" })

      }, error = function(e) {
        print("Upload error (SM from Pop Model)...")
        output$upload_error_msg_sm_pop <- renderText({
          "Upload Error: Stressor Magnitude Workbook (xlsx) did not import correctly. Check data format and column names."
        })
      })
    }) # end Stressor Magnitude Workbook upload (Pop Model page)



    # Add observe event to listen to Nstage input
    # and hide additional input boxes depending on the value
    # Make additional boxes disappear
    observeEvent(input$Nstage, {
      print("Listen to Nstage input to hide boxes....")
      
      req(input$Nstage)
      
      n_stage <- as.numeric(input$Nstage)
      
      # Call the function to handle hiding/showing elements
      hide_show_pop_boxes(n_stage)
      
    })
    
    
    
    
    # Listen for any changes to matrix model input parameters
    #  and on change update the reactive values object
    #  session$userData$rv_life_stages$dat
    observe({
      print("passive values update...")
      
      # Do not run if any input is null (update while typing...)
      req(input$Nstage)
      req(input$Nstage >= 1 & input$Nstage <= 10)
      req(input$k >= 0)
      req(input$events >= 0)
      # req(input$eps >= 0)
      req(input$int > 0)
      req(input$SE >= 0 & input$SE <= 1)
      req(input$S0 >= 0 & input$S0 <= 1)
      req(input$SR >= 0 & input$SR <= 1)
      req(input$surv_1 >= 0 & input$surv_1 <= 1)
      req(input$year_1 > 0)
      req(input$cr_E >= 0)
      req(input$cr_0 >= 0)
      req(input$mat_1 >= 0 & input$mat_1 <= 1)
      req(input$eps_sd >= 0)
      req(input$egg_rho >= 0)
      req(input$M.cv >= 0)
      req(input$M.rho >= 0)
      req(session$userData$rv_life_stages$dat)
      
      n_stage <- input$Nstage
      
      
      print("updating pop. model inputs...")
      
      session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "Nstage")] <-
        input$Nstage
      m_anadromous <- ifelse(input$anadromous, 1, 0) # Convert anadromous input to numeric
      session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "anadromous")] <-
        m_anadromous
      session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "k")] <-
        input$k
      session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "events")] <-
        input$events
      #session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "eps")] <-
      #   input$eps
      
      session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "int")] <-
        input$int
      session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "SE")] <-
        input$SE
      session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "S0")] <-
        input$S0
      session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "SR")] <-
        input$SR
      session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "surv_1")] <-
        input$surv_1
      session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "year_1")] <-
        input$year_1
      session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "cr_E")] <-
        input$cr_E
      session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "cr_0")] <-
        input$cr_0
      session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "cr_1")] <-
        input$cr_1
      session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "mat_1")] <-
        input$mat_1
      session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "eps_sd")] <-
        input$eps_sd
      session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "egg_rho")] <-
        input$egg_rho
      session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "M.cv")] <-
        input$M.cv
      session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "M.rho")] <-
        input$M.rho
      session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "p.cat")] <-
        input$p.cat
      
      print(paste0("New nstage is: ", n_stage))
      
      if (n_stage >= 2) {
        print(".... Updating at stage 2")
        
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "mat_2")] <-
          input$mat_2
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "cr_2")] <-
          input$cr_2
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "year_2")] <-
          input$year_2
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "surv_2")] <-
          input$surv_2
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "smig_2")] <-
          input$smig_2
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "u_2")] <-
          input$u_2
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "eps_2")] <-
          input$eps_2
      }
      if (n_stage >= 3) {
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "mat_3")] <-
          input$mat_3
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "cr_3")] <-
          input$cr_3
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "year_3")] <-
          input$year_3
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "surv_3")] <-
          input$surv_3
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "smig_3")] <-
          input$smig_3
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "u_3")] <-
          input$u_3
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "eps_3")] <-
          input$eps_3
      }
      if (n_stage >= 4) {
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "mat_4")] <-
          input$mat_4
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "cr_4")] <-
          input$cr_4
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "year_4")] <-
          input$year_4
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "surv_4")] <-
          input$surv_4
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "smig_4")] <-
          input$smig_4
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "u_4")] <-
          input$u_4
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "eps_4")] <-
          input$eps_4
      }
      if (n_stage >= 5) {
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "mat_5")] <-
          input$mat_5
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "cr_5")] <-
          input$cr_5
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "year_5")] <-
          input$year_5
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "surv_5")] <-
          input$surv_5
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "smig_5")] <-
          input$smig_5
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "u_5")] <-
          input$u_5
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "eps_5")] <-
          input$eps_5
        
      }
      if (n_stage >= 6) {
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "mat_6")] <-
          input$mat_6
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "cr_6")] <-
          input$cr_6
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "year_6")] <-
          input$year_6
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "surv_6")] <-
          input$surv_6
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "smig_6")] <-
          input$smig_6
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "u_6")] <-
          input$u_6
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "eps_6")] <-
          input$eps_6
      }
      if (n_stage >= 7) {
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "mat_7")] <-
          input$mat_7
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "cr_7")] <-
          input$cr_7
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "year_7")] <-
          input$year_7
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "surv_7")] <-
          input$surv_7
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "smig_7")] <-
          input$smig_7
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "u_7")] <-
          input$u_7
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "eps_7")] <-
          input$eps_7
      }
      if (n_stage >= 8) {
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "mat_8")] <-
          input$mat_8
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "cr_8")] <-
          input$cr_8
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "year_8")] <-
          input$year_8
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "surv_8")] <-
          input$surv_8
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "smig_8")] <-
          input$smig_8
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "u_8")] <-
          input$u_8
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "eps_8")] <-
          input$eps_8
      }
      if (n_stage >= 9) {
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "mat_9")] <-
          input$mat_9
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "cr_9")] <-
          input$cr_9
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "year_9")] <-
          input$year_9
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "surv_9")] <-
          input$surv_9
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "smig_9")] <-
          input$smig_9
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "u_9")] <-
          input$u_9
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "eps_9")] <-
          input$eps_9
      }
      
      if (n_stage >= 10) {
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "mat_10")] <-
          input$mat_10
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "cr_10")] <-
          input$cr_10
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "year_10")] <-
          input$year_10
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "surv_10")] <-
          input$surv_10
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "smig_10")] <-
          input$smig_10
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "u_10")] <-
          input$u_10
        session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "eps_10")] <-
          input$eps_10
      }
      
      
      print("End of passive inputs...")

    })


    #-------------------------------------------------------
    # Compensation Ratios - Render Functions and Modal
    #-------------------------------------------------------

    # Copy adult k to panel
    output$print_adult_k <- renderText({
      print("print_adult_k...")
      dat <- session$userData$rv_life_stages$dat
      val <- dat$Value[which(dat$Name == "k")]
      return(val)
    })

    # Beverton-Holt Sample Plot
    output$bh_plot <- renderPlot({
      bh <- function(K = NA, surv = NA, Nt = NA) {
        Nt2 <- (surv * Nt) / (1 + Nt * (surv/K))
        return(Nt2)
      }

      surv <- 0.8
      K <- 100
      Nt <- seq(0, 1500, by = 5)
      res <- lapply(Nt, bh, K = K, surv = surv)
      Nt2 <- unlist(res)
      plot(
        Nt, Nt2, type = 'l',
        xlim = c(0, 1000), ylim = c(0, 100),
        xlab = expression('N'[' i, t']),
        ylab = expression('N'[' i, t+1']),
        sub = "BH function with K = 100 and productivity = 0.8"
      )
      abline(0, surv, lty = 3, col = "red", lwd = 1.5)
      abline(h = K, lty = 2, col = "blue", lwd = 1.5)
    })

    # Compensation Ratio Sample Plot
    output$cr_samp_plot <- renderPlot({
      crf <- function(CR = NA, Nt = NA, K = NA, surv = NA) {
        val <- (CR * surv) / (1 + (CR - 1) * (Nt / K))
        val <- ifelse(val > 1, 1, val)
      }

      surv <- 0.8
      K <- 100
      Nt <- seq(0, 500, by = 5)

      CR <- 1
      CR_adj_1 <- unlist(lapply(Nt, crf, CR = CR, K = K, surv = surv))
      CR <- 1.1
      CR_adj_1.1 <- unlist(lapply(Nt, crf, CR = CR, K = K, surv = surv))
      CR <- 2
      CR_adj_2 <- unlist(lapply(Nt, crf, CR = CR, K = K, surv = surv))
      CR <- 5
      CR_adj_5 <- unlist(lapply(Nt, crf, CR = CR, K = K, surv = surv))
      CR <- 0.97
      CR_adj_0.9 <- unlist(lapply(Nt, crf, CR = CR, K = K, surv = surv))

      plot(
        Nt, CR_adj_1, type = 'l',
        xlim = c(5, 600), ylim = c(0, 1),
        xlab = expression('N'[' i, t']),
        ylab = expression('Modified survival'[' i, t']),
        sub = "CR function with K = 100 and productivity = 0.8"
      )
      points(Nt, CR_adj_1.1, type = 'l')
      points(Nt, CR_adj_2, type = 'l')
      points(Nt, CR_adj_5, type = 'l')
      points(Nt, CR_adj_0.9, type = 'l', col = 'grey')

      text(550, tail(CR_adj_1.1)[1], labels = c("CR: 1.1"), cex = 0.8)
      text(550, tail(CR_adj_2)[1], labels = c("CR: 2"), cex = 0.8)
      text(550, tail(CR_adj_5)[1], labels = c("CR: 5"), cex = 0.8)
      text(550, tail(CR_adj_1)[1], labels = c("CR: 1.0"), cex = 0.8)
      text(550, tail(CR_adj_0.9)[1], labels = c("CR: 0.97"), cex = 0.8)

      abline(h = 0.8, lty = 3, col = "red", lwd = 1.5)
      abline(v = K, lty = 2, col = "blue", lwd = 1.5)
      abline(h = K * surv, lty = 4, col = "green", lwd = 1.5)
    })

    # Stable stage K data table
    output$dt_stablestage_k <- DT::renderDataTable({
      print("Building dt_stablestage_k...")

      ss <- session$userData$rv_eigen_analysis$dat$ea$stable.stage
      nstage <- session$userData$rv_eigen_analysis$dat$pop_mod_mat$life_histories$Nstage
      Ka <- session$userData$rv_eigen_analysis$dat$pop_mod_mat$life_histories$Ka

      ss <- as.numeric(ss)
      k_stage <- ss * Ka / ss[nstage]
      ss <- round(ss, 3)
      ss <- data.frame(t(ss))
      k_stage <- round(k_stage, 0)

      repro_ss <- rbind(ss, k_stage)
      colnames(repro_ss) <- paste0("Stage ", 1:ncol(ss))
      rownames(repro_ss) <- c("Stable Stage", "Stage Capacities K")

      DT::datatable(
        repro_ss,
        editable = FALSE,
        caption = "Stable Stage Distributions (0 - 1) & Stage Capacities (K)",
        filter = "none",
        selection = "single",
        rownames = TRUE,
        class = "cell-border stripe",
        options = list(
          pageLength = 500,
          info = FALSE,
          dom = 't',
          ordering = FALSE,
          columnDefs = list(list(className = 'dt-left', targets = "_all"))
        )
      )
    })

    # Transition matrix data table for CR modal
    output$dt_transition_matrix_cr <- DT::renderDataTable({
      print("Building dt_transition_matrix_cr...")

      A <- round(session$userData$rv_eigen_analysis$dat$pop_mod_mat$projection_matrix, 3)
      mnames <- paste("Stage", 1:ncol(A))

      anadrmous <- session$userData$rv_eigen_analysis$dat$pop_mod_mat$anadrmous
      if (anadrmous) {
        mnames <- session$userData$rv_eigen_analysis$dat$pop_mod_mat$life_histories$stage_names
        mnames <- gsub("_", " ", mnames)
        mnames <- gsub("stage", "Stage", mnames)
      }

      colnames(A) <- mnames
      rownames(A) <- mnames

      DT::datatable(
        A,
        editable = FALSE,
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
          columnDefs = list(list(className = 'dt-left', targets = "_all"))
        )
      )
    })

    # Compensation Ratios Modal
    observeEvent(input$compensation_ratios, {
      print("compensation_ratios modal ...")

      showModal(
        modalDialog(
          title = "Compensation Ratios",
          tagList(
            tags$p(
              "It is rare for natural populations to grow in perpetuity without any constraints on grow, survival, and reproduction. Therefore, population models will typically include mechanism(s) to constrain population growth or sustained densities. Compensation ratios (CR values) are used in this tool to parameterize and govern density-dependent growth conditions. Compensation ratios (described below) are a re-parameterization of the classical Beverton-Holt function for density-dependent growth."
            ),
            tags$p(
              "The Beverton-Holt function gives the expected number of individuals, in the subsequent time step (N at time + 1, or density) as a function of the number of individuals in the current time step (N at time). In the case of matrix population models, this relationship is expressed as the number of individuals transitioning between two stages (e.g., from stage 2 to stage 3) where density dependent constraints are present. In the Beverton-Holt function, governing parameters include an estimate of carrying capacity (K), a baseline estimate of survival (S) (productivity) for the transition probability (in the absence of any density-dependent effects) and the number of individuals in the current stage class (Nt)."
            ),
            withMathJax(),
            helpText(
              'Beverton-Holt function for density-dependent growth:
              $$N_{t+1}=\\frac{S \\cdot N_t}{1 + (\\frac{S}{K}) \\cdot N_t}$$'
            ),

            fluidRow(
              column(1),
              column(
                10,
                align = "center",
                plotOutput(ns("bh_plot")),
                tags$p(
                  "Overview of the Beverton-Holt function showing the number of individuals at time (t) on the x-axis and the number of individuals at time + 1 one on the y-axis. The curved black line shows the effects of density dependant growth. The steep red line is the productivity of 0.8 under density-independent growth conditions. The blue line is the hypothetical caring capacity of 100, and the green line is the max achievable capacity calculated K (100) multiply by 0.8 = 80."
                )
              ),
              column(1),
            ),
            tags$p(
              "Compensation Ratios (CR) are used in the population modelling component of this tool as a modified version of the Beverton-Holt equation to adjust the survivorship of each life stage based on the observed densities (abundance, N i,t) and stage-specific carrying capacities (Ki):"
            ),

            withMathJax(),
            helpText(
              'Compensation Ratio CR for life stage i:
              $$S_{i,t}=\\frac{S_{i,0} \\cdot w_i}{1 + \\frac{w_i - 1 \\cdot N_{i,t}}{K_i}}$$'
            ),


            tags$p(
              "In the CR equation Si,0 is the baseline survivorship under density-independent growth conditions; wi is the compensation ratio (CR value) of life stage i; Ni,t is the current number of individuals in life stage i in a given time step (t); and Ki is the carrying capacity of life stage i. The compensation ratios, in essence, modify the survivorship of each life stage based on how far the stage-specific abundance (Ni,t) has departed from its assumed carrying capacity (Ki)."
            ),

            tags$p(
              "A plot of compensation ratios is provided below to illustrate their effects of stage-specific survivorship transitions. In this example abundance values of a hypothetical stage class (i) are plotted along the x-axis with a carrying capacity (Ki) set to 100 individuals (blue vertical line). The hypothetical stage class (i) has a baseline survivorship (productivity) value of 0.8 in the absence of density-dependent growth conditions (horizontal red line). The y-axis on the plot shows how the default survivorship value of 0.8 is modified based on the stage-specific compensation ratio for stage class (CR i). The survivorship value for the stage class is suppressed as the abundance values exceed the carrying capacity K. The effects are amplified as compensation ratios are increased. Compensation ratios of 1.0 leave the vital rate unmodified. Compensation ratios less than 1.0 increase survivorship values (allowing for a potential positive effect of density). When the abundance of the age class is less than the carrying capacity baseline survivorship values can actually be increase. However, within the model code adjusted survivorship values are fixed so that they never exceed 1.0 for any stage transition."
            ),

            fluidRow(
              column(1),
              column(10, align = "center", plotOutput(ns("cr_samp_plot"))),
              column(1)
            ),

            tags$b("Carrying Capacity Estimates:"),
            tags$p(
              "It is also useful to understand how stage-specific carrying capacity values are estimated in the tool. Current, it is only possible to modify the carrying capacity for the adult age class (stage 4) via the K parameter in the inputs. The K values for other age classes are calculated from the stable stage distribution of the underlying transition matrix (B):"
            ),
            tags$ul(
              tags$li(
                "K (Stage-0, eggs): Calculated as the project of individuals in the mature age classes, multiplied by the maturation probability for each class, the number of spawning events, eggs per female, sex ration and spawning interval."
              ),
              tags$li(
                "K (Stage-0, fry): K values for young-of-the-year (fry/Age-0) individuals is calculated as the produce of K eggs (above) * the egg survival (SE)."
              ),
              tags$li(
                "K (Stage-1): Calculated from the stable-stage distribution of the transition matrix (B) after setting the adult stage (Stage-4) to K (e.g., 100)."
              ),
              tags$li(
                "K (Stage-2): Calculated from the stable-stage distribution of the transition matrix (B) after setting the adult stage (Stage-4) to K (e.g., 100)."
              ),
              tags$li(
                "K (Stage-3): Calculated from the stable-stage distribution of the transition matrix (B) after setting the adult stage (Stage-4) to K (e.g., 100)."
              ),
              tags$li(
                "K (Stage-4): Manually input by user for the population of interest."
              )
            ),
            tags$b("Stage-stage distribution (portions) under current model settings:"),

            DT::dataTableOutput(ns("dt_stablestage_k")),
            br(),
            p(
              paste0(
                "Current K values per stage class. Calculated from stable-stage distributions after fixing the adult (stage-4) K to: ",
                textOutput(ns("print_adult_k"))
              )
            ),
            br(),

            tags$b("Density-Dependence Matrix (D):"),
            tags$p(
              "Based on the derived stage-specific carry capacities (K values), baseline survivorship values (SE, S0, surv_1, ...) and the corresponding compensation ratios (cr_E, cr_0, cr_1, ...) a density-dependence matrix (D) for a hypothetical population vector of egg: 10,000,000, fry: 1,000,000; stage 1: 100,000, stage 2: 10,000, stage 3: 1,000 & stage 4: 100 we appear as follows:"
            ),

            tags$p(
              "The density-dependence matrix (D) represents modifiers for the survivorship estimate for each stage transition. The density dependence matrix (D) is then multiplied with the corresponding transition matrix (B) of density-independent transition probabilities:"
            ),

            DT::dataTableOutput(ns("dt_transition_matrix_cr")),


            tags$p(
              "Then, to get the finalized projection matrix (A) the two previous matrices are multipled together [A is a product of B*D = A]. The projection matrix (A) is recalculated for each time step. Note that vital rate modifiers from environmental parameters linked to stressor-response functions will result in changes to either the survivorship, capacity or fecundity values (omitted here)."
            ),


            tags$p(
              "Compensation ratios are (generally) widely used as parameters in stock-recruitment functions although they are admittedly less popular in classical matrix population modeling. Steepness (the proportion of recruitment produced when stock size is reduced to 20% of initial biomass) is sometimes used in place of compensation ratios. Numerous other methods exist to introduce density dependence into stage-structured population models. The compensation ratios were used in this tool to represent a versatile mechanism for applications to a large number of hypothetical species profiles. For additional background please review the following references to learn more about compensation ratios and population modelling with density-dependent growth. "
            ),
            tags$b("Useful References:"),
            tags$p(
              "Goodyear, C. P. (1980). Compensation in fish populations. Biological monitoring of fish, 253-280."
            ),
            tags$p(
              "Myers, R. A. (2001). Stock and recruitment: generalizations about maximum reproductive rate, density dependence, and variability using meta-analytic approaches. ICES Journal of Marine Science, 58(5), 937-951."
            ),
            tags$p(
              "Rose, K. A., Cowan Jr, J. H., Winemiller, K. O., Myers, R. A., & Hilborn, R. (2001). Compensatory density dependence in fish populations: importance, controversy, understanding and prognosis. Fish and Fisheries, 2(4), 293-327."
            ),
            tags$p(
              "Myers, R. A., Bowen, K. G., & Barrowman, N. J. (1999). Maximum reproductive rate of fish at low population sizes. Canadian Journal of Fisheries and Aquatic Sciences, 56(12), 2404-2419."
            ),
            tags$p(
              "Walters, C. J., & Martell, S. J. (2004). Fisheries ecology and management. Princeton University Press."
            ),
            tags$p(
              "Forrest, R. E., McAllister, M. K., Dorn, M. W., Martell, S. J., & Stanley, R. D. (2010). Hierarchical Bayesian estimation of recruitment parameters and reference points for Pacific rockfishes (Sebastes spp.) under alternative assumptions about the stock-recruit function. Canadian Journal of Fisheries and Aquatic Sciences, 67(10), 1611-1634."
            )
          ),
          easyClose = TRUE,
          size = "l",
          footer = NULL
        )
      )
    })


  })
}