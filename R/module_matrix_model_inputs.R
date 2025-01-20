#' Matrix Model Inputs UI
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
module_matrix_model_inputs_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$h4("Load Data From Existing Life Cycles Profile"),
    
    
    fluidRow(
      column(
        width = 12,
        fileInput(
          ns("up_vital2"),
          label = "life cycles.csv",
          multiple = FALSE,
          accept = c(".csv")
        )
      )),
    
    
    fluidRow(
      
      column(
        width = 6,
        numericInput(ns("Nstage"), label = "Nstage (# Stages)", value = life_stages$Value[which(life_stages$Name == "Nstage")]),
      ),
      
      column(
        width = 6,
        checkboxInput(ns("anadromous"), label = "Follow an anadromous life history (Yes/checked = semelparous; No/unchecked = iteroparous)", value = life_stages$Value[life_stages$Name == "anadromous"]),
      )
      
      
    ),
    
    div(style = "color: #ffffff; background: #ff000059; border-radius: 5px; margin: 5px;",
        textOutput(ns(
          "upload_error_msg_vitals"
        ))),
    
    
    
    
    # class = "popmode_input",
    
    tags$h4("Survival Parameters"),
    
    tags$p(
      "Define the individual survivorship probabilities for each stage class evaluated on annual time steps in the simulation.",
      class = "pm-ht"
    ),
    
    fluidRow(
      column(
        width = 4,
        numericInput(ns("SE"), label = "SE (Egg Survival)", value = life_stages$Value[which(life_stages$Name == "SE")]),
      ),
      column(
        width = 4,
        numericInput(ns("S0"), label = "S0 (YOY Survival)", value = life_stages$Value[which(life_stages$Name == "S0")]),
      ),
      column(
        width = 4,
        numericInput(ns("surv_1"), label = "surv_1 (Stage 1 Survival)", value = life_stages$Value[which(life_stages$Name == "surv_1")]),
      ),
    ),
    
    fluidRow(
      column(
        width = 4,
        numericInput(ns("surv_2"), label = "surv_2 (Stage 2 Survival)", value = life_stages$Value[which(life_stages$Name == "surv_2")]),
        
      ),
      column(
        width = 4,
        numericInput(ns("surv_3"), label = "surv_3 (Stage 3 Survival)", value = life_stages$Value[which(life_stages$Name == "surv_3")]),
        
      ),
      column(
        width = 4,
        numericInput(ns("surv_4"), label = "surv_4 (Stage 4 Survival)", value = life_stages$Value[which(life_stages$Name == "surv_4")]),
        
      ),
    ),
    
    
    
    
    
    fluidRow(
      column(
        width = 4,
        numericInput(ns("surv_5"), label = "surv_5 (Stage 5 Survival)", value = life_stages$Value[which(life_stages$Name == "surv_5")]),
        
      ),
      column(
        width = 4,
        numericInput(ns("surv_6"), label = "surv_6 (Stage 6 Survival)", value = life_stages$Value[which(life_stages$Name == "surv_6")]),
        
      ),
      column(
        width = 4,
        numericInput(ns("surv_7"), label = "surv_7 (Stage 7 Survival)", value = life_stages$Value[which(life_stages$Name == "surv_7")]),
        
      ),
    ),
    
    fluidRow(
      column(
        width = 4,
        numericInput(ns("surv_8"), label = "surv_8 (Stage 8 Survival)", value = life_stages$Value[which(life_stages$Name == "surv_8")]),
        
      ),
      column(
        width = 4,
        numericInput(ns("surv_9"), label = "surv_9 (Stage 9 Survival)", value = life_stages$Value[which(life_stages$Name == "surv_9")]),
        
      ),
      column(
        width = 4,
        numericInput(ns("surv_10"), label = "surv_10 (Stage 10 Survival)", value = life_stages$Value[which(life_stages$Name == "surv_10")]),
        
      )
      
    ),
    
    
    tags$p(
      "For anadromous life histories (checked above) enter the spawner migration survival of each age class (smig_x)",
      class = "pm-ht"
    ),
    
    fluidRow(
      column(
        width = 3,
        numericInput(ns("smig_1"), label = "smig_1 (Mig. Surv. Age-1)", value = life_stages$Value[which(life_stages$Name == "smig_1")]),
        
      ),
      column(
        width = 3,
        numericInput(ns("smig_2"), label = "smig_2 (Mig. Surv. Age-2)", value = life_stages$Value[which(life_stages$Name == "smig_2")]),
        
      ),
      column(
        width = 3,
        numericInput(ns("smig_3"), label = "smig_3 (Mig. Surv. Age-3)", value = life_stages$Value[which(life_stages$Name == "smig_3")]),
        
      ),
      column(
        width = 3,
        numericInput(ns("smig_4"), label = "smig_4 (Mig. Surv. Age-4)", value = life_stages$Value[which(life_stages$Name == "smig_4")]),
        
      ),
      
    ),
    
    fluidRow(
      column(
        width = 3,
        numericInput(ns("smig_5"), label = "smig_5 (Mig. Surv. Age-5)", value = life_stages$Value[which(life_stages$Name == "smig_5")]),
        
      ),
      column(
        width = 3,
        numericInput(ns("smig_6"), label = "smig_6 (Mig. Surv. Age-6)", value = life_stages$Value[which(life_stages$Name == "smig_6")]),
        
      ),
      column(
        width = 3,
        numericInput(ns("smig_7"), label = "smig_7 (Mig. Surv. Age-7)", value = life_stages$Value[which(life_stages$Name == "smig_7")]),
        
      ),
      column(
        width = 3,
        numericInput(ns("smig_8"), label = "smig_8 (Mig. Surv. Age-8)", value = life_stages$Value[which(life_stages$Name == "smig_8")]),
        
      ),
      
    ),
    
    fluidRow(
      column(
        width = 3,
        numericInput(ns("smig_9"), label = "smig_9 (Mig. Surv. Age-9)", value = life_stages$Value[which(life_stages$Name == "smig_9")]),
        
      ),
      column(
        width = 3,
        numericInput(ns("smig_10"), label = "smig_10 (Mig. Surv. Age-10)", value = life_stages$Value[which(life_stages$Name == "smig_10")]),
        
      )
      
    ),
    
    tags$p(
      "For anadromous life histories (checked above) enter the pre-spawn survival of each age class (u_x)",
      class = "pm-ht"
    ),
    
    fluidRow(
      column(
        width = 3,
        numericInput(ns("u_1"), label = "u_1 (Pre. Spwn. Surv. Age-1)", value = life_stages$Value[which(life_stages$Name == "u_1")]),
        
      ),
      column(
        width = 3,
        numericInput(ns("u_2"), label = "u_2 (Pre. Spwn. Surv. Age-2)", value = life_stages$Value[which(life_stages$Name == "u_2")]),
        
      ),
      column(
        width = 3,
        numericInput(ns("u_3"), label = "u_3 (Pre. Spwn. Surv. Age-3)", value = life_stages$Value[which(life_stages$Name == "u_3")]),
        
      ),
      column(
        width = 3,
        numericInput(ns("u_4"), label = "u_4 (Pre. Spwn. Surv. Age-4)", value = life_stages$Value[which(life_stages$Name == "u_4")]),
        
      ),
      
    ),
    
    fluidRow(
      column(
        width = 3,
        numericInput(ns("u_5"), label = "u_5 (Pre. Spwn. Surv. Age-5)", value = life_stages$Value[which(life_stages$Name == "u_5")]),
        
      ),
      column(
        width = 3,
        numericInput(ns("u_6"), label = "u_6 (Pre. Spwn. Surv. Age-6)", value = life_stages$Value[which(life_stages$Name == "u_6")]),
        
      ),
      column(
        width = 3,
        numericInput(ns("u_7"), label = "u_7 (Pre. Spwn. Surv. Age-7)", value = life_stages$Value[which(life_stages$Name == "u_7")]),
        
      ),
      column(
        width = 3,
        numericInput(ns("u_8"), label = "u_8 (Pre. Spwn. Surv. Age-8)", value = life_stages$Value[which(life_stages$Name == "u_8")]),
        
      ),
      
    ),
    
    fluidRow(
      column(
        width = 3,
        numericInput(ns("u_9"), label = "u_9 (Pre. Spwn. Surv. Age-9)", value = life_stages$Value[which(life_stages$Name == "u_9")]),
        
      ),
      column(
        width = 3,
        numericInput(ns("u_10"), label = "u_10 (Pre. Spwn. Surv. Age-10)", value = life_stages$Value[which(life_stages$Name == "u_10")]),
        
      )
      
    ),
    
    
    

    
    
    tags$p(
      "Interannual stochasticity is introduced into the model. Mortality also correlates intra-annually among size classes and over time (correlation diminishes as distance between stages increases).",
      class = "pm-ht"
    ),
    
    fluidRow(
      column(
        width = 4,
        numericInput(ns("M.cv"), label = "M.cv (Coefficient of variation in stage-specific mortality)", value = life_stages$Value[which(life_stages$Name == "M.cv")]),
      ),
      column(
        width = 4,
        numericInput(ns("M.rho"), label = "M.rho (Correlation in mortality through time)", value = life_stages$Value[which(life_stages$Name == "M.rho")]),
      ),
      column(
        width = 4,
        numericInput(ns("p.cat"), label = "p.cat (Probability of catastrophic event per generation)", value = life_stages$Value[which(life_stages$Name == "p.cat")]),
      )
    ),
    
    tags$hr(),
    
    tags$h4("Growth Parameters"),
    
    tags$p(
      "Growth is represented as time (years) spent in each stage. This circumvents defining size attributes for each stage and allows for more flexibility to parameterize different species in the model.",
      class = "pm-ht"
    ),
    
    fluidRow(
      column(
        width = 3,
        numericInput(ns("year_1"), label = "year_1 (Years as Stage 1)", value = life_stages$Value[which(life_stages$Name == "year_1")]),
      ),
      column(
        width = 3,
        numericInput(ns("year_2"), label = "year_2 (Years as Stage 2)", value = life_stages$Value[which(life_stages$Name == "year_2")]),
      ),
      column(
        width = 3,
        numericInput(ns("year_3"), label = "year_3 (Years as Stage 3)", value = life_stages$Value[which(life_stages$Name == "year_3")]),
      ),
      column(
        width = 3,
        numericInput(ns("year_4"), label = "year_4 (Years as Stage 4)", value = life_stages$Value[which(life_stages$Name == "year_4")]),
      ),
    ),
    
    fluidRow(
      column(
        width = 3,
        numericInput(ns("year_5"), label = "year_5 (Years as Stage 5)", value = life_stages$Value[which(life_stages$Name == "year_5")]),
      ),
      column(
        width = 3,
        numericInput(ns("year_6"), label = "year_6 (Years as Stage 6)", value = life_stages$Value[which(life_stages$Name == "year_6")]),
      ),
      column(
        width = 3,
        numericInput(ns("year_7"), label = "year_7 (Years as Stage 7)", value = life_stages$Value[which(life_stages$Name == "year_7")]),
      ),
      column(
        width = 3,
        numericInput(ns("year_8"), label = "year_8 (Years as Stage 8)", value = life_stages$Value[which(life_stages$Name == "year_8")]),
      ),
    ),
    
    fluidRow(
      column(
        width = 3,
        numericInput(ns("year_9"), label = "year_9 (Years as Stage 9)", value = life_stages$Value[which(life_stages$Name == "year_9")]),
      ),
      column(
        width = 3,
        numericInput(ns("year_10"), label = "year_10 (Years as Stage 10)", value = life_stages$Value[which(life_stages$Name == "year_10")]),
      )
    ),
    
    
    
    tags$hr(),
    
    
    tags$h4("Reproduction Parameters"),
    
    tags$p(
      "The primary reproduction parameters include the spawning events per female (within a year), eggs per female spawner and variation in eggs per female (SD of eggs per female).",
      class = "pm-ht"
    ),
    
    fluidRow(
      column(
        width = 4,
        numericInput(ns("events"), label = "events (Spawn Events per Female)", value = life_stages$Value[which(life_stages$Name == "events")]),
      ),
      column(
        width = 4,
        numericInput(ns("eps_sd"), label = "eps_sd (SD variation in Eggs per Female)", value = life_stages$Value[which(life_stages$Name == "eps_sd")]),
      )
    ),
    
    tags$p(
      "Eggs per female spawner (eps) per mature individual for each age class. These can only vary by stage class for the anadromous version of the population model (semelparous). If you are using the non-anadromous life history strategy then enter and repeat the same value across all age classes.",
      class = "pm-ht"
    ),
    
    fluidRow(
      column(
        width = 3,
        numericInput(ns("eps_1"), label = "eps_1 (Stage 1)", value = life_stages$Value[which(life_stages$Name == "eps_1")]),
      ),
      column(
        width = 3,
        numericInput(ns("eps_2"), label = "eps_2 (Stage 2)", value = life_stages$Value[which(life_stages$Name == "eps_2")]),
      ),
      column(
        width = 3,
        numericInput(ns("eps_3"), label = "eps_3 (Stage 3)", value = life_stages$Value[which(life_stages$Name == "eps_3")]),
      ),
      column(
        width = 3,
        numericInput(ns("eps_4"), label = "eps_4 (Stage 4)", value = life_stages$Value[which(life_stages$Name == "eps_4")]),
      )
    ),
    
    
    fluidRow(
      column(
        width = 3,
        numericInput(ns("eps_5"), label = "eps_5 (Stage 5)", value = life_stages$Value[which(life_stages$Name == "eps_5")]),
      ),
      column(
        width = 3,
        numericInput(ns("eps_6"), label = "eps_6 (Stage 6)", value = life_stages$Value[which(life_stages$Name == "eps_6")]),
      ),
      column(
        width = 3,
        numericInput(ns("eps_7"), label = "eps_7 (Stage 7)", value = life_stages$Value[which(life_stages$Name == "eps_7")]),
      ),
      column(
        width = 3,
        numericInput(ns("eps_8"), label = "eps_8 (Stage 8)", value = life_stages$Value[which(life_stages$Name == "eps_8")]),
      )
    ),
    
    
    fluidRow(
      column(
        width = 3,
        numericInput(ns("eps_9"), label = "eps_9 (Stage 9)", value = life_stages$Value[which(life_stages$Name == "eps_9")]),
      ),
      column(
        width = 3,
        numericInput(ns("eps_10"), label = "eps_10 (Stage 10)", value = life_stages$Value[which(life_stages$Name == "eps_10")]),
      )
    ),
    
    
    
    tags$p(
      "The correlation in egg fecundity through time determines the similarity in residual egg production across age classes through years. (TODO add description for spawning interval).",
      class = "pm-ht"
    ),
    
    fluidRow(
      column(
        width = 6,
        numericInput(ns("egg_rho"), label = "egg_rho (Correlation in egg fecundity through time)", value = life_stages$Value[which(life_stages$Name == "egg_rho")]),
      ),
      column(
        width = 6,
        numericInput(ns("int"), label = "int (Spawning Interval - years)", value = life_stages$Value[which(life_stages$Name == "int")])
      )
    ),
    
    tags$p(
      "The probability (portion) that an individual is sexually mature in each stage class.",
      class = "pm-ht"
    ),
    
    fluidRow(
      column(
        width = 3,
        numericInput(ns("mat_1"), label = "mat_1 (Maturity as Stage 1)", value = life_stages$Value[which(life_stages$Name == "mat_1")]),
      ),
      column(
        width = 3,
        numericInput(ns("mat_2"), label = "mat_2 (Maturity as Stage 2)", value = life_stages$Value[which(life_stages$Name == "mat_2")]),
      ),
      column(
        width = 3,
        numericInput(ns("mat_3"), label = "mat_3 (Maturity as Stage 3)", value = life_stages$Value[which(life_stages$Name == "mat_3")]),
      ),
      column(
        width = 3,
        numericInput(ns("mat_4"), label = "mat_4 (Maturity as Stage 4)", value = life_stages$Value[which(life_stages$Name == "mat_4")]),
      )
    ),
    
    
    fluidRow(
      column(
        width = 3,
        numericInput(ns("mat_5"), label = "mat_5 (Maturity as Stage 5)", value = life_stages$Value[which(life_stages$Name == "mat_5")]),
      ),
      column(
        width = 3,
        numericInput(ns("mat_6"), label = "mat_6 (Maturity as Stage 6)", value = life_stages$Value[which(life_stages$Name == "mat_6")]),
      ),
      column(
        width = 3,
        numericInput(ns("mat_7"), label = "mat_7 (Maturity as Stage 7)", value = life_stages$Value[which(life_stages$Name == "mat_7")]),
      ),
      column(
        width = 3,
        numericInput(ns("mat_8"), label = "mat_8 (Maturity as Stage 8)", value = life_stages$Value[which(life_stages$Name == "mat_8")]),
      )
    ),
    
    
    fluidRow(
      column(
        width = 3,
        numericInput(ns("mat_9"), label = "mat_9 (Maturity as Stage 9)", value = life_stages$Value[which(life_stages$Name == "mat_9")]),
      ),
      column(
        width = 3,
        numericInput(ns("mat_10"), label = "mat_10 (Maturity as Stage 10)", value = life_stages$Value[which(life_stages$Name == "mat_10")]),
      )
    ),
    
    
    
    tags$hr(),
    
    
    tags$h4("Density Dependence"),
    
    tags$p(
      "The adult carrying capacity will determine the mean number of adults during the simulation. The abudnacne of adults in the population will generally hover around this value with stochasticity after the population stabilizes. The abundance estimates for other life stages are essentially back-calculated in each year of the simulation such that the adult carrying capacity is achieved with a global lambda value equal to 1.",
      class = "pm-ht"
    ),
    
    
    fluidRow(column(
      width = 12,
      numericInput(ns("k"), label = "(K) - Adult Carrying Capacity (for compensation ratios)", value = life_stages$Value[which(life_stages$Name == "k")]),
      
    )),
    
    
    tags$h4("Compensation Ratios"),
    
    tags$p(
      "Compensation ratios for density dependent growth. Density dependence is introduced into the model as compensatory density dependence with a fixed adult carrying capacity and the assumption of population stability over a longterm horizon.",
      class = "pm-ht"
    ),
    
    
    fluidRow(
      column(
        width = 4,
        numericInput(ns("cr_E"), label = "cr_E (Egg Survival CR)", value = life_stages$Value[which(life_stages$Name == "cr_E")]),
      ),
      column(
        width = 4,
        numericInput(ns("cr_0"), label = "cr_0 (Stage 0 Survival CR)", value = life_stages$Value[which(life_stages$Name == "cr_0")]),
      ),
      column(
        width = 4,
        numericInput(ns("cr_1"), label = "cr_1 (Stage 1 Survival CR)", value = life_stages$Value[which(life_stages$Name == "cr_1")]),
      ),
      
    ),
    
    
    
    fluidRow(
      column(
        width = 4,
        numericInput(ns("cr_2"), label = "cr_2 (Stage 2 Survival CR)", value = life_stages$Value[which(life_stages$Name == "cr_2")]),
      ),
      column(
        width = 4,
        numericInput(ns("cr_3"), label = "cr_3 (Stage 3 Survival CR)", value = life_stages$Value[which(life_stages$Name == "cr_3")]),
      ),
      column(
        width = 4,
        numericInput(ns("cr_4"), label = "cr_4 (Stage 4 Survival CR)", value = life_stages$Value[which(life_stages$Name == "cr_4")]),
      )
    ),
    
    
    fluidRow(
      column(
        width = 4,
        numericInput(ns("cr_5"), label = "cr_5 (Stage 5 Survival CR)", value = life_stages$Value[which(life_stages$Name == "cr_5")]),
      ),
      column(
        width = 4,
        numericInput(ns("cr_6"), label = "cr_6 (Stage 6 Survival CR)", value = life_stages$Value[which(life_stages$Name == "cr_6")]),
      ),
      column(
        width = 4,
        numericInput(ns("cr_7"), label = "cr_7 (Stage 7 Survival CR)", value = life_stages$Value[which(life_stages$Name == "cr_7")]),
      ),
    ),
    
    
    fluidRow(
      column(
        width = 4,
        numericInput(ns("cr_8"), label = "cr_8 (Stage 8 Survival CR)", value = life_stages$Value[which(life_stages$Name == "cr_8")]),
      ),
      column(
        width = 4,
        numericInput(ns("cr_9"), label = "cr_9 (Stage 9 Survival CR)", value = life_stages$Value[which(life_stages$Name == "cr_9")]),
      ),
      column(
        width = 4,
        numericInput(ns("cr_10"), label = "cr_10 (Stage 10 Survival CR)", value = life_stages$Value[which(life_stages$Name == "cr_10")]),
      )
    ),
    
    
    
    tags$hr(),
    
    
    tags$h4("Other Parameters"),
    
    fluidRow(column(
      width = 6,
      numericInput(ns("SR"), label = " SR - Sex ratio (portion female at birth)", value = life_stages$Value[which(life_stages$Name == "SR")]),
    ))
    
  )
}


#' Matrix Model INPUTS SERVE
#'
#' @param none
#'
#' @return None
#'
module_matrix_model_inputs_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
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
                     if(anadromous == 1) {
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
                     
                   },
                   error = function(e) {
                     print("Upload error...")
                     
                     output$upload_error_msg_vitals <-
                       renderText({
                         "Upload Error: Vital rate parameters did not import correctly. Please check file against the reference and reupload."
                       })
                   })
                 }) # end vital rate import
                 
                 
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
                     
                     if(n_stage >= 2) {
                       
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
                     if(n_stage >= 3) {
                       
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
                     if(n_stage >= 4) {
                       
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
                     if(n_stage >= 5) {
                       
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
                     if(n_stage >= 6) {
                       
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
                     if(n_stage >= 7) {
                       
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
                     if(n_stage >= 8) {
                       
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
                     if(n_stage >= 9) {
                       
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
                     
                     if(n_stage >= 10) {
                      
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
                 
                 
                 
               })
}