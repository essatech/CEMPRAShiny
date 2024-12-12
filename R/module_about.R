#' module_about_ui
#'
#' The UI portion of the about module
#'
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom shinydashboard box
#' @importFrom li shiny
#'
#' @param id The id for this module
#'
#' @return a tagList containing UI elements
#'
#'
#'
module_about_ui <- function(id) {
  ns <- NS(id)
  
  tagList(fluidRow(
    class = "cover-background",
    
    tags$div(
      class = "neat-text",
      
      style = "justify-content: center;
    text-align: center;",
      
      # tags$h2("Cumulative Effects Model for Prioritizing Recovery Actions", class = "main-title"),
      
      fluidRow(
        column(width = 1),
        column(
          width = 10,
          
          shinydashboard::box(
            style = "text-align: left;",
            width = NULL,
            
            tags$h1(
              "CEMPRA - Cumulative Effects Model for Prioritizing Recovery Actions R-Shiny Application",
              style = "justify-content: center; text-align: center;"
            ),
            tags$h3("Contributors"),
            tags$p(
              "Jordan Rosenfeld, UBC/BC-WLRS; Matthew Bayly, MJBA; Alexandra Tekatch, ESSA; Andrew Paul, AEP, Kyle Wilson, SFU; Eva Enders, INRS; Lauren Jarvis, DFO; Sierra Sullivan UB; Anas Usoff UBC; Laura MacPherson, AEP; Julian Heavyside, ESSA; Pedro Gonzalez, UBC; Isuru Dharmasena; Marc Porter, ESSA"
            ),
            tags$p(
              style = "color: grey;",
              "Project funded (in part) through the British Columbia Salmon Restoration and Innovation Fund (BCSRIF)."
            ),
            tags$h2("Project Components"),
            
            
            
            
            
            tags$table(class = "centered-table",
                       tags$tr(class = "centered-table-row",
                               tags$td(class = "square-box",
                                       "GitHub Repository for R-Package ",
                                       tags$a(href = "https://github.com/essatech/CEMPRA", "https://github.com/essatech/CEMPRA")
                               ),
                               tags$td(class = "square-box",
                                       "GitHub Repository for R-Shiny Application ",
                                       tags$a(href = "https://github.com/essatech/CEMPRAShiny", "https://github.com/essatech/CEMPRAShiny")
                               ),
                               tags$td(class = "square-box",
                                       "Guidance Document ",
                                       tags$a(href = "https://mattjbayly.github.io/CEMPRA_documentation/", "https://mattjbayly.github.io/CEMPRA_documentation/")
                               )
                       ),
                       tags$tr(class = "centered-table-row",
                               tags$td(class = "square-box",
                                       "R-Package Tutorials ",
                                       tags$a(href = "https://essatech.github.io/CEMPRA/index.html", "https://essatech.github.io/CEMPRA/index.html")
                               ),
                               tags$td(class = "square-box",
                                       "LIVE (online R-Shiny Application) ",
                                       tags$a(href = "https://essa.shinyapps.io/CEMPRAShiny/", "https://essa.shinyapps.io/CEMPRAShiny/")
                               )
                       )
            ),
            
            
          
            
            tags$hr(),
            
            tags$div(
              class = "neat-text",
              style = "justify-content: center; text-align: center;",
              shiny::HTML(
                '<iframe
          style = "border-style: solid;
      border-width: 5px;
      border-radius: 4px;
      border-color: #e0af00;"
          width="655" height="355" src="https://www.youtube.com/embed/Ln9EYi_NVPo" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'
              )
            ),
            
            
            tags$h2("Summary"),
            tags$p(
              "The Cumulative Effects Model for Prioritizing Recovery Actions (CEMPRA) is a cumulative effects modelling framework. The CEMPRA tool uses a series of standardized user-defined stressor-response functions to link environmental attributes to the system capacity and productivity of a target species/system. This framework design is as generalizable, simple, and versatile as possible so that users can apply the model to various geographic regions, contexts, systems, and species. As the name suggests, the CEMPRA tool helps prioritize recovery actions for data-limited species and species-at-risk, with the flexibility to accommodate both data-rich and data-poor study systems and to facilitate an efficient transition from simple to complex modelling within a single platform as more information becomes available for a target species. The CEMPRA is intended as a low-barrier tool and is accessible as an open-source R package (https://github.com/essatech/CEMPRA) and R Shiny interactive web application (https://github.com/essatech/CEMPRAShiny)."
            ),
            tags$p(
              "Stressor-response functions form the foundation of the CEMPRA tool. A stressor variable is broadly characterized as an environmental driver resulting in an observable biological response in a target population (Pirotta et al. 2022; Rosenfeld et al. 2022; Jarvis et al. 2023). Within the CEMPRA tool, stressors represent and capture various metrics of cumulative effects (direct or proximal) and their associated impact pathways (e.g., stream temperature, sedimentation, habitat loss)."
            ),
            tags$p(
              "Stressor-response functions are developed for each metric in a standardized format and linked to population-level productivity (mean system capacity, usually expressed as density or total abundance of the adult population in the basic Joe model) or specific vital rates within a life cycle modelling framework. Users then populate a matching table of stressor-magnitude values linked to various locations (spatial units) of interest. Finally, the CEMPRA tool runs to generate stochastic simulations of the study system under different user-defined management or recovery scenarios. Comparisons between scenarios are commonly made against a default reference (status quo) scenario. Scenarios generally consist of various 'alternative futures' to characterize potential impacts from development activities and/or alternative restoration/recovery efforts. Comparisons between scenarios can be quantitative (e.g., looking at a weighted mean system capacity or relative productivity) or qualitative by simply looking at a heatmap of stressors across the landscape."
            ),
            tags$p(
              "There are two modelling pathways and associated endpoints within the CEMPRA framework: 1) the first is the basic Joe Model that estimates system capacity (a generalized response metric, typically represented by percent of maximum adult population size since it is a single-stage model); and 2) a stage-structured life cycle model, where stressor-response relationships are directly linked to specific life stages and vital rates. The life cycle model allows users to adjust vital rate parameters to estimate cumulative effects at the population level and is intended for data-rich populations or species. The combination of the Joe model and life-cycle model embedded within the CEMPRA framework allows flexibility to handle data-poor and data-rich species within the same platform."
            ),
            tags$p(
              "Additional supporting resources are being developed to facilitate ease of use and collaboration between individuals studying cumulative effects. These resources include the development of an online stressor-response library (digital archive), example species profiles of population vital rates for running the life cycle model, case studies, and tutorial resources."
            ),
            
            tags$div(
              "Read more in the guidance document: ",
              tags$a(
                href = "https://mattjbayly.github.io/CEMPRA_documentation/",
                "https://mattjbayly.github.io/CEMPRA_documentation/"
              )
              
            ),
            
            
            
            
          ),
          # end of box
          
        ),
        # end of column
        column(width = 1),
      ),
      # end of fluid row
      
    ),
    # end of main page div
    
  )) # tag list and fluid row
}