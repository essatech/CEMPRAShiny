
# CEMPRAShiny <img src="www/img/JoeMCEMPRAll.png" align="right" style="max-width: 120px;"/>

<!-- badges: start -->
<!-- badges: end -->


## Developing a Cumulative Effects Modelling Framework for the Recovery of Aquatic Species at Risk

The Cumulative Effects Model for Prioritizing Recovery Actions (CEMPRA) is a cumulative effects modelling framework. The CEMPRA tool uses a series of standardized stressor-response functions to link environmental attributes to the system capacity and productivity of a target species/system. This framework design is as generalizable, simple, and versatile as possible so that users can apply the model to various geographic regions, contexts, systems, and species. As the name suggests, the CEMPRA tool helps prioritize recovery actions for data-limited species and species-at-risk, with the flexibility to  accommodate both data-rich and data-poor study systems. The CEMPRA tool is accessible as an open-source R package (https://github.com/essatech/CEMPRA) and R Shiny interactive web application (https://github.com/essatech/CEMPRAShiny).

## Project Components

-   GitHub Repository for R-Package (<https://github.com/essatech/CEMPRA>)
-   GitHub Repository for R-Shiny Application (<https://github.com/essatech/CEMPRAShiny>)
-   LIVE (online R-Shiny Application) (<https://essa.shinyapps.io/CEMPRAShiny/>)
-   R-Package Tutorials (<https://essatech.github.io/CEMPRA/index.html>)
-   Guidance Document: (<https://mattjbayly.github.io/CEMPRA_documentation/>)



## Package Contributors:
This is a broad collaboration between Fisheries and Oceans Canada (DFO), B.C. Ministry of Environment and Climate Change Strategy (ECCS), Alberta Environment and Parks (AEP), and Simon Fraser University (SFU). 

Contributors include:
-   [Matthew Bayly](https://github.com/mattjbayly): MJBA, Core application development.
-   Alexandra Tekatch: ESSA, Core application development.
-   [Jordan Rosenfeld](http://www.aferu.ca/rosenfeld-lab): Project design and coordination; ECCS Aquatic Ecologist
-   [Lauren Jarvis](https://github.com/andrewpaul68): Collaborator; AEP Research Scientist
-   [Andrew Paul](https://github.com/andrewpaul68): Collaborator; AEP Research Scientist
-   [Eva Enders](https://profils-profiles.science.gc.ca/en/profile/eva-enders): Project Lead; DFO Research Scientist
-   [Kyle Wilson](https://github.com/klwilson23): Population model development.
-   [Isuru Dharmasena](https://www.linkedin.com/in/isuru-dharmasena-90269895/?originalSubdomain=ca): Core Shiny app development
(https://github.com/julianheavyside) from [ESSA Technologies Ltd](https://essa.com/): R package and Shiny app development support.
-   Alejandra Urcelay
-   Pedro Gonzalez
-   Marc Porter
-   Julian Heavyside


## Features
-   Run custom implementations of the Joe Model on non-standard data formats.
-   Batch-run the integrated Joe Model/Population model across large datasets.
-   Run sensitivity tests.
-   Explore model extensions.


## Installation

The easiest way to install the `CEMPRA` package is from within the [RStudio IDE](https://www.rstudio.com/products/rstudio/download/) using `remotes::install_github()`. At this time the package has not been published to CRAN so the default `install.packages()` will not work. Instead use remotes (or devtools):

``` r
# Installing vis devtools
# You may need to install remotes
# install.packages("devtools")
library(devtools)
devtools::install_github("essatech/CEMPRA")

# You may need to install remotes
library(remotes)
remotes::install_github("essatech/CEMPRA")

```

See additional tutorials here:
- [Setup and installation](https://mattjbayly.github.io/CEMPRA_documentation/04_initial_setup.html)
- [Data inputs](https://mattjbayly.github.io/CEMPRA_documentation/05_data_inputs.html)

## Code of Conduct

Please note that the `CEMPRA` package is released with a [Contributor Code of Conduct](https://pkgs.rstudio.com/rmarkdown/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.