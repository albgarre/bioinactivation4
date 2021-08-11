
library(shiny)
library(bs4Dash)

library(tidyverse)
library(bioinactivation)

## UI --------------------------------------------------------------------------

welcome_module_ui <- function(id) {

  tagList(
    fluidRow(
      column(12,
             bs4Jumbotron(
               status = "info",
               title = "bioinactivation",
               lead = "Making inactivation modeling easier since 2017",
               tags$p("Bioinactivation FE (full environment) has been developed as a colaboration between the departments
of Applied Mathematics and Food Microbiology of the Technical University of Cartagena and the Swedish National Food Safety
                       Agency."),
               tags$p("This application provides a user interface to the functions for modelling of microbial inactivation
                                   implemented in the bionactivation package of R (a.k.a. bioinactivation core)."),
               tags$p("A link to the latest version of this application can be found in the following
                                   webpage:"),
               tags$p("https://foodlab-upct.shinyapps.io/bioinactivationFE/"),
               tags$hr(),
               tags$p("For bug reports and support, please use one of the following e-mail accounts:"),
               tags$p("garre.alberto@gmail.com"),
               tags$p("pablo.fernandez@upct.es"),
               tags$hr(),
               tags$p("When using this application, please cite it as:"),
               tags$p("Alberto Garre, Pablo S. Fernandez, Roland Lindqvist,Jose A. Egea,
                                    Bioinactivation: Software for modelling dynamic microbial inactivation,
                                    Food Research International, Volume 93, March 2017, Pages 66-74, ISSN 0963-9969,
                                    http://dx.doi.org/10.1016/j.foodres.2017.01.012."),
               tags$p("and/or"),
               tags$p("Garre, A., Clemente-Carazo, M., Fernandez, P. S., Lindqvist, R., & Egea, J. A. (2018).
                       Bioinactivation FE: A free web application for modelling isothermal and dynamic microbial inactivation.
                       Food Research International, 112, 353–360. https://doi.org/10.1016/j.foodres.2018.06.057"),
               downloadLink("download_manual", "Download manual"),
               btnName = NULL
             )
      )
    )
  )

}

## Server ----------------------------------------------------------------------

welcome_module_server <- function(id) {

  moduleServer(id, function(input, output, session) {

  })

}

## test ------------------------------------------------------------------------

welcome_module_test <- function(id) {

  shinyApp(
    ui = dashboardPage(
      title = "",
      header = dashboardHeader(),
      sidebar = dashboardSidebar(),
      controlbar = dashboardControlbar(),
      footer = dashboardFooter(),
      body = dashboardBody(
        welcome_module_ui("test")
      )
    ),
    server = function(input, output) {
      welcome_module_server("test")
    }
  )

}
