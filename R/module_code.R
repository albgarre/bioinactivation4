
library(shiny)
library(bs4Dash)

library(tidyverse)
library(bioinactivation)

## UI --------------------------------------------------------------------------

code_module_ui <- function(id) {

  tagList(
    fluidRow(
      column(6,
             userBox(
               title = userDescription(
                 title = "Methodology",
                 subtitle = "",
                 type = 2,
                 image = "https://images-na.ssl-images-amazon.com/images/I/71-A3eC0XsL.png"
               ),
               width = 12,
               status = "success",
               # gradient = TRUE,
               # background = "primary",
               p("The details of the statistical methods implemented in the package are described in several scientific articles."),
               p("Most of the methods are described in:"),
               p("Garre, A., Fernández, P.S., Lindqvist, R., Egea, J.A., 2017. Bioinactivation: Software for modelling dynamic microbial inactivation. Food Research International 93, 66–74. https://doi.org/10.1016/j.foodres.2017.01.012"),
               p("Furthermore, the first version of the online web application is described in:"),
               p("Garre, A., Clemente-Carazo, M., Fernández, P.S., Lindqvist, R., Egea, J.A., 2018. Response to the letter to Editor for “Bioinactivation FE: A free web application for modelling isothermal and dynamic microbial inactivation.” Food Research International. https://doi.org/10.1016/j.foodres.2018.08.083"),
               p("Additional, the documentation of the application provides additional details that may not be covered in the articles.")
             )
      ),
      column(6,
             userBox(
               title = userDescription(
                 title = "Bioinactivation package",
                 subtitle = "",
                 type = 2,
                 image = "https://docs.microsoft.com/nl-nl/azure/architecture/data-guide/images/logo_r.svg",
               ),
               width = 12,
               status = "success",
               # gradient = TRUE,
               # background = "primary",
               p("The operations are based on the bioinactivation package for R. It is available from CRAN in"),
               p("https://cran.r-project.org/package=bioinactivation")
             )
      )
    ),
    fluidRow(
      column(6,
             userBox(
               title = userDescription(
                 title = "Open code",
                 subtitle = "",
                 type = 2,
                 image = "https://www.influxdata.com/wp-content/uploads/GitHub-logo.jpg"
               ),
               width = 12,
               status = "warning",
               # gradient = TRUE,
               # background = "primary",
               p("The complete bioinactivation project is developed in Open Source."),
               p("The latest version of the code for the bioinactivation package can be found in: https://github.com/albgarre/bioinactivation"),
               p("The latest version of the code for the application can be found in https://github.com/albgarre/bioinactivation4")
             )
      ),
      column(6,
             userBox(
               title = userDescription(
                 title = "Citation",
                 subtitle = "",
                 type = 2,
                 image = "https://image.flaticon.com/icons/png/512/806/806177.png",
               ),
               width = 12,
               status = "warning",
               # gradient = TRUE,
               # background = "primary",
               p("We would really appreciate if you could include a citation to bioinactivation when using it in scientific studies."),
               p("Please cite it as "),
               p("Garre, A., Clemente-Carazo, M., Fernández, P.S., Lindqvist, R., Egea, J.A., 2018. Response to the letter to Editor for “Bioinactivation FE: A free web application for modelling isothermal and dynamic microbial inactivation.” Food Research International. https://doi.org/10.1016/j.foodres.2018.08.083")
             )
      )
    ),
    fluidRow(
      column(6,
             userBox(
               title = userDescription(
                 title = "Contact & bug reports",
                 subtitle = "",
                 type = 2,
                 image = "https://w7.pngwing.com/pngs/304/33/png-transparent-red-and-black-chat-box-dialog-box-computer-icons-message-box-material-miscellaneous-painted-text.png"
               ),
               width = 12,
               status = "danger",
               gradient = FALSE,
               # background = "warning",
               p("For bug reports, please open an issue in GitHub: https://github.com/albgarre/bioinactivation4/issues. This makes error tracking easier for everyone"),
               p("For any other queries, please contact Alberto Garre at alberto.garreperez(at)wur.nl")
             )
      )
    )
  )

}

## Server ----------------------------------------------------------------------

code_module_server <- function(id) {

  moduleServer(id, function(input, output, session) {

  })

}

## test ------------------------------------------------------------------------

code_module_test <- function(id) {

  shinyApp(
    ui = dashboardPage(
      title = "",
      header = dashboardHeader(),
      sidebar = dashboardSidebar(),
      controlbar = dashboardControlbar(),
      footer = dashboardFooter(),
      body = dashboardBody(
        code_module_ui("test")
      )
    ),
    server = function(input, output) {
      code_module_server("test")
    }
  )

}
