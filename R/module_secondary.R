
library(shiny)
library(bs4Dash)

library(tidyverse)

## UI --------------------------------------------------------------------------

secondary_module_ui <- function(id) {

  tagList(
    tableInput_module_ui(NS(id, "input_data"), box_title = "Input data"),
    fluidRow(
      column(6,
             bs4Card(
               title = "Model parameters",
               width = 12,
               footer = actionBttn(NS(id, "fit"), "Fit model"),
               pickerInput(NS(id, "model"), "Model",
                           choices = c("Bigelow", "Polynomial", "Peleg")
               ),
               uiOutput(NS(id, "parameters"))
             )
      ),
      column(6,
             bs4Card(
               title = "Model fit",
               width = 12,
               plotOutput(NS(id, "plot_fit"))
             )
      )
    )
  )

}

## Server ----------------------------------------------------------------------

secondary_module_server <- function(id) {

  moduleServer(id, function(input, output, session) {

    ## Input -------------------------------------------------------------------

    my_data <- tableInput_module_server("input_data",
                                        col_names = c("time", "logN"),
                                        xvar = "time", yvar = "logN",
                                        default_data = data.frame(
                                          time = c(0, 1, 2, 3, 4, 5, 6),
                                          logN = c(6, 5.2, 3.9, 3.4, 2.1, 1.6, 1.8)
                                        )
    )

  })

}

## test ------------------------------------------------------------------------

secondary_module_test <- function(id) {

  shinyApp(
    ui = dashboardPage(
      title = "",
      header = dashboardHeader(),
      sidebar = dashboardSidebar(),
      controlbar = dashboardControlbar(),
      footer = dashboardFooter(),
      body = dashboardBody(
        secondary_module_ui("test")
      )
    ),
    server = function(input, output) {
      secondary_module_server("test")
    }
  )

}
