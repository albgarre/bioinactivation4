
library(shiny)
library(bs4Dash)

library(tidyverse)
library(bioinactivation)

## UI --------------------------------------------------------------------------

fit1step_module_ui <- function(id) {

  tagList(
    fluidRow(
      column(12,
             h3("1 step fit")
      )
    )
  )

}

## Server ----------------------------------------------------------------------

fit1step_module_server <- function(id) {

  moduleServer(id, function(input, output, session) {

  })

}

## test ------------------------------------------------------------------------

fit1step_module_test <- function(id) {

  shinyApp(
    ui = dashboardPage(
      title = "",
      header = dashboardHeader(),
      sidebar = dashboardSidebar(),
      controlbar = dashboardControlbar(),
      footer = dashboardFooter(),
      body = dashboardBody(
        fit1step_module_ui("test")
      )
    ),
    server = function(input, output) {
      fit1step_module_server("test")
    }
  )

}
