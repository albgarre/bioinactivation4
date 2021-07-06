
library(shiny)
library(bs4Dash)

library(tidyverse)
library(bioinactivation)

## UI --------------------------------------------------------------------------

fit2step_module_ui <- function(id) {

  tagList(
    fluidRow(
      column(12,
             h3("2 step fit predictions")
      )
    )
  )

}

## Server ----------------------------------------------------------------------

fit2step_module_server <- function(id) {

  moduleServer(id, function(input, output, session) {

  })

}

## test ------------------------------------------------------------------------

fit2step_module_test <- function(id) {

  shinyApp(
    ui = dashboardPage(
      title = "",
      header = dashboardHeader(),
      sidebar = dashboardSidebar(),
      controlbar = dashboardControlbar(),
      footer = dashboardFooter(),
      body = dashboardBody(
        fit2step_module_ui("test")
      )
    ),
    server = function(input, output) {
      fit2step_module_server("test")
    }
  )

}
