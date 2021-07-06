
library(shiny)
library(bs4Dash)

library(tidyverse)
library(bioinactivation)

## UI --------------------------------------------------------------------------

dynapred_module_ui <- function(id) {

  tagList(
    fluidRow(
      column(12,
             h3("Dynamic predictions")
      )
    )
  )

}

## Server ----------------------------------------------------------------------

dynapred_module_server <- function(id) {

  moduleServer(id, function(input, output, session) {

  })

}

## test ------------------------------------------------------------------------

dynapred_module_test <- function(id) {

  shinyApp(
    ui = dashboardPage(
      title = "",
      header = dashboardHeader(),
      sidebar = dashboardSidebar(),
      controlbar = dashboardControlbar(),
      footer = dashboardFooter(),
      body = dashboardBody(
        dynapred_module_ui("test")
      )
    ),
    server = function(input, output) {
      dynapred_module_server("test")
    }
  )

}
