
library(shiny)
library(bs4Dash)

library(tidyverse)
library(bioinactivation)

## UI --------------------------------------------------------------------------

dynafit_module_ui <- function(id) {

  tagList(
    fluidRow(
      column(12,
             h3("Dynamic fitting")
      )
    )
  )

}

## Server ----------------------------------------------------------------------

dynafit_module_server <- function(id) {

  moduleServer(id, function(input, output, session) {

  })

}

## test ------------------------------------------------------------------------

dynafit_module_test <- function(id) {

  shinyApp(
    ui = dashboardPage(
      title = "",
      header = dashboardHeader(),
      sidebar = dashboardSidebar(),
      controlbar = dashboardControlbar(),
      footer = dashboardFooter(),
      body = dashboardBody(
        dynafit_module_ui("test")
      )
    ),
    server = function(input, output) {
      dynafit_module_server("test")
    }
  )

}
