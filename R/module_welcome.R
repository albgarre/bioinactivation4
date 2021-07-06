
library(shiny)
library(bs4Dash)

library(tidyverse)
library(bioinactivation)

## UI --------------------------------------------------------------------------

welcome_module_ui <- function(id) {

  tagList(
    fluidRow(
      column(12,
             h3("Welcome")
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
