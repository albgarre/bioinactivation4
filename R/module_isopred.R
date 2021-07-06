
library(shiny)
library(bs4Dash)

library(tidyverse)
library(bioinactivation)

## UI --------------------------------------------------------------------------

isopred_module_ui <- function(id) {

  tagList(
    fluidRow(
      column(12,
             h3("Isothermal predictions")
             )
    )
  )

}

## Server ----------------------------------------------------------------------

isopred_module_server <- function(id) {

  moduleServer(id, function(input, output, session) {

  })

}

## test ------------------------------------------------------------------------

isopred_module_test <- function(id) {

  shinyApp(
    ui = dashboardPage(
      title = "",
      header = dashboardHeader(),
      sidebar = dashboardSidebar(),
      controlbar = dashboardControlbar(),
      footer = dashboardFooter(),
      body = dashboardBody(
        isopred_module_ui("test")
      )
    ),
    server = function(input, output) {
      isopred_module_server("test")
    }
  )

}

