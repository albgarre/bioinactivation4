
library(shiny)
library(bs4Dash)

library(tidyverse)
library(bioinactivation)

## UI --------------------------------------------------------------------------

stocpred_module_ui <- function(id) {

  tagList(
    fluidRow(
      column(12,
             h3("Stochastic predictions")
      )
    )
  )

}

## Server ----------------------------------------------------------------------

stocpred_module_server <- function(id) {

  moduleServer(id, function(input, output, session) {

  })

}

## test ------------------------------------------------------------------------

stocpred_module_test <- function(id) {

  shinyApp(
    ui = dashboardPage(
      title = "",
      header = dashboardHeader(),
      sidebar = dashboardSidebar(),
      controlbar = dashboardControlbar(),
      footer = dashboardFooter(),
      body = dashboardBody(
        stocpred_module_ui("test")
      )
    ),
    server = function(input, output) {
      stocpred_module_server("test")
    }
  )

}
