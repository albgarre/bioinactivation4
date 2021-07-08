
library(shiny)
library(bs4Dash)

library(tidyverse)
library(bioinactivation)

## UI --------------------------------------------------------------------------

dynapred_module_ui <- function(id) {

  tagList(
    tableInput_module_ui(NS(id, "temp_profile"), box_title = "Temperature profile"),
    tableInput_module_ui(NS(id, "micro_data"), box_title = "Microbial data"),
    fluidRow(
      bs4Card(
        title = "Model",
        footer = actionBttn(NS(id, "go"), "Make prediction"),
        pickerInput(NS(id, "model"), "Model",
                    choices = get_model_data() %>% sort(),
                    selected = "Bigelow"),
        uiOutput(NS(id, "par_selector")),
        hr(),
        numericInput(NS(id, "max_time"), "Treatment time (min)", 30)
      ),
      bs4Card(
        title = "Prediction",
        plotOutput(NS(id, "plot_survivor"))
      )
    )
  )

}

## Server ----------------------------------------------------------------------

dynapred_module_server <- function(id) {

  moduleServer(id, function(input, output, session) {

    ## Input -------------------------------------------------------------------

    my_temperature <- tableInput_module_server("temp_profile",
                                               col_names = c("time", "temperature"),
                                               xvar = "time", yvar = "temperature",
                                               default_data = data.frame(time = c(0, 10, 20),
                                                                         temperature = c(50, 60, 55))
    )

    my_counts <- tableInput_module_server("micro_data",
                                               col_names = c("time", "logN"),
                                               xvar = "time", yvar = "logN",
                                               default_data = data.frame(time = c(0, 10, 20),
                                                                         logN = c(6, 5, 4))
    )

    ## Parameter selector ------------------------------------------------------

    par_map <- tribble(
      ~par, ~label, ~value,
      "z", "z-value (ºC)", 5,
      "D_R", "D-value at Tref (min)", 5,
      "temp_ref", "Tref (ºC)", 60,
      "delta_ref", "delta-value at Tref (min)", 5,
      "p", "p-value (·)", 1,
      "n", "n (·)", 1,
      "k_b", "k", 1e-2,
      "temp_crit", "Tcrit (ºC)", 60,
      "Delta", "Delta (log CFU)", 6,
      "k_ref", "k at Tref", 1e-2,
      "Ea", "Ea", 1e-3,
      "N0", "N0 (CFU/g)", 1e6,
      "N_min", "Tail height (CFU/g)", 100,
      "C_c0", "C_c0 (·)", 1e3,
    )

    make_input <- function(par_name) {

      par_data <- par_map %>%
        filter(par == par_name)

      fluidRow(
        column(6,
               numericInput(NS(id, par_name), par_data$label, par_data$value)
        )
      )

    }

    output$par_selector <- renderUI({
      par_names <- get_model_data(input$model)$parameters
      var_names <- paste0(get_model_data(input$model)$variables, "0")

      c(par_names, var_names) %>%
        map(.,
            ~ make_input(.)
            )


    })

    ## Prediction --------------------------------------------------------------

    my_prediction <- reactiveVal()

    observeEvent(input$model, {
      my_prediction(NULL)
    })

    observeEvent(input$go, {

      ## Extract the parameters

      my_model <- input$model

      par_names <- get_model_data(input$model)$parameters
      var_names <- paste0(get_model_data(input$model)$variables, "0")

      my_pars <- list()

      for (each_name in c(par_names, var_names)) {

        my_pars[[each_name]] <- input[[each_name]]

      }

      ## Make the prediction

      times <- seq(0, input$max_time, length=100)

      out <- predict_inactivation(my_model, times, my_pars, my_temperature())

      my_prediction(out)


    })

    ## Output ------------------------------------------------------------------

    output$plot_survivor <- renderPlot({

      validate(need(my_prediction(), "Make the prediction first"))

      plot(my_prediction(), plot_temp = TRUE)
    })

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
