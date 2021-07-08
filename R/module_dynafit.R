
library(shiny)
library(bs4Dash)

library(tidyverse)
library(bioinactivation)

## UI --------------------------------------------------------------------------

dynafit_module_ui <- function(id) {

  tagList(
    tableInput_module_ui(NS(id, "temp_profile"), box_title = "Temperature profile"),
    tableInput_module_ui(NS(id, "micro_data"), box_title = "Microbial data"),
    fluidRow(
      bs4Card(
        title = "Model",
        pickerInput(NS(id, "model"), "Model",
                    choices = get_model_data() %>% sort(),
                    selected = "Bigelow"),
        uiOutput(NS(id, "par_selector"))
      ),
      bs4Card(
        title = "Initial guess",
        plotOutput(NS(id, "plot_guess"))
      )
    ),
    fluidRow(
      bs4Card(
        title = "Fitting algorithm",
        footer = actionBttn(NS(id, "go"), "Fit the model"),
        fluidRow(
          column(6,
                 pickerInput(NS(id, "algorithm"), "Fitting algorithm",
                             choices = list(`Non-linear regression`="nlr",
                                            `Adaptive Monte Carlo` = "MCMC"))
                 )
        ),
        conditionalPanel("input.algorithm == 'MCMC'", ns = NS(id),
                         fluidRow(
                           column(6, numericInput(NS(id, "niter"), "Number of MC iterations",
                                                  1000, min = 1))
                         ),
                         fluidRow(
                           column(6, numericInput(NS(id, "burnin"), "Burnin length", 0, min = 0))
                         ),
                         fluidRow(
                           column(6, numericInput(NS(id, "updatecov"), "Updatecov", 0, min = 0))
                         ),
                         fluidRow(
                           column(6, actionBttn(NS(id, "reset_seed"), "Reset seed"))
                         )
                         )

      ),
      bs4Card(
        title = "Model fit",
        plotOutput(NS(id, "plot_fit"))
      )
    )
  )

}

## Server ----------------------------------------------------------------------

dynafit_module_server <- function(id) {

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
                                          default_data =  data.frame(
                                            time = seq(0, 20, length = 10),
                                            logN = c(8, 7.9, 7.5, 7.3, 5.5, 1.8, -1, -2, -3.6, -4.3)
                                          )
    )

    ## Parameter selector ------------------------------------------------------

    par_map <- tribble(
      ~par, ~label, ~value, ~fixed,
      "z", "z-value (ºC)", 5, FALSE,
      "D_R", "D-value at Tref (min)", 5, FALSE,
      "temp_ref", "Tref (ºC)", 60, TRUE,
      "delta_ref", "delta-value at Tref (min)", 5, FALSE,
      "p", "p-value (·)", 1, FALSE,
      "n", "n (·)", 1, FALSE,
      "k_b", "k", 1e-2, FALSE,
      "temp_crit", "Tcrit (ºC)", 60, FALSE,
      "Delta", "Delta (log CFU)", 6, TRUE,
      "k_ref", "k at Tref", 1e-2, FALSE,
      "Ea", "Ea", 1e-3, FALSE,
      "N0", "N0 (CFU/g)", 1e6, FALSE,
      "N_min", "Tail height (CFU/g)", 100, FALSE,
      "C_c0", "C_c0 (·)", 1e3, FALSE,
    )

    make_input <- function(par_name) {

      par_data <- par_map %>%
        filter(par == par_name)

      tagList(
        fluidRow(column(12, h4(par_name))),
        fluidRow(
          column(3,
                 numericInput(NS(id, par_name), "Initial guess", par_data$value)
                 ),
          column(3,
                 numericInput(NS(id, paste0(par_name, "_lower")), "Lower bound", par_data$value*.5)
                 ),
          column(3,
                 numericInput(NS(id, paste0(par_name, "_upper")), "Upper bound", par_data$value*2)
                 ),
          column(3,
                 awesomeCheckbox(NS(id, paste0(par_name, "_fixed")), "Fixed?", par_data$fixed)
                 )
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

    ## Initial guess -----------------------------------------------------------

    output$plot_guess <- renderPlot({

      validate(
        need(my_counts(), "Input counts first!"),
        need(my_temperature(), "Input temperature first!")
      )

      my_model <- input$model
      times <- seq(0, max(my_counts()$time), length=100)

      ## Get the model parameters

      par_names <- get_model_data(input$model)$parameters
      var_names <- paste0(get_model_data(input$model)$variables, "0")

      guess <- list()
      lower <- list()
      upper <- list()

      for (each_name in c(par_names, var_names)) {

        guess[[each_name]] <- input[[each_name]]

        if (isTRUE(input[[paste0(each_name, "_fixed")]])) {
          lower[[each_name]] <- input[[each_name]]
          upper[[each_name]] <- input[[each_name]]
        } else {
          lower[[each_name]] <- input[[paste0(each_name, "_lower")]]
          upper[[each_name]] <- input[[paste0(each_name, "_upper")]]
        }

      }

      ## Make the predictions

      pred_guess <- predict_inactivation(my_model, times, unlist(guess), my_temperature())
      pred_lower <- predict_inactivation(my_model, times, unlist(lower), my_temperature())
      pred_upper <- predict_inactivation(my_model, times, unlist(upper), my_temperature())


      ggplot(my_counts()) +
        geom_point(aes(x = time, y = logN)) +
        geom_line(aes(x = time, y = logN), data = pred_guess$simulation) +
        geom_line(aes(x = time, y = logN), data = pred_lower$simulation, colour = "steelblue") +
        geom_line(aes(x = time, y = logN), data = pred_upper$simulation, colour = "magenta")
    })

    ## Model fitting -----------------------------------------------------------

    my_fit <- reactiveVal()

    observeEvent(input$go, {

      validate(
        need(my_temperature(), "Input the temperature profile"),
        need(my_counts(), "Input the microbial counts")
      )

      my_model <- input$model

      ## Get the model parameters

      par_names <- get_model_data(input$model)$parameters
      var_names <- paste0(get_model_data(input$model)$variables, "0")

      guess <- list()
      known <- list()
      lower <- list()
      upper <- list()

      for (each_name in c(par_names, var_names)) {

        if (isTRUE(input[[paste0(each_name, "_fixed")]])) {
          known[[each_name]] <- input[[each_name]]
        } else {
          guess[[each_name]] <- input[[each_name]]
          lower[[each_name]] <- input[[paste0(each_name, "_lower")]]
          upper[[each_name]] <- input[[paste0(each_name, "_upper")]]
        }

      }

      d <- my_counts() %>%
        mutate(N = 10^logN) %>%
        as.data.frame()

      if (input$algorithm == "nlr") {
        out <- fit_dynamic_inactivation(d, my_model, my_temperature(),
                                        unlist(guess), unlist(upper), unlist(lower), unlist(known))
      } else {

        out <- fit_inactivation_MCMC(d, my_model, my_temperature(),
                                     unlist(guess), unlist(upper), unlist(lower), unlist(known),
                                     niter = input$niter,
                                     updatecov = input$updatecov,
                                     burninlength = input$burnin
                                     )

      }

      # print(out)

      my_fit(out)

    })

    ## Output ------------------------------------------------------------------

    output$plot_fit <- renderPlot({

      validate(
        need(my_fit(), "Fit the model first")
      )
      plot(my_fit())
    })

    ## Seed --------------------------------------------------------------------

    observeEvent(input$reset_seed, {
      set.seed(124124)
    })

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
