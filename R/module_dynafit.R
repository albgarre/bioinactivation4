
library(shiny)
library(bs4Dash)
library(shinycssloaders)

library(tidyverse)
library(bioinactivation)

library(plotly)

## UI --------------------------------------------------------------------------

dynafit_module_ui <- function(id) {

  tagList(
    fluidRow(
      column(12,
             bs4Jumbotron(
               width = 12,
               status = "info",
               btnName = NULL,
               title = "Model fitting from dynamic data",
               p(
                 paste(
                   "This module is designed to fit both primary and secondary models to data gathered under dynamic conditions.",
                   "The fitting algorithm combines a numeric solver for the differential equations with a fitting algorithm."
                 )
               ),
               p(
                 "Because these models usually suffer from poor parameter identifiability, the module implements both non-linear regression (Levenberg-Marquardt) and an Adatptive Monte Carlo algorithm."
               )
             )
      )
    ),
    tableInput_module_ui(NS(id, "temp_profile"), box_title = "Temperature profile",
                         status = "primary", status_2 = "primary"),
    tableInput_module_ui(NS(id, "micro_data"), box_title = "Microbial data",
                         status = "primary", status_2 = "primary"),
    fluidRow(
      column(6,
             bs4Card(
               title = "Model", width = 12,
               status = "primary",
               pickerInput(NS(id, "model"), "Model",
                           choices = get_model_data() %>% sort(),
                           selected = "Bigelow"),
               uiOutput(NS(id, "par_selector"))
             )
             ),
      column(6,
             bs4Card(
               title = "Initial guess", width = 12,
               plotOutput(NS(id, "plot_guess"))
             )
             )
    ),
    fluidRow(
      column(6,
             bs4Card(
               title = "Fitting algorithm",
               status = "primary", width = 12,
               # footer = actionBttn(NS(id, "go"), "Fit the model", style = "material-flat"),
               footer = actionButton(NS(id, "go"), "Fit the model",
                                     outline = TRUE, flat = FALSE,
                                     status = "primary"
                                     ),
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
                                  # column(6, actionBttn(NS(id, "reset_seed"), "Reset seed", style = "material-flat"))
                                  column(6, actionButton(NS(id, "reset_seed"), "Reset seed",
                                                         outline = TRUE, flat = FALSE,
                                                         status = "primary"
                                                         ))
                                )
               )

             )
             ),
      column(6,
             bs4Card(
               status = "success",
               title = "Model fit", width = 12,
               footer = tagList(
                 fluidRow(
                   column(6,
                          dropdownButton(
                            circle = TRUE, status = "success", right = TRUE,
                            icon = icon("gear"), width = "300px",
                            textInput(NS(id, "xlabel"), "x-label", "Time (min)"),
                            textInput(NS(id, "ylabel"), "y-label", "Microbial count (log CFU/g)"),
                            numericInput(NS(id, "miny"), "min. y", 0),
                            numericInput(NS(id, "maxy"), "max. y", 11),
                            prettySwitch(NS(id, "add_temp"), "Add temperature", slim = TRUE),
                            conditionalPanel("input.add_temp == true", ns = NS(id),
                                             textInput(NS(id, "ylabel2"), "Sec. y-label", "Temperature (ºC)")
                            )
                          )
                   ),
                   column(6, align = "right",
                          downloadBttn(NS(id, "download"), "",
                                       color = "success",
                                       size = "lg",
                                       style = "unite")
                   )
                 )
               ),
               plotlyOutput(NS(id, "plot_fit")) %>% withSpinner()
             )
             )
    ),
    fluidRow(
      column(6,
             bs4Card(
               title = "Parameter estimates",
               status = "warning", width = 12,
               tableOutput(NS(id, "par_table")),
               tableOutput(NS(id, "res_table"))
             )
             ),
      column(6,
             bs4Card(
               title = "Residuals",
               status = "warning", width = 12,
               plotOutput(NS(id, "res_plot")),
               plotOutput(NS(id, "res_hist")),
               textOutput(NS(id, "shapiro_test")),
               plotOutput(NS(id, "MCMC_chain"))
             )
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
                                                                         temperature = c(50, 60, 55)),
                                               add_lines = TRUE
    )

    my_counts <- tableInput_module_server("micro_data",
                                          col_names = c("time", "logN"),
                                          xvar = "time", yvar = "logN",
                                          default_data =  data.frame(
                                            time = seq(0, 20, length = 8),
                                            logN = c(8, 7.9, 7.5, 7.3, 5.5, 1.8, -1, -2)+2
                                          )
    )

    ## Parameter selector ------------------------------------------------------

    par_map <- tribble(
      ~par, ~label, ~value, ~fixed,
      "z", "z-value (ºC)", 5, FALSE,
      "D_R", "D-value at Tref (min)", 5, FALSE,
      "temp_ref", "Tref (ºC)", 57, TRUE,
      "delta_ref", "delta-value at Tref (min)", 5, FALSE,
      "p", "p-value (·)", 1, FALSE,
      "n", "n (·)", 1, FALSE,
      "k_b", "k", 1e-2, FALSE,
      "temp_crit", "Tcrit (ºC)", 60, FALSE,
      "Delta", "Delta (log CFU)", 6, TRUE,
      "k_ref", "k at Tref", 1e-2, FALSE,
      "Ea", "Ea", 1e-3, FALSE,
      "N0", "N0 (CFU/g)", 1e10, FALSE,
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

    observeEvent(input$go, withProgress(message = "Fitting the model...",
                                        {

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
    )

    ## Output ------------------------------------------------------------------

    output$plot_fit <- renderPlotly({

      validate(
        need(my_fit(), "Fit the model first")
      )

      p <- plot(my_fit(),
           plot_temp = input$add_temp,
           label_y1 = input$ylabel,
           label_y2 = input$ylabel2,
           ylims = c(input$miny, input$maxy)
           ) +
        xlab(input$xlabel)

      ggplotly(p)

    })

    output$par_table <- renderTable({

      validate(
        need(my_fit(), "Fit the model first")
      )

      if (isTRUE(is.FitInactivation(my_fit()))) {

        summary(my_fit())$par %>%
          as_tibble(rownames = "Parameter") %>%
          select(Parameter, Estimate, `Std. Error`)

      } else {

        summary(my_fit()) %>%
          rownames_to_column("Index")


      }

    })

    output$res_table <- renderTable({

      validate(
        need(my_fit(), "")
      )

      goodness_of_fit(my_fit())

    })

    ## Download predictions

    output$download <- downloadHandler(
      filename = "inactivation-curve.csv",
      content = function(file) {

        my_fit()$best_prediction$simulation %>%
          write_excel_csv(., path = file)
        # write_excel_csv(static_prediction_list(), path = file)

      }
    )

    ## Diagnosis ---------------------------------------------------------------

    dyna_modCost <- reactive({

      validate(need(my_fit(), message = FALSE))

      my_simulation <- my_fit() %>%
        .$best_prediction %>%
        .$simulation %>%
        select(time, logN) %>%
        as.data.frame()

      my_data <- my_counts() %>%
        as.data.frame() %>%
        select(time, logN)

      out <- modCost(model = my_simulation,
              obs = my_data)

      out

    })

    output$res_plot <- renderPlot({

      validate(
        need(my_fit(), "")
      )

      dyna_modCost()$residuals %>%
        ggplot() +
        geom_point(aes(x = x, y = res)) +
        xlab("Time") + ylab("Residual") +
        geom_hline(yintercept = 0, linetype = 2)


    })

    output$shapiro_test <- renderText({

      test_results <- shapiro.test(dyna_modCost()$residuals$res)

      if (test_results$p.value < 0.05) {
        paste0("There is enough statistical signifficante to state that residuals are not normal, p-value: ",
               round(test_results$p.value, 3))
      } else {
        paste0("There is NOT enough statistical signifficante to state that residuals are not normal, p-value: ",
               round(test_results$p.value, 3))
      }

    })

    output$res_hist <- renderPlot({


      validate(
        need(my_fit(), "")
      )

      dyna_modCost()$residuals %>%
        ggplot() +
        geom_histogram(aes(res)) +
        xlab("Residual") +
        geom_vline(xintercept = 0, linetype = 2)

    })

    output$MCMC_chain <- renderPlot({

      validate(
        need(my_fit(), "")
      )

      if (isTRUE(is.FitInactivation(my_fit()))) {
        NULL
      } else {
        plot(my_fit()$modMCMC)
      }


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
