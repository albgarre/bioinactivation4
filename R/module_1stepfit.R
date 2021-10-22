
library(shiny)
library(bs4Dash)

library(tidyverse)
library(bioinactivation)

library(rhandsontable)

library(plotly)

## UI --------------------------------------------------------------------------

fit1step_module_ui <- function(id) {

  tagList(
    fluidRow(
      column(12,
             bs4Jumbotron(
               width = 12,
               status = "info",
               btnName = NULL,
               title = "Fitting of isothermal data using a one-step approach",
               p(
                 paste(
                   "This module implements a one-step fitting approach for data gathered under isothermal conditions."
                 )
               )
             )
      )
    ),
    tableInput_module_ui(NS(id, "input_data"), box_title = "Input data",
                         status = "primary", status_2 = "primary"),
    fluidRow(
      column(6,
             bs4Card(
               width = 12,
               title = "Model parameters",
               status = "primary",
               # footer = actionBttn(NS(id, "fit"), "Fit model", style = "material-flat"),
               footer = actionButton(NS(id, "fit"), "Fit model",
                                     outline = TRUE, flat = FALSE,
                                     status = "primary"),
               fluidRow(
                 column(12,
                        pickerInput(NS(id, "model"), "Model",
                                    choices = get_isothermal_model_data() %>% sort(),
                                    selected = "Bigelow"
                        )
                 )
               ),
               fluidRow(
                 fluidRow(
                   column(12,
                          uiOutput(NS(id, "initial_guess"))
                   )
                 )
               )
             )
             ),
      column(6,
             bs4Card(title = "Fitted curves", status = "success",
                     width = 12,
                     maximizable = TRUE,
                     plotlyOutput(NS(id, "fitted_curve")),
                     footer = dropdownButton(
                       circle = TRUE, status = "success", right = TRUE,
                       icon = icon("gear"), width = "300px",
                       textInput(NS(id, "xlabel"), "x-label", "Treatment time (min)"),
                       textInput(NS(id, "ylabel"), "y-label", "logS (log CFU/g)")
                     )
             )
             )
    ),
    # fluidRow(
    #   column(12,
    #          bs4Card(title = "Fitted curves", status = "success",
    #                  width = 12,
    #                  maximizable = TRUE,
    #                  plotlyOutput(NS(id, "fitted_curve"))
    #          )
    #          )
    #
    # ),
    fluidRow(
      column(6,
             bs4Card(
               title = "Fitted parameters", status = "warning",
               width = 12,
               collapsible = TRUE,
               tableOutput(NS(id, "pars_table")),
               hr(),
               tableOutput(NS(id, "residual_statistics"))
             )
             ),
      column(6,
             bs4TabCard(
               title = "Residuals", side = "right",
               maximizable = TRUE, width = 12,
               type = "tabs", status = "warning",
               tabPanel("Plot vs time",
                        plotOutput(NS(id, "residual_plot"))
               ),
               tabPanel("Histogram",
                        plotOutput(NS(id, "residuals_hist"))
               ),
               tabPanel("Statistics",
                        tags$h3("Shapiro-Wilk normality test of the residuals"),
                        verbatimTextOutput(NS(id, "residuals_normality"))
               )
             )
             )

    )
  )

}

## Server ----------------------------------------------------------------------

fit1step_module_server <- function(id) {

  moduleServer(id, function(input, output, session) {

    ## Input -------------------------------------------------------------------

    data("isothermal_inactivation")

    my_data <- tableInput_module_server("input_data",
                                        col_names = c("time", "temperature", "logS"),
                                        # default_data = head(isothermal_inactivation, 20),
                                        xvar = "time", yvar = "logS", colvar = "temperature",
                                        default_data = isothermal_inactivation %>%
                                          head(., 20) %>%
                                          set_names(c("time", "temperature", "logS"))
                                        )

    ## Parameters --------------------------------------------------------------

    par_map <- tribble(
      ~par, ~label, ~value, ~fixed,
      "z", "z-value (ºC)", 5, FALSE,
      "D_R", "D-value at Tref (min)", 5, FALSE,
      "temp_ref", "Tref (ºC)", 100, TRUE,
      "delta_ref", "delta-value at Tref (min)", 5, FALSE,
      "p", "p-value (·)", 1, FALSE,
      "n", "n (·)", 1, FALSE,
      "k_b", "k", 1e-2, FALSE,
      "temp_crit", "Tcrit (ºC)", 60, FALSE,
      "Delta", "Delta (log CFU)", 6, TRUE,
      "k_ref", "k at Tref", 1e-2, FALSE,
      "Ea", "Ea", 1e-3, FALSE

    )

    make_input <- function(par_name) {

      par_data <- par_map %>%
        filter(par == par_name)

      fluidRow(
        column(6,
               numericInput(NS(id, par_name), par_data$label, par_data$value)
               ),
        column(6,
               awesomeCheckbox(NS(id, paste0(par_name, "_fixed")), "Fixed?", par_data$fixed)
               )
      )

    }

    output$initial_guess <- renderUI({

      pars <- get_isothermal_model_data(input$model)$params

      pars %>%
        map(.,
            ~ make_input(.)
            )
    })

    ## Model fitting -----------------------------------------------------------

    my_fit <- eventReactive(input$fit, {

      ## Get the data

      d <- my_data() %>%
        rename(time = time, temp = temperature, log_diff = logS)

      ## Get the parameters

      known_params <- list()
      starting_point <- list()

      for (each_par in get_isothermal_model_data(input$model)$params) {

        if (input[[paste0(each_par, "_fixed")]]) {
          known_params[each_par] <- input[[each_par]]
        } else {
          starting_point[each_par] <- input[[each_par]]
        }

      }


      ## Fit the model

      fit_isothermal_inactivation(input$model,
                                  d,
                                  starting_point,
                                  known_params)


    })

    ## Model output ------------------------------------------------------------

    output$fitted_curve <- renderPlotly({

      p <- plot(my_fit(), make_gg = TRUE) +
        xlab(input$xlabel) +
        ylab(input$ylabel)

      ggplotly(p)

    })

    output$residual_statistics <- renderTable({
      goodness_of_fit(my_fit())
    })

    output$pars_table <- renderTable({
      summary(my_fit())$parameters %>%
        as_tibble(rownames = "Parameter") %>%
        select(-`t value`, -`Pr(>|t|)`)
    })

    output$residual_plot <- renderPlot({

      my_fit() %>%
        .$data %>%
        mutate(res = residuals(summary(my_fit()))) %>%
        mutate(temperature = factor(temp)) %>%
        ggplot() +
        geom_point(aes(x = time, y = res)) + # , colour = temperature)) +
        geom_hline(yintercept = 0, linetype = 2, size = 1) +
        ylab("Residual") + xlab("Time") +
        facet_wrap("temperature", scales = "free_x")
    })

    output$residuals_hist <- renderPlot({

      my_fit() %>%
        .$data %>%
        mutate(res = residuals(summary(my_fit()))) %>%
        mutate(temperature = factor(temp)) %>%
        ggplot() +
        geom_histogram(aes(res, fill = temperature)) +
        geom_vline(xintercept = 0, linetype = 2, colour = "red") +
        xlab("Residual")

    })

    output$residuals_normality <- renderText({

      test_results <- shapiro.test(residuals(my_fit()$nls))

      if (test_results$p.value < 0.05) {
        paste0("There is enough statistical signifficante to state that residuals are not normal, p-value: ",
               round(test_results$p.value, 3))
      } else {
        paste0("There is NOT enough statistical signifficante to state that residuals are not normal, p-value: ",
               round(test_results$p.value, 3))
      }

    })

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
