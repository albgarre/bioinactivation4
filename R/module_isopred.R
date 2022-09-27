
library(shiny)
library(bs4Dash)

library(tidyverse)
library(bioinactivation)

library(plotly)
library(FME)

## UI --------------------------------------------------------------------------

isopred_module_ui <- function(id) {

  tagList(
    fluidRow(
      column(12,
             bs4Jumbotron(
               btnName = NULL,
               width = 12,
               status = "info",
               title = "Model predictions based on primary models",
               p(paste0(
                 "This module makes model predictions based on primary inactivation models.",
                 "The solution is calculated based on the algebraic equation of the selected model."
               )),
               p(
                 "Note that this module is intended for model predictions under constant conditions (e.g. isothermal). In case of dynamic conditions, it is recommended to use the appropriate module."
               )
             )
             )
    ),
    fluidRow(
      column(6,
             bs4Card(
               status = "primary",
               title = "Model parameters",
               width = 12,
               footer = tagList(actionButton(NS(id, "go"), "Make prediction",
                                             outline = TRUE, flat = FALSE,
                                             status = "primary"
                                             ),
                                actionButton(NS(id, "clean"), "Clear",
                                             outline = TRUE, flat = FALSE,
                                             status = "secondary"
                                             )
                                ),
               # footer = tagList(actionBttn(NS(id, "go"), "Make prediction",
               #                             style = "material-flat"),
               #                  actionBttn(NS(id, "clean"), "Clear",
               #                             style = "material-flat")
               # ),
               pickerInput(NS(id, "model"), "Model",
                           choices = c("Bigelow", "Mafart", "Peleg", "Metselaar",
                                       "Geeraerd", "Trilinear",
                                       "Weibull_2phase")
               ),
               uiOutput(NS(id, "parameters")),
               numericInput(NS(id, "max_time"), "Treatment time (min)", 10, min = 0),
               textInput(NS(id, "pred_name"), "Prediction label", "Prediction 1")
             )
      ),
      column(6,
             bs4Card(
               status = "success",
               title = "Model predictions",
               width = 12,
               # dropdownMenu = boxDropdown(
               #   boxDropdownItem(
               #     textInput(NS(id, "xlabel"), "x-label", "Time (min)"),
               #     textInput(NS(id, "ylabel"), "y-label", "Microbial count (log CFU/g)")
               #   )
               # ),
               plotlyOutput(NS(id, "plot_fit")),
               footer = tagList(
                 fluidRow(
                   column(6,
                          dropdownButton(
                            circle = TRUE, status = "success", right = TRUE,
                            icon = icon("gear"), width = "300px",
                            textInput(NS(id, "xlabel"), "x-label", "Time (min)"),
                            textInput(NS(id, "ylabel"), "y-label", "Microbial count (log CFU/g)")
                          )
                   ),
                   column(6, align = "right",
                          downloadBttn(NS(id, "download"), "",
                                       color = "success",
                                       size = "lg",
                                       style = "unite")
                   )
                 )
               )
             )
      )
    ),
    fluidRow(
      column(6,
             bs4Card(
               status = "warning", width = 12,
               title = "Log-count at time X",
               fluidRow(column(6, numericInput(NS(id, "target_time"), "Time (min)", 5))),
               fluidRow(column(12, tableOutput(NS(id, "table_counts"))))
             )
             ),
      column(6,
             bs4Card(
               status = "warning", width = 12,
               title = "Time to X reductions",
               fluidRow(
                 column(6, numericInput(NS(id, "target_count"), "Microbial count (log CFU/g)", 4))
               ),
               fluidRow(
                 column(12, tableOutput(NS(id, "table_times")))
               )
             )
             )
    ),
    tableInput_module_ui(NS(id, "input_data"), box_title = "Compare against data",
                         status = "primary", status_2 = "primary"),
    fluidRow(
      column(6,
             bs4TabCard(
               width = 12,
               status = "warning",
               type = "tabs",
               title = "Visual analyses", side = "right",
               tabPanel(
                 "Predictions",
                 status = "warning",
                 plotOutput(NS(id, "pred_vs_data"))
               ),
               tabPanel(
                 "Residual plot",
                 status = "warning",
                 plotOutput(NS(id, "res_plot"))
               ),
               tabPanel(
                 "Histogram",
                 status = "warning",
                 plotOutput(NS(id, "res_hist"))
               )
             )
             ),
      column(6,
             # bs4Card(
             #   width = 12,
             #   tableOutput(NS(id, "res_table"))
             # )
             bs4TabCard(
               width = 12,
               status = "warning",
               # solidHeader = TRUE,
               type = "tabs",
               title = "Numerical analysis", side = "right",
               tabPanel(
                 "Residual table",
                 status = "warning",
                 tableOutput(NS(id, "res_table"))
               ),
               tabPanel(
                 "Residual indexes",
                 status = "warning",
                 tableOutput(NS(id, "res_indexes"))
               )
             )
             )
    )
  )

}

## Server ----------------------------------------------------------------------

isopred_module_server <- function(id) {

  moduleServer(id, function(input, output, session) {

    ## Data validation ---------------------------------------------------------

    my_data <- tableInput_module_server("input_data",
                                        col_names = c("time", "logN"),
                                        xvar = "time", yvar = "logN",
                                        default_data = data.frame(
                                          time = c(0, 1, 2, 3, 4, 5, 6),
                                          logN = c(8, 7.2, 5.9, 5.4, 4.1, 3.6, 3.8)
                                        )
    )

    my_res <- eventReactive(my_data(), {

      my_predictions() %>%
        map(., ~ select(., time, logN)) %>%
        map(as.data.frame) %>%
        map(.,
            ~ modCost(model = .,
                      obs = as.data.frame(my_data())
                      )
            )

    })

    output$res_table <- renderTable({

      my_res() %>%
        map(., ~ .$residuals) %>%
        imap_dfr(., ~ mutate(.x, condition = .y)) %>%
        select(condition, time = x, observed = obs, predicted = mod, residual = res)
    })

    output$res_indexes <- renderTable({
      my_res() %>%
        map(., ~ .$residuals) %>%
        imap_dfr(., ~ mutate(.x, condition = .y)) %>%
        mutate(res2 = res^2) %>%
        group_by(condition) %>%
        summarize(
          ME = mean(res),
          RMSE = sqrt(mean(res2))
        )
    })

    output$pred_vs_data <- renderPlot({

      validate(
        need(my_predictions(), ""),
        need(my_data(), "")
      )

      my_predictions() %>%
        bind_rows() %>%
        ggplot() +
        geom_line(aes(x = time, y = logN, colour = sim)) +
        xlab(input$xlabel) + ylab(input$ylabel) +
        theme(legend.title = element_blank()) +
        geom_point(aes(x = time, y = logN),
                   data = my_data(),
                   inherit.aes = FALSE)

    })

    output$res_hist <- renderPlot({
      validate(
        need(my_res(), "")
      )

      my_res() %>%
        map(., ~ .$residuals) %>%
        imap_dfr(., ~ mutate(.x, condition = .y)) %>%
        ggplot() +
        geom_histogram(aes(res, fill = condition), position = "dodge") +
        geom_vline(xintercept = 0, linetype = 2) +
        theme(legend.title = element_blank()) +
        xlab("Residual (log CFU/g)") +
        ylab("Count")

    })

    output$res_plot <- renderPlot({

      validate(
        need(my_res(), "")
      )

      my_res() %>%
        map(., ~ .$residuals) %>%
        imap_dfr(., ~ mutate(.x, condition = .y)) %>%
        ggplot() +
        geom_point(aes(x = x, y = res, colour = condition)) +
        geom_hline(yintercept = 0, linetype = 2) +
        theme(legend.title = element_blank()) +
        xlab("Time (min)") + ylab("Residual (log CFU/g)")
    })

    ## Download predictions

    output$download <- downloadHandler(
      filename = "inactivation-curves.csv",
      content = function(file) {

        my_predictions() %>%
          bind_rows() %>%
          write_excel_csv(., path = file)
        # write_excel_csv(static_prediction_list(), path = file)

      }
    )

    ## Dynamic parameters ------------------------------------------------------

    model_map <- list(
      Bigelow =  c("D", "logN0"),
      Mafart = c("delta", "p", "logN0"),
      Peleg = c("b", "n", "logN0"),
      Metselaar = c("D", "p", "Delta", "logN0"),
      Geeraerd = c("SL", "D", "logNres", "logN0"),
      Trilinear = c("SL", "D", "logNres", "logN0"),
      Weibull_2phase = c("logN0", "delta1", "p1", "delta2", "p2", "alpha")
    )

    par_map <- tribble(
      ~par, ~label, ~value, ~fixed,
      "D", "D-value (min)", 2, FALSE,
      "delta", "delta-value (min)", 2, FALSE,
      "p", "p-value (·)", 1, FALSE,
      "n", "n (·)", 1, FALSE,
      "Delta", "Delta (log CFU)", 6, TRUE,
      "logN0", "logN0 (log CFU/g)", 8, FALSE,
      "b", "b", .1, FALSE,
      "SL", "Shoulder length (min)", 5, FALSE,
      "logNres", "Tail height (log CFU/g)", 1, FALSE,
      "delta1", "delta-value of pop. 1 (min)", 2, FALSE,
      "delta2", "delta-value of pop. 2 (min)", 4, FALSE,
      "p1", "p-value of pop. 1 (·)", 1, FALSE,
      "p2", "p-value of pop. 2 (·)", .8, FALSE,
      "alpha", "logit population ratio (log f/(f-1))", .99, FALSE

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

    output$parameters <- renderUI({

      par_names <- model_map[[input$model]]

      par_names %>%
        map(.,
            ~ make_input(.)
        )

    })

    ## Predictions -------------------------------------------------------------

    prediction_map <- list(

      Bigelow = function(p, t) {

        p <- as.list(p)

        p$logN0 - t/p$D

      },

      Mafart = function(p, t) {

        p <- as.list(p)

        p$logN0 - (t/p$delta)^p$p

      },

      Geeraerd = function(p, t) {

        p <- as.list(p)

        k <- log(10)/p$D

        p$logNres + log10(( (10^(p$logN0-p$logNres)-1)*exp(k*p$SL) )/(exp(k*t) + exp(k*p$SL) - 1) + 1)

      },

      Peleg = function(p, t) {

        p <- as.list(p)

        p$logN0 - p$b * t^p$n

      },

      Metselaar = function(p, t) {

        p <- as.list(p)

        p$logN0 - p$Delta*(t/p$Delta/p$D)^p$p

      },

      Weibull_2phase = function(p, t) {

        p <- as.list(p)

        N0 <- 10^p$logN0

        part1 <- 10^(- (t/p$delta1)^p$p1 + p$alpha )
        part2 <- 10^(- (t/p$delta2)^p$p2)
        N <- N0/(1 + 10^p$alpha)*(part1 + part2)

        log10(N)

      },

      Trilinear = function(p, t) {
        p <- as.list(p)

        tibble(logN = p$logN0 - (t - p$SL)/p$D) %>%
          mutate(logN = ifelse(t < p$SL, p$logN0, logN),
                 logN = ifelse(logN < p$logNres, p$logNres, logN)) %>%
          pull(logN)
      }

    )

    my_predictions <- reactiveVal()

    observeEvent(input$go, {

      out <- my_predictions()

      ## Get the parameters

      par_names <- model_map[[input$model]]

      p <- list()

      for (i in par_names) {

        p[[i]] <- input[[i]]

      }



      ## Make the prediction

      out[[input$pred_name]] <- tibble(
        time = seq(0, input$max_time, length = 1000),
        logN = prediction_map[[input$model]](p, seq(0, input$max_time, length = 1000)),
        sim = input$pred_name
      )

      my_predictions(out)

    })

    observeEvent(input$clean, {
      my_predictions(NULL)
    })

    ## Output ------------------------------------------------------------------

    output$plot_fit <- renderPlotly({

      validate(
        need(my_predictions(), "")
      )

      p <- my_predictions() %>%
        bind_rows() %>%
        ggplot() +
        geom_line(aes(x = time, y = logN, colour = sim)) +
        xlab(input$xlabel) + ylab(input$ylabel) +
        theme(legend.title = element_blank())

      ggplotly(p)
    })

    output$table_counts <- renderTable({


      validate(
        need(my_predictions(), "")
      )

      my_predictions() %>%
        map(.,
            ~ approx(x = .$time, y = .$logN, xout = input$target_time)$y
            ) %>%
        # print()
        imap_dfr(.,
            ~ tibble(condition = .y, Time = input$target_time, `log count (log CFU/g)` = .x)
            )

    })

    output$table_times <- renderTable({


      validate(
        need(my_predictions(), "")
      )

      my_predictions() %>%
        map(.,
            ~ approx(x = .$logN, y = .$time, xout = input$target_count)$y
        ) %>%
        # print()
        imap_dfr(.,
                 ~ tibble(condition = .y, `log count (log CFU/g)` = input$target_count,
                          Time = .x)
        )

    })



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

