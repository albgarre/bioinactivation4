
library(shiny)
library(bs4Dash)

library(tidyverse)
library(bioinactivation)

## UI --------------------------------------------------------------------------

isopred_module_ui <- function(id) {

  tagList(
    fluidRow(
      column(6,
             bs4Card(
               status = "primary",
               title = "Model parameters",
               width = 12,
               footer = tagList(actionBttn(NS(id, "go"), "Make prediction",
                                           style = "material-flat"),
                                actionBttn(NS(id, "clean"), "Clear",
                                           style = "material-flat")
                                ),
               pickerInput(NS(id, "model"), "Model",
                           choices = c("Bigelow", "Mafart", "Peleg", # "Metselaar",
                                       "Geeraerd", "Trilinear")
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
               dropdownMenu = boxDropdown(
                 boxDropdownItem(
                   textInput(NS(id, "xlabel"), "x-label", "Time (min)"),
                   textInput(NS(id, "ylabel"), "y-label", "Microbial count (log CFU/g)")
                 )
               ),
               plotOutput(NS(id, "plot_fit")),
               footer = downloadBttn(NS(id, "download"), "Download simulations",
                            style = "material-flat")
             )
      )
    ),
    fluidRow(
      bs4Card(
        status = "warning",
        title = "Log-count at time X",
        fluidRow(column(6, numericInput(NS(id, "target_time"), "Time (min)", 5))),
        fluidRow(column(12, tableOutput(NS(id, "table_counts"))))
      ),
      bs4Card(
        status = "warning",
        title = "Time to X reductions",
        fluidRow(
          column(6, numericInput(NS(id, "target_count"), "Microbial count (log CFU/g)", 4))
        ),
        fluidRow(
          column(12, tableOutput(NS(id, "table_times")))
        )
      )
    )
  )

}

## Server ----------------------------------------------------------------------

isopred_module_server <- function(id) {

  moduleServer(id, function(input, output, session) {

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
      Trilinear = c("SL", "D", "logNres", "logN0")
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
      "logNres", "Tail height (log CFU/g)", 1, FALSE

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

      # Metselaar = function(p, t) {
      #
      #   p <- as.list(p)
      #
      #   p$logN0 - p$Delta*(t/p$Delta/p$D)^p$p
      #
      # }
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

    output$plot_fit <- renderPlot({

      validate(
        need(my_predictions(), "")
      )

      my_predictions() %>%
        bind_rows() %>%
        ggplot() +
        geom_line(aes(x = time, y = logN, colour = sim)) +
        xlab(input$xlabel) + ylab(input$ylabel) +
        theme(legend.title = element_blank())
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

