
library(shiny)
library(bs4Dash)

library(tidyverse)
library(bioinactivation)

## UI --------------------------------------------------------------------------

stocpred_module_ui <- function(id) {

  tagList(
    fluidRow(
      column(6,
             bs4Card(
               title = "Model parameters",
               width = 12,
               footer = actionBttn(NS(id, "go"), "Make prediction"),
               pickerInput(NS(id, "model"), "Model",
                           choices = c("Bigelow", "Mafart", "Peleg", # "Metselaar",
                                       "Geeraerd", "Trilinear")
               ),
               uiOutput(NS(id, "parameters")),
               hr(),
               numericInput(NS(id, "max_time"), "Treatment time (min)", 10, min = 0),
               numericInput(NS(id, "niter"), "Number of iterations", 100, min = 1)
             )
      ),
      column(6,
             bs4Card(
               title = "Model prediction",
               width = 12,
               plotOutput(NS(id, "plot_fit"))
             )
      )
    ),
    fluidRow(
      bs4Card(
        title = "Log count at t",
        fluidRow(
          column(6, numericInput(NS(id, "target_time"), "Time (min)", 5, min = 0))
          ),
        fluidRow(
          plotOutput(NS(id, "hist_logN"))
        )
      ),
      bs4Card(
        title = "Time to log count",
        fluidRow(
          column(6, numericInput(NS(id, "target_logN"), "Log count (log CFU/g)", 3))
          ),
        fluidRow(
          plotOutput(NS(id, "hist_time"))
        )
      )
    )
  )

}

## Server ----------------------------------------------------------------------

stocpred_module_server <- function(id) {

  moduleServer(id, function(input, output, session) {


    ## Dynamic parameters ------------------------------------------------------

    model_map <- list(
      Bigelow =  c("logD", "logN0"),
      Mafart = c("log_delta", "p", "logN0"),
      Peleg = c("b", "n", "logN0"),
      Metselaar = c("logD", "p", "Delta", "logN0"),
      Geeraerd = c("SL", "logD", "logNres", "logN0"),
      Trilinear = c("SL", "logD", "logNres", "logN0")
    )

    par_map <- tribble(
      ~par, ~label, ~value, ~fixed,
      "logD", "log D-value (log min)", 1, FALSE,
      "log_delta", "log delta-value (min)", 2, FALSE,
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

      tagList(
        fluidRow(
          column(12, h4(par_name))
        ),
        fluidRow(
          column(6,
                 numericInput(NS(id, paste0("mean_", par_name)), "mean", par_data$value)
          ),
          column(6,
                 numericInput(NS(id, paste0("sd_", par_name)), "SD", par_data$value*.1)
          )
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

    ## Prediction --------------------------------------------------------------

    prediction_map <- list(

      Bigelow = function(p, t) {

        p <- as.list(p)

        p$logN0 - t/10^p$logD

      },

      Mafart = function(p, t) {

        p <- as.list(p)

        p$logN0 - (t/10^p$log_delta)^p$p

      },

      Geeraerd = function(p, t) {

        p <- as.list(p)

        k <- log(10)/10^p$logD

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

        tibble(logN = p$logN0 - (t - p$SL)/10^p$logD) %>%
          mutate(logN = ifelse(t < p$SL, p$logN0, logN),
                 logN = ifelse(logN < p$logNres, p$logNres, logN)) %>%
          pull(logN)
      }

    )

    my_simulations <- reactiveVal()
    my_pars <- reactiveVal()

    observeEvent(input$go, {

      ## Create the parameter sample

      par_names <- model_map[[input$model]]

      pars <- par_names %>%
        set_names(., .) %>%
        map_dfc(.,
                ~ rnorm(input$niter, input[[paste0("mean_", .)]], input[[paste0("sd_", .)]])
                )

      my_pars(pars)

      ## Make the predictions

      t <- seq(0, input$max_time, length = 1000)

      sims <- pars %>%
        mutate(i = row_number()) %>%
        split(.$i) %>%
        map(unlist) %>%
        map(.,
            ~ tibble(time = t,
                     logN = prediction_map[[input$model]](., t)
                     )
            )

      my_simulations(sims)

    })

    ## Output ------------------------------------------------------------------

    output$plot_fit <- renderPlot({

      validate(need(my_simulations(), ""))

      my_simulations() %>%
        imap_dfr(.,
                 ~ mutate(.x, sim = .y)
                 ) %>%
        group_by(time) %>%
        summarize(mean_logN = mean(logN, na.rm = TRUE),
                  med_logN = median(logN, na.rm = TRUE),
                  q05 = quantile(logN, .05, na.rm = TRUE),
                  q95 = quantile(logN, .95, na.rm = TRUE)) %>%
        ggplot(aes(x = time)) +
        geom_line(aes(y = mean_logN)) +
        geom_line(aes(y = med_logN), linetype = 2) +
        geom_ribbon(aes(ymin = q05, ymax = q95), alpha = .5)


    })

    output$hist_logN <- renderPlot({

      validate(need(my_simulations(), ""))

      counts <- my_simulations() %>%
        map_dfr(.,
            ~ tibble(logN = approx(x = .$time, y = .$logN, xout = input$target_time)$y)
            )

      counts %>%
        summarize(mean_logN = mean(logN, na.rm = TRUE),
                  med_logN = median(logN, na.rm = TRUE),
                  q05 = quantile(logN, .05, na.rm = TRUE),
                  q95 = quantile(logN, .95, na.rm = TRUE)) %>%
        ggplot() +
        geom_histogram(aes(logN), data = counts) +
        geom_vline(aes(xintercept = mean_logN), linetype = 2, colour = "grey") +
        geom_vline(aes(xintercept = med_logN), linetype = 3, colour = "grey") +
        geom_vline(aes(xintercept = q05), linetype = 1, colour = "grey") +
        geom_vline(aes(xintercept = q95), linetype = 1, colour = "grey")

    })

    output$hist_time <- renderPlot({

      validate(need(my_simulations(), ""))

      counts <- my_simulations() %>%
        map_dfr(.,
                ~ tibble(time = approx(x = .$logN, y = .$time, xout = input$target_logN)$y)
        )

      counts %>%
        summarize(mean_time = mean(time, na.rm = TRUE),
                  med_time = median(time, na.rm = TRUE),
                  q05 = quantile(time, .05, na.rm = TRUE),
                  q95 = quantile(time, .95, na.rm = TRUE)) %>%
        ggplot() +
        geom_histogram(aes(time), data = counts) +
        geom_vline(aes(xintercept = mean_time), linetype = 2, colour = "grey") +
        geom_vline(aes(xintercept = med_time), linetype = 3, colour = "grey") +
        geom_vline(aes(xintercept = q05), linetype = 1, colour = "grey") +
        geom_vline(aes(xintercept = q95), linetype = 1, colour = "grey")

    })

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
