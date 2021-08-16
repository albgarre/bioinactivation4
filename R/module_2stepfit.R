
library(shiny)
library(bs4Dash)

library(tidyverse)
library(bioinactivation)

## UI --------------------------------------------------------------------------

fit2step_module_ui <- function(id) {

  tagList(

    fluidRow(
      column(12,
             bs4Jumbotron(
               width = 12,
               status = "info",
               title = "Fitting of isothermal data using a two-step approach",
               "asfsa"
             )
      )
    ),
    tableInput_module_ui(NS(id, "input_data"), box_title = "Input data",
                         status = "primary", status_2 = "primary"),
    fluidRow(
      column(6,
             bs4Card(
               width = 12,
               title = "Primary model",
               status = "primary",
               footer = actionButton(NS(id, "fit_first"), "Fit primary",
                                     outline = TRUE, flat = FALSE,
                                     status = "primary"),
               fluidRow(
                 column(12,
                        pickerInput(NS(id, "model"), "Model",
                                    choices = c("Bigelow", "Mafart", "Peleg", "Geeraerd"),
                                    selected = "Bigelow"
                        )
                 )
               )
             )
      ),
      column(6,
             bs4Card(
               width = 12,
               title = "Primary fits",
               status = "success",
               plotOutput(NS(id, "plot_primary"))
             )
             )
    ),
    fluidRow(
      column(6,
             bs4Card(
               width = 12,
               title = "Secondary model",
               status = "primary",
               footer = actionButton(NS(id, "fit_secondary"), "Fit secondary",
                                     outline = TRUE, flat = FALSE,
                                     status = "primary"),
               fluidRow(
                 column(12, tableOutput(NS(id, "table_primary")))
               ),
               fluidRow(
                 column(6, numericInput(NS(id, "Tref"), "Tref", 100))
                        )
               )
             ),
      column(6,
             bs4Card(
               width = 12,
               title = "Secondary fits",
               status = "success",
               fluidRow(
                 column(12, plotOutput(NS(id, "plot_secondary")))
               ),
               hr(),
               fluidRow(
                 column(12, verbatimTextOutput(NS(id, "summary_secondary")))
               )
             )
      )
    )

  )

}

## Server ----------------------------------------------------------------------

fit2step_module_server <- function(id) {

  moduleServer(id, function(input, output, session) {

    ## Input -------------------------------------------------------------------

    data("isothermal_inactivation")

    my_data <- tableInput_module_server("input_data",
                                        col_names = c("time", "temperature", "logN"),
                                        # default_data = head(isothermal_inactivation, 20),
                                        xvar = "time", yvar = "logN", colvar = "temperature",
                                        default_data = isothermal_inactivation %>%
                                          head(., 20) %>%
                                          set_names(c("time", "temperature", "logS")) %>%
                                          mutate(logN = logS + 8) %>%
                                          select(-logS)
    )

    ## Functions primary fitting  -----------------------------------------------------------

    smart_guy <- list(
      D = function(time, logN) {
        max(time)/(max(logN) - min(logN))
      },
      logN0 = function(time, logN) {
        max(logN)
      },
      delta = function(time, logN) {
        max(time)/(max(logN) - min(logN))
      },
      p = function(time, logN) {
        1
      },
      b = function(time, logN) {
        (max(logN) - min(logN))/max(time)
      },
      n = function(time, logN) {
        1
      },
      SL = function(time, logN) {
        0
      },
      logNres = function(time, logN){
        min(logN)
      }
    )

    model_map <- list(
      Bigelow =  c("D", "logN0"),
      Mafart = c("delta", "p", "logN0"),
      Peleg = c("b", "n", "logN0"),
      # Metselaar = c("D", "p", "Delta", "logN0"),
      Geeraerd = c("SL", "D", "logNres", "logN0")
      # Trilinear = c("SL", "D", "logNres", "logN0")
    )

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

    get_residuals <- function(parameters, data, model, known) {

      pred <- prediction_map[[model]](c(parameters, known), data$time)
      res <- pred - data$logN

      res

    }

    get_initialguess <- function(model, d) {


      par_names <- model_map[[model]]

      p <- list()

      for (i in par_names) {

        p[[i]] <- smart_guy[[i]](d$time, d$logN)

      }
      # names(p) <- par_names

      p <- unlist(p)


    }

    ## Primary fitting ---------------------------------------------------------

    primary_models <- reactiveVal()
    primary_pars <- reactiveVal()

    observeEvent(input$fit_first, {

      ## Get the initial guesses of the parameters

      initial_guess <- my_data() %>%
        split(.$temperature) %>%
        map(.,
            ~ get_initialguess(input$model, .)
            )

      # print(initial_guess)

      ## Fit the model

      out <- my_data() %>%
        split(.$temperature) %>%
        map2(., initial_guess,
             ~ modFit(get_residuals, .y, data = .x, model = input$model, known = c())
        )

      # out %>% map(summary) %>% map(print)
      primary_models(out)

      ## Extract the model parameters

      out_pars <- out %>%
        map(coef) %>%
        map(.,
            ~ tibble(par = names(.), value = .)
            ) %>%
        imap_dfr(., ~ mutate(.x, temperature = .y)) %>%
        pivot_wider(., names_from = "par", values_from = "value")

      primary_pars(out_pars)

    })

    observeEvent(input$model, {
      primary_models(NULL)
      primary_pars(NULL)
      secondary_fit(NULL)
    })

    ## Output of the primary fit -----------------------------------------------

    output$plot_primary <- renderPlot({

      validate(
        need(primary_models(), "")
        )

        my_predictions <- my_data() %>%
          split(.$temperature) %>%
          map2(., primary_models(),
               ~ tibble(t = seq(0, max(.x$time), length = 100),
                        logN = prediction_map[[input$model]](coef(.y), t)
                        )
               ) %>%
          imap_dfr(., ~ mutate(.x, temperature = .y))

        ggplot() +
          geom_point(aes(x = time, y = logN), data = my_data()) +
          geom_line(aes(x = t, y = logN), data = my_predictions) +
          facet_wrap("temperature", scales = "free")

    })

    output$table_primary <- renderTable({
      validate(need(primary_pars(), ""))

      primary_pars()
    })

    ## Secondary fitting -------------------------------------------------------

    secondary_fit <- reactiveVal()

    observeEvent(input$fit_secondary, {

      p <- primary_pars() %>% mutate(temperature = as.numeric(temperature))
      Tref <- input$Tref

      # print(p)

      if (is.null(p)) return(NULL)

      out <- if (input$model == "Peleg") {
        nls(b ~ log(1 + exp(k*(temperature - Tc))),
            data = p,
            start = list(Tc = 100, k = 1),
            control = list(warnOnly = TRUE),
            model = TRUE
            )
      } else if (input$model == "Mafart") {
        nls(log_delta ~ log_deltaref - (temperature - Tref)/z,
            data = p %>% mutate(log_delta = log10(delta)),
            start = list(log_deltaref = 1, z = 5),
            control = list(warnOnly = TRUE),
            model = TRUE
        )

      } else {
        nls(logD ~ logDref - (temperature - Tref)/z,
            data = p %>% mutate(logD = log10(D)),
            start = list(logDref = 1, z = 5),
            control = list(warnOnly = TRUE),
            model = TRUE
            )
      }

      secondary_fit(out)


    })

    ## Output secondary --------------------------------------------------------

    output$summary_secondary <- renderPrint({

      validate(need(secondary_fit(), ""))

      summary(secondary_fit())

    })

    output$plot_secondary <- renderPlot({

      validate(need(secondary_fit(), ""))

      d <- primary_pars() %>%
        mutate(temperature = as.numeric(temperature),
               model = predict(secondary_fit()))

      if (is.null(p)) return(NULL)

      if (input$model == "Peleg") {

        d %>%
          ggplot(aes(x = temperature)) +
          geom_point(aes(y = b)) +
          geom_line(aes(y = model))

      } else if (input$model == "Mafart") {

        d %>%
          mutate(log_delta = log10(delta)) %>%
          ggplot(aes(x = temperature)) +
          geom_point(aes(y = log_delta)) +
          geom_line(aes(y = model))

      } else {

        d %>%
          mutate(logD = log10(D)) %>%
          ggplot(aes(x = temperature)) +
          geom_point(aes(y = logD)) +
          geom_line(aes(y = model))

      }

    })

  })

}

## test ------------------------------------------------------------------------

fit2step_module_test <- function(id) {

  shinyApp(
    ui = dashboardPage(
      title = "",
      header = dashboardHeader(),
      sidebar = dashboardSidebar(),
      controlbar = dashboardControlbar(),
      footer = dashboardFooter(),
      body = dashboardBody(
        fit2step_module_ui("test")
      )
    ),
    server = function(input, output) {
      fit2step_module_server("test")
    }
  )

}
