
library(shiny)
library(bs4Dash)

library(tidyverse)
library(bioinactivation)

library(plotly)

## UI --------------------------------------------------------------------------

fit2step_module_ui <- function(id) {

  tagList(

    fluidRow(
      column(12,
             bs4Jumbotron(
               width = 12,
               status = "info",
               btnName = NULL,
               title = "Fitting of isothermal data using a two-step approach",
               p(
                 paste(
                   "This model is designed to fit primary and secondary models following a two-step approach.",
                   "The primary model is fitted for each temperature condition by non-linear regression.",
                   "The model implements a heuristic to find realistic starting guesses for the model parameters.",
                   "Once the primary models have been fitted, the secondary model is fitted on a second step (also by non-linear regression)."
                   )
               ),
               p(
                 paste(
                   "When using Weibullian models (Peleg or Mafart), be mindful that, unlike usually recommended, this module does not fix the shape factor.",
                   "Therefore, it is recommended to compare the results against those obtained using the one-step approach (which fixes a unique shape parameter)."
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
               title = "Primary model",
               status = "primary",
               footer = actionButton(NS(id, "fit_first"), "Fit primary",
                                     outline = TRUE, flat = FALSE,
                                     status = "primary"),
               fluidRow(
                 column(12,
                        pickerInput(NS(id, "model"), "Model",
                                    choices = c("Bigelow", "Mafart", "Peleg", "Geeraerd"
                                                # "Trilinear", "Metselaar"
                                                ),
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
               plotlyOutput(NS(id, "plot_primary")),
               footer = dropdownButton(
                 circle = TRUE, status = "success", right = TRUE,
                 icon = icon("gear"), width = "300px",
                 textInput(NS(id, "xlabel"), "x-label", "Time (min)"),
                 textInput(NS(id, "ylabel"), "y-label", "Microbial count (log CFU/g)"),
                 colourInput(NS(id, "linecol"), "Line colour", "black"),
                 numericInput(NS(id, "linesize"), "Line size", 1),
                 numericInput(NS(id, "linetype"), "Line type",
                              1, min = 0, step = 1),
                 colourInput(NS(id, "pointcol"), "Point colour", "black"),
                 numericInput(NS(id, "pointsize"), "Point size", 4),
                 numericInput(NS(id, "pointshape"), "Point shape", 1, min = 0, step = 1)
               )
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
               ),
               footer = dropdownButton(
                 circle = TRUE, status = "success", right = TRUE,
                 icon = icon("gear"), width = "300px",
                 textInput(NS(id, "xlabel2"), "x-label", "Temperature (ºC)"),
                 textInput(NS(id, "ylabel2"), "y-label", NULL),
                 colourInput(NS(id, "linecol2"), "Line colour", "black"),
                 numericInput(NS(id, "linesize2"), "Line size", 1),
                 numericInput(NS(id, "linetype2"), "Line type",
                              1, min = 0, step = 1),
                 colourInput(NS(id, "pointcol2"), "Point colour", "black"),
                 numericInput(NS(id, "pointsize2"), "Point size", 4),
                 numericInput(NS(id, "pointshape2"), "Point shape", 1, min = 0, step = 1)
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
      Metselaar = c("D", "p", "Delta", "logN0"),
      Geeraerd = c("SL", "D", "logNres", "logN0"),
      Trilinear = c("SL", "D", "logNres", "logN0")
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

      Metselaar = function(p, t) {

        p <- as.list(p)

        p$logN0 - p$Delta*(t/p$Delta/p$D)^p$p

      },

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
        na.omit() %>%
        split(.$temperature) %>%
        map(.,
            ~ get_initialguess(input$model, .)
            )

      # print(initial_guess)

      ## Fit the model

      out <- my_data() %>%
        na.omit() %>%
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

    output$plot_primary <- renderPlotly({

      validate(
        need(primary_models(), "")
        )

      my_predictions <- my_data() %>%
        na.omit() %>%
        split(.$temperature) %>%
        map2(., primary_models(),
             ~ tibble(t = seq(0, max(.x$time), length = 100),
                      logN = prediction_map[[input$model]](coef(.y), t)
             )
        ) %>%
        imap_dfr(., ~ mutate(.x, temperature = .y))

      p <- ggplot() +
        geom_point(aes(x = time, y = logN), data = na.omit(my_data()),
                   colour = input$pointcol,
                   size = input$pointsize,
                   shape = input$pointshape) +
        geom_line(aes(x = t, y = logN), data = my_predictions,
                  colour = input$linecol,
                  size = input$linesize,
                  linetype = input$linetype) +
        facet_wrap("temperature", scales = "free") +
        xlab(input$xlabel) +
        ylab(input$ylabel)

      ggplotly(p)

    })

    output$table_primary <- renderTable({
      validate(need(primary_pars(), ""))

      primary_pars()
    })

    ## Secondary fitting -------------------------------------------------------

    secondary_fit <- reactiveVal()

    observeEvent(input$fit_secondary, {

      # browser()

      p <- primary_pars() %>% mutate(temperature = as.numeric(temperature))
      Tref <- input$Tref

      # print(p)

      if (is.null(p)) return(NULL)

      out <- if (input$model == "Peleg") {

        Tc_guess <- min(p$temperature, na.rm = TRUE)
        k_guess <- coef(lm(b ~ temperature, data = p))[[2]]

        nls(b ~ log(1 + exp(k*(temperature - Tc))),
            data = p,
            start = list(Tc = Tc_guess, k = k_guess),
            control = list(warnOnly = TRUE),
            model = TRUE
            )
      } else if (input$model == "Mafart") {

        my_lm <- lm(log10(delta) ~ temperature, data = p)
        guess_logdelta <- predict(my_lm, newdata = tibble(temperature = Tref))[[1]]
        guess_zeta <- -1/coef(my_lm)[[2]]


        nls(log_delta ~ log_deltaref - (temperature - Tref)/z,
            data = p %>% mutate(log_delta = log10(delta)),
            start = list(log_deltaref = guess_logdelta,
                         z = guess_zeta),
            control = list(warnOnly = TRUE),
            model = TRUE
        )

      } else {

        my_lm <- lm(log10(D) ~ temperature, data = p)
        guess_logD <- predict(my_lm, newdata = tibble(temperature = Tref))[[1]]
        guess_zeta <- -1/coef(my_lm)[[2]]

        nls(logD ~ logDref - (temperature - Tref)/z,
            data = p %>% mutate(logD = log10(D)),
            start = list(logDref = guess_logD,
                         z = guess_zeta),
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

        p <- d %>%
          ggplot(aes(x = temperature)) +
          geom_point(aes(y = b),
                     colour = input$pointcol2,
                     size = input$pointsize2,
                     shape = input$pointshape2) +
          geom_line(aes(y = model),
                    colour = input$linecol2,
                    size = input$linesize2,
                    linetype = input$linetype2)

      } else if (input$model == "Mafart") {

        p <- d %>%
          mutate(log_delta = log10(delta)) %>%
          ggplot(aes(x = temperature)) +
          geom_point(aes(y = log_delta),
                     colour = input$pointcol2,
                     size = input$pointsize2,
                     shape = input$pointshape2) +
          geom_line(aes(y = model),
                    colour = input$linecol2,
                    size = input$linesize2,
                    linetype = input$linetype2)

      } else {

        p <- d %>%
          mutate(logD = log10(D)) %>%
          ggplot(aes(x = temperature)) +
          geom_point(aes(y = logD),
                     colour = input$pointcol2,
                     size = input$pointsize2,
                     shape = input$pointshape2) +
          geom_line(aes(y = model),
                    colour = input$linecol2,
                    size = input$linesize2,
                    linetype = input$linetype2)

      }

      p + xlab(input$xlabel2) + ylab(input$ylabel2)


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
