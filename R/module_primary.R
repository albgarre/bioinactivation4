

library(shiny)
library(bs4Dash)
library(colourpicker)

library(tidyverse)

library(FME)

## UI --------------------------------------------------------------------------

primary_module_ui <- function(id) {

  tagList(
    tableInput_module_ui(NS(id, "input_data"), box_title = "Input data",
                         status = "primary", status_2 = "primary"),
    fluidRow(
      column(6,
             bs4Card(
               title = "Model parameters",
               status = "primary",
               width = 12,
               footer = actionBttn(NS(id, "fit"), "Fit model", style = "material-flat"),
               pickerInput(NS(id, "model"), "Model",
                           choices = c("Bigelow", "Mafart", "Peleg", # "Metselaar",
                                       "Geeraerd", "Trilinear")
                           ),
               uiOutput(NS(id, "parameters"))
             )
             ),
      column(6,
             bs4Card(
               status = "success",
               title = "Model fit",
               width = 12,
               footer = downloadBttn(NS(id, "download"), "Download fitted curve",
                                     style = "material-flat"),
               plotOutput(NS(id, "plot_fit")),
               dropdownMenu = boxDropdown(
                 boxDropdownItem(
                   textInput(NS(id, "xlabel"), "x-label", "Time (min)"),
                   textInput(NS(id, "ylabel"), "y-label", "Microbial count (log CFU/g)"),
                   colourInput(NS(id, "linecol"), "Line colour", "black"),
                   numericInput(NS(id, "linesize"), "Line size", 1),
                   numericInput(NS(id, "linetype"), "Line type",
                               1, min = 0, step = 1),
                   colourInput(NS(id, "pointcol"), "Point colour", "black"),
                   numericInput(NS(id, "pointsize"), "Point size", 5),
                   numericInput(NS(id, "pointshape"), "Point shape", 1, min = 0, step = 1)
                 )
               )
             )
             )
    ),
    fluidRow(
      bs4Card(
        status = "warning",
        title = "Parameter estimates",
        fluidRow(tableOutput(NS(id, "par_table"))),
        fluidRow(tableOutput(NS(id, "res_table")))
      ),
      bs4TabCard(
        status = "warning",
        # solidHeader = TRUE,
        type = "tabs",
        title = "Residuals", side = "right",
        tabPanel("Plot vs time",
                 status = "warning",
                 plotOutput(NS(id, "residual_plot"))
        ),
        tabPanel("Histogram",
                 status = "warning",
                 plotOutput(NS(id, "residuals_hist"))
        ),
        tabPanel("Statistics",
                 status = "warning",
                 tags$h3("Shapiro-Wilk normality test of the residuals"),
                 verbatimTextOutput(NS(id, "residuals_normality"))
        )
      )
    )
  )

}

## Server ----------------------------------------------------------------------

primary_module_server <- function(id) {

  moduleServer(id, function(input, output, session) {


    ## Download predictions

    output$download <- downloadHandler(
      filename = "fitted-primary.csv",
      content = function(file) {

        validate(
          need(my_fit(), "Fit the model first")
        )

        ## Get the known pars

        par_names <- model_map[[input$model]]

        known <- list()

        for (i in par_names) {


          if (isTRUE(input[[paste0(i, "_fixed")]])) {
            known[[i]] <- input[[i]]
          }

        }

        known <- unlist(known)

        ## Output the prediction

        tibble(time = seq(0, max(my_data()$time), length = 1000),
               logN = prediction_map[[input$model]](c(my_fit()$par, known), time)
               ) %>%
          write_excel_csv(., path = file)
        # write_excel_csv(static_prediction_list(), path = file)

      }
    )

    ## Input -------------------------------------------------------------------

    my_data <- tableInput_module_server("input_data",
                                        col_names = c("time", "logN"),
                                        xvar = "time", yvar = "logN",
                                        default_data = data.frame(
                                          time = c(0, 1, 2, 3, 4, 5, 6),
                                          logN = c(6, 5.2, 3.9, 3.4, 2.1, 1.6, 1.8)
                                        )
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
      "SL", "Shoulder length (min)", 1, FALSE,
      "logNres", "Tail height (log CFU/g)", 1, FALSE

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

    output$parameters <- renderUI({

      par_names <- model_map[[input$model]]

      par_names %>%
        map(.,
            ~ make_input(.)
            )

    })

    ## Model fitting -----------------------------------------------------------

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

    # get_residuals(c(D = 5), data = d, "Bigelow", c(logN0 = 6))

    my_fit <- reactiveVal()

    observeEvent(input$fit, {

      ## Get the model parameters

      par_names <- model_map[[input$model]]

      p <- list()
      known <- list()

      for (i in par_names) {

        if (isTRUE(input[[paste0(i, "_fixed")]])) {
          known[[i]] <- input[[i]]
        } else {
          p[[i]] <- input[[i]]
        }

      }
      # names(p) <- par_names

      p <- unlist(p)
      known <- unlist(known)

      ## Fit the model

      out <- modFit(get_residuals, p, data = my_data(), model = input$model, known = known)
      my_fit(out)
    })

    observeEvent(input$model, {
      my_fit(NULL)
    })

    ## Output ------------------------------------------------------------------

    output$plot_fit <- renderPlot({

      validate(
        need(my_fit(), "Fit the model first")
      )

      ## Get the known pars

      par_names <- model_map[[input$model]]

      known <- list()

      for (i in par_names) {


        if (isTRUE(input[[paste0(i, "_fixed")]])) {
          known[[i]] <- input[[i]]
        }

      }

      known <- unlist(known)

      ## Make the plot

      my_data() %>%
        # mutate(pred = prediction_map[[input$model]](time, my_fit()$par)) %>%
        ggplot() +
        geom_point(aes(x=time, y = logN),
                   colour = input$pointcol,
                   size = input$pointsize,
                   shape = input$pointshape) +
        geom_line(aes(x, y), colour = input$linecol,
                  size = input$linesize,
                  linetype = input$linetype,
                  data = tibble(x = seq(0, max(my_data()$time), length = 1000),
                                y = prediction_map[[input$model]](c(my_fit()$par, known), x))
                  ) +
        xlab(input$xlabel) + ylab(input$ylabel)
        # geom_line(aes(x = time, y = pred))

    })

   output$par_table <- renderTable({

     validate(
       need(my_fit(), "Fit the model first")
     )

     summary(my_fit())$par %>%
       as_tibble(rownames = "Parameter") %>%
       select(-`t value`, -`Pr(>|t|)`)

   })

   output$residual_plot <- renderPlot({

     validate(
       need(my_fit(), "Fit the model first")
     )

     my_data() %>%
       mutate(res = residuals(my_fit())) %>%
       ggplot() +
       geom_point(aes(x = time, y = res)) +
       xlab("Treatment time (min)") +
       ylab("Residual (log CFU/g)")

   })

   output$residuals_hist <- renderPlot({

     validate(
       need(my_fit(), "Fit the model first")
     )

     tibble(res = residuals(my_fit())) %>%
       ggplot() +
       geom_histogram(aes(res)) +
       geom_vline(xintercept = 0, linetype = 2, colour = "red") +
       xlab("Residual")

   })

   output$residuals_normality <- renderText({

     validate(
       need(my_fit(), "Fit the model first")
     )

     test_results <- shapiro.test(residuals(my_fit()))

     if (test_results$p.value < 0.05) {
       paste0("There is enough statistical signifficante to state that residuals are not normal, p-value: ",
              round(test_results$p.value, 3))
     } else {
       paste0("There is NOT enough statistical signifficante to state that residuals are not normal, p-value: ",
              round(test_results$p.value, 3))
     }

   })

   output$res_table <- renderTable({

     validate(
       need(my_fit(), "Fit the model first")
     )

     tibble(res = residuals(my_fit())) %>%
       mutate(res2 = res^2,
              LL = dnorm(res, mean(res), sd(res)), log = TRUE) %>%
       summarize(
         RMSE = sqrt(mean(res2)),
         ME = mean(res),
         SER = sqrt(sum(res2)/my_fit()$df.residual),
         loglik = sum(LL),
         Bf = 10^mean(res),
         Af = 10^RMSE
       )

   })

  })

}

## test ------------------------------------------------------------------------

primary_module_test <- function(id) {

  shinyApp(
    ui = dashboardPage(
      title = "",
      header = dashboardHeader(),
      sidebar = dashboardSidebar(),
      controlbar = dashboardControlbar(),
      footer = dashboardFooter(),
      body = dashboardBody(
        primary_module_ui("test")
      )
    ),
    server = function(input, output) {
      primary_module_server("test")
    }
  )

}




