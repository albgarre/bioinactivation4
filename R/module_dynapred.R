
library(shiny)
library(bs4Dash)
library(colourpicker)

library(tidyverse)
library(bioinactivation)

## UI --------------------------------------------------------------------------

dynapred_module_ui <- function(id) {

  tagList(
    fluidRow(
      column(12,
             bs4Jumbotron(
               width = 12,
               status = "info",
               btnName = NULL,
               title = "Model predictions under dynamic conditions",
               p(
                 paste(
                   "This module makes predictions under dynamic conditions combining primary and secondary models.",
                   "For flexibility, the solution of the equation is approximated numerically."
                 )
               )
             )
      )
    ),
    tableInput_module_ui(NS(id, "temp_profile"), box_title = "Temperature profile",
                         status = "primary", status_2 = "primary"),
    fluidRow(
      column(6,
             bs4Card(
               status = "primary",
               title = "Model", width = 12,
               footer = actionButton(NS(id, "go"), "Make prediction",
                                     outline = TRUE, flat = FALSE,
                                     status = "primary"),
               # footer = actionBttn(NS(id, "go"), "Make prediction",
               #                     style = "material-flat"),
               pickerInput(NS(id, "model"), "Model",
                           choices = get_model_data() %>% sort(),
                           selected = "Bigelow"),
               uiOutput(NS(id, "par_selector")),
               hr(),
               numericInput(NS(id, "max_time"), "Treatment time (min)", 30)
             )
             ),
      column(6,
             bs4Card(
               status = "success", width = 12,
               title = "Prediction",
               footer = tagList(
                 fluidRow(
                   column(6,
                          dropdownButton(
                            circle = TRUE, status = "success", right = TRUE,
                            icon = icon("gear"), width = "300px",
                            textInput(NS(id, "xlabel"), "x-label", "Time (min)"),
                            textInput(NS(id, "ylabel"), "y-label", "Microbial count (log CFU/g)"),
                            numericInput(NS(id, "axis_text"), "Size of axis text", 14),
                            numericInput(NS(id, "axis_title"), "Size of axis title", 16),
                            numericInput(NS(id, "ymin"), "min. y", 0),
                            numericInput(NS(id, "ymax"), "max. y", 6),
                            hr(),
                            prettySwitch(NS(id, "add_temp"), "Add temperature",
                                         slim = TRUE),
                            conditionalPanel("input.add_temp == true", ns = NS(id),
                                             textInput(NS(id, "ylabel_2"), "Secondary y-label", "Temperature (ºC)")
                                             # colourInput(NS(id, "line_col2"), "Line colour", "black"),
                                             # selectInput(NS(id, "line_type2"), "Line type",
                                             #             choices = list("solid", "dashed", "dotted", "dotdash",
                                             #                            "longdash", "twodash"))
                            ),
                            hr(),
                            prettySwitch(NS(id, "add_timeto"), "Add time to reduction",
                                         slim = TRUE),
                            conditionalPanel("input.add_timeto == true", ns = NS(id),
                                             numericInput(NS(id, "target_logN"), "target reduction", 1)
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
               # dropdownMenu = boxDropdown(
               #   boxDropdownItem(
               #     textInput(NS(id, "xlabel"), "x-label", "Time (min)"),
               #     textInput(NS(id, "ylabel"), "y-label", "Microbial count (log CFU/g)"),
               #     numericInput(NS(id, "ymin"), "min. y", 0),
               #     numericInput(NS(id, "ymax"), "max. y", 6)
               #     # colourInput(NS(id, "line_col"), "Line colour", "black"),
               #     # selectInput(NS(id, "line_type"), "Line type",
               #     #             choices = list("solid", "dashed", "dotted", "dotdash",
               #     #                            "longdash", "twodash"))
               #   ),
               #   boxDropdownItem(
               #     prettySwitch(NS(id, "add_temp"), "Add temperature",
               #                  slim = TRUE),
               #     conditionalPanel("input.add_temp == true", ns = NS(id),
               #                      textInput(NS(id, "ylabel_2"), "Secondary y-label", "Temperature (ºC)")
               #                      # colourInput(NS(id, "line_col2"), "Line colour", "black"),
               #                      # selectInput(NS(id, "line_type2"), "Line type",
               #                      #             choices = list("solid", "dashed", "dotted", "dotdash",
               #                      #                            "longdash", "twodash"))
               #     )
               #   ),
               #   boxDropdownItem(
               #     prettySwitch(NS(id, "add_timeto"), "Add time to reduction",
               #                  slim = TRUE),
               #     conditionalPanel("input.add_timeto == true", ns = NS(id),
               #                      numericInput(NS(id, "target_logN"), "target reduction", 1)
               #     )
               #   )
               # ),
               plotOutput(NS(id, "plot_survivor"))
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

dynapred_module_server <- function(id) {

  moduleServer(id, function(input, output, session) {

    ## Data validation ---------------------------------------------------------

    my_data <- tableInput_module_server("input_data",
                                        col_names = c("time", "logN"),
                                        xvar = "time", yvar = "logN",
                                        default_data = data.frame(
                                          time = c(0, 5, 10, 15, 20, 25, 30),
                                          logN = c(6.2, 6, 4.5, 2.1, .8, .6, -.5)
                                        )
    )

    my_res <- eventReactive(my_data(), {

      my_prediction()$simulation %>%
        select(time, logN) %>%
        as.data.frame() %>%
        modCost(model = .,
                obs = as.data.frame(my_data()))

    })

    output$res_table <- renderTable({

      my_res()$residuals

    })

    output$res_indexes <- renderTable({
      my_res()$residuals %>%
        mutate(res2 = res^2) %>%
        summarize(
          ME = mean(res),
          RMSE = sqrt(mean(res2))
        )
    })

    output$pred_vs_data <- renderPlot({

      validate(
        need(my_prediction(), ""),
        need(my_data(), "")
      )

      plot(my_prediction()) +
        geom_point(aes(x = time, y = logN),
                   data = my_data(),
                   inherit.aes = FALSE)

    })

    output$res_hist <- renderPlot({
      validate(
        need(my_res(), "")
      )

      my_res()$residuals %>%
        ggplot() +
        geom_histogram(aes(res), position = "dodge") +
        geom_vline(xintercept = 0, linetype = 2) +
        xlab("Residual (log CFU/g)") +
        ylab("Count")

    })

    output$res_plot <- renderPlot({

      validate(
        need(my_res(), "")
      )

      my_res()$residuals %>%
        ggplot() +
        geom_point(aes(x = x, y = res)) +
        geom_hline(yintercept = 0, linetype = 2) +
        xlab("Time (min)") + ylab("Residual (log CFU/g)")
    })

    ## Download prediction -----------------------------------------------------

    output$download <- downloadHandler(
      filename = "inactivation-curve.csv",
      content = function(file) {

        my_prediction()$simulation %>%
          write_excel_csv(., path = file)
        # write_excel_csv(static_prediction_list(), path = file)

      }
    )

    ## Input -------------------------------------------------------------------

    my_temperature <- tableInput_module_server("temp_profile",
                                               col_names = c("time", "temperature"),
                                               xvar = "time", yvar = "temperature",
                                               default_data = data.frame(time = c(0, 10, 20),
                                                                         temperature = c(50, 60, 55))
    )

    ## Parameter selector ------------------------------------------------------

    par_map <- tribble(
      ~par, ~label, ~value,
      "z", "z-value (ºC)", 5,
      "D_R", "D-value at Tref (min)", 5,
      "temp_ref", "Tref (ºC)", 57,
      "delta_ref", "delta-value at Tref (min)", 5,
      "p", "p-value (·)", 1,
      "n", "n (·)", 1,
      "k_b", "k", .5,
      "temp_crit", "Tcrit (ºC)", 60,
      "Delta", "Delta (log CFU)", 6,
      "k_ref", "k at Tref", .3,
      "Ea", "Ea", .1,
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

      validate(need(my_prediction(), ""))

      p <- plot(my_prediction(), plot_temp = input$add_temp,
           label_y1 = input$ylabel, label_y2 = input$ylabel_2,
           ylims = c(input$ymin, input$ymax)) +
        xlab(input$xlabel)

      if (isTRUE(input$add_timeto)) {

        out_t <- time_to_logreduction(input$target_logN, my_prediction())

        if (!is.na(out_t)) {
          p <- p +
            geom_vline(xintercept = out_t, linetype = 3, colour = "red") +
            geom_label(aes(x = out_t, y = input$ymax - input$target_logN,
                           label = paste("t", "=", round(out_t, 2))))
        }

      }

      p  +
        theme(axis.text = element_text(size = input$axis_text),
              axis.title = element_text(size = input$axis_title))

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
