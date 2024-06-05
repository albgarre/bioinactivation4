
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
               verbatimTextOutput(NS(id, "pars_summary")),
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
    # fluidRow(
    #   column(6,
    #          bs4TabCard(
    #            type = "tabs",
    #            solidHeader = FALSE,
    #            headerBorder = TRUE,
    #            title = "Export as FSK-ML", status = "danger",
    #            width = 12,
    #            collapsible = TRUE,
    #            # tabsetPanel(
    #              tabPanel("General Information",
    #                       textInput(NS(id, "fsk_name"), "Model name"),
    #                       textInput(NS(id, "fsk_source"), "Source"),
    #                       textInput(NS(id, "fsk_identifier"), "Identifier"),
    #                       dateInput(NS(id, "fsk_date"), "Creation Date"),
    #                       textInput(NS(id, "fsk_rights"), "Rights", value = "Creative Commons Attribution-NonCommercial 4.0"),
    #                       textInput(NS(id, "fsk_language"), "Language", value = "English"),
    #                       tags$h4("Creators"),
    #                       rHandsontableOutput(NS(id, "fsk_pred_creators")),
    #                       tags$h4("Authors"),
    #                       rHandsontableOutput(NS(id, "fsk_pred_authors")),
    #                       tags$h4("Reference"),
    #                       rHandsontableOutput(NS(id, "fsk_pred_reference"))
    #              ),
    #              tabPanel("Scope",
    #                       textInput(NS(id, "fsk_genCom"), "General comment"),
    #                       tags$h4("Product/Matrix"),
    #                       rHandsontableOutput(NS(id, "fsk_pred_product")),
    #                       tags$h4("Hazard"),
    #                       rHandsontableOutput(NS(id, "fsk_pred_hazard"))
    #              ),
    #              tabPanel("Data background",
    #                       textInput(NS(id, "fsk_studyTitle"), "Study title")
    #              ),
    #              tabPanel("Model math",
    #                       rHandsontableOutput(NS(id, "fsk_pred_model"))
    #              ),
    #              tabPanel("Export",
    #                       downloadButton(NS(id, "fsk_download"), "Download")
    #              )
    #            # )
    #          )
    #          )
    # )
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
        select(time = time, temp = temperature, log_diff = logS) %>%
        na.omit()

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

    output$pars_summary <- renderPrint({
      summary(my_fit())
    })

    output$pars_table <- renderTable({
      summary(my_fit())$parameters %>%
        as_tibble(rownames = "Parameter") %>%
        select(-`t value`, -`Pr(>|t|)`) %>%
        mutate(`CI 95% left` = Estimate - 1.96*`Std. Error`,
               `CI 95% right` = Estimate + 1.96*`Std. Error`)
    })

    output$residual_plot <- renderPlot({

      my_fit() %>%
        .$data %>%
        na.omit() %>%
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
        na.omit() %>%
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

    ## FSK-ML ------------------------------------------------------------------

    output$fsk_pred_creators <- renderRHandsontable({
      # rhandsontable(data.frame(Email = c("google@chucknorris.com", NA), `Family name` = c("Doe", NA), `Given Name` = c("Jon", NA)),
      #               rowHeaders = NULL, readOnly = FALSE
      #               )

      default_data <- data.frame(
        title = "",
        familyName = "",
        givenName = "",
        email = "",
        telephone = "",
        streetAdress = "",
        country = "",
        city = "",
        region = "",
        organization = ""
      )

      if (!is.null(input$fsk_pred_creators)) {
        DF = hot_to_r(input$fsk_pred_creators)
      } else {
        DF = default_data
      }

      DF %>%
        rhandsontable() %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })

    output$fsk_pred_authors <- renderRHandsontable({

      default_data <- data.frame(
        title = "",
        familyName = "",
        givenName = "",
        email = "",
        telephone = "",
        streetAdress = "",
        country = "",
        city = "",
        region = "",
        organization = ""
      )

      if (!is.null(input$fsk_pred_authors)) {
        DF = hot_to_r(input$fsk_pred_authors)
      } else {
        DF = default_data
      }

      DF %>%
        rhandsontable() %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })

    output$fsk_pred_reference <- renderRHandsontable({

      default_data <- data.frame(
        publicationType = "",
        publicationDate = "",
        doi = "",
        authorList = "",
        publicationTitle = "",
        publicationAbstract = "",
        publicationStatus = "",
        publicationWebsite = "",
        comment = ""
      )

      if (!is.null(input$fsk_pred_reference)) {
        DF = hot_to_r(input$fsk_pred_reference)
      } else {
        DF = default_data
      }

      DF %>%
        rhandsontable() %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })

    output$fsk_pred_product <- renderRHandsontable({

      default_data <- data.frame(
        productName = "",
        productDescription = "",
        productUnit = "",
        productionMethod = "",
        packaging = "",
        productTreatment = "",
        originCountry = "",
        originArea = "",
        fisheriesArea = "",
        productionDate = "",
        expiryDate = ""
      )

      if (!is.null(input$fsk_pred_product)) {
        DF = hot_to_r(input$fsk_pred_product)
      } else {
        DF = default_data
      }

      DF %>%
        rhandsontable() %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })

    output$fsk_pred_hazard <- renderRHandsontable({

      default_data <- data.frame(
        hazardType = "",
        hazardName = "",
        hazardDescription = "",
        hazardUnit = "",
        adverseEffect = "",
        sourceOfContamination = "",
        maximumResidueLimit = "",
        noObservedAdverseAffectLevel = "",
        lowestObservedAdverseAffectLevel = "",
        acceptableOperatorExposureLevel = "",
        acuteReferenceDose = "",
        acceptableDailyIntake = ""
      )

      if (!is.null(input$fsk_pred_hazard)) {
        DF = hot_to_r(input$fsk_pred_hazard)
      } else {
        DF = default_data
      }

      DF %>%
        rhandsontable() %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })

    output$fsk_pred_model <- renderRHandsontable({

      # browser()



      my_model <- my_fit()

      out <- data.frame(parameter = names(my_model$parameters),
                        estimate = unlist(my_model$parameters),
                        unit = "",
                        `Min value` = 0,
                        `Max value` = "") %>%
        left_join(.,
                  tibble(
                    parameter = c("temp_ref", "D_R", "z", "N0", "delta_ref", "p", "n", "k_b", "temp_crit", "N_min", "C_c0",
                                  "k_ref", "Ea"),
                    description = c("Reference temperature.", "Treatment time required for one log-reduction.",
                                    "Temperature increase required to reduce the D (or delta) value a 90%.",
                                    "Initial microbial count",
                                    "Treatment time required for the 1st log reduction.",
                                    "Shape factor of the Weibull distribution",
                                    "Shape factor of the Weibull distribution",
                                    "Slope of the b vs temperature curve for temperatures about the critical one",
                                    "Critical temperature for inactivation.",
                                    "Tail height.",
                                    "Initial value of the ideal substance defining the shoulder.",
                                    "Inactivation rate at the reference temperature",
                                    "Activation energy")
                  ),
                  by = "parameter")

      rhandsontable(out,
                    rowHeaders = NULL, readOnly = FALSE
      )
    })

    output$fsk_iso_model <- renderRHandsontable({
      my_model <- iso_fitted_model()

      out <- data.frame(parameter = names(my_model$parameters),
                        estimate = unlist(my_model$parameters),
                        unit = "",
                        `Min value` = 0,
                        `Max value` = "") %>%
        left_join(., par_description_map, by = "parameter")

      rhandsontable(out,
                    rowHeaders = NULL, readOnly = FALSE
      )
    })

    output$fsk_download <- downloadHandler(
      filename = "fitted_model.fskx",
      content = function(file) {
        file.copy("fitted_model.fskx", file)
      }
    )

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
