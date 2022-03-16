

library(shiny)
library(bs4Dash)
library(colourpicker)

library(tidyverse)

library(FME)
# library(FSK2R)
library(plotly)

## UI --------------------------------------------------------------------------

primary_module_ui <- function(id) {

  tagList(
    fluidRow(
      column(12,
             bs4Jumbotron(
               width = 12,
               status = "info",
               btnName = NULL,
               title = "Fitting of primary inactivation models",
               p(
                 paste(
                   "This module fits a single primary model to experimental data.",
                   "It is designed to describe data where every point was obtained under the same condition (e.g. the same temperature).",
                   "Otherwise, it is recommended to use a different module."
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
               title = "Model parameters",
               status = "primary",
               width = 12,
               footer = actionButton(NS(id, "fit"), "Fit model",
                                     outline = TRUE, flat = FALSE,
                                     status = "primary"),
               # footer = actionBttn(NS(id, "fit"), "Fit model", style = "material-flat"),
               pickerInput(NS(id, "model"), "Model",
                           choices = c("Bigelow", "Mafart", "Peleg", "Metselaar",
                                       "Geeraerd", "Trilinear",
                                       "Weibull_2phase"
                                       )
                           ),
               uiOutput(NS(id, "parameters"))
             )
             ),
      column(6,
             bs4Card(
               status = "success",
               title = "Model fit",
               width = 12,
               # footer = downloadBttn(NS(id, "download"), "Download fitted curve",
               #                       style = "material-flat"),
               footer = tagList(
                 fluidRow(
                   column(6,
                          dropdownButton(
                            circle = TRUE, status = "success", right = TRUE,
                            icon = icon("gear"), width = "300px",
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

                   ),
                   column(6, align = "right",
                          downloadBttn(NS(id, "download"), "",
                                       color = "success",
                                       size = "lg",
                                       # width = "300px",
                                       style = "unite")
                   )

                 )
               ),
               plotlyOutput(NS(id, "plot_fit"))
             )
             )
    ),
    fluidRow(
      column(6,
             bs4Card(
               width = 12,
               status = "warning",
               title = "Parameter estimates",
               fluidRow(
                 verbatimTextOutput(NS(id, "par_summary")),
               ),
               fluidRow(tableOutput(NS(id, "par_table"))),
               fluidRow(tableOutput(NS(id, "res_table")))
             )
             ),
      column(6,
             bs4TabCard(
               width = 12,
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
    )# ,
    # fluidRow(
    #   column(6,
    #          bs4TabCard(title = "Export as FSK-ML", status = "danger",
    #                     width = 12, type = "tabs", side = "right",
    #                     tabPanel("General Information",
    #                              textInput(NS(id, "fsk_pred_name"), "Model name"),
    #                              textInput(NS(id, "fsk_pred_source"), "Source"),
    #                              textInput(NS(id, "fsk_pred_identifier"), "Identifier"),
    #                              dateInput(NS(id, "fsk_pred_date"), "Creation Date"),
    #                              textInput(NS(id, "fsk_pred_rights"), "Rights", value = "Creative Commons Attribution-NonCommercial 4.0"),
    #                              textInput(NS(id, "fsk_pred_language"), "Language", value = "English"),
    #                              tags$h4("Creators"),
    #                              rHandsontableOutput(NS(id, "fsk_pred_creators")),
    #                              tags$h4("Authors"),
    #                              rHandsontableOutput(NS(id, "fsk_pred_authors")),
    #                              tags$h4("Reference"),
    #                              rHandsontableOutput(NS(id, "fsk_pred_reference"))
    #                     ),
    #                     tabPanel("Scope",
    #                              textInput(NS(id, "fsk_pred_genCom"), "General comment"),
    #                              tags$h4("Product/Matrix"),
    #                              rHandsontableOutput(NS(id, "fsk_pred_product")),
    #                              tags$h4("Hazard"),
    #                              rHandsontableOutput(NS(id, "fsk_pred_hazard"))
    #                     ),
    #                     tabPanel("Data background",
    #                              textInput(NS(id, "fsk_pred_studyTitle"), "Study title")
    #                     ),
    #                     tabPanel("Model math",
    #                              rHandsontableOutput(NS(id, "fsk_pred_model"))
    #                     ),
    #                     tabPanel("Export",
    #                              downloadButton(NS(id, "fsk_pred_download"), "Download")
    #                     )
    #          )
    #   )
    # )
  )

}

## Server ----------------------------------------------------------------------

primary_module_server <- function(id) {

  moduleServer(id, function(input, output, session) {

    ## FSK2R -------------------------------------------------------------------

    # ## General info
    #
    # output$fsk_pred_creators <- renderRHandsontable({
    #   # rhandsontable(data.frame(Email = c("google@chucknorris.com", NA), `Family name` = c("Doe", NA), `Given Name` = c("Jon", NA)),
    #   #               rowHeaders = NULL, readOnly = FALSE
    #   #               )
    #
    #   default_data <- data.frame(
    #     title = "",
    #     familyName = "",
    #     givenName = "",
    #     email = "",
    #     telephone = "",
    #     streetAdress = "",
    #     country = "",
    #     city = "",
    #     region = "",
    #     organization = "",
    #     stringsAsFactors = FALSE
    #   )
    #
    #   if (!is.null(input$fsk_pred_creators)) {
    #     DF = hot_to_r(input$fsk_pred_creators)
    #   } else {
    #     DF = default_data
    #   }
    #
    #   DF %>%
    #     rhandsontable() %>%
    #     hot_table(highlightCol = TRUE, highlightRow = TRUE)
    # })
    #
    # output$fsk_pred_authors <- renderRHandsontable({
    #
    #   default_data <- data.frame(
    #     title = "",
    #     familyName = "",
    #     givenName = "",
    #     email = "",
    #     telephone = "",
    #     streetAdress = "",
    #     country = "",
    #     city = "",
    #     region = "",
    #     organization = "",
    #     stringsAsFactors = FALSE
    #   )
    #
    #   if (!is.null(input$fsk_pred_authors)) {
    #     DF = hot_to_r(input$fsk_pred_authors)
    #   } else {
    #     DF = default_data
    #   }
    #
    #   DF %>%
    #     rhandsontable() %>%
    #     hot_table(highlightCol = TRUE, highlightRow = TRUE)
    # })
    #
    # output$fsk_pred_reference <- renderRHandsontable({
    #
    #   default_data <- data.frame(
    #     publicationType = "",
    #     publicationDate = "",
    #     doi = "",
    #     authorList = "",
    #     publicationTitle = "",
    #     publicationAbstract = "",
    #     publicationStatus = "",
    #     publicationWebsite = "",
    #     comment = "",
    #     stringsAsFactors = FALSE
    #   )
    #
    #   if (!is.null(input$fsk_pred_reference)) {
    #     DF = hot_to_r(input$fsk_pred_reference)
    #   } else {
    #     DF = default_data
    #   }
    #
    #   DF %>%
    #     rhandsontable() %>%
    #     hot_table(highlightCol = TRUE, highlightRow = TRUE)
    # })
    #
    # ## Scope
    #
    # output$fsk_pred_product <- renderRHandsontable({
    #
    #   default_data <- data.frame(
    #     productName = "",
    #     productDescription = "",
    #     productUnit = "",
    #     productionMethod = "",
    #     packaging = "",
    #     productTreatment = "",
    #     originCountry = "",
    #     originArea = "",
    #     fisheriesArea = "",
    #     productionDate = "",
    #     expiryDate = ""
    #   )
    #
    #   if (!is.null(input$fsk_pred_product)) {
    #     DF = hot_to_r(input$fsk_pred_product)
    #   } else {
    #     DF = default_data
    #   }
    #
    #   DF %>%
    #     rhandsontable() %>%
    #     hot_table(highlightCol = TRUE, highlightRow = TRUE)
    # })
    #
    #
    # output$fsk_pred_hazard <- renderRHandsontable({
    #
    #   default_data <- data.frame(
    #     hazardType = "",
    #     hazardName = "",
    #     hazardDescription = "",
    #     hazardUnit = "",
    #     adverseEffect = "",
    #     sourceOfContamination = "",
    #     maximumResidueLimit = "",
    #     noObservedAdverseAffectLevel = "",
    #     lowestObservedAdverseAffectLevel = "",
    #     acceptableOperatorExposureLevel = "",
    #     acuteReferenceDose = "",
    #     acceptableDailyIntake = ""
    #   )
    #
    #   if (!is.null(input$fsk_pred_hazard)) {
    #     DF = hot_to_r(input$fsk_pred_hazard)
    #   } else {
    #     DF = default_data
    #   }
    #
    #   DF %>%
    #     rhandsontable() %>%
    #     hot_table(highlightCol = TRUE, highlightRow = TRUE)
    # })
    #
    # ## Model math
    #
    # par_description_map <- tribble(
    #   ~parameter, ~description,
    #   "D",  "Treatment time required for one log-reduction",
    #   "logN0", "log-initial microbial count",
    #   "delta", "Treatment time required for the first log-reduction",
    #   "p", "Shape factor of the Weibull distribution",
    #   "b", "Rate parameter of the Peleg model",
    #   "n", "Shape factor of the Weibull distribution",
    #   "SL", "Shoulder length",
    #   "logNres", "Tail height"
    #
    # )
    #
    # output$fsk_pred_model <- renderRHandsontable({
    #
    #   ## Get the known pars
    #
    #   par_names <- model_map[[input$model]]
    #
    #   known <- list()
    #
    #   for (i in par_names) {
    #
    #
    #     if (isTRUE(input[[paste0(i, "_fixed")]])) {
    #       known[[i]] <- input[[i]]
    #     }
    #
    #   }
    #
    #   known <- unlist(known)
    #
    #   ## Get the fitted parameters
    #
    #   aa <- summary(my_fit())$par %>%
    #     as_tibble(rownames = "Parameter") %>%
    #     select(Parameter, Estimate)
    #
    #   out <- aa$Estimate
    #   names(out) <- aa$Parameter
    #
    #   ## Put them together with the map
    #
    #   out <- data.frame(parameter = names(c(known, out)),
    #                     estimate = c(known, out),
    #                     unit = "",
    #                     `Min value` = 0,
    #                     `Max value` = "") %>%
    #     left_join(., par_description_map, by = "parameter")
    #
    #   rhandsontable(out,
    #                 rowHeaders = NULL, readOnly = FALSE
    #   )
    # })
    #
    # ## Download
    #
    # output$fsk_pred_download <- downloadHandler(
    #   filename = "isofit_model.fskx",
    #   content = function(file) {
    #
    #     ## Import the 'basic' model
    #
    #     my_fsk <- import_fsk("fskBioinactivation.fskx")
    #
    #     ## Metadata - general informatino
    #
    #     my_fsk$metadata$generalInformation$name <- input$fsk_pred_name
    #     my_fsk$metadata$generalInformation$source <- input$fsk_pred_source
    #     my_fsk$metadata$generalInformation$identifier <- input$fsk_pred_identifier
    #     my_fsk$metadata$generalInformation$creationDate <- input$fsk_pred_date
    #     my_fsk$metadata$generalInformation$rights <- input$fsk_pred_rights
    #     my_fsk$metadata$generalInformation$language <- input$fsk_pred_language
    #
    #     creators <- hot_to_r(input$fsk_pred_creators) %>%
    #       mutate(eClass = "http://BfR/bund/de/knime/model/metadata_V1.0.3#//Contact")
    #
    #     authors <- hot_to_r(input$fsk_pred_authors) %>%
    #       mutate(eClass = "http://BfR/bund/de/knime/model/metadata_V1.0.3#//Contact")
    #
    #     refs <- hot_to_r(input$fsk_pred_reference) %>%
    #       mutate(eClass = "http://BfR/bund/de/knime/model/metadata_V1.0.3#//Reference",
    #              isReferenceDescription = TRUE
    #       )
    #
    #     my_fsk$metadata$generalInformation$creators <- apply(creators, 1, as.list)
    #     my_fsk$metadata$generalInformation$author <- apply(authors, 1, as.list)
    #     my_fsk$metadata$generalInformation$reference <- apply(refs, 1, as.list)
    #
    #     ## Metadata - scope
    #
    #     my_fsk$metadata$scope$generalComment <- input$fsk_pred_genCom
    #
    #
    #     prods <- hot_to_r(input$fsk_pred_product) %>%
    #       mutate(eClass = "http://BfR/bund/de/knime/model/metadata_V1.0.3#//Product")
    #
    #     my_fsk$metadata$scope$product <- apply(prods, 1, as.list)
    #
    #     haz <- hot_to_r(input$fsk_pred_hazard) %>%
    #       mutate(eClass = "http://BfR/bund/de/knime/model/metadata_V1.0.3#//Hazard")
    #
    #     my_fsk$metadata$scope$hazard <- apply(haz, 1, as.list)
    #
    #     ## Metadata - data background
    #
    #     my_fsk$metadata$dataBackground$study <- input$fsk_pred_studyTitle
    #
    #     ## Metadata - model parameters
    #
    #     model_name <- paste0("'", input$model, "'")
    #     max_time <- max(my_data()$time)
    #
    #     times <- paste0("seq(0, ", max_time, ", length=100)")
    #
    #     # my_temperature <- as.data.frame(pred_temp_profile()) %>%
    #     #   filter(!is.na(temperature))
    #     #
    #     # time_points <- my_temperature$time
    #     # temp_points <- my_temperature$temperature
    #
    #     # time_points <- paste(time_points, collapse = ",")
    #     # time_points <- paste0("c(", time_points, ")")
    #     # temp_points <- paste(temp_points, collapse = ",")
    #     # temp_points <- paste0("c(", temp_points, ")")
    #
    #     par_table <- hot_to_r(input$fsk_pred_model) %>%
    #       rename(parameterID = parameter,
    #              parameterUnit = unit,
    #              parameterDescription = description,
    #              parameterValueMin = Min.value,
    #              parameterValueMax = Max.value,
    #              parameterValue = estimate
    #       ) %>%
    #       mutate(eClass = "http://BfR/bund/de/knime/model/metadata_V1.0.3#//Parameter",
    #              parameterName = parameterID,
    #              parameterClassification = "Input",
    #              parameterUnitCategory = "NA",
    #              parameterDataType = "Double",
    #              parameterSource = "NA",
    #              parameterSubject = "NA",
    #              parameterDistribution = "Constant",
    #              parameterVariabilitySubject = "NA",
    #              parameterError = "NA"
    #       )
    #
    #     other_pars <- tibble(parameterID = c("model_name", "max_time"),
    #                          parameterName = c("model_name", "max_time"),
    #                          parameterDescription = c("Inactivation model", "Maximum time for the simulation"),
    #                          parameterValue = c(model_name, max_time),
    #                          eClass ="http://BfR/bund/de/knime/model/metadata_V1.0.3#//Parameter",
    #                          parameterUnit = "",
    #                          parameterValueMin = "",
    #                          parameterValueMax = "",
    #                          parameterClassification = "Input",
    #                          parameterUnitCategory = "NA",
    #                          parameterDataType = c("Character", "Double"),
    #                          parameterSource = "NA",
    #                          parameterSubject = "NA",
    #                          parameterDistribution = "Constant",
    #                          parameterVariabilitySubject = "NA",
    #                          parameterError = "NA")
    #
    #     list_par_table <- apply(par_table, 1, as.list)
    #     list_par_other <- apply(other_pars, 1, as.list)
    #
    #     my_fsk$metadata$modelMath$parameter <- c(list_par_table, list_par_other)
    #
    #     ## Define the simulation
    #
    #     par_sims <- lapply(1:nrow(par_table), function(i) {
    #
    #       new_elem <- list()
    #       attr(new_elem, "newValue") <- par_table$parameterValue[i]
    #       attr(new_elem, "target") <- par_table$parameterID[i]
    #       new_elem
    #
    #     })
    #
    #     other_sims <- lapply(1:nrow(other_pars), function(i) {
    #
    #       new_elem <- list()
    #       attr(new_elem, "newValue") <- other_pars$parameterValue[i]
    #       attr(new_elem, "target") <- other_pars$parameterID[i]
    #       new_elem
    #
    #     })
    #
    #     my_sims <- c(par_sims, other_sims)
    #
    #     my_sims <- set_names(my_sims, rep("changeAttribute", length(my_sims)))
    #
    #     # print(my_sims)
    #
    #     my_fsk$simulation$sedML$listOfModels$model$listOfChanges <- my_sims
    #
    #     ## Export the model
    #
    #     export_fsk(my_fsk, file)
    #
    #
    #     # file.copy("ToyModelv4.fskx", file)
    #   }
    # )



    ## Download predictions ----------------------------------------------------

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

        tibble(time = seq(0, max(my_data()$time, na.rm = TRUE), length = 1000),
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
      "SL", "Shoulder length (min)", 1, FALSE,
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
        ),
        column(6,
               awesomeCheckbox(NS(id, paste0(par_name, "_fixed")), "Fixed?", par_data$fixed)
        )
      )

    }

    output$parameters <- renderUI({

      # browser()

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

      out <- modFit(get_residuals, p, data = na.omit(my_data()), model = input$model, known = known)
      my_fit(out)
    })

    observeEvent(input$model, {
      my_fit(NULL)
    })

    ## Output ------------------------------------------------------------------

    output$plot_fit <- renderPlotly({

      validate(
        need(my_fit(), "")
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

      p <- my_data() %>%
        # mutate(pred = prediction_map[[input$model]](time, my_fit()$par)) %>%
        ggplot() +
        geom_point(aes(x=time, y = logN),
                   colour = input$pointcol,
                   size = input$pointsize,
                   shape = input$pointshape) +
        geom_line(aes(x, y), colour = input$linecol,
                  size = input$linesize,
                  linetype = input$linetype,
                  data = tibble(x = seq(0, max(my_data()$time, na.rm = TRUE), length = 1000),
                                y = prediction_map[[input$model]](c(my_fit()$par, known), x))
                  ) +
        xlab(input$xlabel) + ylab(input$ylabel)
        # geom_line(aes(x = time, y = pred))

      ggplotly(p)

    })

    output$par_summary <- renderPrint({

      validate(
        need(my_fit(), "")
      )

      summary(my_fit())

    })

   output$par_table <- renderTable({

     validate(
       need(my_fit(), "")
     )

     summary(my_fit())$par %>%
       as_tibble(rownames = "Parameter") %>%
       select(-`t value`, -`Pr(>|t|)`) %>%
       mutate(
         `CI 95% left` = Estimate - 1.96*`Std. Error`,
         `CI 95% right` = Estimate + 1.96*`Std. Error`
       )

   })

   output$residual_plot <- renderPlot({

     validate(
       need(my_fit(), "")
     )

     my_data() %>%
       na.omit() %>%
       mutate(res = residuals(my_fit())) %>%
       ggplot() +
       geom_point(aes(x = time, y = res)) +
       xlab("Treatment time (min)") +
       ylab("Residual (log CFU/g)") +
       geom_hline(yintercept = 0, linetype = 2)

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




