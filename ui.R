
library(shiny)
library(bs4Dash)
library(tidyverse)
library(fresh)

library(thematic)
thematic_shiny()

## Source the modules

list.files("./R") %>%
  map(., ~ paste0("./R/", .)) %>%
  map(source)

##

ui <- bs4DashPage(
  freshTheme = use_theme(create_theme(
    theme = "united",
    NULL
  )),
  header = dashboardHeader(title = dashboardBrand(title = "bioinactivation4")),
  footer = dashboardFooter(
    left = a(
      href = "https://github.com/albgarre/bioinactivation4",
      target = "_blank", "@AlbertoGarre"
    ),
    right = "beta version - August 2021"
  ),
  sidebar = dashboardSidebar(
    # sidebarUserPanel(
    #   image = "https://image.flaticon.com/icons/svg/1149/1149168.svg",
    #   name = "Welcome Onboard!"
    # ),

    sidebarMenu(
      id = "welcome_menu",
      # sidebarHeader(""),
      menuItem(
        "Welcome",
        tabName = "welcome_tab",
        icon = icon("igloo")
      )
    ),

    sidebarMenu(
      id = "pred_menu",
      sidebarHeader("Predictions"),
      menuItem(
        "Isothermal",
        tabName = "pred_iso",
        icon = icon("vial")
      ),
      menuItem(
        "Dynamic",
        tabName = "pred_dyna",
        icon = icon("plane")
      ),
      menuItem(
        "Stochastic",
        tabName = "pred_stoc",
        icon = icon("dice")
      )
    ),

    sidebarMenu(
      id = "fit_menu",
      sidebarHeader("Model fitting"),
      menuItem(
        "Primary model",
        tabName = "fit_primary",
        icon = icon("circle")
      ),
      menuItem(
        "Two step",
        tabName = "fit_secondary",
        icon = icon("shoe-prints")
      ),
      menuItem(
        "One step",
        tabName = "fit_1step",
        icon = icon("hat-wizard")
      ),
      menuItem(
        "Dynamic",
        tabName = "fit_dyna",
        icon = icon("plane-slash")
      )
    ),

    sidebarMenu(
      id = "doc_menu",
      sidebarHeader("Documentation"),
      menuItem(
        "Manual",
        tabName = "manual",
        icon = icon("book")
      ),
      menuItem(
        "About",
        tabName = "other_page",
        icon = icon("microchip")
      )
    ),



  ),
  body = dashboardBody(

    tabItems(
      tabItem(
        tabName = "pred_iso",
        isopred_module_ui("module_isopred")
      ),
      tabItem(
        tabName = "pred_dyna",
        dynapred_module_ui("module_dynapred")
      ),
      tabItem(
        tabName = "pred_stoc",
        stocpred_module_ui("module_stocpred")
      ),
      tabItem(
        tabName = "fit_primary",
        primary_module_ui("module_primary")
      ),
      tabItem(
        tabName = "fit_secondary",
        fit2step_module_ui("module_2step")
      ),
      tabItem(
        tabName = "fit_1step",
        fit1step_module_ui("module_1step")
      ),
      tabItem(
        tabName = "fit_dyna",
        dynafit_module_ui("module_dynafit")
      ),
      tabItem(
        tabName = "welcome_tab",
        # welcome_module_ui("module_welcome")
        fluidRow(
          column(12,
                 bs4Jumbotron(
                   status = "warning",
                   title = "bioinactivation",
                   lead = h4("Now with dark mode!"),
                   btnName = NULL
                 )
          )
        ),
        fluidRow(
          column(6,
                 carousel(
                   id = "mycarousel",
                   carouselItem(
                     caption = "Isothermal predictions",
                     tags$img(src = "pic_isopred.png")
                   ),
                   carouselItem(
                     caption = "Dynamic fitting",
                     tags$img(src = "fit_dynamic.png")
                   ),
                   carouselItem(
                     caption = "Dynamic predictions",
                     tags$img(src = "dynamic_predictions.png")
                   ),
                   carouselItem(
                     caption = "Fit primary model",
                     tags$img(src = "fit_primary.png")
                   ),
                   carouselItem(
                     caption = "One-step fitting",
                     tags$img(src = "fit_secondary.png")
                   ),
                   carouselItem(
                     caption = "Stochastic predictions",
                     tags$img(src = "stoc_pred.png")
                   )
                 )
          ),
          column(6,
                 bs4Card(title = "",
                         width = 12,
                         gradient = TRUE,
                         background = "primary",
                         collapsible = FALSE,
                         p("Bioinactivation was developed within the Technical University of Cartagena (Spain) with the aim to ease modelling of microbial inactivation."),
                         p("It includes functions for fitting inactivation models commonly used in predictive microbiology to data gathered under static or dynamic conditions."),
                         p("It can also be used to make predictions under static or dynamic conditions."),
                         p("Do not hesitate to contact us if you have any comment!")
                 )
          )
        )
      ),
      tabItem(
        tabName = "other_page",
        code_module_ui("other")
      )
    )

  )
)

















