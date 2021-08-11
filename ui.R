
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
    right = "2021"
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
        icon = icon("grip-lines")
      ),
      # menuItem(
      #   "Secondary model",
      #   tabName = "fit_secondary",
      #   icon = icon("sliders")
      # ),
      menuItem(
        "One step",
        tabName = "fit_1step",
        icon = icon("sliders")
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
        "GitHub page",
        tabName = "github",
        icon = icon("github")
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
        "Secondary model"
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
        welcome_module_ui("module_welcome")
      )
    )

  )
)

















