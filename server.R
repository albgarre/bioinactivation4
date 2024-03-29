

server <- function(input, output) {

  ## Modules ------------------------------------------------------------------<

  isopred_module_server("module_isopred")
  dynapred_module_server("module_dynapred")
  stocpred_module_server("module_stocpred")
  fit2step_module_server("module_2step")
  fit1step_module_server("module_1step")
  dynafit_module_server("module_dynafit")
  welcome_module_server("module_welcome")
  primary_module_server("module_primary")
  code_module_server("other")
}
