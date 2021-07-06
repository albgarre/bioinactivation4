

server <- function(input, output) {

  ## Modules ------------------------------------------------------------------<

  isopred_module_server("module_isopred")
  dynapred_module_server("module_dynapred")
  stocpred_module_server("module_stocpred")
  fit2step_module_server("module_fit2step")
  fit1step_module_server("module_fit1step")
  dynafit_module_server("module_dynafit")
  welcome_module_server("module_welcome")

}
