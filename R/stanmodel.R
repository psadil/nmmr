
#' get_stanmodel
#'
#' @param model_name chracter that is either "deming" or "vtf"
#'
#' @return compiled cmdstanmodel
#' @export
#'
#' @examples
#' get_stanmodel("deming")
#' get_stanmodel("vtf")
get_stanmodel <- function(model_name){
  checkmate::assert_choice(model_name, c("deming","vtf"))
  suppressMessages(

    model <- cmdstanr::cmdstan_model(
      file.path(system.file("stan", package = "nmmr"), fs::path(model_name, ext = "stan")),
      dir = system.file("exec", package = "nmmr")) )

  model

}

