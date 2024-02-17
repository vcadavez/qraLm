#' @title Shiny application for `qraLm` package
#' 
#' @description
#' Shiny application to simulate quantitative risk assessment for *L. monocytogenes* 
#' in Frozen Vegetables, Diced RTE Cantaloupe and Cold-smoked RTE Fish.
#'
#' @author Vasco Cadavez \email{vcadavez@ipb.pt}
#'
#' @references
#' \insertRef{stats}{qraLm}
#'
#' @importFrom Rdpack reprompt
#'
#' @export
runExample <- function() {
  appDir <- system.file("shiny-examples", "qraLmShiny", package = "qraLm")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `qraLm`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}