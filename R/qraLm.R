#' @title qraLm: An R package for the quantitative risk assessment of Listeria monocytogenes in foods
#'
#' @description
#' Functions for the quantitative risk assessment of Listeria monocytogenes in foods
#'
#' @section qraLm functions:
#' The qraLm functions ...
#' 
#' @docType package
#'
#' @name qraLm
#' @aliases qraLm qraLm-package
#'
#' @useDynLib qraLm
#' @importFrom Rcpp sourceCpp
#' @export expandedjamesonC_set_params
#' @export expandedjamesonC
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))