#' @title Generation of contaminated lots
#'
#' @description
#' The function [LotGen()] generates a matrix of contaminated lots from information on the 
#' parameters of a normal distribution (mean, sd)
#' representing the microbial concentration in the contaminated units (log10 CFU/g).
#' Every row of the matrix corresponds to a production lot, broken down in units or portions,
#'  which are the columns of the matrix.
#' There is no assumption that lots can have different prevalence values, and therefore the 
#' microbial numbers of all units or portions produced in a lot
#' are sampled from a microbial concentration distribution.
#' @param nLots number of lots sampled or size of the Monte Carlo simulation (scalar).
#' @param sizeLot number of units or portions produced in a lot (scalar).
#' @param unitSize (`g`) weight of single unit or portion from a lot (scalar).
#' @param P prevalence of contaminated lots (scalar)
#' @param C0MeanLog (log10 CFU/g) mean parameter of the normal distribution representing
#'  the variability in the microbial concentration of contaminated units (scalar or vector).
#' @param C0SdLog (log10 CFU/g) standard deviation parameter of the normal distribution
#'  representing the variability in the microbial concentration of contaminated units (scalar or vector).
#' @param ... Other options used to control [LotGen()]
#' @return A list of two elements:
#'     \describe{
#'              \item{N}{(`CFU`) A matrix of size `nLots` lots by `sizeLot` units representing the microbial numbers in the units or portions from contaminated lots;}
#'              \item{P}{Prevalence of contaminated lots (scalar).}
#'              }
#' @author Vasco Cadavez \email{vcadavez@ipb.pt}
#'
#' @keywords concentration numbers contamination batches
#'
#' @family data generation
#'
#' @references
#' \insertRef{stats}{qraLm}
#'
#' \insertRef{FSAI2022}{qraLm}
#'
#' \insertRef{Jeya2011}{qraLm}
#'
#' \insertRef{Magdovitz2021}{qraLm}
#'
#' \insertRef{Kuan2017}{qraLm}
#'
#' @importFrom stats rnorm
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @note For the prevalence of lots contaminated with \emph{L. monocytogenes}, \eqn{P=0.057} is suggested from the information
#' published in \insertCite{FSAI2022;textual}{qraLm}, which detected \emph{L. monocytogenes} in 21 out of 366 in non-RTE frozen vegetables.
#' The mean (\eqn{C0\_meanLog10=1.023\ log10\ CFU/g}) and standard deviation (\eqn{C0\_sdLog10=0.3267\ log10\ CFU/g}) of the normal distribution
#' representing the variability in \emph{L. monocytogenes} were obtained using data from \insertCite{Jeya2011;textual}{qraLm}
#' (\eqn{3.0\ MPN/g}, \eqn{3.6\ MPN/g} in freshly harvested vegetables), \insertCite{Magdovitz2021;textual}{qraLm} (21, 36 and 75 MPN/g in corn and peas
#' arriving at the frozen food facility), and \insertCite{Kuan2017;textual}{qraLm} (\eqn{3.0\ MPN/g} in bulk carrots at retail).
#'
#' @examples
#' lots <- LotGen(
#'                nLots = 500,
#'                sizeLot = 200,
#'                P = 0.057,
#'                C0MeanLog = 1.023,
#'                C0SdLog = 0.3267,
#'                unitSize = 500
#'                )
#' str(lots)
#' summary(c(lots$N))
LotGen <- function(nLots,
                   sizeLot,
                   P,
                   C0MeanLog,
                   C0SdLog,
                   unitSize,
                   ...) {
    LotGenParameters <- c(as.list(environment()), list(...))
  
  N0 <- matrix(
    10^stats::rnorm(
                    nLots * sizeLot,
                     C0MeanLog, C0SdLog),
    nrow = nLots, 
    ncol = sizeLot
    )
  N <- round(N0 * unitSize, digits = 0)

 data <- list(
              N = N,
              P = P,
              LotGenParameters = LotGenParameters)

  # Set the name for the class
  class(data) <- append(class(data), "qraLm")
  return(data)
}
