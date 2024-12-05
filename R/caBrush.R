#' @title Cleaning cantaloupes by brushing
#'
#' @description
#' The function [caBrush()] models the removal of bacteria during the brushing or scrubbing step.
#' The mean log10 reduction due to brushing must be provided.
#'
#' @param data a list of:
#' \describe{
#'            \item{`N`}{(`CFU`) A matrix of size `nLots` lots by `sizeLot` units representing the numbers of \emph{L. monocytogenes} on the rind,
#'            from contaminated cultivation lots;}
#'            \item{`P`}{Prevalence of contaminated harvested lots (scalar).}
#'          }
#' @param nLots see [Lot2LotGen()] function.
#' @param sizeLot see [Lot2LotGen()] function.
#' @param logDecBrush Mean log10 reduction attained by brushing or scrubbing (scalar, vector of size the number of `N` rows
#'  (representing lot-to-lot variability), or vector of size the number of `N` columns (representing element-to-element variability)).
#' @return
#'  A list of two elements of the data objects:
#'     \describe{
#'              \item{`N`}{(`CFU`) A matrix of size `nLots` lots by `sizeLot` units containing the numbers of \emph{L. monocytogenes}
#'              on brushed cantaloupes}
#'              \item{`P`}{Prevalence of contaminated harvested lots post-brushing (scalar).}
#'              }
#'
#' @author Regis Pouillot \email{rpouillot.work@gmail.com}
#'
#' @keywords melon mechanical removal scrubbing
#'
#' @references
#' \insertRef{mc2d}{qraLm}
#'
#' \insertRef{stats}{qraLm}
#'
#' \insertRef{Fu2020}{qraLm}
#'
#' @importFrom stats rbinom
#'
#' @export
#'
#' @note
#' Abrasive brushing for cantaloupe cleaning has been shown to be efficient in removing biofilms of \emph{L. monocytogenes}
#' attached to the rind in \insertCite{Fu2020;textual}{qraLm}.
#'
#' @examples
#' sizeLot <- 100
#' nLots <- 500
#' df <- list(N=matrix(rpois(sizeLot * nLots, 10),
#'              nrow = nLots, ncol = sizeLot),
#'               P=0.7, cantaWeight=1000, sizeLot=100,
#'               nLots=500)
#' brushed <- caBrush(df,
#'                    logDecBrush = runif(nLots, min = 0, max = 10)
#'                 )
#' hist(brushed$N)
#'                 
caBrush <- function(data = list(),
                    nLots = NULL,
                    sizeLot = NULL,
                    logDecBrush) {
  if (missing(nLots)) nLots <- data$nLots # test if nLots was defined
  if (is.null(nLots)) warning("Add 'nLots=#' to function arguments") # test again if nLots is defined

  if (missing(sizeLot)) sizeLot <- data$sizeLot # test if sizeLot was defined
  if (is.null(sizeLot)) warning("Add 'sizeLot=#' to function arguments") # test again if sizeLot is defined

  #  nLots  <- nrow(data$N)
  #  sizeLot <- ncol(data$N)

  if (!length(logDecBrush) %in% c(1, nLots, nLots * sizeLot)) stop("logDecBrush not conformable size in caBrush")

  # Wash the cantaloupes
  pNotBrush <- 10^c(-logDecBrush)
  # Those stay o, the cantaloupe
  NNotBrush <- matrix(stats::rbinom(
                                    n = nLots * sizeLot,
                                    size = data$N,
                                    prob = pNotBrush
                                    ),
                      ncol = sizeLot, 
                      nrow = nLots)

  # In case of 0 (per lot)
  # Commented out to keep lot
  # zeroes <- rowSums(NNotBrush) == 0
  # if(all(zeroes)) {
  #   NNotBrush[1,] <- 1
  #   warning("No more bacteria in the system following brushing. 1 bacteria set for each lot")
  # } else if(any(zeroes)) {
  #   NNotBrush[zeroes,] <- NNotBrush[sample(which(!zeroes),sum(zeroes), replace=TRUE),]
  # }

  N <- NNotBrush
  lotMeans <- rowMeans(N / data$cantaWeight, na.rm = TRUE)
  unitsCounts <- c(N / data$cantaWeight)
  data$lotMeans <- lotMeans
  data$unitsCounts <- unitsCounts
  data$N <- N

  # Adjust prevalence (all brushed)
  #  At least one not brushed
  # Commented out to keep lot
  # atLeastOneSurvive <- 1 - apply((1-c(pNotBrush))^data$N, 1, prod)
  # data$P <- data$P * mean(atLeastOneSurvive)
  return(data)
}
