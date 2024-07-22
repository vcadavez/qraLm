#' @title Cantaloupe cross-contamination during washing in flume tank
#'
#' @description
#' The function [caFlumeTankCC()] function simulates the potential contamination of cantaloupe, when in direct
#' contact with contaminated water in flume tank.
#' The cross-contamination algorithm accounts for four possible scenarios:
#' \enumerate{
#'    \item cross-contamination occurring in lots already contaminated;
#'    \item re-contamination occurring in lots that were not contaminated;
#'    \item no cross-contamination occurring in lots already contaminated; and
#'    \item no cross-contamination occurring in lots that were not contaminated.
#'    }
#'
#' @param data a list of:
#'     \describe{
#'               \item{`N`}{(`CFU`) A matrix of size `nLots` lots by `sizeLot` units representing the numbers of \emph{L. monocytogenes}
#'               on the rind, from contaminated cultivation lots;}
#'               \item{`P`}{Prevalence of contaminated harvested lots pre-washing (scalar).}
#'             }
#' @param nLots see [Lot2LotGen()] function.
#' @param sizeLot see [Lot2LotGen()] function.
#' @param cantaWeight (`g`) weight of a cantaloupe.
#' @param probCCW Probability that water of flume tank is contaminated  (scalar).
#' @param logWaterMin (log10(cfu/l)) Minimal concentration of \emph{L. monocytogenes} in water of flume tank  (scalar or vector). The value was derived during JEMRA meeting related to unpublished data transmitted by experts
#' @param logWaterMode (log10(cfu/l))  Mode concentration of \emph{L. monocytogenes} in water of flume tank  (scalar or vector). The value was derived during JEMRA meeting related to unpublished data transmitted by experts
#' @param logWaterMax (log10(cfu/l)) Maximal concentration of \emph{L. monocytogenes} in water of flume tank  (scalar or vector). The value was derived during JEMRA meeting related to unpublished data transmitted by experts
#' @param pWaterGain value of the fraction of water gain (ml) relative to the cantaloupe weight in g (\eqn{default=0.004} according to \insertCite{Richards2004;textual}{qraLm}).
#' @param bWater Dispersion factor representing the clustering of cells during flume_tank washing (scalar).
#'
#' @return
#' A list of two elements of the data objects:
#'     \describe{
#'              \item{`N`}{(`CFU`) A matrix of size `nLots` lots by `sizeLot` units representing the numbers of \emph{L. monocytogenes}
#'              on washed cantaloupes.}
#'              \item{`P`}{Prevalence of contaminated harvested lots post-washing (scalar).}
#'              }
#'
#' @author Who is the author
#'
#' @keywords water immersion cross-contamination
#'
#' @references
#' \insertRef{mc2d}{qraLm}
#' \insertRef{stats}{qraLm}
#'
#'
#' @importFrom mc2d rmultinomial rdirichlet
#' @importFrom stats rbinom rmultinom
#' @importFrom utils data
#'
#' @export
#'
#' @note  Write a note!
#'
#' @examples
#' dat <- caPrimaryProduction(
#'   nLots = 100,
#'     sizeLot = 100)
#'     Nf <- caFlumeTankCC(dat,
#'                    probCCW = 0.125,
#'                    logWaterMin = 1,
#'                    logWaterMode = 1,
#'                    logWaterMax = 5,
#'                    bWater = 1)
#' hist(Nf$N)
#'
caFlumeTankCC <- function(data = list(),
                          nLots = NULL,
                          sizeLot = NULL,
                          cantaWeight = NULL,
                          probCCW = 0.5,
                          logWaterMin = 1,
                          logWaterMode = 1,
                          logWaterMax = 5,
                          pWaterGain = 0.004,
                          bWater = 1) {
  #  nLots <- nrow(data$N)
  #  sizeLot <- ncol(data$N)

  if (missing(nLots)) nLots <- data$nLots # test if nLots was defined
  if (is.null(nLots)) warning("Add 'nLots=#' to function arguments") # test again if nLots is defined

  if (missing(sizeLot)) sizeLot <- data$sizeLot # test if sizeLot was defined
  if (is.null(sizeLot)) warning("Add 'sizeLot=#' to function arguments") # test again if sizeLot is defined

  if (missing(cantaWeight)) cantaWeight <- data$cantaWeight # test if cantaWeight was defined
  if (is.null(cantaWeight)) warning("Add 'cantaWeight=#' to function arguments") # test again if cantaWeight is defined

  Nt_batch <- rowSums(data$N)
  # Error in case a non-contaminated lot is present in the matrix
  if (any(Nt_batch == 0)) stop("There should not be any non-contaminated lot before flume tank")

  # calculated probabilities
  p_FieldPos_CCNeg <- data$P * (1 - probCCW)
  p_FieldPos_CCPos <- data$P * probCCW
  p_FieldNeg_CCPos <- (1 - data$P) * probCCW

  n_Field <- rep(0, nLots)
  # Add random
  # 1 is p_FieldPos_CCNeg, 2 is p_FieldPos_CCPos, 3 is p_FieldNeg_CCPos
  matProb <- c(p_FieldPos_CCNeg, p_FieldPos_CCPos, p_FieldNeg_CCPos)
  n_Field <- sample(1:3, nLots, replace = TRUE, prob = matProb)

  # Level of cross-contamination
  C_water <- 10^mc2d::rpert(nLots, min = logWaterMin, mode = logWaterMode, max = logWaterMax, shape = 4)
  N_added <- pmax(C_water * pWaterGain * cantaWeight, 1)

  Nt_batchPostCC <- rep(0, length(Nt_batch))
  # Previously contaminated: take the previous values
  previously <- n_Field %in% c(1, 2)
  Nt_batchPostCC[previously] <- Nt_batch[previously]
  # New contamination: add to 0 or the previous values the cross contamination
  newly <- n_Field %in% c(2, 3)
  Nt_batchPostCC[newly] <- Nt_batch[newly] + N_added[newly]

  # Defining the partition function to generate cells in a pack from the bulk

  if (length(bWater == 1)) {
    p <- mc2d::rdirichlet(nLots, rep(bWater, sizeLot))
    # mc2d multinomial because p is a matrix
    N_out <- mc2d::rmultinomial(n = nLots, size = Nt_batchPostCC, prob = p)
  } else {
    (stop("Vector of bCCw non implemented yet."))
  }

  N <- N_out
  P <- 1 - (1 - data$P) * (1 - probCCW)
  
  lotMeans <- rowMeans(N / data$cantaWeight, na.rm = TRUE)
  unitsCounts <- c(N / data$cantaWeight)
  data$lotMeans <- lotMeans
  data$unitsCounts <- unitsCounts
  data$N <- N
  data$P <- P
  
  return(data)
}
