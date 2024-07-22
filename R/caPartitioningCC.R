#' @title Cross-contamination of cantaloupe during dicing and partitioning into packed units
#'
#' @description
#' The [caPartitioningCC()] function simulates the potential cross-contamination of cantaloupes, when in direct contact with the dicing machine
#' or knives, followed by the partitioning of all dices produced in one processing lot (sublot) into packed units.
#' The cross-contamination algorithm accounts for four possible scenarios:
#' \enumerate{
#'    \item cross-contamination occurring in sublots already contaminated;
#'    \item contamination occurring in sublots that were not contaminated;
#'    \item no cross-contamination occurring in sublots already contaminated; and
#'    \item no cross-contamination occurring in sublots that were not contaminated. Probabilities of occurrence of every event are computed.
#'      }
#' The partitioning algorithm randomly distributes the total numbers of cells from a contaminated sublot of dices into packed units. The dispersion factor `b`,
#' which is a parameter of the beta distribution, indicates the extent of cell clustering in the bulk of dices in the sublot, and ultimately
#' the heterogeneity in the number of cells distributed among pack units.
#'
#' @param data a list of:
#' \describe{
#'    \item{`N`}{(`CFU`) A matrix of size `newMC` sublots by `sizeSublot` portions of dices from one cantaloupe representing the numbers of \emph{L. monocytogenes}
#'    in dices;}
#'    \item{`P`}{Prevalence of contaminated sublots (scalar).}
#'          }
#' @param probCCDice Probability of cross-contamination from the dicing machine (scalar).
#' @param trDicerMean Mean of the transfer coefficient of \emph{L. monocytogenes} from dicing machine to cantaloupe flesh (scalar or vector).
#' @param trDicerSd Standard deviation of the transfer coefficient of \emph{L. monocytogenes} from dicing machine to cantaloupe
#'  flesh (scalar or vector).
#' @param nDicer (`CFU`) Numbers of \emph{L. monocytogenes} on the surface of the dicing machine ready to be transferred (scalar or vector).
#' @param sizeLot Number of cantaloupes from a harvested lot (scalar).
#' @param sizeSublot Number of cantaloupes processed in a sublot. It should be a multiple of `sizeLot` (scalar).
#' @param cantaRindFree (`g`) Weight of a seedless rind-free cantaloupe (scalar).
#' @param unitSize (`g`) Weight of a pack of cantaloupe dices (scalar).
#' @param b Dispersion factor of the beta distribution representing the degree of heterogeneity in the number of cells between pack units (scalar).
#'
#' @return A list with four elements:
#' \describe{
#'       \item{`N`}{(`CFU`) A matrix of size `newMC` sublots by Number_packs number of packs containing the numbers of \emph{L. monocytogenes} in a pack,
#'            from contaminated lots;}
#'       \item{`P`}{Prevalence of contaminated sublots (scalar);}
#'       \item{`sizeSublot`}{Number of cantaloupes processed in a sublot (scalar);}
#'       \item{`sizeLot`}{Number of cantaloupes from a harvested lot (scalar).}
#'        }
#'
#' @author Ursula Gonzales-Barron \email{ubarron@ipb.pt}
#'
#' @keywords cross-contamination portioning
#'
#' @references
#' \insertRef{Hoelzer2012}{qraLm}
#' \insertRef{Nauta2005}{qraLm}
#' \insertRef{extraDistr}{qraLm}
#' \insertRef{mc2d}{qraLm}
#' \insertRef{Rdpack}{qraLm}
#' \insertRef{stats}{qraLm}
#' \insertRef{iRisk2021}{qraLm}
#'
#' @importFrom extraDistr rtnorm
#' @importFrom stats rbinom rmultinom
#' @importFrom mc2d rmultinomial rdirichlet
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @note The value of \eqn{beta = 1} represents moderate clustering of cells in the bulk of diced cantaloupe from a
#' sublot \insertCite{Nauta2005;textual}{qraLm}. \insertCite{Hoelzer2012;textual}{qraLm} established the log 10 of the transfer coefficient
#' of \emph{L. monocytogenes} from knives to vegetables as a normal distribution with \eqn{TRdicer\_mean = -1.42} and \eqn{TRdicer\_sd = 0.52}.
#'
#' @examples
#' library(extraDistr)
#' library(mc2d)
#' sizeLot <- 500 # 500 cantaloupes were harvested from a field
#' sizeSublot <- 50 # sublots of 50 cantaloupes are processed at each time
#' dat <- Lot2LotGen(
#'   nLots = 50,
#'   sizeLot = 100,
#'   unitSize = 500,
#'   betaAlpha = 0.5112,
#'   betaBeta = 9.959,
#'   C0MeanLog = 1.023,
#'   C0SdLog = 0.3267,
#'   propVarInter = 0.7
#' )
#' str(dat)
#' Packs <- caPartitioningCC(dat,
#'                           probCCDice = 0.08,
#'                           trDicerMean = -1.42,
#'                           trDicerSd = 0.52, 
#'                           nDicer = 280,
#'                           b = 1,
#'                           sizeSublot = 50,
#'                           cantaRindFree = 950, 
#'                           unitSize = 250
#'                           )
#' hist(Packs$N)
#' 
caPartitioningCC <- function(data = list(),
                             probCCDice,
                             trDicerMean = -1.42,
                             trDicerSd = 0.52,
                             nDicer,
                             b = 1,
                             unitSize,
                             cantaRindFree = NULL,
                             sizeSublot = NULL,
                             sizeLot = NULL) {
  
  if (missing(sizeLot)) sizeLot <- data$sizeLot # test if sizeLot was defined
  if (is.null(sizeLot)) warning("Add 'sizeLot=#' to function arguments") # test again if sizeLot is defined

  if (missing(sizeSublot)) sizeSublot <- data$sizeSublot # test if sizeSublot was defined
  if (is.null(sizeSublot)) warning("Add 'sizeSublot=#' to function arguments") # test again if sizeSublot is defined

  if (missing(cantaRindFree)) cantaRindFree <- data$cantaRindFree # test if cantaRindFree was defined
  if (is.null(cantaRindFree)) warning("Add 'cantaRindFree=#' to function arguments") # test again if cantaRindFree is defined

  newMC <- nrow(data$N)
  
  # Verifying the exact division of sizeLot/sizeSublot
  if (sizeLot %% sizeSublot != 0) stop("sizeSublot must be a multiple of sizeLot")
  # Verifying the original number of lots
  if ((newMC * sizeSublot) %% sizeLot != 0) stop("enter the original number of lots nLots")

  Nt_sublot <- rowSums(data$N)

  # Contaminated sublots in the input matrix
  # Bulk of non CC (= as input)
  BulkPos_CCNeg <- Nt_sublot
  # Bulk of CC
  p_TR1 <- 10^extraDistr::rtnorm(newMC, mean = trDicerMean, sd = trDicerSd, b = 0)
  N_TR1 <- stats::rbinom(newMC, nDicer, p_TR1)
  BulkPos_CCPos <- Nt_sublot + N_TR1
  # non-contaminated bulk fraction (batches added from "1-probCCDice")
  # New draw (actually unneeded, but fast)
  p_TR2 <- (10^extraDistr::rtnorm(newMC, mean = trDicerMean, sd = trDicerSd, b = 0))
  BulkNeg_CCPos <- stats::rbinom(newMC, nDicer, p_TR2)
  # calculated probabilities
  p_BulkPos_CCNeg <- data$P * (1 - probCCDice)
  p_BulkPos_CCPos <- data$P * probCCDice
  p_BulkNeg_CCPos <- (1 - data$P) * probCCDice
  # Add random
  nLots <- newMC * sizeSublot / sizeLot
  n_Bulk <- stats::rmultinom(n = 1, size = nLots, prob = c(p_BulkPos_CCNeg, p_BulkPos_CCPos, p_BulkNeg_CCPos))

  # arranging the sublots of N in a row to establish the lot
  # as a way of manipulation to resample correctly keeping the lot definition

  # re-sizing matrix of contaminated batches
  origin <- sample(c(
    rep("PosCCNeg", n_Bulk[1, 1]),
    rep("PosCCPos", n_Bulk[2, 1]),
    rep("NegCCPos", n_Bulk[3, 1])
  ))

  N_in_lot <- matrix(NA, nrow = nLots, ncol = sizeLot / sizeSublot)

  BulkPos_CCNeg_Lots <- matrix(BulkPos_CCNeg,
    ncol = sizeLot / sizeSublot,
    nrow = nLots, byrow = TRUE
  )
  BulkPos_CCPos_Lots <- matrix(BulkPos_CCPos,
    ncol = sizeLot / sizeSublot,
    nrow = nLots, byrow = TRUE
  )
  BulkNeg_CCPos_Lots <- matrix(BulkNeg_CCPos,
    ncol = sizeLot / sizeSublot,
    nrow = nLots, byrow = TRUE
  )

  N_in_lot[origin == "PosCCNeg", ] <- BulkPos_CCNeg_Lots[origin == "PosCCNeg", ]
  N_in_lot[origin == "PosCCPos", ] <- BulkPos_CCPos_Lots[origin == "PosCCPos", ]
  N_in_lot[origin == "NegCCPos", ] <- BulkNeg_CCPos_Lots[origin == "NegCCPos", ]

  # joining lots and returning to the sublot arrangement
  N_in_sublot <- c(t(N_in_lot))

  # Partitioning every sublot into packs (the new width of the matrix)
  Number_packs <- floor(cantaRindFree * sizeSublot / unitSize)
  if (length(b == 1)) {
    p <- mc2d::rdirichlet(newMC, rep(b, Number_packs))
    N_out <- mc2d::rmultinomial(newMC, N_in_sublot, p)
  } else {
    (stop("vector of b non implemented yet."))
  }

  # Checking for sublots with no contamination
  Pi_0_index <- (rowSums(N_out) == 0)
  Pi_0 <- mean(Pi_0_index)

  N_out[Pi_0_index, ] <- N_out[sample(which(!Pi_0_index), sum(Pi_0_index), replace = TRUE), ]

  P <- data$P * (1 - Pi_0) # prevalence at the sublot level
  N <- N_out
   
  lotMeans <- rowMeans(N / unitSize, na.rm = TRUE)
  unitsCounts <- c(N / unitSize)
  
  data$lotMeans <- lotMeans
  data$unitsCounts <- unitsCounts
  data$P <- P
  data$N <- N
  data$unitSize <- unitSize
  return(data)
}
