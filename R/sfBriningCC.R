#' @title Internal contamination of fish fillets during brine injection
#'
#' @description
#' The function [sfBriningCC()] simulates the potential internal contamination of fish fillets during brining by injection
#' of salt solution. The cross-contamination algorithm accounts for four possible scenarios:
#'  \enumerate{
#'     \item cross-contamination occurring in lots already contaminated;
#'     \item cross-contamination occurring in lots that were not contaminated;
#'     \item no additional contamination occurring in lots already contaminated; and
#'     \item no cross-contamination occurring in lots that were not contaminated. Probabilities of occurrence of the four events are computed.
#'     }
#' The mean volume of brine injected per fillet (`V_inj`) and the mean concentration of \emph{L. monocytogenes} in brine (`N_brine`) are
#' allowed to vary from lot to lot; and, to this effect, their values are sampled from Pert distributions with parameters `volInjMin`, `volInjMode`,
#' `volInjMax`; and `concBrineMin`, `concBrineMode`, `concBrineMax`, respectively. To estimate the number of cells transferred to the
#' fish fillet, it is assumed that the cells follow a Poisson distribution in the volume of brine.
#'
#' @param data a list of
#' \describe{
#'    \item{`N`}{(`CFU`) A matrix of `size nLots` lots by `sizeLot` units containing the numbers of \emph{L. monocytogenes} in/on fish fillets;}
#'    \item{`P`}{Mean prevalence of contaminated lots (scalar);}
#'    \item{`ProbUnitPos`}{Probability of individual lots being contaminated (vector).}
#'  }
#' @param pccBrine Probability that the brine solution is contaminated with \emph{L. monocytogenes}  (scalar).
#' @param volInjMin (`ml`) minimum volume of brine solution injected in a fish fillet (scalar).
#' @param volInjMode (`ml`) most likely volume of brine solution injected in a fish fillet (scalar).
#' @param volInjMax (`ml`) maximum volume of brine solution injected in a fish fillet (scalar).
#' @param concBrineMin (`CFU/ml`) minimum concentration of \emph{L. monocytogenes} in contaminated brine solution (scalar).
#' @param concBrineMode (`CFU/ml`) most likely concentration of \emph{L. monocytogenes} in contaminated brine solution (scalar).
#' @param concBrineMax (`CFU/ml`) maximum concentration of \emph{L. monocytogenes} in contaminated brine solution (scalar).
#' @return A list of three elements:
#'  \describe{
#'    \item{`N`}{(`CFU`) A matrix of size `nLots` lots by `sizeLot` units containing the numbers of \emph{L. monocytogenes} in brined fish;}
#'    \item{`ProbUnitPos`}{Probability of individual lots being contaminated after brining (vector);}
#'    \item{`P`}{Mean prevalence of contaminated lots after brining (scalar).}
#' }
#'
#' @author Ursula Gonzales-Barron \email{ubarron@ipb.pt} and Regis Pouillot \email{rpouillot.work@gmail.com}
#'
#' @keywords brine solution injection internal contamination transfer

#' @references
#'
#' \insertRef{Gudbjornsdottir2004}{qraLm}
#' \insertRef{Gudbjornsdottir2005}{qraLm}
#' \insertRef{mc2d}{qraLm}
#' \insertRef{stats}{qraLm}
#' \insertRef{iRisk2021}{qraLm}
#'
#' @importFrom mc2d rpert
#' @importFrom stats rpois
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @note The suggested value of \eqn{probCCDice\_brine=0.135} is taken from \insertCite{Gudbjornsdottir2005;textual}{qraLm} and
#' \insertCite{Gudbjornsdottir2004;textual}{qraLm}, who analysed the presence of \emph{L. monocytogenes} in brining solution, detecting
#' 3 positive samples out of 14, and 2 positive samples out of 23, respectively.
#' The parameters of the distributions about mean volume of brine solution injected in a fish fillet and mean concentration of \emph{L. monocytogenes} in brine
#' must be defined by the user and/or tested in scenarios.
#'
#' @examples
#' nLots <- 100
#' sizeLot <- 50
#' pccBrine <- 5 / 37
#' volInjMin <- 0.5
#' volInjMode <- 1.2
#' volInjMax <- 4.5
#' concBrineMin <- 0
#' concBrineMode <- 3
#' concBrineMax <- 40
#' ProbUnitPos <- rep(0.105, nLots)
#'
#' dat <- list(
#'   N = matrix(rpois(nLots * sizeLot, 16),
#'     nrow = nLots,
#'     ncol = sizeLot
#'   ),
#'   P = 0.22,
#'   ProbUnitPos = ProbUnitPos,
#'   nLots = 100,
#'   sizeLot = 50
#' )
#' Nf <- sfBriningCC(
#'   dat,
#'   pccBrine,
#'   volInjMin, volInjMode, volInjMax,
#'   concBrineMin, concBrineMode, concBrineMax
#' )
#' hist(Nf$N)
sfBriningCC <- function(data = list(),
                        pccBrine,
                        volInjMin,
                        volInjMode,
                        volInjMax,
                        concBrineMin,
                        concBrineMode,
                        concBrineMax) {
  N <- data$N
  # Get simulation dimension
  # if(missing(nLots)) nLots <- data$nLots #test if nLots was defined
  # if(is.null(nLots)) warning("Add 'nLots=#' to function arguments") #test again if nLots is defined
  #
  # if(missing(sizeLot)) sizeLot <- data$sizeLot #test if sizeLot was defined
  # if(is.null(sizeLot)) warning("Add 'sizeLot=#' to function arguments") #test again if sizeLot is defined
  
  nLots <- nrow(N)
  sizeLot <- ncol(N)
  # Error in case at least one row of the matrix consists of zeroes
  if (any(rowSums(N) == 0)) stop("The matrix should be composed of contaminated lots only")
  
  # calculated probabilities
  p_LotPos_CCNeg <- data$ProbUnitPos * (1 - pccBrine)
  p_LotPos_CCPos <- data$ProbUnitPos * pccBrine
  p_LotNeg_CCPos <- (1 - data$ProbUnitPos) * pccBrine
  # Add random
  # 1 is p_LotPos_CCNeg, 2 is p_LotPos_CCPos, 3 is p_LotNeg_CCPos
  # It is assumed that if brine from injection machine is contaminated, it will contaminate the entire lot
  # In other words, no contamination event of brine will take place at the middle of a processing lot
  matProb <- cbind(p_LotPos_CCNeg, p_LotPos_CCPos, p_LotNeg_CCPos)
  n_lot <- sapply(1:nLots, function(x) base::sample(1:3, size = 1, prob = matProb[x, ]))
  
  # sampling V_inj nd N_brine for every lot
  V_inj <- mc2d::rpert(nLots, min = volInjMin, mode = volInjMode, max = volInjMax, shape = 4)
  C_brine <- mc2d::rpert(nLots, min = concBrineMin, mode = concBrineMode, max = concBrineMax, shape = 4)
  # calculating the lot-specific level of cross-contamination assuming cells in brine distribute as a Poisson
  # Note: V_inj*C_brine is of size nLots and will recycle nicely
  N_transfer <- matrix(stats::rpois(nLots * sizeLot, V_inj * C_brine), nrow = nLots, ncol = sizeLot)
  
  # Re-arranging the matrix
  N_lotPostCC <- matrix(0, nrow = nLots, ncol = sizeLot)
  # Previously contaminated: take the previous values
  previously <- n_lot %in% c(1, 2)
  N_lotPostCC[previously, ] <- N[previously, ]
  # New contamination: add the cross-contamination to zero (case 3) or to the previous values (case 2)
  newly <- n_lot %in% c(2, 3)
  N_lotPostCC[newly, ] <- N_lotPostCC[newly, ] + N_transfer[newly, ]
  
  # Preparing the output
  N <- N_lotPostCC
  P <- 1 - (1 - data$P) * (1 - pccBrine)
  ProbUnitPos <- 1 - (1 - data$ProbUnitPos) * (1 - pccBrine)
  
  #lotMeans <- rowMeans(N / data$unitSize, na.rm = TRUE)
  unitsCounts <- c((ProbUnitPos / mean(ProbUnitPos)) * (N / data$unitSize))
  
  #data$lotMeans <- lotMeans
  data$unitsCounts <- unitsCounts
  data$N <- N
  data$P <- P
  data$ProbUnitPos <- ProbUnitPos
  
  return(data)
}
