#' @title Application of a testing regime for \emph{L. monocytogenes} in a sample of food units from a lot
#'
#' @description
#' [fvTesting()] is a function that simulates the microbiological testing of food unit samples taken from a lot.
#' Lots are subjected, at a given probability (`pLotTested`), to sampling and testing, according to a sampling plan defined by the user.
#' The algorithm allows for two-class or three-class sampling plans, although the most commonly used for \emph{L. monocytogenes} is the former one:
#' `n` (sample size), `c` (maximum level accepted), `w` (weight used in the enrichment essay). By performing a bootstrapping on the sampling and testing scheme,
#' the algorithm estimates that probability of detecting contaminated lots, and the probability that individual lots are contaminated given that they were not detected
#' as positive during testing. Contaminated lots detected after testing are not removed from the matrix, and therefore the input matrix `N` is returned unchanged.
#'
#' @param data a list of:
#'  \describe{
#'      \item{N}{(`CFU`) A matrix of size `nLots` lots by `sizeLot` units representing the numbers of \emph{L. monocytogenes} per pack unit, from contaminated lots.}
#'      \item{ProbUnitPos}{Probability of individual lots being contaminated (a lot is considered contaminated if at least one pack unit is contaminated) (vector).}
#'      \item{P}{Mean prevalence of contaminated lots (scalar).}
#'       }
#' @param nLots Number of lots sampled or size of the Monte Carlo simulation (scalar).
#' @param sizeLot Number of units or portions produced in a lot (scalar).
#' @param nTested sample size or number of units tested (scalar).
#' @param gTested (`g`) sample weight tested per unit (scalar or vector).
#' @param MTested (`CFU/g`) maximum concentration accepted in a sample (scalar or vector). If \eqn{>0}, will lead to a 3-class plan.
#' @param cTested maximum number of samples accepted between \eqn{m=0} and `M` (scalar or vector).
#' @param pLotTested proportion of lots subjected to sampling and testing or frequency of testing (scalar or vector).
#' @param unitSize (`g`) weight of the contents of a pack unit (scalar or vector).
#' @param Se Sensibility of the test or probability to detect each bacterial cell applying a given microbiological essay (scalar or vector).
#' @param gTestedEnum (`g`) sample weight tested for enumeration (scalar or vector).
#' @param iterSub number of internal repetition for the testing. If `NULL`, it will be set to the minimum of `nLots` and 1000.
#'
#' @return A list of five elements of the data objects:
#'     \describe{
#'              \item{N}{(`CFU`) A matrix of size `nLots` lots by `sizeLot` units representing the numbers of \emph{L. monocytogenes} per
#'               pack unit, after within-lot testing.}
#'              \item{P}{Mean prevalence of contaminated lots after within-lot testing (scalar).}
#'              \item{ProbUnitPos}{Probability that the individual lot is contaminated given that it was not detected after testing (vector).}
#'              \item{ProbThisLot}{Individual probability that this lot is still in. It is the complement of the probability
#'              that this specific lot was detected (vector).}
#'              \item{D}{Mean probability of detecting contaminated lots (scalar).}
#'              }
#'
#' @author Regis Pouillot \email{rpouillot.work@gmail.com}
#'
#' @keywords within-lot testing sampling plan
#'
#' @references
#' \insertRef{PROFEL2020}{qraLm}
#'
#' @importFrom stats rbinom
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @note Although microbiological criteria for \emph{L. monocytogenes} are not intended for non-ready-to-eat foods, this function was
#' conceived to allow the determination of the performance of sampling plans in frozen blanched vegetables as a
#' potential risk management strategy. \insertCite{PROFEL2020;textual}{qraLm} explains that quick-frozen vegetables should be analysed
#'  for detection of\emph{L. monocytogenes} within the frame of the verification of the food safety management system.
#'  If the pathogen is detected in 25 g, further enumeration needs to be conducted,
#'  on the same samples, to verify if intermediate set limits are reached (\eqn{< 10 CFU/g}) or not.
#'
#' @examples
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
#' # Testing the microbiological criterion for RTE foods of absence of \emph{L. monocytogenes}
#' # in five samples of 25 g each (two-class sampling plan)
#' Nf1 <- fvTesting(dat,
#'   nTested = 5, gTested = 25, MTested = 0, cTested = 0,
#'   pLotTested = 0.5, unitSize = 500, Se = 0.9, iterSub = NULL
#' )
#' # Applying an in-house sampling plan of a maximum of one sample (c=1)
#' # below the M limit 10 CFU/g from a total of five samples taken from a lot
#' Nf2 <- fvTesting(dat,
#'   nTested = 5, gTested = 25, MTested = 10, cTested = 1,
#'   pLotTested = 0.5, unitSize = 500, Se = 0.9, gTestedEnum = 10, iterSub = 800
#' )
#'
fvTesting <- function(data = list(),
                      nTested = 5,
                      gTested = 25,
                      MTested = 0,
                      cTested = 0,
                      pLotTested,
                      nLots = NULL,
                      sizeLot = NULL,
                      unitSize = NULL,
                      Se = 1,
                      gTestedEnum = 10,
                      iterSub = NULL) {
  if (pLotTested == 0 | nTested == 0 | gTested == 0) {
    data$D <- 0
    data$ProbThisLot <- rep(1, nrow(data$N))
    return(data)
  }

  N <- data$N

  ifelse(exists("nLots", data) == TRUE, nLots <- data$nLots, nLots <- nrow(data$N))
  ifelse(exists("sizeLot", data) == TRUE, sizeLot <- data$sizeLot, sizeLot <- ncol(data$N))
  ifelse(exists("unitSize", data) == TRUE, unitSize <- data$unitSize, print("Add 'unitSize=#' to function arguments"))
  #  nLots <- nrow(N)
  #  sizeLot <- ncol(N)

  # Assume no correlation between lot. We will take nTested columns

  if (is.null(iterSub)) iterSub <- min(1000, nLots)

  # Detection of bacteria for each portions
  # Assume that the probability of each bacteria to be in the sample size is gTested/unitSize * Se
  Detection <- matrix(stats::rbinom(nLots * sizeLot, N, prob = Se * gTested / unitSize) > 0,
    ncol = sizeLot, nrow = nLots
  )

  # Build an array of (nTested, iterSub, nLots) indexes, sampling nTested*iterSub values per lot
  # the apply function will throw a (nSample*iterSub)x(iter) matrix
  Index <- matrix(1:(nLots * sizeLot), ncol = sizeLot, nrow = nLots)

  sampleDraw <- function(x, n, iter) {
    x[sample.int(length(x), size = n * iter, replace = (n * iter) > length(x))]
  }

  IndexBoot <- array(apply(Index, 1, sampleDraw, n = nTested, iter = iterSub), dim = c(nTested, iterSub, nLots))
  # Put the values
  Detected <- array(Detection[IndexBoot], dim = c(nTested, iterSub, nLots))

  # Enumeration (3 class plan) if MTested>0
  if (MTested > 0) {
    if (gTestedEnum > unitSize) stop("gTestedEnum should be < unitSize")
    # observed concentration (`CFU/g`)
    Enumeration <- matrix(stats::rbinom(nLots * sizeLot, N, prob = Se * gTestedEnum / unitSize) / gTestedEnum, ncol = sizeLot, nrow = nLots)
    # Use same Index Boot
    Enumerated <- array(Enumeration[IndexBoot], dim = c(nTested, iterSub, nLots))
    # Enumeration done only if detected (if not detected, enumeration is 0)
    Enumerated <- Detected * Enumerated

    # sum over nTested
    # values between >0 (detected) and <=MTested
    mM <- apply(Detected > 0 & Enumerated <= MTested, c(2, 3), sum)
    # values above MTested
    M <- apply(Enumerated > MTested, c(2, 3), sum)
    # Positive lot if between 0 and MTested > cTested or if one > MTested
    testPos <- M > 0 | mM > cTested
    # Mean over iterSub is the expected probability to detect the lot
    testPos <- colMeans(testPos)
  } else { # 2-class plan
    # positive among the nTested
    testPos <- apply(Detected, c(2, 3), sum) > cTested
    testPos <- colMeans(testPos)
  }
  testPos <- testPos * pLotTested
  # Discarded: positive * prevalence
  data$D <- mean(testPos) * data$P
  # Overall
  P <- data$P # Prevalence pre test
  Se <- mean(testPos) # Sensitivity at lot level= Prob(Detected|+)
  # P post Test is P(+|Non Detected)
  # According to Bayes: P(+|ND) = P(ND|+)P(+)/(P(ND|+)P(+) + P(ND|-)P(-))
  data$P <- (1 - Se) * P / ((1 - Se) * P + 1 * (1 - P))

  # per Lot
  P <- data$ProbUnitPos # Prevalence pre test
  Se <- testPos # Sensitivity at lot level = Prob(Detected|+)
  # P post Test is P(+|Non Detected)
  # According to Bayes: P(+|ND) = P(ND|+)P(+)/(P(ND|+)P(+) + P(ND|-)P(-))
  data$ProbUnitPos <- (1 - Se) * P / ((1 - Se) * P + 1 * (1 - P))
  data$ProbUnitPos[Se == 1] <- 1E-20
  data$ProbThisLot <- (1 - testPos)

  lotMeans <- rowMeans(data$N / data$unitSize, na.rm = TRUE)
  unitsCounts <- c((data$ProbUnitPos/mean(data$ProbUnitPos)) * (data$N/data$unitSize))
  
  data$lotMeans <- lotMeans
  data$unitsCounts <- unitsCounts
  # N is unchanged
  data$N <- N
  data$unitSize <- unitSize
  return(data)
}
