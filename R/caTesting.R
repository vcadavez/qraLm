#' @title Application of a testing regime for \emph{L. monocytogenes} in a sample of RTE cantaloupe from a lot/sublot
#'
#' @description
#' The [caTesting()] function is a function that simulates the microbiological testing of food unit samples taken from a lot.
#' Lots are subjected, at a given probability (`pLotTested`), to sampling and testing, according to a sampling plan defined by the user.
#' The algorithm allows for two-class or three-class sampling plans, although the most commonly used for \emph{L. monocytogenes} is the former one:
#' `n` (sample size), `c` (maximum level accepted), `w` (weight used in the enrichment essay). By performing a bootstrapping on the sampling and testing scheme,
#' the algorithm estimates that probability of detecting contaminated lots. Contaminated lots detected after testing are not removed from the matrix.
#' If the input matrix is in the sublot arrangement, the size of the sublot (`sizeSublot`) must be provided, so that
#' the testing is carried out at the lot level (and not at the sublot level!). Furthermore, the user has the option
#' to have the output matrix `N` returned in the original lot arrangement by setting `backToSublot = FALSE`. If no value is assigned to this argument,
#' the output matrix `N` is returned in the sublot arrangement.
#'
#' @param data a list of:
#' \describe{
#'    \item{N}{(`CFU`) A matrix of size number of lots/sublots by number of packs, representing the numbers of \emph{L. monocytogenes} per pack unit;}
#'    \item{P}{Mean prevalence of contaminated lots (scalar).}
#'    }
#' @param nTested Sample size or number of units tested (scalar).
#' @param gTested (`g`) Sample weight tested per unit (scalar or vector).
#' @param MTested (`CFU/g`) Maximum concentration accepted in a sample (scalar or vector). If \eqn{>0}, will lead to a 3-class plan.
#' @param cTested Maximum number of samples accepted between \eqn{m = 0} and M (scalar or vector).
#' @param pLotTested Proportion of lots subjected to sampling and testing or frequency of testing (scalar or vector).
#' @param unitSize (`g`) Weight of the contents of a pack unit (scalar or vector).
#' @param Se Sensibility of the test or probability to detect each bacterial cell applying a given microbiological essay (scalar or vector).
#' @param gTestedEnum (`g`) Sample weight tested for enumeration (scalar or vector).
#' @param iterSub Number of internal repetition for the testing. If `NULL`, it will be set to the minimum of `nLots` and 1000.
#' @param sizeLot Total number of cantaloupes making up a lot, \emph{i.e.}, cantaloupes harvested in a field batch.
#' @param sizeSublot Number of cantaloupes processed in a sublot. `sizeSublot` must be a multiple of `sizeLot`.
#' NOTE: keep as `NULL` if the input matrix is in lot arrangement.
#' @param backToSublot Set to `FALSE` if the output matrix `N` is to be returned in the lot arrangement. By default, if `sizeSublot`
#' is provided, the output matrix `N` is returned in the sublot arrangement.
#'
#' @return A list of three elements of data objects:
#'     \describe{
#'         \item{`N`}{(`CFU`) A matrix of size number of lots/sublots by number of packs, representing the numbers
#'              of \emph{L. monocytogenes} per pack unit;}
#'         \item{`P`}{Prevalence of contaminated lots after within-lot testing (scalar),}
#'         \item{`D`}{Probability of detecting contaminated lots (scalar).}
#'              }
#'
#' @author Regis Pouillot \email{rpouillot.work@gmail.com}
#'
#' @importFrom stats rbinom
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' # columns <- 175
#' # rows <- 200
#' # N0 <- list(N=matrix(rpois(columns*rows, 3),
#' #            nrow = rows, ncol = columns), P=0.16)
#'
#' # Test directly (No notion of sublot)
#' # Nf <- caTesting (N0, nTested = 5, gTested = 25, MTested = 0,
#' #                 cTested = 0, pLotTested = .1, sizeLot=100,
#' #                 Se=1, unitSize=200)
#' # dim(Nf$N)
#'
#' # backToSublot = TRUE (default):
#' # Input matrix in sublots -> lot -> testing -> Output matrix in sublots
#' # Nf <- caTesting (N0, nTested = 5, gTested = 25, MTested = 0,
#' #                  cTested = 0, pLotTested = .1,
#' #                  Se=1, unitSize=200,
#' #                  sizeLot = 100, sizeSublot=20)
#' # dim(Nf$N)
#'
#' # backToSublot = FALSE: Input matrix in sublots -> lot -> testing -> Input matrix in lots
#' # Nf <- caTesting (N0,
#' #                  nTested = 5,
#' #                 gTested = 25,
#' #                 MTested = 0,
#' #                 cTested = 0,
#' #                 pLotTested = .1,
#' #                 Se=1,
#' #                 unitSize=200,
#' #                 sizeLot = 100,
#' #                 sizeSublot=20,
#' #                 backToSublot = FALSE)
#' # dim(Nf$N)
caTesting <- function(data = list(),
                      nTested,
                      gTested,
                      MTested,
                      cTested,
                      pLotTested,
                      Se,
                      unitSize = NULL,
                      gTestedEnum = 10,
                      sizeLot = NULL,
                      sizeSublot = NULL,
                      backToSublot = TRUE,
                      iterSub = NULL) {
  if (missing(unitSize)) unitSize <- data$unitSize # test if unitSize was defined
  if (is.null(unitSize)) warning("Add 'unitSize=#' to function arguments") # test again if unitSize is defined

  if (missing(sizeLot)) sizeLot <- data$sizeLot # test if sizeLot was defined
  if (is.null(sizeLot)) warning("Add 'sizeLot=#' to function arguments") # test again if sizeLot is defined

  if (missing(sizeSublot)) sizeSublot <- data$sizeSublot # test if sizeSublot was defined
  if (is.null(sizeSublot)) warning("Add 'sizeSublot=#' to function arguments") # test again if sizeSublot is defined


  if ((pLotTested == 0 | nTested == 0 | gTested == 0) & backToSublot) {
    data$D <- 0
    return(data)
    }

  N <- data$N

  # Get the dimensions
  Number_packs_sublot <- ncol(N)
  newMC <- nrow(N)

  if (is.null(sizeSublot)) {
    # Unchanged
    Number_packs_lot <- Number_packs_sublot
    old_nLots <- newMC
  } else {
    # redimensioning in case matrix is given in sublots form
    if (sizeLot %% sizeSublot != 0) stop("sizeLot should be a multiple of sizeSublot")
    Number_packs_lot <- Number_packs_sublot * sizeLot / sizeSublot
    old_nLots <- newMC * sizeSublot / sizeLot
    N <- matrix(t(N), ncol = Number_packs_lot, nrow = old_nLots, byrow = TRUE)
  }

  # Test (taken from fvTesting)
  if (is.null(iterSub)) iterSub <- min(1000, old_nLots)

  # Detection of bacteria for each portions
  # Assume that the probability of each bacteria to be in the sample size is gTested/unitSize * Se
  Detection <- matrix(stats::rbinom(old_nLots * sizeLot, N, prob = Se * gTested / unitSize) > 0,
    ncol = sizeLot, nrow = old_nLots
  )

  # Build an array of (nTested, iterSub, old_nLots) indexes, sampling nTested*iterSub values per lot
  # the apply function will throw a (nSample*iterSub)x(iter) matrix
  Index <- matrix(1:(old_nLots * sizeLot), ncol = sizeLot, nrow = old_nLots)

  sampleDraw <- function(x, n, iter) {
    x[sample.int(length(x), size = n * iter, replace = (n * iter) > length(x))]
  }

  IndexBoot <- array(apply(Index, 1, sampleDraw, n = nTested, iter = iterSub), dim = c(nTested, iterSub, old_nLots))
  # Put the values
  Detected <- array(Detection[IndexBoot], dim = c(nTested, iterSub, old_nLots))

  # Enumeration (3 class plan) if MTested>0
  if (MTested > 0) {
    if (gTestedEnum > unitSize) stop("gTestedEnum should be < unitSize")
    # observed concentration (`CFU/g`)
    Enumeration <- matrix(stats::rbinom(old_nLots * sizeLot, N, prob = Se * gTestedEnum / unitSize) / gTestedEnum,
      ncol = sizeLot, nrow = old_nLots
    )
    # Use same Index Boot
    Enumerated <- array(Enumeration[IndexBoot], dim = c(nTested, iterSub, old_nLots))
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
  # Old version
  # # assume no correlation between lot. We will take nTested columns
  # Sample <- sample(1:ncol(N), nTested, replace=FALSE)
  # NTested <- N[,Sample] * rbinom(old_nLots, 1, pLotTested)
  # # Assume that the probability of each bacteria to be in the sample size is gTested/unitSize * Se
  # NDetected <- matrix(stats::rbinom(nTested*nrow(N), NTested, prob= Se * gTested/unitSize), ncol=nTested)
  # # Concentration detected (/g)
  # CDetected <- NDetected/gTested
  # # Discarded if at least one positive (> cTested)
  # Discarded <- rowSums(CDetected > cTested) > 0
  # if(all(Discarded)) stop("issue in Testing: all lot discarded")
  # # Resample Discarded LOT from non discarded
  # N[Discarded,] <- N[sample(which(!Discarded), sum(Discarded), replace=TRUE),]
  # # Update P
  # # Old version P <- data$P * (1- sum(Discarded)/old_nLots)
  # P  <- data$P #Prevalence pre test
  # Se <- sum(Discarded)/old_nLots # Sensitivity = Prob(Detected|+)
  # # P post Test is P(+|Non Detected)
  # # According to Bayes: P(+|ND) = P(ND|+)P(+)/(P(ND|+)P(+) + P(ND|-)P(-))
  # P <- (1-Se)*P / ((1-Se)*P + 1*(1-P))

  Discarded <- stats::rbinom(old_nLots, size = 1, prob = testPos)
  data$N[Discarded, ] <- 0

  # Back to sublot, if sizeLot != sizeSublot and backToSublot == TRUE
  if (is.numeric(sizeSublot) && backToSublot) {
    N <- matrix(t(N),
      ncol = Number_packs_sublot,
      nrow = newMC, byrow = TRUE
    )
    # Check new empty sublots
    # zeroes <- rowSums(N) == 0
    # # Resample emptied sublots
    # if(any(zeroes)){
    #   N[zeroes,] <- N[sample(which(!zeroes),sum(zeroes), replace=TRUE),]
    #   # Update P again (using MC), prevalence of contaminated sublots accordingly
    #   P <- P * (1- sum(Zeroes)/newMC)
    # }
  }
  
  lotMeans <- rowMeans(N / unitSize, na.rm = TRUE)
  unitsCounts <- c(N / unitSize)
  
  data$lotMeans <- lotMeans
  data$unitsCounts <- unitsCounts
  
  data$N <- N
  # Discarded: Positive * prevalence
  data$D <- mean(testPos) * data$P
  return(data)
}
