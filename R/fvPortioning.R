#' @title Portioning of the contents of a pack at home
#'
#' @description
#' The [fvPortioning()] function represents the portioning of a pack of frozen vegetables into a smaller unit. It is assumed that the microbial
#' cells present in a contaminated pack are distributed into servings following a beta-binomial distribution, although the algorithm only retains
#' one portion per pack (and not all the portions that can be obtained from a pack). The dispersion factor `bPort` represents the extent of cell
#' clustering in the frozen vegetables within the package. The fact that the algorithm takes into account that a contaminated pack can produce a
#' non-contaminated portion, enables the estimation of the prevalence of contaminated portions. It is assumed that the dispersion parameter and
#' the number of portions (that can be obtained from a pack) are independent of the microbial numbers.
#'
#' @param data a list of:
#' \describe{
#'    \item{`N`}{(`CFU`) A matrix of size `nLots` lots by `sizeLot` units representing the numbers of \emph{L. monocytogenes} per
#'              pack before handling at home;}
#'    \item{`ProbUnitPos`}{Probability of (tested) individual lots being contaminated (vector);}
#'    \item{`P`}{Mean prevalence of contaminated lots (scalar).}
#'           }
#' @param nLots Number of lots sampled or size of the Monte Carlo simulation (scalar).
#' @param sizeLot Number of units or portions produced in a lot (scalar).
#' @param bPort dispersion factor of cells within the package (scalar or vector).
#' @param servingSize (`g`) is the portion taken from a pack, which will later equals to the serving size (scalar or vector).
#' @param unitSize (`g`) is the weight of a pack of frozen vegetables (scalar).
#' @return A list of three elements:
#'     \describe{
#'              \item{`N`}{(`CFU`) A matrix of size `nLots` lots by `sizeLot` units representing the numbers of \emph{L. monocytogenes}
#'              in the portions of frozen vegetables.}
#'              \item{`ProbUnitPos`}{Lot-specific probability of contaminated portions or servings (vector).}
#'              \item{`P`}{Mean prevalence of contaminated portions or servings (scalar).}
#'              }
#'
#' @author Ursula Gonzales-Barron \email{ubarron@ipb.pt} and Regis Pouillot \email{rpouillot.work@gmail.com}
#'
#' @keywords servings portions handling
#'
#' @references
#' \insertRef{Nauta2005}{qraLm}
#'
#' @importFrom extraDistr rtnorm
#' @importFrom mc2d rtriang rmultinomial
#' @importFrom Rdpack reprompt
#' @importFrom extraDistr dbbinom
#'
#' @export
#'
#' @note A dispersion factor \eqn{bPort=1} represents moderate clustering of cells \insertCite{Nauta2005;textual}{qraLm} in the frozen vegetables
#' within the package.
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
#' Nf <- fvPortioning(dat, servingSize = 150, unitSize = 500, bPort = 1)
#' str(Nf)
#' hist(Nf$N) # histogram of microbial cells in contaminated servings
#'
fvPortioning <- function(data = list(),
                         nLots = NULL,
                         sizeLot = NULL,
                         servingSize,
                         unitSize = NULL,
                         bPort) {
  N <- data$N

  if (missing(nLots)) nLots <- nrow(data$N) # test if nLots was defined
  if (is.null(nLots)) warning("Add 'nLots=#' to function arguments") # test again if nLots is defined

  if (missing(sizeLot)) sizeLot <- ncol(data$N) # test if sizeLot was defined
  if (is.null(sizeLot)) warning("Add 'sizeLot=#' to function arguments") # test again if sizeLot is defined

  if (missing(unitSize)) unitSize <- data$unitSize # test if unitSize was defined
  if (is.null(unitSize)) warning("Add 'unitSize=#' to function arguments") # test again if unitSize is defined

  # nLots <- nrow(N)
  # sizeLot <- ncol(N)

  n_serv <- pmax(round(unitSize / servingSize), 1)

  # finding probability of zero cells in a serving taken from a contaminated pack (including 0)
  # with the extraDistr::dbbinom (beta binomial) function
  prob_zero <- matrix(extraDistr::dbbinom(0, N, bPort, bPort * (n_serv - 1)),
    ncol = sizeLot, nrow = nLots
  )
  prob_zero <- rowMeans(prob_zero)

  # Extract non zeroes and resample (per line)
  lN <- nLots * sizeLot
  # need to transpose because apply bring back a matrix of size n * dim(mat[MARGIN])
  N <- t(apply(N, 1, function(x) {
    if (sum(x) == 0) {
      return(c(1, rep(0, sizeLot - 1)))
    }
    # Using the trick to avoid strange behavior of sample when x is a scalar
    sample(rep(x[x > 0], 2), size = sizeLot, replace = TRUE) # added as.integer() VC
  }))

  # 1 will give 1 through the multinomial. Won't send them in the function
  # for sake of speed
  whichOnes <- N == 1
  whichNotOnes <- !whichOnes
  NNotOnes <- c(N[whichNotOnes])
  lNm1 <- length(NNotOnes)

  # Defining the partition function to generate cells in servings from packs
  # only values >0
  if (length(bPort) == 1 & length(n_serv) == 1) {
    # If bPort and n_serv are scalar, this is quicker
    # see tests
    p <- mc2d::rdirichlet(lNm1, rep(bPort, n_serv))
    N_out <- mc2d::rmultinomial(lNm1, NNotOnes, p)
    N_pos <- apply(N_out, 1, function(x) (x[x > 0])[1])
  } else {
    # if bPort or n_serv is a vector, mapply a function
    f <- function(N, b, n_serv) {
      p <- mc2d::rdirichlet(1, rep(b, n_serv))
      N_out <- mc2d::rmultinomial(1, N, p)
      return((N_out[N_out > 0])[1])
    }
    N_pos <- mapply(f,
      N = NNotOnes,
      # assume bPort and n_serv are independent of N
      # (take the first length(N) values)
      b = rep(bPort, length = length(NNotOnes)),
      n_serv = rep(n_serv, length = length(NNotOnes))
    )
  }

  # Add the ones and shuffle
  N[whichNotOnes] <- N_pos

  # calculating overall probability of a serving having positive counts
  P <- data$P * (1 - mean(prob_zero))
  
  ProbUnitPos <- data$ProbUnitPos * (1 - prob_zero)
  
  lotMeans <- rowMeans(N / data$unitSize, na.rm = TRUE)
  unitsCounts <- c((ProbUnitPos/mean(ProbUnitPos)) * (N/data$unitSize))
  unitsServing <- c((ProbUnitPos/mean(ProbUnitPos)) * (N/servingSize))
  
  data$lotMeans <- lotMeans
  data$unitsCounts <- unitsCounts
  data$unitsServing <- unitsServing
  
  data$P <- P
  data$N <- N
  data$ProbUnitPos <- ProbUnitPos
  data$unitSize <- unitSize
  data$servingSize <- servingSize
  return(data)
}