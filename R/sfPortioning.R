#' @title Portioning of the contents of a seafood pack at home
#'
#' @description
#' The [sfPortioning()] function represents the portioning of a pack of RTE seafood into a smaller unit to be consumed. It is assumed that the microbial cells present in a contaminated pack
#' are distributed into servings following a beta-binomial distribution, although the algorithm only retains one portion per pack (and not all the portions that can be obtained from a pack).
#' The dispersion factor `bPortSF` represents the extent of cell clustering in the RTE seafood within the package.
#'
#' @param data a list with a minimum element:
#' \describe{
#'           \item{N}{(CFU) A matrix of size nLots lots by sizeLot pack units  containing the numbers of \emph{L. monocytogenes} per pack before handling at home.}
#'           }
#' @param nLots Number of lots sampled or size of the Monte Carlo simulation (scalar).
#' @param sizeLot Number of units or portions produced in a lot (scalar).
#' @param bPortSF dispersion factor of cells within the package (scalar or vector).
#' @param servingSize (g) is the portion taken from a pack, which will later equals to the serving size (scalar or vector).
#' @param unitSize (g) is the weight of a pack of RTE seafood (scalar).
#' @return the data object with modified:
#'     \describe{
#'              \item{N}{(CFU) A matrix of size nLots lots by sizeLot units containing the numbers of \emph{L. monocytogenes} in the portions of RTE seafood.}
#'              }
#'
#' @author Ursula Gonzales-Barron \email{ubarron@ipb.pt} and Regis Pouillot \email{rpouillot.work@gmail.com}
#'
#' @keywords servings portions handling
#'
#' @references
#'
#' \insertRef{Nauta2005}{qraLm}
#'
#' \insertRef{stats}{qraLm}
#'
#' @importFrom stats rbinom
#'
#' @export
#'
#' @note A dispersion factor `bPortSF=1` represents moderate clustering of cells (\insertCite{Nauta2005}{qraLm}) in the RTE seafood within the package.
#' The serving size `servingSize` should be provided by the user and/or could be tested in scenarios.
#'
#' @examples
#' nLots <- 1000
#' sizeLot <- 500
#' dat <- list(
#'   N = matrix(rpois(sizeLot * nLots, 10),
#'     nrow = nLots, ncol = sizeLot
#'   ), P = 0.16,
#'   ProbUnitPos = rep(0.16, nLots)
#' )
#' Nf <- sfPortioning(dat, servingSize = 150, unitSize = 500, bPortSF = 1)
#' str(Nf)
#' hist(Nf$N) # histogram of microbial cells in contaminated servings
#'
sfPortioning <- function(data = list(),
                         nLots = NULL,
                         sizeLot = NULL,
                         unitSize = NULL,
                         servingSize,
                         bPortSF) {
  N <- data$N

  if (missing(nLots)) nLots <- nrow(data$N) # test if nLots was defined
  if (is.null(nLots)) warning("Add 'nLots=#' to function arguments") # test again if nLots is defined

  if (missing(sizeLot)) sizeLot <- ncol(data$N) # test if sizeLot was defined
  if (is.null(sizeLot)) warning("Add 'sizeLot=#' to function arguments") # test again if sizeLot is defined

  if (missing(unitSize)) unitSize <- data$unitSize # test if unitSize was defined
  if (is.null(unitSize)) warning("Add 'unitSize=#' to function arguments") # test again if unitSize is defined


  #  ifelse (exists("servingSize", data)==TRUE, servingSize <- data$servingSize, print("Add 'servingSize=#' to function arguments"))

  #  nLots <- nrow(N)
  #  sizeLot <- ncol(N)

  lN <- nLots * sizeLot

  n_serv <- pmax(round(unitSize / servingSize), 1)

  p <- stats::rbeta(lN, shape1 = bPortSF, shape2 = bPortSF * (n_serv - 1))
  N_out <- stats::rbinom(lN, N, p)
  N_out <- matrix(N_out, ncol = sizeLot, nrow = nLots)
  
  # Lines of zeros occur after portioning
  zeroes <- rowSums(N_out) == 0
  if (any(zeroes)) {
    N_out[zeroes, 1] <- 1
    Pi_0 <- sum(zeroes) / nLots
    data$P <- data$P * (1 - Pi_0)
    data$ProbUnitPos <- data$ProbUnitPos*(1 - Pi_0)
  }
  # output
  N <- N_out
  lotMeans <- rowMeans(N / servingSize, na.rm = TRUE)
  unitsServing <- c((data$ProbUnitPos/mean(data$ProbUnitPos)) * (N / servingSize))
  
  data$lotMeans <- lotMeans
  data$unitsServing <- unitsServing
  data$N <- N
  data$servingSize <- servingSize
  return(data)
}