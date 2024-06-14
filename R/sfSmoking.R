#' @title Reduction of \emph{L. monocytogenes} due to smoking brined or dry-salted fish
#'
#' @description
#' The function [sfSmoking()] describes the combined effect of smoking brined or salted fish fillets and maturing
#' for 18-24 hours. Based on the literature, different reduction factors of \emph{L. monocytogenes} are applied to
#' brine-injected fish fillets and dry-salted fillets. Lot-specific log10 reduction values are sampled from normal
#' distributions, attending to the type of salting.
#'
#' @param data See [Lot2LotGen()] function.
#' @param saltingType Salting method employed in each of the lots: `brined` or `salted` (vector).
#' @param nLots Number of lots sampled or size of the Monte Carlo simulation (scalar).
#' @param sizeLot Number of units or portions produced in a lot (scalar).
#' @param rBrineMean (`log10`) mean of the normal distribution about the log10 reduction in \emph{L. monocytogenes} in brine-injected
#'  fish fillets due to smoking and 18-24-h maturation (scalar).
#' @param rBrineSd (`log10`) standard deviation of the normal distribution about the log10 reduction in \emph{L. monocytogenes}
#'   in brine-injected fish fillets due to smoking and 18-24-h maturation  (scalar).
#' @param rDrysaltMean (`log10`) mean of the normal distribution about the log10 reduction in \emph{L. monocytogenes} in dry-salted
#' fish fillets due to smoking and 18-24-h maturation (scalar).
#' @param rDrysaltSd (`log10`) standard deviation of the normal distribution about the log10 reduction in \emph{L. monocytogenes}
#'  in dry-salted fish fillets due to smoking and 18-24-h maturation (scalar).
#'
#' @return A list of two elements:
#'     \describe{
#'        \item{N}{(CFU) A matrix of size nLots lots by sizeLot units containing the numbers of \emph{L. monocytogenes}
#'            in salted fish fillets after smoking and 18-24-h maturation}
#'        \item{pSurvSmoking}{Probability of a microbial cell to survive the smoking treatment (vector).}
#'            }
#'
#' @author Ursula Gonzales-Barron \email{ubarron@ipb.pt}
#'
#' @keywords log-reduction smoke effect maturation curing
#'
#' @references
#'
#' \insertRef{Eklund1995}{qraLm}
#' \insertRef{extraDistr}{qraLm}
#' \insertRef{Neunlist2005}{qraLm}
#' \insertRef{Porsby2008}{qraLm}
#' \insertRef{stats}{qraLm}
#'
#' @importFrom extraDistr rtnorm
#' @importFrom stats rbinom
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @note The suggested parameters `rBrineMean=0.871` and `rBrineSd=0.807` defining the normal distribution about the
#' variability in the log10 reduction in \emph{L. monocytogenes} in brined-injected fish were modelled using data extracted from
#' \insertCite{Eklund1995;textual}{qraLm} and \insertCite{Porsby2008;textual}{qraLm}; where these authors inoculated \emph{L. monocytogenes}
#' in smoked salmon through brine injection; submitted samples to cycles of cold-smoking between 6-8 hours; and determined the microbial
#' concentrations after 18-24 hours maturation. The suggested parameters `rDrysaltMean=1.093` and `rDrysaltSd=0.532` characterising
#' the normal distribution about the variability in the log10 reduction in \emph{L. monocytogenes} in dry-salted fish were modelled using data from
#' \insertCite{Eklund1995;textual}{qraLm}, \insertCite{Neunlist2005;textual}{qraLm} and \insertCite{Porsby2008;textual}{qraLm}, who performed
#' experiments inoculating inoculated \emph{L. monocytogenes} on the surface of dry-salted salmon, and quantified the pathogen concentrations after
#' 18-24 hours maturation.
#'
#' @examples
#' nLots <- 10
#' sizeLot <- 5
#' rBrineMean <- 0.871
#' rBrineSd <- 0.807
#' rDrysaltMean <- 1.093
#' rDrysaltSd <- 0.532
#' saltingType <- rbinom(nLots, size = 1, prob = 0.5)
#' saltingType <- ifelse(saltingType == 1, "brined", "salted")
#' data <- list(
#'   N = matrix(rpois(nLots * sizeLot, 25), nrow = nLots, ncol = sizeLot),
#'   P = 0.05, ProbUnitPos = rep(0.05, nLots), saltingType = saltingType
#' )
#'
#' smokedfish <- sfSmoking(data,
#'   rBrineMean = 0.871, rBrineSd = 0.807,
#'   rDrysaltMean = 1.093, rDrysaltSd = 0.532
#' )
#' hist(smokedfish$N)
sfSmoking <- function(data = list(),
                      saltingType = NULL,
                      nLots = NULL,
                      sizeLot = NULL,
                      rBrineMean = 0.871,
                      rBrineSd = 0.807,
                      rDrysaltMean = 1.093,
                      rDrysaltSd = 0.532) {
  N <- data$N
  #  saltingType <- data$saltingType


  if (missing(saltingType)) saltingType <- data$saltingType # test if saltingType was defined
  if (is.null(saltingType)) {
    saltingType <- "salted"
    warning("Add 'saltingType=#' to function arguments") # test again if saltingType is defined
  }

  # Get the simulation dimension

  if (missing(nLots)) nLots <- nrow(N) # test if nLots was defined
  if (is.null(nLots)) warning("Add 'nLots=#' to function arguments") # test again if nLots is defined

  if (missing(sizeLot)) sizeLot <- ncol(N) # test if sizeLot was defined
  if (is.null(sizeLot)) warning("Add 'sizeLot=#' to function arguments") # test again if sizeLot is defined

  #  nLots <- nrow(N)
  #  sizeLot <- ncol(N)
  index_brine <- which(rep(saltingType, length.out = nLots) == "brined")
  index_salt <- which(rep(saltingType, length.out = nLots) == "salted")

  # Creating the empty output matrices
  N_out <- matrix(0, ncol = sizeLot, nrow = nLots)
  atLeastOneSurvive_out <- matrix(0, ncol = sizeLot, nrow = nLots)
  pSurvSmoking <- rep(0, nLots)

  # Lots of fish that were injected with brine, assuming smoking effect varies from lot to lot
  if (any(saltingType == "brined")) { # some fishes are brined

    R_brine <- extraDistr::rtnorm(length(index_brine), rBrineMean, rBrineSd, a = 0)
    pSurvive_brine <- 10^-R_brine
    rbinom(
      n = sizeLot * length(index_brine),
      size = N[index_brine, ],
      prob = pSurvive_brine
    )
    N_out[index_brine, ] <- stats::rbinom(
      n = sizeLot * length(index_brine),
      size = N[index_brine, ],
      prob = pSurvive_brine
    )
    atLeastOneSurvive_out[index_brine, ] <- 1 - (1 - pSurvive_brine)^N[index_brine, ]
  }

  # Lots of fish that were dry-salted, assuming smoking effect varies from lot to lot
  if (any(saltingType == "salted")) { # some fishes are dry salted

    R_drysalt <- extraDistr::rtnorm(length(index_salt), rDrysaltMean, rDrysaltSd, a = 0)
    pSurvive_salt <- 10^-R_drysalt
    N_out[index_salt, ] <- stats::rbinom(
      n = sizeLot * length(index_salt),
      size = N[index_salt, ],
      prob = pSurvive_salt
    )
    atLeastOneSurvive_out[index_salt, ] <- 1 - (1 - pSurvive_salt)^N[index_salt, ]
  }

  # Preparing the outputs
  # pSurvSmoking[index_brine] <- pSurvive_brine
  # pSurvSmoking[index_salt] <- pSurvive_salt

  data$N <- N_out
  data$ProbUnitPos <- data$ProbUnitPos * rowMeans(atLeastOneSurvive_out)
  data$P <- data$P * mean(atLeastOneSurvive_out)
  
  #Testing Correction factor  - just a test
  #CF = data$ProbUnitPos/data$P 
  #data$ProbUnitPos= data$ProbUnitPos/CF

  #data$pSurvSmoking <- pSurvSmoking
  return(data)
}
