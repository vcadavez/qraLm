#' @title Cross-contamination of fish fillets during brine injection or dry-salting
#'
#' @description
#' The function [sfBrineORsaltCC()] simulates the potential internal or external cross-contamination of fish fillets during the process of salting, either by
#' brine injection or by dry-salting, respectively. The type of salting applied in a lot of fish fillets is mutually exclusive; so
#' the probability that a lot of fish fillets is salted via brine injection is `pBrine`, and the complement `1-Pcc_salting` is the
#' probability that the lot of fish fillets is treated by external dry-salting. Since this algorithm relies on two functions: [sfBriningCC()] and
#' [sfSmearingCC()], the arguments of both functions must be provided to [sfBrineORsaltCC()].
#'
#' @param data a list of
#'    \describe{
#'      \item{`N`}{(`CFU`) A matrix of size `nLots` lots by `sizeLot` units containing the numbers
#'         of \emph{L. monocytogenes} in/on fish fillets;}
#'      \item{`P`}{Mean prevalence of contaminated lots (scalar);}
#'      \item{`ProbUnitPos`}{Probability of individual lots being contaminated (vector).}
#'    }
#' @param pBrine Probability that a lot of fish fillets is salted by brine injection (as opposed to dry-salting) (scalar).
#' @param pccBrine Probability that the brine solution is contaminated with \emph{L. monocytogenes}  (scalar).
#' @param volInjMin (`ml`) minimum volume of brine solution injected in a fish fillet (scalar).
#' @param volInjMode (`ml`) most likely volume of brine solution injected in a fish fillet (scalar).
#' @param volInjMax (`ml`) maximum volume of brine solution injected in a fish fillet (scalar).
#' @param concBrineMin (`CFU/ml`) minimum concentration of \emph{L. monocytogenes} in contaminated brine solution (scalar).
#' @param concBrineMode (`CFU/ml`) most likely concentration of \emph{L. monocytogenes} in contaminated brine solution (scalar).
#' @param concBrineMax (`CFU/ml`) maximum concentration of \emph{L. monocytogenes} in contaminated brine solution (scalar).
#' @param pccSmearing Probability that cross-contamination with \emph{L. monocytogenes} occurs during dry salting, or smearing
#'  the fillets with sugar/spices, through tables or other surfaces (scalar).
#' @param trSmearingMean Mean parameter of the normal distribution representing the variability in the log 10 of the transfer coefficient
#'  of \emph{L. monocytogenes} cells from surfaces to fish fillets (scalar or vector).
#' @param trSmearingSd Standard deviation parameter of the normal distribution representing the variability in the log 10 of the transfer
#'  coefficient of \emph{L. monocytogenes} from surfaces to fish fillets (scalar or vector).
#' @param nSurface (CFU) Numbers of \emph{L. monocytogenes} cells on table or surfaces in contact with fish fillets while dry salting
#'   (scalar or vector).
#' @return A list of three elements:
#'     \describe{
#'              \item{`N`}{(`CFU`) A matrix of size nLots lots by sizeLot units containing the numbers of \emph{L. monocytogenes} in salted fish;}
#'              \item{`ProbUnitPos`}{Probability of individual lots being contaminated after salting (vector);}
#'              \item{`P`}{Mean prevalence of contaminated lots after salting (scalar).}
#'              }
#'
#' @author Ursula Gonzales-Barron \email{ubarron@ipb.pt}
#'
#' @keywords brine solution injection internal salt smearing external surface contamination transfer

#' @references
#'
#' \insertRef{Gudbjornsdottir2004}{qraLm}
#' \insertRef{Gudbjornsdottir2005}{qraLm}
#' \insertRef{Hoelzer2012}{qraLm}
#' \insertRef{stats}{qraLm}
#'
#' @importFrom stats rbinom
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @note The suggested value of \eqn{probCCDice\_brine=0.135} is taken from \insertCite{Gudbjornsdottir2005;textual}{qraLm} and
#' \insertCite{Gudbjornsdottir2004;textual}{qraLm}, who analysed the presence of \emph{L. monocytogenes} in brine solution, detecting
#' 3 positive samples out of 14, and 2 positive samples out of 23, respectively. The suggested parameters \eqn{TR\_smearing\_mean=-0.29}
#' and \eqn{TR\_smearing\_sd=0.31} for the normal distribution about the variability in the log 10 of the transfer coefficient of \emph{L. monocytogenes} were taken
#' from \insertCite{Hoelzer2012;textual}{qraLm}, to represent cross-contamination from board to meat.
#' The parameters of the Pert distributions about mean volume of brine solution injected in a fish fillet (`volInjMin`, `volInjMode` and `volInjMax`),
#' and about mean concentration of \emph{L. monocytogenes} in brine (`concBrineMin`, `concBrineMode` and `concBrineMax`) must be defined by the user and/or
#' tested in scenarios. Similarly, the values of `pBrine`, `Pcc_salting` and `nSurface` must be defined by the user and/or assessed in scenarios.
#'
#' @examples
#' nLots <- 100
#' sizeLot <- 100
#' pBrine <- 0.2
#' pccBrine <- 5 / 37
#' volInjMin <- 0.5
#' volInjMode <- 1.2
#' volInjMax <- 4.5
#' concBrineMin <- 0
#' concBrineMode <- 3
#' concBrineMax <- 40
#' pccSmearing <- 0.05 #  unlikely to occur
#' trSmearingMean <- -0.29 # TR board to meat
#' trSmearingSd <- 0.31 # TR board to meat
#' nSurface <- 200
#'
#' dat <- list(
#'   N = matrix(rpois(nLots * sizeLot, 25),
#'     nrow = nLots,
#'     ncol = sizeLot
#'   ),
#'   P = 0.05,
#'   ProbUnitPos = rep(0.05, nLots),
#'   nLots = 100,
#'   sizeLot = 100
#' )
#' Nf <- sfBrineORsaltCC(
#'   dat, pBrine, pccBrine,
#'   volInjMin, volInjMode, volInjMax,
#'   concBrineMin, concBrineMode, concBrineMax,
#'   pccSmearing, nSurface, trSmearingMean, trSmearingSd
#' )
#' hist(Nf$N)
sfBrineORsaltCC <- function(data = list(),
                            pBrine,
                            pccBrine,
                            volInjMin,
                            volInjMode,
                            volInjMax,
                            concBrineMin,
                            concBrineMode,
                            concBrineMax,
                            pccSmearing,
                            nSurface,
                            trSmearingMean,
                            trSmearingSd) {
  N <- data$N
  # Get the simulation dimension

  # if(missing(nLots)) nLots <- data$nLots #test if nLots was defined
  # if(is.null(nLots)) warning("Add 'nLots=#' to function arguments") #test again if nLots is defined
  #
  # if(missing(sizeLot)) sizeLot <- data$sizeLot #test if sizeLot was defined
  # if(is.null(sizeLot)) warning("Add 'sizeLot=#' to function arguments") #test again if sizeLot is defined

  nLots <- nrow(N)
  sizeLot <- ncol(N)
  # Sample the lots that will be brining (1) vs salting (0) as a function of pBrine
  # in the vector n_lot, 1=brining, 0=salting
  n_lot <- stats::rbinom(nLots, size = 1, prob = pBrine)
  index_brine <- which(n_lot == 1)
  index_salt <- which(n_lot == 0)

  # Lots of fish being injected with brine

  if (any(n_lot == 1)) { # some fishes are brined

    N_brine_in <- N[index_brine, ]
    ProbUnitPos_brine_in <- data$ProbUnitPos[index_brine]
    data_brine_in <- list(
      N = N_brine_in,
      ProbUnitPos = ProbUnitPos_brine_in,
      P = data$P
    )

    data_brine_out <- sfBriningCC(
      data = data_brine_in,
      pccBrine = pccBrine,
      volInjMin = volInjMin,
      volInjMode = volInjMode,
      volInjMax = volInjMax,
      concBrineMin = concBrineMin,
      concBrineMode = concBrineMode,
      concBrineMax = concBrineMax
    )

    N_brine_out <- data_brine_out$N
    ProbUnitPos_brine_out <- data_brine_out$ProbUnitPos * pBrine

    data$N[index_brine, ] <- N_brine_out
    data$ProbUnitPos[index_brine] <- ProbUnitPos_brine_out
  }

  # Lots of fish being dry-salted
  if (any(n_lot == 0)) { # some fishes are salted
    N_salt_in <- N[index_salt, ]
    ProbUnitPos_salt_in <- data$ProbUnitPos[index_salt]
    data_salt_in <- list(
      N = N_salt_in,
      ProbUnitPos = ProbUnitPos_salt_in,
      P = data$P
    )

    data_salt_out <- sfSmearingCC(
      data = data_salt_in,
      pccSmearing = pccSmearing,
      trSmearingMean = trSmearingMean,
      trSmearingSd = trSmearingSd,
      nSurface = nSurface
    )
    N_salt_out <- data_salt_out$N
    ProbUnitPos_salt_out <- data_salt_out$ProbUnitPos * (1 - pBrine)
    data$N[index_salt, ] <- N_salt_out
    data$ProbUnitPos[index_salt] <- ProbUnitPos_salt_out
  }
  # Calculating P
  data$P <- 1 - (pBrine * (1 - data$P) * (1 - pccBrine) +
    (1 - pBrine) * (1 - data$P) * (1 - pccSmearing))
  data$saltingType <- ifelse(n_lot == 1, "brined", "salted") # 1 for brined-injected lots and 0 dry-salted lots
  return(data)
}
