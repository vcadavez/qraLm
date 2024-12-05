#' @title Dicing of cantaloupe for preparation of RTE pre-cut cantaloupe
#'
#' @description
#' The function [caDicing()] simulates the transfer of \emph{L. monocytogenes} from rind to flesh during dicing
#' of cantaloupe at processing environment. It is assumed that every cantaloupe is separately diced,
#' and if contaminated on the rind, a fraction of \emph{L. monocytogenes} cells is transferred to the dices.
#' Notice that this function does not consider cross-contamination from dicing machine or from knives.
#' The transfer rate (`TR`) in % (`CFU/g` dices per \eqn{CFU/cm^2\ rind)*100} has been modelled as a Pert distribution
#' with parameters `minTR`, `modeTR` and `maxTR`.
#'
#' @param data a list of:
#' \describe{
#'     \item{`N`}{(`CFU`) A matrix of size `nLots` lots by `sizeLot` units representing the numbers of \emph{L. monocytogenes}
#'       on cantaloupe rind, from contaminated lots;}
#'     \item{`P`}{Prevalence of contaminated harvested lots (scalar).}
#'         }
#' @param sizeSublot Number of cantaloupes to be diced in a sublot or processing lot (the sizeSublot should be multiple of sizeLot) (scalar).
#' @param cantaSurface (\eqn{cm^2}) Surface area of a cantaloupe (scalar or vector).
#' @param cantaRindFree (`g`) Weight of a seedless rind-free cantaloupe equivalent to the weight of dices from one cantaloupe (scalar or vector).
#' @param minTR (`%`) Minimum transfer rate from rind to flesh (\eqn{default=0.087} for use in a Pert distribution) (scalar).
#' @param modeTR (`%`) Mode of the transfer rate from rind to flesh (\eqn{default=0.55} for use in a Pert distribution) (scalar).
#' @param maxTR (`%`) Maximum transfer rate from rind to flesh (\eqn{default=2.82} for use in a Pert distribution) (scalar).
#'
#' @return A list of two elements:
#'     \describe{
#'        \item{`N`}{(`CFU`) A matrix in the sublot arrangement size `newMC` sublots by sublot_size portions of dices from one cantaloupe
#'           representing the numbers of \emph{L. monocytogenes} in dices, from contaminated production lots.}
#'        \item{`P`}{Prevalence of sublots of dices contaminated with \emph{L. monocytogenes}(scalar).}
#'        }
#'
#' @author Ursula Gonzales-Barron \email{ubarron@ipb.pt}
#'
#' @keywords melon transfer rind cutting flesh
#'
#' @references
#' \insertRef{mc2d}{qraLm}
#' \insertRef{Ukuku2002}{qraLm}
#' \insertRef{Ukuku2012}{qraLm}
#'
#' @importFrom mc2d rpert
#' @importFrom utils data
#'
#' @export
#'
#' @note The matrix `N` is returned in a sublot arrangement, meaning that the number of rows corresponds to the number of sublots,
#'  and the number of columns the number of cantaloupes processed in a sublot. This is done because the production lot size is not
#'  necessarily the same as the processing lot size. Therefore, the numbers and prevalence outputs are given in the sublot basis (processing lot).
#' The variability in the transfer rate from rind to flesh (`CFU/g` dices per \eqn{CFU/cm^2} rind) was modelled as a Pert distribution using data
#' digitised from \insertCite{Ukuku2002;textual}{qraLm} and \insertCite{Ukuku2012;textual}{qraLm}.
#'
#' @examples
#' nLots <- 1000
#' sizeLot <- 1000
#' df <- list(N = matrix(rpois(sizeLot*nLots, 200), 
#'            ncol=sizeLot, nrow=nLots),
#'            P = .16)
#' CantaDices <- caDicing (df,
#'                         cantaSurface=780,
#'                         cantaRindFree=950,
#'                         sizeSublot=100)
#' hist(CantaDices$N)
#'
caDicing <- function(data = list(),
                     minTR = 0.087,
                     modeTR = 0.55,
                     maxTR = 2.82,
                     cantaSurface = 580, 
                     cantaRindFree = 950,
                     sizeSublot) {
  
  nLots <- nrow(data$N)
  sizeLot <- ncol(data$N)
  
  Nt <- data$N
  
  # Verifying the exact division of sizeLot/sizeSublot
  if (sizeLot %% sizeSublot != 0) stop("sizeSublot must be a multiple of sizeLot")

  newMC <- sizeLot * nLots / sizeSublot
  N_sublots <- matrix(
                      t(Nt),
                      ncol = sizeSublot,
                      nrow = newMC, 
                      byrow = TRUE
                      )

  # Applying TR (%) of rind (CFU/cm2) to dices of same melon (`CFU/g`)
  # melons with no contamination produce non-contaminated dices
  TR <- matrix(mc2d::rpert(sizeLot * nLots, minTR, modeTR, maxTR, shape = 4),
    ncol = sizeSublot, nrow = newMC, byrow = TRUE
  )
  N_trans <- round(cantaRindFree * (TR * (N_sublots / cantaSurface) / 100), 0)

  # REMOVING sublots OF ZEROES AND ALTERING PREVALENCE
  # therefore necessary to get back to original matrix arrangement of lots
  # N_trans_lot <- matrix(t(N_trans), ncol = sizeLot, nrow = nLots, byrow=TRUE)
  Pi_0_index <- (rowSums(N_trans) == 0)
  Pi_0 <- mean(Pi_0_index)

  # # LG : I had this condition to consider the case where no Lm are transferred. Here I generate 1 cell
  # if(Pi_0==1){Pi_0_index[1]=FALSE
  # N_trans_lot[1,1]=1}
  #
  N_trans[Pi_0_index, ] <- N_trans[sample(which(!Pi_0_index), sum(Pi_0_index), replace = TRUE), ]
  #
  # # Back to the sublots matrix but now only with contaminated lots ##### LG There I don't understand why there is no resampling
  # N_out <- matrix(t(N_trans_lot), ncol=sizeSublot,
  #                 nrow = newMC, byrow=TRUE)

  P <- data$P * (1 - Pi_0) # prevalence at the sublot level
  N <- N_trans
  
  lotMeans <- rowMeans(N / cantaRindFree, na.rm = TRUE)
  unitsCounts <- c(N / cantaRindFree)
  
  data$lotMeans <- lotMeans
  data$unitsCounts <- unitsCounts
  data$P <- P
  data$N <- N
  data$sizeSublot <- sizeSublot
  data$sizeLot <- sizeLot
  data$cantaRindFree <- cantaRindFree

  return(data)
}
