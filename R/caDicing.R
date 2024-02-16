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
#' @param nLots Number of lots (scalar).
#' @param sizeLot Number of cantaloupes in a production lot (scalar).
#' @param sizeSublot Number of cantaloupes to be diced in a sublot or processing lot (the sizeSublot should be multiple of sizeLot) (scalar).
#' @param cantaSurface (\eqn{cm^2}) Surface area of a cantaloupe (scalar or vector).
#' @param cantaWeight (`g`) weight of a cantaloupe
#' @param pulpYield (`%`) Cantaloupe pulp yield (scalar or vector).
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
#' 
#' \insertRef{Ukuku2002}{qraLm}
#' 
#' \insertRef{Ukuku2012}{qraLm}
#' 
#' @importFrom mc2d rpert
#' @importFrom utils data
#' 
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
#' nLots        <- 1000
#' sizeLot      <- 100
#' sizeSublot   <- 20
#' cantaSurface <- 780
#' pulpYield    <- 0.75
#' cantaWeight  <- 1000
#' data         <- list(N = matrix(rpois(sizeLot*nLots, 100),
#'                                 ncol=sizeLot, 
#'                                 nrow=nLots),
#'                                 P = 0.16,
#'                       cantaWeight=cantaWeight,
#'                       nLots=nLots,
#'                       sizeLot=sizeLot)
#' str(data)
#' CantaDices <- caDicing (data,
#'                         minTR=0.087,
#'                         modeTR=0.55,
#'                         maxTR=2.82,
#'                         pulpYield=pulpYield,
#'                         cantaSurface=cantaSurface,
#'                         sizeSublot=sizeSublot)
#' hist(CantaDices$N)
#'
caDicing <- function(data=list(), 
                      minTR=0.087, 
                      modeTR=0.55,
                      maxTR=2.82,
                      cantaSurface=580, # 
                      pulpYield=0.6, # values from 0.6 to 0.8 paper
                      cantaWeight=NULL,
                      nLots=NULL,
                      sizeLot=NULL,
                      sizeSublot=20){
  if(missing(nLots)) nLots <- data$nLots #test if nLots was defined
  if(is.null(nLots)) warning("Add 'nLots=#' to function arguments") #test again if nLots is defined 
  
  if(missing(sizeLot)) sizeLot <- data$sizeLot #test if sizeLot was defined
  if(is.null(sizeLot)) warning("Add 'sizeLot=#' to function arguments") #test again if sizeLot is defined 
  
  if(missing(cantaWeight)) cantaWeight <- data$cantaWeight #test if cantaWeight was defined
  if(is.null(cantaWeight)) warning("Add 'cantaWeight=#' to function arguments") #test again if cantaWeight is defined 
  
  cantaPulp <- cantaWeight*pulpYield # variation between 60 and 80%
  
  Nt <- data$N
  # Verifying the exact division of sizeLot/sizeSublot
  if(sizeLot %% sizeSublot != 0)  stop("sizeSublot must be a multiple of sizeLot")
  
  newMC <- sizeLot*nLots/sizeSublot
  N_sublots <- matrix(t(Nt), ncol=sizeSublot, 
                      nrow = newMC, byrow=TRUE)
  
  #Applying TR (%) of rind (CFU/cm2) to dices of same melon (`CFU/g`)
  #melons with no contamination produce non-contaminated dices
  TR <- matrix(mc2d::rpert(sizeLot*nLots, minTR, modeTR, maxTR, shape=4), 
               ncol=sizeSublot, nrow = newMC, byrow=TRUE)
  N_trans <- round(cantaPulp*(TR*(N_sublots/cantaSurface)/100),0)
  
  # REMOVING sublots OF ZEROES AND ALTERING PREVALENCE
  # therefore necessary to get back to original matrix arrangement of lots
  #N_trans_lot <- matrix(t(N_trans), ncol = sizeLot, nrow = nLots, byrow=TRUE) 
  Pi_0_index <- (rowSums(N_trans) == 0)
  Pi_0 <- mean(Pi_0_index)

  # # LG : I had this condition to consider the case where no Lm are transferred. Here I generate 1 cell 
  # if(Pi_0==1){Pi_0_index[1]=FALSE
  # N_trans_lot[1,1]=1} 
  # 
  N_trans[Pi_0_index,] <- N_trans[sample(which(!Pi_0_index), sum(Pi_0_index), replace=TRUE),]
  # 
  # # Back to the sublots matrix but now only with contaminated lots ##### LG There I don't understand why there is no resampling
  # N_out <- matrix(t(N_trans_lot), ncol=sizeSublot, 
  #                 nrow = newMC, byrow=TRUE)
  
  data$P=data$P*(1-Pi_0) #prevalence at the sublot level
  data$N <- N_trans
  data$sizeSublot <- sizeSublot
  data$cantaPulp <- cantaPulp
  data$pulpYield <- pulpYield
  return(data)
}
