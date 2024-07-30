#' @title Packaging of slices of ready-to-eat seafood
#'
#' @description
#' The [sfPackaging()] function is a simple function that groups consecutive slices of recently-sliced RTE seafood into packages of
#'  finished product.
#'
#'
#' @param data a list with a minimum of elements:
#' \describe{
#'       \item{`N`}{(`CFU`) A matrix of size number of lots by number of slices, containing the numbers of \emph{L. monocytogenes} per slice unit;}
#'       \item{`P`}{Mean prevalence of contaminated slices (scalar).}
#'       }
#' @param slicesPerPack Number of slices per pack (scalar).
#'
#' @return the data object with modified:
#'     \describe{
#'              \item{`N`}{(`CFU`) A matrix of size number of lots by number of packs, containing the numbers of \emph{L. monocytogenes}
#'               per pack unit.}
#'              }
#'
#' @author Regis Pouillot \email{rpouillot.work@gmail.com}
#'
#' @keywords grouping package
#'
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @note If the actual number of slices is not a multiple of the number of slices per pack, some slices will be discarded
#' with a warning. The prevalence `P` is not intended to change since at that step, zeroes will  be retained. No cross-contamination
#' is assumed during packaging.
#'
#' @examples
#' columns <- 200
#' rows <- 10
#' slicesPerPack <- 5
#' dat <- Lot2LotGen(
#'                   nLots = 50,
#'                   sizeLot = 100,
#'                   unitSize = 500,
#'                   betaAlpha = 0.5112,
#'                   betaBeta = 9.959,
#'                   C0MeanLog = 1.023,
#'                   C0SdLog = 0.3267,
#'                   propVarInter = 0.7
#'                   )
#' dat$N[1, ] <- 1 # To check packages
#' N1 <- sfPackaging(dat, 5)
#' # Check the change of dimension
#' dim(dat$N)
#' dim(N1$N)
#' # Check the mass balance of slices
#' sum(dat$N)
#' sum(N1$N)
#' # Check the packaging (should be TRUE)
#' all(N1$N[1, ] == slicesPerPack)
#' N2 <- sfPackaging(dat, 2)
#'
sfPackaging <- function(data = list(),
                        slicesPerPack) {
  N <- data$N

  # Get the dimensions
  nSlices <- ncol(N)
  size_mc <- nrow(N)

  nPacks <- nSlices %/% slicesPerPack
  remaining_Slices <- nSlices %% slicesPerPack
  if (remaining_Slices != 0) {
    warning("the number of slices is not a multiple
    of the number of slices per pack. Some slices will be discarded")
    # new number of slices
    nSlices <- slicesPerPack * (nPacks)
    N <- N[, 1:nSlices]
  }

  # Through proper data recycling, build the packs
  Npack <- matrix(t(N), ncol = slicesPerPack, nrow = nPacks * size_mc, byrow = TRUE)
  # Sum the number of Lm per pack
  Npack <- rowSums(Npack)
  # rebuild the lots
  Npack <- matrix(t(Npack), ncol = nPacks, nrow = size_mc, byrow = TRUE)

  # test to show that it works. Leave as comment or remove
  # size_mc=6; slicesPerPack=5; nPacks=4;
  # N <- matrix(1:size_mc,ncol=slicesPerPack*nPacks,nrow=size_mc) +
  #   matrix(rep((1:nPacks)/10, each = slicesPerPack),ncol=slicesPerPack*nPacks,nrow=size_mc, byrow=TRUE);
  # print(N)
  # Npack <- matrix(t(N), ncol=slicesPerPack, nrow=nPacks*size_mc, byrow=TRUE)
  # Npack <- rowSums(Npack)
  # matrix(t(Npack), ncol=nPacks, nrow=size_mc, byrow=TRUE)

  # output
  unitSize <- floor(data$unitSize * slicesPerPack) # update unitSize and add it to the dataset
  N <- Npack
  
  lotMeans <- rowMeans(N / unitSize, na.rm = TRUE)
  unitsCounts <- c((data$ProbUnitPos/mean(data$ProbUnitPos)) * (N / unitSize))
  
  data$lotMeans <- lotMeans
  data$unitsCounts <- unitsCounts 
  data$unitSize <- unitSize
  data$N <- N
  data$sizeLot <- ncol(Npack) # update the sizeLot to the number of packs
  return(data)
}