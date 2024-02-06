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
#' N0 <- list(N = matrix(rpois(columns * rows, 3),
#'   nrow = rows, ncol = columns
#' ))
#' N0$N[1, ] <- 1 # To check packages
#' N1 <- sfPackaging(N0, 5)
#' # Check the change of dimension
#' dim(N0$N)
#' dim(N1$N)
#' # Check the mass balance of slices
#' sum(N0$N)
#' sum(N1$N)
#' # Check the packaging (should be TRUE)
#' all(N1$N[1, ] == slicesPerPack)
#' N2 <- sfPackaging(N0, 6)
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

  data$unitSize <- floor(data$unitSize * slicesPerPack) # update unitSize and add it to the dataset

  data$N <- Npack
  data$sizeLot <- ncol(Npack) # update the sizeLot to the number of packs
  return(data)
}
