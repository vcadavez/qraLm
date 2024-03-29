% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sfPackaging.R
\name{sfPackaging}
\alias{sfPackaging}
\title{Packaging of slices of ready-to-eat seafood}
\usage{
sfPackaging(data = list(), slicesPerPack)
}
\arguments{
\item{data}{a list with a minimum of elements:
\describe{
\item{\code{N}}{(\code{CFU}) A matrix of size number of lots by number of slices, containing the numbers of \emph{L. monocytogenes} per slice unit;}
\item{\code{P}}{Mean prevalence of contaminated slices (scalar).}
}}

\item{slicesPerPack}{Number of slices per pack (scalar).}
}
\value{
the data object with modified:
\describe{
\item{\code{N}}{(\code{CFU}) A matrix of size number of lots by number of packs, containing the numbers of \emph{L. monocytogenes}
per pack unit.}
}
}
\description{
The \code{\link[=sfPackaging]{sfPackaging()}} function is a simple function that groups consecutive slices of recently-sliced RTE seafood into packages of
finished product.
}
\note{
If the actual number of slices is not a multiple of the number of slices per pack, some slices will be discarded
with a warning. The prevalence \code{P} is not intended to change since at that step, zeroes will  be retained. No cross-contamination
is assumed during packaging.
}
\examples{
columns <- 200
rows <- 10
slicesPerPack <- 5
N0 <- list(N = matrix(rpois(columns * rows, 3),
  nrow = rows, ncol = columns
))
N0$N[1, ] <- 1 # To check packages
N1 <- sfPackaging(N0, 5)
# Check the change of dimension
dim(N0$N)
dim(N1$N)
# Check the mass balance of slices
sum(N0$N)
sum(N1$N)
# Check the packaging (should be TRUE)
all(N1$N[1, ] == slicesPerPack)
N2 <- sfPackaging(N0, 6)

}
\author{
Regis Pouillot \email{rpouillot.work@gmail.com}
}
\keyword{grouping}
\keyword{package}
