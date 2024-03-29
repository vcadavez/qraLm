% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fvCooking.R
\name{fvCooking}
\alias{fvCooking}
\title{Microbial reduction due to cooking at home}
\usage{
fvCooking(
  data = list(),
  nLots = NULL,
  sizeLot = NULL,
  pCooked = 0.825,
  minCook = 1,
  modeCook = 5,
  maxCook = 9
)
}
\arguments{
\item{data}{a list of:
\describe{
\item{\code{N}}{(\code{CFU}) A matrix containing the numbers of \emph{L. monocytogenes} per portion of defrosted or frozen vegetables.}
\item{\code{ProbUnitPos}}{Lot-specific probability of contaminated portions or servings, defrosted or not (vector).}
\item{\code{P}}{Mean prevalence of contaminated portions of defrosted or frozen vegetables.}
}}

\item{nLots}{Number of lots sampled or size of the Monte Carlo simulation (scalar).}

\item{sizeLot}{Number of units or portions produced in a lot (scalar).}

\item{pCooked}{Probability of cooking the defrosted or frozen vegetables for consumption}

\item{minCook}{(\code{log10}) Minimum value of the triangular distribution representing the variability in the reduction of \emph{L. monocytogenes} in the event of cooking}

\item{modeCook}{(\code{log10}) Mode value of the triangular distribution representing the variability in the reduction of \emph{L. monocytogenes} in the event of cooking}

\item{maxCook}{(\code{log10}) Maximum value of the triangular distribution representing the variability in the reduction of \emph{L. monocytogenes} in the event of cooking}
}
\value{
A list of three elements:
\describe{
\item{\code{N}}{(\code{CFU}) A matrix of size \code{nLots} lots by \code{sizeLot} units containing the numbers of \emph{L. monocytogenes}
in the portions or servings to be consumed;}
\item{\code{ProbUnitPos}}{Lot-specific probability of contaminated portions to be consumed (vector);}
\item{\code{P}}{Mean prevalence of contaminated portions or servings to be consumed (scalar).}
}
}
\description{
The \code{\link[=fvCooking]{fvCooking()}} function simulates the reduction in \emph{L. monocytogenes} numbers in defrosted (or still frozen) vegetables due to cooking.
The function can represent a scenario where the consumer gives the non-RTE frozen vegetables a non-intended use such as direct (uncooked) consumption in salads, smoothies, etc.
In that case, the probability of cooking in the argument should be set to a value different from one.
The variability in the effect of cooking defrosted (or frozen) vegetables is represented by a triangular distribution,
as assumed in \insertCite{EFSA2020;textual}{qraLm}.
}
\note{
To represent variability in the effect of cooking defrosted (or frozen) vegetables, \insertCite{EFSA2020;textual}{qraLm} assumed a
triangular distribution, with parameters \eqn{minCook=1}, \eqn{modeCook=5} and \eqn{maxCook=9}. According to \insertCite{FSAI2022;textual}{qraLm}, 84-89\% of the respondants recognised that
frozen vegetables need to be cooked prior to consumption, whereas\insertCite{Willis2020;textual}{qraLm} found that 19\% of the non-RTE frozen vegetables packages
have no indication for cooking. An average value for the probability of cooking of \eqn{p\_cooked=0.825} can be used in this function.
}
\examples{
library(mc2d)
# N0 <- list(N=matrix(stats::rpois(2000, 15),
#            ncol=200, nrow=10),
#            P=0.12,
#            ProbUnitPos = 0.7)
#
# N_postcooking=fvCooking(N0,
#                          pCooked=0.825,
#                          minCook=1,
#                          modeCook=5,
#                          maxCook=9)
# hist(N_postcooking$N)

}
\references{
\insertRef{mc2d}{qraLm}

\insertRef{EFSA2020}{qraLm}

\insertRef{Willis2020}{qraLm}

\insertRef{FSAI2022}{qraLm}
}
\author{
Laurent Guillier
}
\keyword{non-intended}
\keyword{preparation}
\keyword{use}
