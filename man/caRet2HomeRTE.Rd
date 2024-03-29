% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/caRet2HomeRTE.R
\name{caRet2HomeRTE}
\alias{caRet2HomeRTE}
\title{Transport of RTE diced cantaloupe packs from retail to home}
\usage{
caRet2HomeRTE(
  data = list(),
  MPD = NULL,
  Tmin = -2.0196,
  tempMin,
  tempMode,
  tempMax,
  timeShape,
  timeScale
)
}
\arguments{
\item{data}{a list of:
\describe{
\item{N}{(\code{CFU}) A matrix containing the numbers of \emph{L. monocytogenes} in packs of RTE diced cantaloupe at the
end of retail, from contaminated lots;}
\item{P}{Prevalence of contaminated lots (scalar);}
\item{lnQt}{Natural log of the \code{Q} parameter at the end of retail (matrix);}
\item{lotEGR5}{(\eqn{h^{-1}}) Growth rate of \emph{L. monocytogenes} in cantaloupe flesh specific to every lot (vector);}
\item{unitSize}{(\code{g}) Weight of a pack of cantaloupe dices.}
}}

\item{MPD}{(log10 CFU/g) Maximum population density of \emph{L. monocytogenes} in cantaloupe flesh (scalar).}

\item{Tmin}{(\eqn{^\circ C}) Nominal minimum temperature for growth of \emph{L. monocytogenes} in cantaloupe flesh (suggested \eqn{default=-2.0196\ ^\circ C}) (scalar).}

\item{tempMin}{(\eqn{^\circ C}) Minimum transportation temperature (scalar).}

\item{tempMode}{(\eqn{^\circ C}) Mode of the transportation temperature (scalar).}

\item{tempMax}{(\eqn{^\circ C}) Maximum transportation temperature (scalar).}

\item{timeShape}{Shape parameter of the gamma distribution representing transportation time in hours (scalar) (suggested \eqn{default=6.2}).}

\item{timeScale}{Scale parameter of the gamma distribution representing transportation time in hours (scalar) (suggested \eqn{default=8.2}).}
}
\value{
A list of five elements:
\describe{
\item{N}{(\code{CFU}) A matrix containing the numbers of \emph{L. monocytogenes} in packs of RTE diced cantaloupe at the end of transportation,
from contaminated lots;}
\item{P}{Prevalence of RTE diced cantaloupe lots contaminated with \emph{L. monocytogenes} (scalar);}
\item{lnQt}{Natural log of the \code{Q} parameter at the end of transportation (matrix);}
\item{lotEGR5}{(\eqn{h^{-1}}) Growth rate of \emph{L. monocytogenes} in cantaloupe flesh specific to every lot (vector);}
\item{unitSize}{(\code{g}) Weight of a pack of cantaloupe dices.}
}
}
\description{
The function \code{\link[=caRet2HomeRTE]{caRet2HomeRTE()}} simulates the growth of \emph{L. monocytogenes} in RTE diced cantaloupe during transport
from retail to home, and is based on the function \code{\link[=caGrowthBaranyi]{caGrowthBaranyi()}}. The algorithm samples transportation time and temperature
at the unit level since they depend on the consumer. The input \code{data} provides the algorithm with the lot-specific values of \code{EGR5}
and the unit-specific values of \code{lnQt} obtained from the previous logistics stage. A Gamma distribution represent
the variability in transport time, whereas a Pert distribution the variability in transport temperature.
}
\note{
The parameter \eqn{Tmin=-2.0196\ ^\circ C} was determined from fitting a square-root model to data extracted from multiple sources
(refer to the function \code{\link[=caGrowthBaranyi]{caGrowthBaranyi()}}). Parameters for the distributions of the transportation conditions
should be defined by the user and/or tested in scenarios.
In addition to the final \emph{L. monocytogenes} numbers, the function also returns the values of \code{lotEGR5} and \code{lnQt}
so that the \emph{L. monocytogenes} growth could be followed up in subsequent logistics stages.
}
\examples{
tempMin <- 3
tempMode <- 5
tempMax <- 12
Tmin <- -2.0196
nLots <- 1000
sizeLot <- 250
N <- matrix(230, nLots, sizeLot)
N[5, ] <- 0
lnQt <- matrix(2.3, nLots, sizeLot)
lnQt[5, ] <- 0
dat <- list(
  N = N, lnQt = lnQt,
  lotEGR5 = extraDistr::rtnorm(nLots, 0.03557288, 0.004, a = 0),
  P = 0.4, unitSize = 200
)

ArriveHome <- caRet2HomeRTE(dat,
  MPD = 8.5,
  Tmin - 2.0196,
  tempMin = tempMin,
  tempMode = tempMode,
  tempMax = tempMax,
  timeShape = 6.2,
  timeScale = 8.2
)
hist(ArriveHome$N)

}
\references{
\insertRef{mc2d}{qraLm}

\insertRef{stats}{qraLm}
}
\author{
Ursula Gonzales-Barron \email{ubarron@ipb.pt}
}
\keyword{Baranyi}
\keyword{growth}
\keyword{lag-phase}
\keyword{logistics}
\keyword{stochastic}
