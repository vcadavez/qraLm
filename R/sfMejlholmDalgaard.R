#' @title Secondary growth rate model of \emph{L. monocytogenes} in RTE seafood
#'
#' @description
#' The function [sfMejlholmDalgaard()] estimates the growth rate of \emph{L. monocytogenes} in RTE seafood, based on the
#' predictive secondary model published by \insertCite{Mejlholm2009;textual}{qraLm}, and later validated in RTE foods including
#' seafood (\insertCite{Mejlholm2010}{qraLm}).
#'
#' @param Temp (\eqn{^\circ C}) Storage temperature of the RTE product
#' @param aw Water activity of the RTE product. If NULL, it will evaluate from NaCl
#' @param NaCl (%) Salt content of the RTE product.
#' @param pH pH of the RTE product.
#' @param P (ppm) Phenol concentration in the RTE product.
#' @param CO2equi (proportion) CO2 content in atmosphere in the RTE product (e.g. 0.25).
#' @param NIT (ppm) Nitrites concentration in the RTE product.
#' @param aaWph (ppm) Acetic acid concentration in the RTE product.
#' @param baWph (ppm) Benzoic acid concentration in the RTE product.
#' @param caWph (ppm) Citric acid concentration in the RTE product.
#' @param daWph (ppm) Diacetate concentration in the RTE product.
#' @param laWph (ppm) Lactic acid concentration in the RTE product.
#' @param saWph (ppm) Sorbic acid concentration in the RTE product.
#' @param Tmin (\eqn{^\circ C}) Minimum temperature for growth.
#' @param Tref (\eqn{^\circ C}) Reference temperature.
#' @param awmin  Minimum water activity for growth.
#' @param pHmin  Minimum pH for growth.
#' @param pheMax  (ppm) MIC for phenols.
#' @param NITmax (ppm) MIC for nitrites.
#' @param CO2max (ppm) MIC for `CO_2`  .
#' @param micLACu (mM) MIC for undissociated lactic acid.
#' @param micDACu (mM) MIC for undissociated diacetate.
#' @param micAACu (mM) MIC for undissociated acetic acid.
#' @param micBACu (mM) MIC for undissociated benzoic acid.
#' @param micCACu (mM) MIC for undissociated citric acid.
#' @param micSACu (mM) MIC for undissociated sorbic acid.
#' @param mumaxref (1/h) Maximum growth rate at the reference temperature `Tref`.
#'
#' @return mumax (`1/h`) Specific growth rate of \emph{L. monocytogenes} in RTE seafood at the given intrinsic and extrinsic characteristics (scalar, vector or matrix).
#'
#' @author Laurent Guillier
#'
#' @keywords secondary growth model lightly-preserved seafood
#'
#' @references
#'
#' \insertRef{Mejlholm2009}{qraLm}
#'
#' \insertRef{Mejlholm2010}{qraLm}
#'
#' @export
#'
#' @note
#' The parameters of the growth rate model are set by default to those fitted and validated by \insertCite{Mejlholm2010;textual}{qraLm}.
#' This model can also be found in the user friendly software FSSP: [](http://fssp.food.dtu.dk/Help/Listeria/Lm-LAB/lm-lab.htm).
#'
#' @examples
#' sfMejlholmDalgaard(Temp = c(-3, 15, 12), aw = 0.98, P = 3, saWph = 6000)
#'
sfMejlholmDalgaard <- function(Temp,
                               aw = NULL,
                               NaCl = 0,
                               pH = 7,
                               P = 0,
                               CO2equi = 0,
                               NIT = 0,
                               aaWph = 0,
                               baWph = 0,
                               caWph = 0,
                               daWph = 0,
                               laWph = 0,
                               saWph = 0,
                               Tmin = -2.83,
                               Tref = 25,
                               awmin = 0.923,
                               pHmin = 4.97,
                               pheMax = 32,
                               NITmax = 350,
                               CO2max = 3140,
                               micLACu = 3.79,
                               micDACu = 4.8,
                               micAACu = 10.3,
                               micBACu = 0.349,
                               micCACu = 2.119,
                               micSACu = 1.896,
                               mumaxref = 0.419) {
  # Conversion of factor's units to the ones used in the equations
  # CO2
  H1 <- exp(-6.8346 + 12817 / (Temp + 273.15) - 3766800 / (Temp + 273.15)^2 + 2.997e8 / (Temp + 273.15)^3) # Henrys's constant
  H2 <- 101323 * 2.4429 / H1 # Henrys's constant
  CO2 <- CO2equi * H2

  if (is.null(aw)) aw <- 0.999489 - 0.005179 * NaCl - 0.0001272 * NaCl^2

  # Organic acids: assessment of the undissociated forms
  AA_total_mM <- aaWph / 60.05
  AACu <- AA_total_mM / (1 + 10^(pH - 4.76))
  BA_total_mM <- baWph / 122.12
  BACu <- BA_total_mM / (1 + 10^(pH - 4.19))
  CA_total_mM <- caWph / 192.13
  CACu <- CA_total_mM / (1 + 10^(pH - 3.13))
  DA_total_mM <- daWph / 119.1
  DACu <- DA_total_mM / (1 + 10^(pH - 4.76))
  LA_total_mM <- laWph / 90.08
  LACu <- LA_total_mM / (1 + 10^(pH - 3.86))
  SA_total_mM <- saWph / 112.1
  SACu <- SA_total_mM / (1 + 10^(pH - 4.76))


  # Set input condition values to almost (esp) minimal cardinal values/MICs if they are inferior to them (avoid warnings for some phi terms and helps to return 0 to mumax)
  eps <- 10^-9
  Temp <- pmax(Temp, Tmin + eps)
  pH <- pmax(pH, pHmin + eps)
  aw <- pmax(aw, awmin + eps)
  CO2 <- pmin(CO2, CO2max - eps)
  P <- pmin(P, pheMax - eps)
  NIT <- pmin(NIT, NITmax - eps)
  AACu <- pmin(AACu, micAACu - eps)
  BACu <- pmin(BACu, micBACu - eps)
  CACu <- pmin(CACu, micCACu - eps)
  DACu <- pmin(DACu, micDACu - eps)
  LACu <- pmin(LACu, micLACu - eps)
  SACu <- pmin(SACu, micSACu - eps)

  ## Equations for phi terms for each environmental parameter
  phi_temp <- (1 - ((Temp - Tmin) / (Tref - Tmin)))^2
  phi_aw <- (1 - sqrt((aw - awmin) / (1 - awmin)))^2
  phi_pH <- (1 - sqrt(1 - 10^(pHmin - pH)))^2
  phi_phenol <- (1 - sqrt((pheMax - P) / pheMax))^2
  phi_NIT <- (1 - ((NITmax - NIT) / NITmax))^2
  phi_CO2 <- (1 - sqrt((CO2max - CO2) / CO2max))^2
  phi_acids <- (1 - ((1 - sqrt(LACu / micLACu)) * (1 - sqrt(DACu / micDACu)) * (1 - sqrt(AACu / micAACu)) * (1 - (BACu / micBACu)) *
    (1 - (CACu / micCACu)) * (1 - (SACu / micSACu))))^2

  ## Equations for phi terms for each environmental parameter
  # cartesian product with all environmental parameters: (total 7 phi calculated)
  # (1-phi_temp)*(1-phi_aw)*(1-phi_pH)*(1-phi_phenol)*(1-phi_NIT)*(1-phi_CO2)*(1-phi_acids)
  psi_temp <- phi_temp / ((1 - phi_aw) * (1 - phi_pH) * (1 - phi_phenol) * (1 - phi_NIT) * (1 - phi_CO2) * (1 - phi_acids))
  psi_aw <- phi_aw / ((1 - phi_temp) * (1 - phi_pH) * (1 - phi_phenol) * (1 - phi_NIT) * (1 - phi_CO2) * (1 - phi_acids))
  psi_pH <- phi_pH / ((1 - phi_temp) * (1 - phi_aw) * (1 - phi_phenol) * (1 - phi_NIT) * (1 - phi_CO2) * (1 - phi_acids))
  psi_phenol <- phi_phenol / ((1 - phi_temp) * (1 - phi_aw) * (1 - phi_pH) * (1 - phi_NIT) * (1 - phi_CO2) * (1 - phi_acids))
  psi_NIT <- phi_NIT / ((1 - phi_temp) * (1 - phi_aw) * (1 - phi_pH) * (1 - phi_phenol) * (1 - phi_CO2) * (1 - phi_acids))
  psi_CO2 <- phi_CO2 / ((1 - phi_temp) * (1 - phi_aw) * (1 - phi_pH) * (1 - phi_phenol) * (1 - phi_NIT) * (1 - phi_acids))
  psi_acids <- phi_acids / ((1 - phi_temp) * (1 - phi_aw) * (1 - phi_pH) * (1 - phi_phenol) * (1 - phi_NIT) * (1 - phi_CO2))

  psi <- (psi_temp + psi_aw + psi_pH + psi_phenol + psi_NIT + psi_CO2 + psi_acids) / 2
  psi <- ifelse(psi < 0, "", psi)

  # Equation for xi value
  xi <- ifelse(psi >= 1, 0, ifelse(psi <= 0.5, 1, ((1 - psi) * 2)))

  mumax <- mumaxref *
    ((Temp - Tmin) / (Tref - Tmin))^2 *
    ((aw - awmin) / (1 - awmin)) *
    (1 - 10^(pHmin - pH)) *
    (1 - (LACu / micLACu)) *
    ((pheMax - P) / pheMax) *
    ((NITmax - NIT) / NITmax)^2 *
    ((CO2max - CO2) / CO2max) *
    (1 - sqrt(DACu / micDACu)) *
    (1 - sqrt(AACu / micAACu)) *
    (1 - (BACu / micBACu)) *
    (1 - (CACu / micCACu)) *
    (1 - (SACu / micSACu)) *
    xi
  return(mumax)
}
