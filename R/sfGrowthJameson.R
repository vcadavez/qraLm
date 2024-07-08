#' @title Growth model of \emph{L. monocytogenes} in RTE seafood including background flora competition
#'
#' @description
#' The [sfGrowthJameson()] function is used to simulate the growth of \emph{L. monocytogenes} in RTE seafood, as affected by lactic acid bacteria (LAB),
#' according to the Jameson-effect competition model published in \insertCite{Gimenez2004;textual}{qraLm}.
#'
#' @param time (h) Storage time of a pack of RTE seafood
#' @param N0Lm (CFU) Numbers of \emph{L. monocytogenes} in RTE seafood at time=0
#' @param N0LAB (CFU) Numbers of LAB in RTE seafood at time=0
#' @param q0Lm Initial parameter Q of the Baranyi and Roberts' model related to lag phase for \emph{L. monocytogenes} at time=0
#' @param q0LAB Initial parameter Q of the Baranyi and Roberts' model related to lag phase for LAB at time=0
#' @param muLm (h-1) Growth rate of \emph{L. monocytogenes} in RTE seafood
#' @param muLAB (h-1) Growth rate of LAB in RTE seafood
#' @param MPDLm (log10 CFU/g) Maximum population density of \emph{L. monocytogenes} in RTE seafood
#' @param MPDLAB (log10 CFU/g) Maximum population density of LAB in RTE seafood
#' @param unitSize (g) Weight of the contents of an RTE seafood pack (for MPD)
#' @param gamma Interaction parameter of the competition model. Note: if gamma=1, population 1 stops when population 2 reaches MPD.
#' If gamma < 1, population 1 still increases when population 2 reaches MPD. If gamma > 1, population 1 decreases when population 2 reaches MPD.
#' @param lim = 1 A parameter indicating when the competition should be considered as meaningful.
#' @param step = 1 Integration step.
#'
#' @return a list of four elements:
#'     \describe{
#'              \item{N1Lm}{(CFU) Numbers of \emph{L. monocytogenes} at the end of storage;}
#'              \item{N1LAB}{(CFU) Numbers of LAB at the end of storage;}
#'              \item{lnQtLm}{Natural logarithm of q0 for \emph{L. monocytogenes} at the end of storage;}
#'              \item{lnQtLAB}{Natural logarithm of q0 for LAB at the end of storage.}
#'              }
#'
#' @author Regis Pouillot \email{rpouillot.work@gmail.com} and Laurent Guillier
#'
#' @keywords interaction competition inhibition
#'
#' @references
#'
#' \insertRef{Gimenez2004}{qraLm}
#'
#' \insertRef{Moller2013}{qraLm}
#'
#' @export
#'
#' @note
#' For sake of speed, the function first evaluates a classical Baranyi and Roberts model without interactions.
#' If any of the two population reaches a value greater than \eqn{10^{(MPD - lim)}} CFU, an expanded version of the Jameson-effect model
#' by \insertCite{Gimenez2004;textual}{qraLm} is used. This model includes the parameter gamma proposed by \insertCite{Moller2013;textual}{qraLm}.
#' The function is vectorized for all parameters.
#'
#' @examples
#' library(qraLm)
#' iter <- 1000
#' set.seed(666)
#' vtime <- 200
#' vN0Lm <- 10^runif(iter, 0, 3)
#' vN0LAB <- 10^runif(iter, 0, 3)
#' vq0Lm <- exp(runif(iter, 0, 1))
#' vq0LAB <- exp(runif(iter, 0, 1))
#' vmuLm <- runif(iter, 0, 0.1)
#' vmuLAB <- runif(iter, 0, 0.1)
#' sfGrowthJameson(
#'   time = vtime, N0Lm = vN0Lm, N0LAB = vN0LAB,
#'   q0Lm = vq0Lm, q0LAB = vq0LAB,
#'   muLm = vmuLm, muLAB = vmuLAB,
#'   MPDLm = 8, MPDLAB = 7,
#'   unitSize = 100,
#'   gamma = 0.8
#' )
#'
#' # This is not very efficient (201 integrations), but it works.
#' Res <- sfGrowthJameson(
#'   time = 0:200, N0Lm = rep(1, 201), N0LAB = rep(100, 201),
#'   q0Lm = rep(1, 201), q0LAB = rep(1, 201),
#'   muLm = rep(.1, 201), muLAB = rep(.3, 201),
#'   MPDLm = rep(8, 201), MPDLAB = rep(7, 201),
#'   unitSize = rep(100, 201),
#'   gamma = rep(0.8, 201)
#' )
#' plot(0:200, log10(Res$N1Lm), type = "l", ylim = c(0, 9), xlab = "time", ylab = "log10 cfu")
#' points(0:200, log10(Res$N1LAB), type = "l", col = "blue")
#' legend(x = 0, y = 8.5, legend = c("Listeria", "Food flora"), col = c("black", "blue"), lty = 1)
#'
sfGrowthJameson <- function(time,
                            N0Lm, N0LAB,
                            q0Lm,
                            q0LAB,
                            muLm, muLAB,
                            MPDLm,
                            MPDLAB,
                            unitSize,
                            gamma = 1,
                            lim = 1,
                            step = 1) {
  ######### DEFINES THE FUNCTIONS

  # Check if the C++ functions are available.
  # Should be available if qraLm package is well built.
  # If not, build them

  if (!exists("expandedjamesonC_set_params")) {
    stop("C++ functions for Jameson effect are missing")
    # cat("Rebuilding Jameson\n")
    # 
    # expandedjamesonC.sys <- "
    #   /*       Q1:x[0]  dQ1<-mumax1*Q1
    #            Q2:x[1]  dQ2<-mumax2*Q2
    #            y1:x[2]  dy1<-(Q1/(1+Q1))*mumax1*(1-(y1/ymax1))*(1-(gamma*y2/ymax2))*y1
    #            y2:x[3]  dy2<-(Q2/(1+Q2))*mumax2*(1-(y1/ymax1))*(1-(y2/ymax2))*y2
    #   */
    # 
    #   dxdt[0] = mumax1 * x[0];
    #   dxdt[1] = mumax2 * x[1];
    #   dxdt[2] = (x[0]/(1+x[0])) * mumax1 * (1-(x[2]/ymax1)) * (1-(gamma*x[3]/ymax2))*x[2];
    #   dxdt[3] = (x[1]/(1+x[1])) * mumax2 * (1-(x[2]/ymax1)) * (1-(x[3]/ymax2))*x[3];
    # "
    # # Do compile once
    # pars <- c(mumax1 = 0.14, mumax2 = 0.3, ymax1 = 10^8, ymax2 = 10^7, gamma = 1.0)
    # compile_sys(
    #   name = "expandedjamesonC",
    #   sys = expandedjamesonC.sys,
    #   pars = pars,
    #   const = FALSE, sys_dim = 4
    # )
  }

  # Define a function for growth with Competition integrate the
  # C++ function 
  growthCompetC <- function(time, N0Lm, N0LAB, q0Lm, q0LAB,
                            muLm, muLAB, MPDLm, MPDLAB, unitSize,
                            gamma,
                            step,
                            pb = NULL) {
    # Set parameters
    if (!is.null(pb)) utils::setTxtProgressBar(pb, utils::getTxtProgressBar(pb) + 1)
    expandedjamesonC_set_params(
      mumax1 = muLm, mumax2 = muLAB,
      ymax1 = 10^MPDLm * unitSize,
      ymax2 = 10^MPDLAB * unitSize,
      gamma = gamma
    )
    # Set initial values
    # Note: need to be in this order: Q1, Q2, y1, y2
    init <- c(q0Lm, q0LAB, N0Lm, N0LAB)
    # Integrate
    out <- try(expandedjamesonC(init = init, duration = time, step_size = step))
    # These lines to track errors
    if (class(out)[1] == "try-error") {
      # Tools for debugging
      # Init <<- init
      # Params <<- list(mumax1 = muLm, mumax2=muLAB,
      #                 ymax1=10^MPDLm*unitSize,
      #                 ymax2=10^MPDLAB*unitSize, gamma=gamma)
      stop("Error in Jameson integration. Init and Params saved.")
    }
    # Keep only the value at time
    out <- out[nrow(out), ]
    # Rename and reorder the outputs
    out <- data.frame(N1Lm = out$X3, N1LAB = out$X4, q0Lm = out$X1, q0LAB = out$X2)
    return(out)
  }

  # Vectorized version
  growthCompetCV <- function(time, N0Lm, N0LAB, q0Lm, q0LAB, muLm, muLAB,
                             MPDLm, MPDLAB, unitSize, gamma, step, pb) {
    res <- mapply(growthCompetC,
      time = time, N0Lm = N0Lm, N0LAB = N0LAB,
      q0Lm = q0Lm, q0LAB = q0LAB, muLm = muLm, muLAB = muLAB,
      MPDLm = MPDLm, MPDLAB = MPDLAB, unitSize = unitSize,
      gamma = gamma,
      MoreArgs = list(step = step, pb = pb)
    )
    # arrange outputs
    return(data.frame(
      N1Lm = unlist(res["N1Lm", ]),
      N1LAB = unlist(res["N1LAB", ]),
      q0Lm = unlist(res["q0Lm", ]),
      q0LAB = unlist(res["q0LAB", ])
    ))
  }

  ## Start the function

  NN <- length(N0Lm)

  # Evaluate the growth without competition using a Baranyi model
  N0Lm <- pmin(N0Lm, round(10^MPDLm * unitSize))
  AlreadyMPDLm <- N0Lm == round(10^MPDLm * unitSize)
  # For Listeria
  ALM <- time + 1 / muLm * log((exp(-muLm * time) + q0Lm) / (1 + q0Lm))
  log_NtLm <- log(N0Lm) + muLm * ALM - log(1 + (exp(muLm * ALM) - 1) / exp(log(10^MPDLm * unitSize) - log(N0Lm)))
  N1Lm <- exp(log_NtLm) # output concentration in CFU at time t

  # For The Food Flora
  N0LAB <- pmin(N0LAB, round(10^MPDLAB * unitSize))
  AlreadyMPDLAB <- N0LAB == round(10^MPDLAB * unitSize)
  ALAB <- time + 1 / muLAB * log((exp(-muLAB * time) + q0LAB) / (1 + q0LAB))
  log_NtLAB <- log(N0LAB) + muLAB * ALAB - log(1 + (exp(muLAB * ALAB) - 1) / exp(log(10^MPDLAB * unitSize) - log(N0LAB)))
  N1LAB <- exp(log_NtLAB) # output concentration in CFU at time t

  # Deals with 0es
  lnQtLm <- rep(muLm * time + log(q0Lm), length.out = NN) # output lnQ(t) at time t
  lnQtLAB <- rep(muLAB * time + log(q0LAB), length.out = NN) # output lnQ(t) at time t

  N1Lm[N0Lm == 0] <- 0
  N1LAB[N0LAB == 0] <- 0

  N1Lm[muLm == 0] <- N0Lm[muLm == 0]
  N1LAB[muLAB == 0] <- N0LAB[muLAB == 0]

  lnQtLm[N0Lm == 0] <- -Inf
  lnQtLAB[N0LAB == 0] <- -Inf

  # Check the one with competition (if higher than MPD - lim)
  # Also: no competition if no bacteria of the other type
  whichCompet <- N0LAB != 0 &
    N0Lm != 0 &
    !AlreadyMPDLm &
    !AlreadyMPDLAB &
    ((log10(N1Lm / unitSize) > (MPDLm - lim)) |
      (log10(N1LAB / unitSize) > (MPDLAB - lim)))
  # cat("Nb to evaluate",sum(whichCompet),"\n") #
  # Integrate in C
  # Note (need to extend the vectors for proper reference)
  if (any(whichCompet)) {
    cat("Integrate over ", sum(whichCompet), " portions\n")
    # browser()
    pb <- utils::txtProgressBar(max = sum(whichCompet))
    on.exit(close(pb))
    Integ <- growthCompetCV(
      time = rep(time, length = NN)[whichCompet],
      N0Lm = rep(N0Lm, length = NN)[whichCompet],
      N0LAB = rep(N0LAB, length = NN)[whichCompet],
      q0Lm = rep(q0Lm, length = NN)[whichCompet],
      q0LAB = rep(q0LAB, length = NN)[whichCompet],
      muLm = rep(muLm, length = NN)[whichCompet],
      muLAB = rep(muLAB, length = NN)[whichCompet],
      MPDLm = rep(MPDLm, length = NN)[whichCompet],
      MPDLAB = rep(MPDLAB, length = NN)[whichCompet],
      unitSize = rep(unitSize, length = NN)[whichCompet],
      gamma = rep(gamma, length = NN)[whichCompet],
      step = step,
      pb = pb
    )

    N1Lm[whichCompet] <- Integ$N1Lm
    N1LAB[whichCompet] <- Integ$N1LAB
    lnQtLm[whichCompet] <- log(Integ$q0Lm)
    lnQtLAB[whichCompet] <- log(Integ$q0LAB)
  }
  # Need to round because we had some small negative values
  return(list(N1Lm = round(N1Lm), N1LAB = round(N1LAB), lnQtLm = lnQtLm, lnQtLAB = lnQtLAB))
}
