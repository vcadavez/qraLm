nLots <- 1000
sizeLot <- 500 # Units: bag of frozen V

N <- matrix(rpois(sizeLot * nLots, 10),
  nrow = nLots, ncol = sizeLot
)
N0 <- list(N = N, P = 0.16)
b <- 1

# Nf <- fvPortioning (N0, servingSize=50,
#                      unitSize=500, bPort=1)
# #debugonce(fvPortioning)
# Nf <- fvPortioning (N0, servingSize=runif(500*1000, 10, 100), unitSize=500, bPort=1)

# I tried this
# # This is awful, but quick
# But bug when two many servings
# p <- rep(NA, lN)
# quel <- is.na(p)
# limit <- 100
# loop <- 0
# while(any(quel)){
#   loop <- loop + 1
#   if(loop > limit) stop("infinite loop in Portioning")
#   p[quel] <- extraDistr::rbbinom(sum(quel), N[quel], b, b*(n_serv-1))
#   p[p==0] <- NA
#   quel <- is.na(p)
# }

# library(microbenchmark)
Nt_batch <- matrix(rpois(500 * 1000, 5), nrow = 500, ncol = 1000)
lN <- length(Nt_batch)
N <- Nt_batch[Nt_batch > 0]
N <- sample(N, size = lN, replace = TRUE)
N <- N[N > 1]
lNm1 <- length(N)
b <- 1
n_serv <- 10

f <- function(N, b, n_serv) {
  p <- mc2d::rdirichlet(1, rep(b, n_serv))
  N_out <- mc2d::rmultinomial(1, N, p)
  return((N_out[N_out > 0])[1])
}

f1 <- function() {
  p <- mc2d::rdirichlet(lNm1, rep(b, n_serv))
  N_out <- mc2d::rmultinomial(lNm1, N, p)
  # Take the first non 0
  N_pos <- apply(N_out, 1, function(x) (x[x > 0])[1])
  # Add the ones and shuffle
  N_pos <- sample(c(N_pos, rep(1, lN - lNm1)))
}

n_rep <- 10
partition <- function(N, n_serv) {
  Nf_m <- rbinom(1, N, rbeta(1, b, b * (n_serv - 1)))
  return(c(Nf_m))
}

# Ursula function
f2 <- function() {
  # the 10 replicates allow to have sufficient observations to resample
  # only the positive counts in the output matrix
  N_out <- sapply(N, function(u) {
    replicate(n = n_rep, expr = {
      partition(N = u, n_serv = n_serv)
    })
  })
  N_pos <- N_out[which(N_out > 0)] # removing servings with zero cells
}

# microbenchmark(
#   f1(),
#   f2(),
#   mapply(f, N, b=1, n_serv=10),
# times=1
# )
