##########################################################################################
#
# arma_to_ma.R
#
# R wrapper for arma_to_ma_cpp() (see arma_to_ma.cpp in this folder). Matches the
# signature and behavior of `deseats::arma_to_ma()` exactly -- vendored rather than
# depending on the deseats package itself, since deseats hard-requires shiny and
# animation (see arma_to_ma.cpp header for why) just for this one function.
#
##########################################################################################

if (!requireNamespace("Rcpp", quietly = TRUE)) {
  stop("Package 'Rcpp' is required. Install with install.packages('Rcpp').")
}
if (!requireNamespace("RcppArmadillo", quietly = TRUE)) {
  stop("Package 'RcppArmadillo' is required. Install with install.packages('RcppArmadillo').")
}
Rcpp::sourceCpp(here::here("code/R/functions/vendor/arma_to_ma.cpp"))

#' MA(infinity) representation of an ARMA model (vendored from deseats::arma_to_ma)
#'
#' @param ar numeric vector of AR parameters (a_1, ..., a_p)
#' @param ma numeric vector of MA parameters (b_1, ..., b_q)
#' @param max_i number of coefficients to return (max_i + 1 total, including index 0)
arma_to_ma <- function(ar = numeric(0), ma = numeric(0), max_i = 1000) {
  stopifnot(
    "ar must be a numeric vector" = is.numeric(ar),
    "ma must be a numeric vector" = is.numeric(ma),
    "max_i must be a numeric vector of length one" = (is.numeric(max_i) && length(max_i) == 1)
  )
  c(arma_to_ma_cpp(ar = ar, ma = ma, max_i = max_i))
}
