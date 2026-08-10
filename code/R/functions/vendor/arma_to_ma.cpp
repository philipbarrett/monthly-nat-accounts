// arma_to_ma.cpp
//
// Vendored from the `deseats` R package (CRAN, version 1.1.2), function
// `MAinftySHORT` in src/LinearFilters.cpp. Not installed as a package dependency
// because deseats hard-requires `shiny`/`animation` (transitively needing a
// `libuv` system library unavailable/unneeded here) just to compute this one
// MA(infinity)-representation utility, used by code/R/functions/mly_gdp_gmm.R and
// mly_gdp_asymptotics.R (arma_to_ma(), called ~9 times in GMM moment-condition
// construction).
//
// Original source: https://cran.r-project.org/package=deseats
// Copyright (c) Yuanhua Feng, Dominik Schulz (Paderborn University)
// Licensed under GPL-3 (same as the original package) -- see LICENSE-GPL3-vendor.
// Verbatim except for the function rename (MAinftySHORT -> arma_to_ma_cpp) to
// avoid any namespace confusion with the original package.
//
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
arma::vec arma_to_ma_cpp(const arma::rowvec& ar, const arma::vec& ma, const int max_i) {

  if (max_i == 0) {

    return arma::ones<arma::vec>(1);

  } else {

    int l_ar = ar.size();
    const int l_ma = ma.size();

    arma::rowvec ar2 = ar;

    if (l_ar == 0) {
      l_ar = 1;
      ar2 = arma::zeros<arma::rowvec>(1);
    }

    const arma::rowvec ar_n = arma::reverse<arma::rowvec>(ar2);

    int times = max_i - l_ma;

    if (times < 0) {
      times = 0;
    }

    const arma::vec ma_s = arma::join_cols(ma, arma::zeros<arma::vec>(times));

    arma::vec coefOut = arma::join_cols(arma::zeros<arma::vec>(l_ar - 1), arma::ones<arma::vec>(1), arma::zeros<arma::vec>(max_i));

    const int lc = coefOut.size();

    for (int i = l_ar; i < max_i + l_ar; ++i) {

      coefOut.subvec(i, i) = ar_n * coefOut.subvec(i - l_ar, i - 1) + ma_s(i - l_ar);

    }

    return arma::join_cols(arma::ones<arma::vec>(1), coefOut.subvec(l_ar, lc - 1));

  }

}
