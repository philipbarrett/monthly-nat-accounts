##########################################################################################
#
# mly_GDP.R
#
# Computes the monthly GDP series
#
# Philip Barrett, Washington DC
# First version: 26jan2025
#
##########################################################################################

#### TO DO:
# SET UP TO ALLOW FOR DIFFERENT VERSIONS [DONE]
# ADD AUTOCORRELATION IN OBSERVATION EQUATION [DONE]
# ADD SEs for the GMM estimates [DONE]
# VALIDATION [DONE]
# WRITE UP [DONE]
# ADD THE NEW METHOD FOR AUTOCORRELATION IN THE OBSERVATION EQUATION [DONE]
# CHECK THE AR COVARIANCES. THE DIAGNAL SHOULD MATCH THE VARIANCES [DONE]
# COMPUTE THE COVARIANCE OF PARAMETERS CORRECTLY [DONE]
# FIX THE R2 BEING TOO HIGH BY REDUCING KAPPA [DONE]
# INTERMEDIATE VALIDATION CHECK [DONE]
# BOOTSTRAP [DONE]
# LOOK AT THE ARIMA/Q ESTIMATION THING [DONE]
# Re-VALIDATE [TO DO: SERVICES] [1]
# MAKE SOME SLIDES [DONE]
# CHECK WHY THE SE IS SMALLEST IN MONTH 2 [DONE]
# ADD CODE TO PULL DATA ONLY WHEN NEEDED [4]
# DON'T BOOTSTRAP. USE DELTA METHOD.
# BETTER BOOTSTRAP WITH COVARIANCE PARAMETERS [DONE]

#### 0. Set up ####
source(here::here("code/R/sessions/_setup.R"))
rm(list=ls())
# Core tidyverse packages loaded directly (not via library(tidyverse)) -- the
# tidyverse meta-package pulls in optional extras (googlesheets4, rmarkdown, shiny
# transitively) that need system libs (e.g. libuv) this pipeline doesn't otherwise
# need.
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(forcats)
library(lubridate)
library(ggplot2)
library(data.table)
library(ts.extend)
  # CRAN-archived (not on the current index), installed from
  # https://cran.r-project.org/src/contrib/Archive/ts.extend/ts.extend_0.1.1.tar.gz
  # -- genuinely needed (ARMA.autocov(), called from mly_gdp_gmm.R's state-equation
  # GMM moment conditions). An earlier pass incorrectly dropped this after only
  # grepping for the package name, not its actual exported function names -- see
  # PLAN.md Stage 2 for the correction.
library(forecast)
library(zoo)
library(Matrix)
library(openxlsx)
library(fredr)
library(httr2)
options(dplyr.summarise.inform = FALSE)
fredr::fredr_set_key(Sys.getenv("FRED_API_KEY"))
source('code/R/functions/data_sources/fred.R')
source('code/R/functions/data_sources/bea.R')
source('code/R/functions/data_sources/splice_historical.R')
source('code/R/functions/data_sources/pull_public.R')
source('code/R/functions/vendor/arma_to_ma.R')
  # Vendored from deseats::arma_to_ma() -- see code/R/functions/vendor/README.md
  # for why (same story as ts.extend: deseats itself needs shiny/animation just
  # for this one function, which mly_gdp_gmm.R/mly_gdp_asymptotics.R call ~9 times).
source('code/R/functions/data_helpers.R')
source('code/R/functions/mly_gdp_gmm.R')
source('code/R/functions/mly_gdp_kalman.R')
source('code/R/functions/kf_ham_convert.R')
source('code/R/functions/kf_filter.R')
source('code/R/functions/mly_gdp_asymptotics.R')
source('code/R/functions/param_convert.R')
source('code/R/functions/mly_gdp_uncertainty.R')
source('code/R/functions/mly_gdp_kf_deriv.R')
source('code/R/functions/mly_gdp_model_selection.R')

#### 1. Controls ####
l.options <- list(
  trend.order=2,        # Polynomial exponential trend order  Baseline: 2
  max.ar.order=2,           
  max.ar.moments=8,         
    # NB: Code will estimate ARs for higher orders and moments, but the Kalman
    # filter code will only work with constant ar order and moments for all
    # variables, which need selecting with the next two controls.
  ar.order = 1,           # Order of AR state equation.         Baseline: 1
  n.order = 8,            # Number of dynamic moments to fit.   Baseline: 8
  ar.eff=TRUE,                    # baseline: TRUE
  start.date = '1950-01-01',
  end.date = as.character(Sys.Date()),
    # Auto-detected, not hardcoded: this is deliberately generous (today's date,
    # not a specific quarter/month), since it's only an upper filter bound in
    # make.gdp.data(). Each pulled series naturally has no rows beyond whatever
    # it has actually published -- BEA/Census/FRED simply don't return future
    # dates -- so the real cutoff per series (and hence the latest complete
    # quarter for the quarterly target/components) falls out of the data itself,
    # not this setting. No downstream filtering logic needed to change.
  small.number = 1e-06,
  v.small.number = 1e-10,
  make.gdp.err = FALSE, # TRUE, #          # baseline: FALSE
  obs.resids = TRUE,              # baseline: TRUE
  ar.resids = FALSE,              # baseline: FALSE
  obs.match.ar = FALSE,           # baseline: FALSE
  save.dir = 'outputs/baseline_aug2025_pub/',
    # "_pub" suffix: keeps this run's output separate from outputs/baseline_aug2025/,
    # which holds the original Haver-sourced data and results backing the
    # currently-published working paper. Never overwrite that folder.
  r2.max=0.9,                     # baseline: 0.9
  efficient.gmm=FALSE,            # baseline: FALSE
  pull.data=TRUE                  # baseline: TRUE. Pulls fresh from public FRED/BEA
                                   # APIs -- see docs/data_sources.md. `haver=` fields
                                   # below are kept for provenance only, not used at
                                   # runtime; the old Haver pull path is quarantined
                                   # in code/R/functions/legacy_haver/.
)
source(here::here("code/R/sessions/_data_spec.R"))
  # Defines l.quarterly.data, l.monthly.data, l.derived.series, l.x, l.q -- shared
  # with validate_data_sources.R so both always agree on the series list.


#### 2. Download Data ####
make.dir.if.needed <- function(dir.path) if(!file.exists(dir.path)) dir.create(dir.path,recursive = TRUE)
for(x in c('graphs', 'tables', 'input_data', 'final_data') ) make.dir.if.needed(paste0( l.options$save.dir, x ) )
  # Make the output folder if required
l.dfs <- make.gdp.data( l.quarterly.data, l.monthly.data, l.derived.series,
                        l.x, l.q, l.options )
l.quarterly.data <- l.dfs$data.qly
l.options$end.date <- l.dfs$end.date.effective
  # Reflects the GDP-quarter-capped sample end (see data_helpers.R), not the
  # generous upper bound set above -- keeps saved provenance honest.
cat("Effective end date (capped to last complete GDP quarter):", l.options$end.date, "\n")


#### 3. Make Trends ####
l.df.trends <- make.trend.dfs( l.dfs, l.quarterly.data, l.options )
l.df.hats <- make.hat.dfs( l.dfs, l.df.trends, l.options )

#### 4. Parameterization ####

## 4.1 State equations ##
v.x <- l.x %>% sapply( ., function(z) z$label)
v.y <- setdiff(l.quarterly.data$components %>% names(), v.x) %>% set_names(.,.)
  # l.q %>% names() %>% set_names(.,.)
l.state.est <- state.eq.est( v.x, v.y, l.df.hats, l.options )

## 4.2 Simplify for the aggregation error ##
if(l.options$make.gdp.err){
  l.state.est$ar.coef[,'err'] <- 
    c( l.state.est$ar.coef[1,'err'] / (1-l.state.est$ar.coef[2,'err']^2), 0 )
  l.state.est$Q[,'err'] <- 0 * l.state.est$Q[,'err']
  l.state.est$Q['err',] <- 0 * l.state.est$Q['err',]
  l.state.est$Q['err','err'] <- l.state.est$ar.coef[1,'err']
}


## 4.2 Observation equations ##
# if(length(v.x)>1) l.Q.x.est.resid <- obs.exact.resid( v.x, l.df.hats, l.state.est, l.options )
  # Check that the observation equation estimation method works on the exact
  # data
l.Q.est <- obs.eq.est.resid( v.x, v.y, l.q, l.df.hats, l.state.est, l.options )


## 4.3 Grand GMM estimation ##
l.psi <- make.psi.list(l.state.est, l.Q.est)
l.meta <- l.psi$meta
v.theta <- make.theta.H(l.psi)
  # Formulate the giant parameter vectors
vcov.gmm <- make.asym.cov.theta( v.theta, l.df.hats, v.x, v.y, l.q, l.meta,
                                  l.options$n.order, return.W=FALSE )
  # Asymptotic covariance of the initial (pre-reweighting) theta -- computed
  # unconditionally (moved out of the else branch below 2026-08-09) since the
  # final save() call references vcov.gmm regardless of efficient.gmm. Confirmed
  # this was a real latent bug, not just a theoretical one: outputs/
  # baseline_no_gov_eff/final_data/everything.rdata (efficient.gmm=TRUE) exists
  # with no vcov.gmm object, meaning a from-scratch rerun of that variant would
  # crash on save() under the *current* mly_GDP.R even though this archived file
  # completed under some earlier version of the script.
if(l.options$efficient.gmm){
  l.gmm <- do.big.gmm( v.theta, l.df.hats, v.x, v.y, l.q, l.meta, l.options$n.order )
  l.psi.gmm <- theta.to.psi.list(l.gmm$par, l.meta)
  psi.ar.coef <- rbind( l.psi.gmm$Q %>% diag, l.psi.gmm$rho ) %>%
    set_rownames(rownames(l.state.est$ar.coef)) %>%
    set_colnames(colnames(l.state.est$ar.coef))
  ar.diff <- abs(psi.ar.coef - l.state.est$ar.coef)
  Q.diff <- abs(l.state.est$Q - l.psi.gmm$Q)
  kappa.diff <- abs(l.Q.est$kappa - l.psi.gmm$kappa)
  R.diff <- abs(l.Q.est$R - l.psi.gmm$R)

  l.state.est$ar.coef <- psi.ar.coef
  l.state.est$Q <- l.psi.gmm$Q
  l.Q.est$kappa <- l.psi.gmm$kappa
  l.Q.est$R <- l.psi.gmm$R
}


# cov.theta <- make.asym.cov.theta( v.theta,  l.df.hats, v.x, v.y, l.q, l.meta, l.options$n.order )
#   # The covariance of the parameters
# ar.ks.deriv <- make.ks.deriv( l.dfs.all, l.state.est, l.Q.est,
#                               l.quarterly.data, l.x, l.q, l.options, 
#                               l.kf.lag, l.kf.smoothed )

#### 5. Kalman Filter ####

## 5.1 Approximate version without time-varying coefficients ##
l.kf.lag <- make.kf.lag.apx( l.df.hats, l.df.trends, l.state.est, l.Q.est,
                             l.quarterly.data, l.x, l.q, l.options )
    # Approximate representation with fixed coefficients
l.kf.ham <- kf_laglist_to_ham(l.kf.lag)
l.kf.ham$C.0 <- kf_ricatti(l.kf.ham)
l.kf.filtered <- kf_filter_ham(l.kf.ham)
  # The filter
l.kf.smoothed <- kf_smooth( l.kf.filtered, l.kf.ham$F )

## 5.2 Create some output ##
df.names <- data.frame(var.name=c( gdp=l.quarterly.data$target$name,
                       sapply(l.quarterly.data$components, function(x) x$name ),
                       cov.err='Covariances') ) %>%
  rownames_to_column('variable')
v.states <- c( v.x, v.y )
l.df.filt <- ks.process( l.kf.smoothed, l.df.trends, df.names, l.quarterly.data, l.dfs$df.sign, v.states )


## 5.3 Adjust for estimation uncertainty ##
l.dfs.all <- list( hats=l.df.hats, trends=l.df.trends )
l.vcov.states <- make.ks.cov( l.dfs.all, v.x, v.y, l.q, l.state.est, l.Q.est, l.options )
  # Make the asymptotic covariance

l.kf.var <- list( xi.smooth=l.kf.smoothed$xi.smooth,
                  P.smooth=l.vcov.states$ar.vcov )
  # Set up a pseudo-smoother output using the state covariances
l.df.var <- ks.process( l.kf.var, l.df.trends, df.names, 
                        l.quarterly.data, l.dfs$df.sign, v.states )

df.out <- l.df.filt$df %>%
  left_join( l.df.var$df %>% 
               select( date, t, variable, 
                       param_se=ks_se, y.param.se=y.ks.se ),
             by=c('date', 't', 'variable')) %>%
  mutate( y.ks.se.all=sqrt(y.ks.se^2+y.param.se^2) )

df.se.q <- l.df.filt$df.se.q %>%
  left_join( l.df.var$df.se.q %>% 
               select( date, t, variable, param.se.pc100=y.ks.se.pc100 ),
             by=c('date', 't', 'variable' ) ) %>%
  mutate( y.ks.se.all.pc100=sqrt( y.ks.se.pc100^2 + param.se.pc100^2 ) )

## 5.4 Make model fit measures ##
n.params <- l.vcov.states$theta %>% length
kf.AIC <- 2 * ( n.params - l.kf.filtered$loglik )
# xx.ll <- make.smoother.ll( l.kf.filtered$xi.filt, l.kf.filtered$P.fcast, l.kf.ham$Y, l.kf.ham$H, l.kf.ham$R )
ks.ll <- make.smoother.ll( l.kf.smoothed$xi.smooth, l.kf.smoothed$P.smooth, l.kf.ham$Y, l.kf.ham$H, l.kf.ham$R )
ks.AIC <- 2 * ( n.params - ks.ll )
P.smooth.params <- 0 * l.kf.smoothed$P.smooth
P.smooth.params[] <- apply( l.vcov.states$ar.vcov, 3,
                           function(x) kronecker( matrix( 1, max(3,1+l.options$ar.order), max(3,1+l.options$ar.order) ),
                                                  x ) )
  # Assumed highly correlated estimates of the state
ks.ll.all <- make.smoother.ll( l.kf.smoothed$xi.smooth, l.kf.smoothed$P.smooth + P.smooth.params,
                               l.kf.ham$Y, l.kf.ham$H, l.kf.ham$R )
ks.AIC.all <- 2 * ( n.params - ks.ll.all )
l.fit <- list( aic=c( kf=kf.AIC, ks=ks.AIC, all=ks.AIC.all ), k=n.params )

#### 6.  Make some charts ####
source('code/R/sessions/charts.R')

#### 7.  Make some tables ####
source('code/R/sessions/tables.R')

#### 8. Save the Output ####
v.out.sheets <- c('level'='y.ks', 'trend'='trend', 'hat'='hat.ks', 
                  'full_se'='y.ks.se.all', 'smoother_se'='y.ks.se', 
                  'ks_param_se'='y.param.se')
l.xl.out <- lapply( v.out.sheets, function(x) df.out %>% 
                      select( date, variable, x) %>%
                      spread( variable, x ) )
write.xlsx(l.xl.out, file = paste0(l.options$save.dir, 'final_data/output.xlsx' ) )
save(file=paste0(l.options$save.dir, 'final_data/save_names.rdata' ),
     df.indicator.names, df.names )
save(file=paste0(l.options$save.dir, 'final_data/everything.rdata' ), l.dfs, l.df.hats, 
     l.df.trends, l.dfs.all, df.out, l.options, df.se.q, v.theta,
     vcov.gmm, l.quarterly.data, l.q, l.x, l.derived.series, l.Q.est, l.state.est, l.kf.lag,
     l.kf.ham, l.kf.filtered, kf.AIC, l.kf.smoothed, df.names, 
     df.indicator.names, df.state.names, l.vcov.states, l.fit )
