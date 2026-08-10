##########################################################################################
#
# run_variant.R
#
# Parameterized runner for Stage 2's "regenerate every run-folder variant" task
# (PLAN.md). Mirrors mly_GDP.R's baseline pipeline exactly (same stages, same
# defaults, same vcov.gmm-computed-unconditionally fix) but applies one named
# variant's overrides from variant_configs.R after _data_spec.R is sourced.
#
# Usage: Rscript code/R/sessions/run_variant.R <variant_name>
#
# Full-fidelity chart/table generation (charts.R/tables.R, ~29 main.tex artifacts)
# only runs for the "baseline" variant -- the only one embedded throughout the
# paper body. Every other variant only needs output.xlsx/everything.rdata/
# save_names.rdata, which is all comparison_tables.R/comparison_charts.R/
# robustness.R actually read (confirmed by reading those three scripts in full).
# The small df.state.names/df.indicator.names computation charts.R normally
# provides (needed by the final save() call) is inlined for non-baseline variants
# instead of sourcing all of charts.R.
#
##########################################################################################

source(here::here("code/R/sessions/_setup.R"))
variant.name <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(variant.name)) stop("Usage: Rscript run_variant.R <variant_name>")

library(dplyr); library(tidyr); library(readr); library(purrr); library(tibble)
library(stringr); library(forcats); library(lubridate); library(ggplot2)
library(data.table)
library(ts.extend)
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
if (file.exists('code/R/functions/legacy_haver/pull_manual_csv.R')) {
  source('code/R/functions/legacy_haver/pull_manual_csv.R')
}
  # legacy_haver/ is excluded from the public repo (Haver-sourced robustness-only
  # series, not redistributable) -- conditional so public users can still run
  # "baseline" and every pure-l.options variant via this same script. Only the
  # variants with manual.labels set (add_extra_gov/nrfi/rfi, everything,
  # cons_g_rrs_no_gov) actually call pull.manual.csv(), and will correctly fail
  # with a clear "could not find function" error if attempted without it.
source('code/R/functions/vendor/arma_to_ma.R')
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
source('code/R/sessions/variant_configs.R')

if (!variant.name %in% names(l.variant.configs)) {
  stop("Unknown variant '", variant.name, "'. Known: ", paste(names(l.variant.configs), collapse = ", "))
}
cfg <- l.variant.configs[[variant.name]]

#### 1. Controls -- baseline defaults, overridden per variant ####
l.options <- modifyList(list(
  trend.order=2, max.ar.order=2, max.ar.moments=8, ar.order=1, n.order=8, ar.eff=TRUE,
  start.date='1950-01-01', end.date=as.character(Sys.Date()),
  small.number=1e-06, v.small.number=1e-10, make.gdp.err=FALSE,
  obs.resids=TRUE, ar.resids=FALSE, obs.match.ar=FALSE,
  save.dir=cfg$save.dir,
  r2.max=0.9, efficient.gmm=FALSE, pull.data=TRUE
), cfg$options)

for(x in c('graphs', 'tables', 'input_data', 'final_data') ) {
  d <- paste0(l.options$save.dir, x)
  if(!file.exists(d)) dir.create(d, recursive=TRUE)
}

source(here::here("code/R/sessions/_data_spec.R"))

# Add manual-CSV specs to l.monthly.data if this variant needs them (add_extra_*,
# everything, cons_g_rrs_no_gov -- see data/manual/robustness_series.csv).
if (!is.null(cfg$manual.labels)) {
  manual.specs <- lapply(cfg$manual.labels, function(lbl) list(label=lbl, name=lbl, manual=TRUE))
  l.monthly.data <- c(l.monthly.data, manual.specs)
}

# Apply this variant's structural overrides (l.q/l.x/l.quarterly.data$components)
cfg$apply()

cat("\n\n========== VARIANT:", variant.name, "==========\n")
cat("save.dir:", l.options$save.dir, "\n")
cat("l.q:\n"); print(l.q)
cat("l.x names:", paste(names(l.x), collapse=", "), "\n")
cat("quarterly components:", paste(names(l.quarterly.data$components), collapse=", "), "\n")
cat("=========================================\n\n")

#### 2. Download data ####
l.dfs <- make.gdp.data( l.quarterly.data, l.monthly.data, l.derived.series, l.x, l.q, l.options )
l.quarterly.data <- l.dfs$data.qly

#### 3. Trends ####
l.df.trends <- make.trend.dfs( l.dfs, l.quarterly.data, l.options )
l.df.hats <- make.hat.dfs( l.dfs, l.df.trends, l.options )

#### 4. Parameterization ####
v.x <- l.x %>% sapply( ., function(z) z$label)
v.y <- setdiff(l.quarterly.data$components %>% names(), v.x) %>% set_names(.,.)
l.state.est <- state.eq.est( v.x, v.y, l.df.hats, l.options )

if(l.options$make.gdp.err){
  l.state.est$ar.coef[,'err'] <-
    c( l.state.est$ar.coef[1,'err'] / (1-l.state.est$ar.coef[2,'err']^2), 0 )
  l.state.est$Q[,'err'] <- 0 * l.state.est$Q[,'err']
  l.state.est$Q['err',] <- 0 * l.state.est$Q['err',]
  l.state.est$Q['err','err'] <- l.state.est$ar.coef[1,'err']
}

l.Q.est <- obs.eq.est.resid( v.x, v.y, l.q, l.df.hats, l.state.est, l.options )

l.psi <- make.psi.list(l.state.est, l.Q.est)
l.meta <- l.psi$meta
v.theta <- make.theta.H(l.psi)
vcov.gmm <- make.asym.cov.theta( v.theta, l.df.hats, v.x, v.y, l.q, l.meta,
                                  l.options$n.order, return.W=FALSE )
if(l.options$efficient.gmm){
  l.gmm <- do.big.gmm( v.theta, l.df.hats, v.x, v.y, l.q, l.meta, l.options$n.order )
  l.psi.gmm <- theta.to.psi.list(l.gmm$par, l.meta)
  psi.ar.coef <- rbind( l.psi.gmm$Q %>% diag, l.psi.gmm$rho ) %>%
    set_rownames(rownames(l.state.est$ar.coef)) %>%
    set_colnames(colnames(l.state.est$ar.coef))
  l.state.est$ar.coef <- psi.ar.coef
  l.state.est$Q <- l.psi.gmm$Q
  l.Q.est$kappa <- l.psi.gmm$kappa
  l.Q.est$R <- l.psi.gmm$R
}

#### 5. Kalman Filter ####
l.kf.lag <- make.kf.lag.apx( l.df.hats, l.df.trends, l.state.est, l.Q.est,
                             l.quarterly.data, l.x, l.q, l.options )
l.kf.ham <- kf_laglist_to_ham(l.kf.lag)
l.kf.ham$C.0 <- kf_ricatti(l.kf.ham)
l.kf.filtered <- kf_filter_ham(l.kf.ham)
l.kf.smoothed <- kf_smooth( l.kf.filtered, l.kf.ham$F )

df.names <- data.frame(var.name=c( gdp=l.quarterly.data$target$name,
                       sapply(l.quarterly.data$components, function(x) x$name ),
                       cov.err='Covariances') ) %>%
  rownames_to_column('variable')
v.states <- c( v.x, v.y )
l.df.filt <- ks.process( l.kf.smoothed, l.df.trends, df.names, l.quarterly.data, l.dfs$df.sign, v.states )

l.dfs.all <- list( hats=l.df.hats, trends=l.df.trends )
l.vcov.states <- make.ks.cov( l.dfs.all, v.x, v.y, l.q, l.state.est, l.Q.est, l.options )

l.kf.var <- list( xi.smooth=l.kf.smoothed$xi.smooth, P.smooth=l.vcov.states$ar.vcov )
l.df.var <- ks.process( l.kf.var, l.df.trends, df.names, l.quarterly.data, l.dfs$df.sign, v.states )

df.out <- l.df.filt$df %>%
  left_join( l.df.var$df %>%
               select( date, t, variable, param_se=ks_se, y.param.se=y.ks.se ),
             by=c('date', 't', 'variable')) %>%
  mutate( y.ks.se.all=sqrt(y.ks.se^2+y.param.se^2) )

df.se.q <- l.df.filt$df.se.q %>%
  left_join( l.df.var$df.se.q %>%
               select( date, t, variable, param.se.pc100=y.ks.se.pc100 ),
             by=c('date', 't', 'variable' ) ) %>%
  mutate( y.ks.se.all.pc100=sqrt( y.ks.se.pc100^2 + param.se.pc100^2 ) )

n.params <- l.vcov.states$theta %>% length
kf.AIC <- 2 * ( n.params - l.kf.filtered$loglik )
ks.ll <- make.smoother.ll( l.kf.smoothed$xi.smooth, l.kf.smoothed$P.smooth, l.kf.ham$Y, l.kf.ham$H, l.kf.ham$R )
ks.AIC <- 2 * ( n.params - ks.ll )
P.smooth.params <- 0 * l.kf.smoothed$P.smooth
P.smooth.params[] <- apply( l.vcov.states$ar.vcov, 3,
                           function(x) kronecker( matrix( 1, max(3,1+l.options$ar.order), max(3,1+l.options$ar.order) ),
                                                  x ) )
ks.ll.all <- make.smoother.ll( l.kf.smoothed$xi.smooth, l.kf.smoothed$P.smooth + P.smooth.params,
                               l.kf.ham$Y, l.kf.ham$H, l.kf.ham$R )
ks.AIC.all <- 2 * ( n.params - ks.ll.all )
l.fit <- list( aic=c( kf=kf.AIC, ks=ks.AIC, all=ks.AIC.all ), k=n.params )

#### 6/7. Charts & tables -- ONLY for the real baseline (see file header) ####
if (variant.name == "baseline") {
  source('code/R/sessions/charts.R')
  source('code/R/sessions/tables.R')
} else {
  df.state.names <- as.data.frame(do.call(rbind, l.quarterly.data$components ) ) %>%
    select( variable=label, name ) %>%
    mutate_all(unlist) %>%
    rbind( as.data.frame( l.quarterly.data$target ) %>% select( variable=label, name ) ) %>%
    as_tibble()
  df.indicator.names <- as.data.frame(do.call(rbind, l.monthly.data ) ) %>%
    select( variable=label, name ) %>%
    mutate_all(unlist) %>%
    rbind( as.data.frame(do.call(rbind, l.derived.series ) ) %>%
                 select( variable=label, name ) %>%
                 mutate_all(unlist) ) %>%
    as_tibble
}

#### 8. Save the output ####
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

cat("\n\n*** VARIANT", variant.name, "COMPLETE ***\n")
