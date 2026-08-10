##########################################################################################
#
# tables.R
#
# Makes tables for the monthly GDP series
#
# Philip Barrett, Washington DC
# First version: 26jan2025
#
##########################################################################################

#### 0. Set up ####
source(here::here("code/R/sessions/_setup.R"))
# rm(list=ls())
library(dplyr); library(tidyr); library(readr); library(purrr); library(tibble)
library(stringr); library(forcats); library(lubridate); library(ggplot2)
  # Core tidyverse packages loaded directly, not via library(tidyverse) -- see
  # mly_GDP.R for why (avoids an unnecessary heavy dependency chain).
library(stargazer)
library(sandwich)
library(xtable)
library(mFilter)
library(scales)
cffilter_low_mly <- function(x) c( rep(NA, sum(is.na(x))), cffilter(x[!is.na(x)], pl = 1, pu = 60, root = TRUE)$cycle %>% c )
cffilter_low_qly <- function(x) c( rep(NA, sum(is.na(x))), cffilter(x[!is.na(x)], pl = 1, pu = 20, root = TRUE)$cycle %>% c )

#### 1. Controls ####
save.dir <- l.options$save.dir
rec.names <- c(' 1. Early 1950s', ' 2. Late 1950s', ' 3. Early 1960s', ' 4. Late 1960s', 
               ' 5. Oil Crisis', ' 6. 1980 recession', ' 7. Early 1980s', ' 8. Early 1990s', 
               ' 9. Dot-com bubble', '10. Great Recession', '11. COVID-19')
rec.start.dates <- c( '1953-07-01', '1957-08-01', '1960-04-01', '1969-12-01', '1973-11-01', 
                      '1980-01-01', '1981-07-01', '1990-07-01', '2001-03-01', 
                      '2007-12-01', '2020-02-01') %>% as.Date
rec.end.dates <- c( '1954-05-01', '1958-04-01', '1961-02-01', '1970-11-01', '1975-03-01', 
                    '1980-07-01', '1982-11-01', '1991-03-01', '2001-11-01', 
                    '2009-06-01', '2020-04-01' ) %>% as.Date
df.recessions <- data.frame( name=rec.names, start=rec.start.dates, end=rec.end.dates )


#### 2. Read in the data ####
# load( paste0( 'outputs/', save.dir, '/final_data/everything.rdata' ) )
  # The easy bit :)

#### 3. Estimation Tables ####

## 3.1 Trends ##
simple_names <- function(x) x %>%
  gsub('([0-9]|[0-9][a-b])\\. ', '', .) %>%
  gsub('consumption', '', .) %>%
  gsub('Non-', 'N', .) %>%
  gsub( '(?i)Residential fixed Investment', 'RFI', . ) %>%
  gsub('GDP(.*)', 'GDP', .) %>%
  gsub('ernment|entories', '\\.', .)
    # Name-simplifying function
dep.vars <- df.names %>% filter(variable !='cov.err') %>% pull(var.name, name = variable)
column.labels <- dep.vars[l.df.trends$lm %>% names] %>%
  simple_names

stargazer(l.df.trends$lm, se=lapply( l.df.trends$lm, function(x) x %>% vcovHAC %>% diag %>% sqrt ),
          omit.stat=c('f', 'adj.rsq', 'ser'), 
          covariate.labels=paste0( 'Order ', 1:l.options$trend.order, ' time polynomial'), 
          dep.var.caption = '', float = FALSE,
          column.labels = column.labels, dep.var.labels.include = FALSE,
          out=paste0( save.dir, '/tables/trend_coefs.tex' ) )
# type='text' )


## 3.2 AR coefficients ##
add_stars <- function(p_value) {
  if(is.na(p_value)) return('')
  if (p_value < 0.01) {
    return("***")
  } else if (p_value < 0.05) {
    return("**")
  } else if (p_value < 0.10) {
    return("*")
  } else {
    return("")
  }
}

df.ar <- data.frame( 
  variable=l.state.est$ar.coef %>% colnames,
  coef=l.state.est$ar.coef %>% t %>% c,
  se=l.state.est$ar.se %>% t %>% c,
  type=rep( c( 'sigma2', paste0( 'rho', 1:(nrow(l.state.est$ar.coef)-1) )), 
            each=ncol(l.state.est$ar.coef) ),
  est='GMM'
  ) %>%
  left_join( df.names, by='variable' )
    # The estimated 

df.ar.x <- data.frame( 
  variable=l.state.est$ar.coef.x %>% colnames,
  coef=l.state.est$ar.coef.x %>% t %>% c,
  se=l.state.est$ar.se.x %>% t %>% c,
  type=rep( c( 'sigma2', paste0( 'rho', 1:(nrow(l.state.est$ar.coef.x)-1) )), 
            each=ncol(l.state.est$ar.coef.x) ),
  est='AR'
) %>%
  left_join( df.names, by='variable' )

df.ar.tab <- rbind( df.ar.x, df.ar ) %>%
  mutate( coef=ifelse(type=='sigma2', coef*100, coef),
          se=ifelse(type=='sigma2', se*100, se) ) %>%
  # Multiple the variances by 100
  mutate(p_value = 2 * pnorm(-abs(coef / se)),
         coef=paste0(sprintf('%.3g', coef), sapply(p_value, add_stars)),
         se=paste0('(', sprintf('%.3g', se), ')') ) %>%
  select(-variable, -p_value) %>%
  gather( measure, value, coef, se) %>%
  unite( title, est, var.name ) %>%
  spread(title, value ) %>%
  mutate( names=case_when(measure=='se'~'',
                          grepl('rho',type)~paste0('$\\rho_',str_sub(type,-1,-1),'$'),
                          type=='sigma2'~'$100\\times\\sigma^2_{i,i}$') ) %>%
  select(names, everything(), -type, -measure) %>%
  set_names( names(.) %>% simple_names %>% gsub('AR_|GMM_','',.) %>% gsub('names','',.) )


tab.ar <- xtable(df.ar.tab, align = c('l', 'l', rep('c', ncol(df.ar.tab)-1 )) )

addtorow <- list(
  pos = list(-1, nrow(tab.ar)),
  command = c( 
    paste0( 
      '\\toprule & \\multicolumn{', nrow(df.ar.x)/2, 
      '}{c}{OLS} &  \\multicolumn{', nrow(df.ar)/2, '}{c}{GMM} \\\\ \n',
      '\\cmidrule(lr){', 2 , '-', 1+nrow(df.ar.x)/2, '} ',
      '\\cmidrule(lr){', 2+nrow(df.ar.x)/2 , '-', 1+nrow(df.ar.x)/2+nrow(df.ar)/2, '}'
    ),
    paste0( '\\midrule Observations & ', paste0( l.state.est$nobs %>% unlist(), collapse=' & ' ),
            '\\\\ \n',
            'Moment error & & & ', paste(round(sapply( l.state.est$est.gmm[setdiff(names(dep.vars),'gdp')], 
                                                       function(x) x$est$est$minimum ), 3 ),
                                         collapse=' & '), '\\\\' )
  )
)

print(tab.ar, floating = FALSE, include.rownames = FALSE, type = 'latex', 
      sanitize.colnames.function = identity, add.to.row = addtorow, 
      hline.after=c( 0, nrow(tab.ar)),
      sanitize.text.function=identity, booktabs = TRUE, 
      file=paste0( save.dir, 'tables/ar_coefs.tex' ) )


## 3.3 State Variables ##
state.idx <- which(names(v.theta) %in% (l.q %>% unlist()) )
    # Index for Ses
df.state <- l.Q.est$df.coefs %>% 
  rename(coef=kappa) %>% 
  select(-sample.cor.qly, -N, -R2) %>%
  mutate(se=vcov.gmm[state.idx,state.idx] %>% diag %>% sqrt) %>%
  rownames_to_column('variable') %>%
  left_join( df.indicator.names, by='variable' ) %>%
  left_join( df.names %>% rename(state=variable), by='state' ) %>%
  mutate_at( vars(name:var.name), gsub, 
             pattern='([0-9]|[0-9][a-b])\\. ', replacement='' )
df.correl <- l.Q.est$df.coefs %>% 
  select(-kappa) %>% 
  rownames_to_column('variable')

df.state.tab <- df.state %>%
  mutate(p_value = 2 * pnorm(-abs(coef / se)),
       coef=paste0(sprintf('%.3g', coef), sapply(p_value, add_stars)),
       se=paste0('(', sprintf('%.3g', se), ')') ) %>%
  select( -p_value) %>%
  gather( measure, value, coef, se) %>%
  left_join( df.correl %>% mutate(measure='coef') ) %>%
  select( var.name, name, measure, value, sample.cor.qly, N, R2 ) %>%
  arrange( var.name, name ) %>%
  mutate( var.name=ifelse(var.name==c('',var.name[-n()]), '', var.name),
          name=ifelse(name==c('',name[-n()]), '', name)) %>%
  select(-measure) %>%
  rename( '$y^i_t$'=var.name, '$q_t^{i,j}$'=name, '$\\kappa^{i,j}$'=value,
          '$N_{obs}$'=N, 'Pseudo-$R^2$'=R2,'$Cor(E_t^{i,j},\\hat Y^i_t)$'=sample.cor.qly )

tab.state <- xtable(df.state.tab, align = c('l', 'l', 'l', rep('c', ncol(df.state.tab)-2 )) )

print(tab.state, floating = FALSE, include.rownames = FALSE, type = 'latex', 
      sanitize.colnames.function = identity, 
      sanitize.text.function=identity, booktabs = TRUE, 
      file=paste0( save.dir, '/tables/obs_coefs.tex' ) )


#### 4. Relative volatilities ####

##### TO DO: ADD IN THE CORRECT INNOVATION SDs ####
## 4.1 Create the GDP innovation covariance ##
df.wts.decade <- df.out %>%
  mutate(decade=paste0(10*floor(year(date)/10)), 's') %>%
  group_by( variable, decade, var.name ) %>%
  summarise( trend=mean(trend) ) %>%
  left_join( {.} %>% filter( variable=='gdp') %>% ungroup %>%
               select(decade, gdp.trend=trend), by='decade' ) %>%
  mutate( wt = trend / gdp.trend )

m.wts.decade <- sapply( df.wts.decade$decade %>% unique,
        function(this.decade) df.wts.decade %>% 
          filter(decade==this.decade, variable != 'gdp') %>% 
          pull(wt, name=variable) ) %>% t
m.gdp.var.innov <- m.wts.decade[, colnames(l.state.est$Q)] %*% l.state.est$Q %*%
  t(m.wts.decade[, colnames(l.state.est$Q)])
v.gdp.se.innov <- m.gdp.var.innov %>% diag %>% sqrt
v.gdp.se.innov.avg <- v.gdp.se.innov %>% mean %>% set_names('gdp')

# Use l.state.est$Q and multiply by decadal GDP weights to get the implied GDP
# innovation variance

## 4.2 Make the Signal-to-noise-ratios ##
df.snr <- df.out %>%
  group_by(variable) %>%
  summarise( avg.se=100*mean(abs(y.ks.se.all/y.ks), na.rm=T),
             sd.hat=100*sd(hat.ks),
             se.innov.apx=100*sd( lm( hat.ks[-1] ~ hat.ks[-n()] ) %>% resid ) ) %>%
  left_join( data.frame( se.innov=100*c( l.state.est$ar.coef[1,] %>% sqrt, v.gdp.se.innov.avg ) ) %>% 
               rownames_to_column('variable'), by='variable' ) %>%
  mutate( se.innov=ifelse(is.na(se.innov),se.innov.apx,se.innov),
          fcast.se=sqrt(avg.se^2 + se.innov^2),
          snr.hat=sd.hat/avg.se,
          snr.fcast=fcast.se/avg.se )
    
df.snr.decade <- df.out %>%
  mutate(decade=paste0(10*floor(year(date)/10)), 's') %>%
  group_by(variable, decade) %>%
  summarise( avg.se=100*mean(abs(y.ks.se.all/y.ks), na.rm=T),
             sd.hat=100*sd(hat.ks),
             se.innov.apx=100*sd( lm( hat.ks[-1] ~ hat.ks[-n()] ) %>% resid ) ) %>%
  left_join( data.frame( se.innov=100*(l.state.est$ar.coef[1,] %>% sqrt) ) %>% 
               rownames_to_column('variable'), by='variable' ) %>%
  left_join( data.frame( se.innov.gdp=100*v.gdp.se.innov) %>% 
               rownames_to_column('decade') %>% mutate(variable='gdp'), 
             by=c('variable','decade') ) %>%
  mutate( se.innov=ifelse( variable=='gdp',se.innov.gdp,se.innov),
          fcast.se=sqrt(avg.se^2 + se.innov^2),
          snr.hat=sd.hat/avg.se,
          snr.fcast=fcast.se/avg.se ) %>%
  ungroup() %>%
  select(-se.innov.gdp)
    
## 4.3 Make the tables
df.tab.snr <- df.snr %>% left_join(df.names, by='variable') %>%
  arrange( var.name) %>%
  mutate( var.name = var.name %>% gsub('([0-9]|[0-9][a-b])\\. ', '', .) ) %>%
  select( Variable=var.name, '(1)'=sd.hat, '(2)'=fcast.se, '(3)'=avg.se,
          '(1)/(3)'=snr.hat, '(2)/(3)'=snr.fcast)

add.lines <- list(
  pos = list(-1),
  command = c( 
    paste0( 
      '\\toprule & $\\sqrt{Var \\mu_t^i }$ & $\\sqrt{Var_t \\mu_{t+1}^i }$ &',
      ' $\\sqrt{\\Sigma_t^{i,i}}$ & ',
      '\\multicolumn{2}{c}{Ratios} \\\\ \n',
      '\\cmidrule(lr){2-4}',
      '\\cmidrule(lr){5-6} \n'
    )
  )
)

tab.snr <- xtable(df.tab.snr, digits=c(2, 2, 2, 2, 2, 1, 1),
                  align = c('l', 'l',  rep('c', ncol(df.tab.snr)-1 )) )

print(tab.snr, floating = FALSE, include.rownames = FALSE, type = 'latex', 
      sanitize.colnames.function = identity, add.to.row = add.lines, 
      hline.after=c( 0, nrow(df.tab.snr)),
      sanitize.text.function=identity, booktabs = TRUE, 
      file=paste0( save.dir, '/tables/snr_vars.tex' ) )
  
  
df.tab.snr.gdp <- df.snr.decade %>% filter(variable=='gdp') %>%
  arrange( decade ) %>%
  select( Decade=decade, '(1)'=sd.hat, '(2)'=fcast.se, '(3)'=avg.se,
          '(1)/(3)'=snr.hat, '(2)/(3)'=snr.fcast)

add.lines <- list(
  pos = list(-1),
  command = c( 
    paste0( 
      '\\toprule & $\\sqrt{Var \\mu_t^{gdp} }$ & $\\sqrt{Var_t\\mu_{t+1}^{gdp} }$ &',
      ' $\\sqrt{\\Sigma_t^{gdp,gdp}}$ & ',
      '\\multicolumn{2}{c}{Ratios} \\\\ \n',
      '\\cmidrule(lr){2-4}',
      '\\cmidrule(lr){5-6} \n'
    )
  )
)


tab.snr.gdp <- xtable(df.tab.snr.gdp, digits=c(2, 2, 2, 2, 2, 1, 1),
                  align = c('l', 'l',  rep('c', ncol(df.tab.snr)-1 )) )

print(tab.snr.gdp, floating = FALSE, include.rownames = FALSE, type = 'latex', 
      sanitize.colnames.function = identity, add.to.row = add.lines, 
      hline.after=c( 0, nrow(df.tab.snr.gdp)),
      sanitize.text.function=identity, booktabs = TRUE, 
      file=paste0( save.dir, '/tables/snr_gdp.tex' ) )


##### 5. Business Cycle Statistics ####

## 5.1 Correlations ##
df.gdp.corel.q.in <- l.df.hats$df.y.hat %>%
  select( date, variable, hat ) %>%
  group_by(variable) %>%
  mutate( hat=cffilter_low_qly(hat) ) %>%
  ungroup() %>%
  left_join( {.} %>% filter( variable=='gdp') %>% 
               select(-variable, ) %>% rename(gdp.hat=hat), by='date' ) %>%
  filter( variable != 'gdp')

df.gdp.corel.q <- df.gdp.corel.q.in %>%
  mutate(decade=paste0(10*floor(year(date)/10), 's')) %>%
  group_by( variable, decade ) %>%
  summarise( correl=cor(hat, gdp.hat) ) %>%
  full_join(df.gdp.corel.q.in %>%
              mutate(decade='All') %>%
              group_by( variable, decade ) %>%
              summarise( correl=cor(hat, gdp.hat) ) ) %>%
  ungroup() %>%
  left_join(df.names, by='variable') %>%
  select(-variable) %>%
  spread(var.name, correl )%>%
  rename(Decade=decade) %>%
  set_names( simple_names(names(.)))
    # Quarterly GDP correlations of each variable

df.gdp.corel.in <- df.out %>%
  select( date, variable, hat.ks ) %>%
  group_by(variable) %>%
  mutate( hat=cffilter_low_mly(hat.ks) ) %>%
  ungroup() %>%
  left_join( {.} %>% filter( variable=='gdp') %>% 
               select(-variable, -hat.ks) %>% rename(gdp.hat=hat), by='date' ) %>%
  filter( variable != 'gdp')

df.gdp.corel <- df.gdp.corel.in %>%
  mutate(decade=paste0(10*floor(year(date)/10), 's')) %>%
  group_by( variable, decade ) %>%
  summarise( correl=cor(hat, gdp.hat) ) %>%
  full_join(df.gdp.corel.in %>%
              mutate(decade='All') %>%
              group_by( variable, decade ) %>%
              summarise( correl=cor(hat, gdp.hat) ) ) %>%
  ungroup() %>%
  left_join(df.names, by='variable') %>%
  select(-variable) %>%
  spread(var.name, correl )%>%
  rename(Decade=decade) %>%
  set_names( simple_names(names(.)))

xtable(df.gdp.corel) %>%
  print(floating = FALSE, include.rownames = FALSE, type = 'latex', 
        booktabs = TRUE,
        file=paste0( save.dir, '/tables/gdp_corel_m.tex' ) )
xtable(df.gdp.corel.q) %>%
  print(floating = FALSE, include.rownames = FALSE, type = 'latex', 
        booktabs = TRUE,
        file=paste0( save.dir, '/tables/gdp_corel_q.tex' ) )

df.correl.plot <- df.gdp.corel %>%
  gather( Component, value, -Decade) %>% mutate(Frequency='Monthly') %>%
  full_join( df.gdp.corel.q %>%
               gather( Component, value, -Decade) %>% 
               mutate(Frequency='Quarterly') )

ggplot( df.correl.plot, 
        aes(x=Decade, y=value, group=Frequency, lty=Frequency) ) + 
  geom_line() + facet_wrap(~Component, scales='free_y')  + 
  theme_minimal() + theme(legend.position = 'bottom')

## 5.2 Variance shares ##
df.gdp.var.q.in <- l.df.hats$df.y.hat %>%
  select( date, variable, trend, hat ) %>%
  group_by(variable) %>%
  mutate( hat=cffilter_low_qly(hat) ) %>%
  ungroup() %>%
  left_join( {.} %>% filter( variable=='gdp') %>% 
               select(-variable, ) %>% rename(gdp.trend=trend, gdp.hat=hat), by='date' ) %>%
  filter( variable != 'gdp')

df.gdp.var.q <- df.gdp.var.q.in %>%
  mutate(decade=paste0(10*floor(year(date)/10), 's')) %>%
  group_by( variable, decade ) %>%
  summarise( cov.gdp=cov(hat, gdp.hat), var.gdp=var(gdp.hat), 
             wt=sum(trend,na.rm=T)/sum(gdp.trend,na.rm=T) ) %>%
  mutate( wt=ifelse(variable=='m', -wt,wt) ) %>%
  ungroup() %>%
  mutate( var.ratio=100*wt * cov.gdp/var.gdp) %>%
  full_join({.} %>%
              mutate(decade='All') %>%
              group_by( variable, decade ) %>%
              summarise( var.ratio=mean(var.ratio) ) ) %>%
  left_join(df.names, by='variable') %>%
  select(-variable, -cov.gdp, -var.gdp, -wt) %>%
  full_join( {.} %>% group_by(decade) %>% 
               summarise(var.ratio=100-sum(var.ratio)) %>%
               mutate(var.name='Cov.') %>%
                        ungroup() ) %>%
  spread(var.name, var.ratio )%>%
  rename(Decade=decade) %>%
  set_names( simple_names(names(.)))
    # Quarterly variance decomposition

df.gdp.var.in <- df.out %>%
  select( date, variable, hat.ks, trend ) %>%
  group_by(variable) %>%
  mutate( hat=cffilter_low_mly(hat.ks) ) %>%
  ungroup() %>%
  left_join( {.} %>% filter( variable=='gdp') %>% 
               select(-variable, -hat.ks) %>% 
               rename(gdp.trend=trend, gdp.hat=hat), by='date' ) %>%
  filter( variable != 'gdp')

df.gdp.var <- df.gdp.var.in %>%
  mutate(decade=paste0(10*floor(year(date)/10), 's')) %>%
  group_by( variable, decade ) %>%
  summarise( cov.gdp=cov(hat, gdp.hat), var.gdp=var(gdp.hat), 
             wt=sum(trend,na.rm=T)/sum(gdp.trend,na.rm=T) ) %>%
  mutate( wt=ifelse(variable=='m', -wt,wt) ) %>%
  ungroup() %>%
  mutate( var.ratio=100*wt * cov.gdp/var.gdp) %>%
  full_join({.} %>%
              mutate(decade='All') %>%
              group_by( variable, decade ) %>%
              summarise( var.ratio=mean(var.ratio) ) ) %>%
  left_join(df.names, by='variable') %>%
  select(-variable, -cov.gdp, -var.gdp, -wt) %>%
  full_join( {.} %>% group_by(decade) %>% 
               summarise(var.ratio=100-sum(var.ratio)) %>%
               mutate(var.name='Cov.') %>%
               ungroup() ) %>%
  spread(var.name, var.ratio )%>%
  rename(Decade=decade) %>%
  set_names( simple_names(names(.)))

xtable(df.gdp.var, digits = 1) %>%
  print(floating = FALSE, include.rownames = FALSE, type = 'latex', 
        booktabs = TRUE,
        file=paste0( save.dir, '/tables/gdp_cov_m.tex' ) )
xtable(df.gdp.var.q, digits = 1) %>%
  print(floating = FALSE, include.rownames = FALSE, type = 'latex', 
        booktabs = TRUE,
        file=paste0( save.dir, '/tables/gdp_cov_q.tex' ) )


#### 6. Recession dating ####
find.local.min.max <- function(df, target='y.ks'){
# Finds local min-max dates, akin to recession dating, for a subset of the data
  max.iter <- 100
  keep.iterating <- TRUE
  iter <- 0
    # Set up
  d.1 <- min(df$date) ; d.2 <- max(df$date)
  
  while( iter< max.iter & keep.iterating){
    # df <- df %>% filter( date >=w.1, date <= w.2 )
      # Initial window
    new.d.1 <- df %>% filter( date < d.2 %m-% months(1) ) %>%
      summarise( new.d.1=date[which.max(!!sym(target))] ) %>%
      pull(new.d.1)
    new.d.2 <- df %>% filter( date > d.1 ) %>%
      summarise( new.d.2=date[which.min(!!sym(target))] ) %>%
      pull(new.d.2)
    
    if(d.1==new.d.1 & d.2==new.d.2){
      keep.iterating <- FALSE
    }else{
      if( new.d.1 >= new.d.2 ){
        d.1 <- d.1 %m+% months(1)
        d.2 <- d.2 %m-% months(1)
      }
      else{
        d.1 <- new.d.1
        d.2 <- new.d.2
      }
    }
    iter <- iter + 1
  }
  return( data.frame(d.1=d.1, d.2=d.2) )
}

## 6.1 Find the recessions ##
window.inc <- 6
  # Search window bounds +/1 months
df.recessions <- df.recessions %>% 
  mutate( w.1 = start %m-% months(window.inc),
          w.2 = end %m+% months(window.inc) )
  # Set up the search window
df.mly.recessions <- NULL
df.out.gdp <- df.out %>%
  filter(variable=='gdp')
for( i.recession in 1:nrow(df.recessions) ){
  this.df <- df.out.gdp %>%
    select( date, y.ks ) %>%
    filter( date >= df.recessions$w.1[i.recession],
            date <= df.recessions$w.2[i.recession] )
  df.mly.recessions <- rbind( df.mly.recessions,
                              find.local.min.max(this.df) )
    # Find the recessions
}

df.recessions.m <- df.recessions %>%
  cbind( df.mly.recessions ) %>%
  left_join(df.out.gdp %>% select(start=date, y.ks.start=y.ks), by='start' ) %>%
  left_join(df.out.gdp %>% select(d.1=date, y.ks.d1=y.ks, y.se.d1=y.ks.se.all), by='d.1' ) %>%
  left_join(df.out.gdp %>% select(end=date, y.ks.end=y.ks), by='end' ) %>%
  left_join(df.out.gdp %>% select(d.2=date, y.ks.d2=y.ks, y.se.d2=y.ks.se.all), by='d.2' ) %>%
  mutate( start.p=pnorm(-abs(y.ks.start-y.ks.d1)/y.se.d1),
          end.p=pnorm(-abs(y.ks.end-y.ks.d2)/y.se.d2),
          d1.threshold = y.ks.d1 - 1.96 * y.se.d1,
          d2.threshold = y.ks.d2 + 1.96 * y.se.d2)
  # Add together the NBER and my monthly identification

df.recessions.tab <- df.recessions.m %>%
  mutate( d.1=ifelse(d.1==start, as.character(d.1), paste0('\\textbf{', d.1, '}') ),
          d.2=ifelse(d.2==end, as.character(d.2), paste0('\\textbf{', d.2, '}') )) %>%
  mutate( d.1=paste0(d.1, sapply(start.p, add_stars) ),
          d.2=paste0(d.2, sapply(end.p, add_stars) ),
          name=simple_names(name),
          start=as.character(start),
          end=as.character(end) ) %>%
  select( name, start, end, d.1, d.2 ) %>%
  set_names( c('Recession', 'Peak', 'Trough', 'Peak', 'Trough') )

add.rec.lines <- list(
  pos = list(-1),
  command = c( 
    paste0( 
      '\\toprule & \\multicolumn{2}{c}{NBER} & \\multicolumn{2}{c}{Monthly GDP} \\\\ \n',
      '\\cmidrule(lr){2-3}',
      '\\cmidrule(lr){4-5} \n'
    )
  )
)

xtable( df.recessions.tab, align=rep('l', ncol(df.recessions.tab)+1) ) %>%
  print(floating = FALSE, include.rownames = FALSE, type = 'latex', 
        sanitize.colnames.function = identity, 
        add.to.row = add.rec.lines, 
        hline.after=c( 0, nrow(df.recessions.tab)),
        sanitize.text.function=identity, booktabs = TRUE, 
        file=paste0( save.dir, 'tables/recession_dates.tex' ) )

## 6.3 Add CIs for the recession windows ##
df.recessions.ci <- NULL
for( i.recession in 1:nrow(df.recessions) ){
  df.peak <- df.out.gdp %>%
    select( date, y.ks, y.ks.se.all ) %>%
    filter( date >= df.recessions.m$w.1[i.recession] %m-% months(2*window.inc),
            date <= df.recessions.m$d.2[i.recession] )
  df.trough <- df.out.gdp %>%
    select( date, y.ks, y.ks.se.all ) %>%
    filter( date <= df.recessions.m$w.2[i.recession] %m+% months(2*window.inc),
            date >= df.recessions.m$d.1[i.recession] )
      # Extract GDP data before the peak and after the trough
  this.ci <- data.frame(
    name=df.recessions.m$name[i.recession],
    d1.lower = df.peak %>% 
      filter( y.ks >= df.recessions.m$d1.threshold[i.recession]) %>% 
      pull(date) %>% min,
    d1.upper = df.peak %>% 
      filter( y.ks >= df.recessions.m$d1.threshold[i.recession]) %>% 
      pull(date) %>% max,
    d2.lower = df.trough %>% 
      filter( y.ks <= df.recessions.m$d2.threshold[i.recession]) %>% 
      pull(date) %>% min,
    d2.upper = df.trough %>% 
      filter( y.ks <= df.recessions.m$d2.threshold[i.recession]) %>% 
      pull(date) %>% max )
      # Compute the confidence interval on the recession dates
  df.recessions.ci <- rbind( df.recessions.ci, this.ci )
      # Combine
}

cleanse.dups <- function(x) ifelse( x==c('',x[-length(x)]), '', x )
df.recessions.ci.tab <- df.recessions.m %>% 
  select( name, start, end, d.1, d.2) %>% 
  left_join(df.recessions.ci, by='name') %>%
  mutate( start=ifelse(start < d1.lower | start > d1.upper, 
                       paste0('\\textbf{', as.yearmon(start), '}'), as.character(as.yearmon(start)) ),
          end=ifelse(end < d2.lower | end > d2.upper, 
                       paste0('\\textbf{', as.yearmon(end), '}'), as.character(as.yearmon(end)) ) ) %>%
  mutate_if( is.Date, . %>% as.yearmon %>% as.character ) %>%
  mutate( d1.ci = paste0('(', d1.lower, ',', d1.upper, ')'),
          d2.ci = paste0('(', d2.lower, ',', d2.upper, ')') ) %>%
  select( -contains(c('upper', 'lower'))) %>%
  gather( variable, value, -name, -start, -end ) %>%
  mutate( type=ifelse( grepl( 'ci', variable ), 'ci', 'pt' ),
          variable=ifelse( grepl( '1', variable ), 'ci.start', 'ci.end' ) ) %>%
  spread(variable, value ) %>%
  arrange( name, desc(type) ) %>%
  select( name, start, end, ci.start, ci.end) %>%
  mutate( name = cleanse.dups( name ), 
          start = cleanse.dups( start ),
          end = cleanse.dups( end )) %>%
  set_names( c('Recession', 'Peak', 'Trough', 'Peak', 'Trough') )
    # Lots of formatting

xtable( df.recessions.ci.tab, align=c( 'l', 'l', rep('c', ncol(df.recessions.ci.tab)-1) ) ) %>%
  print(floating = FALSE, include.rownames = FALSE, type = 'latex', 
        sanitize.colnames.function = identity, 
        add.to.row = add.rec.lines, 
        hline.after=c( 0, nrow(df.recessions.ci.tab)),
        sanitize.text.function=identity, booktabs = TRUE, 
        file=paste0( save.dir, 'tables/recession_dates_ci.tex' ) )


## 6.4 Make a plot
# I know this is a tables file but I neeeed to make a chart here, ok? ##
df.recessions.plot <- NULL
for( i.recession in 1:nrow(df.recessions) ){
  this.df <- df.out.gdp %>%
    select( date, y.ks ) %>%
    filter( date >= df.recessions$w.1[i.recession],
            date <= df.recessions$w.2[i.recession] ) %>%
    mutate( name=df.recessions$name[i.recession])
  df.recessions.plot <- rbind( df.recessions.plot,
                              this.df )
  # Find the recessions
}


my_breaks <- seq(as.Date("1950-01-01"), as.Date("2025-12-01"), by = "12 months")

ggplot( df.recessions.plot ) +
  scale_x_date( breaks = pretty_breaks(n=3), labels = date_format("%Y") ) +
  geom_line(aes(x=date, y=y.ks)) + 
  facet_wrap(~name, scale='free') +
  geom_rect(data=df.recessions.m,
            aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf), alpha=0.2 ) +
  geom_vline(data=df.recessions.m, aes(xintercept=d.1), lty=2 ) +
  geom_vline(data=df.recessions.m, aes(xintercept=d.2), lty=2 ) +
  labs(y='Billions chained 2017 dollars', x='') + theme_minimal() # +
  # theme( axis.text.x = element_text(angle = 45, hjust = 1) )
ggsave( paste0(save.dir, 'graphs/recession_compare.pdf' ), height=6, width=10 )
