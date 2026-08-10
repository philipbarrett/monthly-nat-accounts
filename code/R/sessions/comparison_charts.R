##########################################################################################
#
# comparison_charts.R
#
# Comparison charts across different runs
#
# Philip Barrett, Washington DC
# First version: 06feb2025
#
##########################################################################################

#### 0. Set up ####
rm(list=ls())
library(dplyr); library(tidyr); library(readr); library(purrr); library(tibble)
library(stringr); library(forcats); library(lubridate); library(ggplot2)
  # Core tidyverse packages loaded directly, not via library(tidyverse) -- see
  # mly_GDP.R for why (avoids an unnecessary heavy dependency chain).
library(data.table)
library(scales)
library(readxl)
library(xtable)
source(here::here("code/R/sessions/_setup.R"))

## 0.1 Recession plots
plot.yrs <- list( covid=2019:2022, gfc=2006:2010, nice=1997:2004, volkler=1979:1983, early70s=1969:1976,
                  late50s=1957:1961 )
rec.start.dates <- c('1957-08-01', '1960-04-01', '1969-12-01', '1973-11-01', 
                     '1980-01-01', '1981-07-01', '1990-07-01', '2001-03-01', 
                     '2007-12-01', '2020-02-01') %>% as.Date
rec.end.dates <- c('1958-04-01', '1961-02-01', '1970-11-01', '1975-03-01', 
                   '1980-07-01', '1982-11-01', '1991-03-01', '2001-11-01', 
                   '2009-06-01', '2020-04-01' ) %>% as.Date
df.recessions <- data.frame( start=rec.start.dates, end=rec.end.dates )

#### 1. Validation on the consumption series ####

## 1.1 Set up
base.dir <- 'baseline_no_gov'
do.goods <- TRUE # FALSE # TRUE
if(do.goods){
  validation.vars <- c('cons_g')
  val.dir <- c( # ' Using durable goods and real retail sales'='cons_g_dg_rrs_no_gov',
                ' Using real retail sales'='cons_g_rrs_no_gov', 
                '  Using durable goods data'='cons_g_dg_no_gov', 
                'No monthly data'='cons_g0_no_gov' )
  run.select <- list( no_data=c('cons_g0_no_gov', 'cons_g_dg_no_gov'), 
                      more_data=c('cons_g_dg_no_gov', 'cons_g_rrs_no_gov') #,
                      # dg_rrs=c('cons_g_dg_rrs', 'cons_g_dg_rrs_no_gov'),
                      # dg=c('cons_g_dg', 'cons_g_dg_no_gov')
                      )
}else{
  validation.vars <- c('cons_s')
  val.dir <- c( 
    ' Using housing services and ISM services'='cons_s_housing_ism_no_gov', 
    ' Using housing services and ISM services w/ pub. wages'='cons_s_housing_ism', 
    '  Using housing services data'='cons_s_housing_no_gov', 
    '  Using housing services data w/ pub. wages'='cons_s_housing', 
    'No monthly data'='cons_s0_no_gov', 
    'No monthly data w/ pub. wages'='cons_s0' )
  run.select <- list( no_data=c('cons_s0_no_gov', 'cons_s_housing_no_gov'), 
                      more_data=c('cons_s_housing_no_gov', 'cons_s_housing_ism_no_gov'),
                      housing=c('cons_s_housing_no_gov', 'cons_s_housing') )
}

val.dir.names <- names(val.dir) %>% set_names(val.dir)
all.dirs <- c('Baseline'=base.dir, val.dir)
# all.dir.names <- names(all.dirs) %>% set_names(all.dirs)
start.validation <- '1960-01-01'    # No data prior to 1959 for checking
end.validation <- '2019-12-31'
# pc.check <- c(.05, .1, .25, .5, .75, .9, .95)
pc.check <- seq( .01, .99, by=.01 )

## 1.2 Read in the requisite data ##
df.level <- lapply( all.dirs, function(this.dir){
  df.in <- read_xlsx( paste0('outputs/', this.dir, '/final_data/output.xlsx'), 
                      sheet = 'level' )
  df.out <- df.in %>% mutate( run=this.dir ) %>% select( run, everything() )
} ) %>%
  reduce(full_join) %>%
  # select(-unit) %>%
  gather( variable, level, -run, -date ) %>%
  mutate( date=as.Date(date, origin = "1899-12-30") )

df.trend <- lapply( all.dirs, function(this.dir){
  df.in <- read_xlsx( paste0('outputs/', this.dir, '/final_data/output.xlsx'), 
                      sheet = 'trend' )
  df.out <- df.in %>% mutate( run=this.dir ) %>% select( run, everything() )
} ) %>%
  reduce(full_join) %>%
  gather( variable, trend, -run, -date ) %>%
  mutate( date=as.Date(date, origin = "1899-12-30") )

df.se <- lapply( all.dirs, function(this.dir){
  df.in <- read_xlsx( paste0('outputs/', this.dir, '/final_data/output.xlsx'), 
                      sheet = 'smoother_se' )
  df.out <- df.in %>% mutate( run=this.dir ) %>% select( run, everything() )
} ) %>%
  reduce(full_join) %>%
  # select(-unit) %>%
  gather( variable, se, -run, -date ) %>%
  mutate( date=as.Date(date, origin = "1899-12-30") )

df.se.full <- lapply( all.dirs, function(this.dir){
  df.in <- read_xlsx( paste0('outputs/', this.dir, '/final_data/output.xlsx'), 
                      sheet = 'full_se' )
  df.out <- df.in %>% mutate( run=this.dir ) %>% select( run, everything() )
} ) %>%
  reduce(full_join) %>%
  gather( variable, se.full, -run, -date ) %>%
  mutate( date=as.Date(date, origin = "1899-12-30") )

df.all <- df.level %>%
  full_join( df.trend, by=c( 'run', 'date', 'variable' ) ) %>%
  full_join( df.se, by=c( 'run', 'date', 'variable' ) ) %>%
  full_join( df.se.full, by=c( 'run', 'date', 'variable' ) ) %>%
  ungroup()
load(paste0('outputs/', base.dir, '/final_data/save_names.rdata') )

## 1.3 Make the validation data
df.validate <- df.all %>% 
  filter( variable %in% validation.vars ) %>%
  left_join( {.} %>% filter(run==base.dir) %>% select(date, variable, outcome=level),
             by=c('date', 'variable') ) %>%
  filter(run!=base.dir, date>=start.validation, date <= end.validation) %>%
  mutate( decade=paste0(10*floor(year(date)/10), 's'),
          z.val=(outcome-level)/se, p.val=pnorm(z.val), 
          z.val.full=(outcome-level)/se.full, p.val.full=pnorm(z.val.full) )

df.p.check <- lapply( pc.check, 
                      function(this.p) df.validate %>% group_by(run, variable, decade) %>% 
                        summarise( p.frac=sum(p.val<this.p)/n(), p.frac.full=sum(p.val.full<this.p)/n() ) %>% 
                        ungroup() %>% mutate(p.test=this.p) ) %>%
  rbindlist %>%
  mutate( run.name=val.dir.names[run] )
# df.p.check.tab <- df.p.check %>% 
#   spread(p.test, p.frac ) %>%
#   as_tibble()

ggplot( df.p.check, aes(x=p.test, y=p.frac, color=run.name, group=run.name) ) +
  geom_abline(lty=2) +
  geom_line(lwd=.9) +
  facet_wrap(~decade) +
  ylim(c(0,1)) + xlim(c(0,1)) +
  theme_minimal()

gg.p.full <- 
  ggplot( df.p.check, aes(x=p.test, y=p.frac.full, color=run.name, group=run.name, lty=run.name) ) +
    geom_abline(lty=1, alpha=.5) +
    geom_line(lwd=.9) +
    geom_hline(aes(yintercept=0), alpha=.5) +
    geom_hline(aes(yintercept=1), alpha=.5) +
    geom_vline(aes(xintercept=0), alpha=.5) +
    geom_vline(aes(xintercept=1), alpha=.5) +
    facet_wrap(~decade) +
    ylim(c(0,1)) + xlim(c(0,1)) +
    theme_minimal() +
    theme(legend.position = 'bottom') +
    labs(x='Model p-value', y='Cummulative density', col='', lty='') +
    scale_x_continuous( breaks=pretty_breaks()) +
    scale_y_continuous( breaks=pretty_breaks())
gg.p.full %>% print()
ggsave(paste0('outputs/validation_', paste( validation.vars, collapse='_'), '.pdf' ), 
       gg.p.full, width=12, height=8)

for( i.runs in 1:length(run.select) ){
  this.runs.name <- names(run.select)[i.runs]
  this.runs <- run.select[[i.runs]]
  this.gg.p.full <- 
    ggplot( df.p.check %>% filter( run %in% this.runs ), 
            aes(x=p.test, y=p.frac.full, group=run.name, lty=run.name) ) +
    geom_abline(lty=1, alpha=.5) +
    geom_line(lwd=.9) +
    geom_hline(aes(yintercept=0), alph=.5) +
    geom_hline(aes(yintercept=1), alpha=.5) +
    geom_vline(aes(xintercept=0), alpha=.5) +
    geom_vline(aes(xintercept=1), alpha=.5) +
    facet_wrap(~decade) +
    ylim(c(0,1)) + xlim(c(0,1)) +
    theme_minimal() +
    theme(legend.position = 'bottom') +
    labs(x='Model p-value', y='Cummulative density', col='', lty='') +
    scale_x_continuous( breaks=pretty_breaks()) +
    scale_y_continuous( breaks=pretty_breaks())
  this.gg.p.full %>% print()
  ggsave(paste0('outputs/validation_', paste( validation.vars, collapse='_'), '_', this.runs.name, '_full.pdf' ), 
         this.gg.p.full, width=8, height=4)
  
  this.gg.p <- 
    ggplot( df.p.check %>% filter( run %in% this.runs ), 
            aes(x=p.test, y=p.frac, group=run.name, lty=run.name) ) +
    geom_abline(lty=1, alpha=.5) +
    geom_line(lwd=.9) +
    geom_hline(aes(yintercept=0), alpha=.5) +
    geom_hline(aes(yintercept=1), alpha=.5) +
    geom_vline(aes(xintercept=0), alpha=.5) +
    geom_vline(aes(xintercept=1), alpha=.5) +
    facet_wrap(~decade) +
    ylim(c(0,1)) + xlim(c(0,1)) +
    theme_minimal() +
    theme(legend.position = 'bottom') +
    labs(x='Filter p-value', y='Cummulative density', col='', lty='') +
    scale_x_continuous( breaks=pretty_breaks()) +
    scale_y_continuous( breaks=pretty_breaks())
  this.gg.p %>% print()
  ggsave(paste0('outputs/validation_', paste( validation.vars, collapse='_'), '_', this.runs.name, '.pdf' ), 
         this.gg.p, width=8, height=4)
}

## 1.4 Compare the standard errors across the runs ##
df.se.decade <- df.validate %>%
  filter(run!='baseline', variable %in% validation.vars ) %>%
  mutate( decade=10*floor(year(date)/10), err=100*(level-outcome)/level ) %>%
  group_by( run, decade, variable ) %>%
  summarise( se=100*mean(se/trend), se.boot=100*mean(se.full/trend), rmse=sqrt(mean(err^2)) ) %>%
  gather( type, value, -run, -decade, -variable ) %>%
  mutate( type.name=case_when(
    type=='se'~'Filter uncertainty',
    type=='se.boot'~'Filter + estimation uncertainty',
    type=='rmse'~'RMSE in the data') ) %>%
  left_join(df.names) %>%
  mutate( run.name=val.dir.names[run] )

gg.se.vs.full <- 
  ggplot( df.se.decade, aes(x=decade, y=value, group=type.name, # lty=type.name, 
                            pch=type.name, col=type.name) ) +
    geom_line(lwd=.9) +
    geom_point(size=3) +
    geom_hline(aes(yintercept=0)) +
    facet_wrap(~run.name, dir = 'v', nrow=2) +
    theme_minimal() +
    theme(legend.position = 'bottom') +
    labs(x='', y='Percent', pch='', col='') +
    scale_x_continuous( breaks=pretty_breaks()) +
    scale_y_continuous( breaks=pretty_breaks())
gg.se.vs.full %>% print()
ggsave(paste0('outputs/validation_', paste( validation.vars, collapse='_'), '_se_RMSE_full.pdf' ), 
       gg.se.vs.full, width=10, height=6)

ggplot( df.se.decade, aes(x=decade, y=value, group=run.name, color=run.name) ) +
  geom_line() +
  geom_hline(aes(yintercept=0)) +
  facet_wrap(~type.name) +
  theme_minimal()

df.ks <- df.p.check %>% group_by(run.name, decade) %>% 
  summarise( p.val=ks.test(p.frac, 'punif', min=0, max=1 )$p.value, 
             p.val.full=ks.test(p.frac.full, 'punif', min=0, max=1 )$p.value ) %>%
  ungroup() %>%
  full_join( df.p.check %>% group_by(run.name) %>% 
               summarise( p.val=ks.test(p.frac, 'punif', min=0, max=1 )$p.value, 
                          p.val.full=ks.test(p.frac.full, 'punif', min=0, max=1 )$p.value ) %>%
               mutate( decade='All'))

df.tab.ks.full <- df.ks %>% select(-p.val) %>%
  spread( decade, p.val.full ) %>%
  separate(run.name, into=c('Consumption data', 'Pub. wage data' ), sep=' w/') %>%
  mutate( `Pub. wage data`=ifelse(is.na(`Pub. wage data`), 'No', 'Yes' ) ) %>%
  ungroup() %>%
  arrange(`Pub. wage data`, `Consumption data`)

df.tab.ks <- df.ks %>% select(-p.val.full) %>%
  spread( decade, p.val ) %>%
  separate(run.name, into=c('Consumption data', 'Pub. wage data' ), sep=' w/') %>%
  mutate( `Pub. wage data`=ifelse(is.na(`Pub. wage data`), 'No', 'Yes' ) ) %>%
  ungroup() %>%
  arrange(`Pub. wage data`, `Consumption data`)

tab.ks.full <- xtable( df.tab.ks.full,  digits=3, align = c('l','l','l',rep('c',ncol(df.tab.ks.full)-2)) )
print( tab.ks.full, floating = FALSE, include.rownames = FALSE, type = 'latex', 
       sanitize.colnames.function = identity,
       sanitize.text.function=identity, booktabs = TRUE, file='outputs/validation_ks_pvals.tex' )

stop()

# ##### MAKE PRECISION-ACCURACY TRADEOFF CHART ####
# df.trade.off <- df.validate %>% 
#   mutate(err=(level-outcome)/level) %>%
#   group_by( decade, run, variable ) %>%
#   summarise( rmse=sqrt(mean(err^2)), ks.se=mean(se.full/level) ) %>%
#   mutate( se.factor=rmse/ks.se, run.name=val.dir.names[run], log10rmse=-log(rmse,10), log10se=-log(ks.se,10) )
# 
# xy.limits <- round( range( df.trade.off$log10rmse, df.trade.off$log10se) + c(-.1,.1),1 )
# # xy.limits <- round( 100*range( df.trade.off$ks.se, df.trade.off$rmse) + c(-.1,.1), 1 )
# ggplot( df.trade.off, 
#         aes(x=log10se, y=log10rmse, color=run.name, pch=run.name) ) +
#         # aes(x=100*ks.se, y=100*rmse, color=run.name, pch=run.name) ) +
#   geom_abline(lty=2) +
#   geom_point( size=3 ) +
#   geom_path() +
#   scale_x_continuous(limits=xy.limits, breaks=pretty_breaks()) + 
#   scale_y_continuous(limits=xy.limits, breaks=pretty_breaks()) + 
#   theme_minimal()


#### 2. Comparison of the efficient and unweighted results ####

## 2.1 Set up ##
gmm.dir <- c( 'Efficient GMM'='baseline', 
              'Unweighted GMM'='baseline_ineff' )  
gmm.dir.names <- names(gmm.dir) %>% set_names(gmm.dir)

## 2.2 Read in the requisite data ##
df.level <- lapply( gmm.dir, function(this.dir){
  df.in <- read_xlsx( paste0('outputs/', this.dir, '/final_data/output.xlsx'), 
                      sheet = 'level' )
  df.out <- df.in %>% mutate( run=this.dir ) %>% select( run, everything() )
} ) %>%
  reduce(full_join) %>%
  # select(-unit) %>%
  gather( variable, level, -run, -date ) %>%
  mutate( date=as.Date(date, origin = "1899-12-30") )

df.trend <- lapply( gmm.dir, function(this.dir){
  df.in <- read_xlsx( paste0('outputs/', this.dir, '/final_data/output.xlsx'), 
                      sheet = 'trend' )
  df.out <- df.in %>% mutate( run=this.dir ) %>% select( run, everything() )
} ) %>%
  reduce(full_join) %>%
  gather( variable, trend, -run, -date ) %>%
  mutate( date=as.Date(date, origin = "1899-12-30") )

df.se <- lapply( gmm.dir, function(this.dir){
  df.in <- read_xlsx( paste0('outputs/', this.dir, '/final_data/output.xlsx'), 
                      sheet = 'smoother_se' )
  df.out <- df.in %>% mutate( run=this.dir ) %>% select( run, everything() )
} ) %>%
  reduce(full_join) %>%
  # select(-unit) %>%
  gather( variable, se, -run, -date ) %>%
  mutate( date=as.Date(date, origin = "1899-12-30") )

df.se.full <- lapply( gmm.dir, function(this.dir){
  df.in <- read_xlsx( paste0('outputs/', this.dir, '/final_data/output.xlsx'), 
                      sheet = 'full_se' )
  df.out <- df.in %>% mutate( run=this.dir ) %>% select( run, everything() )
} ) %>%
  reduce(full_join) %>%
  gather( variable, se.full, -run, -date ) %>%
  mutate( date=as.Date(date, origin = "1899-12-30") )

load(paste0('outputs/', gmm.dir[1], '/final_data/save_names.rdata') )
df.all.gmm <- df.level %>%
  full_join( df.trend, by=c( 'run', 'date', 'variable' ) ) %>%
  full_join( df.se, by=c( 'run', 'date', 'variable' ) ) %>%
  full_join( df.se.full, by=c( 'run', 'date', 'variable' ) ) %>%
  ungroup() %>%
  mutate( run.name=gmm.dir.names[run] ) %>%
  left_join( df.names, by='variable' ) %>%
  as_tibble

df.gmm.10 <- df.all.gmm %>%
  mutate(decade=10*round(year(date)/10,0)) %>%
  group_by( run, decade, variable, var.name) %>%
  summarise_if(is.numeric, mean, na.rm=T)

df.gmm.level <- df.all.gmm %>%
  select( date, run, variable, var.name, level ) %>%
  spread( run, level ) %>%
  mutate( diff=100*(baseline_ineff/baseline-1) ) %>%
  left_join(
    df.all.gmm %>%
    select( date, run, variable, var.name, se.full ) %>%
    spread( run, se.full ) %>%
    select(-baseline_ineff) %>%
    rename(se_baseline=baseline), by=c('date', 'variable', 'var.name')
  ) %>%
  mutate( pc.diff=100*diff/baseline, z.diff=diff/se_baseline, 
          z.abs=abs(z.diff), pc.abs=abs(pc.diff) )


df.gmm.se.10 <- df.gmm.10 %>%
  select( decade, run, variable, var.name, se.full ) %>%
  spread( run, se.full ) %>%
  mutate( se_diff=100*(baseline_ineff/baseline-1) )

df.gmm.pdiff.10 <- df.gmm.level %>%
  mutate(decade=10*round(year(date)/10,0)) %>%
  group_by( decade, variable, var.name) %>%
  summarise_if(is.numeric, mean, na.rm=T)


## 2.3 Compare the different runs ##
for(i.episode in 1:length(plot.yrs)){
  episode.name <- names(plot.yrs)[i.episode]

  gg.gmm.compare <- 
      ggplot( df.all.gmm %>% filter( year(date) %in% plot.yrs[[i.episode]] ) ) +
        geom_rect(data=df.recessions %>% filter( year(start) %in% plot.yrs[[i.episode]] ),
                  aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf), alpha=0.2 ) +
        geom_line(aes(x=date, y=level, lty=run.name)) +
        theme_minimal() +
        facet_wrap(~var.name, scale='free_y') +
        labs( y='Billions chained 2017 dollars', x='', lty='') +
        theme(legend.position='bottom')

  gg.gmm.compare %>% print()
}

ggplot( df.gmm.level, aes(x=date, y=diff) ) +
  geom_line() +
  theme_minimal() +
  facet_wrap(~var.name, scale='free_y') +
  labs( y='Percentage difference from baseline', x='', lty='') +
  theme(legend.position='bottom')

ggplot( df.gmm.pdiff.10 %>% filter(pc.abs<1e4), aes(x=decade, y=pc.abs) ) +
  geom_line() +
  geom_hline(aes(yintercept=0), lwd=.1) +
  theme_minimal() +
  facet_wrap(~var.name, scale='free_y') +
  labs( y='Absolute average percentage difference from baseline', x='', lty='') +
  theme(legend.position='bottom')

ggplot( df.gmm.pdiff.10 %>% filter(z.abs<100), aes(x=decade, y=z.abs) ) +
  geom_line() +
  geom_hline(aes(yintercept=0), lwd=.1) +
  theme_minimal() +
  facet_wrap(~var.name, scale='free_y') +
  labs( y='Absolute average z-score, difference from baseline', x='', lty='') +
  theme(legend.position='bottom')
