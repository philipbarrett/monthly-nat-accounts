##########################################################################################
#
# comparison_tables.R
#
# Comparison tables across different runs
#
# Philip Barrett, Washington DC
# First version: 06feb2025
#kf.  
##########################################################################################

#### 0. Set up ####
rm(list=ls())
library(dplyr); library(tidyr); library(readr); library(purrr); library(tibble)
library(stringr); library(forcats); library(lubridate); library(ggplot2)
  # Core tidyverse packages loaded directly, not via library(tidyverse) -- see
  # mly_GDP.R for why (avoids an unnecessary heavy dependency chain).
library()
library(data.table)
library(scales)
library(readxl)
library(xtable)
source(here::here("code/R/sessions/_setup.R"))


#### 1. Controls ####
l.runs <- list( 'Baseline'='baseline_no_gov',
                'Gov. (small)'='baseline',
                'Gov. (large)'='add_extra_gov',
                # 'Trade'='add_extra_trade',
                'NRFI'='add_extra_nrfi',
                'RFI'='add_extra_rfi',
                'Everything'='everything'
                ) #,
                # 'Extra Government and Investment data'='add_extra_gov_inv' )



#### 2. Load the runs ####
df.fit.compare <- df.spec.compare <- NULL

for( this.run in l.runs ){
  data.dir <- paste0('outputs/', this.run, '/final_data')
  load( paste0(data.dir, '/everything.rdata') )
  load( paste0(data.dir, '/save_names.rdata') )
    # Load the data
  
  this.df.spec.compare <- data.frame( run=this.run, 
                                      l.Q.est$df.coefs %>% select( y=state, q, R2 ) )
                                      # y=rep( names(l.q), sapply(l.q,length) ),
                                      # q=l.q %>% unlist ) %>% set_rownames(NULL)
    # The specifications
  
  n.pd <- df.out %>% filter( variable=='gdp') %>% nrow()
  this.df.aic.all <- data.frame( run=this.run, name=c('N params.', 'AIC', 'BIC'), 
                                 value=c( l.fit$k, l.fit$aic['all'], 
                                          l.fit$aic['all'] - 2*l.fit$k + l.fit$k*log(n.pd) ) ) %>%
    set_rownames(NULL)
  this.df.se.all <- df.out %>% filter( variable=='gdp') %>% mutate(name='sd.All', run=this.run) %>% 
    group_by(run, variable, var.name, name ) %>% summarise( value=mean(y.ks.se.all/trend * 100) )
  this.df.se.decade <- df.out %>% filter( variable=='gdp') %>% mutate(name=paste0( 'sd.', 10*floor(year(date)/10), 's'), run=this.run) %>% 
    group_by(run, variable, var.name, name ) %>% summarise( value=mean(y.ks.se.all/trend * 100) )
  
  this.df.fit.compare <- full_join( this.df.aic.all, this.df.se.all )%>%
    full_join( this.df.se.decade ) %>%
    select( run, name, value)
  
  df.spec.compare <- rbind(df.spec.compare, this.df.spec.compare)
  df.fit.compare <- rbind(df.fit.compare, this.df.fit.compare)
  
}


#### 3. Prepare the table ###
df.fit <- df.fit.compare %>%
  mutate( value=case_when( name=='N params.'~sprintf('%.0f', value),
                           name=='AIC'~sprintf('%.0f', value),
                           name=='BIC'~sprintf('%.0f', value),
                           abs(value)>10~sprintf('%.2g', value),
                           TRUE~sprintf('%.3f', value) ) ) %>%
  spread(run, value) %>%
  mutate( q='', R2='', name=gsub('sd.',' ', name) ) %>%
  select( name, q,'pseudo-$R^2$'=R2, l.runs %>% unlist ) %>%
  arrange(desc(name)) %>%
  rename(y=name)
    # The lower half of the table

v.q.unique <- 
  df.spec.compare %>%
  group_by(y,q) %>%
  summarise(nn=n()) %>%
  filter(nn<length(l.runs)) %>%
  pull(q, name = y)

v.var.names <- df.names %>% pull(var.name, name = variable) %>%
  gsub( '[0-9]. ', '', . )
v.q.names <- df.indicator.names %>% pull(name, name = variable) %>%
  gsub( '[0-9]. ', '', . ) %>% gsub( '&', 'and', . ) 

df.spec <- df.spec.compare %>%
  mutate(y.q=paste0(y, '.', q)) %>%
  complete( run, y.q) %>%
  group_by(y.q) %>%
  mutate( R2=mean(R2, na.rm=TRUE ) %>% sprintf('%.3f', .)) %>%
  ungroup %>%
  mutate(value=ifelse(is.na(q), '\\xmark', '\\checkmark') ) %>%
  separate( y.q, into=c('y','q'), sep='\\.') %>%
  spread( run, value ) %>%
  filter(q %in% v.q.unique) %>%
  mutate( q=v.q.names[q], y=v.var.names[y] ) %>%
  select( y, q, 'psuedo-$R^2$'=R2, l.runs %>% unlist )
    # The upper half of the table

df.combine <- df.spec %>%
  mutate( y=ifelse( y==c('', y[-n()]), '', y ) ) %>%
  rbind( df.fit ) %>%
  mutate( y=paste0('\\qquad ', y) ) %>%
  rename(' '=y, '  '=q)

tab.combine <- xtable(df.combine, align = c('l', 'l', 'l', rep('r', ncol(df.combine)-2 )) )

addtorow <- list(
  pos = list(0, nrow(df.spec), nrow(df.spec)+3 ),
  command = c( 
    paste0( '\\midrule \n\\emph{Extra variables}', paste0(rep(' & ', ncol(df.combine)-1 ), collapse='' ), '\\\\ \n'  ),
    paste0( '\\midrule \\emph{In-sample Fit}', paste0(rep(' & ', ncol(df.combine)-1 ), collapse='' ), '\\\\ \n'  ),
    paste0( '\\emph{St. dev. of GDP estimate (percent)}', paste0(rep(' & ', ncol(df.combine)-1 ), 
                                                                   collapse='' ), '\\\\ \n'  ) )
)

print(tab.combine, floating = FALSE, include.rownames = FALSE, type = 'latex', 
      sanitize.colnames.function = identity, add.to.row = addtorow, 
      hline.after=c( -1, nrow(df.combine) ),
      sanitize.text.function=identity, booktabs = TRUE, 
      file=paste0( 'outputs/fit_compare.tex' ) )
