##########################################################################################
#
# robustness.R
#
# Computes the robustness exercise
#
# Philip Barrett, Washington DC
# First version: 06jun2025
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

## 0.1 Useful function ##
simple_names <- function(x) x %>%
  gsub('([0-9]|[0-9][a-b])\\. ', '', .) %>%
  gsub('consumption', '', .) %>%
  gsub('Non-', 'N', .) %>%
  gsub( '(?i)Residential fixed Investment', 'RFI', . ) %>%
  gsub('GDP(.*)', 'GDP', .) %>%
  gsub('ernment|entories', '\\.', .)
    # Name-simplifying function

#### 1. Validation on the consumption series ####

## 1.1 Set up
base.dir <- 'baseline_no_gov'
val.dir <- c( 'Including aggregation eror'='baseline_no_gov_err',
              ' Efficient GMM'='baseline_no_gov_eff',
              ' Third-order trend'='baseline_no_gov_trend_3',
              ' Fitting 12 AR equations'='baseline_no_gov_eq_12' )

val.dir.names <- names(val.dir) %>% set_names(val.dir)
all.dirs <- c('Baseline'=base.dir, val.dir)

## 1.2 Read in the requisite data ##
load( paste0('outputs/', base.dir, '/final_data/save_names.rdata') )

df.level <- lapply( all.dirs, function(this.dir){
  df.in <- read_xlsx( paste0('outputs/', this.dir, '/final_data/output.xlsx'), 
                      sheet = 'level' )
  df.out <- df.in %>% mutate( run=this.dir ) %>% select( run, everything() )
} ) %>%
  reduce(full_join) %>%
  # select(-unit) %>%
  gather( variable, level, -run, -date ) %>%
  mutate( date=as.Date(date, origin = "1899-12-30") )

#### 2. Make the main comparison plot ##

## 2.1 Prepare the data ##
df.level.plot <- df.level %>%
  filter( run != base.dir ) %>%
  left_join( df.level %>%
               filter( run == base.dir ) %>%
               select(-run) %>%
               rename( level.baseline=level), by=c('date', 'variable' ) ) %>%
  mutate( diff=100*(level/level.baseline-1),
          run.cht=val.dir.names[run] ) %>%
  left_join( df.names, by='variable' ) %>%
  filter( variable!='err' )

ggplot( df.level.plot, aes( x=date, y=diff, group=run.cht, color=run.cht, lty=run.cht)) +
  geom_line() +
  facet_wrap(~var.name, scales='free_y') +
  theme_minimal()

df.level.tab.data <- df.level.plot %>%
  mutate( decade=paste0( 10*floor(year(date)/10), 's' )) %>%
  group_by( run.cht, var.name, decade ) %>%
  summarise( 'avg'=mean(diff), 'avg.abs'=mean(abs(diff)) )

df.level.diff <- df.level.tab.data %>%
  mutate( var.name=simple_names(var.name) ) %>%
  select(-avg.abs) %>%
  spread( var.name, avg ) %>%
  mutate(run.cht=ifelse(run.cht==c('',run.cht[-n()]), '', run.cht)) %>%
  set_names( c(' ', 'Decade', names({.})[-(1:2)]) )

df.level.abs.diff <- df.level.tab.data %>%
  mutate( var.name=simple_names(var.name) ) %>%
  select(-avg) %>%
  spread( var.name, avg.abs ) %>%
  mutate(run.cht=ifelse(run.cht==c('',run.cht[-n()]), '', run.cht)) %>%
  set_names( c(' ', 'Decade', names({.})[-(1:2)]) )


tab.level.diff <-
  xtable(df.level.diff, align = c('l', 'l', 'l', rep('r', ncol(df.level.diff ) -2) ), digits=3 )

print(tab.level.diff, floating = FALSE, include.rownames = FALSE, type = 'latex', 
      sanitize.colnames.function = identity, 
      sanitize.text.function=identity, booktabs = TRUE, 
      file= 'outputs/robustness_avg.tex' )

tab.level.abs.diff <-
  xtable(df.level.abs.diff, align = c('l', 'l', 'l', rep('r', ncol(df.level.diff ) -2) ), digits=3 )

print(tab.level.abs.diff, floating = FALSE, include.rownames = FALSE, type = 'latex', 
      sanitize.colnames.function = identity, 
      sanitize.text.function=identity, booktabs = TRUE, 
      file= 'outputs/robustness_abs_avg.tex' )

