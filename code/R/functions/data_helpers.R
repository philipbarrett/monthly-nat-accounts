##########################################################################################
#
# data_helpers.R
#
# Functions to help getting and processing the data
# Philip Barrett, Washington DC
# First version: 24jan2025
#
##########################################################################################

## Conversion functions ##
pc.diff <- function(x, p=1) ( c( rep(NA, p), diff( x, p ) ) / lag(x,p) )
pc.diff.100 <- function(x, p=1) 100 * pc.diff( x, p )
pc.diff.400 <- function(x, p=1) 400 * pc.diff( x, p )
pc.diff.m <- function(x, p=1) 12 / p * pc.diff.100( x, p )
pc.diff.12m <- function(x) pc.diff.m(x, 12)
p.diff <- function(x, p=1) c( rep(NA, p), diff( x, p ) )
mu0.sig1 <- function(x) (x-mean(x,na.rm=T))/sd(x, na.rm=T)
date.adjust <- function(this.date) ifelse(day(as.Date(this.date))==1,
                        this.date, as.Date(this.date)+1) %>% as.Date()
  # Adjusts for the problem due to python calling time-zone dependent dates.

## Data pull functions now live in code/R/functions/data_sources/{fred,bea,
## splice_historical,pull_public}.R (pull.public.list()). The original Haver
## functions are quarantined in code/R/functions/legacy_haver/data_helpers_haver.R
## -- archival reference only, not sourced by default. See docs/data_sources.md.

make.gdp.data <- function( data.qly, data.mly, derived.data, x, q, options ){
  # Makes the data for the GDP project.  Includes checks.
  
  #### 1. Set up ####
  
  ## 1.1 Unpack options ##
  start.date <- options$start.date
  end.date <- options$end.date
  make.gdp.err <- options$make.gdp.err
  pull.data <- options$pull.data   # TRUE: pull fresh from public APIs. FALSE: read cached input_data/*.csv
  save.dir <- options$save.dir
  
  ## 1.2 The signs of the variables ##
  df.sign <- data.frame(sign=c( gdp=1, sapply(data.qly$components, 
                                              function(x) ifelse( is.null(x$sign),1,x$sign) ) ),
                        err.sign=c( gdp=-1, sapply(data.qly$components, 
                                                   function(x) ifelse( is.null(x$sign),1,x$sign) ) )) %>%
    rownames_to_column('variable')
  
  
  #### 2. Download and format the data ####
  
  ## 2.1 Quarterly data ##
  if(pull.data){
    df.qly.in <- pull.public.list( c( list( target=data.qly$target ),
                                     data.qly$components ), options=options )
    df.y <- df.qly.in %>%
      mutate( date=date.adjust(date) %m+% months(2) )%>%
      filter(date >=start.date, date<=end.date) 
      # Clean and prep
    
    write_csv( df.y, file=paste0( save.dir, 'input_data/quarterly.csv' ) )
  }else{
    df.y <- read_csv( paste0( save.dir, 'input_data/quarterly.csv' ) )
  }
  
  if(make.gdp.err){
  # NB. Compute error *after* reading in for speed
    df.y <- df.y %>%
      left_join( {.} %>% gather(variable, value, -date ) %>%
                   left_join(df.sign, by='variable') %>%
                   mutate(sig.val=err.sign*value) %>%
                   group_by(date) %>%
                   summarise( err= -sum(sig.val) ) %>%
                   ungroup(), by=c('date') )
    # Add the aggregation error
    data.qly$components$err <- list( label='err', name='9. NIPA Aggregation error', level.trend=TRUE)
    df.sign <- data.frame(sign=c( gdp=1, sapply(data.qly$components, 
                                                function(x) ifelse( is.null(x$sign),1,x$sign) ) ),
                          err.sign=c( gdp=-1, sapply(data.qly$components, 
                                                     function(x) ifelse( is.null(x$sign),1,x$sign) ) )) %>%
      rownames_to_column('variable')
    # Redefine things to match the aggregation error
  }
  
  ## 2.2 Monthly data ##

  # Full month grid, built here (not just at 2.5) so df.mly can be padded onto it
  # immediately after the pull -- see note below for why that matters.
  df.t <-
    data.frame( date = seq.Date( as.Date(start.date),
                                 as.Date(end.date), by='month' ) ) %>%
    mutate( t=1:n() )

  if(pull.data){
    # 2.2.1 The regular data
    df.mly.in <- pull.public.list(data.mly, options=options)
    df.mly <- df.mly.in %>%
      # mutate( date=as.Date(date) + 1 ) %>%
      mutate( date=date.adjust(date) ) %>%
      filter(date >=start.date, date<=end.date)
      # Download the monthly data
    # Pad onto the full month grid: pull.public.list() full_joins independently-
    # pulled series, so it only has a row for a month if AT LEAST ONE series has
    # data there -- e.g. if the earliest available series starts in 1958, no row
    # exists at all for 1950-1957, rather than an NA-filled one. The old Haver
    # pull didn't have this gap (a single bulk API call returned a rectangular
    # NA-padded matrix from a common start date), and downstream code (e.g.
    # make.kf.lag.apx()'s row-count equality checks) relies on that "one row per
    # month, always" contract. Restoring it here, once, rather than patching
    # every downstream consumer that assumes it.
    df.mly <- df.t %>% select(date) %>% left_join( df.mly, by='date' )
    write_csv( df.mly, file=paste0( save.dir, 'input_data/monthly.csv' ) )
  }else{
    df.mly <- read_csv( paste0( save.dir, 'input_data/monthly.csv' ) )
  }
  
  # 2.2.2 The derived series
  l.df.derived <- NULL
  for(i in 1:length(l.derived.series)){
    l.df.derived[[i]] <- df.mly %>% 
      mutate( !!(derived.data[[i]]$label) := eval(rlang::parse_expr(derived.data[[i]]$fn)) ) %>%
      select( date, derived.data[[i]]$label )
  }
  df.derived <- l.df.derived %>% reduce( full_join, by='date' )
    # Create the compound data series
  df.mly <- df.mly %>% full_join( df.derived, by='date' )
    # Merge back in to the monthly data
  
  ## 2.3 Select the exact monthly components ##
  x.rename <- sapply( x, function(z) z$from ) %>%
    set_names( sapply( x, function(z) z$label ) )
  df.x <- df.mly %>%
    mutate_at( vars(x.rename), function(x) x/3) %>%
    select( date, x.rename )
  
  ## 2.4 Make monthly indicators ##
  df.q <- df.mly %>%
    select( date, unlist(q) %>% unname() )
  
  
  ## 2.5 Add in a unique time identifier ##
  # (df.t already built in 2.2, reused here)
  df.y <- df.y %>%
    left_join(df.t, by='date') %>%
    select(date, t, everything() ) %>%
    filter( t %% 3 == 0)
  df.q <- df.q %>%
    left_join(df.t, by='date') %>%
    select(date, t, everything() )
  df.x <- df.x %>%
    left_join(df.t, by='date') %>%
    select(date, t, everything() )
  # Duh
  
  ## 2.6 Check that things which should add up do add up
  df.x.y.check <- df.x %>%
    gather( variable, value, -date, -t ) %>%
    mutate( t=3*ceiling(t/3) ) %>%
    group_by( t, variable ) %>%
    summarise( date=max(date), mly=sum(value) ) %>%
    select( date, t, variable, mly ) %>%
    left_join( df.y %>% gather( variable, qly, -date, -t ), 
               by=c('date', 't', 'variable') ) %>%
    mutate( diff=mly-qly, pc.diff100=diff/qly*100 ) %>%
    ungroup() %>%
    arrange( variable, date )
  # The quarterly aggregations
  df.x.y.check.10 <- df.x.y.check %>%
    mutate( decade=10*floor(year(date)/10)) %>%
    group_by( decade, variable ) %>%
    summarise( nobs=n(),
               pc.diff.100=mean(pc.diff100, na.rm=T),
               abs.pc.diff.100=mean(abs(pc.diff100), na.rm=T) )
  message('Quarterly/monthly data consistency report' )
  df.x.y.check.10 %>% print(n=Inf)
  
  #### 3. Set up the output ####
  l.out <- list( df.y=df.y, df.x=df.x, df.q=df.q, 
                 df.check=df.x.y.check, data.qly=data.qly, df.sign=df.sign )
  return(l.out)
}

make.trend <- function(df, var.name, order=3, do.log=TRUE){
  # Makes the log trend of the given series
  if(do.log){
    df.lm <- df %>% select( t, var.name ) %>%
      mutate( log.var = log(get(!!var.name) ) )
    lm.trend <- lm( log.var ~ poly(t, order), df.lm )
    df.out <- df %>% select( date, t, var.name ) %>%
      mutate( !!paste0(var.name, '.bar') := exp(predict(lm.trend, newdata=df )) ) %>%
      select(-var.name)
  }else{
    lm.trend <- lm( paste0( var.name, '~ poly(t, order)') %>% as.formula(), df )
    df.out <- df %>% select( date, t, var.name ) %>%
      mutate( !!paste0(var.name, '.bar') := predict(lm.trend, newdata=df ) ) %>%
      select(-var.name)
  }
  return( list( df=df.out, lm=lm.trend ) )
}

make.trend.dfs <- function( l.dfs, data.qly, options ){
  # Makes the trend data frames
  
  #### 1. Set up ####
  
  ## 1.1 Unpack options ##
  trend.order <- options$trend.order
  
  ## 1.2 Unpack data ##
  df.y <- l.dfs$df.y
  df.x <- l.dfs$df.x
  df.q <- l.dfs$df.q
  
  #### 2. Make the trends ####
  
  ## 2.1 Quarterly data ##
  l.trend.y <- lapply( df.y %>% select(-date, -t) %>% names() %>% set_names(.,.),
                       make.trend, df=df.y, order=trend.order )
  v.y.no.log <- data.qly$components %>%
    sapply( function(x) x$level.trend ) %>%
    unlist() %>% names()
  # The names of the variables with non-log trends
  if(length(v.y.no.log) > 0) for(this.var in v.y.no.log){
    l.trend.y[[this.var]] <- make.trend( df.y, this.var, trend.order, do.log=FALSE )
  }
  df.y.bar <- l.trend.y %>% lapply( function(z) z$df ) %>%
    reduce( full_join, by=c('date', 't') )
  # The quarterly trend data
  
  ## 2.2 Monthly states ##
  l.trend.states <- 
    lapply( c(data.qly$target$label, 
              sapply(data.qly$components, function(x) x$label) ),
            function(var.name){
              df.reg <- df.x %>% select( date, t ) %>%
                mutate( t=t+1 )
              df.trend <- df.x %>% select( date, t ) %>%
                mutate( !!paste0(var.name, '.bar') := 
                          exp( predict(l.trend.y[[var.name]]$lm, newdata=df.reg ) ) / 3 )
              # Create the trend using the quarterly formula
              return(df.trend)
            } )
  # Apply the quarterly trends to get approximate monthly trends
  if(length(v.y.no.log) > 0){
    for(this.var in v.y.no.log){
      df.reg <- df.x %>% select( date, t ) %>%
        mutate( t=t+1 )
      l.trend.states[[this.var]] <- 
        df.x %>% select( date, t ) %>%
        mutate( !!paste0(this.var, '.bar') := 
                  predict(l.trend.y[[this.var]]$lm, newdata=df.reg ) / 3 )
    }
  }
  # Correct for non-log series
  df.states.bar <- l.trend.states %>%
    reduce( left_join, by=c('date', 't') )
  # Merge
  l.lm.out <- lapply(l.trend.y, function(x) x$lm )
  
  ## 2.3 Check that the trends line up for the states with monthly & quarterly
  ## data ##
  df.states.qly.check <- df.states.bar %>%
    gather( variable, value, -date, -t ) %>%
    mutate( t=3*ceiling(t/3) ) %>%
    group_by( t, variable ) %>%
    summarise( date=max(date), mly=sum(value) ) %>%
    select( date, t, variable, mly ) %>%
    full_join( df.y.bar %>% gather( variable, qly, -date, -t ), 
               by=c('date', 't', 'variable') ) %>%
    mutate( diff=mly-qly, pc.diff100=diff/qly*100 ) %>%
    ungroup() %>%
    arrange( date, variable )
  # Check that the state monthly trends match the quarterly sums.
  df.states.qly.check.var <- df.states.qly.check %>%
    mutate( decade=10*floor(year(date)/10)) %>%
    group_by( variable ) %>%
    summarise( nobs=n(),
               diff=mean(diff, na.rm=T),
               abs.diff=mean(abs(diff), na.rm=T),
               pc.diff.100=mean(pc.diff100, na.rm=T),
               abs.pc.diff.100=mean(abs(pc.diff100), na.rm=T) )
  message('Quarterly/monthly trend data consistency report' )
  df.states.qly.check.var %>% print(n=Inf)
  
  
  ## 2.4 Monthly indicators ##
  l.trend.q <- lapply( df.q %>% select(-date, -t) %>% names() %>% set_names(.,.),
                       make.trend, df=df.q, order=trend.order )
  df.q.bar <- l.trend.q %>% lapply( function(z) z$df ) %>%
    reduce( full_join, by=c('date', 't') )
  # The monthly trend data
  
  
  #### 3. Output ####
  return( list( df.y.bar=df.y.bar, df.states.bar=df.states.bar, 
                df.q.bar=df.q.bar, no.log=v.y.no.log, 
                df.bar.check=df.states.qly.check.var, lm=l.lm.out ) )
  
}

make.hat.dfs <- function( l.dfs, l.df.trend, options ){
  # Makes the deviations from trend datasets
  
  #### 1. Set up ####
  
  ## 1.1 Unpack data ##
  df.y <- l.dfs$df.y
  df.x <- l.dfs$df.x
  df.q <- l.dfs$df.q
  df.y.bar <- l.df.trends$df.y.bar
  df.states.bar <- l.df.trends$df.states.bar
  df.q.bar <- l.df.trends$df.q.bar
  v.q <- (df.q %>% select(-date, -t) %>% names())
  names(v.q) <- v.q
  ar.resids <- options$ar.resids
  ar.order <- options$ar.order
  obs.match.ar <- options$obs.match.ar
  
  #### 2. Make deviations ####
  
  ## 2.1 Quarterly data ##
  df.y.hat <- df.y %>%
    gather( variable, value, -date, -t ) %>%
    left_join( df.y.bar %>% 
                 gather( variable, trend, -date, -t) %>%
                 mutate( variable = gsub('.bar', '', variable) ),
               by=c('date', 't', 'variable' ) ) %>%
    mutate( hat=value/trend - 1 )
  
  
  ## 2.2 Monthly observed states ##
  df.x.hat <- df.x %>%
    gather( variable, value, -date, -t ) %>%
    left_join( df.states.bar %>% 
                 gather( variable, trend, -date, -t) %>%
                 mutate( variable = gsub('.bar', '', variable) ),
               by=c('date', 't', 'variable' ) ) %>%
    mutate( hat=value/trend - 1 )
  
  ## 2.3 Monthly data ##
  df.q.hat <- df.q %>%
    gather( variable, value, -date, -t ) %>%
    left_join( df.q.bar %>% 
                 gather( variable, trend, -date, -t) %>%
                 mutate( variable = gsub('.bar', '', variable) ),
               by=c('date', 't', 'variable' ) ) %>%
    mutate( hat=value/trend - 1 )
  
  ## 2.3 Monthly data ##
  max.p <- if(obs.match.ar) ar.order else 20
  l.arima.q <- lapply( v.q, function(this.q) df.q.hat %>% filter( variable==this.q ) %>% pull(hat) %>%
                         auto.arima(d=0, D=0, max.p=max.p, max.q= if(ar.resids) 0 else 3 ) )
  df.q.hat.resid <- 
    lapply( v.q, function(this.q) df.q.hat %>% filter( variable==this.q ) %>% 
              mutate( hat.resid=c( rep( NA,(l.arima.q[[this.q]] %>% resid() %>% start())[1]-1),
                                   l.arima.q[[this.q]] %>% resid ) ) ) %>%
    rbindlist
  # df.q.hat <- full_join( df.q.hat, df.q.hat.resid, by=c('date', 't', 'variable'))
  
  
  #### 3. Output ####
  return(list( df.y.hat=df.y.hat, df.x.hat=df.x.hat, df.q.hat=df.q.hat.resid, q.arima=l.arima.q ))
  
}