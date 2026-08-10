##########################################################################################
#
# mly_gdp_kalman.R
#
# Functions for handling the Kalman conversion and processing for the monthly GDP project
#
# Philip Barrett, Washington DC
# First version: 11feb2025
#
##########################################################################################


make.kf.lag.apx <- function(l.df.hats, l.df.trends, coef.state, coef.obs, data.qly, x, q, options ){
  # Makes the lag form of the Kalman filter, approximating using constant
  # coefficients
  
  #### 1. Extract from the input lists
  df.x.hat <- l.df.hats$df.x.hat
  df.y.hat <- l.df.hats$df.y.hat
  df.q.hat <- l.df.hats$df.q.hat
  df.Q.coefs <- coef.obs$df.coefs
  
  df.states.bar <- l.df.trends$df.states.bar
  ar.order <- options$ar.order
  v.small.number <- options$v.small.number
  do.q.resid <- options$obs.resids
  
  # 1.1 Get the trend ratios #
  df.trend.ratio <- l.df.trends$df.states.bar %>%
    gather( variable, small.y, -date, -t) %>%
    mutate( quarter = 3*ceiling(t/3) ) %>%
    left_join( l.df.trends$df.y.bar %>%
                 rename( quarter = t ) %>%
                 select(-date) %>%
                 gather( variable, big.y, -quarter),
               by=c('quarter','variable') ) %>%
    mutate( trend.ratio=small.y / big.y )
  df.trend.ratio.sumy <- df.trend.ratio %>%
    mutate(q.mth=1+ ((t-1) %% 3), variable=gsub('.bar', '', variable)) %>%
    group_by(variable, q.mth) %>%
    summarise( avg.trend.ratio=mean(trend.ratio, na.rm=TRUE),
               sd=sd(trend.ratio, na.rm=TRUE) ) %>%
    ungroup()
  # Basically no time variation in the ratio coefficients for the quarterly
  # GDP aggregation constriant.
  
  #### 2. Set up the empty Kalman Matrices ####
  
  ## 2.1 Dimensions ##
  n.pds <- df.q.hat$t %>% max()
  n.states <- length(data.qly$components)
  n.exact.mly <- length(x)
  n.infer.mly <- length(q)
  n.infer.data <- nrow(df.Q.coefs)
  m.ar.est.all <- coef.state$ar.coef
  
  ## 2.2 Kalman dimensions ##
  # browser()
  v.states <- m.ar.est.all %>% colnames() # c( x %>% names(), q %>% names() ) %>% set_names(.,.)
    # Constant term
  v.y <- v.states[!(v.states %in% v.x)]
  v.q.idx <- sapply( names(q), function(this.q) rep( which(v.y==this.q), q[[this.q]] %>% length ) ) %>% unlist()
  n.exact.qly <- n.states
  n.data <- n.exact.mly + n.infer.data + n.exact.qly
  
  ## 2.3 Make the empty matrices ##
  l.a.kf <- list( matrix(0, n.data, 1) )
    # Will fill with constant terms
  l.h.kf <- lapply( 1:max(ar.order+1,3), function(i)  matrix(0, n.data, n.states ) )
    # Need three lags of the states since we have the quarterly GDP contraint
  l.f.kf <- lapply( 1:max( ar.order,2), function(i) matrix(0, n.states, n.states ) )
    # The lags of the states
  m.R.kf <- matrix( 0, n.data, n.data )
  m.Q.kf <- matrix( 0, n.states, n.states )
  
  #### 3. Fill in the Kalman matrices ####
  if(!do.q.resid) l.a.kf[[1]][,1] <- c( rep(0, n.exact.mly), df.Q.coefs$kappa, rep(0,n.exact.qly) )
      # Intercepts for estimated relationship with the data
  for( i in 1:n.exact.mly) l.h.kf[[1]][i,i] <- 1
    # Slope coefficients for the exact relationship with the monthly data 
  for( i.q in 1:n.infer.data ){
    if(do.q.resid){
      # browser()
      this.rho <- c(1, -coef.state$ar.coef[-1,v.q.idx[i.q]] )
      this.kappa <- df.Q.coefs$kappa[i.q]
      for(i.h in 0:ar.order){
        l.h.kf[[i.h+1]][n.exact.mly+i.q,n.exact.mly+v.q.idx[i.q]] <- this.kappa * this.rho[i.h+1]
      }
    }else{
      l.h.kf[[1]][n.exact.mly+i.q,n.exact.mly+v.q.idx[i.q]] <- df.Q.coefs$beta[i.q]
        # Slope coefficients for the estimated relationships with the monthly data 
    }
  }
  # browser()
  for( i in 1:n.exact.qly ){
    for( k in 1:3){
      l.h.kf[[k]][n.exact.mly+n.infer.data+i,i] <- 
        df.trend.ratio.sumy %>% filter( variable==v.states[i], q.mth==k ) %>%
        pull(avg.trend.ratio)
    }
  } 
  # for( i in 1:n.exact.qly ) l.h.kf[[1]][n.exact.mly+n.infer.data+i,i] <- 
  #   l.h.kf[[2]][n.exact.mly+n.infer.data+i,i] <- l.h.kf[[3]][n.exact.mly+n.infer.data+i,i] <- 1/3
  # Slope coefficients for the exact relationship with the quarterly data. THIS STEP APPROXIMATE
  for(i.state in 1:n.states) for(i in 1:ar.order) l.f.kf[[i]][i.state,i.state] <- m.ar.est.all[1+i,i.state]
    # Estimated autocorrelation coefficients for the state
    # The flat form of the estimated data relationships
  for( i in 1:n.exact.mly) m.R.kf[i,i] <- v.small.number
      # Exact monthly error
  if(do.q.resid){
    m.R.kf[n.exact.mly+1:n.infer.data,n.exact.mly+1:n.infer.data] <- 
      coef.obs$R
  }else{
    l.lm.Q <- coef.obs$lm
    l.lm.Q.flat <- unlist( l.lm.Q, recursive = FALSE)
    for( i in 1:n.infer.data ) for( j in 1:n.infer.data ) m.R.kf[n.exact.mly+i,n.exact.mly+j] <- 
        cov( resid(l.lm.Q.flat[[i]]), resid(l.lm.Q.flat[[j]]), use='pairwise.complete.obs' )
    # Estimated observation errors
  }
  for( i in n.exact.mly + n.infer.data+1:n.exact.qly) m.R.kf[i,i] <- v.small.number
  # Exact quarterly error
  # diag(m.Q.kf) <- m.ar.est.all['sigma2',]
  m.Q.kf <- coef.state$Q
  # The estimated state innovation variance
  
  #### 4. Set up the data matrices ####
  m.X.kf <- matrix(1, n.pds)
  df.q.data <- if(do.q.resid) df.q.hat %>% select( date, t, variable, hat.resid ) %>%
    spread( variable, hat.resid ) else df.q.hat %>% select( date, t, variable, hat ) %>%
    spread( variable, hat )
  m.Y.kf <- df.x.hat %>% select( date, t, variable, hat ) %>%
    spread( variable, hat ) %>%
    left_join( df.q.data %>% 
                 select( date, t, q %>% unlist() %>% set_names(.,.) ),
               by=c('date', 't') ) %>%
    left_join( df.y.hat %>% select( date, t, variable, hat ) %>%
                 spread( variable, hat ) %>% 
                 select( date, t, v.states %>% set_names( ., paste0(., '.nipa')) ),
               by=c('date', 't') ) %>%
    select( -date, -t ) %>%
    as.matrix()
      # Observed data
  v.xi.kf.0 <- df.states.bar %>% 
    filter(t==1) %>%
    select( data.qly$components %>% sapply( function(x) x$label ) %>%
              paste0('.bar') %>%
              set_names( data.qly$components %>% 
                           sapply( function(x) x$label )) ) %>% 
    unlist() %>% rep(.,3) %>%
    matrix( ., nrow=3, byrow = TRUE ) * 0
      # Initial state
  
  l.c0.kf <- lapply( 1:3, function(i) m.Q.kf )
  # Initial state uncertainty
  
  #### 5. Create the output list ####
  l.out <- list( h.i=l.h.kf, f=l.f.kf, q=m.Q.kf, r=m.R.kf, xi.0=v.xi.kf.0, 
                 c.0=l.c0.kf, y=m.Y.kf, x=m.X.kf, a.i=l.a.kf )
  return(l.out)
  
}

ks.process <- function( ks, l.df.trends, df.names, data.qly, df.sign, v.states ){
  # Processes the kalman smoother output
  
  
  #### 1. Extract things from lists ####
  df.states.bar <- l.df.trends$df.states.bar
  n.states <- v.states %>% length
  n.pds <- (ks$xi.smooth %>% nrow()) - 1
  
  #### 2. Extract the smoothed states ####
  df.smooth <- data.frame( t=0:n.pds, 
                           ks$xi.smooth[,1:n.states] %>% set_colnames( v.states ),
                           sapply( 0:n.pds, function(i) sqrt(diag(ks$P.smooth[1:n.states,1:n.states,i+1])) ) %>%
                             t %>% set_colnames( paste0( v.states, '.ks_se' ) ) ) %>%
    right_join( df.states.bar %>% select( t, date ), by='t' ) %>%
    as_tibble() %>%
    gather( variable, value, -date, -t ) %>%
    separate( variable, c('variable', 'type'), sep='\\.' ) %>%
    mutate( type=ifelse(is.na(type), 'hat.ks', type )) %>%
    spread( type, value ) %>%
    select( date, t, everything() )
  # The smoother and standard errors for the components
  df.out.states <- df.states.bar %>% 
    gather( variable, trend, -date, -t) %>% 
    filter( variable %in% paste0(v.states, '.bar')) %>%
    mutate(variable=gsub('.bar', '', variable)) %>%
    left_join( df.smooth, by=c('date', 't', 'variable' ) ) %>%
    mutate( y.ks=trend *(1+hat.ks), y.ks.se=abs(trend)*ks_se )
  # Make the levels estimated for the states
  
  #### 3. Make the estimated GDP series ####
  df.out.y.level <- df.out.states %>% 
    left_join( df.sign, by='variable') %>%
    group_by(date, t) %>% 
    summarise( hat.ks=NA, y.ks=sum(sign*y.ks)) %>%
    left_join( df.states.bar %>% select( date, t, trend=gdp.bar ),
               by=c('date', 't') ) %>%
    mutate(variable='gdp', hat.ks=y.ks/trend - 1 ) %>%
    select( date, t, variable, trend, hat.ks, y.ks )
  # Make the output series from the sum of the components
  df.out.y.se <- 
    data.frame( t=rep( 0:n.pds, each=(n.states)^2), 
                ks_var=sapply( 0:n.pds, function(i) (ks$P.smooth[1:n.states,1:n.states,i+1]) %>%c ) %>% c,
                var.i=rep( c(v.x,v.y), n.states*(1+n.pds ) ),
                var.j=rep( rep( c(v.x,v.y), each=n.states), 1+n.pds ) ) %>%
    # mutate( t>0 ) %>%
    left_join( df.states.bar %>% select( t, date ), by='t' ) %>%
    left_join( df.out.states %>% select( date, t, var.i=variable, trend.x=trend ), by=c('t','var.i','date') ) %>%
    left_join( df.out.states %>% select( date, t, var.j=variable, trend.y=trend ), by=c('t','var.j','date') ) %>%
    left_join( df.sign %>% select( var.i=variable, sign.x=sign), by='var.i' ) %>%
    left_join( df.sign %>% select( var.j=variable, sign.y=sign), by='var.j' ) %>%
    mutate( cov=ks_var*trend.x*trend.y ) %>%
    group_by( date, t ) %>%
    summarise( y.ks.se=sqrt(sum(cov*sign.x*sign.y) ) ) %>%
    mutate(variable='gdp') %>%
    select( date, t, variable, everything() ) %>%
    as_tibble()
  # Make the standard error from the appropriate sum of covariances
  df.out.y <- df.out.y.level %>%
    left_join( df.out.y.se, by=c('date', 't', 'variable') )
  # Put them together
  
  #### 4. Put everything together ####
  df.out <- full_join( df.out.y, df.out.states, by=names(df.out.y)) %>%
    left_join( l.df.hats$df.y.hat %>% select(date, t, variable, nipa=value),
               by=c('date', 't', 'variable' ) ) %>%
    left_join( {.} %>% select( t, date, variable, y.ks ) %>%
                 mutate( t=3*ceiling(t/3) ) %>%
                 group_by( t, variable ) %>%
                 summarise( date=max(date), ks.nipa=sum(y.ks) ) %>%
                 ungroup(),
               by=c('date', 't', 'variable' ) ) %>%
    mutate( ks.nipa.err.pc100=100*(ks.nipa/nipa-1) ) %>%
    left_join( df.names, by='variable' ) %>%
    arrange( variable, date ) %>%
    ungroup()
  # One giant dataset
  df.se.out <- df.out %>%
    select( date, t, variable, y.ks.se ) %>%
    full_join( {.} %>% 
                 group_by(date,t) %>%
                 summarise(y.ks.var=sum(ifelse(variable=='gdp', y.ks.se^2, -y.ks.se^2)),
                           y.ks.se=sign(y.ks.var)*sqrt(abs(y.ks.var)) ) %>%
                 mutate( variable='cov.err' )
    ) %>%
    select(-y.ks.var)
  # Computes the covariance component for the SEs
  df.se.out.q <- df.se.out %>% 
    left_join(df.out %>% select( date, t, variable, trend), 
              by=c('date', 't', 'variable') ) %>%
    mutate( t=3*ceiling(t/3) ) %>%
    group_by( t, variable ) %>%
    summarise( date=max(date), y.ks.se=mean(y.ks.se ), trend=mean(trend) ) %>%
    select( date, t, variable, y.ks.se, trend) %>%
    left_join( {.} %>% filter(variable=='gdp') %>% select( date, t, gdp.trend=trend),
               by=c('t', 'date')) %>%
    mutate( y.ks.se.pc100=y.ks.se/trend*100,
            y.ks.se.pc100.gdp=y.ks.se/gdp.trend*100 ) %>%
    left_join( {.} %>% filter(variable=='gdp') %>% select( date, t, gdp.se.pc100=y.ks.se.pc100 ),
               by=c('date', 't') ) %>%
    mutate( y.ks.se.pc100.gdp.contrib=y.ks.se.pc100.gdp^2 / gdp.se.pc100 ) %>%
    left_join( df.names, by='variable' )
  # SE analysis is better at the quarterly frequency (less erratic jumping
  # around)
  return( list( df=df.out, df.se.q=df.se.out.q ) )
}


make.smoother.ll <- function( m.xi, l.P, m.Y, m.H, m.R ){
# Makes the log likelihood for the smoother
  
  m.Y.pred <- m.xi %*% t(m.H)
      # The predicted observables
  m.S <- array( NA, dim=c(nrow(m.H), nrow(m.H), dim(l.P)[3]) )
  m.S[] <- apply( l.P, 3, function(x) m.H %*% x %*% t(m.H) + m.R )
      # The observation variances
  loglik <- lapply( 1:dim(m.Y)[1], function(i){
    idx.obs <- !is.na(m.Y[i,])
    if(any(idx.obs)){
      this.Y.err <- m.Y[i,idx.obs] - m.Y.pred[i+1,idx.obs]
      this.S <- m.S[idx.obs,idx.obs,i] %>% as.matrix()
      # browser()
      # ll <- dmvnorm( x = this.Y.err, 
      #                mean = 0*this.Y.err,
      #                sigma = this.S, log = TRUE )
      S.inv <- rcond.iterate(this.S)
      ll <- -.5 * ( log(det(2*pi*this.S) ) +
                    t(this.Y.err) %*% solve(S.inv$A) %*% this.Y.err )
      
    }else{
      return(0)
    }
  } ) %>%
    reduce(sum)
      # The log likelihood
  return(loglik)
}