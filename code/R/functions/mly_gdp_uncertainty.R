##########################################################################################
#
# mly_gdp_uncertainty.R
#
# Functions for computing estimated uncertainty
#
# Philip Barrett, Washington DC
# First version: 14feb2025
#
##########################################################################################


ks.theta <- function()



ks.boot <- function( ){
  
}



ks.ar.boot <- function( state.est, all.dfs, x.defs, q.defs, qly.data, df.names, df.sign, options ){
# Computes the Kalman Filter errors by bootstrapping the AR part of the model
  
  set.seed(42)
  
  #### 1. Extract things from lists ####
  n.sim <- options$n.sim
  logsigma2.rho.mean <- state.est$big.logsigma2.rho$mu
  logsigma2.rho.cov <- state.est$big.logsigma2.rho$cov
  x.names <- x.defs %>% names %>% set_names(.,.)
  y.names <- q.defs %>% names %>% set_names(.,.)
  v.states <- c( x.names, y.names ) %>% set_names(.,.)
  state.names <- c( x.names, y.names )
  l.df.hats <- all.dfs$hats
  l.df.trends <- all.dfs$trends
  n.states <- v.states %>% length()
  ar.order <- options$ar.order
  v.small.number <- options$v.small.number
  do.alt.est <- options$do.alt.est
  max.state.draws <- 100
  
  #### 2. Simulate the ar parameters ####
  m.param.sim <- rmvnorm( n.sim, logsigma2.rho.mean, logsigma2.rho.cov )
  
  #### 3. Re-run the filter ####
  df.out.sim <- data.frame()
  message('')
  for(i.sim in 1:n.sim){
    
    message('\n***** Bootstrap = ', i.sim, ' *****\n')
    
    ## 3.1 Make the new Kalman Filter
    do.state.iter <- TRUE ; i.state.iter <- 0
    this.param <- m.param.sim[i.sim,]
    
    # Loop to redraw state variable parameters if the covariance is not PD
    while(do.state.iter & i.state.iter < max.state.draws ){
      this.m.ar <- matrix( this.param, nrow=ar.order+1, ncol=n.states ) %>% 
        set_colnames(c(x.names, y.names)) %>%
        set_rownames(c('sigma2', paste0('rho', 1:ar.order)))
          # Create the matrix of parameters
      this.m.ar[1,] <- exp(this.m.ar[1,])
          # Un-log the variance
      this.state.est <- state.eq.est( x.names, y.names, l.df.hats, options, m.ar.in = this.m.ar )
          # Create the state transition equations
      eigen_decomp <- eigen(this.state.est$Q)
      eigenvalues <- eigen_decomp$values
          # Check that Q is PD
      if(all(eigenvalues>v.small.number)){
        do.state.iter <- FALSE
          # If so, ok
      }else{
        this.param <- rmvnorm( 1, logsigma2.rho.mean, logsigma2.rho.cov )
        i.state.iter <- i.state.iter + 1
        message( '  * Non-PD Q matrix detected, redrawing, attempt ', i.state.iter, ' *')
          # If not, re-draw
      }
    }
      
    this.Q.est <- if(do.alt.est){
      obs.eq.est.resid.alt( x.names, y.names, q.defs, l.df.hats, this.state.est, options )
    }else{
      this.Q.est <- obs.eq.est.resid( x.names, y.names, q.defs, l.df.hats, this.state.est, options )
    }
    this.kf.lag <- make.kf.lag.apx( l.df.hats, l.df.trends, this.state.est, this.Q.est,
                                    qly.data, x.defs, q.defs, options )
    this.kf.ham <- kf_laglist_to_ham(this.kf.lag)
    this.kf.ham$C.0 <- kf_ricatti(this.kf.ham)
    this.kf.filtered <- kf_filter_ham(this.kf.ham)
    this.kf.smoothed <- kf_smooth( this.kf.filtered, this.kf.ham$F )
    
    ## 3.2 Create some output ##
    this.df.out <- ks.process( this.kf.smoothed, l.df.trends, df.names, qly.defs, df.sign, v.states )$df %>%
      mutate( i.sim=i.sim ) %>%
      select( i.sim, everything() )
    df.out.sim <- df.out.sim %>% rbind(this.df.out)
    
  }
  return(df.out.sim)
}

