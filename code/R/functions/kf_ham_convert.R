###########################################################
# kf_ham_convert.R
# Functions for converting a generalized form of the 
# Kalman Filter to a dlm-input case
#
# Philip Barrett
# Washington DC, 01jun2022
###########################################################

library(mvtnorm)
library(dplyr); library(tidyr); library(readr); library(purrr); library(tibble)
library(stringr); library(forcats); library(lubridate); library(ggplot2)
  # Core tidyverse packages loaded directly, not via library(tidyverse) -- see
  # mly_GDP.R for why (avoids an unnecessary heavy dependency chain).
library(magrittr)

kf_laglist_to_ham <- function( l.lag ){
  # Wrapper for kf_lag_to_ham
  this.x <- if('x' %in% names(l.lag)) l.lag$x else NULL
    # Need this to stop incorrect partial matching of l.lag$xi.0
  out <- kf_lag_to_ham ( l.lag$h.i, l.lag$f, l.lag$q, l.lag$r, l.lag$xi.0, 
                         l.lag$c.0, l.lag$y, this.x, l.lag$a.i, l.lag$b.i )
  return(out)
}

kf_lag_to_ham <- function( l.h.i, m.f, m.q, m.r, xi.0, l.c.0, m.y=NULL, m.x=NULL, l.a.i=NULL, l.b.i=NULL ){
# Converts the lagged version of the Kalman Filter to the Hamilton form
#
# From: y[t] = sum_0^nx a[i]*x[t-i] + sum_1^ny b[i]*y[t-i] + sum_0^nxi h[i]*xi[t-i] + w[t]
#       xi[t] = sum_1^nxi f[i]*xi[t-i] + v[t]
#       cov(v) = q   cov(w) = r
#
# To:   Y[t] = A * X[t] + H*Xi[t] + W[t]
#       Xi[t] = F*Xi[t-1] + V[t]
#       cov(V) = Q   cov(W) = R
  
  ## 0. Dimensions
  n.xi <- m.f[[1]] %>% ncol
  n.y <- m.y %>% ncol
  n.x <- m.x %>% ncol
  # The number of states
  n.x.lags <- (l.a.i %>% length) - 1
  n.y.lags <- (l.b.i %>% length)
  n.xi.lags <- (l.h.i %>% length) - 1
  # The number of lags
  T.x <- ( m.x %>% na.omit() %>% nrow ) - n.x.lags
  T.y <- ( m.y %>% nrow ) - n.y.lags
  # The number of simulation periods.  Should be equal.  Will throw an error
  # in the concatenation stage if not.  If it fails
  if( !is.null(m.x) ){
    if( T.x != T.y ) stop('Make sure x & y are formatted to have the same length after lag adjustment')
  }
  
  ## 1. The matrices
  big.A <- if( is.null(l.a.i) && is.null(l.b.i) ) NULL else c( l.a.i, l.b.i ) %>% reduce(cbind)
  big.H <- l.h.i %>% reduce(cbind)
  # big.F <- if(n.xi.lags==0) m.f else  rbind( cbind( m.f, matrix(0, n.xi, n.xi*n.xi.lags ) ), 
  big.F <- if(n.xi.lags==0) m.f else  rbind( cbind( m.f %>% reduce(cbind), matrix(0, n.xi, n.xi ) ), 
                                             cbind( diag( n.xi*n.xi.lags ), 
                                                    matrix(0, n.xi*n.xi.lags, n.xi ) ) )
  big.R <- m.r
  big.Q <- if(n.xi.lags==0) m.q else rbind( cbind( m.q, matrix(0, n.xi, n.xi*n.xi.lags ) ),
                                            matrix(0, n.xi*n.xi.lags, n.xi*(n.xi.lags+1) ) )
  diag( big.Q ) <- pmax(diag(big.Q), 1e-10 )
  big.Xi.0 <- if(n.xi.lags==0) xi.0 else xi.0[(nrow(xi.0):1),] %>% t %>% c
  big.C.0 <- 0*diag( n.xi*(n.xi.lags+1) )
  for( i in 0:n.xi.lags ) big.C.0[ i*n.xi+1:n.xi, i*n.xi+1:n.xi ] <- l.c.0[[n.xi.lags+1-i]]
  # Need to reorient the 0 entry since the oldest lag comes first in the input
  C.0.correl <- big.C.0
  # for( i in 1:(5*n.x.lags) ) C.0.correl <- big.F %*% C.0.correl %*% t(big.F)
    # Trying to partly adjust for serial correlation in the state somewhat sensibly
  # browser()
  C.scale <- diag( sqrt(diag(big.C.0) / diag(C.0.correl) ) )
  C.0.new <- C.scale %*% C.0.correl %*% t(C.scale)
  big.C.0 <- C.0.new
    # Adjusted for autocorrelation
  
  ## 2. The data
  # max.data.lags <- max( n.x.lags, n.y.lags )
  # big.Y <-  m.y[max.data.lags+(1:T.y),]
  big.Y <-  m.y[n.y.lags+(1:T.y),]
  big.X <- 
    ( cbind( if(n.x.lags<0) NULL else lapply( 0:n.x.lags, 
                                              function(i) matrix(m.x[n.x.lags-i+(1:T.x),], ncol=n.x) %>% set_colnames(colnames(m.x)) ) %>% 
               reduce(cbind),
             if(n.y.lags==0) NULL else lapply( 1:n.y.lags, function(i) m.y[n.y.lags-i+(1:T.y),] ) %>% 
               reduce(cbind) ) ) 
  if(!is.null(big.X)) big.X <- big.X %>%
    set_colnames( paste0(colnames(.), '.l', c( if(n.x.lags<0) NULL else rep(0:n.x.lags, each=n.x),
                                               if(n.y.lags==0) NULL else rep(1:n.y.lags, each=n.y) ) ) )
  if(!is.null(big.X)) colnames(big.A) <- colnames(big.X)
  
  return( list( F=big.F, A=big.A, H=big.H, X=big.X, Y=big.Y, R=big.R, Q=big.Q,
                Xi.0=big.Xi.0, C.0=big.C.0 ) )
}

kf_lag_sim <- function(  l.h.i, m.f, m.q, m.r, n.pds, xi.0=NULL, y.0=NULL, 
                         m.x=NULL, l.a.i=NULL, l.b.i=NULL, C.0=NULL, xi.0.mean=NULL ){
  # Simulates the lag-format version of the model:
  #
  #       y[t] = sum_0^ny a[i]*x[t-i] + sum_1^ny b[i]*y[t-i] + sum_0^nxi h[i]*xi[t-i] + w[t]
  #       xi[t] = f*xi[t-1] + v[t]
  #       cov(v) = q   cov(w) = r
  
  ## 1. The number of lags and periods
  n.xi.lags <- (l.h.i %>% length) - 1
  n.x.lags <- (l.a.i %>% length) - 1
  n.y.lags <- (l.b.i %>% length)
  # The number of lags
  n.xi <- m.q %>% ncol
  n.y <- m.r %>% ncol
  n.x <- if(n.x.lags<0) 0 else m.x %>% ncol
  # Number of dimensions
  T.sim <- if(!is.null(m.x)) (m.x %>% nrow) - n.x.lags else n.pds
  # The number of periods to simulate
  
  # 1.1 Error checking
  if( (n.y.lags>0) && is.null(y.0) ){
    stop('Must have initial observation data if the observation equation has an auto-regressive component')
  }
  if(n.y.lags>1){ if( ncol(y.0) == n.y ) y.0 <- y.0 %>% t }
  # Rotate the initial observations if required.  Don't use AND because ncol(y.0) only exists for n.y>1
  if(n.x.lags>=0 && ncol(m.x) == n.x ) m.x <- m.x %>% t
  # Rotate the data if required
  if( (n.x.lags>0) && is.null(m.x) ){
    stop('Must have exogenous data if the observation equation has an exogenous component')
  }
  
  
  ## 2. Create the state variable simulation
  
  # 2.1 Make the initial value if required
  if(is.null(xi.0)){
    xi.0 <- matrix( NA, n.xi, n.xi.lags + 1 )
    # Create empty xi.0
    if(is.null(xi.0.mean)) xi.0.mean <- rep(0, nrow(C.0))
    xi.0[,1] <- rmvnorm( 1, xi.0.mean, C.0 )
    # Initialize xi.0
    if(n.xi.lags>0){ 
      v.0 <- rmvnorm(n.xi.lags, m.q) %>% t
      # The innovations for xi.0
      for( t in 1:n.xi.lags ) xi.0[,t+1] <- m.F %*% xi.0[,t] + v.0[,t]
      # Populate xi.0
    }
  }
  
  # 2.2 Simulate the state
  xi <- matrix( NA, n.xi, T.sim + n.xi.lags + 1 )
  xi[,1:(1+n.xi.lags)] <- xi.0 %>% t
  # Fill in the initial values of the (lagged) state
  v <- rmvnorm(T.sim, rep(0,n.xi), m.q) %>% t
  # The innovations
  for( t in 1:T.sim ){
    xi[,n.xi.lags+t+1] <- m.f %*% xi[,n.xi.lags+t] + v[,t]
    # The simulated state
  }
  
  ## 3. Create the observation simulation
  y <- matrix( NA, n.y, T.sim + n.y.lags )
  if(n.y.lags>0) y[,1:n.y.lags] <- y.0
  # Populate the lagged values of y
  w <- rmvnorm(T.sim, rep(0,n.y), m.r) %>% t
  # The innovations
  for( t in 1:T.sim ){
    state.part <- ar.part <- exog.part <- 0
    if(n.x.lags>=0) for( i in 0:n.x.lags ) exog.part <- exog.part + l.a.i[[i+1]] %*% m.x[,n.x.lags+t-i]
    if(n.y.lags>=1) for( i in 1:n.y.lags ) ar.part <- ar.part + l.b.i[[i]] %*% y[,n.y.lags+t-i]
    for(i in 0:n.xi.lags ) state.part <- state.part + l.h.i[[i+1]] %*% xi[, t+n.xi.lags-i ]
    y[,n.y.lags+t] <- exog.part + ar.part + state.part + w[,t]
    # Create the lagged dynamic model
  }
  
  
  ## 3. Assemble into an output matrix
  n.max.lag <- max( n.xi.lags, n.x.lags-1, n.y.lags-1 )
  # The max number of lags
  big.T <- (-n.max.lag):T.sim
  # The period indices
  m.out <- cbind( t =big.T,
                  v =rbind( matrix( NA, n.max.lag + 1, n.xi ),
                            v %>% t ), 
                  w =rbind( matrix( NA, n.max.lag + 1, n.y ),
                            w %>% t ), 
                  x =if(n.x==0) NULL else rbind( matrix( NA, n.max.lag - n.x.lags + 1, n.x ),
                                                 m.x %>% t ),
                  xi=rbind( matrix( NA, n.max.lag - n.xi.lags, n.xi ),
                            xi %>% t ),
                  y =rbind( matrix( NA, n.max.lag - n.y.lags + 1, n.y ),
                            y %>% t )
  ) %>% set_colnames( c( 't', paste0('v.', 1:n.xi), paste0('w.', 1:n.y), 
                         if(n.x==0) NULL else paste0('x.', 1:n.x), 
                         paste0('xi.', 1:n.xi), paste0('y.', 1:n.y) ) )
  
  return(m.out)
}