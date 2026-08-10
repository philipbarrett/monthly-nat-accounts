##########################################################################################
#
# mly_gdp_kf_deriv.R
#
# Functions for computing the asymptotic covariances of the Kalman Filter estimates of monthly GDP
#
# Philip Barrett, Washington DC
# First version: 25feb2025
#
##########################################################################################

make.ks.cov <- function( l.dfs, v.x, v.y, l.q, state.est, Q.est, options ){
# Makes the covariance for the smoothed states w.r.t. the parameters
  
  l.psi <- make.psi.list(state.est, Q.est)
  l.meta <- l.psi$meta
  v.theta <- make.theta.H(l.psi)
      # Formulate the giant parameter vectors
  cov.theta <- make.asym.cov.theta( v.theta, l.dfs$hats, v.x, v.y, l.q, l.meta, options$n.order )
      # The covariance of the parameters
  ar.ks.deriv <- make.ks.deriv( l.dfs.all, state.est, Q.est,
                                l.quarterly.data, l.x, l.q, options, 
                                l.kf.lag, l.kf.smoothed )
      # The numerical derivative of the KS point estimates wrt each parameter
  v.states <- c( v.x, v.y )
  l.vcov <- lapply( 1:dim(ar.ks.deriv)[3], function(i) t(ar.ks.deriv[,,i]) %*% cov.theta %*% ar.ks.deriv[,,i] %>%
                        set_colnames(v.states) %>% set_rownames(v.states) )
      # Make the big covariance
  ar.vcov <- array( unlist(l.vcov), 
                    dim = c( dim(l.vcov[[1]]), length(l.vcov)) )
      # Make array format
  return(list( ar.vcov=ar.vcov, cov.theta=cov.theta, deriv=ar.ks.deriv, theta=v.theta ))
}

make.ks.deriv <- function(l.dfs, state.est, Q.est,
                          l.quarterly.data, l.x, l.q, options, 
                          l.kf.lag.init=NULL, l.kf.smooth.init=NULL ){
# Makes the derivatives of the KS point estimate w.r.t. theta
  
  # Unpack lists
  l.df.hats <- l.dfs$hats
  l.df.trends <- l.dfs$trends
  
  # Initialize (if required)
  if( is.null(l.kf.lag.init)){
    l.kf.lag.init <- make.kf.lag.apx( l.df.hats, l.df.trends, state.est, Q.est,
                                 l.quarterly.data, l.x, l.q, options )
  }
  l.kf.ham.init <- kf_laglist_to_ham(l.kf.lag.init)
  l.kf.ham.init$C.0 <- kf_ricatti(l.kf.ham.init)
  if(is.null(l.kf.smooth.init)){
    l.kf.smooth.init <- kf_smooth( kf_filter_ham(l.kf.ham.init), l.kf.ham.init$F )
  }
    # Initial KS solution
  
  # Extract lists
  l.psi <- make.psi.list(state.est, Q.est)
  l.meta <- l.psi$meta
  v.theta <- make.theta.H(l.psi)
  
  # Make the derivative
  v.inc <- pmax( pmin( 1e-05 * v.theta, 1e-05 ), 1e-06 )
  n.theta <- v.theta %>% length()
  n.pds <- l.kf.smooth.init$xi.smooth %>% nrow
  ar.d.ks <- array( NA, dim=c( n.theta, l.meta$n.N, n.pds ) )
    # Set up the outgoing derivative
  state.est.upper <- state.est.lower <- state.est
  Q.est.upper <- Q.est.lower <- Q.est
    # Set up the state and observation estimates to change in the derivatives
  
  # browser()
  for( i in 1:n.theta ){
    message('  Differentiating Kalman output w.r.t. theta, dimension ', i, ' of ', n.theta )
    theta.upper <- v.theta ; theta.upper[i] <- v.theta[i] + v.inc[i]
    theta.lower <- v.theta ; theta.lower[i] <- v.theta[i] - v.inc[i]
      # The upper and lower values for theta
    l.psi.upper <- theta.to.psi.list( theta.upper, l.meta )
    l.psi.lower <- theta.to.psi.list( theta.lower, l.meta )
      # The Psi-form equivalents
    state.est.upper$ar.coef <- rbind( diag( l.psi.upper$Q ), l.psi.upper$rho ) %>%
      set_colnames(l.psi$meta$v.states)
    state.est.upper$Q <- l.psi.upper$Q
    Q.est.upper$kappa <- l.psi.upper$kappa
    Q.est.upper$R <- l.psi.upper$R
    l.kf.lag.upper <- make.kf.lag.apx( l.df.hats, l.df.trends, state.est.upper, Q.est.upper,
                                      l.quarterly.data, l.x, l.q, options )
    l.kf.lag.upper$C.0 <- l.kf.ham.init$C.0
      # Don't redo the Ricatti
    l.kf.ham.upper <- kf_laglist_to_ham(l.kf.lag.upper)
    l.kf.smoothed.upper <- kf_smooth( kf_filter_ham( l.kf.ham.upper ), l.kf.ham.upper$F )
      # The upper value of the KS
    state.est.lower$ar.coef <- rbind( diag( l.psi.lower$Q ), l.psi.lower$rho ) %>%
      set_colnames(l.psi$meta$v.states)
    state.est.lower$Q <- l.psi.lower$Q
    Q.est.lower$kappa <- l.psi.lower$kappa
    Q.est.lower$R <- l.psi.lower$R
    l.kf.lag.lower <- make.kf.lag.apx( l.df.hats, l.df.trends, state.est.lower, Q.est.lower,
                                       l.quarterly.data, l.x, l.q, options )
    l.kf.lag.lower$C.0 <- l.kf.ham.init$C.0
    l.kf.ham.lower <- kf_laglist_to_ham(l.kf.lag.lower)
    l.kf.smoothed.lower <- kf_smooth( kf_filter_ham(l.kf.ham.lower), l.kf.ham.lower$F )
      # The upper value of the KS
    xi.upper <- l.kf.smoothed.upper$xi.smooth[,1:l.meta$n.N]
    xi.lower <- l.kf.smoothed.lower$xi.smooth[,1:l.meta$n.N]
    xi.deriv <- ( xi.upper - xi.lower ) / (2*v.inc[i])
      # Compute the derivative
    ar.d.ks[i,,] <- t(xi.deriv)
  }
  
  return(ar.d.ks)
}