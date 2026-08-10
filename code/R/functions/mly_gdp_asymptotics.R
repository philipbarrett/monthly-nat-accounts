##########################################################################################
#
# mly_gdp_asymptotics.R
#
# Functions for computing the asymptotic covariances of the Kalman Filter estimates of monthly GDP
#
# Philip Barrett, Washington DC
# First version: 25feb2025
#
##########################################################################################

do.big.gmm <- function( v.theta, df, v.x, v.y, l.q, meta, order, efficient=TRUE ){
# Does the big GMM estimation for theta
  
  m.X <-  make.moments.data(df, v.x, v.y, l.q, order )
    # Moments in the data
  W.hat <- if(efficient) make.asym.cov.theta( v.theta, df, v.x, v.y, l.q, meta, order, return.W=TRUE ) else 
    diag(ncol(m.X))
    # Make the big W.hat matrix
  gmm.obj <- function(this.theta){
    this.g <- make.g.mean.X(this.theta,  m.X, df, meta, order)
    out <- t(this.g) %*% W.hat %*% this.g
    return(c(out))
  }
  gmm.obj.deriv <- function(this.theta){
    this.g <- make.g.mean.X(this.theta,  m.X, df, meta, order)
    this.d.g <- make.d.g.sum.X(this.theta,  m.X, df, meta, order)
    out <- 2 * this.d.g %*% W.hat %*% this.g
    return(c(out))
  }
  
  message('')
  message('**** GRAND GMM ESTIMATION ****')
  message('  Use Nelder-Mead for initial guess')
  opt.nm <- optim( v.theta, gmm.obj, method='Nelder-Mead', 
                   control = list(trace = 2, REPORT = 40, maxit=2000, reltol=.001) )
  message('  Use BFGS for the final guess')
  opt <- optim( opt.nm$par, gmm.obj, method='BFGS', 
                control = list(trace = 1, REPORT = 1, maxit=200, reltol=.00001) )
  # The minimized GMM
  return(opt)
}

make.g.all <- function( v.theta, df, v.x, v.y, l.q, meta, order ){
  # Makes the g function for GMM for all the moments as a function of v.theta
  m.X <-  make.moments.data(df, v.x, v.y, l.q, order )
  return( make.g.all.X(v.theta, m.X, df, meta, order) )
}

make.g.all.X <- function( v.theta, m.X, df, meta, order, replace.na=FALSE ){
  # Makes the moments with the data part pre-computed.
  v.h <- make.moments.params( v.theta, meta, order )
  out <- m.X - matrix(1,nrow=nrow(m.X), ncol=1 ) %*% t(v.h)
  if(replace.na) out[is.na(out)] <- 0
  return(out)
}

make.g.mean.X <- function(v.theta, m.X, df, meta, order){
  return( colMeans(make.g.all.X(v.theta, m.X, df, meta, order), na.rm=T) )
}

make.d.g.sum.X <- function(v.theta, m.X, df, meta, order){
  # Makes the derivative of the mean of the moment conds w.r.t. v.theta
  
  # v.inc <- pmax( pmin( 1e-04 * v.theta, 1e-06 ), 1e-10 )
  v.inc <- pmax( pmin( 1e-05 * v.theta, 1e-05 ), 1e-07 )
  # g.init <- make.g.mean.X(v.theta, m.X, df, meta, order)
  # The baseline value
  n.theta <- v.theta %>% length()
  n.moments <- m.X %>% ncol
  
  m.out <- matrix(NA, n.theta, n.moments)
  for( i in 1:n.theta ){
    theta.upper <- v.theta ; theta.upper[i] <- v.theta[i] + v.inc[i]
    theta.lower <- v.theta ; theta.lower[i] <- v.theta[i] - v.inc[i]
    # The upper and lower values for theta
    g.upper <- make.g.mean.X(theta.upper, m.X, df, meta, order)
    g.lower <- make.g.mean.X(theta.lower, m.X, df, meta, order)
    d.g <- ( g.upper - g.lower ) / (2*v.inc[i])
    m.out[i,] <- d.g
  }
  return(m.out)
}

make.asym.cov.theta <- function( v.theta, df, v.x, v.y, l.q, meta, order, return.W=FALSE ){
  # Make the asymptotic covariance for the theta parameters
  
  m.X <- make.moments.data(df, v.x, v.y, l.q, order )
  m.G <- make.g.all.X( v.theta, m.X, df, meta, order )
  d.G <- make.d.g.sum.X(v.theta, m.X, df, meta, order)
  # browser()
  # S.hat <- make.S.hat(m.G)
  S.test <- S.test.init <- cov(m.G, use='complete.obs')
  # Need to use complete observations only to preserve PD status of S.hat
  s.rank <- qr(S.test)$rank
  counter <- 0
  alter <- 1e-14
  while( alter < 10e-05 & s.rank < ncol(S.test) & counter < 8 ){
    S.test <- S.test.init + alter * diag(ncol(S.test))
    alter <- 10*alter
    s.rank <- qr(S.test)$rank
    counter <- counter + 1
  }
  if(counter>0){
    message('      * SPD moment covariance matrix for theta enforced by altering the diagonal')
    message('            Adjustment=', alter/10)
  }
  S.hat <- S.test
  # Set S.hat with adjustment for numerical singularity
  if(return.W) return(solve(S.hat))
  # Return W.hat if needed
  n.G <- apply( m.G, 2, function(x) sum(!is.na(x)) ) %>% min()
  # The number of observations for the moment conditions
  m.out <- (solve( d.G %*% solve( S.hat / n.G, t(d.G) ) ) %>% make.sym())
  # The output
  return(m.out)
}

make.S.hat <- function( m.G ){
  # Makes S.hat, the moment covariance matrix
  
  # Set up the number of observations
  n.obs.max <- m.G %>% nrow
  n.moments <- m.G %>% ncol
  n.obs <- (!is.na(m.G)) %>% colSums()
  m.n.obs <- matrix(0, n.moments, n.moments )
  for( i in 1:n.moments ) for( j in 1:n.moments ) m.n.obs[i,j] <- min( n.obs[i], n.obs[j])
  
  # Create S.hat  
  S.hat <- matrix(0, n.moments, n.moments )
  for( i.obs in 1:n.obs.max ){
    W.inc <- (m.G[i.obs,] %*% t(m.G[i.obs,])) / m.n.obs
    W.inc[is.na(W.inc)] <- 0
    S.hat <- S.hat +  W.inc
  }
  
  return(S.hat)
}

make.moments.params <- function( v.theta, meta, order ){
  # Makes the parameter part of the moments  
  
  ## Extract useful things
  l.psi <- theta.to.psi.list( v.theta, meta )
  m.rho <- l.psi$rho %>%
    set_colnames(meta$v.states)
  m.Q <- l.psi$Q %>%
    set_rownames(meta$v.states) %>%
    set_colnames(meta$v.states)
  v.kappa <- l.psi$kappa
  m.R <- l.psi$R
  n.N <- meta$n.N
  n.P <- meta$n.P
  R.struct <- meta$R.struct
  v.x.q <- meta$v.x.q
  
  ## Make the autocovariances for the states
  v.logsigma2.rho <- rbind( log(diag(m.Q)), m.rho )
  v.state.acv <- apply(v.logsigma2.rho, 2, make.mu, order=order) %>% c
  
  ## Make the cross-state covariances
  n.sum <- 1000         # Number of MA terms to use for approximate sums
  m.state.cov <- matrix(0, n.N, n.N )
  for( i in 1:n.N ){
    for( j in 1:i ){
      this.ma.i <- arma_to_ma( m.rho[,i], max_i = n.sum )
      this.ma.j <- arma_to_ma( m.rho[,j], max_i = n.sum )
      m.state.cov[i,j] <- m.Q[i,j] / 9 * ( this.ma.i[1]*this.ma.j[1] + sum(this.ma.i[1:2])*sum(this.ma.j[1:2]) +
                                             sum( sapply(1:(n.sum-2), function(k) sum(this.ma.i[k+0:2])*sum(this.ma.j[k+0:2]) ) ) )
    }
  }
  v.state.cov <- m.state.cov[lower.tri(m.state.cov, diag = FALSE)] %>% c
  
  ## Make the observation equation slopes
  v.obs.slope <- rep(NA,n.P)
  for( i.q in 1:n.P ){
    this.ma.coef <- arma_to_ma(m.rho[,v.x.q[i.q]], max_i=2)
    v.obs.slope[i.q] <- 1 / 9 * sum(3:1 * this.ma.coef) * v.kappa[i.q] * m.Q[v.x.q[i.q],v.x.q[i.q]]
  }
  
  ## Make the observation equation covariances
  m.obs.cov <- matrix(0, n.P, n.P)
  for( i.q in 1:n.P ){
    for( j.q in 1:i.q ){
      if(v.x.q[i.q]==v.x.q[j.q]){
        m.obs.cov[i.q, j.q] <- m.obs.cov[j.q, i.q] <- v.kappa[i.q] * v.kappa[j.q] * 
          m.Q[v.x.q[i.q], v.x.q[j.q]] + m.R[j.q, i.q]
      }
    }
  }
  v.obs.cov <- m.obs.cov[lower.tri(m.obs.cov, diag = TRUE) & m.obs.cov != 0 ]
  
  ## Put it all together
  out <- c( v.state.acv, v.state.cov, v.obs.slope, v.obs.cov )
  return(out)
  
}

make.moments.data <- function(l.df, v.x, v.y, l.q, order ){
  # Makes the data part of the moments
  
  ## Extract useful things
  df.y.hat <- l.df$df.y.hat
  df.q.hat <- l.df$df.q.hat
  v.states <- c(v.x, v.y)
  n.obs <- df.y.hat %>% filter(variable=='gdp') %>% nrow()
  n.states <- v.states %>% length()
  n.q <- length(l.q)
  n.P <- sum(sapply(l.q, length))
  n.EE <- sum(sapply(l.q, function(x) .5 * length(x) * (1+length(x))))
  
  ## Set up the states data in a matrix
  m.y.in <- df.y.hat %>% select( date, t, variable, hat) %>%
    spread( variable, hat ) %>% 
    select( v.states ) %>%
    as.matrix
  # The data in deviations from trend
  m.y <- m.y.in - rep(1,n.obs) %*% t(colMeans(m.y.in, na.rm=TRUE))
  # Remove the mean
  
  ## Make the state autocovariances
  m.acv.states <- matrix(NA, n.obs, n.states * (1+order) )
  i.counter <- 0
  for( i.state in 1:n.states ){
    v.y <- m.y[, i.state]
    for( i.lag in 0:order){
      i.counter <- i.counter + 1
      m.acv.states[, i.counter] <- c( rep(NA, i.lag), v.y[(1+i.lag):n.obs] * v.y[1:(n.obs-i.lag)] )
    }
    # Fill in the autocovariance
  }
  
  ## The state cross-correlations 
  m.cov.states <- matrix(NA, n.obs, .5 * n.states * (n.states-1) )
  i.counter <- 0
  for( i.state in 1:(n.states-1) ){
    for( j.state in (i.state+1):n.states ){
      i.counter <- i.counter + 1
      m.cov.states[, i.counter] <- c( m.y[,i.state] * m.y[,j.state] )
    }
  }
  
  ## The full quarterly t-grid (n.obs rows, ascending) -- df.EY below gets this
  ## alignment for free via full_join, but the m.EE.cov loop needs it explicitly
  ## (see below): monthly indicator data can start later than the quarterly
  ## states (e.g. a public source with less history than the state's own start),
  ## so a raw filter(t %% 3 == 0) on monthly-derived data won't have exactly
  ## n.obs rows unless explicitly joined/padded onto this grid.
  v.t.q <- df.y.hat %>% filter(variable == 'gdp') %>% arrange(t) %>% pull(t)

  ## Set up the quarterly observation data
  df.E <- df.q.hat %>%
    group_by( variable ) %>%
    mutate( resid.Q=rollmean(hat.resid, 3, align = 'right', fill=NA ) ) %>%
    filter( t %% 3 == 0 ) %>% 
    select( date, t, variable, resid.Q ) %>%
    spread( variable, resid.Q )
  df.EY <- df.y.hat %>% select( date, t, variable, hat ) %>%
    spread(variable, hat ) %>%
    full_join( df.E, by=c('date', 't')) %>%
    arrange(t) %>%
    mutate_if( is.numeric, function(x) x-mean(x, na.rm=TRUE))
  
  ## Make the moments corresponding to the slopes of the observation equations
  m.EY.cov <- matrix(NA, n.obs, n.P)
  counter <- 0
  for( i.Y in 1:n.q){
    for( i.E in 1:length(l.q[[i.Y]]) ){
      counter <- counter + 1
      this.Y <- names(l.q)[i.Y]
      this.E <- l.q[[i.Y]][i.E]
      m.EY.cov[,counter] <- ( df.EY %>% pull(this.Y) ) * ( df.EY %>% pull(this.E) )
    }
  }
  
  ## Set up the monthly correlations
  m.EE.cov <- matrix( NA, n.obs, n.EE )
  counter <- 0
  for( i.Y in 1:n.q){
    for( i.E in 1:length(l.q[[i.Y]]) ){
      for( j.E in i.E:length(l.q[[i.Y]]) ){
        counter <-  counter + 1
        this.E.i <- l.q[[i.Y]][i.E]
        this.E.j <- l.q[[i.Y]][j.E]
        this.df.EE <- df.q.hat %>%
          filter( variable %in% c(this.E.i, this.E.j)) %>%
          select( date, t, variable, hat.resid ) %>%
          spread(variable, hat.resid ) %>%
          mutate( e.cov=get(this.E.i)*get(this.E.j) ) %>%
          mutate( e.cov.Q=rollmean(e.cov, 3, align = 'right', fill=NA ) )
        # The covariance of the monthly epsilons is the same as that from
        # adding up the covariances *within* each quarter and then averaging
        # over the quarters. Aligned onto the full quarterly t-grid (v.t.q) --
        # not just filter(t %% 3 == 0) -- since this.df.EE only has rows where
        # the underlying monthly indicator(s) actually have data, which can
        # start later than the quarterly states themselves; without this join,
        # positional assignment into m.EE.cov silently misaligns (or, if the
        # row count also happens to differ, errors outright).
        m.EE.cov[,counter] <- tibble(t = v.t.q) %>%
          left_join( this.df.EE %>% filter( t %% 3 == 0 ) %>% select(t, e.cov.Q), by = 't' ) %>%
          arrange(t) %>%
          pull(e.cov.Q)
      }
    }
  }
  
  out <- cbind( m.acv.states, m.cov.states, m.EY.cov, m.EE.cov )
  return(out)
}