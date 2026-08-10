##########################################################################################
#
# mly_gdp_gmm.R
#
# Functions for estimating the model for the monthly GDP project
#
# Philip Barrett, Washington DC
# First version: 02feb2025
#
##########################################################################################


make.mu <- function( v.logsigma2.rho, order ){
# Makes the quarterly autocorrelation from a vector of sigma and autoregressive 
# monthly coefficients
  sigma2 <- exp(v.logsigma2.rho[1]) ; v.rho <- v.logsigma2.rho[-1]
  # Use log sigma2 as the input to avoid scaling problems
  theta <- sigma2 * ARMA.autocov( 3*(1+order), v.rho )
  mu <- rep(NA, order+1)
  mu[1] <- 1/9 * ( 3*theta[1] + 4*theta[2] + 2*theta[3] )
  if(order>0){
    for(k in 1:order){
      mu[k+1] <- 1/9 * ( theta[3*k-1] + 2*theta[3*k] + 3*theta[3*k+1] + 
                           2*theta[3*k+2] + theta[3*k+3] )
    }
  }
  return(mu)
}

d.make.mu <- function( v.logsigma2.rho, order ){
# Computes the numerical derivative of make.mu
  
  p.inc <- 1e-04
  n.params <- v.logsigma2.rho %>% length()
  deriv <- matrix(NA, nrow=n.params, ncol=order+1)
  params.upper <- v.logsigma2.rho * (1+p.inc/2)
  params.lower <- v.logsigma2.rho * (1-p.inc/2)
    # The increments to use
  for(i in 1:n.params){
    v.lower <- v.upper <- v.logsigma2.rho
    v.upper[i] <- params.upper[i]
    v.lower[i] <- params.lower[i]
    deriv[i,] <- ( make.mu( v.upper, order) - make.mu(v.lower, order) ) / 
      (v.upper[i]-v.lower[i])
  }
  return(deriv)
}

make.acv.vcv <- function(v.logsigma2.rho, x, order){
# Makes the variance-covariance matrix for the autocorrelations
  
  n.x <- x %>% length()
  n.big.X <- n.x - order
  m.X <- matrix(NA, nrow=n.big.X, ncol=order+1 )
  for( i in 1:(order+1) ) m.X[,i] <- x[ 1:n.big.X + i - 1 ]
    # Make the matrix of lags
  m.mu <- matrix( 1, nrow=n.big.X) %*% t(make.mu(v.logsigma2.rho, order=order))
  acv <- cov(m.X)[,1]
    # The autocovariance (just a check)
  m.cov.obs <- sapply( 0:order, function(i) m.X[,1] * m.X[,1+i] )
  m.g <- m.cov.obs - m.mu
    # The errors on the moment conditions
  E.g <- m.g %>% colMeans()
    # The average errors on the moment conditions
  g.cov <- cov(m.g)
  # m.out <- matrix( 0, order+1, order+1 )
  # for(t in 1:n.big.X) m.out <- m.out + 1 / n.big.X * m.g[t,] %*% t(m.g[t,])
      # The covariance of the moments
  return(list( acv=acv, mu=m.mu[1,], avg.g=E.g, g=m.g, g.cov=g.cov))
}

state.eq.est <- function( x.names, y.names, l.df.hats, options, m.ar.in=NULL ){
# Estimates the state transition matrices using the quarterly data

  #### 1. Unpacking from the list of datasets ####
  ar.order <- options$ar.order
  ar.moments <- options$n.order
  max.ar.order <- options$max.ar.order
  max.ar.moments <- options$max.ar.moments
  ar.eff <- options$ar.eff
  df.x.hat <- l.df.hats$df.x.hat
  df.y.hat <- l.df.hats$df.y.hat
  n.x <- x.names %>% length()
  n.y <- y.names %>% length()
  v.small.number <- options$v.small.number
  
  if(is.null(m.ar.in)){
    #### 2. The known states ####
    
    ## 2.1 By GMM ##
    l.est.x.gmm <- lapply(x.names, function(this.var){
      x <- df.y.hat %>% filter(variable==this.var) %>% 
        pull(hat) %>% na.omit
      ar.gmm.mmsc.opt(x, max.ar = max.ar.order, max.moments = max.ar.moments, 
                      eff =ar.eff, ar.order = ar.order, ar.moments=ar.moments )
    } )
    n.ar.x <- sapply( l.est.x.gmm, function(x) x$est$n.ar )
    n.moments.ar.x <- sapply( l.est.x.gmm, function(x) x$est$n.moments )
    m.ar.est.x <- sapply( l.est.x.gmm, function(x) x$est$v.sigma2.rho )
    m.se.x <- sapply( l.est.x.gmm, function(x) x$est$se )
    n.obs.x <- lapply(x.names, function(this.var) df.y.hat %>% filter(variable==this.var) %>%
                        pull(hat) %>% na.omit %>% length() )
    
    ## 2.2 Direct estimation ##
    l.ar.x <- lapply( x.names,
                      function(this.var) ar( df.x.hat %>% filter(variable==this.var) %>% 
                                               pull(hat) %>% na.omit, 
                                             order=l.est.x.gmm[[this.var]]$est$n.ar, aic=FALSE ) )
    
    n.obs.x.ar <- lapply(x.names, function(this.var) df.x.hat %>% filter(variable==this.var) %>%
                           pull(hat) %>% na.omit %>% length() )
    l.ar.x.logsigma2.rho <- lapply( l.ar.x,
                                    function(x) c( log(x$resid %>% na.omit %>% var), x$ar ) )
    # Get the coefficients
    m.ar.sigma2.rho <- sapply( l.ar.x.logsigma2.rho, function(z) c( sigma2=exp(z[1]), rho=z[-1] ))
    # Assemble into a matrix
    m.ar.se.sigma2.rho <- rbind( m.ar.sigma2.rho[1,] * sqrt( 2 / (unlist(n.obs.x)-1-n.ar.x) ),
                                 sapply( l.ar.x, function(x) x$asy.var.coef %>% diag %>% sqrt ) )
    
    
    n.x <- x.names %>% length()
    df.acv.check.1 <- lapply( x.names, 
                              function(this.var) data.frame(h=0:(l.est.x.gmm[[this.var]]$est$n.moments-1), 
                                                            var=this.var, qly.acv=l.est.x.gmm[[this.var]]$est$v.cov.obs,
                                                            mly.agg.acv=l.est.x.gmm[[this.var]]$est$v.mu ) ) %>%
      rbindlist() %>%
      as_tibble %>%
      mutate( diff.eft= mly.agg.acv - qly.acv,
              pc.diff100 = 100 * ( mly.agg.acv / qly.acv - 1 ) )
    
    df.acv.check.2 <- lapply( x.names,
                              function(this.var) data.frame( variable=this.var, l.est.x.gmm[[this.var]]$df[1,] ) ) %>%
      rbindlist() %>%
      as_tibble()
      # as_tibble() added: newer data.table versions' print.data.table() has an
      # argument-matching conflict with n=Inf that the original code didn't hit --
      # diagnostic-print-only fix, matches the pattern already used for
      # df.acv.check.1/df.acv.check.3 in this same function.
    
    # Check the MM coefficients deliver the correct autocorrelations.  
    message('Quarterly/monthly DGP estimation consistency report' )
    message('  Known monthly data' )
    df.acv.check.1 %>% print(n=Inf)
    df.acv.check.2 %>% print(n=Inf)
    message('  Model selection criteria' )
    lapply( l.est.x.gmm, function(x) x$df %>% head) %>% print()
    
    #### 3. The unobserved states ####
    # browser()
    n.obs.y <- lapply(y.names, function(this.var) df.y.hat %>% filter(variable==this.var) %>%
                        pull(hat) %>% na.omit %>% length() )
    # Number of observations
    # browser()
    l.est.y.gmm <- lapply(y.names, function(this.var){
      # message(this.var)
      x <- df.y.hat %>% filter(variable==this.var) %>% 
        pull(hat) %>% na.omit
      ar.gmm.mmsc.opt(x, max.ar = max.ar.order, max.moments = max.ar.moments, 
                      eff=ar.eff, ar.order = ar.order, ar.moments=ar.moments )
    } )
    n.ar.y <- sapply( l.est.y.gmm, function(x) x$est$n.ar )
    n.moments.ar.y <- sapply( l.est.y.gmm, function(x) x$est$n.moments )
    m.ar.est.y <- sapply( l.est.y.gmm, function(x) x$est$v.sigma2.rho )
    m.se.y <- sapply( l.est.y.gmm, function(x) x$est$se )
    
    m.ar.est.all <- cbind( m.ar.est.x, m.ar.est.y )
    m.ar.se.all <- cbind( m.se.x, m.se.y )
    # The full set of estimates
    df.acv.check.3 <- 
      lapply( y.names, 
              function(this.var) data.frame(h=0:(l.est.y.gmm[[this.var]]$est$n.moments-1), 
                                            var=this.var, qly.acv=l.est.y.gmm[[this.var]]$est$v.cov.obs,
                                            mly.agg.acv=l.est.y.gmm[[this.var]]$est$v.mu ) ) %>%
      rbindlist() %>%
      as_tibble %>%
      mutate( diff.eft= mly.agg.acv - qly.acv,
              pc.diff100 = 100 * ( mly.agg.acv / qly.acv - 1 ) )
    
    message('  Unknown monthly data' )
    df.acv.check.3 %>% print(n=Inf)
    message('  Model selection criteria' )
    lapply( l.est.y.gmm, function(x) x$df %>% head) %>% print()
    
  }else{
    m.ar.est.all <- m.ar.in
  }
  
  #### 5. The covariances ####
  m.cov.Y <- df.y.hat %>% select(-value, -trend) %>% spread( variable, hat ) %>% 
    select( x.names, y.names ) %>% as.matrix %>% cov()
  # The covariance of the quarterly data
  m.Q.est <- matrix(0,ncol(m.cov.Y), ncol(m.cov.Y))
  n.sum <- 1000         # Number of MA terms to use for approximate sums
  for( i in 1:ncol(m.cov.Y) ){
    for( j in 1:ncol(m.cov.Y) ){
      this.ma.i <- arma_to_ma( m.ar.est.all[-1,i], max_i = n.sum )
      this.ma.j <- arma_to_ma( m.ar.est.all[-1,j], max_i = n.sum )
      m.Q.est[i,j] <- 9 / ( this.ma.i[1]*this.ma.j[1] + sum(this.ma.i[1:2])*sum(this.ma.j[1:2]) +
                              sum( sapply(1:(n.sum-2), function(k) sum(this.ma.i[k+0:2])*sum(this.ma.j[k+0:2]) ) ) ) * 
        m.cov.Y[i,j]
    }
  }
  m.Q.est <- m.Q.est %>% make.pd.eigen
    # Enforce positive-definiteness ()
  v.Q.err <- 100 * ( (m.Q.est %>% diag()) / m.ar.est.all[1,] - 1)
    # Check that the covariance is correct on the diagonal
  sig.Q.inv <- (m.Q.est %>% diag %>% sqrt %>% diag %>% rcond.iterate)$A %>% solve
  m.Q.cor <- sig.Q.inv %*% m.Q.est %*% sig.Q.inv
    # Make the matrix of correlations
  m.Q.alt <- diag(sqrt(m.ar.est.all[1,])) %*% m.Q.cor %*% diag(sqrt(m.ar.est.all[1,]))
    # Rescale to match the covariances
  m.Q.est <- m.Q.alt %>% set_colnames( colnames(m.ar.est.all) ) %>% 
    set_rownames( colnames(m.ar.est.all) )
  # diag(m.Q.est) <- m.ar.est.all[1,]
  
  #### 6. The output ####
  l.out <- list( ar.coef=m.ar.est.all, ar.se=m.ar.se.all, Q=m.Q.est, 
                 check=list( df.acv.check.1, df.acv.check.2, df.acv.check.3),
                 ar.coef.x=m.ar.sigma2.rho, ar.se.x=m.ar.se.sigma2.rho, 
                 est.gmm=c( l.est.x.gmm, l.est.y.gmm ),
                 nobs=c(n.obs.x.ar, n.obs.x, n.obs.y))
  
  return(l.out)
}

obs.eq.est <- function( y.names, q, l.df.hats, l.df.trends, options ){
  # Estimates the observation equations
  
  ## Extract things from lists
  df.q.hat <- l.df.hats$df.q.hat
  df.y.hat <- l.df.hats$df.y.hat
  df.states.bar <- l.df.trends$df.states.bar
  do.q.resid <- options$obs.resids
  
  
  ## Run the regressions ##
  l.lm.Q <- list()
  for( this.var in y.names ){
    this.q <- q[[this.var]] %>% set_names(.,.)
    df.Q <- df.q.hat %>% 
      filter( variable %in% this.q ) %>% 
      left_join( df.states.bar %>% select(date, t, y.bar=paste0(this.var, '.bar') ),
                 by=c('date', 't') ) %>%
      mutate( lhs = if(do.q.resid) y.bar * hat.resid else y.bar * value / trend,
              big.Q = rollsum(lhs, 3, align = 'right', fill=NA ) )
    df.reg.Q <- df.Q %>% select( date, t, variable, big.Q ) %>%
      right_join( df.y.hat %>% filter(variable==this.var) %>%
                    select( date, t, big.Y=value, big.Y.bar=trend ),
                  by=c('t','date') ) %>%
      mutate( lhs=big.Q/big.Y.bar, rhs=big.Y/big.Y.bar )
    l.lm.Q[[this.var]] <- lapply( this.q, function(q) lm( lhs ~ rhs, data=df.reg.Q %>% filter(variable==q), na.action=na.exclude ) )
  }
  
  ## Assemble into a dataframe ##
  l.coefs <- lapply( l.lm.Q, function(q) data.frame( t(sapply(q, coef ))) )
  df.Q.coefs <- lapply( names(l.coefs), function(state) l.coefs[[state]] %>% mutate( state=state ) %>% 
                          select( state, everything() ) %>%
                          set_names(c('state', 'kappa', 'beta')) ) %>%
    reduce(rbind) %>%
    rownames_to_column('q') %>%
    select( state, q, everything() )
  
  return( list(df.coefs=df.Q.coefs, lm=l.lm.Q ) )
}

obs.eq.est.resid <- function( x.names, y.names, q, l.df.hats, l.state, options ){
  # Estimates the observation equations
  
  #### 1. Extract things from lists ####
  v.q.names <- l.q %>% unlist %>% unname
  n.q <- v.q.names %>% length
  v.y.q.names <- names(q)[lapply( 1:length(q), function(i) rep( i, length(q[[i]])) ) %>% unlist]
  m.ar <- l.state$ar.coef
  m.Q <- l.state$Q %>% set_rownames(c(x.names, y.names)) %>% set_colnames(c(x.names, y.names))
  df.q.hat <- l.df.hats$df.q.hat
  df.y.hat <- l.df.hats$df.y.hat
  v.small.number <- options$v.small.number
  r2.max <- options$r2.max
  
  #### 2. Set up the quarterly data ####
  df.E <- df.q.hat %>%
    group_by( variable ) %>%
    mutate( resid.Q=rollmean(hat.resid, 3, align = 'right', fill=NA ) ) %>%
    filter( t %% 3 == 0 ) %>% 
    select( date, t, variable, resid.Q ) %>%
    spread( variable, resid.Q ) %>%
    select( date, t, v.q.names )
  # Select the quarterly residuals of the indicators
  df.EY <- df.y.hat %>% select( date, t, variable, hat ) %>%
    spread(variable, hat ) %>%
    full_join( df.E, by=c('date', 't'))
  sample.vcv <- sapply( 1:n.q, function(i) df.EY %>% select(v.y.q.names[i], v.q.names[i]) %>% 
                          as.matrix %>% na.omit %>% cov()  )
  sample.covar <- sample.vcv[2,]
  # The "true" cross-covariances (entry [1,2] in the covariance matrix)
  sample.cor.qly <- (sapply( 1:n.q, function(i) df.EY %>% select(v.y.q.names[i], v.q.names[i]) %>% 
                               as.matrix %>% na.omit %>% cor  ))[2,] %>%
    set_names( paste0(v.y.q.names, '_', v.q.names) )
  
  sample.vcv.nobs <- sapply( 1:n.q, function(i) df.EY %>% select(v.y.q.names[i], v.q.names[i]) %>% 
                          as.matrix %>% na.omit %>% nrow  )
  
  #### 3. Make the estimates ####
  v.kappa.out <- rep(NA,n.q) %>% set_names(v.q.names)
  m.sigma.obs.out <- matrix(0, n.q, n.q) %>% set_colnames(v.q.names) %>% 
    set_rownames(v.q.names)
  for( i.q in 1:n.q ){
    this.ma.coef <- arma_to_ma(m.ar[-1,v.y.q.names[i.q]], max_i=2)
    v.kappa.out[i.q] <- 9 / sum(3:1 * this.ma.coef) * sample.covar[i.q] / m.ar[1,v.y.q.names[i.q]]
  }
  # Make a data frame of the output
  
  vcv.eps <- (df.q.hat %>%
                select( date, t, variable, hat.resid ) %>%
                spread(variable, hat.resid) %>%
                select( v.q.names ) %>%
                as.matrix())[,v.q.names] %>%
    cov(use = 'pairwise.complete.obs')
  
  # browser()
  ### Dial down the kappa if the fit is too much
  v.kappa.init <- v.kappa.out
  eps.var <- vcv.eps %>% diag
  r2.obs <- v.kappa.out^2 * m.ar[1,v.y.q.names]/ pmax(eps.var,1e-12)
  # Compute the R2.  Add a tiny number to prevent NaNs
  kappa.mod <- r2.obs > r2.max
  # See if any R2 are too big
  kappa.alt <- sqrt(r2.max/r2.obs) * v.kappa.out
  # Create the upper bound on kappa corresponding to the maximum psuedo-R2
  v.kappa.out[kappa.mod] <- kappa.alt[kappa.mod]
  r2.obs <- v.kappa.out^2 * m.ar[1,v.y.q.names]/eps.var
    # Replace & recompute R2
  df.coefs <- data.frame( state=v.y.q.names, q=v.q.names, kappa=v.kappa.out, 
                          N=sample.vcv.nobs, sample.cor.qly=sample.cor.qly, R2=r2.obs )
  
  ## The cross-equation covariances ##
  for( i.q in 1:n.q ){
    for( j.q in 1:i.q ){
      if(v.y.q.names[i.q]==v.y.q.names[j.q]){
        m.sigma.obs.out[i.q, j.q] <- m.sigma.obs.out[j.q, i.q] <- 
          vcv.eps[i.q,j.q] - v.kappa.out[i.q] * v.kappa.out[j.q] * 
          m.Q[v.y.q.names[i.q], v.y.q.names[j.q]]
      }
    }
  }
  # diag(m.sigma.obs.out)[diag(m.sigma.obs.out)<v.small.number] <- v.small.number
  #   # Replaces the zero errors resulting from having a constant indicator
  
  ## Guarantee positive-definiteness of the covariance matrix
  # browser()
  m.sigma.init <- m.sigma.obs.out
  eigen_decomp <- eigen(m.sigma.obs.out)
  eigenvalues <- eigen_decomp$values
  eigenvalues[eigenvalues <= v.small.number] <- v.small.number
  m.sigma.obs.out <- eigen_decomp$vectors %*% diag(eigenvalues) %*% t(eigen_decomp$vectors)
  if( min(eigen_decomp$values) < v.small.number ){
    message('     Negative eigenvalue in R matrix detected. ', sum(eigen_decomp$values<v.small.number),
            ' eigenvalues replaced with max size ', sprintf('%e',  min(eigen_decomp$values) ) )
    message( '         Max difference in R values = ', round(max(abs(m.sigma.init - m.sigma.obs.out)), 3) )
    message( '         Max pc difference in R values = ', 
             round(100*max(abs(m.sigma.init[m.sigma.init!=0] - m.sigma.obs.out[m.sigma.obs.out!=0])/
                             m.sigma.init[m.sigma.init!=0]), 2) )
  }
  
  # iter <- 1
  # max.it <- 10000
  # tol <- 1e-05
  # min.eigen <- min(eigen(m.sigma.obs.out)$values)
  # inc <- .5 * min(abs(diag(m.sigma.obs.out)))
  # while( iter<max.it & min.eigen < tol ){
  #   m.sigma.obs.out.new <- inc * diag(i.q) + m.sigma.obs.out
  #   min.eigen <- min(eigen(m.sigma.obs.out.new)$values)
  #   iter <- iter + 1
  #   m.sigma.obs.out <- m.sigma.obs.out.new
  # }
  
  obs.err.log10 <- log( abs( eps.var / (v.kappa.out^2 * m.ar[1,v.y.q.names] + 
                                          diag(m.sigma.obs.out)) - 1 ), 10 )
  # Should be zero if the implied covariance of the errors and the state is
  # zero.
  
  message(' * Observation equation summary statistics *')
  data.frame( q.resid=v.q.names, y.resid=v.y.q.names, 
              kappa.init=v.kappa.init, kappa=v.kappa.out,
              sigma2.init=diag(m.sigma.init),
              sigma2=diag(m.sigma.obs.out),
              obs.err.log10=obs.err.log10,
              kappa.mod=kappa.mod, sample.cor.qly, r2.obs ) %>% 
    as_tibble() %>% print(n=Inf)
  
  return( list( kappa=v.kappa.out, R=m.sigma.obs.out, df.coefs=df.coefs, 
                r2=r2.obs, err.log10=obs.err.log10, sample.cor.qly=sample.cor.qly ) )
}

obs.exact.resid <- function(x.names, l.df.hats, l.state, options ){
  # Checks the observation equation formulas on the 
  
  #### 1. Extract things from lists ####
  m.ar <- l.state$ar.coef
  ar.resids <- options$ar.resids
  ar.order <- options$ar.order
  obs.match.ar <- options$obs.match.ar
  df.x.hat <- l.df.hats$df.x.hat
  df.y.hat <- l.df.hats$df.y.hat
  n.x <- x.names %>% length()
  m.Q <- l.state$Q[1:n.x,1:n.x] %>% 
    as.matrix() %>%
    set_rownames(x.names) %>% 
    set_colnames(x.names)
  rel.noise <- .1
  r2.max <- .8
  
  #### 2. Make the data residuals ####
  max.p <- if(obs.match.ar) ar.order else 20
  l.arima.x <- lapply( x.names, function(this.x) df.x.hat %>% filter( variable==this.x ) %>% pull(hat) %>%
                         auto.arima(d=0, D=0, max.p=max.p, max.q= if(ar.resids) 0 else 3 ) )
  df.x.hat.resid <- 
    lapply( x.names, function(this.x) df.x.hat %>% filter( variable==this.x ) %>% 
              mutate( hat.resid=(l.arima.x[[this.x]] %>% resid %>% c)*
                        rnorm(mean = 1,sd = rel.noise,n = n()) ) ) %>%
    rbindlist
  
  #### 3. Make the state residuals ####
  make.ar.errs <- function(y, v.rho){
    order <- v.rho %>% length()
    n.y <- y %>% length()
    m.Y <- matrix(NA, nrow=n.y-order, ncol=1+order)
    for(i in 0:order) m.Y[,1+i] <- y[i+1:(n.y-order)]
    out <- c( rep(NA,order), m.Y %*% c( -v.rho, 1 ) )
    return(out)
  }
  
  df.x.hat <- df.x.hat %>%
    group_by(variable) %>%
    mutate( hat.resid=make.ar.errs(hat, m.ar[-1,variable[1]]) )
  df.lm <- df.x.hat.resid %>% 
    select( date, t, variable, q=hat.resid ) %>% 
    left_join( df.x.hat, by=c('t','date','variable') )
  l.kappa.lm <- lapply( x.names, function(this.x) 
    lm( q~0+hat.resid, data=df.lm %>% filter(variable==this.x)) )
  
  #### 3. Set up the quarterly data
  df.E <- df.x.hat.resid %>%
    group_by( variable ) %>%
    mutate( resid.X=rollmean(hat.resid, 3, align = 'right', fill=NA ) ) %>%
    filter( t %% 3 == 0 ) %>% 
    mutate(variable=paste0(variable, '_resid_sum') ) %>%
    select( date, t, variable, resid.X ) %>%
    spread( variable, resid.X )
  df.EY <- df.y.hat %>% select( date, t, variable, hat ) %>%
    spread(variable, hat ) %>%
    full_join( df.E, by=c('date', 't'))
  sample.vcv <- sapply( 1:n.x, function(i) df.EY %>% select(paste0(x.names[i], '_resid_sum'), x.names[i]) %>% 
                          as.matrix %>% na.omit %>% cov()  )
  sample.covar <- sample.vcv[2,]
  # The "true" cross-covariances (entry [1,2] in the covariance matrix)
  sample.cor.qly <- (sapply( 1:n.x, function(i) df.EY %>% 
                               select(paste0(x.names[i], '_resid_sum'), x.names[i]) %>% 
                               as.matrix %>% na.omit %>% cor  ))[2,]
  
  #### 4. Compute the kappa values ####
  v.kappa.out <- rep(NA,n.x) %>% set_names(x.names)
  m.sigma.obs.out <- matrix(0, n.x, n.x) %>% set_colnames(x.names) %>% 
    set_rownames(x.names)
  for( i.x in 1:n.x ){
    this.ma.coef <- arma_to_ma(m.ar[-1,x.names[i.x]], max_i=2)
    v.kappa.out[i.x] <- 9 / sum(3:1 * this.ma.coef) * 
      sample.covar[i.x] / m.ar[1,x.names[i.x]]
  }
  
  vcv.eps <- (df.x.hat %>%
                select( date, t, variable, hat.resid ) %>%
                spread(variable, hat.resid) %>%
                select( x.names ) %>%
                as.matrix())[,x.names] %>%
    cov(use = 'pairwise.complete.obs')
  
  v.kappa.init <- v.kappa.out
  eps.var <- vcv.eps %>% diag
  r2.obs <- v.kappa.out^2 * m.ar[1,1:n.x]/eps.var
  # Compute the R2
  kappa.mod <- r2.obs > r2.max
  # See if any R2 are too big
  kappa.alt <- sqrt(r2.max/r2.obs) * v.kappa.out
  # Create the upper bound on kappa corresponding
  v.kappa.out[kappa.mod] <- kappa.alt[kappa.mod]
  r2.obs <- v.kappa.out^2 * m.ar[1,1:n.x]/eps.var
  # Replace & recompute R2
  
  #### 5. Compute the cross-equation covariances ####
  for( i.x in 1:n.x ){
    for( j.x in 1:i.x ){
      m.sigma.obs.out[i.x, j.x] <- m.sigma.obs.out[j.x, i.x] <- 
        vcv.eps[i.x,j.x] - v.kappa.out[i.x] * v.kappa.out[j.x] * 
        m.Q[x.names[i.x], x.names[j.x]]
    }
  }
  obs.err <- eps.var / (v.kappa.out^2 * m.ar[1,x.names] + 
                          diag(m.sigma.obs.out)) - 1
  
  df.coefs <- data.frame( state=x.names, q=x.names, kappa.est=v.kappa.out,
                          kappa.true=sapply(l.kappa.lm, function(x) x$coef['hat.resid'] ),
                          kappa.init=v.kappa.init, sd.u.est=diag(m.sigma.obs.out) %>% sqrt,
                          sd.u.true=sapply(l.kappa.lm, function(x) x %>% vcov %>% diag %>% sqrt ),
                          r2=r2.obs, kappa.mod=kappa.mod )
  # Make a data frame of the output
  
  message('*** Checking estimated observation equation on the known state ***')
  df.coefs %>% print()
  
  return( list( kappa=v.kappa.out, R=m.sigma.obs.out, df.coefs=df.coefs, 
                r2=r2.obs, err=obs.err ) )
  
}

obs.eq.est.resid.alt <- function( x.names, y.names, q, l.df.hats, l.state, options ){
  # Estimates the observation equations
  
  #### 1. Extract things from lists ####
  v.q.names <- l.q %>% unlist %>% unname
  n.q <- v.q.names %>% length
  v.y.q.names <- y.names[lapply( 1:length(q), function(i) rep( i, length(q[[i]])) ) %>% unlist]
  m.ar <- l.state$ar.coef
  m.Q <- l.state$Q %>% set_rownames(c(x.names, y.names)) %>% set_colnames(c(x.names, y.names))
  df.q.hat <- l.df.hats$df.q.hat
  df.y.hat <- l.df.hats$df.y.hat
  q.arima <- l.df.hats$q.arima
  v.small.number <- options$v.small.number
  r2.max <- options$r2.max
  
  #### 3. Make the point estimates ####
  v.kappa.out <- rep(NA,n.q) %>% set_names(v.q.names)
  v.sample.cov <- rep(NA,n.q) %>% set_names(v.q.names)
  n.sum <- 1000         # Number of MA terms to use for approximate sums
  for( i.q in 1:n.q ){
    this.q <- v.q.names[i.q]
    this.x <- v.y.q.names[i.q]
    # Variable names
    q.coef <- q.arima[[this.q]]$coef
    q.ar.coef <- q.coef[grepl('ar',names(q.coef))]
    q.ma.coef <- q.coef[grepl('ma',names(q.coef))]
    q.var <- q.arima[[this.q]]$sigma2
    # The time series coefficients for the Q process
    this.sigma <- m.ar[1,this.x]
    this.a <- arma_to_ma( ar=m.ar[-1,this.x], max_i=n.sum )
    this.b <- arma_to_ma( ar=q.ar.coef, ma=q.ma.coef, max_i=n.sum )
    rhs.coef.sum <- this.a[1] * this.b[1] + sum(this.a[1:2])*sum(this.b[1:2]) +
      sum( rollsum( this.a, 3 ) * rollsum( this.b, 3 ) )
    # The RHS sum coefficients
    df.q.qly <- df.q.hat %>% 
      filter( variable==this.q) %>% 
      group_by(variable) %>% 
      mutate( t=3*ceiling(t/3)) %>% 
      group_by(t) %>% 
      summarise_if(is.numeric, mean, na.rm=T) %>%
      select( t, q.hat.resid=hat.resid, q.hat=hat ) %>%
      right_join( df.y.hat %>% filter(variable==this.x) %>% 
                    rename(y.hat=hat), by='t' )
    this.cov <- with( df.q.qly, 
                      cov(y.hat, q.hat, use = 'pairwise.complete.obs') )
    # The data moment
    v.sample.cov[i.q] <- this.cov
    v.kappa.out[i.q] <- this.cov * 9 / (this.sigma * rhs.coef.sum)
    # The estiamte of kappa
  }
  df.coefs <- data.frame( state=v.y.q.names, q=v.q.names, kappa=v.kappa.out )
  # Make a data frame of the output
  
  vcv.eps <- (df.q.hat %>%
                select( date, t, variable, hat.resid ) %>%
                spread(variable, hat.resid) %>%
                select( v.q.names ) %>%
                as.matrix())[,v.q.names] %>%
    cov(use = 'pairwise.complete.obs')
  
  # browser()
  ### Dial down the kappa if the fit is too much
  v.kappa.init <- v.kappa.out
  eps.var <- vcv.eps %>% diag
  r2.obs <- v.kappa.out^2 * m.ar[1,v.y.q.names]/ pmax(eps.var,1e-12)
  # Compute the R2.  Add a tiny number to prevent NaNs
  kappa.mod <- r2.obs > r2.max
  # See if any R2 are too big
  kappa.alt <- sqrt(r2.max/r2.obs) * v.kappa.out
  # Create the upper bound on kappa corresponding
  v.kappa.out[kappa.mod] <- kappa.alt[kappa.mod]
  r2.obs <- v.kappa.out^2 * m.ar[1,v.y.q.names]/eps.var
  # Replace & recompute R2
  
  ## The cross-equation covariances ##
  m.sigma.obs.out <- matrix(0, n.q, n.q) %>% set_colnames(v.q.names) %>% 
    set_rownames(v.q.names)
  for( i.q in 1:n.q ){
    for( j.q in 1:i.q ){
      if(v.y.q.names[i.q]==v.y.q.names[j.q]){
        m.sigma.obs.out[i.q, j.q] <- m.sigma.obs.out[j.q, i.q] <- 
          vcv.eps[i.q,j.q] - v.kappa.out[i.q] * v.kappa.out[j.q] * 
          m.Q[v.y.q.names[i.q], v.y.q.names[j.q]]
      }
    }
  }
  diag(m.sigma.obs.out) <- diag(vcv.eps) - v.kappa.out^2 * diag(m.Q)[v.y.q.names]
  # Unnecessary line. Keeping it in for good luck
  diag(m.sigma.obs.out)[diag(m.sigma.obs.out)<v.small.number] <- v.small.number
  # Replaces the zero errors resulting from having a constant indicator
  
  ## Guarantee positive-definiteness of the covariance matrix
  # browser()
  m.sigma.init <- m.sigma.obs.out
  eigen_decomp <- eigen(m.sigma.obs.out)
  eigenvalues <- eigen_decomp$values
  eigenvalues[eigenvalues <= v.small.number] <- v.small.number
  m.sigma.obs.out <- eigen_decomp$vectors %*% diag(eigenvalues) %*% t(eigen_decomp$vectors)
  if( min(eigen_decomp$values) < v.small.number ){
    message('     Negative eigenvalue in R matrix detected. ', sum(eigen_decomp$values<v.small.number),
            ' eigenvalues replaced with max size ', sprintf('%e',  min(eigen_decomp$values) ) )
    message( '         Max difference in R values = ', round(max(abs(m.sigma.init - m.sigma.obs.out)), 3) )
    message( '         Max pc difference in R values = ', 
             round(100*max(abs(m.sigma.init[m.sigma.init!=0] - m.sigma.obs.out[m.sigma.obs.out!=0])/
                             m.sigma.init[m.sigma.init!=0]), 2) )
  }
  
  # iter <- 1
  # max.it <- 10000
  # tol <- 1e-05
  # min.eigen <- min(eigen(m.sigma.init)$values)
  # inc <- .5 * abs(diag(m.sigma.init)[kappa.mod])
  # while( iter<max.it & min.eigen < tol ){
  #   m.sigma.obs.out.new <- m.sigma.obs.out
  #   diag(m.sigma.obs.out.new[kappa.mod,kappa.mod]) <- diag(m.sigma.obs.out.new[kappa.mod,kappa.mod]) + inc
  #   min.eigen <- min(eigen(m.sigma.obs.out.new)$values)
  #   iter <- iter + 1
  #   m.sigma.obs.out <- m.sigma.obs.out.new
  # }
  
  obs.err <- abs( eps.var / (v.kappa.out^2 * m.ar[1,v.y.q.names] + 
                               diag(m.sigma.obs.out)) - 1 )
  # Should be zero since we use that the implied covariance of the errors and
  # the state is zero.
  
  message(' * Observation equation summary statistics *')
  data.frame( q.resid=v.q.names, y.resid=v.y.q.names, 
              kappa.init=v.kappa.init, kappa=v.kappa.out,
              sigma2.init=diag(m.sigma.init),
              sigma2=diag(m.sigma.obs.out),
              obs.err=obs.err,
              kappa.mod=kappa.mod, sample.cov=v.sample.cov, r2.obs ) %>% 
    as_tibble() %>% print(n=Inf)
  
  return( list( kappa=v.kappa.out, R=m.sigma.obs.out, df.coefs=df.coefs, 
                r2=r2.obs, err=obs.err ) )
}

obs.exact.resid <- function(x.names, l.df.hats, l.state, options ){
  # Checks the observation equation formulas on the 
  
  #### 1. Extract things from lists ####
  m.ar <- l.state$ar.coef
  ar.resids <- options$ar.resids
  ar.order <- options$ar.order
  obs.match.ar <- options$obs.match.ar
  df.x.hat <- l.df.hats$df.x.hat
  df.y.hat <- l.df.hats$df.y.hat
  n.x <- x.names %>% length()
  m.Q <- l.state$Q[1:n.x,1:n.x] %>% 
    as.matrix() %>%
    set_rownames(x.names) %>% 
    set_colnames(x.names)
  rel.noise <- .1
  r2.max <- options$r2.max
  
  #### 2. Make the data residuals ####
  max.p <- if(obs.match.ar) ar.order else 20
  l.arima.x <- lapply( x.names, function(this.x) df.x.hat %>% filter( variable==this.x ) %>% pull(hat) %>%
                         auto.arima(d=0, D=0, max.p=max.p, max.q= if(ar.resids) 0 else 3 ) )
  df.x.hat.resid <- 
    lapply( x.names, function(this.x) df.x.hat %>% filter( variable==this.x ) %>% 
              mutate( hat.resid=(l.arima.x[[this.x]] %>% resid %>% c)*
                        rnorm(mean = 1,sd = rel.noise,n = n()) ) ) %>%
    rbindlist
  
  #### 3. Make the state residuals ####
  make.ar.errs <- function(y, v.rho){
    order <- v.rho %>% length()
    n.y <- y %>% length()
    m.Y <- matrix(NA, nrow=n.y-order, ncol=1+order)
    for(i in 0:order) m.Y[,1+i] <- y[i+1:(n.y-order)]
    out <- c( rep(NA,order), m.Y %*% c( -v.rho, 1 ) )
    return(out)
  }
  
  df.x.hat <- df.x.hat %>%
    group_by(variable) %>%
    mutate( hat.resid=make.ar.errs(hat, m.ar[-1,variable[1]]) )
  df.lm <- df.x.hat.resid %>% 
    select( date, t, variable, q=hat.resid ) %>% 
    left_join( df.x.hat, by=c('t','date','variable') )
  l.kappa.lm <- lapply( x.names, function(this.x) 
    lm( q~0+hat.resid, data=df.lm %>% filter(variable==this.x)) )
  
  #### 3. Set up the quarterly data
  df.E <- df.x.hat.resid %>%
    group_by( variable ) %>%
    mutate( resid.X=rollmean(hat.resid, 3, align = 'right', fill=NA ) ) %>%
    filter( t %% 3 == 0 ) %>% 
    mutate(variable=paste0(variable, '_resid_sum') ) %>%
    select( date, t, variable, resid.X ) %>%
    spread( variable, resid.X )
  df.EY <- df.y.hat %>% select( date, t, variable, hat ) %>%
    spread(variable, hat ) %>%
    full_join( df.E, by=c('date', 't'))
  sample.vcv <- sapply( 1:n.x, function(i) df.EY %>% select(paste0(x.names[i], '_resid_sum'), x.names[i]) %>% 
                          as.matrix %>% na.omit %>% cov()  )
  sample.covar <- sample.vcv[2,]
  # The "true" cross-covariances (entry [1,2] in the covariance matrix)
  sample.cor.qly <- (sapply( 1:n.x, function(i) df.EY %>% 
                               select(paste0(x.names[i], '_resid_sum'), x.names[i]) %>% 
                               as.matrix %>% na.omit %>% cor  ))[2,]
  
  #### 4. Compute the kappa values ####
  v.kappa.out <- rep(NA,n.x) %>% set_names(x.names)
  m.sigma.obs.out <- matrix(0, n.x, n.x) %>% set_colnames(x.names) %>% 
    set_rownames(x.names)
  for( i.x in 1:n.x ){
    this.ma.coef <- arma_to_ma(m.ar[-1,x.names[i.x]], max_i=2)
    v.kappa.out[i.x] <- 9 / sum(3:1 * this.ma.coef) * 
      sample.covar[i.x] / m.ar[1,x.names[i.x]]
  }
  
  vcv.eps <- (df.x.hat %>%
                select( date, t, variable, hat.resid ) %>%
                spread(variable, hat.resid) %>%
                select( x.names ) %>%
                as.matrix())[,x.names] %>%
    cov(use = 'pairwise.complete.obs')
  
  v.kappa.init <- v.kappa.out
  eps.var <- vcv.eps %>% diag
  r2.obs <- v.kappa.out^2 * m.ar[1,1:n.x]/eps.var
  # Compute the R2
  kappa.mod <- r2.obs > r2.max
  # See if any R2 are too big
  kappa.alt <- sqrt(r2.max/r2.obs) * v.kappa.out
  # Create the upper bound on kappa corresponding
  v.kappa.out[kappa.mod] <- kappa.alt[kappa.mod]
  r2.obs <- v.kappa.out^2 * m.ar[1,1:n.x]/eps.var
  # Replace & recompute R2
  
  #### 5. Compute the cross-equation covariances ####
  for( i.x in 1:n.x ){
    for( j.x in 1:i.x ){
      m.sigma.obs.out[i.x, j.x] <- m.sigma.obs.out[j.x, i.x] <- 
        vcv.eps[i.x,j.x] - v.kappa.out[i.x] * v.kappa.out[j.x] * 
        m.Q[x.names[i.x], x.names[j.x]]
    }
  }
  obs.err <- eps.var / (v.kappa.out^2 * m.ar[1,x.names] + 
                          diag(m.sigma.obs.out)) - 1
  
  df.coefs <- data.frame( state=x.names, q=x.names, kappa.est=v.kappa.out,
                          kappa.true=sapply(l.kappa.lm, function(x) x$coef['hat.resid'] ),
                          kappa.init=v.kappa.init, sd.u.est=diag(m.sigma.obs.out) %>% sqrt,
                          sd.u.true=sapply(l.kappa.lm, function(x) x %>% vcov %>% diag %>% sqrt ),
                          r2=r2.obs, kappa.mod=kappa.mod )
  # Make a data frame of the output
  
  message('*** Checking estimated observation equation on the known state ***')
  df.coefs %>% print()
  
  return( list( kappa=v.kappa.out, R=m.sigma.obs.out, df.coefs=df.coefs, 
                r2=r2.obs, err=obs.err ) )
  
}