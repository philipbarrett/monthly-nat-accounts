##########################################################################################
#
# mly_gdp_model_selection.R
#
# Functions for model selection for monthly GDP 
#
# Philip Barrett, Washington DC
# First version: 25feb2025
#
##########################################################################################

rcond.iterate <- function(A, epsilon=1e-12, rcond.threshold=1e-12, maxit=100 ){
# Modifies A until the reciprocal condition number is above the threshold
  A.old <- A.new <- A
  rcond.A <- rcond(A.new)
  iter <- 0
  n.A <- nrow(A)
  
  while(iter < maxit & rcond.A < rcond.threshold ){
    A.new <- A.old + diag(n.A) * epsilon
    iter <- iter+1
    rcond.A <- rcond(A.new)
    epsilon <- epsilon*10
    A.old <- A.new
  }
  return(list(A=A.new, iter=iter, epsilon=epsilon/10, rcond=rcond.A, n=n.A) )
}

make.pd.eigen <- function(mat, inc=1e-08){
# Uses the eigenvector decomposition to enforce positive-definiteness
  MM <- mat %>% eigen
  if(all(MM$values>0)) return(mat)
  warning('ENFORCING POSITIVE-DEFINITENESS')
  return(MM$vectors %*% diag(pmax( MM$values, inc ) ) %*% t(MM$vectors))
}

ar.gmm.mmsc <- function( x, n.ar, n.moments, v.sigma2.rho=NULL, 
                         m.W=NULL, eff=FALSE ){
# Computes the GMM estimates of the AR process using the quarterly data
  
  # Set up
  # n.params <- length(v.sigma2.rho)
  n.x <- x %>% length()
    # Dimensions
  
  # Make the weighting matrix
  if(is.null(m.W)) m.W <- diag(n.moments)
  if(eff){
    est.init <- ar.gmm.mmsc( x, n.ar, n.moments, v.sigma2.rho, m.W=NULL, eff=FALSE )
    m.W <- solve(est.init$g.cov)
    v.sigma2.rho <- est.init$v.sigma2.rho
  }
  
  # Make the moments and moment errors
  n.big.X <- n.x - n.moments
  m.X <- matrix(NA, nrow=n.big.X, ncol=n.moments )
  for( i in 1:(n.moments) ) m.X[,i] <- 
    x[ 1:n.big.X + i - 1 ] - mean(x[ 1:n.big.X + i - 1 ])
      # Make the matrix of lags
  m.cov.obs <- sapply( 0:(n.moments-1), function(i) m.X[,1] * m.X[,1+i] )
  v.cov.obs <- m.cov.obs %>% colMeans()
    # NB: So the moments are scaled by n
  
  # Set the initial value
  lr.root <- mean((v.cov.obs[-1] / v.cov.obs[-n.moments])^(1/3), na.rm=T)
    # The long-run root
  if(is.null(v.sigma2.rho)){
    v.sigma2.rho <- if(n.ar==0) 3*var(x) else c( 3*var(x)*(1-lr.root^2), lr.root, rep(0,n.ar-1) )
  } 
  v.logsigma2.rho <- c( log(v.sigma2.rho[1]), v.sigma2.rho[-1] ) 
    # Manipulate the initial values
  
  # Make the objective function and minimize
  # browser()
  obj.fn <- function(theta){
    this.mu <- make.mu(theta, order=n.moments-1)
    moment.conds <- t(this.mu - v.cov.obs) %*% 
      m.W %*% (this.mu - v.cov.obs) %>% c
    this.root <- 1/polyroot( c(1,-theta[-1]) )
    max.root <- max(abs(this.root))
    # ar.1 <- if(length(theta)>1) theta[2] else 0.1
    penalty <- 100 * exp(10000*(max.root-1)) / ( 1 + exp(10000*(max.root-1)))
      # 100 * exp(1000*(-ar.1)) / ( 1 + exp(1000*(-ar.1)))
    return(moment.conds) #+penalty)
  }
  est <- nlm( obj.fn, v.logsigma2.rho, iterlim=1000, steptol=1e-12,  gradtol=1e-18 )
    # The GMM estimate
  v.logsigma2.rho.gmm <- est$estimate
  v.sigma2.rho.gmm <- c( exp(est$estimate[1]), est$estimate[-1] )
    # The estimate
  opt.roots <- polyroot((c(1, -est$estimate[-1])))
  max.root <- 1/max(abs(opt.roots))
  # penalty <- 0
  penalty <- 100 * exp(10000*(max.root-1)) / ( 1 + exp(10000*(max.root-1)))
    # Compute the penalty function for non-invertibility at the optimum
  J.stat <- n.x * (est$minimum - penalty)
    # The J-statistic
  mmsc.bic <- J.stat - (n.moments - (1+n.ar)) * log(n.x)
    # The selection criterion
  
  # Return useful outputs
  v.mu <- make.mu(v.logsigma2.rho.gmm, order=n.moments-1)
  m.mu <- matrix( 1, nrow=n.big.X) %*% t(v.mu)
  m.g <- m.cov.obs - m.mu
      # The errors on the moment conditions
  E.g <- m.g %>% colMeans()
      # The average errors on the moment conditions
  g.cov <- cov(m.g)
  
  ## Make the covariance
  g.deriv <- -d.make.mu(v.logsigma2.rho.gmm, n.moments-1)
  # if(rcond(g.cov<1e-12)) g.cov <- g.cov + 1e-08 * diag(nrow(n.moments))
  if(eff){
    to.invert <- g.deriv %*% solve( g.cov, t(g.deriv) )
    if(rcond(to.invert) < 1e-12 ) to.invert <- rcond.iterate(to.invert)$A
    m.logsigma2.rho.cov <- 1/n.x * solve( to.invert )
  }else{
    to.invert <- g.deriv %*% m.W %*% t(g.deriv)
    if(rcond(to.invert) < 1e-12 ) to.invert <- rcond.iterate(to.invert)$A
    m.logsigma2.rho.cov <- 1/n.x * solve( to.invert ) %*%
      g.deriv %*% m.W %*% g.cov %*% m.W %*% t(g.deriv) %*% solve( to.invert )
  }
  mult.mat <- if(n.ar==0) v.sigma2.rho.gmm[1] else diag( c( v.sigma2.rho.gmm[1], rep(1,n.ar) ) )
  m.sigma2.rho.cov <- mult.mat %*% m.logsigma2.rho.cov %*% mult.mat
      # The covariance of the errors
  v.se <- m.sigma2.rho.cov %>% diag %>% sqrt
  
  return( list( v.sigma2.rho=v.sigma2.rho.gmm, J.stat=J.stat, 
                sigma2.rho.cov=m.sigma2.rho.cov, se=v.se,
                mmsc.bic=mmsc.bic, E.g=E.g, g.cov=g.cov,
                max.root=max.root, penalty=penalty,
                v.cov.obs=v.cov.obs, v.mu=v.mu, W=m.W,
                n.ar=n.ar, n.moments=n.moments, est=est ) )
}



ar.gmm.mmsc.opt <- function( x, max.ar, max.moments, ar.order=NULL, ar.moments=NULL,
                             v.sigma2.rho=NULL, m.W=NULL, eff=FALSE ){
# Selects the optimal model and moment conditions for the AR GMM estimation
  
  l.test.0 <- list()
  l.test.0[[1]] <- ar.gmm.mmsc(x, 0, 1,v.sigma2.rho, m.W, eff )
    # Make the test for AR(0)
  v.ar <- v.moments <- c()
  counter <- 0
  for( i.ar in 1:max.ar ){
    for(i.moments in (i.ar+1):max.moments){
      counter <- counter + 1
      v.ar[counter] <- i.ar
      v.moments[counter] <- i.moments
    }
  }
    # The AR order and number of moments to test over
  l.test.1 <- lapply(1:counter, 
                     function(i) ar.gmm.mmsc(x, v.ar[i], v.moments[i], v.sigma2.rho, m.W, eff ) )
  l.test <- c( l.test.0, l.test.1)
    # The list of tests
  df.test <- lapply( l.test, function(x) data.frame( n.ar=x$n.ar, n.moments=x$n.moments, code=x$est$code,
                                                     J.stat=x$J.stat, mmsc.bic=x$mmsc.bic, penalty=x$penalty,
                                                     max.root=x$max.root, sigma=sqrt(x$v.sigma2.rho[1]), ar.1=x$v.sigma2.rho[2] ) ) %>%
    rbindlist()
  
  if(is.null(ar.order) & is.null(ar.moments)){
    n.opt <- (df.test$mmsc.bic + 1e10 * (df.test$code > 3) + 1e10 * (df.test$penalty > 1) ) %>% which.min
  }else{
    n.opt <- ((df.test$n.ar==ar.order) + (df.test$n.moments==ar.moments)) %>% which.max
      # Overwrite selection using fixed parameters
  }
  est.opt <- l.test[[n.opt]]
  
  return( list( est=est.opt, df=df.test %>% arrange(mmsc.bic) ) )
  
}



