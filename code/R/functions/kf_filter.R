###########################################################
# kf_filter.R
# Functions for the Kalman Filter
#
# Philip Barrett
# Washington DC, 20jun2022
###########################################################

library(mvtnorm)

make.sym <- function(X) return(.5*(X+t(X)))

kf_ricatti <- function(l.ham){
# Solves the ricatti equation to give a good starting guess of the filter uncertainty
  
  # Extract the Kalman matrices
  mH <- l.ham$H ; mF <- l.ham$F
  mQ <- l.ham$Q ; mR <- l.ham$R
  
  # Initial guess for P
  P <- .5* ( l.ham$C.0 + t(l.ham$C.0) )
  tolerance <- 1e-10
  max_iterations <- 100000
  k <- 0
  inc <- 2*tolerance
  
  while(k < max_iterations & inc > tolerance) {
    
    ricatti.inverse <- solve(mH %*% P %*% t(mH) + mR)
    ricatti.inverse.sym <- make.sym(ricatti.inverse)
      # This step needed since the numerical inversion here can get pretty wild
    P_delta <- mF %*% P %*% t(mH) %*% ricatti.inverse.sym %*% mH %*% P %*% t(mF)
    P_delta_sym <- make.sym( P_delta )
      # Again, numerical stuff can get kind of funky here.  Don't want to break stuff
    P_next <- mF %*% P %*% t(mF) + mQ - P_delta_sym
    P_next_sym <- make.sym(P_next)
      # Same.  Can get very small errors here (1e-20ish). Also helps with convergence.
    
    # ## DEBUG LINE
    # if(!isSymmetric(P_next_sym)) browser()
    
    inc <- norm(P_next_sym - P, type = "F")
    P <- P_next_sym
    k <- k+1
  }
  message( '* Ricatti equation solved after ', k, ' iterations, with log10 precision: ', log(inc,10) %>% round(3) )
  return(P)
}

kf_filter_ham <- function( ham ){
# Wrapper for kh_filter_ham_mat
  out <- kf_filter_ham_mat( ham$Y, ham$H, ham$F, ham$Q, ham$R, ham$Xi.0, ham$C.0, 
                             ham$A, ham$X )
  return( out )
}

kf_filter_ham_mat <- function( m.y, big.H, big.F, m.Q, m.R, big.Xi.0, 
                               big.C0, big.A=NULL, m.X=NULL ){
# Computes the Kalman filter for the Hamilton form, [equation numbers] from
# Hamilton.  
  
# Idea: To incorporate missing data just need to drop the relevant elements from
# y and the rows/columns from A, H, and R.
  
  ### 1. Dimensions ###
  n.pds <- nrow(m.y)
  n.x <- m.X %>% ncol
  n.xi <- big.F %>% ncol
  n.y <- big.H %>% nrow
    # Dimensions
  
  ### 2. Checks ###
  # What should these be?
  
  ### 3. Set up the output ###
  m.xi.filt <- matrix(NA, n.pds+1, n.xi) %>% set_colnames( big.Xi.0 %>% names() )
  a.p.filt <- array( NA, dim=c( n.xi, n.xi, n.pds+1 ) )
  m.se.filt <- matrix(NA, n.pds+1, n.xi)
  m.xi.fcast <- matrix(NA, n.pds, n.xi) %>% set_colnames( big.Xi.0 %>% names() )
  a.p.fcast <- array( NA, dim=c( n.xi, n.xi, n.pds ) )
  a.k.gain <- array( NA, dim=c(n.xi, n.y, n.pds ) )
    # The filtered estimate and the forecast
  if(is.null(big.A)) big.A <- matrix( 0, n.y, 1 )
  if(is.null(m.X)) m.X <- matrix( 0, n.pds, 1 )
  m.xi.filt[1,] <- big.Xi.0
  a.p.filt[,,1] <- big.C0
  loglik <- 0
  
  ### 4. Some useful matrices ####
  H.prime <- big.H %>% t
  F.prime <- big.F %>% t
  
  # browser()
  ### 5. Main iteration ####
  for( this.pd in 1:n.pds ){
    
    xi.old <- m.xi.filt[this.pd,]
    P.old <- a.p.filt[,,this.pd]
      # The old filter point estimate and uncertainty. NB: These use "this.pd"
      # and note "this.pd-1" because there is an extra entry in the
      # corresponding matrix/array.  That is, these are offset by one vs. the
      # other variables here.  So entry t is really referring to the value in
      # period t-1.
    
    
    # Set up for missing data
    data.select <- !is.na(c(m.y[this.pd,]))
    # if(!any(data.select)){
    #   browser()
    # } #stop('All data missing in period #', this.pd)
    n.select <- sum(data.select)
    this.y <- c(m.y[this.pd, data.select])
    this.A <- matrix( big.A[ data.select, ], nrow=n.select, ncol=ncol(big.A) ) %>%
      set_colnames(big.A %>% colnames )
    this.H <- matrix( big.H[ data.select, ], nrow=n.select, ncol=ncol(big.H) ) %>%
      set_colnames(big.H %>% colnames )
    this.H.prime <- this.H %>% t
    this.R <- matrix( m.R[ data.select, data.select ], n.select, n.select )
    this.X <- c(m.X[this.pd,]) ; this.X[is.na(this.X)] <- 0
      # Create local copies of the observation equation matrices
    P.fcast <- big.F %*% P.old %*% F.prime + m.Q
      # The forecast uncertainty [13.2.21]
    y.cov <- this.H %*% P.fcast %*% this.H.prime + this.R
    if( n.select>0 ){
      K.gain.half <- solve( y.cov )
      K.gain <- big.F %*% P.fcast %*% this.H.prime %*% K.gain.half
        # The Kalman gain, [13.2.19]
      P.filt <- P.fcast - P.fcast %*% this.H.prime %*% K.gain.half %*% this.H %*% P.fcast
        # The filtered uncertainty [13.2.16]
    }else{
      P.filt <- P.fcast
    }
  
    if(!isSymmetric(P.filt)) P.filt <- make.sym(P.filt)
      # Sometimes can be numerically asymmetric
    xi.fcast <- big.F %*% xi.old
      # The forecast point estimate [13.2.17]
    if( n.select>0){
      xi.filt <- xi.fcast + 
        P.fcast %*% this.H.prime %*% K.gain.half %*% 
          ( this.y - this.A %*% this.X - this.H %*% xi.fcast )
        # The new point estimate [13.2.15]
      
    }else{
      xi.filt <- xi.fcast
    }
    
    # ## DEBUG LINE
    # if(!isSymmetric(P.filt)) browser()
    # if(!isSymmetric(y.cov)) browser()
    
    m.xi.filt[this.pd+1,] <- xi.filt
    m.xi.fcast[this.pd,] <- xi.fcast
      # The state
    a.p.filt[,,this.pd+1] <- P.filt
    a.p.fcast[,,this.pd] <- P.fcast
    m.se.filt[this.pd,] <- P.filt %>% diag %>% sqrt
      # The covariance
    if(n.select>0) a.k.gain[,data.select,this.pd] <- K.gain
    a.k.gain[,!data.select,this.pd] <- 0
      # The Kalman gain
    
    n.y <- this.y %>% length()
    loglik <- loglik + 
      if(n.y>0) dmvnorm( x = this.y, 
               mean = this.A %*% this.X + this.H %*% xi.fcast,
               sigma = y.cov, log = TRUE ) else 0
      # The log likelihood
  }
  
  out <- list( xi.filt=m.xi.filt, P.filt=a.p.filt, se.filt=m.se.filt,
               xi.fcast=m.xi.fcast, P.fcast=a.p.fcast, loglik=loglik,
               loglik.T=loglik / n.pds )
  return(out)
}

kf_smooth <- function( kf.filter, big.F ){
# Computes the Kalman Smoother from the filter
  
  ## Set up
  n.pds <- (kf.filter$P.fcast %>% dim)[3]
  F.prime <- big.F %>% t
  J.fn.stable <- function(i){
  # Stabilizes the calculation of J
    # this.P.fcast <- kf.filter$P.fcast[,,i] + 
    #   if(rcond(kf.filter$P.fcast[,,i])<1e-12) diag(ncol(kf.filter$P.fcast[,,i]))*1e-10 else 0
    i.counter <- 12
    this.P.fcast <- kf.filter$P.fcast[,,i]
    while( rcond(this.P.fcast) < 1e-15 & i.counter >0 ){
      this.P.fcast <- this.P.fcast + diag(ncol(this.P.fcast))*10^(-i.counter)
      i.counter <- i.counter-1
    }
    if( rcond(this.P.fcast) < 1e-16 ){
      browser()
    }
    return(kf.filter$P.filt[,,i] %*% F.prime %*% solve(this.P.fcast))
  }
  J <- lapply( 1:n.pds, J.fn.stable )
    # Hamilton eq [13.6.11]
  
  ## Main recursion
  xi.smooth <- kf.filter$xi.filt
  P.smooth <- kf.filter$P.filt
    # Initialize with the filtered versions
  for( this.pd in n.pds:1 ){
    xi.smooth[this.pd,] <- kf.filter$xi.filt[this.pd,] + 
      J[[this.pd]] %*% ( xi.smooth[this.pd+1,] - kf.filter$xi.fcast[this.pd,] )
        # Hamilton eq [13.6.16]
    P.smooth[,,this.pd] <- kf.filter$P.filt[,,this.pd] + 
      J[[this.pd]] %*% ( P.smooth[,,this.pd+1] - kf.filter$P.fcast[,,this.pd] ) %*% 
        t(J[[this.pd]])
        # Hamilton eq [13.6.20]
  }
  return( list(xi.smooth=xi.smooth, P.smooth=P.smooth) )
}