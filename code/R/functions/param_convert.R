##########################################################################################
#
# param_convert.R
#
# Functions for converting between the parameter forms for the big GMM
#
# Philip Barrett, Washington DC
# First version: 25feb2025
#
##########################################################################################

#### Test code ####
# l.psi <- make.psi.list(l.state.est, l.Q.est)
# l.meta <- l.psi$meta
# v.theta <- make.theta.H(l.psi)
# v.theta - psi.to.theta.v( theta.to.psi.v( v.theta, l.meta ), l.meta )
# v.psi <- make.psi.vector( l.psi )
# v.psi - theta.to.psi.v( psi.to.theta.v( v.psi, l.meta ), l.meta )

make.psi.list <- function( state.eqs, obs.eqs ){
  # Makes the list form of Psi from the initial estimates
  
  m.rho <- matrix( state.eqs$ar.coef[-1,],
                   ncol=ncol(state.eqs$ar.coef) )
  # The matrix of AR coefficients
  m.Q <- state.eqs$Q
  # The matrix of covariances
  kappa <- obs.eqs$kappa
  # The vector of data coefficients
  m.R <- obs.eqs$R
  # The matrix of observation covariances
  R.struct <- which( m.R!=0, arr.ind=T)
  # The block-diagnoal structure of R
  l.meta <- list( n.N=ncol(m.rho), n.L=nrow(m.rho), 
                  n.Q=.5*ncol(m.rho)*(ncol(m.rho)+1),
                  n.P=length(kappa), R.struct=R.struct,
                  v.x.q=obs.eqs$df.coefs$state,
                  v.states=state.eqs$ar.coef %>% colnames() )
  
  return( list(rho=m.rho, Q=m.Q, kappa=kappa, R=m.R, meta=l.meta ) )
}

make.psi.vector <- function( l.psi ){
# Makes the Psi vector
  out <- c( l.psi$rho, 
            l.psi$Q[lower.tri(l.psi$Q, diag = TRUE)],
            l.psi$kappa, l.psi$R[lower.tri(l.psi$R, diag = TRUE) & l.psi$R != 0 ] )
  return(out)
}

make.psi.list.v <- function( v.psi, meta ){
# Makes the psi list from the vector
  
  # Rho
  m.rho <-  matrix( v.psi[1:(meta$n.N)*(meta$n.L)], ncol=meta$n.N, nrow=meta$n.L )
  
  # Q
  m.Q <- matrix(0, ncol=meta$n.N, nrow=meta$n.N )
  counter <- (meta$n.N)*(meta$n.L)
  for( i in 1:meta$n.N ){
    for( j in i:meta$n.N ){
      counter <- counter+1
      m.Q[i,j] <- m.Q[j,i] <- v.psi[counter]
    }
  }
  
  # kappa
  kappa <- v.psi[counter + 1:meta$n.P ]
  
  # R
  m.R <- matrix( 0, meta$n.P, meta$n.P )
  counter <- counter+meta$n.P
  R.struct <- meta$R.struct[ meta$R.struct[,'row'] >= meta$R.struct[,'col'], ]
  for( k in 1:nrow(R.struct) ){
    counter <- counter+1
    this.i <- R.struct[k, 'row']
    this.j <- R.struct[k, 'col']
    m.R[this.i,this.j] <- m.R[this.j,this.i] <- v.psi[counter]
  }
  
  return( list(rho=m.rho, Q=m.Q, kappa=kappa, R=m.R, meta=meta ) )
}

make.theta.H <- function( l.psi ){
  # Makes theta from a Psi list-form
  Q.chol <- l.psi$Q %>% chol
  R.chol <- l.psi$R %>% chol
  out <- c( l.psi$rho, 
            Q.chol[upper.tri(Q.chol, diag = TRUE)],
            l.psi$kappa, R.chol[upper.tri(R.chol, diag = TRUE) & R.chol != 0 ] )
  return(out)
}

theta.to.psi.list <- function( v.theta, meta ){
  # Converts vector theta to the vector psi
  v.psi <- 0*v.theta
  
  # Rho
  m.rho <-  matrix( v.theta[1:(meta$n.N)*(meta$n.L)], ncol=meta$n.N, nrow=meta$n.L )
  
  # Q
  m.Q.chol <- matrix(0, ncol=meta$n.N, nrow=meta$n.N )
  counter <- (meta$n.N)*(meta$n.L)
  for( i in 1:meta$n.N ){
    for( j in 1:i ){
      counter <- counter+1
      m.Q.chol[j,i] <- v.theta[counter]
    }
  }
  m.Q <- t(m.Q.chol) %*% m.Q.chol
  
  # kappa
  kappa <- v.theta[counter + 1:meta$n.P ]
  
  # R
  m.R.chol <- matrix( 0, meta$n.P, meta$n.P )
  counter <- counter+meta$n.P
  R.struct <- meta$R.struct[ meta$R.struct[,'row'] <= meta$R.struct[,'col'], ]
  for( k in 1:nrow(R.struct) ){
    counter <- counter+1
    this.i <- R.struct[k, 'row']
    this.j <- R.struct[k, 'col']
    m.R.chol[this.i,this.j] <- v.theta[counter]
  }
  m.R <- t(m.R.chol) %*% m.R.chol
  
  return( list(rho=m.rho, Q=m.Q, kappa=kappa, R=m.R, meta=meta ) )
}

theta.to.psi.v <- function( v.theta, meta ) return( make.psi.vector(theta.to.psi.list(v.theta, meta)))
psi.to.theta.v <- function( v.psi, meta ) return( make.theta.H( make.psi.list.v( v.psi, meta) ) )
