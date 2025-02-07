create_data <- function(sample_size, n_features, singular_values, noise){
  
  # function ro create a data matrix with a given 
  # singular value decomposition and noise
  
  # Parameters:
  #  - sample_size: number of samples (rows)
  #  - n_features: number of features (columns)
  #  - singular_values: vector of singular values
  #  - noise: type of noise ('gaussian', 'heavy', 'colored')
  
  
  n = sample_size              # sample size
  p = n_features               # number of features
  r = length(singular_values)  # rank of the matrix
  
  D = diag(singular_values)    # diagonal matrix of singular values
  U = rnorm(n*r,0,1/sqrt(n))   # random matrix U
  U = matrix(U,n,r)            # reshape U
  V = rnorm(p*r,0,1/sqrt(p))   # random matrix V
  V = matrix(V,p,r)            # reshape V
  
  X = sqrt(n) * U%*%D%*%t(V)   # create the matrix X
  E = matrix(0,n,p)            # create the noise matrix E
  
  if (noise == 'gaussian'){
    E = rnorm(n*p,0,1)         # create the noise matrix E
  }
  else if (noise == 'heavy'){
    df = 3                     # degrees of freedom  
    sigma = sqrt(df/(df - 2))  # scale parameter
    E = rt(n*p,df)/sigma       # create the noise matrix E
  }
  else if (noise == 'colored'){
    df1 = 3                    # degrees of freedom 1
    df2 = 3                    # degrees of freedom 2
    sigma2 = 1/rchisq(n,df1)   # scale parameter 1
    tau2 = 1/rchisq(p,df2)     # scale parameter 2
    c = sqrt(1/(df1-2) + 1/(df2-2)) # scale parameter
    for (i in 1:n)
      for (j in 1:p)
        E[(i-1)*p+j] = rnorm(1,0,sqrt(sigma2[i] + tau2[j]))
  }
  else if (noise == 'null'){
    E = rep(0,n*p)             # create the noise matrix E
  }
  else
    stop('Unknown noise type')
  
  E = matrix(E,n,p)           # reshape E
  X = X + E                   # add the noise to X
  l <- list(X = X, E = E)
  return(l)
}