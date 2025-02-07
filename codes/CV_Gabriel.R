source("create_data.R")


# #install scatterplot
# install.packages("scatterplot3d")

#import scatterplot
library(scatterplot3d)



CV_PCA_repaired <- function(X, r_rank, K, Pi=0.7) { 
  # Ensure X is centered
  column_means <- colMeans(X)
  if(any(abs(column_means) > 1e-12)){
    warning("X should be centered. Attempting to center now.")
    X <- scale(X, center = TRUE, scale = FALSE)
  }
  
  # Dimensions
  N <- nrow(X)
  p <- ncol(X)
  
  # K-fold partitioning of row indices
  # Each row of Ind represents a fold
  Ind <- matrix(sample(1:N), nrow = K)
  
  # Array to store errors for each fold and rank
  Err <- array(0, c(K, 1))
  
  # Load the MASS package for the Moore-Penrose inverse
  if(!requireNamespace("MASS", quietly = TRUE)){
    stop("Package 'MASS' is required.")
  }
  
  # Define sample covariance function
  sample_cov <- function(Y) {
    # Compute sample covariance with denominator N (not N-1) since PCA often uses unbiased estimator
    # For PCA, often the unbiased estimator (cov(Y)) is sufficient.
    # Here, we assume Y is already centered.
    # cov(Y) uses (N-1) in the denominator. If you prefer the unbiased estimator, just use cov(Y).
    cov(Y)
  }
  
  start <- Sys.time()
  
  # Main loop for cross-validation
  for(k in 1:K){
    # Training set (rows not in the k-th fold)
    Xact <- X[-Ind[k,], ]
    # Test set (the k-th fold)
    Xout <- X[Ind[k,], ]
    
    # Compute full-sample covariance from training data
    C_hat <- cov(Xact)
    EIG <- eigen(C_hat)
    
    # For each rank, approximate covariance and perform prediction
      
    # Construct rank-r covariance approximation
    D <- diag(c(EIG$values[1:r_rank],rep(0,p-r_rank)))
    U <- EIG$vectors
    C_hat_r <- U %*% D %*% t(U)
    
    # Reconstruct Xout using half of the variables as "observed"
    X_hat <- matrix(0, nrow = nrow(Xout), ncol = p)
    for(m in 1:nrow(Xout)){
      # Randomly select half of the features as observed
      ind <- sample(1:p, floor(p*Pi))
      # Partition the covariance matrix accordingly
      Sigma22 <- C_hat_r[ind, ind]
      Sigma12 <- C_hat_r[-ind, ind]
      
      # Compute predictions for missing part
      # ginv is the Moore-Penrose generalized inverse from MASS
      X_hat[m, -ind] <- Sigma12 %*% MASS::ginv(Sigma22) %*% Xout[m, ind]
      X_hat[m, ind] <- Xout[m, ind]  # observed part is used as is
        
      }
      
      # Compute sum of squared errors for this fold and rank
      Err[k] <- sum((Xout - X_hat)^2)/(nrow(Xout)*(ncol(Xout)-floor(p*Pi)))
    
  }
  
  end <- Sys.time()
  
  l <- list(CV = mean(Err), runtime = end - start)
  
  # Return total error per rank (summing over folds)
  return(l)
}