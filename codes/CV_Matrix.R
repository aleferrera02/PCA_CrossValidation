hardtresholding <- function(X, X_hat, R) {
  
  tol <- 10^-5
  nmax <- 100
  M_help <- X_hat
  k <- 0
  CV_error <- 0
  mask <- (X_hat == 0)
  number <- sum(!mask)
  err_new <- 0
  delta <-tol + 1
  
  while( k < nmax & delta > tol) {
    
    k <- k + 1
    err_old <- err_new
    
    decomposition <- svd(M_help)
    D_reduced <- diag(c(decomposition$d[1:R], rep(0, length(decomposition$d) - R)))
    M <- decomposition$u %*% D_reduced %*% t(decomposition$v)
    
    err_new <- sum((M[!mask] - X_hat[!mask])^2)/number
    
    delta <- abs(err_new - err_old)
    
    M_help <- X_hat + M * mask
    
  }

  CV_error <- sum ((M[mask]-X[mask])^2) / (sum(mask))
  
  return(CV_error)
}

create_random_folds <- function(n,p,K) {
  
  l <- list()
  number <- n*p
  shuffled_values <- sample(1:number, number, replace = FALSE)
  groups <- split(shuffled_values, cut(seq_along(shuffled_values), K, labels = FALSE))
  
  for (k in 1:K) {
    l[[k]] <- matrix(0, length(groups[[k]]), 2)
    l[[k]][,1] <- ((groups[[k]] - 1) %% n) + 1
    l[[k]][,2] <- (groups[[k]] - 1) %/% n + 1
  }
  
  return(l)
}

CV_hardtresholding  <- function(X, R, K) {
  
  n <- nrow(X)
  p <- ncol(X)
  
  folds<- create_random_folds(n, p, K)
  
  CV <- 0
  
  start <- Sys.time()
  
  for (k in 1:K) {
    X_hat <- X
    for (count in 1:nrow(folds[[k]])){
      i <- folds[[k]][count,1]
      j <- folds[[k]][count,2]
      X_hat[i,j] <- 0
    }
    CV <- CV + hardtresholding(X, X_hat, R)
  }
  
  end <- Sys.time()
  
  l <- list(CV = CV/ K, runtime = end - start)
  
  return(l)
}