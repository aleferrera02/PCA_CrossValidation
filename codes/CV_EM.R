library(MASS)
library(norm)
library(Matrix)

folds_creation <- function(n,p,K) {
  
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


EM_CV <- function(X,R,K){
  n <- nrow(X)
  p <- ncol(X)
  test <- folds_creation(n,p,K)
  CV <- rep(0,K)
  
  start <- Sys.time()

  for (fold in 1:K){
    
    Xcopy <- X
    mapply(function(r, c) Xcopy[r, c] <<- NA, test[[fold]][,1], test[[fold]][,2])
    s <- prelim.norm(Xcopy)
    thetahat <- em.norm(s,showits = FALSE)
    mu <- getparam.norm(s, thetahat)$mu
    sigma <- getparam.norm(s, thetahat)$sigma
    
    spec <- eigen(sigma)
    sigma_truncated <- spec$vectors %*% diag(c(spec$values[1:R],rep(0,p-R))) %*% t(spec$vectors)
    
    for (i in 1:n){
      
      row <- Xcopy[i,]
      na_indices <- which(is.na(row))
      mu_miss <- mu[na_indices]
      
      if (length(na_indices) == p){
        x_miss <- mu_miss
        CV[fold] <- CV[fold] + sum((x_miss - X[i,na_indices])^2)
      }
      else if (length(na_indices) != 0){
        x_obs <- row[-na_indices]
        mu_obs <- mu[-na_indices]
        sigma_obs <- sigma_truncated[-na_indices,-na_indices]
        sigma12 <- sigma_truncated[na_indices,-na_indices]
        x_miss <- mu_miss + sigma12 %*% ginv(sigma_obs) %*% (x_obs - mu_obs)
        CV[fold] <- CV[fold] + sum((x_miss - X[i,na_indices])^2)
      }
    }
    CV[fold] <- CV[fold] / nrow(test[[fold]])
  }
  
  end <- Sys.time()
  
  runtime <- end - start
  
  l <- list(CV = mean(CV), runtime = runtime)
  
  return(l)
}
