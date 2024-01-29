lapply(c('MASS', 'glmnet'), require, character.only=TRUE)

set.seed(123)

# Function for Soft Thresholding
soft_threshold_lasso <- function(x, lambda) {
  sign(x) * max(0, abs(x) - lambda)
}

# Coordinate Descent Algorithm for Lasso  
lasso_coordinate_descent <- function(X, y, lambda, max_iter = 1000, tol = 1e-6) {
  n <- nrow(X)
  p <- ncol(X)
  b <- rep(0, p)
  
  for (iter in 1:max_iter) {
    res <- y - X %*% b
    for (j in 1:p) {
      # obtain partial residuals
      partial_res <- res + X[, j] * b[j]
      
      # calculate beta_j_star 
      beta_j_star <- sum(X[, j] * partial_res) / n
      
      # update beta_j with soft thresholding
      b[j] <- soft_threshold_lasso(beta_j_star, lambda)
      res <- partial_res - X[, j] * b[j]
    }
    
    # condition to check the convergence
    if (max(abs(res)) < tol) {
      break
    }
  }
  return(as.matrix(b)) # returning the matrix of the beta coefficient for further matrix multiplications
}

p <- 100 # number of predictors
n <- 200 # number of observations
sparsity <- 0.2
sigma <- 3
pairwise_cor = 0.5

generate_beta <- function(sparsity, p) {
  # Generate random values of beta
  Beta <- runif(n = p, min = -2, max = 2)
  # Introduce sparsity by setting a percentage of elements to zero
  num_zeros <- round(sparsity * p)
  sparse_indices <- sample(p, num_zeros, replace = FALSE)
  Beta[sparse_indices] <- 0
  
  return(as.matrix(Beta))
}

# generate beta
beta_true <- generate_beta(sparsity, p)

covmatrix <- matrix(rep(0,p*p),p,p)
for (i in 1:p){
  for (j in 1:p){
    covmatrix[i,j] <- pairwise_cor^(abs(i-j)) # assuming X is drawn from population with Variance = 1
  }
}

X <- mvrnorm(n, mu = rep(0,p), Sigma = covmatrix) # generating the random sample
X = scale(X)

# generating error terms
eps <- matrix(rnorm(n, mean = 0, sd = 1)) # generating error terms 

y <- X %*% beta_true + sigma*eps # using the model equation to generate y

# Function for k-fold cross-validation
lasso_cv <- function(x, y, k_fold = 5, n_lambda = 100, max_iter = 1000, tol = 1e-6) {
  
  fold_indices <- sample(rep(1:k_fold, length.out = nrow(x)))
  
  # Split data into folds
  folds <- cut(1:n, breaks = k_fold, labels = FALSE)
  
  lambda_values <- 10^seq(-3, 1, length.out = n_lambda)
  cv_errors <- matrix(0, nrow = k_fold, ncol = n_lambda)
  
  for (k in 1:k_fold) {
    # Create training and validation sets for this fold
    val_indices <- which(folds == k)
    train_indices <- setdiff(1:n, val_indices)
    
    x_train <- x[train_indices, ]
    y_train <- y[train_indices]
    x_val <- x[val_indices, ]
    y_val <- y[val_indices]
    
    for (i in 1:length(lambda_values)) {
      
      beta_hat <- lasso_coordinate_descent(x_train, y_train, lambda_values[i], tol = tol, max_iter = max_iter)
      
      # Predict on validation set
      y_pred <- x_val %*% beta_hat
      
      # Calculate mean squared error and store in the matrix
      cv_errors[k, i] <- mean((y_pred - y_val)^2)
    }
  }
  
  # Calculate mean cross-validated error for each lambda
  mean_cv_errors <- apply(cv_errors, 2, mean)
  
  # Identify the value of lambda that minimizes the cross-validated error
  best_lambda_index <- which.min(mean_cv_errors)
  best_lambda <- lambda_values[best_lambda_index]
  min_mse <- mean_cv_errors[best_lambda_index]
  
  # Calculate standard errors of the estimated test MSE for each lambda value
  sd_cv_errors <- apply(cv_errors, 2, sd)
  se_cv_errors <- sd_cv_errors/sqrt(k_fold)
  
  # caclulate the largest value of lambda with mse within 1 standard error of the minimum mse
  one_se_rule = se_cv_errors[best_lambda_index] + min_mse
  one_se_lambda_index <- which(mean_cv_errors <= one_se_rule)
  one_se_lambda <- max(lambda_values[one_se_lambda_index])
  
  list(best_lambda = best_lambda, mean_cv_errors = mean_cv_errors, one_se_lambda = one_se_lambda, 
       se_cv_errors = se_cv_errors, mse_best_lambda = min_mse, 
       mse_1se_lambda = mean_cv_errors[which.max(lambda_values[one_se_lambda_index])],
       lambda_values = lambda_values)
}

cv_result = lasso_cv(X, y, k_fold = 5, n_lambda = 100, max_iter = 1000, tol = 1e-6)

# Plot mean cross-validated errors
plot(log(cv_result$lambda_values), cv_result$mean_cv_errors, type = "l", col = "red",
     xlab = "log(lambda)", ylab = "Mean CV Error", main = "Cross-Validated Lasso", ylim = (c(8,20)))

lines(log(cv_result$lambda_values), (cv_result$mean_cv_errors + cv_result$se_cv_errors), type = "l", col = "grey",
      xlab = "log(lambda)", ylab = "Mean CV Error", main = "Cross-Validated Lasso")
lines(log(cv_result$lambda_values), (cv_result$mean_cv_errors - cv_result$se_cv_errors), type = "l", col = "grey",
      xlab = "log(lambda)", ylab = "Mean CV Error", main = "Cross-Validated Lasso")

# Add points for the minimum mean CV error and one standard error rule
points(log(cv_result$best_lambda), cv_result$mse_best_lambda, col = "red", pch = 19)
points(log(cv_result$one_se_lambda), cv_result$mse_1se_lambda, col = "grey", pch = 19)

# Add lines for the minimum mean CV error and one standard error rule
abline(v = log(cv_result$best_lambda), col = "black", lty = 2)
abline(v = log(cv_result$one_se_lambda), col = "black", lty = 2)

# Add legend
legend("topleft", legend = c("Mean CV Error", "1 SE from Mean CV Error", "Minimum CV error","Max lambda within 1 SE"),
       col = c("red", "grey", "red","grey"), lty = c(1, 1, 0, 0), pch = c(NA, NA, 19, 19))


# Print the results
cat("Optimal Lambda:", cv_result$best_lambda, "\n")
cat("Lambda within 1 SE:", cv_result$one_se_lambda, "\n")

# calculate MSE on test data
train_ind = sample(nrow(X), size = 0.8*nrow(X)) # 20% test data
X_train = X[train_ind,]
y_train = as.matrix(y[train_ind,])
X_test = X[-train_ind,]
y_test = as.matrix(y[-train_ind,])

# using lambda_1se to generate beta_hat
lambda_1se = cv_result$one_se_lambda
beta_hat = lasso_coordinate_descent(X_train, y_train, lambda_1se)
y_pred = X_test %*% beta_hat
MSE = mean((y_test - y_pred)^2)
MSE

set.seed(42)

# Function for Soft Thresholding
soft_threshold_elasticnet <- function(x, lambda, alpha) {
  sign(x) * max(0, (abs(x) - (alpha)*lambda) / (1 + (1-alpha)*lambda))
}

# Coordinate Descent Algorithm for Lasso  
elastic_net_coordinate_descent <- function(X, y, lambda, alpha, max_iter = 1000, tol = 1e-6) {
  n <- nrow(X)
  p <- ncol(X)
  b <- rep(0, p)
  
  for (iter in 1:max_iter) {
    res <- y - X %*% b
    for (j in 1:p) {
      # obtain partial residuals
      partial_res <- res + X[, j] * b[j]
      
      # calculate beta_j_star 
      beta_j_star <- sum(X[, j] * partial_res) / n
      
      # update beta_j with soft thresholding
      b[j] <- soft_threshold_elasticnet(beta_j_star, lambda, alpha)
      res <- partial_res - X[, j] * b[j]
    }
    
    # condition to check the convergence
    if (max(abs(res)) < tol) {
      break
    }
  }
  return(as.matrix(b)) # returning the matrix of the beta coefficient for further matrix multiplications
}

p <- 200 # number of predictors
n <- 100 # number of observations
sparsity <- 0.5
sigma <- 2
pairwise_cor = 0.8

generate_beta <- function(sparsity, p) {
  # Generate random values of beta
  Beta <- runif(n = p, min = -2, max = 2)
  # Introduce sparsity by setting a percentage of elements to zero
  num_zeros <- round(sparsity * p)
  sparse_indices <- sample(p, num_zeros, replace = FALSE)
  Beta[sparse_indices] <- 0
  
  return(as.matrix(Beta))
}

# generate beta vector
beta_true <- generate_beta(sparsity, p)

covmatrix <- matrix(rep(0,p*p),p,p)
for (i in 1:p){
  for (j in 1:p){
    covmatrix[i,j] <- pairwise_cor^(abs(i-j)) # assuming X is drawn from population with Variance = 1
  }
}

X <- mvrnorm(n, mu = rep(0,p), Sigma = covmatrix) # generating the random sample
X <- scale(X)

eps <- matrix(rnorm(n, mean = 0, sd = 1)) # generating error terms 

y <- X %*% beta_true + sigma*eps # using the model equation to generate y

# Function for k-fold cross-validation
elastic_net_cv <- function(x, y, k_fold = 5, n_lambda = 100, max_iter = 1000, tol = 1e-6) {
  
  n <- nrow(x)
  fold_indices <- sample(rep(1:k_fold, length.out = nrow(x)))
  
  # Split data into folds
  folds <- cut(1:n, breaks = k_fold, labels = FALSE)
  
  alpha_values <- seq(0, 1, 0.1)
  lambda_values <- 10^seq(-3, 1, length.out = n_lambda)
  cv_errors <- matrix(0, nrow = k_fold, ncol = n_lambda)
  
  results <- data.frame(cv_error = numeric(), lambda = numeric(), alpha = numeric(), fold = numeric())
  
  for (k in 1:k_fold) {
    # Create training and validation sets for this fold
    val_indices <- which(folds == k)
    train_indices <- setdiff(1:n, val_indices)
    
    x_train <- x[train_indices, ]
    y_train <- y[train_indices]
    x_val <- x[val_indices, ]
    y_val <- y[val_indices]
    
    for (i in 1:length(alpha_values)) {
      for (j in 1:length(lambda_values)) {
        
        beta_hat <- elastic_net_coordinate_descent(x_train, y_train, lambda_values[j], alpha_values[i], tol = tol, max_iter = max_iter)
        
        # Predict on validation set
        y_pred <- x_val %*% beta_hat
        
        # Calculate mean squared error and store in the data frame
        cv_error <- mean((y_pred - y_val)^2)
        result <- data.frame(cv_error = cv_error,  alpha = alpha_values[i], lambda = lambda_values[j], fold = k)
        results <- rbind(results, result)
      }
    }
  }
  
  # Calculate mean cross-validated error for each lambda
  mean_cv_errors <- aggregate(cv_error ~ lambda+alpha, data = results, mean)
  
  # Identify the value of lambda that minimizes the cross-validated error
  best_index <- which.min(mean_cv_errors$cv_error)
  best_lambda <- mean_cv_errors$lambda[best_index]
  best_alpha <- mean_cv_errors$alpha[best_index]
  min_mse <- mean_cv_errors$cv_error[best_index]
  
  # Calculate standard errors of the estimated test MSE for value of lambda
  sd_cv_errors <- aggregate(cv_error ~ lambda+alpha, data = results, sd)$cv_error
  se_cv_errors <- sd_cv_errors/sqrt(k_fold)
  
  one_se_rule <- se_cv_errors[best_index] + min_mse
  one_se_lambda_index <- which(mean_cv_errors$cv_error <= one_se_rule)
  one_se_lambda <- max(results$lambda[one_se_lambda_index])
  
  list(best_lambda <- best_lambda, mean_cv_errors = mean_cv_errors$cv_error, one_se_lambda = one_se_lambda, 
       se_cv_errors <- se_cv_errors, mse_best_lambda = min_mse, 
       mse_1se_lambda <- mean_cv_errors$cv_error[which.max(mean_cv_errors$lambda[one_se_lambda_index])],
       lambda_values <- lambda_values, best_alpha = best_alpha, results = results)
}

# Function for set validation
elastic_net_set_validation <- function(x, y, train_ratio = 0.8, n_lambda = 100, max_iter = 1000, tol = 1e-6) {
  
  # split data into training and validation
  train_size <- round(train_ratio * n)
  train_indices <- sample(1:n, train_size, replace = FALSE)
  x_train <- x[train_indices, ]
  y_train <- y[train_indices]
  x_val <- x[-train_indices, ]
  y_val <- y[-train_indices]
  
  # set alpha and lambda values
  alpha_values <- seq(0, 1, 0.1)
  lambda_values <- 10^seq(-3, 1, length.out = n_lambda)
  
  results <- data.frame(mse = numeric(), lambda = numeric(), alpha = numeric())
  
  # loop over all combinations of alpha and lambda
  for (i in 1:length(alpha_values)) {
    for (j in 1:length(lambda_values)) {
      
      # estimate beta
      beta_hat <- elastic_net_coordinate_descent(x_train, y_train, lambda_values[j], alpha_values[i], tol = tol, max_iter = max_iter)
      
      # Predict on validation set
      y_pred <- x_val %*% beta_hat
      
      # Calculate mean squared error and store in the data frame
      mse <- mean((y_pred - y_val)^2)
      result <- data.frame(mse = mse,  alpha = alpha_values[i], lambda = lambda_values[j])
      results <- rbind(results, result)
    }
  }
  
  # Identify the value of lambda that minimises the mse
  best_index <- which.min(results$mse)
  best_lambda <- results$lambda[best_index]
  best_alpha <- results$alpha[best_index]
  min_mse <- results$mse[best_index]
  
  list(best_lambda = best_lambda, min_mse = min_mse, lambda_values = lambda_values, alpha_values = alpha_values,
       best_alpha = best_alpha, results = results)
}

# Use set validation approach
set_validation_result <- elastic_net_set_validation(X, y, train_ratio = 0.8, n_lambda = 100, max_iter = 1000, tol = 1e-6)

# Print the results
cat("Optimal Lambda:", set_validation_result$best_lambda, "\n")
cat("Optimal alpha:", set_validation_result$best_alpha, "\n")

# calculate MSE on test data
train_ind <- sample(nrow(X), size = 0.8*nrow(X)) # 20% test data
X_train <- X[train_ind,]
y_train <- as.matrix(y[train_ind,])
X_test <- X[-train_ind,]
y_test <- as.matrix(y[-train_ind,])

lambda_sv <- set_validation_result$best_lambda
alpha <- set_validation_result$best_alpha
beta_hat <- elastic_net_coordinate_descent(X_train, y_train, lambda_sv, alpha)
y_pred <- X_test %*% beta_hat
MSE_sv <- mean((y_test - y_pred)^2)
MSE_sv
