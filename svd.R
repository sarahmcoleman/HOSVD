# load ggplot2/tidyverse
library(tidyverse)

## Define matrix
x <- matrix(c(4, 8, 8, 0, 0,
              -3, -6, -6, 0, 0,
              -4, 4, -2, 0, 0,
              0, 0, 0, -4, 3), 
            nrow = 4, ncol = 5,
            byrow = TRUE)

# Get rank of matrix
rank_x <- qr(x)$rank

## Perform SVD
svd_result <- svd(x, nu = rank_x, nv = rank_x)

# Get out SVD components
u <- svd_result$u # left singular vectors
d <- (svd_result$d)[1:rank_x] # singular values
s <- diag(d, rank_x, rank_x) # sigma matrix (singular values in diag matrix)
w <- svd_result$v
w_t <- t(w) # right singular vectors

# Confirm SVD equation holds
x_test <- u %*% s %*% w_t
all.equal(x, x_test) # note that direct comparisons using == may not work due to 
# the way numbers are stored in R

# Lower-rank approximations of x
x_1 <- matrix(u[, 1], ncol = 1) %*% s[1:1,1:1] %*% matrix(w_t[1, ], nrow = 1)
x_2 <- u[, 1:2] %*% s[1:2,1:2] %*% w_t[1:2, ]

# Show how the Frobenius norm of x decreases with further approximations
norm(x, type = 'F')
norm(x-x_1, type = 'F')
norm(x-x_2, type = 'F')
norm(x-x_test, type = 'F')

## Principal components analysis
t <- x %*% w
t_other_way <- u %*% s
all.equal(t, t_other_way)

# Plot the data on the new dimensions
t |> 
  as.data.frame() |> 
  rename_with(~paste0('PC', 1:3)) |> 
  mutate(n = 1:nrow(t)) |> 
  ggplot(aes(x = PC1, y = PC2)) +
  geom_text(aes(label = n)) + 
  theme_bw()

## Plot the variance contribution
# Make the dataframe
df <- data.frame('r' = 1:rank_x, # singular value index
                 's' = d, # singular value
                 'e' = d^2, # eigenvalue = singular value squared
                 'var_exp'= d^2/sum(d^2), # variance explained 
                 'cum_var_exp' = cumsum(d^2)/sum(d^2)) # cum var explained

# Variance explained by each singular value
df |> 
  ggplot(aes(x = r)) +
  geom_col(aes(y = var_exp)) +
  labs(x = 'Singular value', y = 'Variance Explained')

# Cumulative variance explained by singular value
df |> 
  ggplot(aes(x = r)) +
  geom_col(aes(y = cum_var_exp)) +
  labs(x = 'Singular value', y = 'Cumulative Variance Explained')

