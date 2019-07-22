
# gauss_newton_method -----------------------------------------------------

# data cleaning -----------------------------------------------------------

setwd("C:/Users/yukic/Documents/Data")
df <- readRDS("rakumachi_niigata_waseda_20181230.rds")

sum(is.na(df$price))
sum(is.na(df$gross))
df <- df[is.na(df$gross) == FALSE, ] 

df$area_2 <- ifelse(df$prefecture == "Niigata", "Niigata", "Waseda")
df$district <- ifelse(df$prefecture == "Niigata", "Rural", "Urban")

# Check price and gross relationship --------------------------------------

library(ggplot2)
ggplot(df) +
  geom_point(aes(price, gross)) +
  labs(title = "Property Price vs Yield Percent",
       x = "Income property price (10,000 JPY)",
       y = "Yiled percent before cost (%)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df) +
  geom_point(aes(price, gross, color = area_2)) +
  labs(title = "Property Price vs Yield Percent",
       x = "Income property price (10,000 JPY)",
       y = "Yiled percent before minus cost (%)")

ggplot(df) +
  geom_point(aes(price, log(gross))) +
  labs(title = "Property Price vs Yield Percent",
       x = "Income property price (10,000 JPY)",
       y = "Yiled percent before minus cost (%)")

ggplot(df) +
  geom_point(aes(price, gross, color = district)) +
  labs(title = "Property Price vs Yield Percent",
       x = "Income property price (10,000 JPY)",
       y = "Yiled percent before cost (%)") +
  theme(plot.title = element_text(hjust = 0.5))


# Initial fit -------------------------------------------------------------

model <- lm(log(gross) ~ price, data = df)
summary(model)
coef(model)
plot(df$price, log(df$gross))
abline(model, col = "purple")

b_0 <- coef(model)[1]
b_1 <- coef(model)[2]

g_0.0 <- exp(b_0)
g_1.0 <- b_1

# Saving initial fitted value
g_vec <- c(g_0.0, g_1.0)


# Nonlinear regression by Gauss Newton method -----------------------------

gn <- function(g, x, y) {
  
  # Setting the initial value for iteration
  g_0 <- g[1]
  g_1 <- g[2]
  
  f_iter <- g_0 * exp(g_1 * x)
  
  y_iter <- y - f_iter
  
  # iterative SSE to evaluate model 
  SSE_iter <- sum((y - f_iter)^2)
  
  # Partial derivative
  D_0 <- exp(g_1 * x)
  D_1 <- g_0 * x * exp(g_1 * x)
  D_iter <- cbind(D_0, D_1)
  
  # Obtain the least squares estimates without intercept
  b_vec <- coef(lm(y_iter ~ D_0 + D_1 -1))
  
  # Obtain the revised least squares estimates
  g_0_iter <- g_0 + b_vec[1]
  g_1_iter <- g_1 + b_vec[2]
  g_iter <- c(g_0_iter, g_1_iter)

  return(list(Y = y_iter,
              D = D_iter,
              SSE = SSE_iter,
              g = g_iter))
}

gn_result <- gn(g_vec, df$price, df$gross)
names(gn_result)

gn_result$Y
gn_result$D
gn_result$SSE
gn_result$g

dim(df)

# Check the curve of the model --------------------------------------------

fitted <- function(estimate, x) {
  
  est_0 <- estimate[1]
  est_1 <- estimate[2]
  
  return(est_0 * exp(est_1 * x))
  
}

plot(df$price, df$gross, main = "Initial Model (Red) vs GN Model (Purple)")
x.data <- seq(0, 8000, 1)
lines(x.data, fitted(gn_result$g, x.data), col = "purple")
lines(x.data, fitted(g_vec, x.data), col = "red")


# Iterative nonlinear regression by Gauss Newton method -------------------

iter_num <- 10
SSE_vec <- rep(NA, iter_num)
est_mat <- matrix(NA, nrow = iter_num, ncol = 2)

gn <- function(g, x, y) {
  
  # Setting the initial value for iteration
  g_0 <- g[1]
  g_1 <- g[2]
  
  # iterative provess
  for (i in 1:iter_num) {
    
  f_iter <- g_0 * exp(g_1 * x)
  
  y_iter <- y - f_iter
  
  # Partial derivative
  D_0 <- exp(g_1 * x)
  D_1 <- g_0 * x * exp(g_1 * x)
  D_iter <- cbind(D_0, D_1)
  
  # Obtain the least squares estimates without intercept
  b_vec <- coef(lm(y_iter ~ D_0 + D_1 -1))
  
  # Obtain the revised least squares estimates
  g_0_iter <- g_0 + b_vec[1]
  g_1_iter <- g_1 + b_vec[2]
  g_iter <- c(g_0_iter, g_1_iter)
  
  # iterative SSE to evaluate model 
  SSE_iter <- sum((y - f_iter)^2)
  
  # Saving iterative value
  SSE_vec[i] <- SSE_iter
  est_mat[i, ] <- g_iter
  
  # Update g
  g_0 <- g_0_iter
  g_1 <- g_1_iter

  }
  
  return(list(Y = y_iter,
              D = D_iter,
              SSE = SSE_iter,
              g = g_iter,
              SSE_iter = SSE_vec,
              est_iter = est_mat))
}

gn_result <- gn(g_vec, df$price, df$gross)
names(gn_result)

gn_result$Y
gn_result$D
gn_result$SSE
gn_result$g
gn_result$SSE_iter
gn_result$est_iter


# Check the curve of the model --------------------------------------------

fitted <- function(estimate, x) {
  
  est_0 <- estimate[1]
  est_1 <- estimate[2]
  
  return(est_0 * exp(est_1 * x))
  
}

plot(df$price, df$gross, main = "Initial Model (Red) vs GN Model (Purple)")
x.data <- seq(0, 8000, 1)
lines(x.data, fitted(gn_result$g, x.data), col = "purple")
lines(x.data, fitted(g_vec, x.data), col = "red")

gn_result$g
gn_result$est_iter

# 3 predictors iterative nonlinear regression by Gauss Newton method -------------------

iter_num <- 10
SSE_vec <- rep(NA, iter_num)
est_mat <- matrix(NA, nrow = iter_num, ncol = 3)
length(df$price)

gn <- function(g, x, y) {
  
  # Setting length of vectors
  n <- length(x)
    # Setting the initial value for iteration
  a <- g[1]
  b <- g[2]
  c <- g[3]
  
  # iterative process
  for (i in 1:iter_num) {
    
    # Estimate Y
    est_y <- a + b * exp(c * x)
    # Difference of actual Y and estimated Y
    diff_y <- y - est_y
    # Partial derivative
    D_a <- rep(1, n)
    D_b <- exp(c * x)
    D_c <- b * x * exp(c * x)
    # values to update by regression without intercept (-1)
    update <- coef(lm(diff_y ~ D_a + D_b + D_c -1))
    # Update our parameters by addition
    a <- a + update[1]
    b <- b + update[2]
    c <- c + update[3]
    # SSE for model fit
    SSE <- sum((diff_y)^2)
    
    # arranging for this function's output
    D_iter <- cbind(D_a, D_b, D_c)
    g_iter <- c(a, b, c)
    
    # Saving iterative value
    SSE_vec[i] <- SSE
    est_mat[i, ] <- g_iter
    
  }
  
  return(list(Y = diff_y,
              D = D_iter,
              g = g_iter,
              SSE = SSE,
              SSE_iter = SSE_vec,
              est_iter = est_mat))
}

min(df$gross)
test_vec <- c(3.91, g_vec)
gn_result <- gn(test_vec, df$price, df$gross)
names(gn_result)

gn_result$Y
gn_result$D
gn_result$SSE
gn_result$g
gn_result$SSE_iter
gn_result$est_iter


# Check the curve of the model --------------------------------------------

fitted <- function(estimate, x) {
  
  est_0 <- estimate[1]
  est_1 <- estimate[2]
  est_2 <- estimate[3]
  
  return(est_0 + est_1 * exp(est_2 * x))
  
}

# Graph by base R function
plot(df$price, 
     df$gross, 
     main = "Initial Model (Red) vs GN Model (Purple)",
     xlim = c(0, 4000))
x.data <- seq(0, 8000, 1)
lines(x.data, fitted(gn_result$g, x.data), col = "purple")

gn_result$SSE_iter
gn_result$est_iter

# Display original data
ggplot(df) +
  geom_point(aes(x = price, y = gross))

# Display fitted line
x.df <- data.frame(x = x.data)
ggplot(x.df, aes(x)) +
  stat_function(fun = fitted,
                color = "red",
                args = list(estimate = gn_result$g))
 
# Display data and fitted line
x.df <- data.frame(x = x.data)
ggplot(NULL) +
  stat_function(data = x.df, aes(x),
                fun = fitted,
                args = list(estimate = gn_result$g),
                color = "red",
                size = 1) +
  geom_point(data = df, 
             aes(x = price, y = gross), 
             alpha = 0.5) +
  # xlim(0, 3000) +
  labs(title = "Nonlinear Regression",
       x = "Income property price (10,000 JPY)",
       y = "Yield percent before cost (%)") +
  theme(plot.title = element_text(hjust = 0.5))


# Estimates of Parameters and Least Squares Criterion Measure
iteration <- seq(1, iter_num)
criterion <- data.frame(iteration = iteration,
                        g_0 = gn_result$est_iter[ , 1],
                        g_1 = gn_result$est_iter[ , 2],
                        g_2 = gn_result$est_iter[ , 3],
                        SSE = gn_result$SSE_iter)

round(criterion[criterion$iteration == 5, ], 4)

# G_0
ggplot(criterion, aes(x = iteration, y = g_0)) +
  geom_line(linetype = "dashed") +
  geom_point()

# G_1
ggplot(criterion, aes(x = iteration, y = g_1)) +
  geom_line(linetype = "dashed") +
  geom_point()

# G_2
ggplot(criterion, aes(x = iteration, y = g_2)) +
  geom_line(linetype = "dashed") +
  geom_point()

# SSE
ggplot(criterion, aes(x = iteration, y = SSE)) +
  geom_line(linetype = "dashed") +
  geom_point()


# For blogger -------------------------------------------------------------
a <- ggplot(criterion, aes(x = iteration, y = g_0)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  labs(title = "a",
       x = "Iteration",
       y = "Estimate") +
  xlim(1, 5) +
  theme(plot.title = element_text(hjust = 0.5))

b <- ggplot(criterion, aes(x = iteration, y = g_1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  labs(title = "b",
       x = "Iteration",
       y = "Estimate") +
  xlim(1, 5) +
  theme(plot.title = element_text(hjust = 0.5))
  
c <- ggplot(criterion, aes(x = iteration, y = g_2)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  labs(title = "c",
       x = "Iteration",
       y = "Estimate") +
  xlim(1, 5) +
  theme(plot.title = element_text(hjust = 0.5))

sse <- ggplot(criterion, aes(x = iteration, y = SSE)) +
  geom_line(linetype = "dashed", color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Model Fit (Lower, Better)",
       x = "Iteration",
       y = "SSE") +
  xlim(1, 5) +
  theme(plot.title = element_text(hjust = 0.5))

library(gridExtra)
grid.arrange(a, b, c, sse, ncol = 2)





