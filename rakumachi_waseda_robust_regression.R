
# Robust Regression -------------------------------------------------------

setwd("C:/Users/yukic/Documents/Data")
df <- readRDS("rakumachi_waseda_20181221.rds")
str(df)

# Exclusion ---------------------------------------------------------------
# exclude NA data since unable to calculate correlation coef
sum(is.na(df$age))
df <- df[is.na(df$age) == FALSE, ]


# Finding a good example --------------------------------------------------

# plot(df$age, df$price)
# model.1 <- lm(price ~ age, data = df)
# abline(model.1, col = "purple")
# model.1.1 <- lm(price ~ age + I(age^2), data = df) 
# x_vec <- seq(0, 50, .1)
# x_data <- data.frame(age = x_vec)
# y_vec <- predict(model.1.1, newdata = x_data)
# lines(x_vec, y_vec, col = "green")
# 
# library(dplyr)
# mean <- df %>%
#   group_by(age) %>%
#   summarise(mean_space = mean(space)) %>%
#   arrange(desc(age))
# mean
# plot(mean$age, mean$mean_space)
# lines(mean$age, mean$mean_space)
# dim(mean)

df.2 <- df[df$age < 15, ]
# dim(df.2)
# plot(df.2$age, df.2$price)
model.2 <- lm(price ~ age, data = df.2)
# abline(model.2, col = "purple")

library(ggplot2)
ggplot(df.2) +
  geom_point(aes(x = age, y = price)) +
  geom_abline(intercept = coef(model.2)[1], slope = coef(model.2)[2], color = "blue") +
  labs(x = "Years spent since built", 
       y = "Listing price (10,000 JPY)", 
       title = "Scatter Plot of Income Properties in Waseda")

# plot(df$space, df$price)
# model.2 <- lm(price ~ space, data = df)
# abline(model.2, col = "purple")

# plot(df$floor, df$price)
# model.3 <- lm(price ~ floor, data = df)
# abline(model.3, col = "purple")


# Display influential observations ----------------------------------------

dffit_value <- dffits(model.2)
dffit <- data.frame(index = seq(1:41),
                    dffits = dffit_value)
n <- dim(dffit)[1]
p <- 2
ggplot(dffit) +
  geom_point(aes(x = index, y = dffits)) +
  geom_hline(yintercept = 2 * sqrt(p/n), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = -2 * sqrt(p/n), linetype = "dashed", color = "blue")

dffit_value[dffit_value>1]
df[c(28, 89), ]


# Trial IRLS Robust Model -------------------------------------------------------

dim(df.2)
robust <- lm(price ~ age, data = df.2)
e <- residuals(robust)
median_obs <- median(abs(e - median(e)))
# MAD: Median Absolute Deviation
mad_constant <- 0.6745
MAD <- median_obs / mad_constant
scaled_residual <- e / MAD


huber_constant <- 1.345
huber.loss <- function(u) {
  
  return(ifelse(abs(u) <= huber_constant, 1, 
                huber_constant / abs(u)))
  
}

weight.1 <- huber.loss(scaled_residual)

# Regression with weights
robust.2 <- lm(price ~ age, data = df.2, weights = weight.1)

# Display robust regression line
ggplot(df.2) +
  geom_point(aes(x = age, y = price)) +
  geom_abline(intercept = coef(robust.2)[1], slope = coef(robust.2)[2], color = "red") +
  geom_abline(intercept = coef(model.2)[1], slope = coef(model.2)[2], color = "blue")

# Check coefficient change
coef(model.2)
coef(robust.2)
coef(robust.2) / coef(model.2)

# Iteration 2
e.2 <- residuals(robust.2)
median_obs.2 <- median(abs(e.2 - median(e.2)))
# MAD: Median Absolute Deviation
MAD.2 <- median_obs.2 / mad_constant
scaled_residual.2 <- e.2 / MAD.2
weight.2 <- huber.loss(scaled_residual.2)

# Regression with weights
robust.3 <- lm(price ~ age, data = df.2, weights = weight.2)

# Check coefficient change
coef(model.2)
coef(robust.2)
coef(robust.3)
coef(robust.2) / coef(model.2)
coef(robust.3) / coef(robust.2)

# Display robust regression line
ggplot(df.2) +
  geom_point(aes(x = age, y = price)) +
  geom_abline(intercept = coef(robust.2)[1], slope = coef(robust.2)[2], color = "red") +
  geom_abline(intercept = coef(model.2)[1], slope = coef(model.2)[2], color = "blue") +
  geom_abline(intercept = coef(robust.3)[1], slope = coef(robust.3)[2], color = "green")

# Check weight change
weight.1
weight.2

mean(weight.2 / weight.1)

# Iteration 3
e.3 <- residuals(robust.3)
median_obs.3 <- median(abs(e.3 - median(e.3)))
# MAD: Median Absolute Deviation
MAD.3 <- median_obs.3 / mad_constant
scaled_residual.3 <- e.3 / MAD.3
weight.3 <- huber.loss(scaled_residual.3)

# Regression with weights
robust.4 <- lm(price ~ age, data = df.2, weights = weight.3)

# Check coefficient change
coef(model.2)
coef(robust.2)
coef(robust.3)
coef(robust.4)
coef(robust.2) / coef(model.2)
coef(robust.3) / coef(robust.2)
coef(robust.4) / coef(robust.3)

# Display robust regression line
ggplot(df.2) +
  geom_point(aes(x = age, y = price)) +
  geom_abline(intercept = coef(robust.2)[1], slope = coef(robust.2)[2], color = "red") +
  geom_abline(intercept = coef(model.2)[1], slope = coef(model.2)[2], color = "blue") +
  geom_abline(intercept = coef(robust.3)[1], slope = coef(robust.3)[2], color = "green") +
  geom_abline(intercept = coef(robust.4)[1], slope = coef(robust.4)[2], color = "yellow")

# Check weight change
weight.1
weight.2
weight.3

mean(weight.2 / weight.1)
mean(weight.3 / weight.2)

# Iterative Reweighted Least Squares Robust Regression --------------------------------------
# First don't care about conversion
# Make 10 iterating process

# Initialized values for iteration
MAD_constant   <- 0.6745
Huber_constant <- 1.345
iter           <- 15
robust         <- lm(price ~ age, data = df.2)
iter_model     <- robust

# Vectors to be saved in each iteration
n          <- dim(df.2)[1]
weight_mat <- matrix(NA, nrow = iter, ncol = n)
beta_0     <- rep(NA, iter)
beta_1     <- rep(NA, iter)
r_squared  <- rep(NA, iter)
SSE_vec    <- rep(NA, iter) 

# The function to be used in the iteration
huber.loss <- function(u) {
  
  return(ifelse(abs(u) <= Huber_constant, 
                1, 
                Huber_constant / abs(u)))
  
}

for (i in 1:iter) {
  
  e <- residuals(iter_model)
  median_obs <- median(abs(e - median(e)))
  # MAD: Median Absolute Deviation
  MAD <- median_obs / MAD_constant
  scaled_residual <- e / MAD
  weight <- huber.loss(scaled_residual)
  
  # Regression with weights
  iter_model <- lm(price ~ age, data = df.2, weights = weight)
  price.hat <- fitted(iter_model)
  SSE <- sum((df.2$price - price.hat)^2)
  
  # Store results
  weight_mat[i, ] <- weight
  beta_0[i]       <- coef(iter_model)[1]
  beta_1[i]       <- coef(iter_model)[2]
  r_squared[i]    <- summary(iter_model)$r.squared
  SSE_vec[i]      <- SSE
  
}

# beta_0
# beta_1
weight_vec <- apply(weight_mat, 1, mean)
result <- data.frame(iteration = seq(1, iter, 1),
                     beta_0 = beta_0,
                     beta_1 = beta_1,
                     weight = weight_vec,
                     r_square = r_squared,
                     SSE = SSE_vec)

a <- ggplot(result) +
  geom_line(aes(x = iteration, y = beta_0)) +
  labs(title = "Intercept", y = "Coefficient")
b <- ggplot(result) +
  geom_line(aes(x = iteration, y = beta_1)) +
  labs(title = "Slope", y = "Coefficient")
c <- ggplot(result) +
  geom_line(aes(x = iteration, y = weight)) +
  labs(title = "Weight", y = "Mean of weight")
d <- ggplot(result) +
  geom_line(aes(x = iteration, y = SSE)) +
  labs(title = "Error", y = "SSE")

# install.packages("gridExtra")
library(gridExtra)
grid.arrange(a, b, c, d, ncol = 2)

# Display robust regression line
ggplot(df.2) +
  geom_point(aes(x = age, y = price)) +
  geom_abline(intercept = coef(model.2)[1], 
              slope = coef(model.2)[2], 
              color = "blue",
              size = 1,
              alpha = 0.5,
              linetype = 2) +
  geom_abline(intercept = beta_0[10], 
              slope = beta_1[10], 
              color = "red",
              size = 1,
              alpha = 0.5) +
  labs(x = "Years spent since built", 
       y = "Listing price (10,000 JPY)", 
       title = "Income Properties in Waseda and Regression Results")


# weight_mat[10, ]

# For blogger

model <- lm(price ~ age, data = df.2)
e <- residuals(model)
median_obs <- median(abs(e - median(e)))
# MAD: Median Absolute Deviation
mad_constant <- 0.6745
MAD <- median_obs / mad_constant
scaled_residual <- e / MAD

