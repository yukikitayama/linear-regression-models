
# rakumachi_waseda_ridge_regression ---------------------------------------

setwd("C:/Users/yukic/Documents/Data")
df <- readRDS("rakumachi_waseda_20181221.rds")
str(df)

# Exclusion ---------------------------------------------------------------
# exclude NA data since unable to calculate correlation coef
sum(is.na(df$age))
df <- df[is.na(df$age) == FALSE, ]

# Make correlated variables -----------------------------------------------
sum(is.na(df$month))
sum(is.na(df$year))
df$total_month <- (2018 - df$year) * 12 + (6 - df$month)
head(df)

# Check correlation -------------------------------------------------------

cor(df$age, df$total_month)
cor(df$year, df$total_month)
cor(df[ , c("price", "age", "station", "space", "floor")])
cor(df[ , c("price", "year", "total_month")])
pairs(df[ , c("price", "year", "total_month")])

# Run regression ----------------------------------------------------------

# model.1 <- lm(price ~ age, data = df)
model.1 <- lm(price ~ year, data = df)
model.2 <- lm(price ~ total_month, data = df)
# model.3 <- lm(price ~ age + total_month, data = df)
model.3 <- lm(price ~ year + total_month, data = df)



summary(model.1)
summary(model.2)
summary(model.3)

# Ridge process -----------------------------------------------------------

n <- nrow(df)
Z1 <- (df$year - mean(df$year))/(sqrt(n-1)*sd(df$year)) 
Z2 <- (df$total_month - mean(df$total_month))/(sqrt(n-1)*sd(df$total_month)) 
Y.star <- (df$price-mean(df$price))/(sqrt(n-1)*sd(df$price))
  
# Standardized design matrix 
X.star <- cbind(Z1,Z2)
t(X.star) %*% X.star

# Standardized ridge estimate
c <- .01
p <- 2
beta.R.hat <- solve(t(X.star) %*% X.star + diag(rep(c, p))) %*% t(X.star) %*% Y.star

# Reverse transform the model
beta.1 <- beta.R.hat[1]*(sd(df$price)/sd(df$year))
beta.2 <- beta.R.hat[2]*(sd(df$price)/sd(df$total_month))
beta.0 <- mean(df$price)-(beta.1*mean(df$year)+beta.2*mean(df$total_month))

c(beta.0, beta.1, beta.2)
coef(model.3)

# Construct loop for biasing constants ------------------------------------
# for loop for biasing constants (c) to estimate various ridge estimate

c_vec <- seq(0, 1, .1)
beta.1_vec <- rep(NA, length(c_vec))
beta.2_vec <- rep(NA, length(c_vec))
beta.0_vec <- rep(NA, length(c_vec))
# beta.1_vif_vec <- rep(NA, length(c_vec)) 
# beta.2_vif_vec <- rep(NA, length(c_vec)) 
vif_mat <- matrix(NA, nrow = length(c_vec), ncol = 2)

# Standardized design matrix 
n <- nrow(df)
Z1 <- (df$year - mean(df$year))/(sqrt(n-1)*sd(df$year)) 
Z2 <- (df$total_month - mean(df$total_month))/(sqrt(n-1)*sd(df$total_month)) 
X.star <- cbind(Z1,Z2)
Y.star <- (df$price-mean(df$price))/(sqrt(n-1)*sd(df$price))

for (i in 1:length(c_vec)) {

  # Standardized ridge estimate
  c <- c_vec[i]
  p <- 2
  beta.R.hat <- solve(t(X.star) %*% X.star + diag(rep(c, p))) %*% t(X.star) %*% Y.star

  # Reverse transform the model
  beta.1 <- beta.R.hat[1]*(sd(df$price)/sd(df$year))
  beta.2 <- beta.R.hat[2]*(sd(df$price)/sd(df$total_month))
  beta.0 <- mean(df$price)-(beta.1*mean(df$year)+beta.2*mean(df$total_month))

  # Saving different ridge parameter estimate
  beta.1_vec[i] <- beta.1
  beta.2_vec[i] <- beta.2
  beta.0_vec[i] <- beta.0
  
  # Saving VIF
  vif_mat[i, ] <- diag(solve(cor(df[ , c("year", "total_month")]) + c))

}

# beta.1_vec
plot(c_vec, beta.1_vec, type="l")
# beta.2_vec
plot(c_vec, beta.2_vec, type="l")
# beta.0_vec
plot(c_vec, beta.0_vec, type="l")
# vif_mat
rownames(vif_mat) <- c_vec
plot(c_vec, vif_mat[ ,1], type="l")

# Deciding the ridge solution at c = 0.3
which(c_vec == 0.3)
vif_mat[which(c_vec == 0.3), ]
beta.1_vec[which(c_vec == 0.3)]
beta.2_vec[which(c_vec == 0.3)]
beta.0_vec[which(c_vec == 0.3)]

which(c_vec > 0.2 & c_vec < 0.4)
vif_mat[which(c_vec > 0.2 & c_vec < 0.4), ]
beta.1_vec[which(c_vec > 0.2 & c_vec < 0.4)]
beta.2_vec[which(c_vec > 0.2 & c_vec < 0.4)]
beta.0_vec[which(c_vec > 0.2 & c_vec < 0.4)]

# Graphs for blog
result <- data.frame(c = c_vec,
                     Year = beta.1_vec,
                     Total_month = beta.2_vec,
                     VIF = vif_mat[ ,1])
result


# Check r squared for ordinary and ridge --------------------------------------------------------------------

# ordinary
round(summary(model.3)$r.squared, 3)

# ridge
beta.0_final <- beta.0_vec[4]
beta.1_final <- beta.1_vec[4]
beta.2_final <- beta.2_vec[4]
beta.vec_final <- c(beta.0_final, beta.1_final, beta.2_final)
design_mat <- cbind(rep(1, n), df$year, df$total_month)
y.hat_vec <- design_mat %*% beta.vec_final
sum((y.hat_vec - mean(df$price))^2) / sum((df$price - mean(df$price))^2)


# summary for blog --------------------------------------------------------
library(car)
vif(model.3)
round(coef(model.3),2)
round(beta.vec_final,2)

plot(df$year, df$price)
mean(df$total_month)
median(df$total_month)

# Comparing prediction
x_vec <- seq(1970, 2018, 1)

# ordinary
pred.1 <- coef(model.3)[1] + coef(model.3)[2] * x_vec + coef(model.3)[3] * 190
plot(df$year, df$price)
lines(x_vec, pred.1)
slope_o <- coef(model.3)[2]
intercept_o <- coef(model.3)[1] + coef(model.3)[3] * 190

# ridge
pred.2 <- beta.vec_final[1] + beta.vec_final[2] * x_vec + beta.vec_final[3] *190
plot(df$year, df$price)
lines(x_vec, pred.2)
slope_r <- beta.vec_final[2]
intercept_r <- beta.vec_final[1] + beta.vec_final[3] *190

# length(pred.2)
# pred_df <- data.frame(year = c(x_vec, x_vec),
#                       prediction = c(pred.1, pred.2),
#                       type = c(rep("Original regression", length(pred.1)), 
#                                rep("Ridge regression", length(pred.2))))

library(ggplot2)                    
ggplot(df) +
  geom_point(aes(x = year, y = price)) +
  labs(title = "Original (Blue) and Ridge (Red) Regressions",
       x = "Built year",
       y = "Property price") +
  geom_abline(intercept = intercept_r, 
              slope = slope_r, 
              size = 1,
              color = "red") +
  geom_abline(intercept = intercept_o, 
              slope = slope_o, 
              size = 1,
              color = "blue")

(2018 - 2002)*12

# Manual calculation of VIF ------------------------------------------

r_1_sq <- summary(lm(df$year ~ df$total_month))$r.squared
vif_1 <- 1 / (1-r_1_sq)
r_2_sq <- summary(lm(df$total_month ~ df$year))$r.squared
vif_2 <- 1 / (1-r_2_sq)
c(vif_1, vif_2)

# Calculating VIF by using R function
library(car)
vif(model.3)

cor(df$year, df$total_month)
solve(cor(df[ , c("year", "total_month")]))
solve(cor(df[ , c("year", "total_month")]) + 0.01)
solve(cor(df[ , c("year", "total_month")]) + 0.02)
solve(cor(df[ , c("year", "total_month")]) + 0.03)
solve(cor(df[ , c("year", "total_month")]) + 0.1)
solve(cor(df[ , c("year", "total_month")]) + 0.5)
solve(cor(df[ , c("year", "total_month")]) + 1)
diag(solve(cor(df[ , c("year", "total_month")]) + 0.1))

model_3pred <- lm(price ~ age + space + floor, data = df)
library(car)
vif(model_3pred)
diag(solve(cor(df[ , c("age", "space", "floor")])))
lm.b1 <- lm(age ~ space + floor, data = df)
vif.1_3pred <-  1 / (1 - summary(lm.b1)$r.squared)
lm.b2 <- lm(space ~ age + floor, data = df)
vif.2_3pred <-  1 / (1 - summary(lm.b2)$r.squared)
lm.b3 <- lm(floor ~ space + age, data = df)
vif.3_3pred <-  1 / (1 - summary(lm.b3)$r.squared)
c(vif.1_3pred, vif.2_3pred, vif.3_3pred)
vif(model_3pred)


# Calculating VIF by R function -------------------------------------------

library(car)
length(vif(model.3))
model.vif <- lm(age ~ total_month, data = df)
summary(model.vif)

install.packages("ridge")
library(ridge)
model.ridge <- linearRidge(price ~ age + total_month, data = df, lambda = 0.01)
vif(model.ridge)

install.packages("lmridge")
library(lmridge)
model.lmridge <- lmridge(price ~ age + total_month, data = df, lambda = 0.01)
beta.1_vec[2]
beta.2_vec[2]

install.packages("genridge")
library(genridge)
df.y <- df[ , "price"]
df.x <- data.matrix(df[ , c("age", "total_month")])
lridge <- ridge(df.y, df.x, lambda = 0.02)


longley.X <- data.matrix(longley[, c(2:6,1)])
head(longley,1)
head(longley.X, 1)

