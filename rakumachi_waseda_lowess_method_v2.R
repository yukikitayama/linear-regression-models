
# Lowess method -----------------------------------------------------------

setwd("C:/Users/yukic/Documents/Data")
df <- readRDS("rakumachi_waseda_20181221.rds")
str(df)

# Exclusion ---------------------------------------------------------------
# exclude NA data since unable to calculate correlation coef
sum(is.na(df$age))
df <- df[is.na(df$age) == FALSE, ]

library(ggplot2)
a <- ggplot(df, aes(x = space, y = price)) +
  geom_point() +
  labs(title = "Space") +
  theme(plot.title = element_text(hjust = 0.5))

b <- ggplot(df, aes(x = age, y = price)) +
  geom_point() +
  labs(title = "Age") +
  theme(plot.title = element_text(hjust = 0.5))

a
b

library(gridExtra)
grid.arrange(a, b, ncol = 2)

# lowess method for the entire region -------------------------------------
# X_1: space, X_2:age
# Initialize parameters for lowess method
q <- 0.5
n <- dim(df)[1]
sd_x_1 <- sd(df$space)
sd_x_2 <- sd(df$age)

summary(df$space)
summary(df$age)
# Define the prediction range based on summary statistics
x_h_1_vec <- seq(10, 90, 1)
x_h_2_vec <- seq(5, 50, 1)
x_1_length <- length(x_h_1_vec)
x_2_length <- length(x_h_2_vec)
y_hat_vec <- rep(NA, x_2_length)
y_hat_mat <- matrix(NA, 
                    nrow = x_1_length, 
                    ncol = x_2_length)

for (j in 1:x_1_length) {

  x_h_1 <- x_h_1_vec[j]
  
  # For loop to estimate price for each value in the x vectors
  for (i in 1:x_2_length) {
    
    # each value for lowess method
    x_h_2 <- x_h_2_vec[i]
    
    #lowess method
    d <- sqrt(((df$space - x_h_1)/sd_x_1)^2 +
                ((df$age - x_h_2)/sd_x_2)^2)
    d_sorted <- sort(d)
    # n: number of observation, q: 0.5 neighborhood ratio
    w <- ifelse(d > d_sorted[n*q], 
                0, 
                (1 - (d / d_sorted[n*q])^3)^3)
    model_lowess <- lm(price ~ space + age, data = df, weights = w)
    x.data <- data.frame(space = x_h_1,
                         age = x_h_2)
    y.hat_lowess <- predict(model_lowess, newdata = x.data)
    
    # Save result
    y_hat_vec[i] <- y.hat_lowess
  }

  # Save result
  y_hat_mat[j, ] <- y_hat_vec
  
}
rownames(y_hat_mat) <- x_h_1_vec
colnames(y_hat_mat) <- x_h_2_vec
dim(y_hat_mat)
y_hat_mat[1:6, 1:6]
# Vertical: Space
# Horizontal: Age

# Check lowess result for space
plot(df$space, df$price)
median(df$age)
lines(x_h_1_vec, y_hat_mat[ , "17"])

# Check lowess result for age
plot(df$age, df$price, xlim = c(5, 50), ylim = c(0, 5000))
median(df$space)
lines(x_h_2_vec, y_hat_mat["20", ])


# Plot for blog -----------------------------------------------------------

median(df$age)
space_df <- data.frame(space = x_h_1_vec,
                       est_price = y_hat_mat[ , "17"])
head(space_df)
library(ggplot2)
ggplot(NULL) +
  geom_point(data = df,
             aes(x = space, y = price)) +
  geom_line(data = space_df,
            aes(x = space, y = est_price),
            color = "purple",
            size = 1,
            alpha = 0.7) +
  labs(title = "Space plot conditioning on age") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = 25, y = 6000, 
           label = "Age = 17 (Median)")

median(df$space)
age_df <- data.frame(age = x_h_2_vec,
                     est_price = y_hat_mat["23", ])
ggplot(NULL) +
  geom_point(data = df,
             aes(x = age, y = price)) +
  geom_line(data = age_df,
            aes(x = age, y = est_price),
            color = "green",
            size = 1,
            alpha = 0.7) +
  labs(title = "Age plot conditioning on space") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = 35, y = 5000, 
           label = "Space = 23 (Median)")


# Compare with ordinary least squares regression
model_ordinary <- lm(price ~ age + space, data = df)

# Case when x2 = 20
x.data <- data.frame(age = x_h_1_vec,
                     space = x_h_2_vec[2])
y.hat_ordinary <- predict(model_ordinary, newdata = x.data)
plot(df$age, df$price, xlim = c(5, 50))
lines(x_h_1_vec, y.hat_ordinary)
lines(x_h_1_vec, y_hat_mat[ , "20"], col = "red")

# Case when x2 = 30
x.data <- data.frame(age = x_h_1_vec,
                     space = x_h_2_vec[3])
y.hat_ordinary <- predict(model_ordinary, newdata = x.data)
plot(df$age, df$price, xlim = c(5, 50))
lines(x_h_1_vec, y.hat_ordinary)
lines(x_h_1_vec, y_hat_mat[ , "30"], col = "red")
