library(ISLR2)
library(boot)

data(Auto)
str(Auto)
summary(Auto)

lm_fit <- glm(mpg ~ horsepower + weight + year,
              data   = Auto,
              family = gaussian)

summary(lm_fit)

#Mean squared error
mse_cost <- function(observed, predicted) {mean((observed - predicted)^2)}

set.seed(6)    
K <- 10

cv_result <- cv.glm(data   = Auto,
                    glmfit = lm_fit,
                    cost   = mse_cost,
                    K      = K)

#cross validated test error?
cv_mse  <- cv_result$delta[1]
cv_rmse <- sqrt(cv_mse)

cat("Estimated", K, "fold CV MSE for mpg:", round(cv_mse, 3), "\n")
cat("Estimated", K, "fold CV RMSE for mpg:", round(cv_rmse, 3), "\n")

#compare to training set
pred_train <- predict(lm_fit, type = "response")
train_mse  <- mean((Auto$mpg - pred_train)^2)
train_rmse <- sqrt(train_mse)

cat("Training MSE for mpg:",  round(train_mse, 3), "\n")
cat("Training RMSE for mpg:", round(train_rmse, 3), "\n")


