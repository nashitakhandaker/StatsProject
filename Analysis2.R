library(dplyr)
library(boot)
library(lmtest)
library(car)

# Load data
data <- read.csv("train.csv")
test <- read.csv("test.csv")

## Create log variables 
data$logSalePrice <- log(data$SalePrice)
data$logGrLivArea <- log(data$GrLivArea)

test$logGrLivArea <- log(test$GrLivArea)


## SLR MODEL

## Clean data (to not get NaN values due to missing values)
clean_slr <- na.omit(data[, c("logSalePrice", "logGrLivArea")])

# Fitting the SLR (LM for summary + Adjusted R^2)
model_slr_lm <- lm(logSalePrice ~ logGrLivArea, data = clean_slr)
summary(model_slr_lm)   
AIC(model_slr_lm)

# Checking SLR model assumptions
par(mfrow = c(2,2))
plot(model_slr_lm)

# Normality test
shapiro.test(residuals(model_slr_lm))

# Homoscedasticity test
bptest(model_slr_lm)

# Independence
durbinWatsonTest(model_slr_lm)

# Influential points
cooksd_slr <- cooks.distance(model_slr_lm)
which(cooksd_slr > (4 / nrow(clean_slr)))

# SLR CV
slr_form <- logSalePrice ~ logGrLivArea
cv_result_slr <- cv.glm(clean_slr, glm(slr_form, data = clean_slr), K = 10)
cv_result_slr$delta

# PRESS
cv_press_slr <- cv_result_slr$delta[1] * nrow(clean_slr)
cv_press_slr

# SLR Kaggle score (convert BACK from log)
pred_log_slr <- predict(model_slr_lm, newdata = test)
predictions_slr <- exp(pred_log_slr)

# Getting the score by submitting to Kaggle
submission_slr <- data.frame(
  Id = test$Id,
  SalePrice = predictions_slr
)
write.csv(submission_slr, "analysis2_slr.csv", row.names = FALSE)


# numerical -> categorical for MLR var
data$OverallQual <- factor(data$OverallQual)
test$OverallQual <- factor(test$OverallQual)

## MLR MODEL 1

# Clean data (to not get NaN values due to missing values)
clean_mlr1 <- na.omit(data[, c("logSalePrice", "logGrLivArea", "FullBath")])

# Fitting the MLR 1
model_mlr1_lm <- lm(logSalePrice ~ logGrLivArea + FullBath, data = clean_mlr1)
summary(model_mlr1_lm)   
AIC(model_mlr1_lm)

# Checking MLR 1 assumptions
par(mfrow = c(2,2))
plot(model_mlr1_lm)

# Normality test
shapiro.test(residuals(model_mlr1_lm))

# Homoscedasticity test
bptest(model_mlr1_lm)

# Independence
durbinWatsonTest(model_mlr1_lm)

# Multicollinearity
vif(model_mlr1_lm)

# Influential points
cooksd_mlr1 <- cooks.distance(model_mlr1_lm)
which(cooksd_mlr1 > (4 / nrow(clean_mlr1)))

# MLR 1 CV 
mlr1_form <- logSalePrice ~ logGrLivArea + FullBath
cv_result_mlr1 <- cv.glm(clean_mlr1, glm(mlr1_form, data = clean_mlr1), K = 10)
cv_result_mlr1$delta

# PRESS
cv_press_mlr1 <- cv_result_mlr1$delta[1] * nrow(clean_mlr1)
cv_press_mlr1

# MLR 1 Kaggle score 
pred_log_mlr1 <- predict(model_mlr1_lm, newdata = test)
predictions_mlr1 <- exp(pred_log_mlr1)

# Getting the score by submitting to Kaggle
submission_mlr1 <- data.frame(
  Id = test$Id,
  SalePrice = predictions_mlr1
)
write.csv(submission_mlr1, "analysis2_mlr1.csv", row.names = FALSE)

## MLR MODEL 2

# Clean data
clean_mlr2 <- na.omit(data[, c("logSalePrice", "logGrLivArea", "FullBath", "OverallQual")])

# Fitting the MLR 2
model_mlr2_lm <- lm(logSalePrice ~ logGrLivArea + FullBath + OverallQual, data = clean_mlr2)
summary(model_mlr2_lm)
AIC(model_mlr2_lm)

# Checking assumptions
par(mfrow = c(2,2))
plot(model_mlr2_lm)

# Normality test
shapiro.test(residuals(model_mlr2_lm))

# Homoscedasticity test
bptest(model_mlr2_lm)

# Independence
durbinWatsonTest(model_mlr2_lm)

# Multicollinearity
vif(model_mlr2_lm)

# Influential points
cooksd_mlr2 <- cooks.distance(model_mlr2_lm)
which(cooksd_mlr2 > (4 / nrow(clean_mlr2)))

# Model evaluation
# MAE
mae_value_mlr2 <- mean(abs(residuals(model_mlr2_lm)))
mae_value_mlr2
# RMSE
pred_mlr2 <- predict(model_mlr2_lm, newdata = clean_mlr2)
rmse_mlr2 <- sqrt(mean((clean_mlr2$logSalePrice - pred_mlr2)^2))
rmse_mlr2

# cv
mlr2_form <- logSalePrice ~ logGrLivArea + FullBath + OverallQual
cv_result_mlr2 <- cv.glm(clean_mlr2, glm(mlr2_form, data = clean_mlr2), K = 10)
cv_result_mlr2$delta

# PRESS
cv_press_mlr2 <- cv_result_mlr2$delta[1] * nrow(clean_mlr2)
cv_press_mlr2

# Kaggle
pred_log_mlr2 <- predict(model_mlr2_lm, newdata = test)
predictions_mlr2 <- exp(pred_log_mlr2)

submission_mlr2 <- data.frame(
  Id = test$Id,
  SalePrice = predictions_mlr2
)

write.csv(submission_mlr2, "analysis2_mlr2.csv", row.names = FALSE)

