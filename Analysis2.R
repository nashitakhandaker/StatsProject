library(dplyr)

data <- read.csv("train.csv")

# Fitting the SLR
model_slr <- lm(SalePrice ~ GrLivArea, data = data)
summary(model_slr)
AIC(model_slr)

# SLR Kaggle score
test <- read.csv("test.csv")
score <- predict(model_slr, newdata = test)
score

# Getting the score by submitting to Kaggle
submission <- data.frame(
  Id = test$Id,
  SalePrice = predictions
)
write.csv(submission, "analysis2slrscore.csv", row.names = FALSE)

# MLR 1
model_mlr1 <- lm(SalePrice ~ GrLivArea + FullBath, data = data)
summary(model_mlr1)
AIC(model_mlr1)

