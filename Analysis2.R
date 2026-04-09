library(dplyr)

data <- read.csv("train.csv")

# Fitting the SLR
model_slr <- lm(SalePrice ~ GrLivArea, data = data)
summary(model_slr)
AIC(model_slr)

# MLR 1
model_mlr1 <- lm(SalePrice ~ GrLivArea + FullBath, data = data)
summary(model_mlr1)
AIC(model_mlr1)

