library(dplyr)
library(gapminder)
library(ggplot2)

# 현실 세계의 모델링
X = c(3, 6, 9, 12.)
Y = c(3, 4, 5.5, 6.5)
plot(X,Y)

# model 1: y=0.5x1.0
Y1 = 0.5*X + 1.0
Y1

# 평균 제곱 오차: Mean Squared Error
(Y - Y1)**2
sum((Y-Y1)**2)
mes <- sum((Y-Y1)**2) / length(Y)

# model 2: y=2/12x + 7/4
Y2 = 5 * X / 12 + 7/4
Y2
mes2 <- sum((Y - Y2)**2) / length(Y)
mes2


# R의 단순 선별회귀 모델lm
model <- lm(Y ~ X)
model

plot(X,Y)
abline(model, col='red')
fitted(model)
mse_model <- sum((Y - fitted(model)) **2) /length(Y)
mse_model

# 잔차 - Residuals
residuals(model)

# 잔차 제곱합
deviance(model)

# 평균 제곱 오차(MSE)
deviance(model) / length(Y)
summary(model)

# 예측
newx <- data.frame(x=c(1.2, 2.0, 20.65))
newx
newY <- predict(model, newdata=newx)??
