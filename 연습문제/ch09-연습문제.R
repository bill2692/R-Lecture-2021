library(rpart)
ucla <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
str(ucla)
## DT
ucla$admit=factor(ucla$admit)

# 모델만들기
dtc = rpart(admit~., data=ucla)

# 결정 트리
par(mfrow=c(1,1), xpd=NA)
plot(dtc)
text(dtc, use.n = T)

#예측
pred = predict(dtc, ucla, type = 'class')
table(pred, ucla$admit)

# 평가
library(caret)
confusionMatrix(pred, ucla$admit)

# 훈련/테스트 셋으로 분리하여 시행
set.seed(2021)
ucla_index <- sample(1:nrow(ucla), 0.8*nrow(ucla))
ucla_train <- ucla[ucla_index, ]
ucla_test <- ucla[setdiff(1:nrow(ucla), ucla_index), ]

dim(ucla_train)
dim(ucla_test)
table(ucla_train$admit)
table(ucla_test$admit)

# 모델링
dtc <- rpart(admit~., ucla_train)

# 예측
pred <- predict(dtc, ucla_test, type = 'class')

# 평가
confusionMatrix(pred, ucla_test$admit)


## rF
library(randomForest)

set.seed(2021)
train_index <- createDataPartition(ucla$admit, p=0.8, list=F)
ucla_train <- ucla[train_index,]
ucla_test <- ucla[-train_index,]

# 모델링
rF <- randomForest(admit~., ucla)
print(rF)

# 예측
pred <- predict(rF, ucla_test, type='class')

# 평가
confusionMatrix(pred, ucla_test$admit)

## svm
library(caret)
library(e1071)

# 모델
svc <- svm(admit~., ucla_train)

# 예측
pred <- predict(svc, ucla_test, type='class')

# 평가
confusionMatrix(pred, ucla_test$admit)

## K-NN
library(class)
k <- knn(ucla_train[2:4], ucla_test[2:4],
         ucla_train$admit, k=5)
ucla_test$admit
confusionMatrix(k, ucla_test$admit)

# WINE data
getwd()
wine <- readLines('wine.name.txt')
str(wine)
dtc <- rpart(Alcohol~., data = wine)
# 모델만들기
dtc = rpart(admit~., data=ucla)

# 결정 트리
par(mfrow=c(1,1), xpd=NA)
plot(dtc)
text(dtc, use.n = T)

# wine data의 columns
names(wine)
names(wine)[2:14] <- columns
names(wine)[2:14] <- substr(columns,4,nchar(columns))
names(wine)[1] <- "y"
str(wine)
