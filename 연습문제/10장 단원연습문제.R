library(caret)
library(rpart)
library(randomForest)
library(e1071)
library(class)
library(survival)

# Random Froest
clean_colon=na.omit(colon)
clean_colon$status=factor(clean_colon$status)
str(clean_colon)

set.seed(2021)
data=clean_colon[sample(nrow(colon)), ]
k=5
q=nrow(data) / k
l=1:nrow(data)
accuracy=0
precision <- 0
recall <- 0

for (i in 1:k) {
  test_list <- ((i-1)*q+1) : (i*q)
  data_test <- data[test_list,]
  data_train <- data[-test_list,]
  rf <- randomForest(status~., data_train)
  pred <- predict(rf, data_test, type = 'class')
  t <- table(pred, data_test$status)
  accuracy <- accuracy + (t[1,1]+t[2,2]) / nrow(data_test)
  precision <- precision + t[2,2]/(t[2,1]+t[2,2])
  recall <- recall + t[2,2]/(t[1,2]+t[2,2])
}

rf_avg_acc <- accuracy / k
rf_avg_prec <- precision / k
rf_avg_rec <- recall / k

sprintf('랜덤 포레스트: 정확도=%f',
        rf_avg_acc)





rf <-randomForest(status~rx + sex + age + obstruct + perfor + adhere + 
                 nodes + differ + extent + surg + node4, data = clean_colon)
print(f)
