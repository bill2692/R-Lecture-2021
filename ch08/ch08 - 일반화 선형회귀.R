# 일반화 선형 모델
# 로지스틱 회귀

ucla <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
str(ucla)

lr <- glm(admit~., data=ucla, family=binomial) # 모델만들기
coef(lr)

test <- data.frame(gre=(376), gpa=c(3.6), rank=c(3)) # 예측하기
predict(lr, test, type = 'response') # 평가하기

# ucla 데이터 셋 train/test data set으로 분할
train_index <- sample(1:nrow(ucla), 0.8*nrow(ucla))
test_index <- setdiff(1:nrow(ucla), train_index)
ucla_train <- ucla[train_index,]
ucla_test <- ucla[test_index,]
dim(ucla_train)
dim(ucla_test)

# 분할 비율은 적절한가?
table(ucla$admit) # 127/400 -> 0.3175
table(ucla_train$admit) # 101/320 -> 0.3156

# 훈련 데이터셋으로 학습, 데스트 데이터셋으로 예측
lr <- glm(admit~., ucla_train, family = binomial())
pred <- predict(lr, ucla_test, type = 'response')
pred
ucla_test$admit
