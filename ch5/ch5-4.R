# 모델링을 위한 가공
library(dplyr)
library(gapminder)
library(ggplot2)

#
wine <- read.table('data/wine.data.txt', sep=',')

# substr 함수
a <- 'A quick brown fox jumps over the lazy dog.'
nchar(a)
substr(a, 3, 7)
substr(a, nchar(a)-3,nchar(a)-1)


names(wine)[2:14] <- substr(columns, 4, nchar(columns))

# 데이터셋 분할
train_set = sample_frac(wine, 0.75)
str(train_set)
table(wine$Y)
table(train_set$Y)

test_set = setdiff(wine, train_set)
table(test_set$Y)
