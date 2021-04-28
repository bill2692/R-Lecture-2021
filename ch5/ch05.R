# base R을 이용한 데이터 가공
library(gapminder)
library(dplyr)
library(dplyr)
glimpse(gapminder)

library(gapminder)
library(dplyr)
glimpse(gapminder)

# 각 나라의 기대 수명(LifeExp)
gapminder[, c("country", "lifeExp")]
gapminder[, c("country", "lifeExp", "year")]

# 샘플과 속성의 추출
gapminder[1000:1009, c("country", "lifeExp", "year")]
gapminder[gapminder$country=='Croatia',]
gapminder[gapminder$country=='Croatia',c('year', 'pop')]

# 크로아티아의 1990년도 이후의 연도, 기대수명과 인구
gapminder[gapminder$country=='Croatia' & gapminder$year>1990, c("year", "lifeExp", "pop")]

# 행/열 단위의 연산
apply(gapminder[gapminder$country=='Croatia', c('lifeExp', 'pop')], 2, mean)

cagr = function(x,year)
peak2peak = function(x) { + return(max(x) - min(x))}

# mpg(mile-per-gallon)
library(vctrs)
library(ggplot2)
install.packages("ggplot2")
library(ggplot2)
head(mpg)
gapminder %>%
  filter(continent=='Asia') %>%
  group_by(country) %>%
  summarise(life_avg=mean(lifeExp)) %>%
  arrange(desc(life_avg)) %>%
  head(5)
