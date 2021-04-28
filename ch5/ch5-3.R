library(dplyr)
library(gapminder)
library(ggplot2)
avocado <- read.csv('data/avocado.csv')
str(avocado)
(x_avg = avocado %>% 
  group_by(region) %>%
  summarize(V_avg = mean(`Total Volume`), P_avg = mean(AveragePrice)))
(x_avg = avocado %>% 
    group_by(region, year) %>%
    summarize(V_avg = mean(`Total Volume`), P_avg = mean(AveragePrice)))
(x_avg = avocado %>% 
    group_by(region, year, type) %>%
    summarize(V_avg = mean(`Total Volume`), P_avg = mean(AveragePrice)))
x_avg %>%
  filter(region != "TotalUS") %>%
  ggplot(aes(year, V_avg, col = type)) + geom_line() + facet_wrap(~region)

# 연도별이 아닌 월별 집계
library(lubridate)
year("2021-04-26")
month("2021-04-26")
day("2021-04-26")
wday("2021-04-26")

m_avg <- avocado %>% 
    group_by(region, year, month(Date), type) %>%
    summarize(V_avg = mean(`Total Volume`), P_avg = mean(AveragePrice))
head(m_avg)

