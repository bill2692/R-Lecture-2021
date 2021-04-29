library(dplyr)
library(gapminder)
library(ggplot2)
library(gridExtra)
library(tidyr)

# 2.
head(midwest)
options(scipen = 10) # 지수 표기를 일반 표기로 변환
mw <- midwest %>% 
  filter(poptotal <= 50000 & popasian <= 10000)
ggplot(mw, aes(x=poptotal, y=popasian, col=state)) + 
  geom_point() + 
  xlim(0,50000) + 
  ylim(0,10000)

# 3.
mpg %>% 
  filter(class=='suv') %>% 
  group_by(manufaturer) %>% 
  summarise(mean_cty=mean(cty)) %>% 
  arrange(desc(mean_cty)) %>% 
  head(5) %>% 
  ggplot(aes(x=reorder(manufacturer, -mean_cty), y=mean))

# 4.
mpg %>% 
  group_by(class) %>% 
  summarise(count(=n)) %>% 
  ggplot(aes(x=reorder(class, -count), y=count, fill=class)) + 
         geom_col() + 
           labs(x='class')

# 5. 
head(economics)
ggplot(economics, aes(date, psavert)) + 
  geom_line()

ggplot(economics) + 
  geom_line(aes(date, psavert)) + 
  geom_line(aes(date, uempmed))

# 6. 
mpg %>% 
  filter(class %in% c('compact', 'suv', 'subcompact')) %>% 
  ggplot(aes(x=class, y = cty, col=class)) + 
  geom_boxplot()

# 7. 
head(diamonds)
str(diamonds)

# 7-1.
diamonds %>% 
  group_by(cut) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=cut, y=count, fill=cut)) + 
  geom_bar()

#################

ggplot(diamonds, aes(x=cut, fill=cut)) + 
  geom_bar()

# 7.2
diamonds %>% 
  group_by(cut) %>% 
  summarise(mean_price = mean(price)) %>% 
  ggplot(aes(cut, mean_price, fill=cut)) + 
  geom_col()

# 7.3
#color에 따른 가격 변화
diamonds %>% 
  group_by(color) %>% 
  summarise(mean_price = mean(price)) %>% 
  ggplot(aes(color, mean_price, fill=color)) + 
  geom_col()
# cut, color에 따른 분포
ggplot(diamonds, aes(x=price)) + 
  geom_histogram(bins=5) + 
  facet_wrap(~cut + color)
