# 시각화 도구
library(dplyr)
library(gapminder)
library(ggplot2)

# 1. plot
head(cars)
plot(cars,type='p', main='cars') # p-point, main-title
plot(cars,type='l', main='cars') # l-line
plot(cars,type='b', main='cars') # b-both point and line
plot(cars,type='h', main='cars') # h- 히스토그램과 같은 막대그래프


x = gapminder %>% 
  filter(year == 1952 & continent == "Asia") %>% 
  mutate(gdp = gdpPercap*pop) %>% 
  select(country, gdp) %>%  # 나라만
  arrange(desc(gdp)) %>% #gdp내림차순 순서대로
  head()

pie(x$gdp, x$country)
barplot(x$gdp, names.arg = x$country)


x=gapminder %>% 
  filter(year==2007 & continent=='Asia') %>% 
  mutate(gdp=gdpPercap*pop) %>% 
  select(country, gdp) %>% 
  arrange(desc(gdp)) %>% 
  head()

pie(x$gdp, x$country)
barplot(x$gdp, names.arg = x$country)

matplot(iris[1:4], type='l')
legend('topleft', names(iris)[1:4], lty = c(1, 2, 3, 4), col = c(1, 2, 3, 4))

hist(cars$speed)
hist(iris$Sepal.Length)


ggplot(gapminder, aes(x=gdpPercap, y=lifeExp, col=continent)) + 
  geom_point(alpha=0.2)





gapminder %>% 
  filter(year==2007) %>% 
  ggplot(aes(lifeExp, col=continent)) + 
  geom_histogram()

gapminder %>% 
  filter(year==2007) %>% 
  ggplot(aes(lifeExp, col=continent)) + 
  geom_histogram(position = 'dodge')

# 2. geom_boxplot
gapminder %>% 
  filter(year==2007) %>% 
  ggplot(aes(continent, lifeExp, col=continent)) + 
  geom_boxplot()

# 3. scale_x_log10, scale_x_log10
ggplot(gapminder, aes(gdpPercap, lifeExp, col=continent)) + 
  geom_point(alpha=0.2)
ggplot(gapminder, aes(gdpPercap, lifeExp, col=continent)) + 
  geom_point(alpha=0.2) + 
  scale_x_log10()

# 4. coord_flip
gapminder %>% 
  filter(continent=='Africa') %>% 
  ggplot(aes(country, lifeExp)) + 
  geom_bar(stat='identity')

gapminder %>% 
  filter(continent=='Africa') %>% 
  ggplot(aes(country, lifeExp, col=country)) + 
  geom_bar(stat='identity') + 
  coord_flip()

# 5. scale_fill_brewer()
library(RColorBrewer)
display.brewer.all()

# 기본 팔레트
gapminder %>% 
  filter(lifeExp > 70) %>% 
  group_by(continent) %>% 
  summarise(n=n_distinct(country)) %>% 
  ggplot(aes(x=continent, y=n)) + 
  geom_bar(stat = 'identity', aes(fill=continent))

gapminder %>% 
  filter(lifeExp > 70) %>% 
  group_by(continent) %>% 
  summarise(n=n_distinct(country)) %>% 
  ggplot(aes(x=continent, y=n)) + 
  geom_bar(stat = 'identity', aes(fill=continent)) + 
scale_fill_brewer(palette = 'Spectral')

gapminder %>% 
  filter(lifeExp > 70) %>% 
  group_by(continent) %>% 
  summarise(n=n_distinct(country)) %>% 
  ggplot(aes(x=continent, y=n)) + 
  geom_bar(stat = 'identity', aes(fill=continent)) + 
  scale_fill_brewer(palette = 'Blues')

gapminder %>% 
  filter(lifeExp > 70) %>% 
  group_by(continent) %>% 
  summarise(n=n_distinct(country)) %>% 
  ggplot(aes(x=continent, y=n)) + 
  geom_bar(stat = 'identity', aes(fill=continent)) + 
  scale_fill_brewer(palette = 'Oranges')

gapminder %>% 
  filter(lifeExp > 70) %>% 
  group_by(continent) %>% 
  summarise(n=n_distinct(country)) %>% 
  ggplot(aes(x=continent, y=n)) + 
  geom_bar(stat = 'identity', aes(fill=continent)) + 
  scale_fill_brewer(palette = 'RdBu')

# 그래프 표시 순서
gapminder %>% 
  filter(lifeExp > 70) %>% 
  group_by(continent) %>% 
  summarise(n=n_distinct(country)) %>% 
  ggplot(aes(x=reorder(continent, -n), y=n)) + 
  geom_bar(stat = 'identity', aes(fill=continent)) + 
  scale_fill_brewer(palette = 'Blues')
