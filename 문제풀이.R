library(dplyr)
library(gapminder)
library(ggplot2)



gapminder$pop
head(gapminder)
1-2
x=gapminder %>% 
  filter(year==1952) %>% 
  select(country, pop) %>% 
  arrange(desc(pop)) %>% 
  head()
pie(x$pop, x$country)
barplot(x$pop, names.arg=x$country)

1-2
for (i in seq(1952, 2007, 5)) {
  print(i)
  x=gapminder %>% 
    filter(year==i) %>% 
    select(country, pop) %>% 
    arrange(desc(pop)) %>% 
    head()
  pie(as.numeric(x$pop), x$country)
  barplot(x$pop, names.arg=x$country)
  title(i)
}

# 2
library(tidyr)
# 2-1 airquality
head(airquality)
air_tidy <- gather(airquality, key = 'Measure', value = 'Value', 
                   -Day, -Month)

head(air_tidy)
tail(air_tidy)
dim(airquality)
dim(air_tidy)

air_tidy %>% 
  ggplot(aes(Day, Value, col=Measure)) + 
  geom_point() + 
  facet_wrap(~Month)  # ~Month - Month로 묶어라

# 2-2 iris
iris_tidy <- gather(iris, key = 'feat', value = 'value', 
                    -Species)
head(iris_tidy)
tail(iris_tidy)
iris_tidy %>% 
  ggplot(aes(feat, value, col=Species)) + 
  geom_point(position = 'jitter') # jitter - 약간의 효과

# 3
library(gridExtra)
seto <- filter(iris, Species=='setosa')
vers <- filter(iris, Species=='versicolor')
virg <- filter(iris, Species=='virginica')

seto_s <- seto %>% 
  ggplot(aes(Sepal.Length, Sepal.Width, col=Species)) + 
  geom_point()
seto_p <- seto %>% 
  ggplot(aes(Petal.Length, Petal.Width, col=Species)) + 
  geom_point()
vers_s <- vers %>% 
  ggplot(aes(Sepal.Length, Sepal.Width, col=Species)) + 
  geom_point()
vers_p <- vers %>% 
  ggplot(aes(Petal.Length, Petal.Width, col=Species)) + 
  geom_point()
virg_s <- virg %>% 
  ggplot(aes(Sepal.Length, Sepal.Width, col=Species)) + 
  geom_point()
virg_p <- virg %>% 
  ggplot(aes(Petal.Length, Petal.Width, col=Species)) + 
  geom_point()

grid.arrange(seto_s,seto_p,vers_s,vers_p,virg_s,virg_p,ncol=2)

# 3-2
#barplot + legend
seto_mean <- apply(iris[iris$Species=='setosa',1:4],2,mean)
vers_mean <- apply(iris[iris$Species=='versicolor',1:4],2,mean)
virg_mean <- apply(iris[iris$Species=='virginica',1:4],2,mean)
mean_of_iris <- rbind(seto_mean, vers_mean, virg_mean)
mean_of_iris

barplot(mean_of_iris, beside = T,
        main = '품종별 평균', yilm=c(0.8), col=c('red', 'green', 'blue'))
legend('topright', 
       legend = c('setosa','versicolor', 'virginica'), 
       fill = c('red', 'green', 'blue'))

# ggplot
df <- iris %>% 
  group_by(Species) %>% 
  summarise(Sepal.Length=mean(Sepal.Length), Sepal.Width=mean(Sepal.Width), 
            Petal.Length=mean(Petal.Length), Petal.Width=mean(Petal.Width))

df
df_tidy <- gather(df, key ='Feature', value = 'Value', -Species)
df_tidy
ggplot(df_tidy, aes(x=Feature, y=Value, fill=Species)) + 
  geom_bar(stat = 'identity')

ggplot(df_tidy, aes(x=Feature, y=Value, fill=Species)) + 
  geom_bar(stat = 'identity', position = 'dodge')

# 3-3
# boxplot



품종별로 Sepal/Petal의 Length, Width 산점도 그리기

head(iris)
iris[, c('Species')] 
iris[iris$Species=='setosa' , ]
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_point(alpha=0.2)



x <- iris[iris$Species=='setosa' , ]
ggplot(x, aes(x=Sepal.Length, y=Sepal.Width, col=Species)) + 
  geom_point(alpha=0.2)
ggplot(iris$Species=='setosa', aes(x=Petal.Length, y=Petal.Width)) + 
  geom_point(alpha=0.2)

ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, col=Species)) + 
  geom_point(alpha=0.2)

ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, col=Species)) + 
  geom_point(alpha=0.2)

ggplot(iris, aes(x=Sepal.Length, y=Petal.Width, col=Species)) + 
  geom_point(alpha=0.2)

ggplot(iris, aes(x=Sepal.Width, y=Petal.Length, col=Species)) + 
  geom_point(alpha=0.2)

ggplot(iris, aes(x=Sepal.Width, y=Petal.Width, col=Species)) + 
  geom_point(alpha=0.2)

ggplot(iris, aes(x=Petal.Length, y=Petal.Width, col=Species)) + 
  geom_point(alpha=0.2)