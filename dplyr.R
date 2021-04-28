library(dplyr)
library(gapminder)
library(ggplot2)

head(mpg)
glimpse(mpg)
summary(mpg)

mpg$total <- (mpg$cty + mpg$hwy)/2
head(mpg)
mean(mpg$total)
summary(mpg$total)
hist(mpg$total)

# 1
mpg %>%
  mutate(displ45=ifelse(displ<=4, 'DISPL4', 'DISPL5')) %>%
  group_by(displ45) %>%
  summarise(avg_hwy=mean(hwy)) %>%
  arrange(desc(avg_hwy))

# 2
mpg %>%
  filter(manufacturer %in% c('audi','toyota')) %>%
  group_by(manufacturer) %>%
  summarize(avg_cty=mean(cty)) %>%
  arrange(desc(avg_cty))

# 3
mpg %>%
filter(manufacturer %in% c('chevrolet', 'ford', 'honda')) %>%
  summarise(avg_hwy=mean(hwy))

# 4
mpg4 <- mpg %>%
  select(class, cty)
head(mpg4)

mpg4 <- mpg %>%
  select(class, cty)
head(mpg4)
# 5
mpg %>%
  filter(class %in% c('suv', 'compact')) %>%
  group_by(class) %>%
  summarise(avg_cty=mean(cty)) %>%
  arrange(desc(avg_cty))

# 6
mpg %>%
  filter(manufacturer='audi') %>%
  arrange(desc(hwy)) %>%
  head(5)

# 7
df <- mpg %>%
  mutate(total=cty+hwy) %>%
head(df)
dfsavg <- dftotal

# 8
mpg %>%
  group_by(class) %>%
  summarise(avg_cty=mean(cty))

# 9
mpg %>%
  group_by(class) %>%
  summarise(avg_cty=mean(cty)) %>%
  arrange(desc(avg_cty))

# 10
mpg %>%
  group_by(manufacturer) %>%
  summarise(avg_hwy=mean(hwy)) %>%
  arrange(desc(avg_hwy)) %>%
  head(3)

# 11 #n() - 행의 갯수
mpg %>%
  filter(class=="compact") %>%
  group_by(manufacturer) %>%
  summarise(num_kind=n()) %>%
  arrange(desc(num_kind))




a <- mpg
