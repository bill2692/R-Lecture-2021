library(dplyr)
library(gapminder)
library(ggplot2)
library(gridExtra)

# 1. mpg 데이터의 cty(도시 연비)와 hwy(고속도로 연비). x축은 cty, y축은 hwy로 된 산점도

head(mpg)
mpg %>% 
  ggplot(aes(x=cty,y=hwy,col='red'))+
  geom_point()

# 2. 

head(midwest)

midwest %>% 
  select(poptotal,popasian) %>% 
  ggplot(aes(poptotal,popasian, size=0.5))+
  geom_point()+
  xlim(0,5e+06)+
  ylim(0,1e+05)

# 3. 
mpg <- as.data.frame((ggplot2::mpg))

df <- mpg %>% 
  filter(class == "suv") %>% 
  group_by(manufacturer) %>% 
  summarise(mean.cty = mean(hwy)) %>% 
  arrange(desc(mean.cty)) %>% 
  head(5)
df
ggplot(df, aes(x = reorder(manufacturer, -mean.cty), y = mean.cty)) + geom_col()

# 4. 
ggplot(mpg, aes(x = class)) + geom_bar()

# 5. 
head(economics)
x = economics

  ggplot(x, aes(date, psavert)) + 
    geom_line()

 economics %>% 
    ggplot(aes(date,psavert))+geom_line(linetype="solid",color="orange",size=1)
  
# 6. 
head(mpg)
x
x <- mpg %>% 
  filter(class=='compact'& class== 'suv', class=='subcompact') %>% 
  group_by(manufacturer)
x <- mpg %>% 
  filter(class=='compact', class=='suv', class=='subcompact') %>% 
  group_by(manufacturer) %>% 
  summarise(aes(compact, suv, subcompact))
x <- mpg

ggplot(x, aes(compact, subcompact, suv, cty)) + 
  geom_boxplot()

m_c <- mpg %>% 
  filter(class %in% c("compact","subcompact","suv"))
m_c %>% 
  ggplot(aes(class, cty, col=class)) +
  geom_boxplot()

mpg %>% 
  filter(class=='compact'| class=='subcompact'| class=='suv') %>% 
  ggplot(aes(class,cty))+
  geom_boxplot()
