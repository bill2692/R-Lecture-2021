library(dplyr)
library(gapminder)
library(ggplot2)

# Galton 선형회귀
choose.files()
read.csv("C:\\Workspace\\R\\Galtons Height Data.csv")

df <- read.csv("C:\\Workspace\\R\\Galtons Height Data.csv")
head(df)

galton <- df %>% 
  filter(Gender=='M') %>% 
  mutate(Father=2.54*Father, son=2.54*Height)
head(galton)

ggplot(galton, aes(Father, son)) + 
  geom_point(position = 'jitter', color='darkorange')

model <- lm(son~Father, data=galton)
coef(model)
ggplot(galton, aes(Father,son)) + 
  geom_point(position = 'jitter', color='darkorange') + 
  geom_abline(intercept = coef(model)[1], slope = coef(model)[2],
              color='darkblue', size=1)
summary(model)
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))