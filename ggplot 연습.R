library(dplyr)
library(gapminder)
library(ggplot2)


# 그래프 그리기
ggplot(anscombe) +
  geom_point(aes(x1, y1),color='darkorange', size=3) + 
  scale_x_continuous(breaks = seq(2, 20, 2)) + 
  scale_y_continuous(breaks = seq(2, 14, 2)) + 
  xlim(2, 20) + 
  ylim(2, 14) + 
  geom_abline(slope = 0.5, intercept = 3, 
              color='conflowerblue')
  
ggplot(anscombe) + 
  geom_point(aes(x1, y1),color='darkorange', size=3) + 
  scale_x_continuous(breaks = seq(2, 20, 2)) + 
  scale_y_continuous(breaks = seq(2, 14, 2)) + 
  xlim(2, 20) + 
  ylim(2, 14) + 
  geom_abline(slope = 0.5, intercept = 3, 
              color='cornflowerblue', size=1) + 
  labs(title = 'Dataset 1')

p1 <- ggplot(anscombe) +
  geom_point(aes(x1, y1), color='darkorange', size=3) + 
  xlim(2, 20) + 
  ylim(2, 14) + 
  geom_abline(slope = 0.5, intercept = 3, 
              color='cornflowerblue', size=1) + 
  labs(title = 'Dataset 1')

p2 <- ggplot(anscombe) +
  geom_point(aes(x2, y2), color='darkorange', size=3) + 
  xlim(2, 20) + 
  ylim(2, 14) + 
  geom_abline(slope = 0.5, intercept = 3, 
              color='cornflowerblue', size=1) + 
  labs(title = 'Dataset II')

p3 <- ggplot(anscombe) +
  geom_point(aes(x3, y3), color='darkorange', size=3) + 
  xlim(2, 20) + 
  ylim(2, 14) + 
  geom_abline(slope = 0.5, intercept = 3, 
              color='cornflowerblue', size=1) + 
  labs(title = 'Dataset III')


p4 <- ggplot(anscombe) +
  geom_point(aes(x4, y4), color='darkorange', size=3) + 
  xlim(2, 20) + 
  ylim(2, 14) + 
  geom_abline(slope = 0.5, intercept = 3, 
              color='cornflowerblue', size=1) + 
  labs(title = 'Dataset IV')

grid.arrange(p1, p2, p3, p4, ncol=2, top="Anscomde's Quartet")

library(gridExtra)

figures <- list()
figures <- append(figures, p1)
figures <- append(figures, p2)
figures <- append(figures, p3)
figures <- append(figures, p4)
figures[1]

# source Refactoring
x <- ggplot(anscombe) +
  geom_point(aes(x4, y4), color='darkorange', size=3)
m1 <- x

assign(paste('m', 4, sep='.'), x)


for (i in 1:4) {
  x <- ggplot(anscombe) +
    geom_point(aes(anscombe[,i], anscombe[,i-4]), color='darkorange', size=3) + 
    xlim(2, 20) + 
    ylim(2, 14) + 
    geom_abline(slope = 0.5, intercept = 3, 
                color='cornflowerblue', size=1) + 
    labs(title = 'Dataset', i)
         x=paste0('x', i), y=paste0('y',i))
assign(paste('m',i,sep = '.'), x)
}
anscombe[,i]
anscombe[,5]
assign(paste('m',i,sep = '.'), x)
