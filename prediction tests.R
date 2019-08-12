
library("readr")
library("dplyr")
library("plotly")
library("ggplot2")
library("htmlwidgets")
library("crosstalk")
library("forecast")

x1 <-1:100
x2 <- tibble(x=1:200)
y1 <- x1^2+x1*5*rnorm(100)
y2<-2*(x1*(0.5+runif(100)))^2
y3<-3*(x1*(0.5+runif(100)))^2
y4<-4*(x1*(0.5+runif(100)))^2

dat<-tibble(x=x1,y1=y1,y2=y2,y3=y3,y4=y4)
m1<- lm(y1~poly(x,2,raw=T),data = dat)
summary(m1)
p <- 
  plot_ly()%>%
  add_markers(x=~x1,y=~y1)%>%
  add_lines(x=x2$x, y=as.data.frame(predict(m1,x2,interval = "confidence"))$fit)%>%
  layout(
    xaxis = list(range = c(0, 200))
  )%>%
add_ribbons(
  
  x=tibble(x=101:200)$x,
  ymin = as.data.frame(predict(m1,tibble(x=101:200),interval = "confidence"))$lwr,
  ymax = as.data.frame(predict(m1,tibble(x=101:200),interval = "confidence"))$upr,
  line = list(color = 'rgba(7, 164, 181, 0.05)'),
  fillcolor = 'rgba(7, 164, 181, 0.2)',
  name = "Standard Error")

p
p2<-plot_ly()%>%
  add_ribbons(
    
    x=dat$x1,
    ymin =dat$y1,
    ymax =dat$y2,
    line = list(color = 'rgba(7, 164, 181, 0.05)'),
    fillcolor = 'rgba(7, 164, 181, 0.2)',
    name = "Standard Error")
  
p2


