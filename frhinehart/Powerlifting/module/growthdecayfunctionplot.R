growth_decay_curve1 <- function(x, a, b) {
  a * x * exp(-b * x)}


library(ggplot2)


# given b

#b1 = 0.005
x=seq(0,300,by=0.1)
a=c(5, 10, 20, 25)
b=0.005

datx = rep(x,4)
daty = c(growth_decay_curve1(x,a[1],b),growth_decay_curve1(x,a[2],b),growth_decay_curve1(x,a[3],b), growth_decay_curve1(x,a[4],b))
data=rep(a,each=length(x))
dat = cbind(x=datx,y=daty,a=data)

ggplot(dat,aes(x=x,y=y,color=as.factor(a))) + 
  geom_smooth(method="loess",aes(color=as.factor(a)),se=FALSE) 


# b2 = 0.01
x=seq(0,300,by=0.1)
a=c(5, 10, 20, 25)
b=0.01

datx = rep(x,4)
daty = c(growth_decay_curve1(x,a[1],b),growth_decay_curve1(x,a[2],b),growth_decay_curve1(x,a[3],b), growth_decay_curve1(x,a[4],b))
data=rep(a,each=length(x))
dat = cbind(x=datx,y=daty,a=data)

ggplot(dat,aes(x=x,y=y,color=as.factor(a))) + 
  geom_smooth(method="loess",aes(color=as.factor(a)),se=FALSE) 


# b3=0.025
x=seq(0,300,by=0.1)
a=c(5, 10, 20, 25)
b=0.025

datx = rep(x,4)
daty = c(growth_decay_curve1(x,a[1],b),growth_decay_curve1(x,a[2],b),growth_decay_curve1(x,a[3],b), growth_decay_curve1(x,a[4],b))
data=rep(a,each=length(x))
dat = cbind(x=datx,y=daty,a=data)

ggplot(dat,aes(x=x,y=y,color=as.factor(a))) + 
  geom_smooth(method="loess",aes(color=as.factor(a)),se=FALSE) 


#b4=0.04
x=seq(0,300,by=0.1)
a=c(5, 10, 20, 25)
b=0.04

datx = rep(x,4)
daty = c(growth_decay_curve1(x,a[1],b),growth_decay_curve1(x,a[2],b),growth_decay_curve1(x,a[3],b), growth_decay_curve1(x,a[4],b))
data=rep(a,each=length(x))
dat = cbind(x=datx,y=daty,a=data)

ggplot(dat,aes(x=x,y=y,color=as.factor(a))) + 
  geom_smooth(method="loess",aes(color=as.factor(a)),se=FALSE) 
