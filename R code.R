f= function(x){
     return(6*x^3-5*x+3) 
}
ggplot()+
  stat_function(data=data.frame(x=c(-1,5)), aes(x=x), fun=f)

dat=read.csv("dat.csv")

ggplot()+ 
  geom_point(data=dat, aes(x=x, y=y))
lm(y~x, data=dat)
f= function(x){
        return(34.04*x-65.27) 
}
ggplot()+ 
  geom_point(data=dat, aes(x=x, y=y))+
  stat_function(data=data.frame(x=c(-5,15)), aes(x=x), fun=f)
x=dat$x
y=f(x)
means=data.frame(x,y)
ggplot()+ 
  geom_point(data=dat, aes(x=x, y=y))+
  stat_function(data=data.frame(x=c(-5,15)), aes(x=x), fun=f)+
  geom_point(data=means, aes(x=x,y=y), color="red", size=3)
dat$group=1:100
head(dat)
means$group=1:100
groups=rbind(dat, means)
ggplot()+ 
  geom_point(data=dat, aes(x=x, y=y))+
  stat_function(data=data.frame(x=c(-5,15)), aes(x=x), fun=f)+
  geom_point(data=means, aes(x=x,y=y), color="red", size=3)+
  geom_line(data=groups, aes(x=x,y=y, group=group))

lm(y~x+I(x^2), data=dat)
f= function(x){
          return(2.9522*x^2+0.9719*x-0.5685) 
}

means$y=f(means$x) #becoz new f(x)
groups=rbind(dat, means)#means changed, hence need to reassign groups

#regression line of 2nd degree 
ggplot()+ 
  geom_point(data=dat, aes(x=x, y=y))+
  stat_function(data=data.frame(x=c(-5,15)), aes(x=x), fun=f)+
  geom_point(data=means, aes(x=x,y=y), color="red", size=3)+
  geom_line(data=groups, aes(x=x,y=y, group=group))

#regression line of a 3rd degree
lm(y~x+I(x^2)+I(x^3), data=dat)
f= function(x){
          return(0.02024*x^3+2.60142*x^2+2.49528*x-1.67268) 
}

ggplot()+ 
  geom_point(data=dat, aes(x=x, y=y))+
  stat_function(data=data.frame(x=c(-5,15)), aes(x=x), fun=f)
  #geom_point(data=means, aes(x=x,y=y), color="red", size=3)
  #geom_line(data=groups, aes(x=x,y=y, group=group))