library(ggplot2)
library(pspline)

pos <- read.csv(file="drivers/1011/140.csv", head=TRUE, sep=",", colClasses=c("numeric", "numeric"))

i_s = c(5,8,11)
  
i = 5

data.vel = NULL

xlowlim = 0
xhilim = 500

time_end = length(pos$x) - 1

#for these, x = time, y = vel
pp.velx <- predict(smooth.spline(pos$x, df=time_end/i), deriv=1)
pp.vely <- predict(smooth.spline(pos$y, df=time_end/i), deriv=1)

pp.vel <- sqrt(pp.velx$y^2 + pp.vely$y^2)

data.this_one = data.frame(time = pp.velx$x, vel = pp.vel, i = i)

data.vel = rbind(data.vel, data.this_one)

#plot the velocity data using ggplot
ggplot(data = data.vel, aes(x=time, y=vel, group = i, colour = i)) +geom_line()+ geom_point() 

#calculate the heading based on angle of vector

pp.heading$angle2 <- pp.heading$angle
indices <- pp.heading$angle2 < 0
pp.heading$angle2[indices] <- 2*3.14159 + pp.heading$angle2[indices]

qplot(pp.heading$time, pp.heading$angle) #+ xlim(xlowlim,xhilim)
qplot(pp.heading$time, pp.heading$angle2)#+ xlim(xlowlim,xhilim)

acfdata <- acf(pp.vel[pp.vel>4], lag.max=30)