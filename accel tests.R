library(ggplot2)
library(pspline)

pos <- read.csv(file="drivers/1015/189.csv", head=TRUE, sep=",", colClasses=c("numeric", "numeric"))



i_s = c(2,6)
  
data.vel = NULL

#for now, use i=5 for best results
i = 5

#print the x-y data
qplot(pos$x, pos$y)

time_end = length(pos$x) - 1

#for these, x = time, y = vel
pp.velx <- predict(smooth.spline(pos$x, df=time_end/i), deriv=1)
pp.vely <- predict(smooth.spline(pos$y, df=time_end/i), deriv=1)

pp.vel <- sqrt(pp.velx$y^2 + pp.vely$y^2)

data.this_one = data.frame(time = pp.velx$x, vel = pp.vel, i = i)

data.vel = rbind(data.vel, data.this_one)


#plot the velocity data using ggplot
ggplot(data = data.vel, aes(x=time, y=vel, group = i, colour = i)) +geom_line()+ geom_point()

#first need to verify that velocity is good data

#get the acceleration

data.acc = NULL



pp.acc <- predict(smooth.spline(pp.vel, df=time_end/i), deriv=1)

#calcualte the 2nd derivs of position
pp.accx <- predict(smooth.spline(pp.velx$y, df=time_end/i), deriv=1)
pp.accy <- predict(smooth.spline(pp.vely$y, df=time_end/i), deriv=1)

#plot the accel data to see how it looks

pp.acc_sq <- sqrt(pp.accx$y^2 + pp.accy$y^2)

#make the data frame with accel

data.acc_i <- data.frame(time = pp.acc$x, accel = pp.acc$y, group=i)

data.acc <- rbind(data.acc, data.acc_i)


ggplot(data = data.acc, aes(x=time, y=accel, group = group, colour = group)) +geom_line()+ geom_point()

acf(pp.acc$y)
