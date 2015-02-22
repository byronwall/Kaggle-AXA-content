library(ggplot2)
library(pspline)

pos <- read.csv(file="drivers/1011/67.csv", head=TRUE, sep=",", colClasses=c("numeric", "numeric"))

i=10
time_end = length(pos$x) - 1

qplot(pos$x, pos$y)

#for these, x = time, y = vel
pp.velx <- predict(smooth.spline(pos$x, df=time_end/i), deriv=1)
pp.vely <- predict(smooth.spline(pos$y, df=time_end/i), deriv=1)

pp.vel <- sqrt(pp.velx$y^2 + pp.vely$y^2)

qplot(pp.velx$x, pp.velx$y)
qplot(pp.vely$x, pp.vely$y)
qplot(pp.velx$x, pp.vel)

#plot the velocity data using ggplot
ggplot(data = data.frame(x = pp.velx$x, y = pp.vel), aes(x=x, y=y)) + geom_line()

#try to get all the velocities onto one plot
data.vel <- cbind(pp.velx$x, pp.velx$y, c("vel_x"))

#use rbind to stick more rows on
data.vel <- rbind(data.vel, cbind(pp.vely$x, pp.vely$y, c("vel_y")))
data.vel <- rbind(data.vel, cbind(pp.vely$x, pp.vel, c("vel")))

data.vel <- data.frame(data.vel)

# Basic line graph with points
ggplot(data=data.vel, aes(x=V1, y=pp.vel, group=V3, colour=V3)) + geom_line()

#get the acceleration
pp.acc <- predict(smooth.spline(pp.vel, df=time_end/i), deriv=1)

#calcualte the 2nd derivs of position
pp.accx <- predict(smooth.spline(pp.velx$y, df=time_end/i), deriv=1)
pp.accy <- predict(smooth.spline(pp.vely$y, df=time_end/i), deriv=1)

#make plots of the acceleration variables
#qplot(pp.accx$x, pp.accx$y)
#qplot(pp.accy$x, pp.accy$y)
#qplot(pp.acc$x, pp.acc$y)

#calc radius of curvature info
#R = [(x..^2 + y..^2)^3/2] / (x.y.. - y.x..)

radius_ep = 1e-6;
pp.radius <- abs(( (pp.accx$y^2 + pp.accy$y^2 )^(3/2) ) / (pp.velx$y * pp.accy$y - pp.vely$y * pp.accx$y))
pp.cent_acc <- pp.vel^2 / (pp.radius + radius_ep)

#filter out bad values
pp.rad_thresh = 2
pp.radius[pp.radius>pp.rad_thresh] <- pp.rad_thresh

pp.cent_acc_threshold = 1e5
pp.cent_acc[pp.cent_acc > pp.cent_acc_threshold ] = pp.cent_acc_threshold

#qplot(pp.velx$x, pp.radius)
#qplot(pp.velx$x, pp.cent_acc)