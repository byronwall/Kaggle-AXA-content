library("ggplot2")  # this library is needed
library("zoo")

# analyze_driver <- function(driver){

driver = 1133
trips_to_vis = 1:200

trips=NULL
dens_info = NULL
acf_info = NULL
vel_info = NULL
acc_shift_info = NULL

trip_data = NULL

plot_rad_data = NULL

rad_dens_all = NULL

acc_acf_all = NULL
vel_acf_all = NULL

acc_deriv_all = NULL


dirPath = paste0("drivers/", driver, "/")
for(k in trips_to_vis)
{
  pos = read.csv(paste0(dirPath, k, ".csv"), header=TRUE, sep=",", colClasses=c("numeric", "numeric"))
  
  #for now, use i=5 for best results
  i = 5
  
  time_end = length(pos$x) - 1
  
  #for these, x = time, y = vel
  pp.velx <- predict(smooth.spline(pos$x, df=time_end/i), deriv=1)
  pp.vely <- predict(smooth.spline(pos$y, df=time_end/i), deriv=1)    
  pp.vel <- sqrt(pp.velx$y^2 + pp.vely$y^2)
  
  #work on acceleration info
  pp.acc <- predict(smooth.spline(pp.vel, df=time_end/i), deriv=1)  
  pp.acc_deriv <- predict(smooth.spline(pp.acc, df=time_end/i), deriv=1)
  
  
  
  pp.radius = (1 + pp.vel^2)^(3/2) / abs(pp.acc$y)
  pp.radius[pp.radius > 10000] = NA
  
  pp.acc_cent = pp.vel^2 / pp.radius
  
  #acf(pp.acc$y)
  vel0 = pp.vel > 2
  
  
  
  #this section being skipped since only velocity plots are desired
  if(sum(vel0)>2){
    
    #create records for teh deriv of acceleration
    pp.acc_deriv_dens = density(pp.acc_deriv$y[vel0], n=21, from=-0.5, to=0.5, na.rm=TRUE)  
    acc_deriv_all = rbind(acc_deriv_all, data.frame(x=pp.acc_deriv_dens$x, dens=pp.acc_deriv_dens$y, trip=k))
    
    #combine the acceleration info
    trips <- rbind(trips, data.frame(time=pp.acc$x, acc=pp.acc$y, trip=k))    
    
    #calculate density info for acc
    dens_acc = density(pp.acc$y[vel0], from=-2, to = 2, n=21)
    dens_info <- rbind(dens_info, data.frame(x=dens_acc$x, y=dens_acc$y, trip=k))
    
    #calculate density info for acc
    dens_vel = density(pp.vel[vel0], from=2, to=35, n=20)
    vel_info <- rbind(vel_info, data.frame(x=dens_vel$x, y=dens_vel$y, trip=k))
    
    #create a data frame for the different pieces
    pp.dist = cumsum(pp.vel)
    pp.dist.grab = seq(from=0, to=length(pp.dist), length.out = 5)
    #       trip = data.frame(t(dens_acc$y))#, t(pp.dist[pp.dist.grab]))
    trip = data.frame(t(dens_vel$y), t(dens_acc$y), t(pp.dist[pp.dist.grab]))
    
    trip_data = rbind(trip_data, trip)
    
    plot_rad = data.frame(time = pp.velx$x, radius = pp.radius, trip=k)
    plot_rad_data = rbind(plot_rad_data, plot_rad)
    
    rad_dens = density(pp.acc_cent[vel0], from=0, to=0.4, n=20, na.rm=T)
    rad_dens_all = rbind(rad_dens_all, data.frame(acc=rad_dens$x, val=rad_dens$y, trip=k))
    
    acc_acf = acf(pp.acc$y, lag.max = 10, plot=FALSE)
    acc_acf_all = rbind(acc_acf_all, data.frame(lag=acc_acf$lag, acf=acc_acf$acf, trip=k))
    
    vel_acf = acf(pp.vel[vel0], lag.max = 10, plot=FALSE)
    vel_acf_all = rbind(vel_acf_all, data.frame(lag=vel_acf$lag, acf=vel_acf$acf, trip=k))
  }
  else{
    #this line exists to catch routes with no velocity. these are really garbage trips
    trip_data = rbind(trip_data, setNames(data.frame(t(rep(0,length(trip_data)))), names(trip_data)))
  }
}



plot_radius = F
plot2 = F
plot4 = F
plot_trip = F

plot_acc_deriv_all = F
if(plot_acc_deriv_all){
  #   png(paste0("accdens_plots/", driver, "_acc_dens.png"), width=1400, height=1000)
  
  p2 = ggplot(acc_deriv_all, aes(x, dens, colour=trip, group=trip)) +
    geom_point(size=0.01) + geom_line()+
    ggtitle("acc_deriv for all trips")
  
  plot(p2)
  plot(p2+ylim(0,10))
  
  #   dev.off()
}

plot_vel_acf = T
if(plot_vel_acf){
  #   png(paste0("accdens_plots/", driver, "_acc_dens.png"), width=1400, height=1000)
  
  p2 = ggplot(vel_acf_all, aes(lag, acf, colour=trip, group=trip)) +
    geom_point(size=0.01) + geom_line()+
    ggtitle("velocity acf values")
  
  plot(p2)
  
  #   dev.off()
}

plot_acc_acf = T
if(plot_acc_acf){
  #   png(paste0("accdens_plots/", driver, "_acc_dens.png"), width=1400, height=1000)
  
  p2 = ggplot(acc_acf_all, aes(lag, acf, colour=trip, group=trip)) +
    geom_point(size=0.01) + geom_line()+
    ggtitle("acc acf values")
  
  plot(p2)
  
  #   dev.off()
}

plot_acc_cent = F
if(plot_acc_cent){
  #   png(paste0("accdens_plots/", driver, "_acc_dens.png"), width=1400, height=1000)
  
  p2 = ggplot(rad_dens_all, aes(acc, val, colour=trip, group=trip)) +
    geom_point(size=0.01) + geom_line()+
    ggtitle("acc_cent density") + ylim(0,10)
  
  plot(p2)
  
  #   dev.off()
}

if(plot_trip){
  #   png(paste0("accdens_plots/", driver, "_acc_dens.png"), width=1400, height=1000)
  
  p2 = ggplot(pos, aes(x, y)) +
    geom_point(size=0.01) + geom_line()+
    ggtitle("acc density plots") #+ ylim(0,5)
  
  plot(p2)
  
  #   dev.off()
}


if(plot_radius){
#   png(paste0("accdens_plots/", driver, "_acc_dens.png"), width=1400, height=1000)
  
  p2 = ggplot(plot_rad_data, aes(time, radius, colour=trip, group=trip)) +
    geom_point(size=0.01) + geom_line()+
    ggtitle("acc density plots") + ylim(0,1000)
  
  plot(p2)
  
#   dev.off()
}

if(plot2){
  #png(paste0("accdens_plots/", driver, "_acc_dens.png"), width=1400, height=1000)
  
  p2 = ggplot(dens_info, aes(x, y, colour=trip, group=trip)) +
    geom_point(size=0.01) + geom_line()+
    ggtitle("acc density plots") + ylim(0,5)
  
  plot(p2)
  
  #dev.off()
}

if(plot4){
  png(paste0("veldens_plots/", driver, "_vel_dens.png"), width=1400, height=1000)
  
  p2 = ggplot(vel_info, aes(x, y, colour=trip, group=trip)) +
    geom_point(size=0.01) + geom_line()+
    ggtitle("vel density plots")
  
  plot(p2)
  
  dev.off()
}