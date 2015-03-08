library("ggplot2")  # this library is needed
library("zoo")

# analyze_driver <- function(driver){

driver = 1140
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

vel_dens_cum_all = NULL
acc_dens_cum_all = NULL
acc2_dens_cum_all = NULL
centr_dens_cum_all = NULL
turns_dens_cum_all = NULL


dirPath = paste0("drivers/", driver, "/")
for(k in trips_to_vis)
{
  pos = read.csv(paste0(dirPath, k, ".csv"), header=TRUE, sep=",", colClasses=c("numeric", "numeric"))
  
  #for now, use i=5 for best results
  i = 5
  
  time_end = length(pos$x) - 1
  
  posx.spline = smooth.spline(pos$x, df=time_end/i)
  posy.spline = smooth.spline(pos$y, df=time_end/i)
  
  velx.deriv = predict(posx.spline, deriv=1)
  vely.deriv = predict(posy.spline, deriv=1)    
  
  pp.vel = sqrt(velx.deriv$y^2 + vely.deriv$y^2)
  
  #work on acceleration info
  
  accx.deriv = predict(posx.spline, deriv=2)
  accy.deriv = predict(posy.spline, deriv=2)    
  
  pp.acc = (velx.deriv$y * accx.deriv$y + vely.deriv$y * accy.deriv$y)/pp.vel
  
  accx.2deriv = predict(posx.spline, deriv=3)
  accy.2deriv = predict(posy.spline, deriv=3)    
  
  pp.acc_mag = sqrt(accx.deriv$y^2 + accy.deriv$y^2)
  pp.acc_deriv = (accx.deriv$y * accx.2deriv$y + accy.deriv$y * accy.2deriv$y)/pp.acc_mag
  
  
  
  pp.radius = ((velx.deriv$y^2 + vely.deriv$y^2) ^ (3/2)) / 
    abs(velx.deriv$y * accy.deriv$y - vely.deriv$y * accx.deriv$y)
  #pp.radius[pp.radius > 10000] = NA
  
  pp.acc_cent = pp.vel^2 / pp.radius
  
  #adding in heading/turn info
  heading = atan2(velx.deriv$y, vely.deriv$y) * 180 / pi
  turns = diff(heading)
  
  #acf(pp.acc$y)
  vel0 = pp.vel > 2
  
  
  
  #this section being skipped since only velocity plots are desired
  if(sum(vel0)>2){
    
    #create records for teh deriv of acceleration
    pp.acc_deriv_dens = density(pp.acc_deriv[vel0], n=21, from=-0.5, to=0.5, na.rm=TRUE)  
    acc_deriv_all = rbind(acc_deriv_all, data.frame(x=pp.acc_deriv_dens$x, dens=pp.acc_deriv_dens$y, trip=k)) 
    
    #calculate density info for acc
    dens_acc = density(pp.acc[vel0], from=-2, to = 2, n=21)
    dens_info <- rbind(dens_info, data.frame(x=dens_acc$x, y=dens_acc$y, trip=k))
    
    #calculate density info for acc
    dens_vel = density(pp.vel[vel0], from=2, to=35, n=20)
    vel_info <- rbind(vel_info, data.frame(x=dens_vel$x, y=dens_vel$y, trip=k))
  
    #2015 03 06 - added in the cumulative viz
    vel_cdf = ecdf(pp.vel[vel0])
    vel_cdf_seq = seq(0,35,length.out=15)
    vel_dens_cum = data.frame(vel = vel_cdf_seq, cum = vel_cdf(vel_cdf_seq), trip=k)
    vel_dens_cum_all = rbind(vel_dens_cum_all, vel_dens_cum)
    
    acc_cdf = ecdf(pp.acc[vel0])
    acc_cdf_seq = seq(-2,2,length.out=15)
    acc_dens_cum = data.frame(acc = acc_cdf_seq, cum = acc_cdf(acc_cdf_seq), trip=k)
    acc_dens_cum_all = rbind(acc_dens_cum_all, acc_dens_cum)
    
    acc2_cdf = ecdf(pp.acc_deriv[vel0])
    acc2_cdf_seq = seq(-1,1,length.out=15)
    acc2_dens_cum = data.frame(acc2 = acc2_cdf_seq, cum = acc2_cdf(acc2_cdf_seq), trip=k)
    acc2_dens_cum_all = rbind(acc2_dens_cum_all, acc2_dens_cum)
    
    centr_cdf = ecdf(pp.acc_cent[vel0])
    centr_cdf_seq = seq(0,2.5,length.out=15)
    centr_dens_cum = data.frame(centr = centr_cdf_seq, cum = centr_cdf(centr_cdf_seq), trip=k)
    centr_dens_cum_all = rbind(centr_dens_cum_all, centr_dens_cum)
    
    turns_cdf = ecdf(turns[vel0])
    turns_cdf_seq = seq(-30,30,length.out=15)
    turns_dens_cum = data.frame(turn = turns_cdf_seq, cum = turns_cdf(turns_cdf_seq), trip=k)
    turns_dens_cum_all = rbind(turns_dens_cum_all, turns_dens_cum)
    
    #create a data frame for the different pieces
    pp.dist = cumsum(pp.vel)
    pp.dist.grab = seq(from=0, to=length(pp.dist), length.out = 5)
    #       trip = data.frame(t(dens_acc$y))#, t(pp.dist[pp.dist.grab]))
    trip = data.frame(t(dens_vel$y), t(dens_acc$y), t(pp.dist[pp.dist.grab]))
    
    trip_data = rbind(trip_data, trip)
    
    plot_rad = data.frame(time = seq(pp.vel), radius = pp.radius, trip=k)
    plot_rad_data = rbind(plot_rad_data, plot_rad)
    
    rad_dens = density(pp.acc_cent[vel0], from=0, to=0.4, n=20, na.rm=T)
    rad_dens_all = rbind(rad_dens_all, data.frame(acc=rad_dens$x, val=rad_dens$y, trip=k))
    
    acc_acf = acf(pp.acc, lag.max = 10, plot=FALSE)
    acc_acf_all = rbind(acc_acf_all, data.frame(lag=acc_acf$lag, acf=acc_acf$acf, trip=k))
    
    vel_acf = acf(pp.vel[vel0], lag.max = 10, plot=FALSE)
    vel_acf_all = rbind(vel_acf_all, data.frame(lag=vel_acf$lag, acf=vel_acf$acf, trip=k))
  }
  else{
    #this line exists to catch routes with no velocity. these are really garbage trips
    trip_data = rbind(trip_data, setNames(data.frame(t(rep(0,length(trip_data)))), names(trip_data)))
  }
}


# plots -------------------------------------------------------------------

plot_radius = F
plot2 = F
plot4 = F
plot_trip = F
plot_acc2_cum = F
plot_acc_cum = F
plot_vel_cum = F
plot_acc_deriv_all = F
plot_vel_acf = F
plot_acc_acf = F
plot_acc_cent = F
plot_centr_cum = T
plot_turns_cum = T

if(plot_turns_cum){
  #   png(paste0("accdens_plots/", driver, "_acc_dens.png"), width=1400, height=1000)
  
  p2 = ggplot(turns_dens_cum_all, aes(turn, cum, colour=trip, group=trip)) +
    geom_point(size=0.01) + geom_line()+
    ggtitle("cumulative turns dist for all trips")
  
  plot(p2+ylim(0,1))
  
  #   dev.off()
}
if(plot_centr_cum){
  #   png(paste0("accdens_plots/", driver, "_acc_dens.png"), width=1400, height=1000)
  
  p2 = ggplot(centr_dens_cum_all, aes(centr, cum, colour=trip, group=trip)) +
    geom_point(size=0.01) + geom_line()+
    ggtitle("cumulative centripacc dist for all trips")
  
  plot(p2+ylim(0,1))
  
  #   dev.off()
}
if(plot_acc2_cum){
  #   png(paste0("accdens_plots/", driver, "_acc_dens.png"), width=1400, height=1000)
  
  p2 = ggplot(acc2_dens_cum_all, aes(acc2, cum, colour=trip, group=trip)) +
    geom_point(size=0.01) + geom_line()+
    ggtitle("cumulative accleration deriv dist for all trips")
  
  plot(p2+ylim(0,1))
  
  #   dev.off()
}
if(plot_acc_cum){
  #   png(paste0("accdens_plots/", driver, "_acc_dens.png"), width=1400, height=1000)
  
  p2 = ggplot(acc_dens_cum_all, aes(acc, cum, colour=trip, group=trip)) +
    geom_point(size=0.01) + geom_line()+
    ggtitle("cumulative accleration dist for all trips")
  
  plot(p2+ylim(0,1))
  
  #   dev.off()
}
if(plot_vel_cum){
  #   png(paste0("accdens_plots/", driver, "_acc_dens.png"), width=1400, height=1000)
  
  p2 = ggplot(vel_dens_cum_all, aes(vel, cum, colour=trip, group=trip)) +
    geom_point(size=0.01) + geom_line()+
    ggtitle("cumulative velocity dist for all trips")
  
  plot(p2+ylim(0,1))
  
  #   dev.off()
}
if(plot_acc_deriv_all){
  #   png(paste0("accdens_plots/", driver, "_acc_dens.png"), width=1400, height=1000)
  
  p2 = ggplot(acc_deriv_all, aes(x, dens, colour=trip, group=trip)) +
    geom_point(size=0.01) + geom_line()+
    ggtitle("acc_deriv for all trips")
  
  plot(p2)
  plot(p2+ylim(0,10))
  
  #   dev.off()
}
if(plot_vel_acf){
  #   png(paste0("accdens_plots/", driver, "_acc_dens.png"), width=1400, height=1000)
  
  p2 = ggplot(vel_acf_all, aes(lag, acf, colour=trip, group=trip)) +
    geom_point(size=0.01) + geom_line()+
    ggtitle("velocity acf values")
  
  plot(p2)
  
  #   dev.off()
}
if(plot_acc_acf){
  #   png(paste0("accdens_plots/", driver, "_acc_dens.png"), width=1400, height=1000)
  
  p2 = ggplot(acc_acf_all, aes(lag, acf, colour=trip, group=trip)) +
    geom_point(size=0.01) + geom_line()+
    ggtitle("acc acf values")
  
  plot(p2)
  
  #   dev.off()
}
if(plot_acc_cent){
  #   png(paste0("accdens_plots/", driver, "_acc_dens.png"), width=1400, height=1000)
  
  p2 = ggplot(rad_dens_all, aes(acc, val, colour=trip, group=trip)) +
    geom_point(size=0.01) + geom_line()+
    ggtitle("acc_cent density") 
  
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
    geom_point(size=0.01) + geom_line()+ ylim(0,1000)
    ggtitle("radius of curve vs. time") 
  
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