library("ggplot2")  # this library is needed
library("zoo")

# analyze_driver <- function(driver){

  driver = 2030

  trips=NULL
  dens_info = NULL
  acf_info = NULL
  vel_info = NULL
  acc_shift_info = NULL

  trip_data = NULL
  
  
  dirPath = paste0("drivers/", driver, "/")
  for(k in 1:200)
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
    pp.accx <- predict(smooth.spline(pp.velx$y, df=time_end/i), deriv=1)
    pp.accy <- predict(smooth.spline(pp.vely$y, df=time_end/i), deriv=1)  
    
    #acf(pp.acc$y)
    vel0 = pp.vel > 2
    
    #this section being skipped since only velocity plots are desired
    if(sum(vel0)>2){
      
      #combine the acceleration info
      trips <- rbind(trips, data.frame(time=pp.acc$x, acc=pp.acc$y, trip=k))    
      
      #calculate density info for acc
      dens_acc = density(pp.acc$y[vel0], from=-2, to = 2, n=20)
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
    }
    else{
      #this line exists to catch routes with no velocity. these are really garbage trips
      trip_data = rbind(trip_data, setNames(data.frame(t(rep(0,length(trip_data)))), names(trip_data)))
    }
  }
  
#work out some of the trip analysis math --- very similar to trip matching
trips_scaled = scale(trip_data)

dst = dist(trips_scaled, method="euclidean")
hc = hclust(dst, method="single")

ord = cmdscale(dst, k=3)
den = as.dendrogram(hc)
x = scores(ord, display="sites")
oden = reorder(den, x)

plot(oden)
  
  plot2 = F
  plot4 = F
  
  if(plot2){
    png(paste0("accdens_plots/", driver, "_acc_dens.png"), width=1400, height=1000)
    
    p2 = ggplot(dens_info, aes(x, y, colour=trip, group=trip)) +
      geom_point(size=0.01) + geom_line()+
      ggtitle("acc density plots") + ylim(0,5)
    
    plot(p2)
    
    dev.off()
  }
  
  if(plot4){
    png(paste0("veldens_plots/", driver, "_vel_dens.png"), width=1400, height=1000)
    
    p2 = ggplot(vel_info, aes(x, y, colour=trip, group=trip)) +
      geom_point(size=0.01) + geom_line()+
      ggtitle("vel density plots")
    
    plot(p2)
    
    dev.off()
  }
# }
# 
# analyze_driver(2018)