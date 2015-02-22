library("ggplot2")  # this library is needed
library("zoo")

analyze_driver <- function(driver){
  trips=NULL
  dens_info = NULL
  acf_info = NULL
  vel_info = NULL
  acc_shift_info = NULL
  
  
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
      dens_acc = density(pp.acc$y[vel0], from=-2, to = 2)
      dens_info <- rbind(dens_info, data.frame(x=dens_acc$x, y=dens_acc$y, trip=k))
      
      #calculate the acf info for acc
      acf_acc = acf(pp.vel[vel0], plot=FALSE, lag.max=10)
      acf_info <- rbind(acf_info, data.frame(x=acf_acc$lag, y=acf_acc$acf, trip=k))
      
    }
    #calculate the acc_t vs. acc_t+n data
    acc_shift = lag(zoo(pp.vel), -3, na.pad=TRUE)
    acc_shift_info <- rbind(acc_shift_info, data.frame(acc=pp.vel, acc_t=acc_shift, trip=k))
    
    #calculate density info for acc
    dens_vel = density(pp.vel, from=1, to=50)
    vel_info <- rbind(vel_info, data.frame(x=dens_vel$x, y=dens_vel$y, trip=k))
  }
  
  
  plot2 = T
  plot3 = T
  plot4 = T
  plot5 = T
  
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
  
  #acf density plots
  if(plot3){
    png(paste0("acf_plots/", driver, "_vel_acf_lrg.png"), width=1400, height=1000)
    
    p3 = ggplot(acf_info, aes(x, y, colour=trip, group=trip)) +
      geom_point(size=0.1) + xlim(0,10) + ylim(0,1)+
      ggtitle("acc acf plots")
    plot(p3)
    
    dev.off()
  }
  if(plot5){
    png(paste0("acc_nplus_plots/", driver, "_vel_nplus.png"), width=1400, height=1000)
    
    p3 = ggplot(acc_shift_info, aes(acc, acc_t, colour=trip, group=trip)) +
      geom_point(size=0.01) +
      ggtitle("acc t+3 plots") + xlim(0,50) + ylim(0,50)
    plot(p3)
    
    dev.off()
  }
}

analyze_driver(2016)