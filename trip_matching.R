library("vegan")
library("plyr")
library("ggplot2")

trip_match <- function(driver){
  trips=NULL
  dens_info = NULL
  acf_info = NULL
  vel_info = NULL
  acc_shift_info = NULL
  dist_max = NULL
  
  dirPath = paste0("drivers/", driver, "/")
  for(k in 1:200)
  {
    pos = read.csv(paste0(dirPath, k, ".csv"), header=TRUE, sep=",", colClasses=c("numeric", "numeric"))
    
    #rotate the trip based on the last point
    last_point = tail(pos, 1)
    angle = atan2(last_point$x, last_point$y) - 3.14159 / 2
    
    rotated = data.frame(x = pos$x * cos(angle) - pos$y*sin(angle), 
                         y=pos$y * cos(angle) + pos$x*sin(angle))
    
    check_1 = floor(length(rotated$y)/2)
    check_2 = floor(length(rotated$y)/4)
    if( abs(rotated$y[check_1]) > 100 && rotated$y[check_1] < 0){
      rotated$y = rotated$y *-1
    } else if( rotated$y[check_2] < 0){
      rotated$y = rotated$y *-1
    }
    
    #for now, use i=5 for best results
    i = 5
    
    time_end = length(rotated$x) - 1
    
    #for these, x = time, y = vel
    pp.velx <- predict(smooth.spline(rotated$x, df=time_end/i), deriv=1)
    pp.vely <- predict(smooth.spline(rotated$y, df=time_end/i), deriv=1)    
    pp.vel <- sqrt(pp.velx$y^2 + pp.vely$y^2)
    
    #have velocity and position data
    
    #need to create a metric of distance traveled over time
    #distance = integral of velocity = sum of velocity since time is unit
    pp.dist <- cumsum(pp.vel)
    
    spots = 15
    
    max_dist = max(pp.dist)
    
    dist_indices <- seq(spots)
    #dist_target = max_dist / spots
    
    dist_index = 1
    
    dist_delta = max_dist / spots
    dist_target = dist_delta
    
    for(spot in seq_along(pp.dist)){
      #the -1 below handles small errors (epsilon)
      if(pp.dist[spot]>=dist_target-1){
        dist_indices[dist_index] = spot
        
        if(dist_index == spots){break}
        
        dist_index = dist_index + 1        
        dist_target = dist_target + dist_delta
      }
      
    }
    
    #get the position at those points and log the delta from previous point (x,y)
    
    start_x = rotated$x[dist_indices]
    start_y = rotated$y[dist_indices]
    
    end_x = rotated$x[dist_indices[2:spots]]
    end_y = rotated$y[dist_indices[2:spots]]
    
    delta_x = (end_x - start_x[1:(spots-1)]) / dist_delta
    delta_y = (end_y - start_y[1:(spots-1)]) / dist_delta
    
    #need to create the object with all values
    dx = as.data.frame(t(delta_x))
    colnames(dx) = paste(c("dx"), 1:(spots-1), sep="")
    
    dy = as.data.frame(t(delta_y))
    colnames(dy) = paste(c("dy"), 1:(spots-1), sep="")
    
    dists = as.data.frame(t(pp.dist[dist_indices]))
    colnames(dy) = paste(c("dist"), 1:(spots-1), sep="")
    
    #     all = data.frame(dx, dy, miny=min(rotated$y), maxy=max(rotated$y), minx=min(rotated$x), maxx=min(rotated$x))
    
    all = data.frame(dx, dy, dists)
    
    trips <- rbind(trips, all)
    
    #record this and then compare or cluster somehow across all trips checked
    
    
  }
  
  trips_scaled = scale(trips)
  
  #at this point, trips contains all the trip deltas for analysis
  dst = dist(trips_scaled, method="euclidean")
  hc = hclust(dst, method="average")
  
  png(paste0("clusts/", driver, "_clust.png"), width=2000, height=2000)
  
  cut_pos = 3
  
  plot(hc)
  rect.hclust(hc, h=cut_pos)
  
  dev.off()
  
  plot(density(dst, from=0))
  
  c = cutree(hc, h=cut_pos)
  
  plot1 = F
  
  if(plot1){
    p1 = ggplot(trips, aes(x, y, colour=k, group=k)) +
      geom_point(size=3) + geom_line() + 
      ggtitle("delta for trips") 
    
    plot(p1)
  }
  
  for( k in which(count(c)$freq > 2)){
    plotDriver(dr, which(c %in% k))
  }
  
}
