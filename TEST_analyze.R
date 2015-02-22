library("ggplot2")  # this library is needed

driver = 106

  trips=NULL
  dens_info = NULL
  acf_info = NULL
  vel_info = NULL
  acc_shift_info = NULL
  trip_data = NULL
  
  row_names = NULL
  
  #   for(driver in drivers){
  
  
  dirPath = paste0("drivers/", driver, "/")

indices = 1:200
  for(k in indices)
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
    
    pos = rotated
    
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
      
      
      
      #calculate density info for acc
      dens_acc = density(pp.acc$y[vel0], from=-2, to = 2, n=20)
      
      
      #calculate density info for acc
      dens_vel = density(pp.vel[vel0], from=2, to=35, n=20)
      
      
      #create a data frame for the different pieces
      pp.dist = cumsum(pp.vel)
      pp.dist.grab = seq(from=0, to=length(pp.dist), length.out = 15)
      #       trip = data.frame(t(dens_acc$y))#, t(pp.dist[pp.dist.grab]))
      
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
      
      start_x = pos$x[dist_indices]
      start_y = pos$y[dist_indices]
      
      end_x = pos$x[dist_indices[2:spots]]
      end_y = pos$y[dist_indices[2:spots]]
      
      delta_x = (end_x - start_x[1:(spots-1)]) / dist_delta
      delta_y = (end_y - start_y[1:(spots-1)]) / dist_delta
      
      #need to create the object with all values
      dx = as.data.frame(t(delta_x))
      colnames(dx) = paste(c("dx"), 1:(spots-1), sep="")
      
      dy = as.data.frame(t(delta_y))
      colnames(dy) = paste(c("dy"), 1:(spots-1), sep="")      
      
      trip = data.frame(t(dens_vel$y), t(dens_acc$y), t(pp.dist[pp.dist.grab]), dx, dy)
      names(trip) = names(trip_data)
      
      trip_data = rbind(trip_data, trip)
      
      
    }
    else{
      #this line exists to catch routes with no velocity. these are really garbage trips
      
      if(is.null(trip_data)){
        trip_data = rbind(trip_data, data.frame(t(rep(0,82))))
      }else{
        trip_data = rbind(trip_data, setNames(data.frame(t(rep(0,length(trip_data)))), names(trip_data)))
      }
      
      
      
      
    }
    message(paste0(k, "=", nrow(trip_data)))
  }
  #   }
  
  # row.names(trip_data) = make.unique(as.character(row_names))
  #work out some of the trip analysis math --- very similar to trip matching
  trips_scaled = scale(trip_data, center=FALSE)
  
  dst = dist(trips_scaled, method="euclidean")
  hc = hclust(dst, method="single")
  #   plot(hc)
  # ord = cmdscale(dst, k=2)
  # den = as.dendrogram(hc)
  # x = scores(ord, display="sites")
  # oden = reorder(den, x)
  # plot(oden)
  
  # this is some math to get to probabilities
  cop = as.matrix(cophenetic(hc))
  cop[cop==0] = NA
  cop.min = as.data.frame(apply(cop, 2, min, na.rm = TRUE))
  
  prob = ecdf(dst)
  probs = 1 - apply(cop.min, 1, prob)  
  probs.df = data.frame(probs, trip=1:200, driver = driver)
  
  write.csv(file=paste0("results/", driver, ".csv"), x= probs.df, row.names=FALSE)
  
 
