library("ggplot2")  # this library is needed
library("zoo")

#PURPOSE: this file is the main analysis code for the unsupervised technique

driver = 2033
trip_data = NULL
missing_rows = NULL


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
  pp.acc_deriv <- predict(smooth.spline(pp.acc, df=time_end/i), deriv=1)
  
  #acf(pp.acc$y)
  vel0 = pp.vel > 2
  
  #this section being skipped since only velocity plots are desired
  #increased to 10 for the ACF stuff to work
  if(sum(vel0)>10){
    
    
    
    #calculate density info for acc
    dens_acc = density(pp.acc$y[vel0], from=-2, to = 2, n=15)    
    
    #calculate density info for acc
    dens_vel = density(pp.vel[vel0], from=2, to=35, n=15)
    
    #get density for the acc_deriv
    dens_accDeriv = density(pp.acc_deriv$y[vel0], n=15, from=-0.5, to=0.5)
    
    #create a data frame for the different pieces
    pp.dist = cumsum(pp.vel)
    pp.dist.grab = seq(from=0, to=length(pp.dist), length.out = 15)
    
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
    
    df.dens_vel = data.frame(t(dens_vel$y))
    colnames(df.dens_vel) = paste(c("vel"), seq(dens_vel$y), sep="")
    
    df.dens_acc = data.frame(t(dens_acc$y))
    colnames(df.dens_acc) = paste(c("acc"), seq(dens_acc$y), sep="")
    
    df.dens_acc_deriv = data.frame(t(dens_accDeriv$y))
    colnames(df.dens_acc_deriv) = paste(c("accD"), seq(dens_accDeriv$y), sep="")
    
    trip = data.frame(df.dens_vel, df.dens_acc, df.dens_acc_deriv, t(pp.dist[pp.dist.grab]), dx, dy)    
    trip_data = rbind(trip_data, trip)
  }
  else{
    #create a default option for those routes with no good data
    message(k)
    missing_rows = c(missing_rows, k)
  }
}
#   }

# row.names(trip_data) = make.unique(as.character(row_names))
#work out some of the trip analysis math --- very similar to trip matching
trips_scaled = scale(trip_data, center=FALSE)

dst = dist(trips_scaled, method="euclidean")
hc = hclust(dst, method="single")

# this is some math to get to probabilities
cop = as.matrix(cophenetic(hc))
cop[cop==0] = NA
cop.min = as.data.frame(apply(cop, 2, min, na.rm = TRUE))
colnames(cop.min) = c("height")

#do the PCA bit here and get a second set of heights
trips.pca = prcomp(trip_data, scale=T)
dst.pca = dist(trips.pca$x)
hc.pca = hclust(dst.pca, method="single")

cop.pca = as.matrix(cophenetic(hc.pca))
cop.pca[cop.pca==0] = NA
cop.pca.min = as.data.frame(apply(cop.pca, 2, min, na.rm = TRUE))
colnames(cop.pca.min) = c("height_pca")

cop.all = cbind(cop.min, cop.pca.min)

#need to drop in the replacement rows w/ very large height
for(missing in missing_rows){
  missing = missing - 1
  cop.all = rbind(cop.all[1:missing,], c(1000,1000), cop.all[-(1:missing),])
}