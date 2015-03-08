#PURPOSE: this file is the main analysis code for the unsupervised technique

analyze_driver <- function(driver){
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
    
    posx.spline = smooth.spline(pos$x, df=time_end/i)
    posy.spline = smooth.spline(pos$y, df=time_end/i)
    
    velx.deriv = predict(posx.spline, deriv=1)
    vely.deriv = predict(posy.spline, deriv=1)    
    
    pp.vel = sqrt(velx.deriv$y^2 + vely.deriv$y^2)
    
    
    
    #acf(pp.acc$y)
    vel0 = pp.vel > 2
    
    #this section being skipped since only velocity plots are desired
    #increased to 10 for the ACF stuff to work
    if(sum(vel0)>10){
      
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
      
      ###this is now the distribution section
      
      #calculate density info for acc
      dens_acc = density(pp.acc[vel0], from=-2, to = 2, n=15)    
      
      #calculate density info for acc
      dens_vel = density(pp.vel[vel0], from=2, to=35, n=15)
      
      #get density for the acc_deriv
      dens_accDeriv = density(pp.acc_deriv[vel0], n=15, from=-0.5, to=0.5)
      
      #these are the new cumulative infos
      vel_cdf = ecdf(pp.vel[vel0])
      vel_cdf_seq = seq(0,35,length.out=15)
      vel_dens_cum = vel_cdf(vel_cdf_seq)
      
      acc_cdf = ecdf(pp.acc[vel0])
      acc_cdf_seq = seq(-2,2,length.out=15)
      acc_dens_cum = acc_cdf(acc_cdf_seq)
      
      acc2_cdf = ecdf(pp.acc_deriv[vel0])
      acc2_cdf_seq = seq(-1,1,length.out=15)
      acc2_dens_cum = acc2_cdf(acc2_cdf_seq)
      
      centr_cdf = ecdf(pp.acc_cent[vel0])
      centr_cdf_seq = seq(0,2.5,length.out=15)
      centr_dens_cum = centr_cdf(centr_cdf_seq)
      
      #create a data frame for the different pieces
      pp.dist = cumsum(pp.vel)
      pp.dist.grab = seq(from=0, to=length(pp.dist), length.out = 5)
      
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
      
      #add in cumulatives
      df.cum_vel = data.frame(t(vel_dens_cum))
      colnames(df.cum_vel) = paste(c("vel_cum"), seq(df.cum_vel), sep="")
      
      df.cum_acc = data.frame(t(acc_dens_cum))
      colnames(df.cum_acc) = paste(c("acc_cum"), seq(df.cum_acc), sep="")
      
      df.cum_acc2 = data.frame(t(acc2_dens_cum))
      colnames(df.cum_acc2) = paste(c("acc2_cum"), seq(df.cum_acc2), sep="")
      
      df.cum_centr = data.frame(t(centr_dens_cum))
      colnames(df.cum_centr) = paste(c("centr_cum"), seq(df.cum_centr), sep="")      
      
      
      trip = data.frame(df.dens_vel, df.dens_acc, df.dens_acc_deriv,
                        df.cum_vel, df.cum_acc, df.cum_acc2, df.cum_centr,
                        t(pp.dist[pp.dist.grab]), 
                        dx, dy)    
      trip_data = rbind(trip_data, trip)
    }
    else{
      #create a default option for those routes with no good data
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
  trips.pca = prcomp(trip_data[,apply(trip_data, 2, var, na.rm=TRUE) != 0], scale=T)
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
  
  rownames(cop.all) = seq(cop.all[[1]])
  
  write.csv(file=paste0("../results_round3/", driver, ".csv"), x= cop.all, row.names=TRUE)
}
