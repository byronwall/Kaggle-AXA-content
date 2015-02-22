library("vegan")
library("zoo")
library("dtw")
library("reshape2")

for(driver in 1003){
  trips=NULL
  dens_info = NULL
  acf_info = NULL
  vel_info = NULL
  acc_shift_info = NULL
  x_all = zoo()
  
  x_data = list()
  
  
  dirPath = paste0("drivers/", driver, "/")
  for(k in 1:50)
  {
    pos = read.csv(paste0(dirPath, k, ".csv"), header=TRUE, sep=",", colClasses=c("numeric", "numeric"))
    
    #rotate the trip based on the last point
    last_point = tail(pos, 1)
    angle = atan2(last_point$x, last_point$y) - 3.14159 / 2
    
    rotated = data.frame(x = pos$x * cos(angle) - pos$y*sin(angle), 
                         y=pos$y * cos(angle) + pos$x*sin(angle))
    
    if( rotated$y[floor(length(rotated$y)/2)] < 0 ){
      rotated$y = rotated$y *-1
    }
    
    
#     z_pos_x = zoo(rotated$x)
    
    x_data[[k]] = rotated$x
    
    
    #create a matrix with all the trips together
    
#     x_all = merge(x_all, z_pos_x)
#     m = as.matrix(x_all)
#     m[is.na(m)] = 0
    
  }
  
#   dtw_results = dtw(x_data[[34]], x_data[[49]], keep=TRUE)
  
  
#   this code is used to compare several all at once
  dtw_all = data.frame()
  
  for(id in 1:49){
    for(id_1 in (id+1):50){
      dtw_results = dtw(x_data[[id]], x_data[[id_1]])
      
      dtw_all = rbind(dtw_all, data.frame(id, id_1, dist=dtw_results$normalizedDistance))
      
    }
    message(paste0("done with ", id))
  }
  
  dtw_dist = dcast(dtw_all, id ~ id_1)  
  
}
