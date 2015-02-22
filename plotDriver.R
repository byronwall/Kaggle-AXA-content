library("ggplot2")  # this library is needed
library("plyr")

plotDriver <- function(driver, trip_nos){
  trips=NULL
  sq=(driver:driver)
  for(j in sq){
    driver=as.character(j)
    dirPath = paste0("drivers/", driver, "/")
    for(i in trip_nos)
    {
#       if(count(c)$freq[c[i]] > 3){
#         message(i)
#         next
#       }
      
      trip = read.csv(paste0(dirPath, i, ".csv"), header=TRUE, sep=",", colClasses=c("numeric", "numeric"))
      
      #rotate the trip based on the last point
      last_point = tail(trip, 1)
      angle = atan2(last_point$x, last_point$y) - 3.14159 / 2
      
      rotated.x = trip$x * cos(angle) - trip$y*sin(angle)
      rotated.y = trip$y * cos(angle) + trip$x*sin(angle)
      
      check_1 = floor(length(rotated.y)/2)
      check_2 = floor(length(rotated.y)/4)
      if( abs(rotated.y[check_1]) > 100 && rotated.y[check_1] < 0){
        rotated.y = rotated.y *-1
      } else if( rotated.y[check_2] < 0){
        rotated.y = rotated.y *-1
      }
      
      
      trip=data.frame(x = rotated.x, y = rotated.y,i = i,j = j)
      trips=rbind(trips,trip)
    }
  }
  p1 = ggplot(trips, aes(x, y, colour=i, group=i)) +
    geom_point(size=0.01) +
    facet_wrap(~ j,ncol=2) +
    ggtitle(paste(trip_nos, collapse=' ')) 
  
  plot(p1)
}