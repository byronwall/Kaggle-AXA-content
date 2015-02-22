trip_match(2018)

# for( k in which(count(c)$freq > 3)){
#   plotDriver(dr, which(c %in% k))
# }
# plotDriver(dr, 1:200)
# plotDriver(dr, which(c %in% 6))
plotDriver(2020, c(195,182,100,92,23,70))
plotDriver(2020, c(101,142))

plotDriver(driver, c(178))

for(i in 1020:1024){
  analyze_driver(i)
}

plot(hc.single)
rect.hclust(hc.single, h=5)