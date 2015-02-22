library("mclust")

x <- as.matrix(trips)

trips_scaled = scale(trips)

fit = Mclust(trips_scaled, G=1:(nrow(trips_scaled)/2))
plot(fit)
summary(fit)