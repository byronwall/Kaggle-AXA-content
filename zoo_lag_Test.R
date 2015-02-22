require("zoo")
require("ggplot2")

a <- zoo(1:100)
b <- lag(a, -3, na.pad = TRUE)

indices <- ! is.na(b)

c <- data.frame(a, b)

qplot(c$a,c$b)