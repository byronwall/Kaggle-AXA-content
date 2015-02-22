# a Bayesian clustering method, good for high-dimension data, more details:
# http://vahid.probstat.ca/paper/2012-bclust.pdf

library(bclust)
x <- trips




d.bclus <- bclust(x, transformed.par=c(0, -50, log(4), 0, 0, 0))


ditplot(d.bclus)


