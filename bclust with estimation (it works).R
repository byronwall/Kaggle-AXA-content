library("bclust")

trips_na = trips_scaled


mc.gaelle<-meancss(trips_na)
optimfunc<-function(theta)
{
  -loglikelihood(x.mean=mc.gaelle$mean,x.css=mc.gaelle$css,
                 repno=mc.gaelle$repno,transformed.par=theta)#compute - log likelihood
}
transpar<-optim(rep(0,6),optimfunc,method="BFGS")$par
#gives argmin(-loglikelihood)
#put a vector of correct length for the evaluation of the likelihood

bclus = bclust(trips_na,transformed.par=transpar)

plot(bclus)
ditplot(bclus)