library("vegan")

#d = dist
#csin/caver = hclust
#dune = data
#cl = cutree
#ord = cmdscale
#den = as.dendrogram
#oden = reorder(dendro)

ord = cmdscale(dst, k=3)
den = as.dendrogram(hc)
x = scores(ord, display="sites")
oden = reorder(den, x)

ordiplot(ord, display="sites")
ordispider(ord, cutree(hc, h=5))



plot(oden)

dst.man = dist(trips_scaled, method="manhattan")
hc.man = hclust(dst.man, method="single")
plot(hc.man)

plot(oden)
rect.hclust(oden, 1)


## function to set label color
labelCol <- function(x) {
  if (is.leaf(x)) {
    ## fetch label
    label <- attr(x, "label") 
    ## set label color to red for A and B, to blue otherwise
    attr(x, "nodePar") <- list(lab.col=ifelse(substr(label, 1,4) %in% c(2030), "red", "blue"))
  }
  return(x)
}

d <- dendrapply(as.dendrogram(hc), labelCol)

plot(d)

cop = as.matrix(cophenetic(hc))
cop[cop==0] = NA
cop.min = as.data.frame(val = apply(cop, 2, min, na.rm = TRUE))

prob = ecdf(dst)
probs = 1 - apply(cop.min, 1, prob)

