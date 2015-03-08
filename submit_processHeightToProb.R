#this file will combine results from the different result files

#this code was used to read a common file of heights and convert them to an ECDF
#from there the PCA and non-PCA files were split out for submission.

filename = "submission_round3_noPCA.csv"

all_heights = read.csv(filename, header=T, sep=",", colClasses=c("character", "numeric", "numeric"))

#process each height

ecd_noPCA = ecdf(all_heights$height)
submit_noPCA = data.frame(driver_trip=all_heights$driver_trip, prob=round(1-ecd_noPCA(all_heights$height),7))

write.table(x = submit_noPCA, file = "submission_round3_probNoPCA.csv",
            row.names = FALSE, quote=FALSE, append=FALSE, sep=",",
            col.names = TRUE)

plot(submit_noPCA$prob[runif(1000,0,length(submit_noPCA[[1]]))])

ecd_PCA = ecdf(all_heights$height_pca)
submit_PCA = data.frame(driver_trip=all_heights$driver_trip, prob=round(1-ecd_PCA(all_heights$height_pca),7))

write.table(x = submit_PCA, file = "submission_round3_probWithPCA.csv", 
            row.names = FALSE, quote=FALSE, append=FALSE, sep=",",
            col.names = TRUE)

plot(submit_PCA$prob[runif(1000,0,length(submit_noPCA[[1]]))])
