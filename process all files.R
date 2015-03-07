#this file will combine results from the different result files

filename = "submission_round2_noPCA.csv"
file.create(filename)

files = dir("results_round2/")

for(file in 1:length(files)){
  preds = read.csv(paste0("results_round2/", files[file]), 
                   header=TRUE, sep=",", 
                   colClasses=c("character", "numeric", "numeric"))
  
  #this combines the driver/trip and rounds off probability
  answers = data.frame(driver_trip = paste0(file_path_sans_ext(files[file]), "_", preds$X), 
                       height=preds$height, height_pca=preds$height_pca)
  
  write.table(x = answers, file = filename, 
              row.names = FALSE, quote=FALSE, append=TRUE, sep=",",
              col.names = file==1)
  
  #this is just progress tracking
  if(file%%100 == 0){
    message(file)
  }
}