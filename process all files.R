#this file will combine results from the different result files

filename = "submission_01.csv"
file.create(filename)

files = dir("results/")

for(file in 1:length(files)){
  preds = read.csv(paste0("results/", files[file]), 
                   header=TRUE, sep=",", 
                   colClasses=c("numeric", "numeric", "numeric"))
  
  #this combines the driver/trip and rounds off probability
  answers = data.frame(driver_trip = paste0(preds$driver, "_", preds$trip), 
                       prob=round(preds$probs,0))
  
  write.table(x = answers, file = filename, 
              row.names = FALSE, quote=FALSE, append=TRUE, sep=",",
              col.names = file==1)
  
  #this is just progress tracking
  if(file%%100 == 0){
    message(file)
  }
}