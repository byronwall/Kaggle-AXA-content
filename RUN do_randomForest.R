#file to write estimates to a SQLite DB

library("foreach")
library("doParallel")

driver.list = unique(driver.DT$driver)

set.seed(42)
registerDoParallel(cores = 3)

run_data = function(driver_no, con){
  library("RSQLite")
  data1 = do_randomForest(driver_no)  
  
  con = dbConnect(dbDriver("SQLite"), dbname = "driver_ests_round5.db")
  dbWriteTable(con, "probs", data1, append=T, row.names=F)
  dbDisconnect(con)
}

foreach(driver_no = driver.list) %dopar% run_data(driver_no, con)

stopImplicitCluster()