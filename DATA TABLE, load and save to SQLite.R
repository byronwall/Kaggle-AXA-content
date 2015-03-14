
#read each file from the disk

feature_files = dir("driver_features/", full.names=T)

driver.DT = NULL

for(filename in feature_files){
  driver.DT = rbindlist(list(driver.DT, fread(filename)))
}

#create a big data table as it goes

#code to save to SQLite

require("RSQLite")
# Set up database    
drv <- dbDriver("SQLite")
tfile <- "driver_features.db"
con <- dbConnect(drv, dbname = tfile)

# Write data "USArrests" in table "USArrests" in database "test.db"    
dbWriteTable(con, "features", driver.DT, overwrite=T)
dbDisconnect(con)
