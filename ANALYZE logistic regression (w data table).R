#PURPOSE: this file is the main analysis code for the unsupervised technique

#do logistic regression


#get the data together, add probs

do_randomForest = function (driver_no, rando_count = 200, repeats = 4, cross_val_frac = 0.3){
  
  library("data.table")
  library("randomForest")
  library("verification")
  library("plyr")
  
  driver_data = driver.DT[driver %in% driver_no ]
  
  results = NULL
  ESTS = NULL
  
  AUC_best= 0
  driver_ests = NULL
  
  for(k in 1:repeats){
    
    random_data = driver.DT[V1 %in% sample(driver.DT$V1, rando_count)]    
    random_data = random_data[random_data$driver != driver_no]
    
    model_data = rbind(driver_data, random_data)
    
    model_data[,c("driver", "trip"):=NULL]
    #drop out certain terms
    #   dx_dy = substr(colnames(model_data), 1,2) %in% c("dx", "dy")
    #   model_data[,colnames(model_data)[dx_dy]:=NULL]
    #   
    
    model_data = as.data.frame(model_data)
    rownames(model_data) = model_data$V1
    model_data$V1 = NULL
    
    
    
    #   pr = prcomp(model_data, scale=T, center=T, tol=0)
    #   pr_model_data = as.data.frame(predict(pr, model_data))
    
    pr_model_data = as.data.frame(scale(model_data, center=T, scale=T))
    
    #add the probs in now
    probs = c(rep(1, length(driver_data[[1]])),
              rep(0, length(random_data[[1]])))
    
    #model_data = model_data[,apply(model_data, 2, var, na.rm=TRUE) != 0]
    
    pr_model_data = cbind(pr_model_data, prob = probs)
    
    #take a subset
    mod_count = length(pr_model_data[[1]])
    
    
    subset = sample(mod_count, round(cross_val_frac*mod_count))
    
    #these are for cross validation
    train_data = pr_model_data[-subset,]
    test_data = pr_model_data[subset,]
    
    model=randomForest(as.factor(prob) ~ ., data =train_data, importance = T, ntree=100)
    est = predict(model, newdata = test_data, type = "prob")[,2]
    
    AUC = roc.area(test_data$prob, est)$A
    
    if(AUC > AUC_best){
      driver_ests = predict(model, newdata = pr_model_data[pr_model_data$prob %in% 1,], type="prob")[,2]
      AUC_best = AUC
    }
    
    
    #this is useful to get all the predictions and test linearity b/t model
    #ESTS = rbind.fill(ESTS, as.data.frame(t(est)))
    
    #this is great for cross-val.  Need to run the driver data through alone and get a score all.  AVG those at end.
    
  }
  
  #make the ests a data.frame
  
  driver_ests = data.frame(probs = driver_ests)
  driver_ests$driver = rownames(driver_ests)
    
  return(driver_ests)
  
}

#couple of methods of vizing the reuslts
#pairs(t(ESTS))
#boxplot(results)
