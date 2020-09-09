#EY Data Science Competition
#Model List v2

##############################
#library

library(caret)
library(doParallel)

##############################
#Global Variables


##############################
#User Defined Functions

parallel_processing = function(){
    cl <<- makeCluster(5)
    registerDoParallel(cl)
    
    
    # stopCluster(cl)
}

##############################
#Produce Classification Result

vel_xy_to_class = function(vel_x, vel_y, attri){
    result_x = vel_x * attri["delta_time"]+ attri["last_x"]
    result_y = vel_y * attri["delta_time"]+ attri["last_y"]
    
    result_class = ifelse(abs(result_x) <= target_x_dev & abs(result_y) <= target_y_dev, 1, 0)
    
    return(result_class)
}

delta_xy_to_class = function(del_x, del_y, sd_x, sd_y, attri){
    result_x = del_x + attri[["last_x"]]
    result_y = del_y + attri[["last_y"]]
    
    prob = NSC_v3(result_x, result_y, sd_x, sd_y)
    
    return(prob)
}

last_xy_to_class = function(pred_x, pred_y){
    result_x = pred_x
    result_y = pred_y
    
    result_class = ifelse(abs(result_x) <= target_x_dev & abs(result_y) <= target_y_dev, 1, 0)
    result_class = unlist(result_class, use.names = F)
    
    return(result_class)
}

##############################
# Models

pca_v1 = function(){
    
    inTrain <<- createDataPartition(
        y = param_set_x$last_delta_x,
        p = 0.9,
        list = F
    )
    
    training_x <<- param_set_x[inTrain,]
    training_y <<- param_set_y[inTrain,]
    testing_x <<- param_set_x[-inTrain,]
    testing_y <<- param_set_y[-inTrain,]
    
    ctrl <<- trainControl(method = "repeatedcv", 
         repeats = 3, 
         classProbs = TRUE,
         #preProcOptions = list(pcaComp = 7),
         summaryFunction = twoClassSummary,
         verboseIter = TRUE
    )

    fit_x <<- train(
        last_delta_x ~ .,
        data = multinom,
        method = "lasso",
        trControl = ctrl,
        tuneGrid = data.frame(fraction = seq(0.9,0.99,0.01))
    )
    fit_y <<- train(
        last_delta_y ~ .,
        data = multinom,
        method = "lasso",
        trControl = ctrl,
        tuneGrid = data.frame(fraction = seq(0.9,0.99,0.01))
    )
}

pca_v2 = function(result_tibble){
    
    inTrain <<- createDataPartition(
        y = param_set$result_class,
        p = 0.9,
        list = F
    )
    
    training <<- param_set[inTrain,]
    testing <<- param_set[-inTrain,]
    
    ctrl = trainControl(
        method = "repeatedcv",
        repeats = 3,
        classProbs = TRUE,
        #preProcOptions = list(pcaComp = 7),
        summaryFunction = twoClassSummary,
        verboseIter = TRUE
    )
    
    fit <<- train(
        result_class ~ .,
        data = training,
        method = "multinom",
        family=binomial,
        trControl = ctrl,
        verbose = TRUE, 
        preProcess=c("scale", "pca")
        
    )
    
    prob <<- predict(fit, testing, type="prob")
    fact <<- unlist(result_tibble[-inTrain,"result_class"], use.names=F)
    
    result <<- NSC_v3_cutoff(prob$T, 0.3)
    
    cat("F1 Score:", F1Score(fact,result)$F1score, "\n\n")
    confusionMatrix(as.factor(1-fact), as.factor(1-result))
}



##############################

lm_v1 = function(attri, result_tibble){
    
    inTrain <<- createDataPartition(
        y = param_set_x$last_delta_x,
        p = 0.9,
        list = F
    )
    
    training_x <<- param_set_x[inTrain,]
    training_y <<- param_set_y[inTrain,]
    testing_x <<- param_set_x[-inTrain,]
    testing_y <<- param_set_y[-inTrain,]
    
    ctrl = trainControl(
        method = "repeatedcv",
        number = 10,
        repeats = 5,
        verboseIter = TRUE
    )
    
    fit_x <<- train(
        last_delta_x ~ .,
        data = training_x,
        method = "lasso",
        trControl = ctrl,
        tuneGrid = data.frame(fraction = seq(0.9,0.99,0.01))
    )
    fit_y <<- train(
        last_delta_y ~ .,
        data = training_y,
        method = "lasso",
        trControl = ctrl,
        tuneGrid = data.frame(fraction = seq(0.9,0.99,0.01))
    )
    
    pred_x <<- predict(fit_x, testing_x)
    pred_y <<- predict(fit_y, testing_y)
    sd_x <<- fit_x$results[as.numeric(row.names(fit_x$bestTune)),"RMSE"]
    sd_y <<- fit_y$results[as.numeric(row.names(fit_y$bestTune)),"RMSE"]
    sd_x <<- rep(sd_x, length(pred_x))
    sd_y <<- rep(sd_y, length(pred_y))
    
    prob <<- delta_xy_to_class(pred_x, pred_y, sd_x, sd_y, attri[-inTrain,])
    
    fact <<- unlist(result_tibble[-inTrain,"result_class"], use.names=F)
    
    result <<- NSC_v3_cutoff(prob, 0.2)
    cat("F1 Score:", F1Score(fact,result)$F1score, "\n\n")
    confusionMatrix(as.factor(1-fact), as.factor(1-result))
}

lmc_v1 = function(attri, result_tibble){
    
    inTrain <<- createDataPartition(
        y = param_set$result_class,
        p = 0.9,
        list = F
    )
    
    training <<- param_set[inTrain,]
    testing <<- param_set[-inTrain,]
    
    ctrl = trainControl(
        method = "repeatedcv",
        number = 10,
        repeats = 5,
        verboseIter = TRUE
    )
    
    fit <<- train(
        as.factor(result_class) ~ .,
        data = training,
        method = "ranger",
        trControl = ctrl
    )
    
    prob <<- predict(fit, testing, type="prob")
    fact <<- unlist(result_tibble[-inTrain,"result_class"], use.names=F)
    
    result <<- NSC_v3_cutoff(prob, 0.3)
    # result <<- as.numeric(pred)-1
    
    cat("F1 Score:", F1Score(fact,result)$F1score, "\n\n")
    confusionMatrix(as.factor(1-fact), as.factor(1-result))
}




#############################
#parameter set

param_1_1 = function(attri, result_tibble){
    param_set_x <<- cbind(attri[c("last_x")], result_tibble["result_x"])
    param_set_y <<- cbind(attri[c("last_y")], result_tibble["result_y"])
    
    lm_v1(attri, result_tibble)
}

param_1_2 = function(attri, result_tibble){
    param_set_x <<- cbind(attri[c("adj_home_x","last_x")], result_tibble["result_x"])
    param_set_y <<- cbind(attri[c("adj_home_y","last_y")], result_tibble["result_y"])
    
    lm_v1(attri, result_tibble)
}

param_1_3 = function(attri, result_tibble){
    param_set_x <<- cbind(attri[c("adj_home_x")], result_tibble["result_x"])
    param_set_y <<- cbind(attri[c("adj_home_y")], result_tibble["result_y"])
    
    lm_v1(attri, result_tibble)
}

param_1_4 = function(attri, result_tibble){
    param_set_x <<- cbind(attri[c("adj_home_x","adj_home_y","last_x","last_y","delta_time")], result_tibble["result_x"])
    param_set_y <<- cbind(attri[c("adj_home_x","adj_home_y","last_x","last_y","delta_time")], result_tibble["result_y"])
    
    lm_v1(attri, result_tibble)
}

param_1_5 = function(attri, result_tibble){
    param_set_x <<- cbind(train_vel,attri[c("adj_home_x","adj_home_y","last_x","last_y","delta_time")], result_tibble["last_delta_x"])
    param_set_y <<- cbind(train_vel,attri[c("adj_home_x","adj_home_y","last_x","last_y","delta_time")], result_tibble["last_delta_y"])
    
    lm_v1(attri, result_tibble)
}

param_1_6 = function(attri, result_tibble){
    param_set_x <<- cbind(train_vel[good,21:30],attri[c("x_displacement","delta_time")], result_tibble["last_delta_x"])
    param_set_y <<- cbind(train_vel[good,51:60],attri[c("y_displacement","delta_time")], result_tibble["last_delta_y"])
    
    lm_v1(attri, result_tibble)
}

#############################
#parameter set

param_2_1 = function(attri, result_tibble){
    param_set <<- cbind(train_vel[good,c(21:30,51:60)],attri[c("adj_home_x","adj_home_y","last_x","last_y","x_displacement","y_displacement","delta_time")], result_tibble["result_class"])
    
    lmc_v1(attri, result_tibble)
}












##############################

run = function(){
    training_set <<- train_single_attri[c("last_time","last_x","last_y","delta_time","result_time","result_class")]
    
    inTrain = createDataPartition(
        y = training_set$result_class,
        p = 0.8,
        list = F
    )
    
    training <<- training_set[inTrain,]
    testing <<- training_set[-inTrain,]
    
    ctrl = trainControl(
        method = "repeatedcv",
        number = 10,
        repeats = 10
    )
    
    model_fit <<- train(
        as.factor(result_class) ~ .,
        data = training,
        method = "ranger",
        trControl = ctrl
    )
    
    pred <<- predict(model_fit, testing)
    confusionMatrix(pred, as.factor(testing$result_class))
    
    F1Score(testing$result_class, as.numeric(pred)-1)$F1score
}



















