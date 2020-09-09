#EY Data Science Competition
#Ideas List

##############################
#library

library(randomForest)

##############################
#Global Variables


##############################
#User Defined Functions




##############################

subset = good & set0


train = train_attri[subset,]
result_fact = train_result[subset,]
t_vel_2 = train_vel_2[subset,]
t_vel_1 = train_vel[subset,]

result_class = as.factor(ifelse(result_fact[["result_class"]], "T", "F"))

names(t_vel_2) = paste("2_",names(t_vel_2),sep="")

ext_attri = tibble(
    
)

ext_result = tibble(
    
)

##############################

nominal_direction = function(diff){
    if (diff > 180){
        diff = diff - 360
    }
    else if (diff <= -180){
        diff = diff + 360
    }
    
    return(diff)
}

fun_1 = function(x,y){
    return(nominal_direction(y-x))
}

fun_2 = function(x,y){
    return(nominal_direction(x + fun_1(x,y)/2))
}


dir_diff = mapply(fun_1, train$direction, result_fact$result_direction)
dir_home_diff = mapply(fun_1, train$home_direction, result_fact$result_direction)
dir_home_diff = mapply(fun_1, dir_home_diff, 180)

dir_diff_attri_home = mapply(fun_1, train$direction, train$home_direction)

same_dir = abs(dir_diff_attri_home) <= 90

fun_3 = function(dir,home,same){
    if (!same){
        dir = nominal_direction(dir-180)
    }
    
    diff = nominal_direction(dir + fun_1(dir,home)/2)
    diff = nominal_direction(diff-180)
        
    return(diff)
}

avg_dir = mapply(fun_3, train$direction, train$home_direction,same_dir)

avg_dir_diff = mapply(fun_1, avg_dir, train$direction)
avg_dir_home_diff = mapply(fun_1, avg_dir, train$home_direction)
avg_dir_diff_res = mapply(fun_1, avg_dir, result_fact$result_direction)


# train_attri$home_direction - 180 : best predict the direction of travel

##############################

result_vel = result_fact[["vel"]]

param_set = cbind(
    t_vel_1,
    t_vel_2,
    train[-1],
    result_vel
)


rf_model = function(){
    fit = randomForest(result_vel ~ .,data=param_set)
}



##############################

rf_r_v1 = function(){
    
    inTrain <<- createDataPartition(
        y = param_set$result_vel,
        p = 0.9,
        list = F
    )
    
    training <<- param_set[inTrain,]
    testing <<- param_set[-inTrain,]
    
    ctrl = trainControl(
        method = "repeatedcv",
        repeats = 1,
        #summaryFunction = twoClassSummary,
        verboseIter = TRUE
    )
    
    fit <<- train(
        result_vel ~ .,
        data = training,
        method = "ranger",
        trControl = ctrl,
        verbose = TRUE
        #preProcess=c("scale", "pca")
        
    )
    
    pred <<- predict(fit, testing)
}

glm_v1 = function(){
    
    inTrain <<- createDataPartition(
        y = param_set$result_vel,
        p = 0.9,
        list = F
    )
    
    training <<- param_set[inTrain,]
    testing <<- param_set[-inTrain,]
    
    ctrl = trainControl(
        method = "repeatedcv",
        repeats = 3,
        #summaryFunction = twoClassSummary,
        verboseIter = TRUE
    )
    
    tune.grid = data.frame(
        nvmax = 1:147
    )
    
    
    fit <<- train(
        result_vel ~ .,
        data = training,
        method = "leapSeq",
        trControl = ctrl,
        tuneGrid = tune.grid,
        verbose = TRUE
        #preProcess=c("scale", "pca")
        
    )
    
    pred <<- predict(fit, testing)
}

##############################











