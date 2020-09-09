#EY Data Science Competition
#Ideas List

##############################
#library

library(MASS)

##############################
#Global Variables


##############################
#User Defined Functions

box_cox_trans = function(x, lambda){
    if(lambda == 0){
        return(log(x))
    }
    else{
        return((x^lambda - 1) / lambda)
    }
}

box_cox_inv = function(x, lambda){
    if(lambda == 0){
        return(exp(x))
    }
    else{
        return((x*lambda + 1)^(1/lambda))
    }
}


#############################

idea = function(i, cut, input_attri = train_attri, input_result = train_result){
    inTrain <<- createDataPartition(
        y = input_result$result_class,
        p = 0.7,
        list = F
    )
    
    d_var <<- sqrt(input_result$last_delta_x^2 + input_result$last_delta_y^2)
    
    y <<- d_var[inTrain]
    x <<- input_attri$delta_time[inTrain]
    
    test_y <<- d_var[-inTrain]
    test_x <<- input_attri$delta_time[-inTrain]
    
    y <<- box_cox_trans(y+1, 0)
    x <<- box_cox_trans(x, 0)
    
    fit_mod <<- lm(y ~ x)
    
    y2 <<- box_cox_trans(test_y+1, 0)
    x2 <<- box_cox_trans(test_x, 0)
    
    y3 <<- predict(fit_mod, data.frame(x = x2))
    
    train_MSE <<- mean(fit_mod$residuals^2)
    test_MSE <<- mean(y2^2-y3^2)
    
    res_y <<- box_cox_inv(y3, 0)
    
    pred_x = input_attri$last_x[-inTrain]
    pred_y = input_attri$last_y[-inTrain]
    sd = res_y
    
    prob <<- NSC_v3(pred_x, pred_y, sd, sd)
        
    x = F1Score(input_result$result_class[-inTrain], NSC_v3_cutoff(prob, cut))
    return(x$F1score)
}

f1 = function(x){
    return(x = mean(sapply(1:50, idea, x)))
}

f2 = function(){
    pos_value <<- seq(0.10,0.30,0.02)
    
    x = pbsapply(pos_value, f1)
    print(x)
}

appli = function(){
    d_var <<- sqrt(inp_result$last_delta_x^2 + inp_result$last_delta_y^2)
    
    y <<- d_var
    x <<- inp_attri$delta_time
    
    test_x <<- inp_attri$delta_time
    
    y <<- box_cox_trans(y+1, 0.25)
    x <<- box_cox_trans(x, 0.25)
    
    fit_mod <<- lm(y ~ x)
    
    x2 <<- box_cox_trans(test_x, 0.25)
    
    y3 <<- predict(fit_mod, data.frame(x = x2))
    
    res_y <<- box_cox_inv(y3, 0.25)
    
    pred_x = inp_attri$last_x
    pred_y = inp_attri$last_y
    sd = res_y
    
    prob <<- NSC_v3(pred_x, pred_y, sd, sd)
    
    result <<- NSC_v3_cutoff(prob, 0.14)
}

#############################

inp_attri = train_attri[good,]
inp_result = train_result[good,]

print(F1Score(inp_result$result_class, simple_lag_1_classifier(inp_attri)))





























































