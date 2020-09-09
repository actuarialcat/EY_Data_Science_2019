#EY Data Science Competition
#Data Selection

##############################
#library


##############################
#Global Variables


##############################
#User Defined Functions


sort_out_outliers = function(){
    total = 66252

    vel_x = abs(train_result$vel_x)<=40
    vel_y = abs(train_result$vel_y)<=500
    delta_time = train_attri$delta_time <= 4500
    
    
    
    good1 <<- vel_x & vel_y & delta_time
}

sort_out_outliers_sd = function(){

    vel_x = abs(train_result$vel_x)<=140
    vel_y = abs(train_result$vel_y)<=740
    delta_time = train_attri$delta_time <= 6000 #4800
    
    
    
    
    good1 <<- vel_x & vel_y & delta_time
}

sort_out_useful = function(){
    sort_out_outliers()
    
    delta = train_attri$delta_time >= 400
    
    good <<- good1 & delta
    
    x_stat <<- abs(train_result[[1]])<100
    y_stat <<- abs(train_result[[2]])<1000
    
}

sort_1 = function(){
    set_01 = train_attri$delta_time < 1500
    set_02 = train_attri$delta_time > 1500
}



















