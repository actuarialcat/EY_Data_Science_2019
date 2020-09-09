#EY Data Science Competition
#Model Analytic

##############################
#library


##############################
#Global Variables


##############################
#User Defined Functions

ana_time = function(){
    f1 = function(inp){
        o <<- (train_attri$delta_time > inp-200) & (train_attri$delta_time <= inp)
        
        r1 = F1Score(train_result$result_class[o], simple_lag_1_classifier(train_attri)[o])
        return(r1$F1score)
    }
    
    
    x = seq(100,5000,200) 
    
    y = sapply(x,f1)
    
    plot(x,y, type="o")
    abline(h = 0.7222253, col="blue")
}

ana_n = function(){
    f1 = function(inp){
        o <<- (train_attri$n_entry > inp-1) & (train_attri$n_entry <= inp)
        
        r1 = F1Score(train_result$result_class[o], simple_lag_1_classifier(train_attri)[o])
        return(r1$F1score)
    }
    
    
    x = seq(2,20,1) 
    
    y = sapply(x,f1)
    
    plot(x,y, type="o")
    abline(h = 0.7222253, col="blue")
}

ana_wake = function(){
    f1 = function(inp){
        o <<- (train_attri$wake_up_time > inp-1800) & (train_attri$wake_up_time <= inp)
        
        r1 = F1Score(train_result$result_class[o], simple_lag_1_classifier(train_attri)[o])
        return(r1$F1score)
    }
    
    
    x = seq(1800,54000,1800) 
    
    y = sapply(x,f1)
    
    plot(x,y, type="o")
    abline(h = 0.7222253, col="blue")
}

ana_toltime = function(){
    f1 = function(inp){
        o <<- (train_attri$total_time_last > inp-1800) & (train_attri$total_time_last <= inp)
        
        r1 = F1Score(train_result$result_class[o], simple_lag_1_classifier(train_attri)[o])
        return(r1$F1score)
    }
    
    
    x = seq(1800,57600,1800) 
    
    y = sapply(x,f1)
    
    plot(x,y, type="o")
    abline(h = 0.7222253, col="blue")
}


































