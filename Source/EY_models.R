#EY Data Science Competition
#Models List

##############################
#library

library(stats4)
library(mvtnorm)

##############################
#Global Variables




##############################
#User Defined Functions

simple_lag_1_classifier = function(input_attri){
    
    est_x = input_attri[["last_x"]]
    est_y = input_attri[["last_y"]]
    
    result = ifelse(abs(est_x) <= target_x_dev & abs(est_y) <= target_y_dev, 1, 0)
    
    return(result)
}

simple_lag_1_classifier_v2 = function(input_attri){
    
    est_x = input_attri[["adj_home_x"]]
    est_y = input_attri[["adj_home_y"]]
    
    result = ifelse(abs(est_x) <= target_x_dev & abs(est_y) <= target_y_dev, 1, 0)
    
    return(result)
}


##############################
#Naval ship classifier

NSC_v1 = function(vel, input_attri, cutoff = 0.5){
    
    result = mapply(NSC_probability, vel, cutoff,
                    input_attri[["last_x"]],input_attri[["last_y"]],input_attri[["delta_time"]])
    
    return(result)
}

NSC_probability = function(vel, cutoff, last_x, last_y, time){
    
    sample_size = 0.01
    sample_seq = seq(0,2-sample_size, sample_size)
    vel_x = vel
    vel_y = vel
    
    prob_x = cospi(sample_seq) * vel_x * time
    prob_y = sinpi(sample_seq) * vel_y * time
    
    est_x = last_x + prob_x
    est_y = last_y + prob_y
    
    probability = sum(ifelse(abs(est_x) <= target_x_dev & abs(est_y) <= target_y_dev, sample_size, 0))
    return(ifelse(probability >= cutoff, 1, 0))
}

NSC_train = function(input_attri, cutoff){
    
    fact = input_attri[["result_class"]]
    
    opt_fun = function(x){
        x1 = x
        x2 = cutoff
        
        pred = NSC_v1(x1, input_attri, cutoff = x2)
        score = 1-F1Score(fact, pred)$F1score
        
        return(score)
    }
    
    opt_val = optimize(opt_fun, c(0,100))
    
    return(opt_val)
}

NSC_train_2 = function(input_attri, vel){
    
    fact = input_attri[["result_class"]]
    
    opt_fun = function(x){
        x1 = vel
        x2 = x
        
        pred = NSC_v1(x1, input_attri, cutoff = x2)
        score = 1-F1Score(fact, pred)$F1score
        
        return(score)
    }
    
    opt_val = optimize(opt_fun, c(0,1))
    
    return(opt_val)
}

NSC_train_plot = function(input_attri){
    #parameters
    sample_seq = seq(30,100,10)
    
    #variable
    score = vector()
    fact = input_attri[["result_class"]]
    
    #progress bar
    pb = startpb(sample_seq[1], sample_seq[length(sample_seq)])
    on.exit(closepb(pb))
    
    for(i in sample_seq){
        pred = NSC_v1(vel=i, input_attri)
        score = c(score, F1Score(fact, pred)$F1score)
        
        setpb(pb, i)
    }
    
    return(score)
}

NSC_train_iterate = function(vel_ini, cut_ini, iteration, input_attri){
    vel = vel_ini
    cut = cut_ini
    
    #progress bar
    pb = startpb(0, iteration)
    on.exit(closepb(pb))
    
    for(i in 1:iteration){
        vel = NSC_train(input_attri, cut)$minimum
        cut = NSC_train_2(input_attri, vel)$minimum
        
        setpb(pb, i)
    }
    
    return(list(vel=vel, cut_off=cut))
}

NSC_train_results = function(){
    NSC_single_vel <<- 11.22488
    NSC_single_cut <<- 0.3732406
    
    NSC_vel <<- 0.02753093
    NSC_cut <<- 0.4960428
}

##############################
#Naval ship classifier v2

NSC_v2 = function(x_pos, x_neg, y_pos, y_neg, input_attri, cut_off=0.5){
    
    result = mapply(NSC_v2_probability, x_pos, x_neg, y_pos, y_neg,
                    input_attri[["last_x"]],input_attri[["last_y"]],input_attri[["delta_time"]], cut_off)
    
    return(result)
}

NSC_v2_probability = function(x_pos, x_neg, y_pos, y_neg, last_x, last_y, time, cut_off){
    
    sample_size = 0.01
    sample_seq_Q1 = seq(0,0.5-sample_size, sample_size)
    sample_seq_Q2 = seq(0.5,1-sample_size, sample_size)
    sample_seq_Q3 = seq(1,1.5-sample_size, sample_size)
    sample_seq_Q4 = seq(1.5,2-sample_size, sample_size)
    
    prob_x_Q1 = cospi(sample_seq_Q1) * x_pos * time
    prob_y_Q1 = sinpi(sample_seq_Q1) * y_pos * time
    
    prob_x_Q2 = cospi(sample_seq_Q2) * x_neg * time
    prob_y_Q2 = sinpi(sample_seq_Q2) * y_pos * time
    
    prob_x_Q3 = cospi(sample_seq_Q3) * x_neg * time
    prob_y_Q3 = sinpi(sample_seq_Q3) * y_neg * time
    
    prob_x_Q4 = cospi(sample_seq_Q4) * x_pos * time
    prob_y_Q4 = sinpi(sample_seq_Q4) * y_neg * time
    
    est_x = last_x + c(prob_x_Q1, prob_x_Q2, prob_x_Q3, prob_x_Q4)
    est_y = last_y + c(prob_y_Q1, prob_y_Q2, prob_y_Q3, prob_y_Q4)
    
    probability = sum(ifelse(abs(est_x) <= target_x_dev & abs(est_y) <= target_y_dev, sample_size, 0))
    return(ifelse(probability >= cut_off, 1, 0))
}

NSC_v2_train = function(input_attri, x, y, cut, i){
    
    fact = input_attri[["result_class"]]
    
    opt_fun = function(par){
        if(i==1){x = par}
        else if(i==2){y = par}
        else{cut = par}
        
        pred = NSC_v2(x,x,y,y, input_attri, cut)
        score = 1-F1Score(fact, pred)$F1score
        
        return(score)
    }

    if(i==3){
        opt_val = optimize(opt_fun, c(0,1))
    }
    else{
        opt_val = optimize(opt_fun, c(0,100))
    }
    
    return(opt_val)
}

NSC_v2_train_iterate = function(x, y, cut, iteration, slow=1, input_attri){
    
    #progress bar
    pb = startpb(0, iteration)
    on.exit(closepb(pb))
    
    for(i in 1:iteration){
        new_x = NSC_v2_train(input_attri, x, y, cut, 1)$minimum
        new_y = NSC_v2_train(input_attri, x, y, cut, 2)$minimum
        
        x = approx(c(0,1),c(x,new_x),xout=slow)$y
        y = approx(c(0,1),c(y,new_y),xout=slow)$y
        
        new_cut = NSC_v2_train(input_attri, x, y, cut, 3)$minimum
        
        cut = approx(c(0,1),c(cut,new_cut),xout=slow)$y
        
        print(F1Score(input_attri[["result_class"]], NSC_v2(x,x,y,y, input_attri, cut))$F1score)
        
        setpb(pb, i)
    }
    
    return(list(x=x, y=y, cutoff=cut))
}

NSC_v2_train_2 = function(input_attri, x1, x2, y1, y2, cut, i){
    
    fact = input_attri[["result_class"]]
    
    opt_fun = function(x){
        if(i==1){x1 = x}
        else if(i==2){x2 = x}
        else if(i==3){y1 = x}
        else if(i==4){y2 = x}
        else{cut = x}
        
        pred = NSC_v2(x1,x2,y1,y2, input_attri, cut)
        score = 1-F1Score(fact, pred)$F1score
        
        return(score)
    }
    
    if(i==5){
        opt_val = optimize(opt_fun, c(0,1))
    }
    else{
        opt_val = optimize(opt_fun, c(0,100))
    }
    
    return(opt_val)
}

NSC_v2_train_iterate_2 = function(x1, x2, y1, y2, cut, iteration, slow=1, input_attri){
    
    #progress bar
    pb = startpb(0, iteration)
    on.exit(closepb(pb))
    
    for(i in 1:iteration){
        new_x1 = NSC_v2_train_2(input_attri, x1, x2, y1, y2, cut, 1)$minimum
        new_x2 = NSC_v2_train_2(input_attri, x1, x2, y1, y2, cut, 2)$minimum
        new_y1 = NSC_v2_train_2(input_attri, x1, x2, y1, y2, cut, 3)$minimum
        new_y2 = NSC_v2_train_2(input_attri, x1, x2, y1, y2, cut, 4)$minimum
        
        x1 = approx(c(0,1),c(x1,new_x1),xout=slow)$y
        x2 = approx(c(0,1),c(x2,new_x2),xout=slow)$y
        y1 = approx(c(0,1),c(y1,new_y1),xout=slow)$y
        y2 = approx(c(0,1),c(y2,new_y2),xout=slow)$y
        
        new_cut = NSC_v2_train_2(input_attri, x1, x2, y1, y2, cut, 5)$minimum
        
        cut = approx(c(0,1),c(cut,new_cut),xout=slow)$y
        
        print(F1Score(input_attri[["result_class"]], NSC_v2(x1,x2,y1,y2, input_attri, cut))$F1score)
        
        setpb(pb, i)
    }
    
    return(list(x1=x1, x2=x2, y1=y1, y2=y2, cutoff=cut))
}

NSC_v2_train_results = function(){
    NSC_v2_single_vel <<- c(11.0707,22.38097)
    NSC_V2_single_cut <<- 0.4097269
    
    NSC_v2_single_vel_2 <<- c(12.84876,11.12087,44.40684,36.588)
    NSC_V2_single_cut_2 <<- 0.4108631
    
    NSC_v2_vel <<- c(0,0,0,0)
}

##############################

NSC_v3 = function(pred_x, pred_y, pred_sd_x, pred_sd_y){
    
    probability = mapply(NSC_v3_probability, pred_x, pred_y, pred_sd_x, pred_sd_y)
    
    return(probability)
}

NSC_v3_probability = function(pred_x, pred_y, pred_sd_x, pred_sd_y){
    
    var = diag(2)
    var[1,1] = pred_sd_x ^ 2
    var[2,2] = pred_sd_y ^ 2
    
    prob = pmvnorm(
        lower = c(-target_x_dev, -target_y_dev),
        upper = c(target_x_dev, target_y_dev),
        mean = c(pred_x, pred_y),
        sigma = var
    )
    
    return(prob[1])
}

NSC_v3_cutoff = function(probability, cutoff){
    return(ifelse(probability >= cutoff, 1, 0))
}

##############################
#Application




























