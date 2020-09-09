#EY Data Science Competition
#Ideas List

##############################
#library


##############################
#Global Variables


##############################
#User Defined Functions




##############################


para_to_xy = function(last_x, last_y, distance, degree){
    
    degree = degree /180 * pi
    
    delta_x = distance * cos(degree)
    delta_y = distance * sin(degree)
    
    est_x = last_x + delta_x
    est_y = last_y + delta_y
    
    result = ifelse(abs(est_x) <= target_x_dev & abs(est_y) <= target_y_dev, 1, 0)
    
    return(result)
}



line_prob = function(last_x, last_y, time, degree, dis_val, dis_prob){
    
    prob = 0
    
    for(i in 1:201){
        est_dis = dis_val[i] * time
        prob = prob + dis_prob[i] * para_to_xy(last_x, last_y, est_dis, degree)
    }
    
    return(prob)
    
}


disk_prob = function(last_x, last_y, time, degree, deg_val, deg_prob, idea_vel, idea_prob){
    
    prob = 0

    for(i in 1:43){
        est_deg = degree + deg_val[i]
        
        dis_val = idea_vel[[i]]
        dis_prob = idea_prob[[i]]
        prob = prob + (deg_prob[i]/2) * line_prob(last_x, last_y, time, est_deg, dis_val, dis_prob)
    }
    for(i in 1:43){
        est_deg = degree - deg_val[i]
        
        dis_val = idea_vel[[i]]
        dis_prob = idea_prob[[i]]
        prob = prob + (deg_prob[i]/2) * line_prob(last_x, last_y, time, est_deg, dis_val, dis_prob)
    }
    
    return(prob)
}



##############################

time_set_1 = train_attri$wake_up_time >= 0 & train_attri$wake_up_time < 10000
time_set_2 = train_attri$wake_up_time >= 10000 & train_attri$wake_up_time < 20000
time_set_3 = train_attri$wake_up_time >= 20000 & train_attri$wake_up_time < 30000
time_set_4 = train_attri$wake_up_time >= 30000 & train_attri$wake_up_time < 40000
time_set_5 = train_attri$wake_up_time >= 40000

idea_set = good & time_set_5

idea_train = train_attri[idea_set,]
idea_result = train_result[idea_set,]

train_model = function(){

    dir_home_diff <<- mapply(fun_1, idea_train$home_direction, idea_result$result_direction)
    dir_home_diff <<- mapply(fun_1, dir_home_diff, 180)
    
    
    a0 <<- c(
        0.00000000, 
        0.03785086,
        0.13133691,
        0.32400186,
        0.66666576,
        1.16463571,
        1.91580432,
        2.95264157,
        4.32297129,
        6.22510368
        )
    
    a1 <<- seq(10,15,5)
    a2 <<- seq(20,150,5)
    a3 <<- seq(160,180,5)
    
    #a0 <<- seq(0,3,0.5)
    
    deg_bound <<- c(a0,a1,a2,a3)
    
    deg_prob <<- c()
    deg_val <<- c()
    deg_set <<- list()
    for(i in 1:43){
        deg_val[i] <<- (deg_bound[i+1] + deg_bound[i])/2
        deg_set[i] <<- list(abs(dir_home_diff) > deg_bound[i] & abs(dir_home_diff) <= deg_bound[i+1])
        deg_prob[i] <<- mean(abs(dir_home_diff) > deg_bound[i] & abs(dir_home_diff) <= deg_bound[i+1])
    }
    deg_val[1] <<- 0
    deg_val[43] <<- 180
    
    
    ##############################
    
    idea_vel <<- list()
    idea_prob <<- list()
    
    
    for(i in 1:43){
    
        b0 <<- seq(0,20,1)
        
        vel_bound <<- b0 ^ 2
        
        vel_prob <<- c()
        vel_val <<- c()
        for(j in 1:20){
            vel_val[j] <<- (vel_bound[j+1] + vel_bound[j])/2
            vel_prob[j] <<- mean(idea_result[deg_set[[i]],]$vel > vel_bound[j] & idea_result[deg_set[[i]],]$vel <= vel_bound[j+1])
        }
        
        
        b1 <<- seq(0,20,0.1)
        
        vel_val_2 <<- b1 ^ 2
        vel_prob_2 <<- approx(c(0,vel_val,400),c(vel_prob[1],vel_prob,vel_prob[20]), xout=vel_val_2)$y
        vel_prob_2 <<- vel_prob_2 / sum(vel_prob_2)
    
        idea_vel[i] <<- list(vel_val_2)
        idea_prob[i] <<- list(vel_prob_2)
    
    }
    
}


##############################

time = test_attri$wake_up_time >= 40000
test_set = test_attri$delta_time >= 400 & time

last_x = test_attri[test_set,]$last_x
last_y = test_attri[test_set,]$last_y
time = test_attri[test_set,]$delta_time
degree = test_attri[test_set,]$direction

idea_run_1 = function(last_x, last_y, time, degree){

    ans = disk_prob(last_x, last_y, time, degree, deg_val, deg_prob, idea_vel, idea_prob)
    return(ans)
}

idea_run = function(){
    prob_5 <<- pbmapply(idea_run_1, last_x, last_y, time, degree)
}


##############################





























