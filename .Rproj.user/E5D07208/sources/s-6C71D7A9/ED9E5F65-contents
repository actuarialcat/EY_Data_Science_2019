#EY Data Science Competition
#Data Cleaning V2

##############################
#library

library(pbapply)

##############################
#Global Variables


##############################
#Data Cleaning V2

additional_raw_data = function(raw_list){
    print("additional_raw_data")
    
    total_length = nrow(raw_list)
    
    raw_list["sec_entry"] = as.numeric(raw_list[["time_entry"]])
    raw_list["sec_exit"] = as.numeric(raw_list[["time_exit"]])
    raw_list["delta_sec"] = raw_list["sec_exit"] - raw_list["sec_entry"]
    
    raw_list["delta_x"] = raw_list["x_exit"] - raw_list["x_entry"]
    raw_list["delta_y"] = raw_list["y_exit"] - raw_list["y_entry"]
    raw_list["delta_total"] = sqrt(raw_list["delta_x"]^2 + raw_list["delta_y"]^2)
    raw_list["direction"] = atan2(raw_list[["delta_y"]],raw_list[["delta_x"]])/pi*180
    
    raw_list["vel_x"] = raw_list["delta_x"] / raw_list["delta_sec"]
    raw_list["vel_y"] = raw_list["delta_y"] / raw_list["delta_sec"]
    raw_list["vel_total"] = raw_list["delta_total"] / raw_list["delta_sec"]
    
    raw_list["adj_x_entry"] = raw_list["x_entry"] - target_x_mid
    raw_list["adj_x_exit"] = raw_list["x_exit"] - target_x_mid
    raw_list["adj_y_entry"] = raw_list["y_entry"] - target_y_mid
    raw_list["adj_y_exit"] = raw_list["y_exit"] - target_y_mid
    
    raw_list["vmax"] = raw_list["vmax"] * ifelse(raw_list["vmax"] < 0 & !is.na(raw_list["vmax"]), NA, 1)
    raw_list["vmin"] = raw_list["vmin"] * ifelse(raw_list["vmin"] < 0 & !is.na(raw_list["vmin"]), NA, 1)
    raw_list["vmean"] = raw_list["vmean"] * ifelse(raw_list["vmean"] < 0 & !is.na(raw_list["vmean"]), NA, 1)
    raw_list["avail_v"] = ifelse(is.na(raw_list["vmax"])|is.na(raw_list["vmin"])|is.na(raw_list["vmean"]), FALSE, TRUE)
    
    raw_list["stationary"] = ifelse((raw_list["delta_sec"] == 0), TRUE, FALSE)
    
    return(raw_list)
}

generate_data_list = function(raw_list){
    print("generate_data_list")
    
    data_list_stationary = list()
    data_list_single = list()
    data_list_useful = list()
    
    attri_list_stationary = tibble()
    attri_list_single = tibble()
    attri_list_useful = tibble()
    
    list_index_stationary = 1
    list_index_single = 1
    list_index_useful = 1
    
    start = 1
    end = 1
    current_hash = raw_list[1,"hash"]
    total_length = nrow(raw_list)
    
    #progress bar
    pb = startpb(0, total_length)
    on.exit(closepb(pb))
    
    for(i in 1:total_length){
        if(current_hash != raw_list[i,"hash"] || i == total_length){
            end = i-1
            if (i == total_length){
                end = i
            }
            
            #attributes
            n_entry = end - start + 1
            
            #add to list
            if(raw_list[[end,"stationary"]]){
                #data
                data_list_stationary[list_index_stationary] = list(raw_list[start:end,])
                list_index_stationary = list_index_stationary + 1
            }
            else if(n_entry == 1){
                #data
                data_list_single[list_index_single] = list(raw_list[start:end,])
                list_index_single = list_index_single + 1
            }
            else{
                #data
                data_list_useful[list_index_useful] = list(raw_list[start:end,])
                list_index_useful = list_index_useful + 1
            }
            
            
            #iterate list
            current_hash = raw_list[i,"hash"]
            start = i
        }
        setpb(pb, i)
    }
    
    return(list(data_list_stationary = data_list_stationary, data_list_useful = data_list_useful,
                data_list_single = data_list_single))
}

generate_attri_tibble = function(data_list){
    print("generate_attri_tibble")
    
    #attributes
    traj_name = sapply(data_list,function(x){(tail(x[["trajectory_id"]],n=1))})
    n_entry = sapply(data_list,nrow)
    stationary_entry = sapply(data_list,function(x){sum(x[["stationary"]])})
    
    wake_up_time = sapply(data_list,function(x){(x[["time_entry"]])[1]})
    # wake_up_sec = raw_list[[start,"sec_entry"]]
    adj_home_x = sapply(data_list,function(x){(x[["adj_x_entry"]])[1]})
    adj_home_y = sapply(data_list,function(x){(x[["adj_y_entry"]])[1]})
    
    sec_last_time = sapply(data_list,function(x){(tail(x[["time_exit"]],n=2)[1])})
    sec_last_x = sapply(data_list,function(x){(tail(x[["adj_x_exit"]],n=2)[1])})
    sec_last_y = sapply(data_list,function(x){(tail(x[["adj_y_exit"]],n=2)[1])})
    
    last_time = sapply(data_list,function(x){(tail(x[["time_entry"]],n=1))})
    last_x = sapply(data_list,function(x){(tail(x[["adj_x_entry"]],n=1))})
    last_y = sapply(data_list,function(x){(tail(x[["adj_y_entry"]],n=1))})
    
    delta_x = last_x - sec_last_x
    delta_y = last_y - sec_last_y
    delta = sqrt(delta_x^2 + delta_y^2)
    
    result_time = sapply(data_list,function(x){(tail(x[["time_exit"]],n=1))})
    delta_time = result_time - last_time
    
    vel_x = delta_x / delta_time
    vel_y = delta_y / delta_time
    vel = sqrt(vel_x^2 + vel_y^2)
    
    direction = atan2(delta_y, delta_x)/pi*180
    
    x_displacement = last_x - adj_home_x
    y_displacement = last_y - adj_home_y
    total_displacement = sqrt(x_displacement^2 + y_displacement^2)
    home_direction = atan2(y_displacement, x_displacement)/pi*180
    
    total_time_last = last_time - wake_up_time
    time_moving = sapply(data_list,function(x){sum(x[["delta_sec"]])})
    
    #Generate tibble
    attri_tibble = tibble(
        traj_name = traj_name,
        n_entry = n_entry,
        stationary_entry = stationary_entry,
        
        wake_up_time = wake_up_time,
        # wake_up_sec = wake_up_sec,
        adj_home_x = adj_home_x,
        adj_home_y = adj_home_y,
        
        sec_last_time = sec_last_time,
        sec_last_x = sec_last_x,
        sec_last_y = sec_last_y,
        
        last_time = last_time,
        last_x = last_x,
        last_y = last_y,
        
        delta_x = delta_x,
        delta_y = delta_y,
        delta = delta,
        
        vel_x = vel_x,
        vel_y = vel_y,
        vel = vel,
        
        direction = direction,
        
        delta_time = delta_time,
        result_time = result_time,
        
        x_displacement = x_displacement,
        y_displacement = y_displacement,
        total_displacement = total_displacement,
        home_direction = home_direction,
        
        total_time_last = total_time_last,
        time_moving = time_moving
    )
    return(attri_tibble)
}

generate_result_tibble = function(data_list){
    print("generate_result_tibble")
    
    last_time = sapply(data_list,function(x){(tail(x[["time_entry"]],n=1))})
    result_time = sapply(data_list,function(x){(tail(x[["time_exit"]],n=1))})
    delta_time = result_time - last_time
    
    last_delta_x = sapply(data_list,function(x){(tail(x[["delta_x"]],n=1))})
    last_delta_y = sapply(data_list,function(x){(tail(x[["delta_y"]],n=1))})
    last_delta = sqrt(last_delta_x^2 + last_delta_y^2)
    
    vel_x = last_delta_x / delta_time
    vel_y = last_delta_y / delta_time
    vel = sqrt(vel_x^2 + vel_y^2)
    result_direction = sapply(data_list,function(x){(tail(x[["direction"]],n=1))})
    
    result_x = sapply(data_list,function(x){(tail(x[["adj_x_exit"]],n=1))})
    result_y = sapply(data_list,function(x){(tail(x[["adj_y_exit"]],n=1))})
    result_class = ifelse(abs(result_x) <= target_x_dev & abs(result_y) <= target_y_dev, 1, 0)
    
    result_tibble = tibble(
        last_delta_x = last_delta_x,
        last_delta_y = last_delta_y,
        last_delta = last_delta,
        
        vel_x = vel_x,
        vel_y = vel_y,
        vel = vel,
        result_direction = result_direction,
        
        result_x = result_x,
        result_y = result_y,
        result_class = result_class
    )
    return(result_tibble)
}

generate_time_xy_list = function(data_tibble){
    total_length = nrow(data_tibble)
    
    time_xy = tibble()
    
    for(i in 1:total_length){
        
        new_row = tibble(
            time = data_tibble[[i,"time_entry"]],
            x = data_tibble[[i,"adj_x_entry"]],
            y = data_tibble[[i,"adj_y_entry"]]
        )
        time_xy = rbind(time_xy, new_row)
        
        if(data_tibble[[i,"stationary"]] == FALSE && i != total_length){
            new_row = tibble(
                time = data_tibble[[i,"time_exit"]],
                x = data_tibble[[i,"adj_x_exit"]],
                y = data_tibble[[i,"adj_y_exit"]]
            )
            time_xy = rbind(time_xy, new_row)
        }
    }
    
    return(time_xy)
}

##############################

interpolate_xy = function(data_tibble){
    #15:00:00 = 54000 secs
    #16:00:00 = 57600 secs
    
    sample_time = seq(0,54000,1800)
    
    track_xy = tibble(time = sample_time)
    track_xy = cbind(track_xy, x = approx(c(0,data_tibble[["time"]],54000),c(data_tibble[[1,"x"]],data_tibble[["x"]],tail(data_tibble[["x"]], n=1)),xout=sample_time)$y)
    track_xy = cbind(track_xy, y = approx(c(0,data_tibble[["time"]],54000),c(data_tibble[[1,"y"]],data_tibble[["y"]],tail(data_tibble[["y"]], n=1)),xout=sample_time)$y)
    track_xy = cbind(track_xy, vel_x = c(NA, diff(track_xy[["x"]])))
    track_xy = cbind(track_xy, vel_y = c(NA, diff(track_xy[["y"]])))
    track_xy = cbind(track_xy, raw_x = approx(data_tibble[["time"]],data_tibble[["x"]],xout=sample_time)$y)
    track_xy = cbind(track_xy, raw_y = approx(data_tibble[["time"]],data_tibble[["y"]],xout=sample_time)$y)
    
    return(track_xy)
}

extract_vel = function(data_tibble){
    return(cbind(rbind(data_tibble[2:31,"vel_x"]),rbind(data_tibble[2:31,"vel_y"])))
}

generate_vel_tibble = function(data_list){
    
    vel_tibble = as_tibble(t(sapply(data_list, extract_vel)))
    names(vel_tibble) = c(paste("vel_x", 1:30, sep=""), paste("vel_y", 1:30, sep=""))
    
    return(vel_tibble)
}

extract_raw = function(data_tibble){
    return(cbind(rbind(data_tibble[1:31,"raw_x"]),rbind(data_tibble[1:31,"raw_y"])))
}

generate_raw_tibble = function(data_list){
    
    raw_tibble = as_tibble(t(sapply(data_list, extract_raw)))
    names(raw_tibble) = c(paste("raw_x", 0:30, sep=""), paste("raw_y", 0:30, sep=""))
    
    return(raw_tibble)
}

##############################

interpolate_xy_2 = function(data_tibble){
    #15:00:00 = 54000 secs
    #16:00:00 = 57600 secs
    
    final_time = as.numeric(tail(data_tibble[["time"]], n=1))
    
    sample_time = seq(final_time - 300*30 ,final_time,300)
    
    track_xy = tibble(time = sample_time)
    track_xy = cbind(track_xy, x = approx(c(0,data_tibble[["time"]]),c(data_tibble[[1,"x"]],data_tibble[["x"]]),xout=sample_time)$y)
    track_xy = cbind(track_xy, y = approx(c(0,data_tibble[["time"]]),c(data_tibble[[1,"y"]],data_tibble[["y"]]),xout=sample_time)$y)
    track_xy = cbind(track_xy, vel_x = c(NA, diff(track_xy[["x"]])))
    track_xy = cbind(track_xy, vel_y = c(NA, diff(track_xy[["y"]])))
    
    return(track_xy)
}

##############################
#Export Results

lag_1_result = function(){
    stationary_result <<- simple_lag_1_classifier(test_stationary_attri)
    single_result <<- simple_lag_1_classifier(test_single_attri)
    useful_result <<- simple_lag_1_classifier(test_attri)
}

create_result = function(){
    stationary_export = tibble(id=test_stationary_attri[["traj_name"]])
    stationary_export = cbind(stationary_export, tibble(target = stationary_result))
    
    single_export = tibble(id=test_single_attri[["traj_name"]])
    single_export = cbind(single_export, tibble(target = single_result))
    
    useful_export = tibble(id=test_attri[["traj_name"]])
    useful_export = cbind(useful_export, tibble(target = useful_result))
    
    export_file <<- rbind(stationary_export, single_export, useful_export)
}

export_result = function(){
    
    write.table(export_file, file = "Result.csv", quote = F, sep = ",", row.names = F)
}

##############################
#How to use

data_cleaning = function(){
    addit_train <<- additional_raw_data(raw_train)
    addit_test <<- additional_raw_data(raw_test)
    
    return_list = generate_data_list(addit_train)
    train_stationary_data <<- return_list$data_list_stationary
    train_single_data <<- return_list$data_list_single
    train_data <<- return_list$data_list_useful
    
    return_list = generate_data_list(addit_test)
    test_stationary_data <<- return_list$data_list_stationary
    test_single_data <<- return_list$data_list_single
    test_data <<- return_list$data_list_useful

    print("generate_time_xy: train")
    train_time <<- pblapply(train_data,generate_time_xy_list)
    print("generate_time_xy: test")
    test_time <<- pblapply(test_data,generate_time_xy_list)
    
}

create_attri = function(){
    
    train_stationary_attri <<- generate_attri_tibble(train_stationary_data)
    train_single_attri <<- generate_attri_tibble(train_single_data)
    train_attri <<- generate_attri_tibble(train_data)
    
    train_stationary_result <<- generate_result_tibble(train_stationary_data)
    train_single_result <<- generate_result_tibble(train_single_data)
    train_result <<- generate_result_tibble(train_data)
    
    test_stationary_attri <<- generate_attri_tibble(test_stationary_data)
    test_single_attri <<- generate_attri_tibble(test_single_data)
    test_attri <<- generate_attri_tibble(test_data)
}

create_track = function(){
    print("generate_track: train")
    train_track <<- pblapply(train_time,interpolate_xy)
    train_vel <<- generate_vel_tibble(train_track)
    train_pt <<- generate_raw_tibble(train_track)

    print("generate_track: test")
    test_track <<- pblapply(test_time,interpolate_xy)
    test_vel <<- generate_vel_tibble(test_track)
    test_pt <<- generate_raw_tibble(test_track)
    
}

create_track_2 = function(){
    print("generate_track_2: train")
    train_track_2 <<- pblapply(train_time,interpolate_xy_2)
    train_vel_2 <<- generate_vel_tibble(train_track_2)
    
    print("generate_track_2: test")
    test_track_2 <<- pblapply(test_time,interpolate_xy_2)
    test_vel_2 <<- generate_vel_tibble(test_track_2)
}

simple_initiate = function(){
    simple_addit_train <<- additional_raw_data(raw_train[1:20002,])
    
    return_list = generate_data_list(simple_addit_train)
    simple_train_stationary_data <<- return_list$data_list_stationary
    simple_train_single_data <<- return_list$data_list_single
    simple_train_data <<- return_list$data_list_useful

    simple_train_stationary_attri <<- generate_attri_tibble(simple_train_stationary_data)
    simple_train_single_attri <<- generate_attri_tibble(simple_train_single_data)
    simple_train_attri <<- generate_attri_tibble(simple_train_data)
    
    simple_train_stationary_result <<- generate_result_tibble(simple_train_stationary_data)
    simple_train_single_result <<- generate_result_tibble(simple_train_single_data)
    simple_train_result <<- generate_result_tibble(simple_train_data)
    
    print("generate_time_xy: simple_train")
    simple_train_time <<- pblapply(simple_train_data,generate_time_xy_list)
    
    print("generate_track: simple_train")
    simple_train_track <<- pblapply(simple_train_time,interpolate_xy)
    
    simple_train_vel <<- generate_vel_tibble(simple_train_track)
    
    
}



##############################







































