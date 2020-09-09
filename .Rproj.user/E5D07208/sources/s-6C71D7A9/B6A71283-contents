#EY Data Science Competition
#Data Cleaning V1

##############################
#library



##############################
#Global Variables


##############################
#Data Cleaning V1

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
    
    data_list = list()
    attri_list = data.frame(
        n_entry = integer(),
        stationary_entry = integer(),
        wake_up_time = integer(),
        wake_up_sec = integer(),
        adj_home_x = numeric(),
        adj_home_y = numeric(),
        time_moving = integer(),
        result_x = numeric(),
        result_x = numeric(),
        result_class = integer()
    )
    
    list_index = 1
    start = 1
    end = 1
    current_hash = raw_list[1,"hash"]
    total_length = nrow(raw_list)
    
    for(i in 1:total_length){
        if(current_hash != raw_list[i,"hash"] || i == total_length){
            end = i-1
            if (i == total_length){
                end = i
            }
            
            #data
            data_list[list_index] = list(raw_list[start:end,])
            
            #attributes
            n_entry = end - start + 1
            stationary_entry = sum(unlist(raw_list[start:end,"stationary"]))
            
            wake_up_time = raw_list[[start,"time_entry"]]
            wake_up_sec = raw_list[[start,"sec_entry"]]
            adj_home_x = raw_list[[start,"adj_x_entry"]]
            adj_home_y = raw_list[[start,"adj_y_entry"]]
            
            # total_distance = sum(raw_list[start:end-1,"delta_total"])
            # x_displacement = sum(raw_list[start:end-1,"delta_x"])
            # y_displacement = sum(raw_list[start:end-1,"delta_y"])
            # total_displacement = sqrt(x_displacement^2 + y_displacement^2)
            time_moving = sum(raw_list[start:end,"delta_sec"])
            
            last_x = raw_list[[end,"adj_x_entry"]]
            last_y = raw_list[[end,"adj_y_entry"]]
            result_x = raw_list[[end,"adj_x_exit"]]
            result_y = raw_list[[end,"adj_y_exit"]]
            result_class = ifelse(abs(result_x) < target_x_dev && abs(result_y) < target_y_dev, 1, 0)
            
            attri_list = rbind(attri_list, data.frame(
                n_entry = n_entry,
                stationary_entry = stationary_entry,
                
                wake_up_time = wake_up_time,
                wake_up_sec = wake_up_sec,
                adj_home_x = adj_home_x,
                adj_home_y = adj_home_y,
                
                time_moving = time_moving,
                
                last_x = last_x,
                last_y = last_y,
                result_x = result_x,
                result_y = result_y,
                result_class = result_class
            ))
            
            #iterate list
            current_hash = raw_list[i,"hash"]
            start = i
            list_index = list_index + 1
        }
        loading_bar(i,total_length)
    }
    
    return(list(data_list = data_list, attri_list = attri_list))
}

##############################
# EXport Result V1

export_result_full = function(){
    print("export_result_full")
    
    output_result_list <<- data.frame(id = character(), target = integer())
    
    for(i in 1:33515){
        current_id = test_list[[i]][nrow(test_list[[i]]),"trajectory_id"]
        current_result = final_result[i]
        
        current_frame = data.frame(id = current_id, target = current_result)
        output_result_list <<- rbind(output_result_list, current_frame)
        
        loading_bar(i,33515)
    }
    
    colnames(output_result_list) <<- c("id", "target")
    write.table(output_result_list, file = "Result.csv", quote = F, sep = ",", row.names = F)
}

prep_export_result_quick = function(){
    print("prep_export_result_quick")
    
    output_result_list <<- data.frame(id = character(), target = integer())
    
    for(i in 1:33515){
        current_id = test_list$data_list[[i]][nrow(test_list$data_list[[i]]),"trajectory_id"]
        current_result = -1
        
        current_frame = data.frame(id = current_id, target = current_result)
        output_result_list <<- rbind(output_result_list, current_frame)
        
        loading_bar(i,33515)
    }
    colnames(output_result_list) <<- c("id", "target")
}

export_result_quick = function(){
    for(i in 1:33515){
        output_result_list[i,2] = final_result[i]
    }
    write.table(output_result_list, file = "Result.csv", quote = F, sep = ",", row.names = F)
}

##############################
#How to use

data_cleaning = function(){
    addit_train <<- additional_raw_data(raw_train)
    addit_test <<- additional_raw_data(raw_test)
    
    train_list <<- generate_data_list(addit_train)
    test_list <<- generate_data_list(addit_test)
    
    
    prep_export_result_quick()

    final_result <<- rep(0,33515)
}

simple_initiate = function(){
    simple_addit_train <<- additional_raw_data(raw_train[1:2004,])
    simple_train_list <<- generate_data_list(simple_addit_train)
}

##############################































