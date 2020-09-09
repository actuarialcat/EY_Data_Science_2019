#EY Data Science Competition
#Utilities Functions List

##############################
#library

library(tibble)
library(readr)


##############################
#Global Variables

loading_counter = 0

##############################
#User Defined Functions

loading_bar = function(i,total,interval=1000){
    # Visual aid on compuational progress
    
    if (loading_counter >= interval){
        cat(round(i/total*100,0),"% done!\n")
        loading_counter <<- 0
    }
    else{
        loading_counter <<- loading_counter + 1
    }
}

# clean_mem = function(interval=5000){
#     if(loading_counter >= interval){
#         gc()
#         #cat("Mem Clean!\n")
#         loading_counter <<- 0
#     }
#     else{
#         loading_counter <<- loading_counter + 1
#     }
# }

load_data = function(mac = F){
    # Load data from csv
    
    print("load_data")
    
    if(mac == T){
        raw_train <<- read_csv("~/Google Drive/University of Hong Kong/EY/data_train.csv")
        raw_test <<- read_csv("~/Google Drive/University of Hong Kong/EY/data_test.csv")
    }
    else{
        raw_train <<- read_csv("data_train.csv")
        raw_test <<- read_csv("data_test.csv")
    }
}

set_target_var = function(){
    # Load target value from question
    
    target_x_low <<- 3750901.5068
    target_x_up <<- 3770901.5068
    target_y_low <<- -19268905.6133
    target_y_up <<- -19208905.6133
    target_x_dev <<- (target_x_up-target_x_low)/2
    target_y_dev <<- (target_y_up-target_y_low)/2
    target_x_mid <<- target_x_up - target_x_dev
    target_y_mid <<- target_y_up - target_y_dev
}

##############################

F1Score = function(fact, pred){
    # Calculate F1 Score, True positive rate, Precision, and Recall
    
    TP = sum(fact==1 & pred==1)
    
    precision = TP/sum(pred)
    recall = TP/sum(fact)
    F1score = 2 * precision * recall / ( precision + recall)
    
    return(list(F1score = F1score, TP = TP, precision = precision, recall = recall))
    
}

error_rate = function(fact, pred){
    # Calculate classification error rate
    
    class_error = sum(fact != pred)
    
    class_error_rate = class_error / length(fact)
    
    return(class_error_rate)
}


##############################
#How to use

initiate = function(){
    load_data()
    set_target_var()
}


##############################






