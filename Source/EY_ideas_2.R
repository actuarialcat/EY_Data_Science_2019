#EY Data Science Competition
#Ideas List

##############################
#library


##############################
#Global Variables


##############################
#User Defined Functions




##############################






t1 = abs(train_attri$x_displacement)>300
cla_t1 = good&t1&abs(train_result[[1]])>300
# plot(train_attri$x_displacement[cla_t1], train_result$last_delta_x[cla_t1], pch=".")
# fm1 = lm(train_result$last_delta_x[cla_t1] ~ train_attri$x_displacement[cla_t1])


t2 = abs(train_attri$y_displacement)>1000
cla_t2 = good&t2&abs(train_result[[2]])>1000
# plot(train_attri$y_displacement[cla_t2], train_result$last_delta_y[cla_t2], pch=".")
# fm2 = lm(train_result$last_delta_y[cla_t2] ~ train_attri$y_displacement[cla_t2])



res_1 = simple_lag_1_classifier(train_attri[good&t1&t2,])

res_2 = simple_lag_1_classifier_v2(train_attri[good&t1&t2,])



t_fact = c(train_result$result_class[good&(t1&t2)], train_result$result_class[good&!(t1&t2)])
t_res = c(simple_lag_1_classifier_v2(train_attri[good&t1&t2,]), simple_lag_1_classifier(train_attri[good&!(t1&t2),]))

print(F1Score(t_fact,t_res))

