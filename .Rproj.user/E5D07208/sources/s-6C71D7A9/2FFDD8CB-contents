#EY Data Science Competition
#Graph List

##############################
#library



##############################
#Global Variables


##############################
#User Defined Functions

plot_last_xy = function(i){
    
    if(i==0){
        data_i = train_stationary_attri
        title = "train_stationary_attri_last_xy"
    }
    else if(i==1){
        data_i = train_single_attri
        title = "train_single_attri_last_xy"
    }
    else{
        data_i = train_attri
        title = "train_attri_last_xy"
    }
    
    a = data_i[["result_class"]] == 1

    plot(data_i[!a,c("last_x","last_y")], pch=".", main = title)
    lines(data_i[a,c("last_x","last_y")], pch=".", type="p", col="red")
    abline(h=c(-target_y_dev,target_y_dev), v=c(-target_x_dev,target_x_dev), col="blue")
}


plot_home_result = function(i){
    
    if(i==0){
        data_i = train_stationary_attri
        title = "train_stationary_attri"
    }
    else if(i==1){
        data_i = train_single_attri
        title = "train_single_attri"
    }
    else{
        data_i = train_attri
        title = "train_attri"
    }
    
    a = data_i[["result_class"]] == 1
    
    plot(data_i[!a,c("adj_home_x","result_x")], pch=".", main = paste(title,"home vs last x"))
    lines(data_i[a,c("adj_home_x","result_x")], pch=".", type="p", col="red")
    abline(h=c(-target_x_dev,target_x_dev), v=c(-target_x_dev,target_x_dev),col="blue")
    
    plot(data_i[!a,c("adj_home_y","result_y")], pch=".", main = paste(title,"home vs last y"))
    lines(data_i[a,c("adj_home_y","result_y")], pch=".", type="p", col="red")
    abline(h=c(-target_y_dev,target_y_dev), v=c(-target_y_dev,target_y_dev), col="blue")
}


plot_track = function(){
    plot(simple_train_track[[1]][,2:3], pch=20,type="n", xlim=c(-20000,20000), ylim=c(-150000,200000))
    
    for(i in 1:175){
        if(simple_train_attri[[i,"result_class"]] == TRUE){
            lines(simple_train_track[[i]][,2:3], pch=20,type="l", col="red")
        }
        else{
            lines(simple_train_track[[i]][,2:3], pch=20,type="l")
        }
    }
    
    abline(h=c(-target_y_dev,target_y_dev), v=c(-target_x_dev,target_x_dev), col="blue")
    
}

plot_attri_result = function(){
    par(mfrow=c(6,15), mar=c(4.5,4.1,3.5,1.5))

    #progress bar
    pb = startpb(0, 6*15)
    on.exit(closepb(pb))
    
    p = train_result$result_class == 1
    
    for(i in 1:6){
        for(j in 1:15){
            plot(train_attri[[j+1]][(!p)&good],train_result[[i]][(!p)&good],
                 # xlim=c( min(train_attri[[j+1]][(!p)&good]), max(train_attri[[j+1]][(!p)&good])),
                 # ylim=c( min(train_result[[i]][(!p)&good]), max(train_result[[i]][(!p)&good])),
                 pch=".",
                 xlab=names(train_attri[j+1]),
                 ylab=names(train_result[i])
                 )
            lines(train_attri[[j+1]][p&good],train_result[[i]][p&good], 
                  # xlim=c( min(train_attri[[j+1]][p&good]), max(train_attri[[j+1]][p&good])),
                  # ylim=c( min(train_result[[i]][p&good]), max(train_result[[i]][p&good])),
                  pch=".",
                  type="p", 
                  col="red"
                  )
            
            setpb(pb, (i-1)*15+j)
        }
    }
        
    par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1))
    print("Done")
}

plot_vel_result = function(){
    par(mfrow=c(6,16), mar=c(4.5,4.1,3.5,1.5))
    
    #progress bar
    pb = startpb(0, 6*16)
    on.exit(closepb(pb))
    
    p = train_result$result_class == 1
    
    for(i in 1:6){
        for(j in 23:30){
            plot(train_vel[[j]][(!p)&good],train_result[[i]][(!p)&good],
                 # xlim=c( min(train_attri[[j+1]][(!p)&good]), max(train_attri[[j+1]][(!p)&good])),
                 # ylim=c( min(train_result[[i]][(!p)&good]), max(train_result[[i]][(!p)&good])),
                 pch=".",
                 xlab=names(train_vel[j]),
                 ylab=names(train_result[i])
            )
            lines(train_vel[[j]][p&good],train_result[[i]][p&good], 
                  # xlim=c( min(train_attri[[j+1]][p&good]), max(train_attri[[j+1]][p&good])),
                  # ylim=c( min(train_result[[i]][p&good]), max(train_result[[i]][p&good])),
                  pch=".",
                  type="p", 
                  col="red"
            )
            
            setpb(pb, (i-1)*16+(j-22))
        }
        
        for(j in 53:60){
            plot(train_vel[[j]][(!p)&good],train_result[[i]][(!p)&good],
                 # xlim=c( min(train_attri[[j+1]][(!p)&good]), max(train_attri[[j+1]][(!p)&good])),
                 # ylim=c( min(train_result[[i]][(!p)&good]), max(train_result[[i]][(!p)&good])),
                 pch=".",
                 xlab=names(train_vel[j]),
                 ylab=names(train_result[i])
            )
            lines(train_vel[[j]][p&good],train_result[[i]][p&good], 
                  # xlim=c( min(train_attri[[j+1]][p&good]), max(train_attri[[j+1]][p&good])),
                  # ylim=c( min(train_result[[i]][p&good]), max(train_result[[i]][p&good])),
                  pch=".",
                  type="p", 
                  col="red"
            )
            
            setpb(pb, (i-1)*16+(j-53))
        }
    }
    
    par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1))
    print("Done")
}

plot_vel_2_result = function(){
    par(mfrow=c(6,16), mar=c(4.5,4.1,3.5,1.5))
    
    #progress bar
    pb = startpb(0, 6*16)
    on.exit(closepb(pb))
    
    p = train_result$result_class == 1
    
    for(i in 1:6){
        for(j in 15:30){
            plot(train_vel_2[[j]][(!p)&good],train_result[[i]][(!p)&good],
                 # xlim=c( min(train_attri[[j+1]][(!p)&good]), max(train_attri[[j+1]][(!p)&good])),
                 # ylim=c( min(train_result[[i]][(!p)&good]), max(train_result[[i]][(!p)&good])),
                 pch=".",
                 xlab=names(train_vel_2[j]),
                 ylab=names(train_result[i])
            )
            lines(train_vel_2[[j]][p&good],train_result[[i]][p&good], 
                  # xlim=c( min(train_attri[[j+1]][p&good]), max(train_attri[[j+1]][p&good])),
                  # ylim=c( min(train_result[[i]][p&good]), max(train_result[[i]][p&good])),
                  pch=".",
                  type="p", 
                  col="red"
            )
            
            setpb(pb, (i-1)*16+(j-14))
        }
    }
    
    pb = startpb(0, 6*16)
    for(i in 1:6){
        for(j in 45:60){
            plot(train_vel_2[[j]][(!p)&good],train_result[[i]][(!p)&good],
                 # xlim=c( min(train_attri[[j+1]][(!p)&good]), max(train_attri[[j+1]][(!p)&good])),
                 # ylim=c( min(train_result[[i]][(!p)&good]), max(train_result[[i]][(!p)&good])),
                 pch=".",
                 xlab=names(train_vel_2[j]),
                 ylab=names(train_result[i])
            )
            lines(train_vel_2[[j]][p&good],train_result[[i]][p&good], 
                  # xlim=c( min(train_attri[[j+1]][p&good]), max(train_attri[[j+1]][p&good])),
                  # ylim=c( min(train_result[[i]][p&good]), max(train_result[[i]][p&good])),
                  pch=".",
                  type="p", 
                  col="red"
            )
            
            setpb(pb, (i-1)*16+(j-46))
        }
    }
    
    par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1))
    print("Done")
}

plot_rel_1 = function(){
    par(mfrow=c(6,15), mar=c(4.5,4.1,3.5,1.5))
    
    #progress bar
    pb = startpb(0, 6*15)
    on.exit(closepb(pb))
    
    p = train_result$result_class == 1
    
    cat1 = good & (!x_stat) & (!y_stat)
    cat2 = good & x_stat & (!y_stat)
    cat3 = good & (!x_stat) & y_stat
    cat4 = good & x_stat & y_stat
    
    
    for(i in 1:6){
        for(j in 1:15){
            plot(train_attri[[j+1]][cat1],train_result[[i]][cat1],
                 pch=".",
                 xlab=names(train_attri[j+1]),
                 ylab=names(train_result[i])
            )
            lines(train_attri[[j+1]][cat2],train_result[[i]][cat2], 
                  pch=".",
                  type="p", 
                  col="red"
            )
            lines(train_attri[[j+1]][cat3],train_result[[i]][cat3], 
                  pch=".",
                  type="p", 
                  col="green"
            )
            lines(train_attri[[j+1]][cat4],train_result[[i]][cat4], 
                  pch=".",
                  type="p", 
                  col="blue"
            )
            
            setpb(pb, (i-1)*15+j)
        }
    }
    
    par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1))
    print("Done")
}

plot_rel_2 = function(){
    par(mfrow=c(15,15), mar=c(2,2,2,0.5))
    
    #progress bar
    pb = startpb(0, 15*15)
    on.exit(closepb(pb))
    
    p = train_result$result_class == 1
    
    cat1 = good & (!x_stat) & (!y_stat)
    cat2 = good & x_stat & (!y_stat)
    cat3 = good & (!x_stat) & y_stat
    cat4 = good & x_stat & y_stat
    
    
    for(i in 1:15){
        for(j in 1:15){
            plot(train_attri[[j+1]][cat1],train_attri[[i+1]][cat1],
                 pch=".",
                 xlab=names(train_attri[j+1]),
                 ylab=names(train_attri[i+1])
            )
            lines(train_attri[[j+1]][cat2],train_attri[[i+1]][cat2], 
                  pch=".",
                  type="p", 
                  col="red"
            )
            lines(train_attri[[j+1]][cat3],train_attri[[i+1]][cat3], 
                  pch=".",
                  type="p", 
                  col="green"
            )
            lines(train_attri[[j+1]][cat4],train_attri[[i+1]][cat4], 
                  pch=".",
                  type="p", 
                  col="blue"
            )
            
            setpb(pb, (i-1)*15+j)
        }
    }
    
    par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1))
    print("Done")
}

plot_attri = function(){
    par(mfrow=c(15,15), mar=c(2,2,2,0.5))
    
    #progress bar
    pb = startpb(0, 15*15)
    on.exit(closepb(pb))
    
    p = train_result$result_class == 1
    
    cat1 = good & !p
    cat2 = good & p
    
    for(i in 1:15){
        for(j in 1:15){
            plot(train_attri[[j+1]][cat1],train_attri[[i+1]][cat1],
                 pch=".",
                 xlab=names(train_attri[j+1]),
                 ylab=names(train_attri[i+1])
            )
            lines(train_attri[[j+1]][cat2],train_attri[[i+1]][cat2], 
                  pch=".",
                  type="p", 
                  col="red"
            )
            
            setpb(pb, (i-1)*15+j)
        }
    }
    
    par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1))
    print("Done")
}

plot_result = function(){
    par(mfrow=c(6,6), mar=c(4.5,4.1,3.5,1.5))
    
    #progress bar
    pb = startpb(0, 6*6)
    on.exit(closepb(pb))
    
    p = train_result$result_class == 1
    
    for(i in 1:6){
        for(j in 1:6){
            plot(train_result[[j]][(!p)&good],train_result[[i]][(!p)&good],
                 # xlim=c( min(train_attri[[j+1]][(!p)&good]), max(train_attri[[j+1]][(!p)&good])),
                 # ylim=c( min(train_result[[i]][(!p)&good]), max(train_result[[i]][(!p)&good])),
                 pch=".",
                 xlab=names(train_result[j]),
                 ylab=names(train_result[i])
            )
            lines(train_result[[j]][p&good],train_result[[i]][p&good], 
                  # xlim=c( min(train_attri[[j+1]][p&good]), max(train_attri[[j+1]][p&good])),
                  # ylim=c( min(train_result[[i]][p&good]), max(train_result[[i]][p&good])),
                  pch=".",
                  type="p", 
                  col="red"
            )
            
            setpb(pb, (i-1)*6+j)
        }
    }
    
    par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1))
    print("Done")
}

plot_track = function(){
    par(mfrow=c(3,11), mar=c(4.5,4.1,3.5,1.5))
    
    #progress bar
    pb = startpb(0, 31)
    on.exit(closepb(pb))
    
    p = train_result$result_class == 1
    
    for(i in 1:31){
        plot(train_pt[[i]][(!p)&good], train_pt[[i+31]][(!p)&good],
            xlim = c(-20000,20000),
            ylim = c(-150000,200000),
            #pch=".",
            xlab=names(train_pt[i]),
            ylab=names(train_pt[i+31])
        )
        lines(train_pt[[i]][p&good], train_pt[[i+31]][p&good], 
              #pch=".",
              type="p", 
              col="red"
        )
        
        setpb(pb, i)
    }
    
    par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1))
    print("Done")
}

plot_1 = function(x,y){
    p = train_result$result_class == 1
    
    plot(x[!p&good],y[!p&good], pch=".")
    lines(x[p&good],y[p&good], pch=".",type="p", col="red")
}





































