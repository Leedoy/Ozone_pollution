library(dplyr)
library(plyr)
library(lattice)
library(ggplot2)
library(sp)
library(gstat)
library(maptools)
library(automap)
library(spdep)

####################delete_double1441A###############
ozone_root_d <- "E:\\leedoy\\Ozone_pollution\\Ozone_data\\Ozone_data_raw\\site_20140513_20141231_double1441A\\"
ozone_root <- "E:\\leedoy\\Ozone_pollution\\Ozone_data\\Ozone_data_raw\\site_20140513_20141231\\"
ozone_listfiles <- list.files(ozone_root_d)
for(i in 1:length(ozone_listfiles)){
  samples <- read.csv(paste(ozone_root_d,ozone_listfiles[i],sep=''),stringsAsFactors = F,header = T)
  samples <- samples[,-440]
  write.csv(samples,paste(ozone_root,ozone_listfiles[i],sep=''),row.names = F)
}

####################data2014_2_2015###############
ozone_root <- "E:\\leedoy\\Ozone_pollution\\Ozone_data\\Ozone_data_raw\\site_20140513_20141231\\"
ozone_listfiles <- list.files(ozone_root)

xy <- read.csv("E:\\leedoy\\Ozone_pollution\\Ozone_data\\Ozone_data_pre\\xy_2015_1482.csv")
xy$code <- substr(xy$code,1,4)

ozone_root_pre <- "E:\\leedoy\\Ozone_pollution\\Ozone_data\\Ozone_data_pre\\site_20140513_20141231\\"
#grid
x <- seq(73, 136, 0.1)
y <- seq(18, 54, 0.1)
x_len <- length(x)
x <- rep(x, each=length(y))
y <- rep(y, x_len)
x <- rep(x, 1)
y <- rep(y, 1)
grid <- data.frame(x=x, y=y)

for(i in 1:length(ozone_listfiles)){
  samples <- read.csv(paste(ozone_root,ozone_listfiles[i],sep=''),stringsAsFactors = F,header = T)
  name <- samples$date[1]
  
  samples_o3 <- samples[which((samples$hour=="23")&(samples$type=="O3_24h")),]
  samples_o3 <- rbind(samples_o3,code=colnames(samples))
  samples_o3 <- samples_o3[,-c(1:3)]

  samples_o3 <- as.data.frame(t(samples_o3),stringsAsFactors = F)
  colnames(samples_o3) <- c("value","code")
  samples_o3$value <- as.numeric(samples_o3$value)
  samples_o3$code <- substr(samples_o3$code,2,5)
  samples_o3 <- left_join(xy,samples_o3,by="code")
  #
  grid_temp = grid
  coordinates(grid_temp) <- ~x+y
  na_id <- which(is.na(samples_o3$value))
  samples_o3_temp <- samples_o3[-na_id,]
  coordinates(samples_o3_temp) <- ~x+y
  # model = autofitVariogram(log(value)~1,samples_o3_temp,model = c("Sph", "Exp", "Gau"))
  # result = krige(log(value)~1, samples_o3_temp, grid_temp, model = model$var_model)
  result <- idw(value~1, samples_o3_temp, grid_temp, idp=2)
  names(result@data) <- c("pred","var")
  result@data <- cbind.data.frame(result@data,grid_temp)
  
  for(j in na_id){
    selected_id <- which((abs(samples_o3$x[j]-result@data$x)<0.05000001)&(abs(samples_o3$y[j]-result@data$y)<0.05000001))
    if(length(selected_id)==1){
      samples_o3$value[j] <- result@data$pred[selected_id]
    }else{
      samples_o3$value[j] <- result@data$pred[selected_id[1]]
    }
    
  }
  
  write.csv(samples_o3,paste(ozone_root_pre,name,".csv",sep=''),row.names = F)
}

###
ozone_listfiles <- list.files(ozone_root_pre)
samples <- read.csv(paste(ozone_root_pre,ozone_listfiles[1],sep=''),stringsAsFactors = F,header = T)
name <- substr(ozone_listfiles[1],1,8)
colnames(samples)[length(colnames(samples))] <- name
for(i in 2:length(ozone_listfiles)){
  sample <- read.csv(paste(ozone_root_pre,ozone_listfiles[i],sep=''),stringsAsFactors = F,header = T)
  samples <- cbind(samples,sample$value)
  name <- substr(ozone_listfiles[i],1,8)
  colnames(samples)[length(colnames(samples))] <- name
}
write.csv(samples,"E:\\leedoy\\Ozone_pollution\\Ozone_data\\Ozone_data_pre\\sites_20140513_20141231.csv",row.names = F)

####################data2015&2016###############
ozone_root <- "E:\\leedoy\\Ozone_pollution\\Ozone_data\\Ozone_data_raw\\site_20170101_20170715\\"
ozone_listfiles <- list.files(ozone_root)

xy <- read.csv("E:\\leedoy\\Ozone_pollution\\Ozone_data\\Ozone_data_pre\\xy_2015_1482.csv")
xy$code <- substr(xy$code,1,4)

ozone_root_pre <- "E:\\leedoy\\Ozone_pollution\\Ozone_data\\Ozone_data_pre\\site_20170101_20170715\\"


for(i in 1:length(ozone_listfiles)){
  samples <- read.csv(paste(ozone_root,ozone_listfiles[i],sep=''),stringsAsFactors = F,header = T)
  name <- samples$date[1]
  
  samples_o3 <- samples[which((samples$hour=="23")&(samples$type=="O3_24h")),]
  if(length(samples_o3$date)==0){
    samples_o3 <- samples[which((samples$hour=="22")&(samples$type=="O3_24h")),]
  }
  if(length(samples_o3$date)==0){
    samples_o3 <- samples[which((samples$hour=="21")&(samples$type=="O3_24h")),]
  }
  if(length(samples_o3$date)==0){
    samples_o3 <- samples[which((samples$hour=="20")&(samples$type=="O3_24h")),]
  }
  if(length(samples_o3$date)==0){
    samples_o3 <- samples[which((samples$hour=="19")&(samples$type=="O3_24h")),]
  }
  if(length(samples_o3$date)==0){
    samples_o3 <- samples[which((samples$hour=="18")&(samples$type=="O3_24h")),]
  }
  if(length(samples_o3$date)==0){
    samples_o3 <- samples[which((samples$hour=="17")&(samples$type=="O3_24h")),]
  }
  if(length(samples_o3$date)==0){
    samples_o3 <- samples[which((samples$hour=="16")&(samples$type=="O3_24h")),]
  }
  samples_o3 <- rbind(samples_o3,code=colnames(samples))
  samples_o3 <- samples_o3[,-c(1:3)]
  
  samples_o3 <- as.data.frame(t(samples_o3),stringsAsFactors = F)
  colnames(samples_o3) <- c("value","code")
  samples_o3$value <- as.numeric(samples_o3$value)
  samples_o3$code <- substr(samples_o3$code,2,5)
  samples_o3 <- left_join(xy,samples_o3,by="code")
  #
  grid_temp = grid
  coordinates(grid_temp) <- ~x+y
  na_id <- which(is.na(samples_o3$value))
  samples_o3_temp <- samples_o3[-na_id,]
  coordinates(samples_o3_temp) <- ~x+y
  # model = autofitVariogram(log(value)~1,samples_o3_temp,model = c("Sph", "Exp", "Gau"))
  # result = krige(log(value)~1, samples_o3_temp, grid_temp, model = model$var_model)
  result <- idw(value~1, samples_o3_temp, grid_temp, idp=2)
  names(result@data) <- c("pred","var")
  result@data <- cbind.data.frame(result@data,grid_temp)
  
  for(j in na_id){
    selected_id <- which((abs(samples_o3$x[j]-result@data$x)<0.05000001)&(abs(samples_o3$y[j]-result@data$y)<0.05000001))
    if(length(selected_id)==1){
      samples_o3$value[j] <- result@data$pred[selected_id]
    }else{
      samples_o3$value[j] <- result@data$pred[selected_id[1]]
    }
    
  }
  
  write.csv(samples_o3,paste(ozone_root_pre,name,".csv",sep=''),row.names = F)
}

###
ozone_listfiles <- list.files(ozone_root_pre)
samples <- read.csv(paste(ozone_root_pre,ozone_listfiles[1],sep=''),stringsAsFactors = F,header = T)
name <- substr(ozone_listfiles[1],1,8)
colnames(samples)[length(colnames(samples))] <- name
for(i in 2:length(ozone_listfiles)){
  sample <- read.csv(paste(ozone_root_pre,ozone_listfiles[i],sep=''),stringsAsFactors = F,header = T)
  samples <- cbind(samples,sample$value)
  name <- substr(ozone_listfiles[i],1,8)
  colnames(samples)[length(colnames(samples))] <- name
}
write.csv(samples,"E:\\leedoy\\Ozone_pollution\\Ozone_data\\Ozone_data_pre\\sites_20170101_20170715.csv",row.names = F)

####################evident2017_0520###############
ozone_root <- "E:\\leedoy\\Ozone_pollution\\Ozone_data\\Ozone_data_raw\\site_20170101_20170715\\"

xy <- read.csv("E:\\leedoy\\Ozone_pollution\\Ozone_data\\Ozone_data_pre\\xy_201702_1547.csv")
xy$code <- substr(xy$code,1,4)

ozone_root_pre <- "E:\\leedoy\\Ozone_pollution\\Ozone_data\\Ozone_data_pre\\incident_2017_0515_0523\\sample_hour_8mean\\"
#grid
x <- seq(73, 136, 0.1)
y <- seq(18, 54, 0.1)
x_len <- length(x)
x <- rep(x, each=length(y))
y <- rep(y, x_len)
x <- rep(x, 1)
y <- rep(y, 1)
grid <- data.frame(x=x, y=y)

date <- c(20170522:20170524)
hour <- c(0:23)

for(i in 1:length(date)){
  samples <- read.csv(paste(ozone_root,"china_sites_",date[i],".csv",sep=''),stringsAsFactors = F,header = T)
  samples_o3_day <- xy
  for(j in 1:length(hour)){
    
    samples_o3 <- samples[which((samples$hour==hour[j])&(samples$type=="O3_8h")),] #O3
    samples_o3 <- rbind(samples_o3,code=colnames(samples))
    samples_o3 <- samples_o3[,-c(1:3)]
    samples_o3 <- as.data.frame(t(samples_o3),stringsAsFactors = F)
    colnames(samples_o3) <- c("value","code")
    samples_o3$value <- as.numeric(samples_o3$value)
    samples_o3$code <- substr(samples_o3$code,2,5)
    samples_o3 <- left_join(xy,samples_o3,by="code")
    #
    grid_temp = grid
    coordinates(grid_temp) <- ~x+y
    na_id <- which(is.na(samples_o3$value))
    na_id <- c(na_id,which(samples_o3$value==0))
    samples_o3_temp <- samples_o3[-na_id,]
    coordinates(samples_o3_temp) <- ~x+y
    # model = autofitVariogram(log(value)~1,samples_o3_temp,model = c("Sph", "Exp", "Gau"))
    # result = krige(log(value)~1, samples_o3_temp, grid_temp, model = model$var_model)
    result <- idw(value~1, samples_o3_temp, grid_temp, idp=2)
    names(result@data) <- c("pred","var")
    result@data <- cbind.data.frame(result@data,grid_temp)
    
    for(k in na_id){
      selected_id <- which((abs(samples_o3$x[k]-result@data$x)<0.05000001)&(abs(samples_o3$y[k]-result@data$y)<0.05000001))
      if(length(selected_id)==1){
        samples_o3$value[k] <- result@data$pred[selected_id]
      }else{
        samples_o3$value[k] <- result@data$pred[selected_id[1]]
      }
    }
    
    samples_o3_day <- cbind(samples_o3_day,value=samples_o3$value)
    names(samples_o3_day)[length(samples_o3_day)] <- paste(date[i],"_",hour[j],sep='')
    
  }
  
  write.csv(samples_o3_day,paste(ozone_root_pre,date[i],".csv",sep=''),row.names = F)
}


####################evident2017_0520_day###############
for(i in 1:length(ozone_listfiles)){
  samples <- read.csv(paste(ozone_root,ozone_listfiles[i],sep=''),stringsAsFactors = F,header = T)
  name <- samples$date[1]
  # samples <- samples[-which(substr(samples$type,1,2)=="PM"),]
  # samples <- samples[-which(substr(samples$type,1,2)=="SO"),]
  #day
  samples <- samples[which(samples$hour=='0'),]
  samples <- samples[-which(substr(samples$type,1,2)=="PM"),]
  samples <- samples[-which(substr(samples$type,1,2)=="SO"),]
  samples <- samples[-which(samples$type=="AQI"),]
  samples <- samples[-which(samples$type=="NO2"),]
  samples <- samples[-which(samples$type=="O3"),]
  samples <- samples[-which(samples$type=="O3_8h"),]
  samples <- samples[-which(samples$type=="CO"),]
  
  samples <- t(samples) #transposition
  #colnames(samples) <- paste(samples[1,],samples[2,],samples[3,],sep = '_') #hour
  colnames(samples) <- paste(samples[1,],samples[3,],sep = '_') #day
  samples <- samples[-c(1:3),]
  samples <- cbind(code=substr(row.names(samples),2,5),samples)
  samples <- as.data.frame(samples,stringsAsFactors=F)
  if(i!=1){
    samples1 <- left_join(samples1,samples,by=c("code"="code"))
  }else if(i==1){
    samples1 <- left_join(xy,samples,by=c("code"="code"))
  }
}

#delets which is all NA
id_na <- c()
for(j in 1:length(samples1$code)){
  #if(NA %in% (samples1[j,c(6:length(samples1))])){
  if(all(is.na(samples1[j,c(6:length(samples1))]))){
    id_na <- c(id_na,j)
  }
}
samples2 <- samples1[-id_na,]

inter=4
#NA
id=0
for(i in 1:length(samples2$code)){
  for(j in 10:length(samples2)){
    if(is.na(samples2[i,j])){#???1+???1
      if(!is.null(samples2[i,j-inter])&!is.null(samples2[i,j+inter])){
        if(!is.na(samples2[i,j-inter])&!is.na(samples2[i,j+inter])){
          samples2[i,j] <- (as.numeric(samples2[i,j-inter]) + as.numeric(samples2[i,j+inter]))/2
          id=id+1
        }
      }
    }
  }
}
#
id=0
for(i in 1:length(samples2$code)){
  for(j in 14:length(samples2)){
    if(is.na(samples2[i,j])){#???1+???2
      if(!is.null(samples2[i,j-inter])&!is.null(samples2[i,j+inter*2])){
        if(!is.na(samples2[i,j-inter])&!is.na(samples2[i,j+inter*2])){
          samples2[i,j] <- (as.numeric(samples2[i,j-inter]) + as.numeric(samples2[i,j+inter*2]))/2
          id=id+1
        }
      }
    }
    if(is.na(samples2[i,j])){#???2+???1
      if(!is.null(samples2[i,j-inter*2])&!is.null(samples2[i,j+inter])){
        if(!is.na(samples2[i,j-inter*2])&!is.na(samples2[i,j+inter])){
          samples2[i,j] <- (as.numeric(samples2[i,j-inter*2]) + as.numeric(samples2[i,j+inter]))/2
          id=id+1
        }
      }
    }
    if(is.na(samples2[i,j])){#???2+???2
      if(!is.null(samples2[i,j-inter*2])&!is.null(samples2[i,j+inter*2])){
        if(!is.na(samples2[i,j-inter*2])&!is.na(samples2[i,j+inter*2])){
          samples2[i,j] <- (as.numeric(samples2[i,j-inter*2]) + as.numeric(samples2[i,j+inter*2]))/2
          id=id+1
        }
      }
    }
  }
}
#
while(id!=0){
  id=0
  for(i in 1:length(samples2$code)){
    for(j in 10:length(samples2)){
      if(is.na(samples2[i,j])){#???1
        if(!is.null(samples2[i,j-inter])){
          if(!is.na(samples2[i,j-inter])){
            samples2[i,j] <- as.numeric(samples2[i,j-inter])
            id=id+1
          }
        }
      }
      if(is.na(samples2[i,j])){#???1
        if(!is.null(samples2[i,j+inter])){
          if(!is.na(samples2[i,j+inter])){
            samples2[i,j] <- as.numeric(samples2[i,j+inter])
            id=id+1
          }
        }
      }
    }
  }
}
#
while(id!=0){
  id=0
  for(i in 1:length(samples2$code)){
    for(j in 6:length(samples2)){
      if(is.na(samples2[i,j])){#???1
        if(!is.null(samples2[i,j+inter])){
          if(!is.na(samples2[i,j+inter])){
            samples2[i,j] <- as.numeric(samples2[i,j+inter])
            id=id+1
          }
        }
      }
    }
  }
}

samples3 <- samples2
write.csv(samples3,"E:\\leedoy\\Ozone_pollution\\Ozone_data\\2017_0515_0523\\all_day.csv",row.names = F)

####################evident2017_0520_moran'sI###############
ozone_root <- "E:\\leedoy\\Ozone_pollution\\Ozone_data\\Ozone_data_pre\\incident_2017_0515_0523\\sample_hour_8mean24\\"
ozone_listfiles <- list.files(ozone_root)

result <- data.frame(date=0,moransi=0,p=0)
flag = 1
for(i in 1:length(ozone_listfiles)){
  samples <- read.csv(paste(ozone_root,ozone_listfiles[i],sep=''),stringsAsFactors = F,header = T)
  xy <- samples[,c(4,5)]
  
  for(j in 1:(length(samples)-5)){
    result$date[flag] <- names(samples)[5+j]
    xy_1 <- cbind(xy,samples[,5+j])
    names(xy_1) <- c("x","y","value")
    #moran's i
    coordinates(xy_1) <- ~x+y
    nbk1 <- knn2nb(knearneigh(xy_1, k = 4, longlat = TRUE))
    snbk1 <- make.sym.nb(nbk1)
    moransi <- moran.test(xy_1$value, nb2listw(snbk1))
    result$moransi[flag] <- moransi[[3]][[1]]
    result$p[flag] <- moransi[[2]]
    result <- rbind(result,0)
    flag = flag + 1
  }
}
write.csv(result,"E:\\leedoy\\Ozone_pollution\\Ozone_data\\Ozone_data_pre\\incident_2017_0515_0523\\moransI_hour_8mean24.csv",row.names = F)

####################evident2017_0520_ok###############
ozone_root <- "E:\\leedoy\\Ozone_pollution\\Ozone_data\\Ozone_data_pre\\incident_2017_0515_0523\\sample_hour_8mean\\"
ozone_listfiles <- list.files(ozone_root)

#grid
grid <- read.csv("E:\\leedoy\\Ozone_pollution\\Ozone_data\\grid_grid_pollutedarea3.csv",stringsAsFactors = F,header = T)

for(i in 2:length(ozone_listfiles)){
  result1 <- grid
  samples <- read.csv(paste(ozone_root,ozone_listfiles[i],sep=''),stringsAsFactors = F,header = T)
  xy <- samples[,c(4,5)]
  
  for(j in 1:(length(samples)-5)){
    xy_1 <- cbind(xy,samples[,5+j])
    names(xy_1) <- c("x","y","value")
    #kriging
    grid_temp = grid
    coordinates(grid_temp) <- ~x_1+y_1
    coordinates(xy_1) <- ~x+y
    model = autofitVariogram(value~1,xy_1,model = c("Sph", "Exp", "Gau"))
    # plot(variogram(value ~ 1, data =xy_1))
    # plot(variogram(value ~ 1, data =xy_1),model$var_model)
    result = krige(value~1, xy_1, grid_temp, model = model$var_model)
    names(result@data) <- c("pred","var")
    
    result1 <- cbind(result1,value=result@data$pred)
    name <- names(samples)[5+j]
    names(result1)[length(result1)] <- substr(name,2,nchar(name))

  }
  write.csv(result1,paste("E:\\leedoy\\Ozone_pollution\\Ozone_data\\Ozone_data_pre\\incident_2017_0515_0523\\sample_hour_ok_8mean_pollutedarea3_envelope\\",substr(ozone_listfiles[i],1,8),".csv",sep=''),row.names = F)
  
}

ozone_root <- "E:\\leedoy\\Ozone_pollution\\Ozone_data\\Ozone_data_pre\\incident_2017_0515_0523\\sample_hour_ok_8mean\\"
ozone_listfiles <- list.files(ozone_root)
for(i in 2:length(ozone_listfiles)){
  Id <- grid
  samples <- read.csv(paste(ozone_root,ozone_listfiles[i],sep=''),stringsAsFactors = F,header = T)
  result1 <- left_join(Id,samples,by = c("Id" = "Id"))
  write.csv(result1,paste("E:\\leedoy\\Ozone_pollution\\Ozone_data\\Ozone_data_pre\\incident_2017_0515_0523\\sample_hour_ok_8mean_pollutedarea2\\",substr(ozone_listfiles[i],1,8),".csv",sep=''),row.names = F)
}


###
ozone_listfiles <- list.files(ozone_root_pre)
samples <- read.csv(paste(ozone_root_pre,ozone_listfiles[1],sep=''),stringsAsFactors = F,header = T)
name <- substr(ozone_listfiles[1],1,8)
colnames(samples)[length(colnames(samples))] <- name
for(i in 2:length(ozone_listfiles)){
  sample <- read.csv(paste(ozone_root_pre,ozone_listfiles[i],sep=''),stringsAsFactors = F,header = T)
  samples <- cbind(samples,sample$value)
  name <- substr(ozone_listfiles[i],1,8)
  colnames(samples)[length(colnames(samples))] <- name
}
write.csv(samples,"E:\\leedoy\\Ozone_pollution\\Ozone_data\\Ozone_data_pre\\sites_20170101_20170715.csv",row.names = F)


#grid
grid1 <- read.csv("E:\\leedoy\\Ozone_pollution\\Ozone_data\\grid_grid.csv",stringsAsFactors = F,header = T)
#grid
grid <- read.csv("E:\\leedoy\\Ozone_pollution\\Ozone_data\\grid_point.csv",stringsAsFactors = F,header = T)

result=left_join(grid1,grid,by="Id")

write.csv(result,"E:\\leedoy\\Ozone_pollution\\Ozone_data\\33333333333.csv",row.names = F)


##############################################################################

###################polluted_area############
data <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_data\\Ozone_data_pre\\incident_2017_0515_0523\\sample_hour_ok_8mean_pollutedarea3_envelope\\201705_8mean.csv",stringsAsFactors = F,header = T)
xy <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_data\\grid_point_pollutedarea3.csv",stringsAsFactors = F,header = T)
result <- left_join(xy,data,by = c("Id" = "Id"))
write.csv(result,"E:\\leedoy\\ozone_pollution\\Ozone_data\\201705_polutedarea3_8mean.csv",row.names = F)

quxian_shp <- readShapePoly("E:\\leedoy\\ozone_pollution\\Ozone_data\\shp\\grid_grid_pollutedarea3.shp",proj4string = CRS("+proj=longlat +ellps=WGS84"))
data_result <- read.csv("E:\\leedoy\\Ozone_pollution\\Ozone_pullution_zone\\image_fusion\\1test\\XFUS_T_polutedarea3_8mean1.csv",stringsAsFactors = F,header = T)
quxian_shp_temp <- quxian_shp
quxian_shp_temp@data <- left_join(quxian_shp_temp@data,data_result,by = c("Id" = "Id"))
writePolyShape(quxian_shp_temp,"E:\\leedoy\\ozone_pollution\\Ozone_data\\shp\\grid_grid_image_pollutedarea3.shp")

###################polluted_point############
data_sample <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_pullution_zone\\precision_validation\\grid_grid_pol_join_xy_samples3.csv",stringsAsFactors = F,header = T)
data_sample <- data.frame(Id=data_sample[,1])
data_sample_temp <- left_join(data_sample,data,by = c("Id" = "Id"))
data_sample_temp_M <-  as.matrix(data_sample_temp[,-c(1:5)])
#
data_sample_temp_M[data_sample_temp_M <= 160 ] <- 0
data_sample_temp_M[data_sample_temp_M > 160 ] <- 1
data_sample_temp_T <- as.data.frame(data_sample_temp_M)
data_sample_temp_T <- cbind(Id=data_sample_temp$Id,data_sample_temp_T,sta=0)
for(i in 1:length(data_sample_temp_T$Id)){
  data_sample_temp_T$sta[i] <- sum(data_sample_temp_T[i,c(2:202)])
}

write.csv(data_sample_temp_T[,c(1,203)],"E:\\leedoy\\ozone_pollution\\Ozone_pullution_zone\\precision_validation\\sta_pollutedarea3_160.csv")
#delete
# pollution_point <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_pullution_zone\\precision_validation\\sample_sta.csv",stringsAsFactors = F,header = T)
# id = which(!duplicated(pollution_point$Id_grid))
# pollution_point <- pollution_point[id,]
# write.csv(pollution_point,"E:\\leedoy\\ozone_pollution\\Ozone_pullution_zone\\precision_validation\\sample_sta1.csv")
####validation####
pollution_point <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_pullution_zone\\precision_validation\\sta_pollutedarea3_160.csv",stringsAsFactors = F,header = T)

validated_point <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_pullution_zone\\spatio_temporal_clustering\\1test\\som_map.unit.classif.72.pollutedarea3_1.csv",stringsAsFactors = F,header = T)
validated_point <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_pullution_zone\\image_fusion\\1test\\XFUS_T_polutedarea3_8mean1.csv",stringsAsFactors = F,header = T)
validated_point <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_pullution_zone\\frequent_pattern\\1test\\160_20190103\\rules.summary_polutedarea3_8mean_0.08_0.7.csv",stringsAsFactors = F,header = T)

validated_point_j <- left_join(pollution_point,validated_point,by = c("Id" = "Id"))

tp <- length(which(validated_point_j$sta24==1&validated_point_j$class==1))
validated_point_j$class[is.na(validated_point_j$class)] <- 0
tn <- length(which(validated_point_j$sta24==0&validated_point_j$class==0))
p <- tp/(tp+270-tn)
r <- tp/316
f <- 2*p*r/(p+r)

tp <- length(which(validated_point_j$sta48==1&validated_point_j$class==1))
validated_point_j$class[is.na(validated_point_j$class)] <- 0
tn <- length(which(validated_point_j$sta48==0&validated_point_j$class==0))
p <- tp/(tp+468-tn)
r <- tp/118
f <- 2*p*r/(p+r)

tp <- length(which(validated_point_j$sta72==1&validated_point_j$class==1))
validated_point_j$class[is.na(validated_point_j$class)] <- 0
tn <- length(which(validated_point_j$sta72==0&validated_point_j$class==0))
p <- tp/(tp+558-tn)
r <- tp/28
f <- 2*p*r/(p+r)

####LLR####
data <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_data\\201705_polutedarea3_8mean.csv",stringsAsFactors = F,header = T)
data_T <- data[,-c(1:5)]
data_T_M <- as.matrix(data_T)
data_T_M[data_T_M < 160] <- 0 #daily max 8-hour mean
data_T_M[data_T_M >= 160] <- 1
np <- sum(data_T_M)
Np <- 216*55644

Nz <- 216*19790 #SOM
Nz <- 216*13155 #IFbWT
Nz <- 216*24278 #Apriori

validated_point <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_pullution_zone\\spatio_temporal_clustering\\1test\\som_map.unit.classif.72.pollutedarea3.csv",stringsAsFactors = F,header = T)
validated_point <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_pullution_zone\\image_fusion\\1test\\XFUS_T_polutedarea3_8mean.csv",stringsAsFactors = F,header = T)
validated_point <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_pullution_zone\\frequent_pattern\\1test\\160_20190103\\rules.summary_polutedarea3_8mean_0.38_0.7.csv",stringsAsFactors = F,header = T)

validated_point_j <- left_join(validated_point,data,by = c("Id" = "Id"))
validated_point_j <- validated_point_j[which(validated_point_j$class==1),]
validated_point_j_T <- validated_point_j[,-c(1:7)]
validated_point_j_T_M <- as.matrix(validated_point_j_T)
validated_point_j_T_M[validated_point_j_T_M < 160] <- 0 #daily max 8-hour mean
validated_point_j_T_M[is.na(validated_point_j_T_M)] <- 0
validated_point_j_T_M[validated_point_j_T_M >= 160] <- 1
nz <- sum(validated_point_j_T_M)

LLR = nz*log(nz/Nz) + (np-nz)*log((np-nz)/(Np-Nz))
#LLR = nz*log(nz/Nz) + (np-nz)*log((np-nz)/(Np-Nz)) - np*log(np/Np)
#LLR = nz*log(nz/Nz) + (Nz-nz)*log((Nz-nz)/Nz) + (np-nz)*log((np-nz)/(Np-Nz)) + (Np-Nz-np+nz)*log((Np-Nz-np+nz)/(Np-Nz))

#Monte Carlo test
#possion distribution simulation
validated_point <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_pullution_zone\\spatio_temporal_clustering\\1test\\som_map.unit.classif.72.pollutedarea3.csv",stringsAsFactors = F,header = T)
validated_point <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_pullution_zone\\image_fusion\\1test\\XFUS_T_polutedarea3_8mean.csv",stringsAsFactors = F,header = T)
validated_point <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_pullution_zone\\frequent_pattern\\1test\\160_20190103\\rules.summary_polutedarea3_8mean_0.38_0.7.csv",stringsAsFactors = F,header = T)
Np=12019104
np=876186
num=55644
Nz <- 216*13155 #216*19790 SOM 216*13155 #IFbWT 216*24278 #Apriori

set.seed(1234)

# num=100
# beta0=1
# beta1=0.2
# x=beta0 + beta1*runif(n=num, min=0, max=5)
# lambda=exp(x)
# y=rpois(n=num, lambda=lambda)
# model = glm(y~x, family=poisson(link = log))
LLR <- c()
for(i in 1:999){
  y=rpois(n=num, lambda=Nz/num)
  #hyper(m,n,k)
  data_new <- data.frame(Id=data$Id, value=y)
  
  validated_point_j <- left_join(validated_point,data_new,by = c("Id" = "Id"))
  validated_point_j$class[is.na(validated_point_j[,length(validated_point_j)])] <- 0
  validated_point_j <- validated_point_j[which(validated_point_j$class==1),]
  nz <- sum(validated_point_j$value.y)
  LLR = c(LLR,nz*log(nz/Nz) + (np-nz)*log((np-nz)/(Np-Nz)))
}
#SOM
LLR <- c(LLR,-1809100)
LLR <- sort(LLR,decreasing = T)
which(LLR==-1809100)
#IFbWT
LLR <- c(LLR,-2072200)
LLR <- sort(LLR,decreasing = T)
which(LLR==-2072200)
#Apriori
LLR <- c(LLR,-1769200)
LLR <- sort(LLR,decreasing = T)
which(LLR==-1769200)






