
library(kohonen)
library(sp)
library(maptools)
library(dplyr)

data <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_data\\201705_polutedarea3_8mean.csv",stringsAsFactors = F,header = T)
data_T <- data[,-c(1:5)]#information only saved
# Change the data frame with training data to a matrix, center and scale all variables to give them equal importance during the SOM training process. 
data_T_M <- as.matrix(data_T)#scale(data_T)
## set dim names
#dimnames(data_T_M) <- list(paste("a",data$Id,sep = ""), paste("b",c(1:168),sep = ""))
#sqrt(N)*5=75=9*8 classes//Hexagonal or Circular topologies are possible
som_grid <- somgrid(xdim = 8, ydim = 9, topo="hexagonal", neighbourhood.fct = "gaussian") 
# train the SOM, set iterations/learning rates/neighbourhood
som_map <- som(data_T_M, grid=som_grid, rlen=1000, alpha=c(0.05,0.01), keep.data = TRUE)

plot(som_map, type="changes")
# jpeg(filename = "E:\\leedoy\\ozone_pollution\\Ozone_pullution_zone\\spatio_temporal_clustering\\1test\\training1.jpg",
#      width = 800, height = 600, units = "px", pointsize = 12, quality = 300)
# dev.off()
# plot(som_map, type="codes")
# plot(som_map, type="dist.neighbours")
# mean(som_map$distances)
#write.csv(som_map$unit.classif,"E:\\leedoy\\ozone_pollution\\Ozone_pullution_zone\\spatio_temporal_clustering\\1test\\som_map.unit.classif.72.pollutedarea2.csv")
#write.csv(cbind(som_id=som_map$unit.classif,data[,1]),"E:\\leedoy\\ozone_pollution\\Ozone_pullution_zone\\spatio_temporal_clustering\\1test\\som_map.unit.classif.72.pollutedarea2.csv",row.names = F)

##############according kmeans##############
som_map_codes = som_map$codes
som_map_codes = matrix(unlist(som_map_codes),nrow=72)
# wss <- c()
# for (i in 1:50) wss[i] <- sum(kmeans(som_map_codes, centers=i)$withinss)
# plot(10:50, wss[10:50], type="b", xlab="Number of Clusters",
#     ylab="Within groups sum of squares", main="Within cluster sum of squares")

# ## use hierarchical clustering to cluster the codebook vectors
# som_map.hc <- cutree(hclust(dist(som_map_codes)), 2)
# # plot these results
# pretty_palette <- c("#FF69B4", "#DB7093", "#9932CC", "#7B68EE","#4169E1", "#00BFFF",
#                     "#00FFFF", "#90EE90",'#ADFF2F','#BDB76B','#F4A460','#808080')
# plot(som_map, type="codes", bgcol =pretty_palette[som_map.hc] , main = "Clusters")
# add.cluster.boundaries(som_map, som_map.hc)
# result1 <- som_map.hc[1:72]
# result1[which(result1==1)] <- 0
# result1[which(is.na(result1))] <- 0
# result1[which(result1==2)] <- 1
# result2 <- data.frame(som_id=c(1:72),class=result1)
# som_result <- data.frame(som_id=som_map$unit.classif,Id=data[,1])
# som_map.unit.classif.72.pollutedarea3 <- left_join(som_result,result2,by = c("som_id" = "som_id"))
# write.csv(som_map.unit.classif.72.pollutedarea3,"E:\\leedoy\\ozone_pollution\\Ozone_pullution_zone\\spatio_temporal_clustering\\1test\\som_map.unit.classif.72.pollutedarea3.csv",row.names = F)

##############according mean##############
#som_map.unit.class.201705 <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_pullution_zone\\spatio_temporal_clustering\\1test\\som_map.unit.class.201705.p.csv",stringsAsFactors = F,header = T)
som_map.unit.class.201705 <- cbind(som_id=som_map$unit.classif,data)
result <- c()
for(i in 1:72){
  a = som_map.unit.class.201705[which(som_map.unit.class.201705$som_id==i),c(7:207)]
  a = as.matrix(a)
  a[which(is.na(a))] <- 0
  a[which(a < 160)] <- 0
  a[which(a >= 160)] <- 1
  result <- c(result,sum(a)/length(a))
}
sort(result)
result[which(is.na(result))] <- 0
# max(result)
# sort(result)
threshold <- c(1:27)/100
pollution_point <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_pullution_zone\\precision_validation\\sta_pollutedarea3_160.csv",stringsAsFactors = F,header = T)
f_all <- c()
for(i in threshold){
  result1 <- result
  result1[which(result1 < i)] <- 0
  result1[which(is.na(result1))] <- 0
  result1[which(result1 >= i)] <- 1
  result2 <- data.frame(som_id=c(1:72),class=result1)
  som_result <- data.frame(som_id=som_map$unit.classif,Id=data[,1])
  som_map.unit.classif.72.pollutedarea2 <- left_join(som_result,result2,by = c("som_id" = "som_id"))
  #validation
  # validated_point_j <- left_join(pollution_point,som_map.unit.classif.72.pollutedarea2,by = c("Id" = "Id"))
  # tp <- length(which(validated_point_j$sta1==1&validated_point_j$class==1))
  # validated_point_j$class[is.na(validated_point_j$class)] <- 0
  # tn <- length(which(validated_point_j$sta1==0&validated_point_j$class==0))
  # p <- tp/(tp+468-tn)
  # r <- tp/118
  # f <- 2*p*r/(p+r)
  # f_all <- c(f_all,f)
  
  data_T <- data[,-c(1:5)]
  data_T_M <- as.matrix(data_T)
  data_T_M[data_T_M < 160] <- 0 #daily max 8-hour mean
  data_T_M[data_T_M >= 160] <- 1
  np <- sum(data_T_M)
  Np <- 216*55644
  
  Nz <- 216*sum(som_map.unit.classif.72.pollutedarea2$class) #SOM
  
  validated_point_j <- left_join(som_map.unit.classif.72.pollutedarea2,data,by = c("Id" = "Id"))
  validated_point_j <- validated_point_j[which(validated_point_j$class==1),]
  validated_point_j_T <- validated_point_j[,-c(1:7)]
  validated_point_j_T_M <- as.matrix(validated_point_j_T)
  validated_point_j_T_M[validated_point_j_T_M < 160] <- 0 #daily max 8-hour mean
  validated_point_j_T_M[is.na(validated_point_j_T_M)] <- 0
  validated_point_j_T_M[validated_point_j_T_M >= 160] <- 1
  nz <- sum(validated_point_j_T_M)
  
  LLR = nz*log(nz/Nz) + (np-nz)*log((np-nz)/(Np-Nz))
  f_all <- c(f_all,LLR)
  #print(f)
}


f_all <- data.frame(Threshold=c(1:27)/100,Fscore=f_all/10000)
library(ggplot2)
ggplot(f_all, aes(x = Threshold, y = Fscore)) +
  geom_line(linetype = 1,size=1)+
  geom_point(aes(0.07,-180.91),size=2,colour="red")+
  annotate("text", x=0.07, y=-179, label="(0.07, -180.91)",size=4.4)+
  #geom_point(size = 4, shape = 1, fill = "white")+
  scale_x_continuous(name = "Ratio",breaks=c(0.0,0.1,0.2,0.3,0.4)) +
  scale_y_continuous(name = "LLR(×10^4)",breaks=c(-220,-210,-200,-190,-180))+
  theme_bw()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16))
  #geom_hline(yintercept=0.88,linetype="dashed", size=0.6, color="red") + 
  #geom_vline(xintercept=0.23,linetype="dashed", size=0.6, color="red")
ggsave("E:\\leedoy\\ozone_pollution\\Ozone_pullution_zone\\spatio_temporal_clustering\\1test\\3.jpg",width=24,height=18,unit="cm",dpi=300)

write.csv(som_map.unit.classif.72.pollutedarea2,"E:\\leedoy\\ozone_pollution\\Ozone_pullution_zone\\spatio_temporal_clustering\\1test\\som_map.unit.classif.72.pollutedarea3_1.csv",row.names = F)

##############join to shp##############
quxian_shp <- readShapePoly("E:\\leedoy\\ozone_pollution\\Ozone_data\\shp\\grid_grid_pollutedarea3.shp")
som_map.summary <- data.frame(Id = data$Id, SOM_id=som_map$unit.classif)
#kmeans_id <- data.frame(id= c(1:length(som_map.hc[1:72])),kmeans_id=som_map.hc[1:72])
#som_map.summary <- left_join(som_map.summary,kmeans_id,by = c("SOM_id" = "id"))
som_map.summary <- left_join(som_map.summary,result2,by = c("SOM_id" = "som_id"))
#write.csv(som_map.summary,"E:\\leedoy\\ozone_pollution\\Ozone_pullution_zone\\spatio_temporal_clustering\\1test\\som_map.summary_pollutedarea.csv")
quxian_shp_temp <- quxian_shp
quxian_shp_temp@data <- left_join(quxian_shp_temp@data,som_map.unit.classif.72.pollutedarea2,by = c("Id" = "Id"))
writePolyShape(quxian_shp_temp,"E:\\leedoy\\ozone_pollution\\Ozone_data\\shp\\grid_grid_som_pollutedarea3_1.shp")

