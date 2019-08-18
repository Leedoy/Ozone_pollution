
library(Matrix)
library(arules)
library(arulesViz)
library(dplyr)

data <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_data\\201705_polutedarea3_8mean.csv",stringsAsFactors = F,header = T)
data_T <- as.data.frame(t(data))
data_T <- data_T[-c(1:5),]
#data.frame to matrix
data_T_M <- as.matrix(data_T)
# to 0 or 1
data_T_M[data_T_M < 160] <- 0 #daily max 8-hour mean
data_T_M[data_T_M >= 160] <- 1
## set dim names
dimnames(data_T_M) <- list(c(1:201), c(data$Id))
## delete invalidate
id_0 <- which(colSums(data_T_M)==0)
data_T_M <- data_T_M[,-id_0]
id_0 <- which(rowSums(data_T_M)==0)
data_T_M <- data_T_M[-id_0,]
#write.csv(data_transaction, "E:\\leedoy\\ozone_pollution\\Ozone_pullution_zone\\frequent_pattern\\1test\\code\\Apriori-master\\data_transaction.csv", quote=TRUE, row.names=FALSE)
#matrix to transactions
data_transaction <- as(data_T_M, "transactions")
#summary(data_transaction)
#summary(size(data_transaction))
#sort(itemFrequency(data_transaction), decreasing=T)#max_supp min_supp
#itemFrequencyPlot(data_transaction, support=0.43)
#inspect(data_transaction)
#apriori
rules <- apriori(data_transaction, parameter = list(supp = 0.50, conf = 0.7, target = "rules", minlen=2))
#summary(rules)
#inspect(rules[1:5])
#ordered_rules <- sort(rules, by="lift")
#inspect(ordered_rules[1:5])
#plot(rules0,measure = c("support","confidence"),shading="lift")
# plot(rules,measure = c("support","confidence"),shading="order")
# plot(rules,method="grouped")
# plot(rules,method = "paracord")
# plot(rules,method = "graph")
#write(rules, file="E:\\leedoy\\ozone_pollution\\Ozone_pullution_zone\\frequent_pattern\\1test\\rules.csv", sep=",", quote=TRUE, row.names=FALSE)
rules.summary.lhs.rhs <- c(rules@lhs@data@i,rules@rhs@data@i)
rules.summary.lhs.rhs <- rules.summary.lhs.rhs[!duplicated(rules.summary.lhs.rhs)]
rules.summary <- data_transaction@itemInfo$labels[rules.summary.lhs.rhs]
length(rules.summary)

rules.summary.lhs.rhs <- which(itemFrequency(data_transaction)>0.09)
rules.summary <- data_transaction@itemInfo$labels[rules.summary.lhs.rhs]
write.csv(rules.summary, file="E:\\leedoy\\ozone_pollution\\Ozone_pullution_zone\\frequent_pattern\\1test\\160_20190103\\rules.summary_polutedarea3_8mean_0.09_0.7.csv", quote=TRUE, row.names=FALSE)

######################
ozone_root <- "E:\\leedoy\\Ozone_pollution\\Ozone_pullution_zone\\frequent_pattern\\1test\\160_20190103\\"
ozone_listfiles <- list.files(ozone_root)

pollution_point <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_pullution_zone\\precision_validation\\sta_pollutedarea3_160.csv",stringsAsFactors = F,header = T)

f_all <- c()
for(i in 1:length(ozone_listfiles)){
  samples <- read.csv(paste(ozone_root,ozone_listfiles[i],sep=''),stringsAsFactors = F,header = T)

  # validated_point_j <- left_join(pollution_point,samples,by = c("Id" = "Id"))
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
  
  Nz <- 216*sum(samples$class) #Apriori
  
  validated_point_j <- left_join(samples,data,by = c("Id" = "Id"))
  validated_point_j <- validated_point_j[which(validated_point_j$class==1),]
  validated_point_j_T <- validated_point_j[,-c(1:6)]
  validated_point_j_T_M <- as.matrix(validated_point_j_T)
  validated_point_j_T_M[validated_point_j_T_M < 160] <- 0 #daily max 8-hour mean
  validated_point_j_T_M[is.na(validated_point_j_T_M)] <- 0
  validated_point_j_T_M[validated_point_j_T_M >= 160] <- 1
  nz <- sum(validated_point_j_T_M)
  
  LLR = nz*log(nz/Nz) + (np-nz)*log((np-nz)/(Np-Nz))
  f_all <- c(f_all,LLR)
  #print(f)
}

# sort(result)
f_all <- data.frame(min_sup=c(0.05,0.06,0.07,0.08,0.09,0.10,0.15,0.23,0.24,0.25,0.26,0.27,0.28,0.29,0.30),Fscore=f_all[c(1:15)]/10000)
library(ggplot2)
ggplot(f_all, aes(x = min_sup, y = Fscore)) +
  geom_line(linetype = 1,size=1)+
  #geom_smooth()+
  geom_point(aes(0.08,-176.9),size=2,colour="red")+
  annotate("text", x=0.08, y=-176.3, label="(0.08, -176.92)",size=4.2)+
  #geom_point(size = 4, shape = 1, fill = "white")+
  scale_x_continuous(name = "min_sup",breaks=c(0.05,0.15,0.25,0.35)) +
  scale_y_continuous(name = "LLR(×10^4)",breaks=c(-195,-190,-185,-180,-170))+
  theme_bw()+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12))
#geom_hline(yintercept=0.88,linetype="dashed", size=0.6, color="red") + 
#geom_vline(xintercept=0.23,linetype="dashed", size=0.6, color="red")
ggsave("E:\\leedoy\\ozone_pollution\\Ozone_pullution_zone\\frequent_pattern\\1test\\precision2.jpg",width=24,height=18,unit="cm",dpi=300)

quxian_shp <- readShapePoly("E:\\leedoy\\ozone_pollution\\Ozone_data\\shp\\grid_grid_pollutedarea3.shp",proj4string = CRS("+proj=longlat +ellps=WGS84"))
data_result <- read.csv("E:\\leedoy\\Ozone_pollution\\Ozone_pullution_zone\\frequent_pattern\\1test\\160_20190103\\rules.summary_polutedarea3_8mean_0.08_0.7.csv",stringsAsFactors = F,header = T)
quxian_shp_temp <- quxian_shp
quxian_shp_temp@data <- left_join(quxian_shp_temp@data,data_result,by = c("Id" = "Id"))
writePolyShape(quxian_shp_temp,"E:\\leedoy\\ozone_pollution\\Ozone_data\\shp\\grid_grid_fp_pollutedarea3.shp")
