
IF_para_select_root <- "E:\\leedoy\\ozone_pollution\\Ozone_pullution_zone\\image_fusion\\1test\\parameters_selection1\\"
IF_para_select <- list.files(IF_para_select_root)

pollution_point <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_pullution_zone\\precision_validation\\sta_pollutedarea3_160.csv",stringsAsFactors = F,header = T)
f_all <- c()
for(i in 1:9){
  samples <- read.csv(paste(IF_para_select_root,IF_para_select[i],sep=''),stringsAsFactors = F,header = T)
  samples <- samples[which(samples$class==1),]
  #validation
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
}

