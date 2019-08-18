library(ggplot2)
library(plotly)

########
pollutionzone_ozone_samples_jion_cities <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_pollution_diffusiontype\\pollutionzone_ozone_samples_jion_cities_samplesdata1.csv",stringsAsFactors = F,header = T)
samples <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_data\\Ozone_data_pre\\incident_2017_0515_0523\\sample_hour_8mean\\201705.csv",stringsAsFactors = F,header = T)
samples <- left_join(pollutionzone_ozone_samples_jion_cities,samples,by = c("code" = "code"))
write.csv(samples,"E:\\leedoy\\ozone_pollution\\Ozone_pollution_diffusiontype\\pollutionzone_ozone_samples_jion_cities_samplesdata.csv")
#mean of each city
samples <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_pollution_diffusiontype\\pollutionzone_ozone_samples_jion_cities_samplesdata.csv",stringsAsFactors = F,header = T)
ZONECODE <- samples$ZONECODE
ZONECODE <- ZONECODE[!duplicated(ZONECODE)]
samples1 <- samples[1,]
samples1 <- samples1[,-c(1,6)]
for (i in 1:length(ZONECODE)) {
  samples_temp <- samples[which(samples$ZONECODE==ZONECODE[i]),]
  samples1$x[i] <- mean(samples_temp$x)
  samples1$y[i] <- mean(samples_temp$y)
  samples1$ZONECODE[i] <- ZONECODE[i]
  samples1$PYNAME[i] <- samples_temp$PYNAME[1]
  samples1$city[i] <- samples_temp$city[1]
  for(j in 1:length(6:length(samples1))) {
    samples1[i,(5+j)] <- mean(samples_temp[,c(7+j)])
  }
  samples1 <- rbind(samples1,0)
}
samples1 <- samples1[-length(samples1$ZONECODE),]
write.csv(samples1,"E:\\leedoy\\ozone_pollution\\Ozone_pollution_diffusiontype\\pollutionzone_ozone_samples_jion_cities_samplesdata1.csv")
#diff
samples <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_pollution_diffusiontype\\pollutionzone_ozone_samples_jion_cities_samplesdata2.csv",stringsAsFactors = F,header = T)
samples_diff <- t(data.frame(c(1:8)))
for(i in 1:length(samples$ZONECODE)) {
  samples_diff[i,] <- t(data.frame(X=diff(as.numeric(samples[i,c(2:length(samples))]), lag = 1)))
  samples_diff <- rbind(samples_diff,0)
}
samples_diff <- samples_diff[-104,]
samples_diff <- cbind(samples,samples_diff)

samples_diff <- cbind(samples_diff,samples_diff[,c(11:18)]/samples_diff[,c(2:9)])
write.csv(samples_diff,"E:\\leedoy\\ozone_pollution\\Ozone_pollution_diffusiontype\\samples_diff2.csv")
#
samples <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_pollution_diffusiontype\\samples_diff1.csv",stringsAsFactors = F,header = T)
pollutinzone_cities <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_pollution_diffusiontype\\pollutinzone_cities.csv",stringsAsFactors = F,header = T)
ZONECODE <- samples$ZONECODE
ZONECODE <- ZONECODE[!duplicated(ZONECODE)]

samples <- samples[,-c(1,2,4,5)]
samples1 <- samples[1,]
for(i in 1:length(ZONECODE)) {
  up_city1 <- pollutinzone_cities$up_city1[which(pollutinzone_cities$ZONECODE==ZONECODE[i])]
  up_city2 <- pollutinzone_cities$up_city2[which(pollutinzone_cities$ZONECODE==ZONECODE[i])]
  samples_temp <- (samples[which(samples$ZONECODE==up_city1),][2:length(samples)] + samples[which(samples$ZONECODE==up_city2),][2:length(samples)])/2
  samples1$ZONECODE[i] <- ZONECODE[i]
  samples1[i,(2:length(samples))] <- samples_temp
  samples1 <- rbind(samples1,0)
}

samples1 <- samples1[-104,]
samples1 <- samples1[,-1]
samples <- cbind(samples,samples1)
write.csv(samples,"E:\\leedoy\\ozone_pollution\\Ozone_pollution_diffusiontype\\samples_diff1.1.csv")
#day_lag <- c(24,24,24,24,24,24,24,12,21)

########
#Hierarchical Clustering
samples <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_pollution_diffusiontype\\samples_diff1.1.1pca.csv",stringsAsFactors = F,header = T)

row.names(samples) <- samples$ZONECODE
samples <- samples[,-(1:5)]
#samples.scale <- scale(samples)
dist <- dist(samples, method = "euclidean")

hc <- hclust(dist, method="ward.D")
plot(hc, hang=-1, cex=.8, main="Clustering")
hc_c <- rect.hclust(hc,k=12)

samples_hc <- cbind(samples,hc=0)
for(i in 1:length(hc_c)) {
  samples_hc$hc[as.numeric(hc_c[[i]])] <- i
}
write.csv(samples_hc,"E:\\leedoy\\ozone_pollution\\Ozone_pollution_diffusiontype\\samples_diff1.1.1pca_hc.csv")

#
r2 <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_pollution_diffusiontype\\SAS_result\\SAS_result1.csv",stringsAsFactors = F,header = T)
corplot <- ggplot(r2, aes(x=clusters, y=value, color=class,shape=class)) + 
  geom_line() +
  geom_point(size=2) +
  labs(x = "聚类数", y = "", title = "")+ ##添加标题
  xlim(0,50)+
  ylim(0,1)+
  theme(panel.background=element_rect(fill="white",color="grey50"),
        panel.grid=element_line(color="grey50",size=2),
        panel.grid.major=element_line(size=1,linetype =3,color="grey70"),
        panel.grid.minor=element_line(size=1,linetype =3,color="grey70"))
ggsave("E:\\leedoy\\ozone_pollution\\Ozone_pollution_diffusiontype\\plot\\clusters_selection1.1.jpg", corplot, width = 8, height = 6)

r2 <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_pollution_diffusiontype\\SAS_result\\SAS_result2.csv",stringsAsFactors = F,header = T)
corplot <- ggplot(r2, aes(x=clusters, y=value, color=class,shape=class)) + 
  geom_line() +
  geom_point(size=2) +
  labs(x = "聚类数", y = "", title = "")+ ##添加标题
  xlim(0,50)+
  theme(panel.background=element_rect(fill="white",color="grey50"),
        panel.grid=element_line(color="grey50",size=2),
        panel.grid.major=element_line(size=1,linetype =3,color="grey70"),
        panel.grid.minor=element_line(size=1,linetype =3,color="grey70"))
ggsave("E:\\leedoy\\ozone_pollution\\Ozone_pollution_diffusiontype\\plot\\clusters_selection2.1.jpg", corplot, width = 8, height = 6)


#############################################################################################
#hc_c <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_pollution_diffusiontype\\samples_diff1.1.1pca_hc.csv",stringsAsFactors = F,header = T)
samples <- read.csv("E:\\leedoy\\ozone_pollution\\Ozone_pollution_diffusiontype\\samples_diff1.1.1.csv",stringsAsFactors = F,header = T)
samples <- cbind(hc=samples_hc$hc,samples)
#samples <- left_join(hc_c,samples,by = c("ZONECODE" = "ZONECODE"))

mean <- data.frame(hc=c(1:12),mean1=0,mean2=0,mean3=0,mean4=0)
for(i in 1:12){
  samples_temp <- samples[which(samples$hc==i),]
  # mean$mean1[i] = mean(as.matrix(samples_temp[,c(5:204)]))
  # mean$mean2[i] = mean(as.matrix(samples_temp[,c(205:404)]))
  # mean$mean3[i] = mean(as.matrix(samples_temp[,c(405:604)]))
  # mean$mean4[i] = mean(as.matrix(samples_temp[,c(605:804)]))
  mean$mean1[i] = sd(as.matrix(samples_temp[,c(5:204)]))
  mean$mean2[i] = sd(as.matrix(samples_temp[,c(205:404)]))
  mean$mean3[i] = sd(as.matrix(samples_temp[,c(405:604)]))
  mean$mean4[i] = sd(as.matrix(samples_temp[,c(605:804)]))
  #sd(as.matrix(samples[,c(5:204)]))
  
}




# samples_temp <- samples[which(samples$name=="day"),]
# plot_ly(samples_temp, x = ~day, y = ~mean, name = '总体均值', type = 'scatter', mode = 'lines',
#              line = list(color = 'rgb(205, 12, 24)', width = 4)) %>%
#   add_trace(y = ~mean.sd, name = '总体均值+1倍sd', line = list(color = 'rgb(205,12,24)', width = 4, dash = 'dash')) %>%
#   add_trace(y = ~mean.sd.1, name = '总体均值-1倍sd', line = list(color = 'rgb(205,12,24)', width = 4, dash = 'dash')) %>%
#   add_trace(y = ~C1, name = '类1均值', line = list(color = 'rgb(22,96,167)', width = 4, dash = 'dot')) %>%
#   add_trace(y = ~C2, name = '类2均值', line = list(color = 'rgb(105,105,105)', width = 4, dash = 'dot')) %>%
#   add_trace(y = ~C3, name = '类3均值', line = list(color = 'rgb(139,69,19)', width = 4, dash = 'dot')) %>%
#   add_trace(y = ~C4, name = '类4均值', line = list(color = 'rgb(255,215,0)', width = 4, dash = 'dot')) %>%
#   add_trace(y = ~C5, name = '类5均值', line = list(color = 'rgb(124,252,0)', width = 4, dash = 'dot')) %>%
#   add_trace(y = ~C6, name = '类6均值', line = list(color = 'rgb(64,224,208)', width = 4, dash = 'dot')) %>%
#   add_trace(y = ~C7, name = '类7均值', line = list(color = 'rgb(0,139,139)', width = 4, dash = 'dot')) %>%
#   add_trace(y = ~C8, name = '类8均值', line = list(color = 'rgb(0,191,255)', width = 4, dash = 'dot')) %>%
#   add_trace(y = ~C9, name = '类9均值', line = list(color = 'rgb(0,0,128)', width = 4, dash = 'dot')) %>%
#   add_trace(y = ~C10, name = '类10均值', line = list(color = 'rgb(138,43,226)', width = 4, dash = 'dot')) %>%
#   layout(title = "", xaxis = list(title = "日期"), yaxis = list (title = "浓度(ug/m3)"))

#day
samples_temp <- samples[which(samples$name=="day"),]
# ggplot(samples_temp, aes(x=name1, y=value, color=class_chinese)) + 
#   geom_line(size=1) +
#   
#   #geom_point(shape=24) + 
#   geom_point(size=2) +
#   scale_colour_hue(breaks = c('总体均值','总体均值+1倍sd',
#                                   '类1均值','类2均值','类3均值','类4均值','类5均值',
#                                   '类6均值','类7均值','类8均值','类9均值','类10均值'),
#                    h = c(0, 360))+ ##legend标题顺序
#   guides(color=guide_legend(title=NULL))+ ##legend标题为空
#   labs(x = "日期", y = "浓度(ug/m3)", title = "")+ ##添加标题
#   scale_x_continuous(breaks=seq(20170515,20170523,1))+
#   theme(panel.background=element_rect(fill="white",color="grey50"),
#         panel.grid=element_line(color="grey50",size=2),
#         panel.grid.major=element_line(size=1,linetype =3,color="grey70"),
#         panel.grid.minor=element_line(size=1,linetype =3,color="grey70"))

plot(c(20170515,20170523),c(50,200),type="n",xlab="日期",ylab="浓度(ug/m3)", xaxt="n")
axis(1,20170515:20170523, 20170515:20170523)
#axis(2,-4:4, -4:4)
abline(h=seq(50,200,by=25), v=0:8, col="gray", lty=3)#添加网格线
lines(samples_temp$day,samples_temp$mean, lty=1, col="red", lwd=2)
lines(samples_temp$day,samples_temp$mean.sd, lty=2, col="red", lwd=2)
lines(samples_temp$day,samples_temp$C1, lty=3, col="blue2", lwd=2)
lines(samples_temp$day,samples_temp$C2, lty=3, col="darkorange2", lwd=2)
lines(samples_temp$day,samples_temp$C3, lty=3, col="cyan3", lwd=2)
lines(samples_temp$day,samples_temp$C4, lty=3, col="green2", lwd=2)
lines(samples_temp$day,samples_temp$C5, lty=3, col="slateblue2", lwd=2)
lines(samples_temp$day,samples_temp$C6, lty=3, col="rosybrown4", lwd=2)
lines(samples_temp$day,samples_temp$C7, lty=3, col="deeppink", lwd=2)
lines(samples_temp$day,samples_temp$C8, lty=3, col="turquoise3", lwd=2)
lines(samples_temp$day,samples_temp$C9, lty=3, col="purple", lwd=2)
lines(samples_temp$day,samples_temp$C10, lty=3, col="gray40", lwd=2)
legend("topright", inset=0.01, cex=0.8, ncol=2,bty="", c('总体均值','总体均值+1倍sd',
                                         '类1均值','类2均值','类3均值','类4均值','类5均值',
                                         '类6均值','类7均值','类8均值','类9均值','类10均值'),
       lty=c(1,2,3,3,3,3,3,3,3,3,3,3), 
       col=c("red","red",
                                                   "blue2","darkorange2","cyan3","green2","slateblue2",
                                                   "rosybrown4","deeppink","turquoise3","purple","gray40"))

#up.day
samples_temp <- samples[which(samples$name=="up.day"),]
plot(c(20170515,20170523),c(50,200),type="n",xlab="日期",ylab="浓度(ug/m3)", xaxt="n")
axis(1,20170515:20170523, 20170515:20170523)
#axis(2,-4:4, -4:4)
abline(h=seq(50,200,by=25), v=0:8, col="gray", lty=3)#添加网格线
lines(samples_temp$day,samples_temp$mean, lty=1, col="red", lwd=2)
lines(samples_temp$day,samples_temp$mean.sd, lty=2, col="red", lwd=2)
lines(samples_temp$day,samples_temp$C1, lty=3, col="blue2", lwd=2)
lines(samples_temp$day,samples_temp$C2, lty=3, col="darkorange2", lwd=2)
lines(samples_temp$day,samples_temp$C3, lty=3, col="cyan3", lwd=2)
lines(samples_temp$day,samples_temp$C4, lty=3, col="green2", lwd=2)
lines(samples_temp$day,samples_temp$C5, lty=3, col="slateblue2", lwd=2)
lines(samples_temp$day,samples_temp$C6, lty=3, col="rosybrown4", lwd=2)
lines(samples_temp$day,samples_temp$C7, lty=3, col="deeppink", lwd=2)
lines(samples_temp$day,samples_temp$C8, lty=3, col="goldenrod3", lwd=2)
lines(samples_temp$day,samples_temp$C9, lty=3, col="purple", lwd=2)
lines(samples_temp$day,samples_temp$C10, lty=3, col="gray40", lwd=2)
legend("topright", inset=0.01, cex=0.8, ncol=2,bty="", c('总体均值','总体均值+1倍sd',
                                                         '类1均值','类2均值','类3均值','类4均值','类5均值',
                                                         '类6均值','类7均值','类8均值','类9均值','类10均值'),
       lty=c(1,2,3,3,3,3,3,3,3,3,3,3), 
       col=c("red","red",
             "blue2","darkorange2","cyan3","green2","slateblue2",
             "rosybrown4","deeppink","goldenrod3","purple","gray40"))

