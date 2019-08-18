library(ggplot2)
library(RColorBrewer)
library(gstat)
library(lattice)
library(sp)
library(automap)

x <- seq(75, 132, 0.5)
y <- seq(18, 51, 0.5)
x_len <- length(x)
x <- rep(x, each=length(y))
y <- rep(y, x_len)
x <- rep(x, 1)
y <- rep(y, 1)
grid <- data.frame(x=x, y=y)


result = read.csv("E:\\leedoy\\Ozone_pollution\\Ozone_data\\2017_0515_0523\\result_day_pca.csv")
id = which(result$x==117.541)
result[id[1],] = (result[id[1],]+result[id[2],])/2
result$code[id[1]] = floor(result$code[id[1]])
result = result[-id[2],]

id = which(result$x==111.9975)
result[id[1],] = (result[id[1],]+result[id[2],])/2
result$code[id[1]] = floor(result$code[id[1]])
result = result[-id[2],]

result1 = data.frame(result[,c(1:4)],day="0515",stringsAsFactors = F)
names(result1) = c("code","x","y","value","day")

result1 = data.frame(result[,c(1:3,12)],day="0523",stringsAsFactors = F)
names(result1) = c("code","x","y","value","day")
# l = length(result1$code)
# for(i in 1:8){
#   result1 = rbind(result1,result1)
#   result1$value[c((l*i+1):(l*(i+1)))] = result[,i+4]
#   result1$day[c((l*i+1):(l*(i+1)))] = paste("0",as.character(515+i),sep = '')
# }

result2 = result1
f_grid <- grid
coordinates(f_grid) <- ~x+y
gridded(f_grid) = TRUE

coordinates(result2) <- ~x+y
model = autofitVariogram(value~1,result2,model = c("Sph", "Exp", "Gau","Mat"))
ok = krige(value~1,result2, f_grid, model = model$var_model)

result3 = cbind(grid,value=ok$var1.pred)

corplot=
ggplot(data = result1,mapping = aes(x=x, y=y, z=value))+
  stat_contour(data = result3,mapping = aes(x=x,y=y,z=value),color="red")+
  geom_point(size = 1, aes(colour=value))+
  scale_colour_gradient(low = 'blue', high = 'yellow',breaks = c(-500,-400,-300,-200,-100,0,100,200,300,400,500))+
  #stat_density2d()+
  #annotate("text",label=paste("r == ", r, sep=''),
  #        parse=TRUE, x=-Inf, y=Inf, hjust= -0.2, vjust= 2)+
  ggtitle("PCA0523")+
  labs(x="X",y="Y")+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~day,scales = "free")


ggsave(paste("E:\\leedoy\\pca0523.jpeg", sep=''), corplot, width = 10, height = 6)





library(ggplot2)
library(easyGgplot2)

result1 =data.frame(x=c(1,2,3,1,2,3,1,2,3),y=c(1,1,1,2,2,2,3,3,3),value=c(1,2,3,4,5,6,7,8,9))
v <- ggplot(result1, aes(x, y, z = value))
v = v + geom_point(size = 1, aes(colour=value))
v + geom_contour()




