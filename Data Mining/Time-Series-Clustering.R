###Load the R Package###
library(cluster)
library(TSclust)
library(zoo)
library(rlang)

###Clustering Distance Measures###
setwd("main_directory")
mydata=read.csv("data.csv",header=TRUE,stringsAsFactors = FALSE)
saham_ISSI=zoo(mydata[,-1],order.by=as.Date(as.character(mydata[,1]),"%m/%d/%Y"))
datasaham=ts(saham_ISSI)

###Distance###
jarakACF=diss(datasaham, "ACF")
matriks_jarak=as.matrix(jarakACF)
View(matriks_jarak)
hc=hclust(dist(jarakACF),method="complete",members=NULL)

###Dendrogram Plot###
plot(hc, hang=-1,cex=0.5)
abline(h=7,col="blue")
rect.hclust(hc,k=12,border="red")
hcd=as.dendrogram(hc)
##Zoom-in the Dendrogram
plot(cut(hcd,h=7)$lower[[1]],main="Cabang ke-1 pada perpotongan h=7")
plot(cut(hcd,h=7)$lower[[2]],main="Cabang ke-2 pada perpotongan h=7")
plot(cut(hcd,h=7)$lower[[3]],main="Cabang ke-3 pada perpotongan h=7")
plot(cut(hcd,h=7)$lower[[4]],main="Cabang ke-4 pada perpotongan h=7")
plot(cut(hcd,h=7)$lower[[5]],main="Cabang ke-5 pada perpotongan h=7")
plot(cut(hcd,h=7)$lower[[6]],main="Cabang ke-6 pada perpotongan h=7")
plot(cut(hcd,h=7)$lower[[7]],main="Cabang ke-7 pada perpotongan h=7")
plot(cut(hcd,h=7)$lower[[8]],main="Cabang ke-8 pada perpotongan h=7")
plot(cut(hcd,h=7)$lower[[9]],main="Cabang ke-9 pada perpotongan h=7")
plot(cut(hcd,h=7)$lower[[10]],main="Cabang ke-10 pada perpotongan h=7")
plot(cut(hcd,h=7)$lower[[11]],main="Cabang ke-11 pada perpotongan h=7")
plot(cut(hcd,h=7)$lower[[12]],main="Cabang ke-12 pada perpotongan h=7")

###Cluster  Subset###
clust=cutree(hc,k=12)
data_saham=t(data.frame(datasaham))
c1=subset(data_saham,clust==1)
c2=subset(data_saham,clust==2)
c3=subset(data_saham,clust==3)
c4=subset(data_saham,clust==4)
c5=subset(data_saham,clust==5)
c6=subset(data_saham,clust==6)
c7=subset(data_saham,clust==7)
c8=subset(data_saham,clust==8)
c9=subset(data_saham,clust==9)
c10=subset(data_saham,clust==10)
c11=subset(data_saham,clust==11)
c12=subset(data_saham,clust==12)

###summary###
desc1=cbind(1,mean(c1),(sd(c1)/mean(c1)),min(c1),max(c1))
desc2=cbind(2,mean(c2),(sd(c2)/mean(c2)),min(c2),max(c2))
desc3=cbind(3,mean(c3),(sd(c3)/mean(c3)),min(c3),max(c3))
desc4=cbind(4,mean(c4),(sd(c4)/mean(c4)),min(c4),max(c4))
desc5=cbind(5,mean(c5),(sd(c5)/mean(c5)),min(c5),max(c5))
desc6=cbind(6,mean(c6),(sd(c6)/mean(c6)),min(c6),max(c6))
desc7=cbind(7,mean(c7),(sd(c7)/mean(c7)),min(c7),max(c7))
desc8=cbind(8,mean(c8),(sd(c8)/mean(c8)),min(c8),max(c8))
desc9=cbind(9,mean(c9),(sd(c9)/mean(c9)),min(c9),max(c9))
desc10=cbind(10,mean(c10),(sd(c10)/mean(c10)),min(c10),max(c10))
desc11=cbind(11,mean(c11),(sd(c11)/mean(c11)),min(c11),max(c11))
desc12=cbind(12,mean(c12),(sd(c12)/mean(c12)),min(c12),max(c12))
desc=rbind(desc1,desc2,desc3,desc4,desc5,desc6,desc7,desc8,desc9,desc10,desc11,desc12)
desc

###Cluster Member###
output=data.frame(clust)
output=within(output,{clust<-as.factor(clust)})
View(output
