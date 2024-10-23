require(PBSmodelling)
require(R2admb)
require(openxlsx)
require(reshape2)
require(ggplot2)
require(beanplot)
require(mcmcplots)
require(RColorBrewer)


################################
setwd(paste(wd, "/magma", sep=""))
year <- 2018
dat <- read.csv(paste0("RR_MAGMA_",year,".csv"))
 dist_stock <- data.frame(dist=c(rep(324,5),rep(322,5),rep(321,5),rep(325,3)), dist.n=c(rep('Naknek-Kvichak',5),rep('Egegik',5),rep('Ugashik',5),rep('Nushagak',3)),
 stock= c(rep(c('Kvichak','Alagnak','Naknek','Egegik','Ugashik'),3),'Igushik', 'Wood', 'Nushagak')) 

  
#windows(record=TRUE)
########  Add Texts  #######################################################
par(mfrow=c(3,5),mar = c(1,1,1,1),oma = c(6,6,2,2)) 
for(i in 1:15){
barplot(t(dat[which(dat$dist==dist_stock[i,1]&dat$stock==dist_stock[i,3]),c('Prop','mean')]),col=c(1,'white'),beside=TRUE,ylim=c(0,1),
 names.arg = c("0.1","0.2","0.3","0.4","0.5",
                     "1.1","1.2","1.3","1.4","1.5",
                     "2.1","2.2","2.3","2.4",
                     "3.1","3.2","3.3","3.4"),las=3,
					 yaxt='n'
					 )
 if(i %in% c(1:5)) title(main = paste(dist_stock[i,3]))
 if(i %in% c(1,6,11)) { 
      axis(2, seq(0,1,0.2),las=3, font=2)
 mtext(paste(dist_stock[i,2]),side=2,line =2.5)
     }
 
 if(i==1)legend('topleft',legend=c('RR','MAGMA'), fill= c(1,'white'),bty='n')  
}

########  Add Texts  #######################################################
mtext('Age prop', side = 2, line = 4, outer = TRUE)
mtext("Scale Age", side = 1, line = 2, outer = TRUE,las=1)

#windows(record=TRUE)
########  Add Texts  #######################################################
year <- 2022
dat <- read.csv(paste0("RR_MAGMA_",year,".csv"))
#par(mfrow=c(5,3),mar = c(1,1,1,1),oma = c(6,6,2,2)) 
for(i in 16:18){
barplot(t(dat[which(dat$dist==dist_stock[i,1]&dat$stock==dist_stock[i,3]),c('Prop','mean')]),col=c(1,'white'),beside=TRUE,ylim=c(0,1),
 names.arg = c("0.1","0.2","0.3","0.4","0.5",
                     "1.1","1.2","1.3","1.4","1.5",
                     "2.1","2.2","2.3","2.4",
                     "3.1","3.2","3.3","3.4"),las=3,
					 yaxt='n'
					 )
# if(i %in% c(16:18)) title(main = paste(dist_stock[i,3]))
 if(i %in% c(16)) { 
      axis(2, seq(0,1,0.2),las=3, font=2)
# mtext(paste(dist_stock[i,2]),side=2,line =2.5)
     }
 
# if(i==16)legend('topleft',legend=c('RR','MAGMA'), fill= c(1,'white'),bty='n')
if(i==16) legend('topright',legend=year,bty='n')  
}

########  Add Texts  #######################################################
mtext('Age prop', side = 2, line = 4, outer = TRUE)
mtext("Scale Age", side = 1, line = 2, outer = TRUE,las=1)




temp.RR <- aggregate(c.RR~stock+Age,FUN=sum, data=dat)
temp.magma <- aggregate(c.magma~stock+Age,FUN=sum, data=dat)
Catch.age <- merge(temp.RR,temp.magma, by = c('stock','Age'),all=TRUE)
stock.t <- c('Kvichak','Alagnak','Naknek','Egegik','Ugashik','Igushik', 'Wood', 'Nushagak')
par(mfrow=c(2,4),mar = c(2,1.5,1.5,2),oma = c(3,3,3,3))
for(i in 1:8){
barplot(t(Catch.age[which(Catch.age$stock==stock.t[i]),3:4]),col=c(2,3),beside=TRUE,
 names.arg = c("0.1","0.2","0.3","0.4","0.5",
                     "1.1","1.2","1.3","1.4","1.5",
                     "2.1","2.2","2.3","2.4",
                     "3.1","3.2","3.3","3.4"),
 main = paste(stock.t[i]),las=3
   )
}
legend('topright',legend=c('RR','magma'), fill= c(2,3),bty='n') 
########  Add Texts  #######################################################
mtext(paste("Harvest by stock,Age",year), side = 3, line = 0, outer = TRUE)
mtext('Harvest', side = 2, line = 1, outer = TRUE)
mtext("Age", side = 1, line = 1, outer = TRUE)

temp.RR <- aggregate(c.RR~dist+Age,FUN=sum, data=RR.t)
temp.magma <- aggregate(c.magma~dist+Age,FUN=sum, data=RR.t)
Catch.age <- merge(temp.RR,temp.magma, by = c('dist','Age'),all=TRUE)
ob.Catch.age <- merge(ob.Catch.age, Catch,by=c('dist'),all=TRUE)
ob.Catch.age[,2:19] <-ob.Catch.age[,2:19]*ob.Catch.age[,20]
  
dist <- c(324,322,321,325)
dist.name <- c('Naknek-Kvichak','Egegik','Ugashik','Nushagak')
par(mfrow=c(2,2),mar = c(2,1.5,1.5,2),oma = c(3,3,3,3))
for(i in 1:4){
temp1 <- t(Catch.age[which(Catch.age$dist==dist[i]),c('c.RR','c.magma')])
temp2 <- ob.Catch.age[which(ob.Catch.age$dist==dist[i]),2:19]
colnames(temp1) <- names(temp2)
barplot(as.matrix(rbind(temp1,temp2)),col=c(2,3,4),beside=TRUE,
 names.arg = c("0.1","0.2","0.3","0.4","0.5",
                     "1.1","1.2","1.3","1.4","1.5",
                     "2.1","2.2","2.3","2.4",
                     "3.1","3.2","3.3","3.4"),
 main = paste(dist[i],dist.name[i]),las=3
   )
}
legend('topright',legend=c('RR','magma','obs'), fill= c(2,3,4),bty='n') 
########  Add Texts  #######################################################
mtext(paste("Harvest by Dist,Age",year), side = 3, line = 0, outer = TRUE)
mtext('Harvest', side = 2, line = 1, outer = TRUE)
mtext("Age", side = 1, line = 1, outer = TRUE)

