require(PBSmodelling)
require(R2admb)
require(openxlsx)
require(reshape2)
require(ggplot2)
require(beanplot)
require(mcmcplots)
require(RColorBrewer)


############## CONTROL SECTION ##############
#Set working directory
# wd <- "O:/DCF/REG2/BBsalmon/Run Reconstruction/2019/Syrah Annual_updated_22Oct18/Syrah Annual"

# wd <- "/Users/curryc2/Documents/Curry's SYRAH Work/Bristol-Bay-Run-Recon"
# NOTE IF YOU SET WORKING DIRECTORY TO THE GITHUB REPO THIS SHOULD WORK FINE...
#wd <- "C:/Projects/Bristol_Bay/Bristol-Bay-Run-Recon-master"
wd <- file.path("C:","Projects","Bristol_Bay","Bristol-Bay-Run-Recon")
create.annual.catch.summary <- function(year, side, wd=wd) {
  setwd(paste(wd, "/Syrah/outputFiles", sep=""))
  ### TESTING ###
#   year <- 2022
#   side <- 'east'
  ###############
  
  if(side == 'west') {
    n.districts <- 1
    n.stocks <- 3
    names.districts <- 'Nushagak'
    names.stocks <- c('Igushik', 'Wood', 'Nushagak')
    esc.dist <- c(325,325,325)
    esc.stream <- c(100,300,700)
  }
  if(side == 'east') {
    n.districts <- 3
    n.stocks <- 5
    names.districts <- c('Naknek-Kvichak','Egegik','Ugashik')
    names.stocks <- c('Kvichak','Alagnak','Naknek','Egegik','Ugashik')	
    esc.dist <- c(324,324,324,322,321)
    esc.stream <- c(100,500,600,100,100)
  }
  if(side != 'west' & side != 'east') { print('##### ERROR side selection is incorrect'); stop(); }
  
#  ac.data <- na.omit(read.csv(paste(wd, "/R/ageComp.annual.csv", sep=""), header=TRUE, stringsAsFactors=FALSE)[,-1])
  
  #Read the data
  if(side == 'west') {
    temp.data <- readList(paste('WestSide/WestSide_',year,'.out',sep='')) 
  }else {
    temp.data <- readList(paste('EastSide/EastSide_',year,'.out',sep=''))  
  }
   
  #Pertinent variables
  n.agecomps <- temp.data$nagecomps
  agecomps <- temp.data$AgeCompLabels
  
   temp.catch.s <- list() 
  ##### FILL IN THE ARRAY #####
  temp.catch <- temp.data$catchByGroup

  for(s in 1:n.stocks) {
    ref <- ((s-1)*n.agecomps+1):(s*n.agecomps) 
    if(side == 'west') {
	  temp.catch.s[[s]]<- temp.catch[ref]/sum(temp.catch[ref])  
    }else {
	  temp.catch.s[[s]] <- temp.catch[,ref]/rowSums(temp.catch[,ref]) 
    }
  }#next s
  RR <- as.data.frame(do.call(rbind,temp.catch.s))
  names(RR) <-  c("0.1","0.2","0.3","0.4","0.5",
                     "1.1","1.2","1.3","1.4","1.5",
                     "2.1","2.2","2.3","2.4",
                     "3.1","3.2","3.3","3.4")
  if(side == 'east') {				 
  RR$stock <- c(rep('Kvichak',3),rep('Alagnak',3),rep('Naknek',3),rep('Egegik',3),rep('Ugashik',3))
  RR$dist <- rep(c(324,322,321),5) 
   } else {
  RR$stock <- names.stocks 
  RR$dist <- 325 
   }  
   out <- list(
   RR = RR,
   Catch = temp.data$obsCatch,
   CatchAge = temp.data$obsAgeCompCatch,
   CatchGen = temp.data$obsGenComp
   )
   return(out)
  }

################################
 setwd(paste(wd, "/magma", sep=""))
    year <- 2018
     magma.data <- readRDS(paste0("stock_age_",year,"_stats_dis.Rds")) 
	 magma <- as.data.frame(do.call(rbind,magma.data))
	 magma$dist <- c(rep(321,11*12),rep(322,11*12),rep(324,11*12),rep(325,11*12),rep(326,11*12))
	 magma$Age <- paste0(substring(magma$age,1,1),'.',substring(magma$age,2,2))
	 names(magma)[1] <- 'stock'
 west <- create.annual.catch.summary(year,'west',wd)
 east <- create.annual.catch.summary(year,'east',wd)
 
 RR.s <- rbind(east$RR,west$RR)
 RR <- melt(RR.s,id.vars = c('dist','stock'), variable.name='Age',value.name='Prop')
 RR.t <- merge(RR,magma[,c('stock','dist','Age','mean')],by=c('dist','stock','Age'),all=TRUE)
 Catch <- data.frame(dist = c(324,322,321,325),Catch = c(east$Catch,west$Catch))
 Stock <- data.frame(dist=c(rep(324,5),rep(322,5),rep(321,5),rep(325,3)),stock=c(rep(c('Kvichak','Alagnak','Naknek','Egegik','Ugashik'),3),'Igushik', 'Wood', 'Nushagak'),ps =  c(t(east$CatchGen),west$CatchGen))
 ob.Catch.age <- data.frame(rbind(east$CatchAge,west$CatchAge))
 ob.Catch.age$dist <- c(324,322,321,325)
 ob.Catch.age.p <- cbind(ob.Catch.age$dist,ob.Catch.age[,-1]/rowSums(ob.Catch.age[,-1]))
 names(ob.Catch.age.p) <- c('dist',c("0.1","0.2","0.3","0.4","0.5",
                     "1.1","1.2","1.3","1.4","1.5",
                     "2.1","2.2","2.3","2.4",
                     "3.1","3.2","3.3","3.4"))
 Catch.age.p <- melt(ob.Catch.age.p,variable.name='Age',id='dist',value.name='CatchP')
 
 RR.t <- merge(RR.t,Catch,by=c('dist'),all=TRUE)
 RR.t <- merge(RR.t,Stock,by=c('dist','stock'),all=TRUE)
 RR.t <- merge(RR.t,Catch.age.p,by=c('dist','Age'),all=TRUE)
 RR.t$c.RR  <- with(RR.t, Prop*Catch*ps)
 RR.t$c.magma  <- with(RR.t, mean*Catch*ps)
 RR.t <- RR.t[order(RR.t$dist,RR.t$stock,RR.t$Age),]
 head(RR.t)
 dist_stock <- data.frame(dist=c(rep(324,5),rep(322,5),rep(321,5),rep(325,3)), dist.n=c(rep('Naknek-Kvichak',5),rep('Egegik',5),rep('Ugashik',5),rep('Nushagak',3)),
 stock= c(rep(c('Kvichak','Alagnak','Naknek','Egegik','Ugashik'),3),'Igushik', 'Wood', 'Nushagak')) 
#windows(record=TRUE)
########  Add Texts  #######################################################
par(mfrow=c(4,5),mar = c(2,2,3,2),oma = c(3,3,3,3)) 
for(i in 1:18){
 barplot(t(RR.t[which(RR.t$dist==dist_stock[i,1]&RR.t$stock==dist_stock[i,3]),c('Prop','mean')]),col=c(2,3),beside=TRUE,ylim=c(0,1),
 names.arg = c("0.1","0.2","0.3","0.4","0.5",
                     "1.1","1.2","1.3","1.4","1.5",
                     "2.1","2.2","2.3","2.4",
                     "3.1","3.2","3.3","3.4")
					 ,
 main = paste(dist_stock[i,1],dist_stock[i,2],'\n',dist_stock[i,3]),las=3
)   
}
legend('topright',legend=c('RR','magma'), fill= c(2,3),bty='n') 
########  Add Texts  #######################################################
mtext(paste("Harvest age prob by dist,stock",year), side = 3, line = 0, outer = TRUE,las=1)
mtext('Age prop', side = 2, line = 1, outer = TRUE)
mtext("Age", side = 1, line = 1, outer = TRUE,las=1)

temp.RR <- aggregate(c.RR~stock+Age,FUN=sum, data=RR.t)
temp.magma <- aggregate(c.magma~stock+Age,FUN=sum, data=RR.t)
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

