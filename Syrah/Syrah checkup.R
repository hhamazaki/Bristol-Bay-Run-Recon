#**************************************************************************
#Project Name: SYRAH ANNUAL - Evaluate output files
#Creator: Curry James Cunningham, SAFS, University of Washington
#Date: 3.14.12
#
#Purpose: Read output files for ADMB reconstruction and create descriptive plots
#**************************************************************************
require(PBSmodelling)

setwd("/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah")
#Testing
data <- readList("WestSide_2006.out")
age.df <- data.frame(c(1:data$ngroups),data$RunSize, data$escByGroup, data$catchByGroup[,1],  rep(data$AgeCompLabels,3))
names(age.df) <- c('group','RunSize', 'esc', 'catch', 'AgeComp')

age.df$AgeComp <- as.factor(age.df$AgeComp)

ac.type <- unique(age.df$AgeComp)

sum.run <- vector(length=length(ac.type))
sum.esc <- vector(length=length(ac.type))
sum.catch <- vector(length=length(ac.type))

ac <- 1
for(ac in 1:length(ac.type)) {
  sum.run[ac] <- sum(age.df$RunSize[age.df$AgeComp==ac.type[ac]])
  sum.esc[ac] <- sum(age.df$esc[age.df$AgeComp==ac.type[ac]])
  sum.catch[ac] <- sum(age.df$catch[age.df$AgeComp==ac.type[ac]])
}
prop.run <- sum.run/sum(sum.run)
prop.esc <- sum.esc/sum(sum.esc)
prop.catch <- sum.catch/sum(sum.catch)

(table.sums <- data.frame(ac.type, round(sum.run,4), round(prop.run,4) , round(sum.esc,4) , round(prop.esc,4), round(sum.catch,4), round(prop.catch,4)))


cbind(c(1:data$ngroups), round(data$obsAgeCompCatch,4), round(data$predAgeCompCatch/sum(data$predAgeCompCatch),4))

cbind(data$predCatch,data$obsCatch)
cbind(data$predEsc, data$obsEsc)

sum(data$predAgeCompEsc[1,])
sum(data$predAgeCompEsc[2,])
sum(data$predAgeCompEsc[3,])

escAgeComp_pred
 1.37835e-10 2.0845e-10 2.04475e-10 1.67647e-10 2.02522e-10 1.95049e-10 719.478 1371.2 5.89949 1.9406e-10 1.43903e-10 2.43628 12.6881 1.61816e-10 1.38268e-10 1.82197e-10 1.68828e-10 1.589e-10
 6.85544e-12 0.430194 1.32615e-09 3.16668e-11 2.2504e-10 2.7626e-10 2158.37 829.143 1.57956 2.20027e-10 7.26385e-12 21.9433 12.0698 2.76822e-11 7.17633e-12 7.43462e-11 3.45076e-11 2.77871e-11
 5.01385e-11 7.86047 63.6964 11.997 1.38855e-10 1.18657 273.122 1316.1 64.0797 1.22949 5.05512e-11 1.3906e-10 8.35338 7.80826e-11 5.05512e-11 1.06113e-10 8.34906e-11 7.872e-11

truepred <- matrix(nrow=data$nstocks, ncol=data$nagecomps)

s <- 1 
for(s in 1:data$nstocks) {
  ac <- 1
  for(ac in 1:data$nagecomps) {
    truepred[s,ac] <- data$predAgeCompEsc[s,ac]#/data$predEsc[s]  	
  }	
}

round(truepred,3)
tempSum <- c(0,0,0)
NLL <- 0
SS <- c(1057,1299,1255)
#Trial likelihood calc POISSON ESC AC
s <- 1
for(s in 1:data$nstocks) {
  
  
  ac <- 1
  for(ac in 1:data$nagecomps) {
    temp <- truepred[s,ac]
    if(temp < 1e-06) {
      temp <- 1e-06/(2-temp/1e-06)	
    } 
    tempSum[s] <- tempSum[s] + data$obsAgeCompEsc[s,ac]*log(temp)	
  }	
  NLL <- NLL + -1*SS[s]*tempSum[s]
}
print(NLL)

########### Checking Escapement Like #########
plot.agecomp <- 

sum(data$escByGroup[1:18])
sum(data$escByGroup[19:36])
sum(data$escByGroup[37:length(data$escByGroup)])

data$predEsc

NLL <- 0
for(s in 1:3) {
  NLL <- NLL + data$predEsc[s] - data$obsEsc[s]*log(data$predEsc[s] + 1e-10) 	
}

data$predCatch - data$obsCatch*log(data$predCatch + 1e-10)


## Ensuring esc calc correctly
data$escByGroup
data$catchByGroup
data$RunSize

cbind(data$RunSize-data$catchByGroup, data$escByGroup)


