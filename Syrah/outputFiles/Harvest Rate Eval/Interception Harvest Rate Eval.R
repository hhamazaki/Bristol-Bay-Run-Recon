require(PBSmodelling)

setwd("/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah/outputFiles/Harvest Rate Eval")


years <- 1963:2013
n.years <- length(years)

stocks <- c('Kvichak','Alagnak','Naknek','Egegik','Ugashik')
n.stocks <- length(stocks)

districts <- c('Kvi-Nak','Egegik','Ugashik')
n.districts <- length(districts)

avg.hr <- matrix(data=0, nrow=n.districts, ncol=n.stocks, dimnames=list(districts, stocks))
hr.arr <- array(data=0, dim=c(n.districts,n.stocks,n.years), dimnames=list(districts,stocks,years))
catch.arr <- array(data=0, dim=c(n.districts,n.stocks,n.years), dimnames=list(districts,stocks,years))
runSize <- matrix(nrow=n.stocks, ncol=n.years, dimnames=list(stocks, years))


#data <- readList("/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah/outputFiles/EastSide/EastSide_2012.out")
#summary(data)
#dim(data$catchByGroup)
#n.ages <- data$nagecomp

y <- 1
for(y in 1:n.years) {
  year <- years[y]
  #print(year)
  #Read in data
  temp.data <- readList(paste("/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah/outputFiles/EastSide/EastSide_",year,".out", sep=''))
  
  d <- 1
  for(d in 1:n.districts) {
    counter <- 1
    s <- 1
    for(s in 1:n.stocks) {
      catch.arr[d,s,y] <- sum(temp.data$catchByGroup[d,c(counter:(s*n.ages))])
      counter <- counter+n.ages
    }#next s
  }#next d
  
  counter <- 1
  s <- 1
  for(s in 1:n.stocks) {
    runSize[s,y] <- sum(temp.data$RunSize[c(counter:(s*n.ages))])
    counter <- counter+n.ages
  }#next s
}#next y


y <- 1
for(y in 1:n.years) {
  d <- 1
  for(d in 1:n.districts) {
    hr.arr[d,,y] <- catch.arr[d,,y]/runSize[,y]
  }#next d
}#next y

#Calculate averages
avg.hr <- apply(hr.arr, c(1,2), mean)

x.lim <- c(min(hr.arr), max(hr.arr))
par(mfrow=c(n.districts,n.stocks), oma=c(4,4,3,0), mar=c(3,2,0,0))
d <- 1
for(d in 1:n.districts) {
  s <- 1
  for(s in 1:n.stocks) {
    hist(hr.arr[d,s,], col='gray', xlab='', yaxt='n', main='')#, xlim=x.lim)
    if(s==1) { mtext(districts[d], side=2, line=1) }
    if(d==n.districts) { mtext(stocks[s], side=1, line=3 )}
  }#next s
  
}#next d
mtext('District', side=2, outer=TRUE, font=2, line=2)
mtext('Stock', side=1, outer=TRUE, font=2, line=2)

avg.hr <- apply(hr.arr, c(1,2), mean)

par(mfrow=c(n.districts,n.stocks), oma=c(4,4,3,0), mar=c(3,2,0,0))
d <- 1
for(d in 1:n.districts) {
  s <- 1
  for(s in 1:n.stocks) {
    temp.dens <- density(hr.arr[d,s,])
    x.lim <- c(0,1)#c(min(temp.dens$x), max(temp.dens$x))
    y.lim <- c(min(temp.dens$y), max(temp.dens$y))
    plot(x=NULL, y=NULL, yaxt='n', ylab='', main='', xlim=x.lim, ylim=y.lim)
    polygon(x=temp.dens$x, y=temp.dens$y, col='red')
    abline(v=median(hr.arr[d,s,]))
    if(s==1) { mtext(districts[d], side=2, line=1) }
    if(d==n.districts) { mtext(stocks[s], side=1, line=3 )}
  }#next s
  
}#next d
mtext('District', side=2, outer=TRUE, font=2, line=2)
mtext('Stock', side=1, outer=TRUE, font=2, line=2)






