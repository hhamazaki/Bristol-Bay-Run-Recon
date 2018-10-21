######### create.total.run.table ########
# INPUTS: 1) side - "east" or "west"
#         2) years - years for which plots will be generated
#
################################

create.total.run.table <- function(side, years, writeCSV=FALSE, wd=wd) {
  
  ##### TESTING
  # side <- 'west'
  # years <- 1963:2017
  # writeCSV <- FALSE
  # wd <- "/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual"
  #####
  
  require(PBSmodelling)
  # require(xlsx)
  
  setwd(paste(wd, "/Syrah/outputFiles", sep=""))
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
  
  n.years <- length(years)
  
  #Data Array to be printed
  total.ret <- array(dim=c(n.years,10,n.stocks), dimnames=list(years,c('Escapement','District Catches','EscapementObsError','OffshoreCatch',
                                                                       'Extra Catches','Total Esc','Total Catch no Offshore', 'Total Catch',
                                                                       'Run Size', 'Run Size no Offshore'),names.stocks))
  
  #READ IN POST-HOC ALLOCATED CATCHES
  #Set and SHA catches
  add.catch <- read.csv('Reallocation/catchAdd.csv', stringsAsFactors=FALSE)
  
  #Offshore catches
  if(side == 'west') { offshore <- allocate.offshoreCatch(years, remove.togiak=TRUE, wd=wd)$westOffshore }
  if(side == 'east') { offshore <- allocate.offshoreCatch(years, remove.togiak=TRUE, wd=wd)$eastOffshore }
  
  y <- 1
  for(y in 1:n.years) {
    year <- years[y]
    #RETREIVE DATA 
    if(side == 'west') {
      temp.data <- readList(paste('WestSide/WestSide_',year,'.out',sep='')) 
    }else {
      temp.data <- readList(paste('EastSide/EastSide_',year,'.out',sep=''))  
    }
    
    nagecomps <- temp.data$nagecomps
    
    s <- 1
    for(s in 1:n.stocks) {
      total.ret[y,1,s] <- sum(temp.data$escByGroup[((s-1)*nagecomps+1):(s*nagecomps)]) #Esc
      if(side == 'west') {
        total.ret[y,2,s] <- sum(temp.data$catchByGroup[((s-1)*nagecomps+1):(s*nagecomps)]) #Catch
      }else {
        total.ret[y,2,s] <- sum(temp.data$catchByGroup[,((s-1)*nagecomps+1):(s*nagecomps)]) #Catch	
      }
      total.ret[y,3,s] <- temp.data$obsEsc[s] - temp.data$predEsc[s] #EscObsError
      total.ret[y,4,s] <- offshore[y,s] #Offshore Catches
      total.ret[y,5,s] <- sum(add.catch$number[add.catch$year==year & add.catch$districtID==esc.dist[s] & add.catch$stream==esc.stream[s]]) #Extra Catches
      total.ret[y,6,s] <- total.ret[y,1,s] + total.ret[y,3,s] #Total Esc
      total.ret[y,7,s] <- total.ret[y,2,s] + total.ret[y,5,s]  #Total Catch no Offshore
      total.ret[y,8,s] <- total.ret[y,2,s] + total.ret[y,5,s] + total.ret[y,4,s] #Total Catch
      #Totals
      total.ret[y,9,s] <- sum(total.ret[y,c(1:5),s]) #Total Run Size
      total.ret[y,10,s] <- sum(total.ret[y,c(1:3,5),s]) #Total Run Size without Offshore Allocation
    }
  }#next y
  
  if(writeCSV == TRUE) {
    s <- 1
    for(s in 1:n.stocks) {
      if(side == 'west') {  
        write.csv(total.ret[,,s], file=paste('WestSide Figs/Extras/', names.stocks[s], ' Run.csv', sep=''))
      }else {
        write.csv(total.ret[,,s], file=paste('EastSide Figs/Extras/', names.stocks[s], ' Run.csv', sep=''))
      }
    }#next s
    #Output for Daniel
    i <- 1
    for(i in c(1,8,9,10)) {
      if(side == 'west') {
        # write.xlsx(x=total.ret[,i,], file=paste('WestSide Figs/Extras/Daniel Summary_west.xlsx', sep=''), sheetName=dimnames(total.ret)[[2]][i], append=ifelse(i==1,FALSE,TRUE) ) 
      }else {
        # write.xlsx(x=total.ret[,i,], file=paste('EastSide Figs/Extras/Daniel Summary_east.xlsx', sep=''), sheetName=dimnames(total.ret)[[2]][i], append=ifelse(i==1,FALSE,TRUE) ) 
      }
    }#next i
    
  }else {
    print(total.ret)
  } 
  
  ######################### TOTAL RUN COMPARISON ###################
  #n.years <- length(years)
  
  total.esc.obs <- vector(length=n.years)
  total.esc.pred <- vector(length=n.years)
  total.esc.pred.ac <- vector(length=n.years)
  total.catch.obs <- vector(length=n.years)
  total.catch.pred <- vector(length=n.years)
  total.catch.pred.ac <- vector(length=n.years)
  total.obs <- vector(length=n.years)
  total.pred <- vector(length=n.years)
  total.pred.ac <- vector(length=n.years)
  
  
  y <- 1
  for(y in 1:n.years) {
    year <- years[y]
    
    west.data <- readList(paste('WestSide/WestSide_',year,'.out',sep=''))
    east.data <- readList(paste('EastSide/EastSide_',year,'.out',sep=''))
    
    total.esc.obs[y] <- sum(west.data$obsEsc) + sum(east.data$obsEsc)
    total.esc.pred[y] <- sum(west.data$predEsc) + sum(east.data$predEsc)
    total.esc.pred.ac[y] <- sum(west.data$escByGroup) + sum(east.data$escByGroup)
    
    total.catch.obs[y] <- sum(west.data$obsCatch) + sum(east.data$obsCatch)
    total.catch.pred[y] <- sum(west.data$predCatch) + sum(east.data$predCatch)
    total.catch.pred.ac[y] <- sum(west.data$catchByGroup) + sum(east.data$catchByGroup)
    
    total.obs[y] <- sum(west.data$obsEsc, west.data$obsCatch) + sum(east.data$obsEsc, east.data$obsCatch)
    total.pred[y] <- sum(west.data$predEsc, west.data$predCatch) + sum(east.data$predEsc, east.data$predCatch)
    total.pred.ac[y] <- sum(west.data$escByGroup, west.data$catchByGroup) + sum(east.data$escByGroup, east.data$catchByGroup)
    
    
    
  }#next y
  
  out <- cbind(years,total.esc.obs, total.catch.obs, total.obs, total.esc.pred, total.catch.pred, total.pred, total.esc.pred.ac, total.catch.pred.ac, total.pred.ac)
  write.csv(out, 'Trial Totals.csv')
}


# create.total.run.table(side='west', years=c(1963:2017), writeCSV=TRUE, wd="/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual")
# create.total.run.table(side='east', years=c(1963:2017), writeCSV=TRUE, wd="/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual")
