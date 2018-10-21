######### calc.avg.avail.sel ########
# INPUTS: 1) side - "east" or "west"
#         2) years - years for which plots will be generated
#         3) writeCsv - T/F whether .csv will be generated
#
#
################################
calc.avg.avail.sel <- function(side, years, writeCsv=FALSE, wd=wd) {
  setwd(paste(wd, "/Syrah/outputFiles", sep=""))
  n.years <- length(years)
  n.selectpar <- 5
  
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
  
  all.sel <- matrix(data=0, nrow=n.selectpar, ncol=n.years, dimnames=list(1:5,years))
  all.avail <- array(data=0, dim=c(n.districts,n.stocks,n.years), dimnames=list(names.districts, names.stocks, years))
  
  avg.sel <- vector(length=n.selectpar)
  avg.avail <- matrix(data=0, nrow=n.districts, ncol=n.stocks, dimnames=list(names.districts,names.stocks))
  
  #RETREIVE DATA
  y <- 1
  for(y in 1:n.years) {
    year <- years[y]
    if(side == 'west') {
      temp.data <- readList(paste('WestSide/WestSide_',year,'.out',sep='')) 
      
    }else {
      temp.data <- readList(paste('EastSide/EastSide_',year,'.out',sep=''))	
    } 
    
    all.sel[,y] <- temp.data$selectivity
    all.avail[,,y] <- temp.data$availability
    
    avg.sel <- avg.sel + temp.data$selectivity
    avg.avail <- avg.avail + temp.data$availability
  }#next y
  
  avg.sel <- avg.sel/n.years
  avg.avail <- avg.avail/n.years
  
  if(writeCsv == TRUE & side == 'west') { 
    write.csv(avg.sel, 'WestSide Figs/Average Selectivity.csv') 
    write.csv(avg.avail, 'WestSide Figs/Average Availability.csv')
  }
  if(writeCsv == TRUE & side == 'east') { 
    write.csv(avg.sel, 'EastSide Figs/Average Selectivity.csv') 
    write.csv(avg.avail, 'EastSide Figs/Average Availability.csv')
  }  
}