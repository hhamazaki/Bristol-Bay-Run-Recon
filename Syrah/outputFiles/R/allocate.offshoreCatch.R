######### allocate.offshoreCatch ########
# ALLOCATES ANNUAL OFFSHORE CATCHES IN PROPORTION TO RECONSTRUCTED INSHORE RUNSIZE BY STOCK
#
# INPUTS: 1) years - years for which plots will be generated
################################

allocate.offshoreCatch <- function(years, remove.togiak=TRUE, wd=wd) {
  setwd(paste(wd, "/Syrah/outputFiles", sep=""))
  ### TESTING ###
  
  #remove.togiak <- TRUE
  #years <- 2012
  ###############
  
  n.agecomps <- 18
  n.stocks.west <- 3
  n.stocks.east <- 5
  n.years <- length(years)
  #Data objects
  westOffshore <- matrix(nrow=n.years, ncol=n.stocks.west, dimnames=list(years, c('Igushik', 'Wood','Nushagak')))
  eastOffshore <- matrix(nrow=n.years, ncol=n.stocks.east, dimnames=list(years, c('Kvichak','Alagnak','Naknek','Egegik','Ugashik')))
  togiakOffshore <- vector(length=n.years)
  offshoreCatch <- read.csv('Reallocation/offshoreCatchAdd.csv', stringsAsFactors=FALSE)
  
  if(remove.togiak == TRUE) {
    togiak <- read.csv('Reallocation/inshoreTogiak.csv', header=TRUE)
  }
  
  y <- 1
  for(y in 1:n.years) {
    year <- years[y]
    
    #Total annual offshore catches (s. penin. + high seas)
    temp.catch <- offshoreCatch$offshoreCatch[which(offshoreCatch$year==year)]
    
    #Inshore returns by stock
    westInshore <- vector(length=n.stocks.west)  #Number
    westInshore.prop <- vector(length=n.stocks.west)  #Proportion
    eastInshore <- vector(length=n.stocks.east)  #Number
    eastInshore.prop <- vector(length=n.stocks.east)  #Proportion
    
    west.data <- readList(paste('WestSide/WestSide_',year,'.out',sep=''))
    east.data <- readList(paste('EastSide/EastSide_',year,'.out',sep=''))
    
    #Total Returns 
    #NOTE: SPECIAL HARVEST AREA CATCHES SHOULD BE INCLUDED AS WELL AS OBSERVATION ERROR REALLOCATION
    #WEST
    counter <- 1
    s <- 1
    for(s in 1:n.stocks.west) {
      westInshore[s] <- sum(west.data$RunSize[counter:(counter+n.agecomps-1)])
      
      
      counter <- counter + n.agecomps
    }#next s
    
    #EAST
    counter <- 1
    s <- 1
    for(s in 1:n.stocks.east) {
      eastInshore[s] <- sum(east.data$RunSize[counter:(counter+n.agecomps-1)])
      counter <- counter + n.agecomps
    }#next s
    
    #This is functioning properly - problem is that togiak is being referenced by y
    
    #Determine total proprotions of Baywide Run
    if(remove.togiak == TRUE) { #Account for togiak inshore when allocating offshore catches
      westInshore.prop <- westInshore/sum(westInshore,eastInshore,togiak$inshore[which(togiak$year==year)])    
      eastInshore.prop <- eastInshore/sum(westInshore,eastInshore,togiak$inshore[which(togiak$year==year)])    	
      togiakInshore.prop <- togiak$inshore[which(togiak$year==year)]/sum(westInshore,eastInshore,togiak$inshore[which(togiak$year==year)]) 
    } else {
      westInshore.prop <- westInshore/sum(westInshore,eastInshore)    
      eastInshore.prop <- eastInshore/sum(westInshore,eastInshore)
    }
    #Partition temp.catch amongst stocks (based on relative returns)
    westOffshore[y,] <- westInshore.prop*temp.catch
    eastOffshore[y,] <- eastInshore.prop*temp.catch
    if(remove.togiak == TRUE) { togiakOffshore[y] <- togiakInshore.prop*temp.catch }else { togiakOffshore[y] <- NA }
  }#next y
  #print(westOffshore)
  #Write out Togiak Offshore Allocation
  write.csv(data.frame(years,togiakOffshore), 'Togiak Offshore Allocation.csv')
  
  output <- NULL
  output$westOffshore <- westOffshore
  output$eastOffshore <- eastOffshore
  output$togiakOffshore <- togiakOffshore
  return(output)
}

#TESTING SECTION
#(west <- allocate.offshoreCatch(years=2012, remove.togiak=TRUE, wd=wd)$westOffshore)
#(east <- allocate.offshoreCatch(years=2012, remove.togiak=TRUE, wd=wd)$eastOffshore)
# (togiak <- allocate.offshoreCatch(years=2012, remove.togiak=TRUE, wd=wd)$togiakOffshore)
# sum(west[nrow(west),],east[nrow(east),],togiak[length(togiak)])
# offshoreCatch[offshoreCatch$year==2012,]