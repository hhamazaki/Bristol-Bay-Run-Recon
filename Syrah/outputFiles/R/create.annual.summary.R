######### create.annual.summary ########
# INPUTS: 1) year
#
################################

create.annual.summary <- function(year, side, wd=wd) {
  setwd(paste(wd, "/Syrah/outputFiles", sep=""))
  ### TESTING ###
  # year <- 2014
  # side <- 'west'
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
  
  ac.data <- na.omit(read.csv(paste(wd, "/R/ageComp.annual.csv", sep=""), header=TRUE, stringsAsFactors=FALSE)[,-1])
  
  #Read the data
  if(side == 'west') {
    temp.data <- readList(paste('WestSide/WestSide_',year,'.out',sep='')) 
  }else {
    temp.data <- readList(paste('EastSide/EastSide_',year,'.out',sep=''))  
  }
  
  #Pertinent variables
  n.agecomps <- temp.data$nagecomps
  agecomps <- temp.data$AgeCompLabels
  
  if(side == 'west') {
    n.source <- 7
    output <- array(data=NA, dim=c(n.source, n.agecomps, n.stocks), dimnames=list(c('Catch','Escapement','Reallocated Esc Obs Error','Igushik Set', 'WRSHA','Offshore Catch', 'Total'), agecomps, names.stocks))	
  }else {
    n.source <- 8
    output <- array(data=NA, dim=c(n.source, n.agecomps, n.stocks), dimnames=list(c('Catch','Escapement','Reallocated Esc Obs Error', 'Kvichak Set', 'ARSHA','NRSHA','Offshore Catch', 'Total'), agecomps, names.stocks))	  
  }
  
  ##### FILL IN THE ARRAY #####
  #CATCH
  dim <- 1
  temp.catch <- temp.data$catchByGroup
  
  s <- 1
  for(s in 1:n.stocks) {
    ref <- ((s-1)*n.agecomps+1):(s*n.agecomps) 
    if(side == 'west') {
      output[dim, , s] <- temp.catch[ref]
    }else {
      output[dim, , s] <- apply(temp.catch[,ref], 2, sum, na.rm=TRUE) #Necessary as catchByGroup is a matrix of districtXgroup for the east side (INTERCEPTION)
    }
  }#next s
  
  #ESCAPEMENT
  dim <- 2
  temp.esc <- temp.data$escByGroup
  s <- 1
  for(s in 1:n.stocks) {
    ref <- ((s-1)*n.agecomps+1):(s*n.agecomps) 
    output[dim,,s] <- temp.esc[ref]
  }#next s
  
  #ESCAPEMENT OBS ERROR REALLOCATION
  dim <- 3
  s <- 1
  for(s in 1:n.stocks) {    
    esc.obs.err <- (temp.data$obsEsc[s] - temp.data$predEsc[s]) * temp.data$predAgeCompEsc[s,]
    output[dim,,s] <- esc.obs.err
  }#next s  
  
  #SUBDISTRICT CATCHES
  
  #Load subdistrict catches
  add.ac <- read.csv('Reallocation/ageCompAdd.csv', stringsAsFactors=FALSE)
  add.ac <- add.ac[add.ac$year==year,]
  add.catch <- read.csv('Reallocation/catchAdd.csv', stringsAsFactors=FALSE)
  add.catch <- add.catch[add.catch$year==year,]
  
  if(side == 'west') {
    #Igushik Set
    dim <- 4
    temp.subdistrict <- 'Igushik Set'
    temp.add.catch <- add.catch[add.catch$subdistrict==temp.subdistrict,]
    n.catches <- nrow(temp.add.catch)
    i <- 1
    for(i in 1:n.catches) {
      # s <- which(names.stocks==temp.add.catch$district[i])
      s <- which(esc.dist==temp.add.catch$districtID[i] & esc.stream==temp.add.catch$stream[i]) 
      temp.stream <- esc.stream[s]
      temp.dist <- esc.dist[s]
      sub.catch <- temp.add.catch$number[i]
      temp.n.agecomps <- nrow(add.ac[add.ac$year==year & add.ac$subdistrict==temp.subdistrict,])
      if(temp.n.agecomps > 0) {  #Agecomp data are available
        ac <- 1
        for(ac in 1:temp.n.agecomps) {
          temp.fa <- add.ac$fage[add.ac$year==year & add.ac$subdistrict==temp.subdistrict][ac]
          temp.oa <- add.ac$oage[add.ac$year==year & add.ac$subdistrict==temp.subdistrict][ac]	
          temp.loc <- which(agecomps == paste(temp.fa,".",temp.oa, sep=''))  	
          #Add subdistrict catch to output array
          output[dim,temp.loc,s] <- sub.catch*add.ac$prop[add.ac$year==year & add.ac$subdistrict==temp.subdistrict][ac]
        }#next ac
      }else {  #Agecomp data are NOT available
        #Find escapement age comp
        if(nrow(ac.data[ac.data$district==temp.dist & ac.data$stream==temp.stream & ac.data$year==year & ac.data$catch.esc=='esc',]) > 0) {
          temp.esc.ac <- as.numeric(ac.data[ac.data$district==temp.dist & ac.data$stream==temp.stream & ac.data$year==year & ac.data$catch.esc=='esc', c(8:ncol(ac.data))])/
            ac.data$n.fish[ac.data$district==temp.dist & ac.data$stream==temp.stream & ac.data$year==year & ac.data$catch.esc=='esc']
        } else {  #Predicted age comp if OBSERVED age comp NOT available
          temp.esc.ac <- temp.data$predAgeCompEsc[s,]
        }	  
        #Add subdistrict catch to output array
        output[dim,,s] <- sub.catch*temp.esc.ac
      }
    }#next i
    
    #WRSHA
    dim <- 5
    temp.subdistrict <- 'WRSHA'
    temp.add.catch <- add.catch[add.catch$subdistrict==temp.subdistrict,]
    n.catches <- nrow(temp.add.catch)
    i <- 1
    for(i in 1:n.catches) {
      # s <- which(names.stocks==temp.add.catch$district[i])
      s <- which(esc.dist==temp.add.catch$districtID[i] & esc.stream==temp.add.catch$stream[i]) 
      temp.stream <- esc.stream[s]
      temp.dist <- esc.dist[s]
      sub.catch <- temp.add.catch$number[i]
      temp.n.agecomps <- nrow(add.ac[add.ac$year==year & add.ac$subdistrict==temp.subdistrict,])
      if(temp.n.agecomps > 0) {  #Agecomp data are available
        ac <- 1
        for(ac in 1:temp.n.agecomps) {
          temp.fa <- add.ac$fage[add.ac$year==year & add.ac$subdistrict==temp.subdistrict][ac]
          temp.oa <- add.ac$oage[add.ac$year==year & add.ac$subdistrict==temp.subdistrict][ac]	
          temp.loc <- which(agecomps == paste(temp.fa,".",temp.oa, sep=''))  	
          #Add subdistrict catch to output array
          output[dim,temp.loc,s] <- sub.catch*add.ac$prop[add.ac$year==year & add.ac$subdistrict==temp.subdistrict][ac]
        }#next ac
      }else {  #Agecomp data are NOT available
        #Find escapement age comp
        if(nrow(ac.data[ac.data$district==temp.dist & ac.data$stream==temp.stream & ac.data$year==year & ac.data$catch.esc=='esc',]) > 0) {
          temp.esc.ac <- as.numeric(ac.data[ac.data$district==temp.dist & ac.data$stream==temp.stream & ac.data$year==year & ac.data$catch.esc=='esc', c(8:ncol(ac.data))])/
            ac.data$n.fish[ac.data$district==temp.dist & ac.data$stream==temp.stream & ac.data$year==year & ac.data$catch.esc=='esc']
        } else {  #Predicted age comp if OBSERVED age comp NOT available
          temp.esc.ac <- temp.data$predAgeCompEsc[s,]
        }	  
        #Add subdistrict catch to output array
        output[dim,,s] <- sub.catch*temp.esc.ac
      }
    }#next i
    
  }else {
    #Kvichak Set
    dim <- 4
    temp.subdistrict <- 'Kvi Set'
    temp.add.catch <- add.catch[add.catch$subdistrict==temp.subdistrict,]
    n.catches <- nrow(temp.add.catch)
    i <- 1
    for(i in 1:n.catches) {
      # s <- which(names.stocks==temp.add.catch$district[i])
      s <- which(esc.dist==temp.add.catch$districtID[i] & esc.stream==temp.add.catch$stream[i]) 
      temp.stream <- esc.stream[s]
      temp.dist <- esc.dist[s]
      sub.catch <- temp.add.catch$number[i]
      temp.n.agecomps <- nrow(add.ac[add.ac$year==year & add.ac$subdistrict==temp.subdistrict,])
      if(temp.n.agecomps > 0) {  #Agecomp data are available
        ac <- 1
        for(ac in 1:temp.n.agecomps) {
          temp.fa <- add.ac$fage[add.ac$year==year & add.ac$subdistrict==temp.subdistrict][ac]
          temp.oa <- add.ac$oage[add.ac$year==year & add.ac$subdistrict==temp.subdistrict][ac]	
          temp.loc <- which(agecomps == paste(temp.fa,".",temp.oa, sep=''))  	
          #Add subdistrict catch to output array
          output[dim,temp.loc,s] <- sub.catch*add.ac$prop[add.ac$year==year & add.ac$subdistrict==temp.subdistrict][ac]
        }#next ac
      }else {  #Agecomp data are NOT available
        #Find escapement age comp
        if(nrow(ac.data[ac.data$district==temp.dist & ac.data$stream==temp.stream & ac.data$year==year & ac.data$catch.esc=='esc',]) > 0) {
          temp.esc.ac <- as.numeric(ac.data[ac.data$district==temp.dist & ac.data$stream==temp.stream & ac.data$year==year & ac.data$catch.esc=='esc', c(8:ncol(ac.data))])/
            ac.data$n.fish[ac.data$district==temp.dist & ac.data$stream==temp.stream & ac.data$year==year & ac.data$catch.esc=='esc']
        } else {  #Predicted age comp if OBSERVED age comp NOT available
          temp.esc.ac <- temp.data$predAgeCompEsc[s,]
        }	  
        #Add subdistrict catch to output array
        output[dim,,s] <- sub.catch*temp.esc.ac
      }
    }#next i
    
    #ARSHA
    dim <- 5
    temp.subdistrict <- 'ARSHA'
    temp.add.catch <- add.catch[add.catch$subdistrict==temp.subdistrict,]
    n.catches <- nrow(temp.add.catch)
    i <- 1
    for(i in 1:n.catches) {
      # s <- which(names.stocks==temp.add.catch$district[i])
      s <- which(esc.dist==temp.add.catch$districtID[i] & esc.stream==temp.add.catch$stream[i]) 
      temp.stream <- esc.stream[s]
      temp.dist <- esc.dist[s]
      sub.catch <- temp.add.catch$number[i]
      temp.n.agecomps <- nrow(add.ac[add.ac$year==year & add.ac$subdistrict==temp.subdistrict,])
      if(temp.n.agecomps > 0) {  #Agecomp data are available
        ac <- 1
        for(ac in 1:temp.n.agecomps) {
          temp.fa <- add.ac$fage[add.ac$year==year & add.ac$subdistrict==temp.subdistrict][ac]
          temp.oa <- add.ac$oage[add.ac$year==year & add.ac$subdistrict==temp.subdistrict][ac]	
          temp.loc <- which(agecomps == paste(temp.fa,".",temp.oa, sep=''))  	
          #Add subdistrict catch to output array
          output[dim,temp.loc,s] <- sub.catch*add.ac$prop[add.ac$year==year & add.ac$subdistrict==temp.subdistrict][ac]
        }#next ac
      }else {  #Agecomp data are NOT available
        #Find escapement age comp
        if(nrow(ac.data[ac.data$district==temp.dist & ac.data$stream==temp.stream & ac.data$year==year & ac.data$catch.esc=='esc',]) > 0) {
          temp.esc.ac <- as.numeric(ac.data[ac.data$district==temp.dist & ac.data$stream==temp.stream & ac.data$year==year & ac.data$catch.esc=='esc', c(8:ncol(ac.data))])/
            ac.data$n.fish[ac.data$district==temp.dist & ac.data$stream==temp.stream & ac.data$year==year & ac.data$catch.esc=='esc']
        } else {  #Predicted age comp if OBSERVED age comp NOT available
          temp.esc.ac <- temp.data$predAgeCompEsc[s,]
        }	  
        #Add subdistrict catch to output array
        output[dim,,s] <- sub.catch*temp.esc.ac
      }
    }#next i
    
    #NRSHA
    dim <- 6
    temp.subdistrict <- 'NRSHA'
    temp.add.catch <- add.catch[add.catch$subdistrict==temp.subdistrict,]
    n.catches <- nrow(temp.add.catch)
    i <- 1
    for(i in 1:n.catches) {
      # s <- which(names.stocks==temp.add.catch$district[i])
      s <- which(esc.dist==temp.add.catch$districtID[i] & esc.stream==temp.add.catch$stream[i]) 
      temp.stream <- esc.stream[s]
      temp.dist <- esc.dist[s]
      sub.catch <- temp.add.catch$number[i]
      temp.n.agecomps <- nrow(add.ac[add.ac$year==year & add.ac$subdistrict==temp.subdistrict,])
      if(temp.n.agecomps > 0) {  #Agecomp data are available
        ac <- 1
        for(ac in 1:temp.n.agecomps) {
          temp.fa <- add.ac$fage[add.ac$year==year & add.ac$subdistrict==temp.subdistrict][ac]
          temp.oa <- add.ac$oage[add.ac$year==year & add.ac$subdistrict==temp.subdistrict][ac]	
          temp.loc <- which(agecomps == paste(temp.fa,".",temp.oa, sep=''))  	
          #Add subdistrict catch to output array
          output[dim,temp.loc,s] <- sub.catch*add.ac$prop[add.ac$year==year & add.ac$subdistrict==temp.subdistrict][ac]
        }#next ac
      }else {  #Agecomp data are NOT available
        #Find escapement age comp
        if(nrow(ac.data[ac.data$district==temp.dist & ac.data$stream==temp.stream & ac.data$year==year & ac.data$catch.esc=='esc',]) > 0) {
          temp.esc.ac <- as.numeric(ac.data[ac.data$district==temp.dist & ac.data$stream==temp.stream & ac.data$year==year & ac.data$catch.esc=='esc', c(8:ncol(ac.data))])/
            ac.data$n.fish[ac.data$district==temp.dist & ac.data$stream==temp.stream & ac.data$year==year & ac.data$catch.esc=='esc']
        } else {  #Predicted age comp if OBSERVED age comp NOT available
          temp.esc.ac <- temp.data$predAgeCompEsc[s,]
        }	  
        #Add subdistrict catch to output array
        output[dim,,s] <- sub.catch*temp.esc.ac
      }
    }#next i
  }#end if EASTSIDE
  
  #OFFSHORE CATCH
  if(side == 'west') {
    dim <- 6
    offshore <- allocate.offshoreCatch(years=year, remove.togiak=TRUE, wd=wd)$westOffshore
    
    s <- 1
    for(s in 1:n.stocks) {
      temp.totals <- vector(length=n.agecomps)
      ac <- 1
      for(ac in 1:n.agecomps) {
        temp.totals[ac] <- sum(output[,ac,s], na.rm=TRUE)
      }#next ac
      temp.prop <- temp.totals/sum(temp.totals)
      output[dim,,s] <- offshore[s]*temp.prop
    }#next s
  }else {
    dim <- 7
    offshore <- allocate.offshoreCatch(years=year, remove.togiak=TRUE, wd=wd)$eastOffshore
    
    s <- 1
    for(s in 1:n.stocks) {
      temp.totals <- vector(length=n.agecomps)
      ac <- 1
      for(ac in 1:n.agecomps) {
        temp.totals[ac] <- sum(output[,ac,s], na.rm=TRUE)
      }#next ac
      temp.prop <- temp.totals/sum(temp.totals)
      output[dim,,s] <- offshore[s]*temp.prop
    }#next s
  }
  
  #TOTALS
  if(side == 'west') {
    dim <- 7
  }else {
    dim <- 8	
  }
  s <- 1
  for(s in 1:n.stocks) {
    ac <- 1
    for(ac in 1:n.agecomps) {
      output[dim,ac,s] <- sum(output[(1:dim),ac,s], na.rm=TRUE)
    }#next ac
  }
  
  #Write Output files
  s <- 1
  for(s in 1:n.stocks) {
    write.csv(output[,,s], paste('Annual Summary/', names.stocks[s], ' ', year, ' Total.csv'))	
  }#next s	 
}

# create.annual.summary(year=2018, side='west', wd=wd)