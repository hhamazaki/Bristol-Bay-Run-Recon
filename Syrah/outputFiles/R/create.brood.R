######### create.brood ########
# INPUTS: 1) side - "east" or "west"
#         2) years - years for which plots will be generated
#         3) pdf - T/F whether .pdf file will be generated
#		  4) reallocate - Reallocate Igushik Set and WRSHA
#         5) renorm.obs.err - Renormalize escapements for differences between catch and escapement and 
#
################################
create.brood <- function(side, years, reallocate=TRUE, allocateOffshore=TRUE, renorm.obs.err=TRUE, wd=wd) {
  setwd(paste(wd, "/Syrah/outputFiles", sep=""))
  # TESTING ##
      # side <- 'east'
      # years <- 1963:2019
      # reallocate <- TRUE
      # allocateOffshore <- TRUE
      # renorm.obs.err <- TRUE
  #############
  
  #Escapement data - for first column of brood table
  esc.data <- na.omit(read.csv(paste(wd, "/R/qry_Annual_ESCAPEMENT_updated.csv", sep=""), 
                               header=TRUE, stringsAsFactors=FALSE))
  
  n.years <- length(years)
  n.agecomps <- 18
  agecomp.codes <- c("0.1","0.2","0.3","0.4","0.5",
                     "1.1","1.2","1.3","1.4","1.5",
                     "2.1","2.2","2.3","2.4",
                     "3.1","3.2","3.3","3.4")
  offset <- c(2,3,4,5,6,  #Offset Years for brood table
              3,4,5,6,7,
              4,5,6,7,
              5,6,7,8)
  
  ac.data <- na.omit(read.csv(paste(wd, "/R/ageComp.annual.csv", sep=""), header=TRUE, stringsAsFactors=FALSE)[,-1])
  
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
  
  #Data read in from annual ADMB output files
  return.data <- array(data=NA, dim=c(n.years, n.agecomps, n.stocks))
  diff.catch <- array(dim=c(n.years,n.agecomps,n.districts), dimnames=list(years,agecomp.codes,names.districts))
  diff.esc <- array(dim=c(n.years,n.agecomps,n.stocks), dimnames=list(years,agecomp.codes,names.stocks))
  predicted.annual.esc <- matrix(nrow=n.years, ncol=n.stocks, dimnames=list(years, names.stocks))
  
  y <- 1
  for(y in 1:n.years) {
    year <- years[y]
    if(side == 'west') {
      temp.data <- readList(paste('WestSide/WestSide_',year,'.out',sep='')) 
      
      #OBS ERROR
      d <- 1
      for(d in 1:n.districts) {
        diff.catch[y,,d] <- (temp.data$obsCatch[d] - temp.data$predCatch[d]) * temp.data$predAgeCompCatch
      }#next d
      
      counter <- 1
      s <- 1
      for(s in 1:n.stocks) {
        #OBS ERROR
        diff.esc[y,,s] <- (temp.data$obsEsc[s] - temp.data$predEsc[s]) * temp.data$predAgeCompEsc[s,]
        
        return.data[y,,s] <- temp.data$RunSize[counter:(s*n.agecomps)]
        counter <- counter+n.agecomps
        
        #Predicted escapement for brood table if obs err NOT reallocated
        predicted.annual.esc[y,s] <- temp.data$predEsc[s]
      }
    }else {
      temp.data <- readList(paste('EastSide/EastSide_',year,'.out',sep=''))	
      
      #OBS ERROR
      d <- 1
      for(d in 1:n.districts) {
        diff.catch[y,,d] <- (temp.data$obsCatch[d] - temp.data$predCatch[d]) * temp.data$predAgeCompCatch[d]
      }#next d
      
      counter <- 1
      s <- 1
      for(s in 1:n.stocks) {
        #OBS ERROR
        diff.esc[y,,s] <- (temp.data$obsEsc[s] - temp.data$predEsc[s]) * temp.data$predAgeCompEsc[s,]
        
        return.data[y,,s] <- temp.data$RunSize[counter:(s*n.agecomps)]
        counter <- counter+n.agecomps
        
        #Predicted escapement for brood table if obs err NOT reallocated
        predicted.annual.esc[y,s] <- temp.data$predEsc[s]
      }#next s
    } 
  }#next y
  
  #Create Brood Table
  brood.data <- array(data=NA, dim=c(n.years+max(offset), (n.agecomps+1), n.stocks))
  escapements <- matrix(nrow=n.years, ncol=n.stocks)
  
  s <- 1
  for(s in 1:n.stocks) {
    if(renorm.obs.err == TRUE) {
      escapements[,s] <- esc.data$SumOfTotal[esc.data$DistrictID == esc.dist[s] & esc.data$Stream == esc.stream[s] & 
                                               esc.data$Year >= min(years) & esc.data$Year <= max(years)]
    }else {
      escapements[,s] <- predicted.annual.esc[,s]  #IF obs err. NOT REALLOCATED	
    }
    brood.data[,1,s] <- c((min(years)-max(offset)):max(years))
    start.row <- which(brood.data[,1,s] == min(years))  #Row on which returns begin
    
    ac <- 1
    for(ac in 1:n.agecomps) {
      temp.row <- start.row-offset[ac] #Starting row shifted for brood year
      brood.data[c(temp.row:(temp.row+n.years-1)),(ac+1),s] <- return.data[,ac,s]
    }#next ac
  }#next s
  
  #SAME TO HERE!
  #Reallocate the subdistrict catches
  if(reallocate == TRUE) {
    add.ac <- read.csv('Reallocation/ageCompAdd.csv', stringsAsFactors=FALSE)
    add.catch <- read.csv('Reallocation/catchAdd.csv', stringsAsFactors=FALSE)
    i <- 1
    for(i in 1:nrow(add.catch)) {
      #print(paste('###',i))
      temp.year <- add.catch$year[i]
      if(temp.year>=min(years) & temp.year<=max(years)) {
        #print(temp.year)
        temp.dist <- add.catch$districtID[i]
        #print(temp.dist)
        temp.stream <- add.catch$stream[i]
        #print(temp.stream)
        temp.stock <- which(esc.dist==temp.dist & esc.stream==temp.stream) #Determine to which stock the extra catch should be allocated
        if(length(temp.stock) != 0) { #Crude way of making sure that the catch to be allocated is on the side on which we are creating brood tables
          temp.subdistrict <- add.catch$subdistrict[i]  #This is the origin of the reallocated catch
          temp.catch <- add.catch$number[i]
          #Number of age comps for the current subdistrict
          temp.n.agecomps <- nrow(add.ac[add.ac$year==temp.year & add.ac$subdistrict==temp.subdistrict,])
          
          if(temp.n.agecomps > 0) {  #Age Comp data are available
            if(sum(add.ac$prop[add.ac$year==temp.year & add.ac$subdistrict==temp.subdistrict]) != 1) { print(paste('##### ERROR IN add.ac! year:', temp.year, 'dist:', 
                                                                                                                   temp.dist, 'stream:', temp.stream, 'stock:', temp.stock, '### SUM:', sum(add.ac$prop[add.ac$year==temp.year & add.ac$subdistrict==temp.subdistrict]),
                                                                                                                   sep=' ')) }
            ac <- 1
            for(ac in 1:temp.n.agecomps) {
              #print(paste('ac: ',ac))
              temp.fa <- add.ac$fage[add.ac$year==temp.year & add.ac$subdistrict==temp.subdistrict][ac]
              temp.oa <- add.ac$oage[add.ac$year==temp.year & add.ac$subdistrict==temp.subdistrict][ac]	
              temp.loc <- which(agecomp.codes == paste(temp.fa,".",temp.oa, sep=''))     #Temporary location in age groups
              #UPDATE RETURN DATE
              return.data[which(years==temp.year),temp.loc,temp.stock] <- return.data[which(years==temp.year),temp.loc,temp.stock] + temp.catch*add.ac$prop[add.ac$year==temp.year & add.ac$subdistrict==temp.subdistrict][ac]
              #UPDATE BROOD TABLE
              temp.broodYr <- temp.year-temp.fa-temp.oa - 1
              loc.broodYr <- which(brood.data[,1,temp.stock]==temp.broodYr)
              brood.data[loc.broodYr,temp.loc+1,temp.stock] <- brood.data[loc.broodYr,temp.loc+1,temp.stock] + temp.catch*add.ac$prop[add.ac$year==temp.year & add.ac$subdistrict==temp.subdistrict][ac]
            }#next ac
          }else {  #NO age comp data
            #Find escapement age comp
            if(nrow(ac.data[ac.data$district==temp.dist & ac.data$stream==temp.stream & ac.data$year==temp.year & ac.data$catch.esc=='esc',]) > 0) {
              
              temp.esc.ac <- as.numeric(ac.data[ac.data$district==temp.dist & ac.data$stream==temp.stream & ac.data$year==temp.year & ac.data$catch.esc=='esc', c(8:ncol(ac.data))])/
                ac.data$n.fish[ac.data$district==temp.dist & ac.data$stream==temp.stream & ac.data$year==temp.year & ac.data$catch.esc=='esc']
            } else {
              # temp.esc.ac <- temp.data$predAgeCompEsc[temp.stock,] #ERROR HERE: If no ac data for catch addition and escapement, was using avg from most recent year. 
              # Instead we will use the predicted agecomp from escapement
              
              if(side == 'west') {
                temp.data <- readList(paste('WestSide/WestSide_',temp.year,'.out',sep='')) 
              }else {
                temp.data <- readList(paste('EastSide/EastSide_',temp.year,'.out',sep='')) 
              }
              
              #Currently we will use the predicted escapement age composition
              temp.esc.ac <- temp.data$predAgeCompEsc[temp.stock,]
              
            }
            
            #UPDATE RETURN DATA
            return.data[which(years==temp.year),,temp.stock] <- return.data[which(years==temp.year),,temp.stock] + temp.catch*temp.esc.ac
            #UPDATE BROOD TABLE - ERROR HERE! locl.broodYr not specified
            #Find location of years
            ac <- 1
            for(ac in 1:n.agecomps) {
              temp.loc <- ac + 1  #Temporary location of age comp class column in brood table
              temp.broodYr <- temp.year - offset[ac]
              loc.broodYr <- which(brood.data[,1,temp.stock]==temp.broodYr)
              brood.data[loc.broodYr,temp.loc,temp.stock] <- brood.data[loc.broodYr,temp.loc,temp.stock] + temp.catch*temp.esc.ac[ac]
            }#next ac 
          }#end if age comp
        }#end if temp.stock is in the side for which we are creating brood tables
      }#end if temp.year is in years
    }#next i
  }#end if
  
  #HERE IS THE DIFFERENCE!
  # 2019: Warning here: In Ops.factor(togiak$inshore[which(togiak$year == year)], ‘/’ not meaningful for factors
  if(allocateOffshore == TRUE) {
    if(side == 'west') { offshore <- allocate.offshoreCatch(years, remove.togiak=TRUE, wd=wd)$westOffshore }
    if(side == 'east') { offshore <- allocate.offshoreCatch(years, remove.togiak=TRUE, wd=wd)$eastOffshore }
    
    y <- 1
    for(y in 1:n.years) {
      year <- years[y]
      
      s <- 1
      for(s in 1:n.stocks) {
        temp.prop <- return.data[y,,s]/sum(return.data[y,,s])
        
        ac <- 1
        for(ac in 1:n.agecomps) {
          ##### Number of fish to be allocated
          temp.offshore <- offshore[y,s] * temp.prop[ac] #Year, stock and age specific partitioned offshore catch
          #####
          #Update RETURN table
          return.data[y,ac,s] <- return.data[y,ac,s] + temp.offshore
          
          #Update BROOD table
          brood.year <- year-offset[ac]
          loc.broodYr <- which(brood.data[,1,s] == brood.year)
          brood.data[loc.broodYr,ac+1,s] <- brood.data[loc.broodYr,ac+1,s] + temp.offshore 
        }#next ac
      }#next s
    }#next y 
    #print(offshore)
  }
  
  #REALLOCATE THE OBSERVATION ERROR DIFFERENCES
  if(renorm.obs.err == TRUE) {
    
    #Return Table
    return.data <- return.data + diff.esc
    
    y <- 1
    for(y in 1:n.years) {
      year <- years[y]  
      
      #Brood Table
      s <- 1
      for(s in 1:n.stocks) {
        ac <- 1
        for(ac in 1:n.agecomps) {
          brood.year <- year - offset[ac]
          loc.broodYr <- which(brood.data[,1,s] == brood.year)
          brood.data[loc.broodYr,ac+1,s] <- brood.data[loc.broodYr,ac+1,s] + diff.esc[y,ac,s]
        }
      }#next s
    }#next y 
  }
  
  ###############################
  #WRITE OUTPUT FILES	
  
  pred.baywide.total <- vector(length=n.years)
  y <- 1
  for(y in 1:n.years) {
    pred.baywide.total[y] <- sum(return.data[y,,])
  }
  
  #Calculate annual Recruitment and R/S
  recruits <- matrix(nrow=n.years, ncol=n.stocks, dimnames=list(years,names.stocks))
  rps <- matrix(nrow=n.years, ncol=n.stocks, dimnames=list(years,names.stocks))
  
  s <- 1
  for(s in 1:n.stocks) {
    y <- 1
    for(y in 1:(n.years)) {  #Plus three years where MAJOR age classes have returned
      year <- years[y]
      recruits[y,s] <- sum(brood.data[which(brood.data[,1,s]==year), c(2:(n.agecomps+1)), s])
      rps[y,s] <- recruits[y,s]/escapements[y,s]
    }#next y
    
    
    if(side == 'west') { 
      write.csv(cbind(c('Return Year', years), rbind(temp.data$AgeCompLabels,return.data[,,s])), 
                file=paste('WestSide Figs/', names.stocks[s], ' Return Table.csv', sep=''), row.names=FALSE) 
      write.csv(cbind(rbind(c('Brood Year', temp.data$AgeCompLabels),brood.data[,,s]), c('Escapement', rep(NA,max(offset)), escapements[,s]), 
                      c('Recruits', rep(NA,max(offset)), recruits[,s]), 
                      c('R/S', rep(NA,max(offset)), rps[,s])), 
                file=paste('WestSide Figs/', names.stocks[s], ' Brood Table.csv', sep=''), row.names=FALSE)
      write.csv(cbind(years,pred.baywide.total), file='WestSide Figs/Extras/West Complete Return.csv')
      
      if(s==1) {
        # write.xlsx(x=cbind(c('Return Year', years), rbind(temp.data$AgeCompLabels,return.data[,,s])),
        #            file=paste('WestSide Figs/ALL Return Table.xlsx', sep=''),
        #            sheetName=names.stocks[s], row.names=FALSE, append=FALSE)
        
        # 	write.xlsx(x=cbind(rbind(c('Brood Year', temp.data$AgeCompLabels),brood.data[,,s]), c('Escapement', rep(NA,max(offset)), escapements[,s]), 
        #                                                                                  c('Recruits', rep(NA,max(offset)), recruits[,s]), 
        #                                                                                  c('R/S', rep(NA,max(offset)), rps[,s])),
        # 	           file=paste('WestSide Figs/ALL Brood Table.xlsx', sep=''),
        # 	           sheetName=names.stocks[s], row.names=FALSE, append=FALSE)      	              	           	           
      }else {
        # 	write.xlsx(x=cbind(c('Return Year', years), rbind(temp.data$AgeCompLabels,return.data[,,s])),
        # 	           file=paste('WestSide Figs/ALL Return Table.xlsx', sep=''),
        # 	           sheetName=names.stocks[s], row.names=FALSE, append=TRUE)
        # 	           
        # 	write.xlsx(x=cbind(rbind(c('Brood Year', temp.data$AgeCompLabels),brood.data[,,s]), c('Escapement', rep(NA,max(offset)), escapements[,s]), 
        #                                                                                  c('Recruits', rep(NA,max(offset)), recruits[,s]), 
        #                                                                                  c('R/S', rep(NA,max(offset)), rps[,s])),
        # 	           file=paste('WestSide Figs/ALL Brood Table.xlsx', sep=''),
        # 	           sheetName=names.stocks[s], row.names=FALSE, append=TRUE)      	         
      }
      
    } else {
      write.csv(cbind(c('Return Year', years), rbind(temp.data$AgeCompLabels,return.data[,,s])), 
                file=paste('EastSide Figs/', names.stocks[s], ' Return Table.csv', sep=''), row.names=FALSE) 
      write.csv(cbind(rbind(c('Brood Year', temp.data$AgeCompLabels),brood.data[,,s]), c('Escapement', rep(NA,max(offset)),escapements[,s]),
                      c('Recruits', rep(NA,max(offset)), recruits[,s]), 
                      c('R/S', rep(NA,max(offset)), rps[,s])),       
                file=paste('EastSide Figs/', names.stocks[s], ' Brood Table.csv', sep=''), row.names=FALSE)
      write.csv(cbind(years,pred.baywide.total), file='EastSide Figs/Extras/East Complete Return.csv')
      
      if(s==1) {
        # 	write.xlsx(x=cbind(c('Return Year', years), rbind(temp.data$AgeCompLabels,return.data[,,s])),
        # 	           file=paste('EastSide Figs/ALL Return Table.xlsx', sep=''),
        # 	           sheetName=names.stocks[s], row.names=FALSE, append=FALSE)
        # 	           
        # 	write.xlsx(x=cbind(rbind(c('Brood Year', temp.data$AgeCompLabels),brood.data[,,s]), c('Escapement', rep(NA,max(offset)), escapements[,s]), 
        #                                                                                  c('Recruits', rep(NA,max(offset)), recruits[,s]), 
        #                                                                                  c('R/S', rep(NA,max(offset)), rps[,s])),
        # 	           file=paste('EastSide Figs/ALL Brood Table.xlsx', sep=''),
        # 	           sheetName=names.stocks[s], row.names=FALSE, append=FALSE)      	              	           	           
      }else {
        # 	write.xlsx(x=cbind(c('Return Year', years), rbind(temp.data$AgeCompLabels,return.data[,,s])),
        # 	           file=paste('EastSide Figs/ALL Return Table.xlsx', sep=''),
        # 	           sheetName=names.stocks[s], row.names=FALSE, append=TRUE)
        # 	           
        # 	write.xlsx(x=cbind(rbind(c('Brood Year', temp.data$AgeCompLabels),brood.data[,,s]), c('Escapement', rep(NA,max(offset)), escapements[,s]), 
        #                                                                                  c('Recruits', rep(NA,max(offset)), recruits[,s]), 
        #                                                                                  c('R/S', rep(NA,max(offset)), rps[,s])),
        # 	           file=paste('EastSide Figs/ALL Brood Table.xlsx', sep=''),
        # 	           sheetName=names.stocks[s], row.names=FALSE, append=TRUE)      	         
      }
      
    }
  }#next s
}

# create.brood(side='east', years=1963:2016, reallocate=TRUE, allocateOffshore=TRUE, renorm.obs.err=TRUE, wd=wd)