##### plot.annual.agecomp ####
# INPUTS: 1) side - "east" or "west"
#         2) years - years for which plots will be generated
#         3) pdf - T/F whether .pdf file will be generated
#         4) input.cex - size of crosshair to include
#         5) plot.page - number of plots per page to be printed
#
#
################################

plot.annual.agecomp <- function(side, years, pdf=FALSE, input.cex=2, plot.page=3, wd=wd) {
  setwd(paste(wd, "/Syrah/outputFiles", sep="")) 
  ### TESTING ###
  #side <- 'east'
  #years <- c(2006:2011)
  #years <- 1964
  #pdf <- FALSE
  #input.cex <- 2
  #plot.page <- 4
  ###############
  
  ac.data <- na.omit(read.csv(paste(wd, "/R/ageComp.annual.csv", sep=""), header=TRUE, stringsAsFactors=FALSE)[,-1])
  
  n.agecomps <- 18
  #Limits
  space <- 0.2
  x.limit <- c(0,n.agecomps+(n.agecomps+1)*space)
  
  #Colors 
  catch.pal <- colorRampPalette(c('red', 'white'))
  catch.cols <- catch.pal(n.agecomps+1)[1:n.agecomps]
  esc.pal <- colorRampPalette(c('blue','white'))
  esc.cols <- esc.pal(n.agecomps+1)[1:n.agecomps]
  
  if(side == 'west') {
    n.districts <- 1
    n.stocks <- 3
    names.districts <- c('Nushagak')
    names.stocks <- c('Igushik', 'Wood', 'Nushagak')
    cat.dist <- c(325)
    esc.dist <- c(325,325,325)
    esc.stream <- c(100,300,700)
  }
  if(side == 'east') {
    n.districts <- 3
    n.stocks <- 5
    names.districts <- c('Naknek-Kvichak','Egegik','Ugashik')
    names.stocks <- c('Kvichak','Alagnak','Naknek','Egegik','Ugashik')
    cat.dist <- c(324,322,321)
    esc.dist <- c(324,324,324,322,321)
    esc.stream <- c(100,500,600,100,100)	
  }
  if(side != 'west' & side != 'east') { print('##### ERROR side selection is incorrect'); stop(); }
  
  n.years <- length(years)
  
  #Retreive Data - AGE COMPOSITION
  pred.catch <- array(dim=c(n.districts,n.agecomps,n.years))
  obs.catch <- array(dim=c(n.districts,n.agecomps,n.years))
  pred.esc <- array(dim=c(n.stocks,n.agecomps,n.years))
  obs.esc <- array(dim=c(n.stocks,n.agecomps,n.years))
  
  y <- 1
  for(y in 1:n.years) {
    year <- years[y]
    
    if(side == 'west') {
      temp.data <- readList(paste('WestSide/WestSide_',year,'.out',sep='')) 
    }else {
      temp.data <- readList(paste('EastSide/EastSide_',year,'.out',sep=''))	
    }
    
    ac <- 1
    for(ac in 1:n.agecomps) {
      d <- 1
      for(d in 1:n.districts) {
        pred.catch[d,ac,y] <- if(side == 'west') { temp.data$predAgeCompCatch[ac] }else { temp.data$predAgeCompCatch[d,ac] }
        obs.catch[d,ac,y] <- if(side == 'west') { temp.data$obsAgeCompCatch[ac] }else { temp.data$obsAgeCompCatch[d,ac] }	
      }#next d
      s <- 1
      for(s in 1:n.stocks) {
        pred.esc[s,ac,y] <- temp.data$predAgeCompEsc[s,ac]
        obs.esc[s,ac,y] <- temp.data$obsAgeCompEsc[s,ac]
      }
    }#next ac
    
    #ONLY FOR NOW AS THE ESCAPEMENT AGE COMP IS STILL UNCOMPILED, OTHERWISE WE DON'T GET A GOOD FIT, ONCE THAT IS FIGURED OUT
    #  THIS SECTION WILL BE REMOVED
    #s <- 1
    #for(s in 1:n.stocks) {
    # pred.esc[s,,y] <- pred.esc[s,,y]/sum(pred.esc[s,,y])
    #obs.esc[s,,y] <- obs.esc[s,,y]/sum(obs.esc[s,,y])
    #}
    
    if(length(which(pred.catch[,,y] > 1))!=0) {print(paste('##### AgeComp ERROR in', year, 'catch', sep=' '))}
    if(length(which(pred.esc[,,y] > 1))!=0) {print(paste('##### AgeComp ERROR in', year, 'escapement', sep=' '))}
    
    if(y == n.years) { ageComps <- temp.data$AgeCompLabels }  #Rereive ac labels	 
  }#next y
  
  #Plot it out
  if(side == 'west') {
    if(pdf == TRUE) { pdf(file='WestSide Figs/WestSide AgeComp.pdf', height=7, width=6) }	
    
    par(mfrow=c(plot.page,1), mar=c(1,3,1,3), oma=c(5,4,4,4))
    #Catch
    y <- 1
    for(y in 1:n.years) {
      if(nrow(ac.data[ac.data$catch.esc=='catch' & ac.data$year == years[y] & ac.data$district==cat.dist[1],]) == 0) { 
        temp.den <- 40 
      } else {temp.den <- NULL}
      ymax <- max(pred.catch[1,,y], obs.catch[1,,y])*1.1
      xpos <- barplot(obs.catch[1,,y], beside=TRUE, xlab='', ylab=year, ylim=c(0,ymax), xlim=x.limit, col=catch.cols, space=space, xaxs='i', density=temp.den)
      points(xpos, pred.catch[1,,y], type='p', pch=21, cex=input.cex, col='black', bg='green')
      mtext(years[y], side=4, line=2)
      if(y %% plot.page == 0 | y == n.years) {
        axis(side=1, labels=ageComps, outer=FALSE, at=xpos, las=2, tick=FALSE)
        mtext(text=paste(names.districts[1],'District Catch', sep=' '), side=3, outer=TRUE, line=2)
        mtext(text='Age Composition Proportions', side=2, outer=TRUE)
        mtext(text='Age Group', side=1, outer=TRUE, line=3)
      }#end if last plot on page or ever
    }#next y
    
    #Escapement
    s <- 1
    for(s in 1:n.stocks) {
      par(mfrow=c(plot.page,1), mar=c(1,3,1,3), oma=c(5,4,4,4))
      y <- 1
      for(y in 1:n.years) {
        if(nrow(ac.data[ac.data$catch.esc=='esc' & ac.data$year == years[y] & ac.data$district==esc.dist[s] & ac.data$stream==esc.stream[s],]) == 0) { 
          temp.den <- 40 
        } else {temp.den <- NULL}
        ymax <- max(pred.esc[s,,y], obs.esc[s,,y])*1.1
        xpos <- barplot(obs.esc[s,,y], beside=TRUE, xlab='', ylab=year, ylim=c(0,ymax), xlim=x.limit, col=esc.cols, space=space, xaxs='i', density=temp.den)
        points(xpos, pred.esc[s,,y], type='p', pch=21, cex=input.cex, col='black', bg='green')
        mtext(years[y], side=4, line=2)	
        if(y %% plot.page == 0) {
          axis(side=1, labels=ageComps, outer=FALSE, at=xpos, las=2, tick=FALSE)
          mtext(text=paste(names.stocks[s],'River', sep=' '), side=3, outer=TRUE, line=2)
          mtext(text='Age Composition Proportions', side=2, outer=TRUE)
          mtext(text='Age Group', side=1, outer=TRUE, line=3)	
        }#end if last plot on page or ever
      }#next y	
    }#next s
  }else {
    if(pdf == TRUE) { pdf(file='EastSide Figs/EastSide AgeComp.pdf', height=7, width=6)	}
    
    
    
    #Catch
    d <- 1
    for(d in 1:n.districts) {
      par(mfrow=c(plot.page,1), mar=c(1,3,1,3), oma=c(5,4,4,4))
      y <- 1
      for(y in 1:n.years) {
        if(nrow(ac.data[ac.data$catch.esc=='catch' & ac.data$year == years[y] & ac.data$district==cat.dist[d],]) == 0) { 
          temp.den <- 40
        } else {temp.den <- temp.den <- NULL}
        ymax <- max(pred.catch[d,,y], obs.catch[d,,y])*1.1
        xpos <- barplot(obs.catch[d,,y], beside=TRUE, xlab='', ylab=year, ylim=c(0,ymax), xlim=x.limit, col=catch.cols, space=space, xaxs='i', density=temp.den)
        points(xpos, pred.catch[d,,y], type='p', pch=21, cex=input.cex, col='black', bg='green')
        mtext(years[y], side=4, line=2)
        if(y %% plot.page | y == n.years) {
          axis(side=1, labels=ageComps, outer=FALSE, at=xpos, las=2, tick=FALSE)
          mtext(text=paste(names.districts[d],'District Catch', sep=' '), side=3, outer=TRUE, line=2)
          mtext(text='Age Composition Proportions', side=2, outer=TRUE)
          mtext(text='Age Group', side=1, outer=TRUE, line=3)
        }#end if end of page
      }#next y
    }#next d
    #Escapement
    s <- 1
    for(s in 1:n.stocks) {
      par(mfrow=c(plot.page,1), mar=c(1,3,1,3), oma=c(5,4,4,4))
      y <- 1
      for(y in 1:n.years) {
        if(nrow(ac.data[ac.data$catch.esc=='esc' & ac.data$year == years[y] & ac.data$district==esc.dist[s] & ac.data$stream==esc.stream[s],]) == 0) { 
          temp.den <- 40 
        } else {temp.den <- NULL}
        ymax <- max(pred.esc[s,,y], obs.esc[s,,y])*1.1
        xpos <- barplot(obs.esc[s,,y], beside=TRUE, xlab='', ylab=year, ylim=c(0,ymax), xlim=x.limit, col=esc.cols, space=space, xaxs='i', density=temp.den)
        points(xpos, pred.esc[s,,y], type='p', pch=21, cex=input.cex, col='black', bg='green')
        mtext(years[y], side=4, line=2)	
        if(y %% plot.page | y == n.years) {
          axis(side=1, labels=ageComps, outer=FALSE, at=xpos, las=2, tick=FALSE)
          mtext(text=paste(names.stocks[s],'River', sep=' '), side=3, outer=TRUE, line=2)
          mtext(text='Age Composition Proportions', side=2, outer=TRUE)
          mtext(text='Age Group', side=1, outer=TRUE, line=3)	
        }#end if end of page
      }#next y	
    }#next s
  }
  if(pdf == TRUE) { dev.off() }	
}

#plot.annual.agecomp(side='east', years=c(2006:2011), pdf=TRUE, input.cex=2, wd=wd)