#**************************************************************************
#Project Name: SYRAH ANNUAL - Evaluate output files
#Creator: Curry James Cunningham, SAFS, University of Washington
#Date: 3.14.12
#
#Purpose: Read output files for ADMB reconstruction and create descriptive plots
#**************************************************************************
require(PBSmodelling)
require(xlsx)
require(reshape2)
require(ggplot2)
require(beanplot)
require(mcmcplots)
require(RColorBrewer)

#setwd("/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah/outputFiles")

colorRampAlpha <- function(..., n, alpha) {
   colors <- colorRampPalette(...)(n)
   paste(colors, sprintf("%x", ceiling(255*alpha)), sep="")
}

##### plot.annual.catch.esc ####
# INPUTS: 1) side - "east" or "west"
#         2) years - years for which plots will be generated
#         3) pdf - T/F whether .pdf file will be generated
#         4) text.cex - text size
#         5) text.cex - size for the crosshairs
#
################################
plot.annual.catch.esc <- function(side, years, pdf, text.cex, cross.cex, wd=wd) { ###CONTINUE HERE
  setwd(paste(wd, "/Syrah/outputFiles", sep=""))
  # # # ### TESTING ###
    # side <- 'east'
    # years <- c(2006:2011)
    # pdf <- FALSE
    # text.cex <- 0.6
  # # # ###############

  if(side == 'west') {
  	n.districts <- 1
  	n.stocks <- 3
  	names.districts <- 'Nushagak'
  	names.stocks <- c('Igushik', 'Wood', 'Nushagak')
  }
  if(side == 'east') {
    n.districts <- 3
    n.stocks <- 5
    names.districts <- c('Naknek-Kvichak','Egegik','Ugashik')
    names.stocks <- c('Kvichak','Alagnak','Naknek','Egegik','Ugashik')	
  }
  if(side != 'west' & side != 'east') { print('##### ERROR side selection is incorrect'); stop(); }
  
  n.years <- length(years)
  
  catch.pal <- colorRampPalette(c('red', 'grey'))
  catch.cols <- catch.pal(n.years+1)[1:n.years]
  esc.pal <- colorRampPalette(c('blue','grey'))
  esc.cols <- esc.pal(n.years+1)[1:n.years]
  
  #Retreive Data
  list.catch.pred <- matrix(nrow=n.districts, ncol=n.years)#, dimnames=list(names.districts,years))
  list.catch.obs <- matrix(nrow=n.districts, ncol=n.years)#, dimnames=list(names.districts,years))
  list.esc.pred <- matrix(nrow=n.stocks, ncol=n.years)#, dimnames=list(names.stocks,years))
  list.esc.obs <- matrix(nrow=n.stocks, ncol=n.years)#, dimnames=list(names.stocks,years))
  
  y <- 1
  for(y in 1:n.years) {
  	year <- years[y]
  	print(year)
  	
    if(side == 'west') {
      temp.data <- readList(paste('WestSide/WestSide_',year,'.out',sep='')) 
  	}else {
  	  temp.data <- readList(paste('EastSide/EastSide_',year,'.out',sep=''))	
  	}
  	
  	d <- 1
  	for(d in 1:n.districts) {
  	  list.catch.pred[d,y] <- temp.data$predCatch[d]/1e6
  	  list.catch.obs[d,y] <- temp.data$obsCatch[d]/1e6
  	}#next d
  	s <- 1
  	for(s in 1:n.stocks) {
  	  list.esc.pred[s,y] <- temp.data$predEsc[s]/1e6
  	  list.esc.obs[s,y] <- temp.data$obsEsc[s]/1e6
  	}#next s
  }#next y
  
  if(side == 'west') {
    write.csv(t(list.catch.pred), file='WestSide Figs/Extras/West Pred Catch.csv')
    write.csv(t(list.catch.obs), file='WestSide Figs/Extras/West Obs Catch.csv')
    write.csv(t(list.esc.pred), file='WestSide Figs/Extras/West Pred Esc.csv')
    write.csv(t(list.esc.obs), file='WestSide Figs/Extras/West Obs Esc.csv')    
  }else {
    write.csv(t(list.catch.pred), file='EastSide Figs/Extras/East Pred Catch.csv')
    write.csv(t(list.catch.obs), file='EastSide Figs/Extras/East Obs Catch.csv')
    write.csv(t(list.esc.pred), file='EastSide Figs/Extras/East Pred Esc.csv')
    write.csv(t(list.esc.obs), file='EastSide Figs/Extras/East Obs Esc.csv')
  }
  
  #Limits
  space <- 0.2
  x.limit <- c(0,n.years+(n.years+1)*space)
  
  if(side == 'west') {
  	if(pdf == TRUE) { 
  	  pdf(file='WestSide Figs/WestSide Catch and Esc.pdf', height=7, width=6)
  	} 	
  	  par(mfrow=c(4,1), mar=c(1,4,0,0), oma=c(5,2,5,2))
  	  ymax <- max(list.catch.pred[1,], list.catch.obs[1,])*1.1
  	  
  	  #Catch
  	  xpos <- barplot(list.catch.obs[1,], beside=TRUE, axes=FALSE, ylim=c(0,ymax), xlim=x.limit, col=catch.cols, space=space, xaxs='i')
  	  axis(side=2, las=2)
  	  axis(side=1, at=xpos, labels=FALSE)
  	  mtext('Nushagak Catch', side=2, line=5, cex=text.cex)
  	  points(xpos, list.catch.pred[1,], type='p', pch=21, cex=cross.cex, col='black', bg='green')
  	  #abline(h=0.0)
  	  #Escapement
  	  s <- 1
  	  for(s in 1:n.stocks) {
  	    ymax <- max(list.esc.pred[s,], list.esc.obs[s,])*1.1
  	    xpos <- barplot(list.esc.obs[s,], beside=TRUE, axes=FALSE, ylim=c(0,ymax), xlim=x.limit, col=esc.cols, space=space, xaxs='i')
  	    axis(side=2, las=2)
  	    axis(side=1, at=xpos, labels=FALSE)
  	    mtext(paste(names.stocks[s],'Escapement',sep=' '), side=2, line=5, cex=text.cex)
  	    points(xpos, list.esc.pred[s,], type='p', pch=21, cex=cross.cex, col='black', bg='green')
  	    #abline(h=0)
  	    if(s == n.stocks) {
  	      axis(side=1, labels=years, at=xpos, las=2, tick=FALSE)
  	      mtext('Year', side=1, line=4, outer=TRUE)
  	      mtext('Catch and Escapement', side=3, outer=TRUE)
  	    }
  	  }

  } else {
    if(pdf == TRUE) {
      pdf(file='EastSide Figs/EastSide Catch and Esc.pdf', height=7, width=6)
  	}
      par(mfrow=c(4,1), mar=c(1,4,0,0), oma=c(5,2,5,2))
  	  ymax <- max(list.catch.pred[1,], list.catch.obs[1,])*1.1
  	  
  	  #Catch
  	  xpos <- barplot(list.catch.obs[1,], beside=TRUE, axes=FALSE, ylim=c(0,ymax), xlim=x.limit, col=catch.cols, space=space, xaxs='i')
  	  axis(side=2, las=2)
  	  axis(side=1, at=xpos, labels=FALSE)
  	  mtext('Naknek-Kvichak District Catch', side=2, line=5, cex=text.cex)
  	  points(xpos, list.catch.pred[1,], type='p', pch=21, cex=cross.cex, col='black', bg='green')
  	  #abline(h=0)
  	  #Escapement
  	  s <- 1
  	  for(s in 1:3) {
  	    ymax <- max(list.esc.pred[s,], list.esc.obs[s,])*1.1
  	    xpos <- barplot(list.esc.obs[s,], beside=TRUE, axes=FALSE, ylim=c(0,ymax), xlim=x.limit, col=esc.cols, space=space, xaxs='i')
  	    axis(side=2, las=2)
  	    axis(side=1, at=xpos, labels=FALSE)
  	    mtext(paste(names.stocks[s],'Escapement',sep=' '), side=2, line=5, cex=text.cex)
  	    points(xpos, list.esc.pred[s,], type='p', pch=21, cex=cross.cex, col='black', bg='green')
  	    #abline(h=0)
  	    if(s == 3) {
  	      axis(side=1, labels=years, at=xpos, las=2, tick=FALSE)
  	      mtext('Year', side=1, line=4, outer=TRUE)
  	      mtext('Catch and Escapement', side=3, outer=TRUE)
  	    }
  	  }
  	  
  	  par(mfrow=c(4,1), mar=c(1,4,0,0), oma=c(5,2,5,2))
  	 
  	  #Catch - EGEGIK
  	  ymax <- max(list.catch.pred[2,], list.catch.obs[2,])*1.1
  	  xpos <- barplot(list.catch.obs[2,], beside=TRUE, axes=FALSE, ylim=c(0,ymax), xlim=x.limit, col=catch.cols, space=space, xaxs='i')
  	  axis(side=2, las=2)
  	  axis(side=1, at=xpos, labels=FALSE)
  	  mtext('Egegik District Catch', side=2, line=5, cex=text.cex)
  	  points(xpos, list.catch.pred[2,], type='p', pch=21, cex=cross.cex, col='black', bg='green')
  	  #Escapement - EGEGIK
  	  ymax <- max(list.esc.pred[4,], list.esc.obs[4,])*1.1
  	  xpos <- barplot(list.esc.obs[4,], beside=TRUE, axes=FALSE, ylim=c(0,ymax), xlim=x.limit, col=esc.cols, space=space, xaxs='i')
  	  axis(side=2, las=2)
  	  axis(side=1, at=xpos, labels=FALSE)
  	  mtext(paste(names.stocks[4],'Escapement',sep=' '), side=2, line=5, cex=text.cex)
  	  points(xpos, list.esc.pred[4,], type='p', pch=21, cex=cross.cex, col='black', bg='green')
      #axis(side=1, labels=years, outer=TRUE, at=xpos)

  	  #Catch - ESCAPEMENT
  	  ymax <- max(list.catch.pred[3,], list.catch.obs[3,])*1.1
  	  xpos <- barplot(list.catch.obs[3,], beside=TRUE, axes=FALSE, ylim=c(0,ymax), xlim=x.limit, col=catch.cols, space=space, xaxs='i')
  	  axis(side=2, las=2)
  	  axis(side=1, at=xpos, labels=FALSE)
  	  mtext('Ugashik District Catch', side=2, line=5, cex=text.cex)
  	  points(xpos, list.catch.pred[3,], type='p', pch=21, cex=cross.cex, col='black', bg='green')
  	  
  	  #Escapement - EGEGIK
  	  ymax <- max(list.esc.pred[5,], list.esc.obs[5,])*1.1
  	  xpos <- barplot(list.esc.obs[5,], beside=TRUE, axes=FALSE, ylim=c(0,ymax), xlim=x.limit, col=esc.cols, space=space, xaxs='i')
      axis(side=2, las=2)
      axis(side=1, at=xpos, labels=FALSE)
  	  mtext(paste(names.stocks[5],'Escapement',sep=' '), side=2, line=5, cex=text.cex)
	  points(xpos, list.esc.pred[5,], type='p', pch=21, cex=cross.cex, col='black', bg='green')
      
      
      #FINAL LABELS
  	  axis(side=1, labels=years, at=xpos, las=2, tick=FALSE)
  	  mtext('Year', side=1, line=4, outer=TRUE)
  	  mtext('Catch and Escapement', side=3, outer=TRUE)
  }
  if(pdf == TRUE) { dev.off() }
}#end fxn

#plot.annual.catch.esc(side='east', years=c(2006:2011), pdf=TRUE, wd=wd)

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

####### plot.agecomp.coord ######
# INPUTS: 1) side - "east" or "west"
#         2) years - years for which plots will be generated
#         3) all - whether all age classes should be plotted (if FALSE will plot 1.1 1.2 1.3 2.1 2.2 2.3)
#         4) pdf - T/F whether .pdf file will be generated
#         5) sz.pts - vector of sizes of points in plots 1)all 2) critical
#		  6) omit.est - omit fits to estimated age composition data
#
################################

plot.agecomp.coord <- function(side, years, all=TRUE, pdf=FALSE, sz.pts, omit.est=TRUE, wd=wd) {
  setwd(paste(wd, "/Syrah/outputFiles", sep=""))
  ### TESTING ###
  #side <- 'west'
  #years <- 1987
  #pdf <- FALSE
  #sz.pts <- c(1,2)
  #omit.est <- TRUE
  ###############
  
  ac.data <- na.omit(read.csv(paste(wd, "/R/ageComp.annual.csv", sep=""), header=TRUE, stringsAsFactors=FALSE)[,-1])
  n.agecomps <- 18
  #Limits
  #space <- 0.2
  #x.limit <- c(0,n.agecomps+(n.agecomps+1)*space)
  
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
        if(nrow(ac.data[ac.data$catch.esc=='catch' & ac.data$year == years[y] & ac.data$district==cat.dist[d],]) == 0 & omit.est == TRUE) {
          pred.catch[d,ac,y] <- NA
          obs.catch[d,ac,y] <- NA	
        } else {
          pred.catch[d,ac,y] <- if(side == 'west') { temp.data$predAgeCompCatch[ac] }else { temp.data$predAgeCompCatch[d,ac] }
          obs.catch[d,ac,y] <- if(side == 'west') { temp.data$obsAgeCompCatch[ac] }else { temp.data$obsAgeCompCatch[d,ac] }	
        }
      }#next d
      s <- 1
      for(s in 1:n.stocks) {
      	if(nrow(ac.data[ac.data$catch.esc=='esc' & ac.data$year == years[y] & ac.data$district==esc.dist[s] & ac.data$stream==esc.stream[s],]) == 0 & omit.est == TRUE) {
      	  pred.esc[s,ac,y] <- NA
      	  obs.esc[s,ac,y] <- NA	
      	}else {
        pred.esc[s,ac,y] <- temp.data$predAgeCompEsc[s,ac]
        obs.esc[s,ac,y] <- temp.data$obsAgeCompEsc[s,ac]
        }
      }#next s
    }#next ac
    
    if(length(which(pred.catch[,,y] > 1))!=0) {print(paste('##### AgeComp ERROR in', year, 'catch', sep=' '))}
    if(length(which(pred.esc[,,y] > 1))!=0) {print(paste('##### AgeComp ERROR in', year, 'escapement', sep=' '))}
  	
    if(y == n.years) { ageComps <- temp.data$AgeCompLabels }  #Rereive ac labels	 
  }#next y
  
  #Location of critical age comps
  crit.ac <- c(6:8,11:13)
  n.crit.ac <- length(crit.ac)
  
  #Colors 
  catch.pal <- colorRampPalette(c('red', 'yellow'))
  catch.cols.all <- catch.pal(n.agecomps+1)[1:n.agecomps]
  catch.cols.crit <- catch.pal(n.crit.ac+1)[1:n.crit.ac]
  esc.pal <- colorRampPalette(c('blue','yellow'))
  esc.cols.all <- esc.pal(n.agecomps+1)[1:n.agecomps]
  esc.cols.crit  <- esc.pal(n.crit.ac+1)[1:n.crit.ac]
  
  ### PLOT IT OUT ###

  
  if(all == TRUE) {  #Plotting all age classes
    if(side == 'west') {  #WESTSIDE
      if(pdf == TRUE) { pdf(file='WestSide Figs/WestSide AgeComp Coord - ALL.pdf', height=10, width=8)	} 
    } else {  #EASTSIDE
  	  if(pdf == TRUE) { pdf(file='EastSide Figs/EastSide AgeComp Coord - ALL.pdf', height=10, width=8)	}
    }
    
    par(mfrow=c(6,3), mar=c(2,2,2,1), oma=c(4,4,4,4))
  	  
  	#CATCHES
    d <- 1
    for(d in 1:n.districts) {
  	  ac <- 1
  	  for(ac in 1:n.agecomps) {
  	    x.limit <- c(0,1.1*max(obs.catch[d,ac,],pred.catch[d,ac,], na.rm=TRUE))
  	    y.limit <- x.limit
  	    plot(x=obs.catch[d,ac,], y=pred.catch[d,ac,], type='p', pch=21, xlab='', ylab='', main='', xlim=x.limit, ylim=y.limit, col=catch.cols.all[ac], cex=sz.pts[1])
  	    lines(x=c(0,1), y=c(0,1), lty=2)
  	    mtext(ageComps[ac], outer=FALSE, side=3, line=0.25, font=2)
  	  }#next ac
  	  mtext(paste(names.districts[d],'District Catch', sep=' '), side=3, outer=TRUE, line=2, cex=1.5)
  	  mtext('Observed Age Proportions', side=1, outer=TRUE, line=2)
  	  mtext('Predicted Age Proportions', side=2, outer=TRUE, line=2)
    }#next d
      
    #ESCAPEMENTS
    s <- 1
    for(s in 1:n.stocks) {
  	  ac <- 1
  	  for(ac in 1:n.agecomps) {
  	    x.limit <- c(0,1.1*max(obs.esc[s,ac,],pred.esc[s,ac,], na.rm=TRUE))
  	    y.limit <- x.limit
  	    plot(x=obs.esc[s,ac,], y=pred.esc[s,ac,], type='p', pch=21, xlab='', ylab='', main='', xlim=x.limit, ylim=y.limit, col=esc.cols.all[ac], cex=sz.pts[1])
  	    lines(x=c(0,1), y=c(0,1), lty=2)
  	    mtext(ageComps[ac], outer=FALSE, side=3, line=0.25, font=2)
  	  }#next ac
  	  mtext(paste(names.stocks[s],'River Escapement', sep=' '), side=3, outer=TRUE, line=2, cex=1.5)
  	  mtext('Observed Age Proportions', side=1, outer=TRUE, line=2)
  	  mtext('Predicted Age Proportions', side=2, outer=TRUE, line=2)
  	}#next s

  
  } else {  #Only plotting main six age classes: 1.1, 1.2, 1.3, 2.1, 2.2, 2.3
    if(side == 'west') {  #WESTSIDE
      if(pdf == TRUE) { pdf(file='WestSide Figs/WestSide AgeComp Coord - CRIT.pdf', height=7, width=6)	} 
    } else {  #EASTSIDE
  	  if(pdf == TRUE) { pdf(file='EastSide Figs/EastSide AgeComp Coord - CRIT.pdf', height=7, width=6)	}
    }
    
    par(mfrow=c(3,2), mar=c(2,2,2,1), oma=c(4,4,4,4))	
    #CATCHES
  	d <- 1
  	for(d in 1:n.districts) {
  	  for(ac in crit.ac) {
  	    x.limit <- c(0,1.1*max(obs.catch[d,ac,],pred.catch[d,ac,], na.rm=TRUE))
  	    y.limit <- x.limit
  	    plot(x=obs.catch[d,ac,], y=pred.catch[d,ac,], type='p', pch=21, xlab='', ylab='', main='', xlim=x.limit, ylim=y.limit, col=catch.cols.all[ac], cex=sz.pts[2])
  	    lines(x=c(0,1), y=c(0,1), lty=2)
  	    mtext(ageComps[ac], outer=FALSE, side=3, line=0.25, font=2)
  	  }#next ac
  	  mtext(paste(names.districts[d],'District Catch', sep=' '), side=3, outer=TRUE, line=2, cex=1.5)
  	  mtext('Observed Age Proportions', side=1, outer=TRUE, line=2)
  	  mtext('Predicted Age Proportions', side=2, outer=TRUE, line=2)
    }#next d
      
    #ESCAPEMENTS
    s <- 1
  	for(s in 1:n.stocks) {
  	  for(ac in crit.ac) {
  	    x.limit <- c(0,1.1*max(obs.esc[s,ac,],pred.esc[s,ac,], na.rm=TRUE))
  	    y.limit <- x.limit
  	    plot(x=obs.esc[s,ac,], y=pred.esc[s,ac,], type='p', pch=21, xlab='', ylab='', main='', xlim=x.limit, ylim=y.limit, col=esc.cols.all[ac], cex=sz.pts[2])
  	    lines(x=c(0,1), y=c(0,1), lty=2)
  	    mtext(ageComps[ac], outer=FALSE, side=3, line=0.25, font=2)
  	  }#next ac
  	  mtext(paste(names.stocks[s],'River Escapement', sep=' '), side=3, outer=TRUE, line=2, cex=1.5)
  	  mtext('Observed Age Proportions', side=1, outer=TRUE, line=2)
  	  mtext('Predicted Age Proportions', side=2, outer=TRUE, line=2)
  	}#next s
  }
  if(pdf == TRUE) { dev.off() }
}#end fxn



##### plot.annual.genComp ####
# INPUTS: 1) side - "east" or "west"
#         2) years - years for which plots will be generated
#         3) pdf - T/F whether .pdf file will be generated
#         4) text.cex.left - size of text on the LEFT side of plots (LARGER)
#         5) text.cex.right - size fo text on the RIGHT side of plots
#
#
################################

plot.annual.genComp <- function(side, years, pdf=FALSE, text.cex.left, text.cex.right, wd=wd) {
  setwd(paste(wd, "/Syrah/outputFiles", sep=""))
   ### TESTING ###
      # side <- 'west'
      # #years <- c(1980,1997,2005,2006:2011)
      # years <- 2010:2012
      # pdf <- FALSE
      # text.cex.left <- 0.75
      # text.cex.right <- 0.75
   ###############
  n.years <- length(years)
  
  if(side == 'west') {
  	n.districts <- 1
  	n.stocks <- 3
  	names.districts <- 'Nushagak'
  	names.stocks <- c('Igushik', 'Wood', 'Nushagak')
  }
  if(side == 'east') {
    n.districts <- 3
    n.stocks <- 5
    names.districts <- c('Naknek-Kvichak','Egegik','Ugashik')
    names.stocks <- c('Kvichak','Alagnak','Naknek','Egegik','Ugashik')	
  }
  if(side != 'west' & side != 'east') { print('##### ERROR side selection is incorrect'); stop(); }
  
  #Retreive Data - GENETIC COMPOSITION OF CATCH
  predGenComp <- array(dim=c(n.districts, n.stocks, n.years))
  obsGenComp <- array(dim=c(n.districts, n.stocks, n.years))
  genSS <- matrix(nrow=n.districts, ncol=n.years)
  
  y <- 1
  for(y in 1:n.years) {
    year <- years[y]
  	
    if(side == 'west') {
      temp.data <- readList(paste('WestSide/WestSide_',year,'.out',sep='')) 
  	
  	}else {
  	  temp.data <- readList(paste('EastSide/EastSide_',year,'.out',sep=''))	
  	} 
  	
  	d <- 1
  	for(d in 1:n.districts) {
  	  #Genetic sample size
  	  genSS[d,y] <- temp.data$genSS[d] 
  	  s <- 1
  	  for(s in 1:n.stocks) {
  	    if(side == 'west') {
  	      predGenComp[d,s,y] <- temp.data$predGenComp[s]
  	      obsGenComp[d,s,y] <- temp.data$obsGenComp[s] 
  	    } else {
  	      predGenComp[d,s,y] <- temp.data$predGenComp[d,s]
  	      obsGenComp[d,s,y] <- temp.data$obsGenComp[d,s] 
  	    }
  	  }#next s
  	}#next d
  }#next y
  
  #cols <- rainbow(n=n.stocks)
  cols <- topo.colors(n=n.stocks)
  if(side=='east') { cols[2] <- 'red' }
  # cols <- brewer.pal(n=n.socks, name='Set1')  
  
  
  if(pdf == TRUE) { 
    if(side == 'west') {
      pdf(file='WestSide Figs/WestSide Genetic Comp.pdf', height=6, width=8)
    } else {
      pdf(file='EastSide Figs/EastSide Genetic Comp.pdf', height=6, width=8)	
    }
  }
  #PLOT IT OUT
  space <- 0.2
  par(mfrow=c(3,1), mar=c(0,6,1,3), oma=c(5,0,5,0))
  
  d <- 1
  for(d in 1:n.districts) {
    #Years with genetic estimates
    gen.est <- which(genSS[d,] != -1) #Location if years in array with available genetic estimates
    n.yr.est <- length(gen.est) #Number of years with genetic estimates
    if(n.yr.est > 0) {
      #Set xlimit
      xlim <- c(0,n.yr.est+(n.yr.est+1)*space)
      #Sample size
      ylim.ss <- c(0, 1.5*max(genSS[d,gen.est]))
      barplot(genSS[d,gen.est], las=2, xlim=xlim, ylim=ylim.ss, space=space, axes=FALSE, col='grey50', xaxs='i')
      axis(2, at=c(ylim.ss[1], pretty(ylim.ss[2])[1]), las=1)
      abline(h=0)
      mtext('Genetic Sample Size', side=2, line=4, outer=FALSE, cex=text.cex.left)
      mtext('(Number of fish sampled)', side=4, line=1, outer=FALSE, cex=text.cex.right)
      #Legend
      legend('top', legend=names.stocks, fill=cols, ncol=n.stocks, title='Stock', bty='n')
  	
      #Observed Genetic Composition
      barplot(obsGenComp[d,,gen.est], xlim=xlim, ylim=c(0,1), axes=FALSE, space=space, col=cols, beside=FALSE, xaxs='i')
      axis(2, at=c(0,1), labels=TRUE, las=1)
      abline(h=0)
  	  mtext('Observed', side=2, line=4, outer=FALSE, cex=text.cex.left)
      mtext('(Genetic Composition)', side=4, line=1, outer=FALSE, cex=text.cex.right)
      abline(h=c(0.25,0.5,0.75), lty=2, col='black')  	
    
      #Predicted Genetic Composition
      bp <- barplot(predGenComp[d,,gen.est], xlim=xlim, ylim=c(0,1), axes=FALSE, space=space, col=cols, beside=FALSE, xaxs='i')
      axis(2, at=c(0,1), labels=TRUE, las=1)
      axis(1, at=bp, labels=years[gen.est], las=2, tick=FALSE)
      abline(h=0)
      mtext('Predicted', side=2, line=4, outer=FALSE, cex=text.cex.left)
      mtext('(Genetic Composition)', side=4, line=1, outer=FALSE, cex=text.cex.right)
  	  abline(h=c(0.25,0.5,0.75), lty=2, col='black')
  	  #Overall labels
  	  mtext(paste('Genetic Proportions of', names.districts[d], 'District Catch', sep=' '), side=3, line=2, outer=TRUE)
  	  mtext('Year', side=1, line=4, outer=TRUE)
    }
  }#next d
  
  if(pdf == TRUE) { dev.off() }
}#end fxn

####### plot.maxGradient ######
# INPUTS: 1) side - "east" or "west"
#         2) years - years for which plots will be generated
#         3) pdf - T/F whether .pdf file will be generated
#
#
################################

plot.maxGradient <- function(side, years, pdf=FALSE, wd=wd) {
  setwd(paste(wd, "/Syrah/outputFiles", sep=""))
   ## TESTING ###
    # side <- 'east'
    # years <- c(2006:2011)
    # pdf <- FALSE
   ##############
  n.years <- length(years)
  
  max.grad <- vector(length=n.years)
  
  #RETREIVE DATA
  y <- 1
  for(y in 1:n.years) {
    year <- years[y]
    if(side == 'west') {
      temp.data <- readList(paste('WestSide/WestSide_',year,'.out',sep='')) 
  	
  	}else {
  	  temp.data <- readList(paste('EastSide/EastSide_',year,'.out',sep=''))	
  	} 

    max.grad[y] <- temp.data$maxGradient 
  }#next y
  if(pdf == TRUE) {
    if(side == 'west') {
      pdf(file='WestSide Figs/WestSide Max Gradient.pdf', height=6, width=8)
    } else {
      pdf(file='EastSide Figs/EastSide Max Gradient.pdf', height=6, width=8)	
    }
  }
  
  #Limits
  space <- 0.2
  x.limit <- c(0,n.years+(n.years+1)*space)
  #if(min(log10(max.grad)+3) >0) { temp.min <- 1+min(log10(max.grad)+3) } else { temp.min <- 0.9*min(log10(max.grad)+3) }
  #if(max(log10(max.grad)+3) >0) { temp.max <- 1.1*max(log10(max.grad)+3) } else { temp.max <- 0.9*max(log10(max.grad)+3) }
  y.limit <- c(min(log10(max.grad)+3,0)-1, max(log10(max.grad)+3,0)+1)
  
  bar.colors <- rep('red', n.years)
  bar.colors[log10(max.grad)+3 < 0] <- 'black'
  xpos <- barplot(log10(max.grad)+3, las=2, cex.names=0.8, col=bar.colors, ylab="Log10 of Maximum Gradient", 
                  ylim=y.limit, xlab="Year", axes=FALSE, space=space, xlim=x.limit, xaxs='i')
  axis(2)
  axis(side=1, at=xpos, labels=years, las=2)
  if(pdf == TRUE) { dev.off() }
}

####### plot.avail.sel.time ######
# INPUTS: 1) side - "east" or "west"
#         2) years - years for which plots will be generated
#         3) pdf - T/F whether .pdf file will be generated
#		      4) plot.avail - whether to plot availability
#		      5) plot.sel  - whether to plot selectivity over time
#         6) plot.fmort - whether to plot fishing mortality over time
#		      7) plot.hist - whether to plot a histogram of the estimated availability values for each stock in each district
################################

plot.avail.sel.time <- function(side, years, pdf=FALSE, plot.avail=FALSE, plot.sel=FALSE, plot.fmort=FALSE, plot.hist=FALSE, wd=wd) {
  setwd(paste(wd, "/Syrah/outputFiles", sep=""))
  ### TESTING ###
#    side <- 'east'
#    years <- 1963:2014
#    pdf <- FALSE
#    plot.avail <- TRUE
#    plot.sel <- FALSE
#    plot.fmort <- FALSE
  ###
  n.select.par <- 5
  
  n.years <- length(years)
  
  if(side == 'west') {
  	n.districts <- 1
  	n.stocks <- 3
  	names.districts <- 'Nushagak'
  	names.stocks <- c('Igushik', 'Wood', 'Nushagak')
  }
  if(side == 'east') {
    n.districts <- 3
    n.stocks <- 5
    names.districts <- c('Naknek-Kvichak','Egegik','Ugashik')
    names.stocks <- c('Kvichak','Alagnak','Naknek','Egegik','Ugashik')	
  }
  if(side != 'west' & side != 'east') { print('##### ERROR side selection is incorrect'); stop(); }
    
  #Availabiltiy
  avail <- array(dim=c(n.districts, n.stocks, n.years), dimnames=list(names.districts,names.stocks,years))
  avail.est <- matrix(nrow=n.districts, ncol=n.years, dimnames=list(names.districts, years)) #Boolean for whether availability is estimated from genetic data
  #Selectivity
  select <- matrix(nrow=n.select.par, ncol=n.years, dimnames=list(c(1:5), years))
  #Instantaneous fishing mortality rate
  fmort <- matrix(nrow=n.districts, ncol=n.years, dimnames=list(names.districts, years))
  
  y <- 1
  for(y in 1:n.years) {
    year <- years[y]
  	
    if(side == 'west') {
      temp.data <- readList(paste('WestSide/WestSide_',year,'.out',sep='')) 
  	
  	}else {
  	  temp.data <- readList(paste('EastSide/EastSide_',year,'.out',sep=''))	
  	} 
  	
  	d <- 1
  	for(d in 1:n.districts) {
    #Model Predicted availability
  	  s <- 1
  	  for(s in 1:n.stocks) {
  	    if(side == 'west') {
  	      avail[d,s,y] <- temp.data$availability[s]
  	      if(temp.data$GENdata[d] == 1) { avail.est[d,y] <- TRUE }else { avail.est[d,y] <- FALSE } #1=TRUE
  	    } else {
  	      avail[d,s,y] <- temp.data$availability[d,s]
  	      if(temp.data$GENdata[d] == 1) { avail.est[d,y] <- TRUE }else { avail.est[d,y] <- FALSE } #1=TRUE
  	    }
  	  }#next s
  	  fmort[d,y] <- temp.data$Fmort[d]
  	}#next d
  	select[,y] <- temp.data$selectivity
  }#next y
  
  #WRITE OUT SELECTIVITY
  if(side=='west') {
    write.csv(select, file='WestSide Figs/WestSide Selectivity.csv')
  }else {
    write.csv(select, file='WestSide Figs/WestSide Selectivity.csv')
  }
  
  ###########################################################
  if(plot.sel == TRUE) {
  	if(pdf == TRUE) {
      if(side == 'west') {
        pdf(file='WestSide Figs/WestSide Selectivity Over Time.pdf', height=6, width=8)
      } else {
        pdf(file='EastSide Figs/EastSide Selectivity Over Time.pdf', height=6, width=8)	
      }
    }

    #Plotting Selectivity - relative
    rel.select <- matrix(nrow=n.select.par, ncol=n.years)
  
#     y <- 1
#     for(y in 1:n.years) {
#       rel.select[,y] <- select[,y]/sum(select[,y])
#     }#next y
    rel.select <- select
    colnames(rel.select) <- years
  
    space <- 0.2
    x.limit <- c(0,n.years+(n.years+1)*space)
    y.limit <- c(0, 1.1*max(rel.select))
  
    par(mfrow=c(n.select.par,1), oma=c(5,3,4,1), mar=c(0,4,0,4))
  
    #cols <- rainbow(n.select.par)
    cols <- topo.colors(n.select.par)
    
    if(pdf == TRUE) { pdf }
    s <- 1
    for(s in 1:n.select.par) {
      xpos <- barplot(rel.select[s,], beside=FALSE, xlim=x.limit, ylim=y.limit, xlab='', xaxt='n', main='', ylab=paste('Marine Age:',s,sep=' '), 
                col=cols[s], space=space)
      abline(h=0)
    }#next s
    axis(side=1, at=xpos, labels=years, las=2, outer=TRUE)
    mtext('Year', side=1, line=3.5, outer=TRUE)  
    mtext('Relative Selectivity Values', side=2, line=1, outer=TRUE)
    if(pdf == TRUE) { dev.off() }
    
    
    # temp.df <- data.frame(rel.select)
    # rownames(temp.df) <- paste('X.',row.names(select), sep='')
    # colnames(temp.df) <- years
    if(pdf == TRUE) {
      if(side == 'west') {
        pdf(file='WestSide Figs/WestSide Selectivity Dist.pdf', height=4, width=8)
      } else {
        pdf(file='EastSide Figs/EastSide Selectivity Dist.pdf', height=4, width=8)	
      }
    }
    par(mfrow=c(1,1), mar=c(3,3,2,1), oma=c(0,0,0,0))
    
    #DISTRIBUTIONS
    list.rel.select <- melt(rel.select)
    list.rel.select <- data.frame(list.rel.select)
    names(list.rel.select) <- c('OceanAge','Year','value')
    list.rel.select$OceanAge <- as.factor(list.rel.select$OceanAge)
    
    tmp <- ggplot(list.rel.select, aes(x=value, fill=OceanAge))
    tmp <- tmp + geom_density(alpha = 0.5, lwd=0.1) 
    tmp <- tmp + xlab('Relative Selectivity Values') + ylab('')
   
    if(side=='west') {
      tmp <- tmp + ggtitle('West Side Bristol Bay')	
    }else {
      tmp <- tmp + ggtitle('East Side Bristol Bay')	
    }
    #tmp <- tmp + scale_fill_hue(l=40)
    plot(tmp)
    tmp <- tmp + theme(legend.position='none')
    plot(tmp)
    
    age.cols <- colorRampPalette(c('blue','green','red'))(5)
    #CATERPILLAR PLOT
    #par(mfrow=c(2,1))
    t.rel.select <- data.frame(t(rel.select))
    colnames(t.rel.select) <- c(1:5) #paste('X.',c(1:5), sep='')

    caterplot(t.rel.select, denstrip=TRUE, quantiles=list(outer=c(0.025,0.975),inner=c(0.25,0.75)), font.lab=2)
    caterpoints(apply(t(rel.select), c(2), median), col='red', cex=2, pch=16)
    mtext('Relative Selectivity Values', side=1, line=2)
    mtext('Ocean Age', side=2, line=2)
    if(side=='west') {
      mtext('West Side Bristol Bay', side=3, font=2, outer=FALSE, line=0.5)
    }else {
      mtext('East Side Bristol Bay', side=3, font=2, outer=FALSE, line=0.5)
    }
    
    caterplot(t.rel.select, quantiles=list(outer=c(0.025,0.975),inner=c(0.25,0.75)), width=3, font.lab=2)
    caterpoints(apply(t(rel.select), c(2), median), col='red')
    mtext('Relative Selectivity Values', side=1, line=2)
    mtext('Ocean Age', side=2, line=2)
    if(side=='west') {
      mtext('West Side Bristol Bay', side=3, font=2, outer=FALSE, line=0.5)
    }else {
      mtext('East Side Bristol Bay', side=3, font=2, outer=FALSE, line=0.5)
    }
    
    #BEANPLOT
    beanplot(t.rel.select, lwd=1, col=c('blue', 'black', 'black', 'red'), ovalline='median', ll=0)
    mtext('Relative Selectivity Values', side=2, line=2)
    mtext('Ocean Age', side=1, line=2)
    if(side=='west') {
      mtext('West Side Bristol Bay', side=3, font=2, outer=FALSE, line=0.5)
    }else {
      mtext('East Side Bristol Bay', side=3, font=2, outer=FALSE, line=0.5)
    }
    beanplot(t.rel.select, lwd=1, col=c('blue', 'black', 'black', 'red'), ovalline='median', ll=0, horizontal=TRUE)
    mtext('Relative Selectivity Values', side=1, line=2)
    mtext('Ocean Age', side=2, line=2)
    if(side=='west') {
      mtext('West Side Bristol Bay', side=3, font=2, outer=FALSE, line=0.5)
    }else {
      mtext('East Side Bristol Bay', side=3, font=2, outer=FALSE, line=0.5)
    }
    
    
    #Violin ggplot
    calc.median <- function(x) {
      meds <- median(x)
      return(meds)
    }
    
    tmp <- ggplot(list.rel.select, aes(OceanAge, value, fill=OceanAge))
    tmp <- tmp + geom_violin(alpha = 0.5, lwd=0.1, scale='width') 
    tmp <- tmp + ylab('Relative Selectivity Values') + xlab('Ocean Age')
    tmp <- tmp + stat_summary(fun.y=calc.median, geom='point')
    if(side=='west') {
      tmp <- tmp + ggtitle('West Side Bristol Bay')	
    }else {
      tmp <- tmp + ggtitle('East Side Bristol Bay')	
    }
    plot(tmp)
    tmp <- tmp + coord_flip()
    plot(tmp)
    tmp <- tmp + theme(legend.position='none')
    plot(tmp)
    
    tmp <- ggplot(list.rel.select, aes(OceanAge, value, fill=OceanAge))
    tmp <- tmp + geom_violin(alpha = 0.5, lwd=0.1, scale='width') 
    tmp <- tmp + ylab('Relative Selectivity Values') + xlab('Ocean Age')
    #tmp <- tmp + stat_summary(fun.y=calc.median, geom='point')
    if(side=='west') {
      tmp <- tmp + ggtitle('West Side Bristol Bay')	
    }else {
      tmp <- tmp + ggtitle('East Side Bristol Bay')	
    }
    tmp <- tmp + geom_boxplot(width=0.25, lwd=0.5)#width=0.1, lwd=0.1
    plot(tmp)
    tmp <- tmp + coord_flip()
    plot(tmp)
    tmp <- tmp + theme(legend.position='none')
    plot(tmp)
        
    tmp <- ggplot(list.rel.select, aes(x=OceanAge, y=value, fill=OceanAge))
    tmp <- tmp + geom_boxplot() 
    tmp <- tmp + ylab('Relative Selectivity Values') + xlab('Stock')
    
    if(side=='west') {
      tmp <- tmp + ggtitle('West Side Bristol Bay')	
    }else {
      tmp <- tmp + ggtitle('East Side Bristol Bay')	
    }
    plot(tmp)
    tmp <- tmp + theme(legend.position='none')
    plot(tmp)
        
    if(pdf == TRUE) { dev.off() }
    
    ### WRITE SELECTIVITY TABLE ###
    sel.mean <- apply(rel.select, c(1), mean)
    sel.sd <- apply(rel.select, c(1), sd)
    sel.cv <- sel.sd/sel.mean
  	sel.quants <- apply(rel.select, c(1), quantile, probs=c(0.025,0.25,0.5,0.75,0.975))
    sel.output <- cbind(sel.mean,sel.sd,sel.cv,t(sel.quants))
    if(side=='west') {
      write.csv(sel.output, file='WestSide Figs/WestSide Selectivity Table.csv')
    }else {
      write.csv(sel.output, file='EastSide Figs/EastSide Selectivity Table.csv')
    }
  }  
  ########################################################### 
  if(plot.avail == TRUE) {
    if(pdf == TRUE) {
      if(side == 'west') {
        pdf(file='WestSide Figs/WestSide Availability Over Time.pdf', height=6, width=8)
      } else {
        pdf(file='EastSide Figs/EastSide Availability Over Time.pdf', height=6, width=8)	
      }
    }

    #PLOTTING AVAILABILITY OVER TIME
    #avail #[d,s,y]
    #avail.est #[d,y]
    #Relative Availability
    rel.avail <- array(dim=c(n.districts, n.stocks, n.years), dimnames=list(names.districts,names.stocks,years))
    #Density for delineating non-estimated availability values
    temp.dens <- matrix(nrow=n.districts, ncol=n.years, dimnames=list(names.districts,years))
  
    y <- 1
    for(y in 1:n.years) {
      d <- 1
  	  for(d in 1:n.districts) {
  	    rel.avail[d,,y] <- avail[d,,y]/sum(avail[d,,y])
  	    if(avail.est[d,y] == TRUE) { temp.dens[d,y] <- 1000 }else { temp.dens[d,y] <- 30 }
  	  }#next d  	
    }#next y
  
    space <- 0.2
    x.limit <- c(0,n.years+(n.years+1)*space)
    y.limit <- c(0,1.1*max(rel.avail))
    #y.limit <- c(0,1.1*max(avail))
    #cols <- rainbow(n.stocks)
    cols <- topo.colors(n.stocks)
  
    par(mfrow=c(n.stocks,1), oma=c(5,3,4,1), mar=c(0,4,0,4))
  
    d <- 1
    for(d in 1:n.districts) {
      s <- 1
      for(s in 1:n.stocks) {
        xpos <- barplot(rel.avail[d,s,], beside=FALSE, xlim=x.limit, ylim=y.limit, xlab='', xaxt='n', main='', 
                          ylab=paste(names.stocks[s], 'River', sep=' '), 
                          col=cols[s], space=space, density=temp.dens[d,], angle=45)
      }#next s
      mtext('Year', side=1, line=3.5, outer=TRUE)  
      mtext('Relative Availability Values', side=2, line=1, outer=TRUE)
      axis(side=1, at=xpos, labels=years, las=2)
      mtext(paste(names.districts[d], 'District Availability', sep=' '), side=3, line=1.5, font=2, cex=1.5, outer=TRUE)
    } #next d 
    
    #NEW FIGURES FOR PUBLICATION
    
    if(pdf == TRUE) { dev.off() }
    #good to here
    if(pdf == TRUE) {
      if(side == 'west') {
        pdf(file='WestSide Figs/WestSide Availability Dist.pdf', height=6, width=6)
      } else {
        pdf(file='EastSide Figs/EastSide Availability Dist.pdf', height=6, width=6)	
      }
    }
    par(mfrow=c(1,1), mar=c(3,3,2,1), oma=c(0,0,0,0))
    
    #DISTRIBUTIONS
    #avail
    #rel.avail
    #avail.est
    
    #Replace assumed values with NA's
    dim(avail)
    dim(avail.est)
    
    avail.dat <- avail
    #avail.dat <- rel.avail
    
    d <- 1
    for(d in 1:n.districts) {
      y <- 1
      for(y in 1:n.years) {
      	if(avail.est[d,y]==FALSE) {
          s <- 1
          for(s in 1:n.stocks) {
          	avail.dat[d,s,y] <- NA
          }#next s
        }
      }#next y
    }#next d
    
    #Create List Object
    list.avail <- melt(avail.dat)
    list.avail <- data.frame(list.avail)
    names(list.avail) <- c('District','Stock','Year','value')
    list.avail$Stock <- as.factor(list.avail$Stock)
    
    #Plot Distribtuions
    tmp <- ggplot(list.avail, aes(x=value, fill=Stock))
    tmp <- tmp + geom_density(alpha = 0.5, lwd=0.1) 
    tmp <- tmp + xlab('Availability Values') + ylab('')
    tmp <- tmp + facet_wrap(~District, ncol=1)
   
    if(side=='west') {
      tmp <- tmp + ggtitle('West Side Bristol Bay')	
    }else {
      tmp <- tmp + ggtitle('East Side Bristol Bay')	
    }
    #tmp <- tmp + scale_fill_hue(l=40)
    plot(tmp)
    tmp <- tmp + theme(legend.position='none')
    plot(tmp)
        
    #Violin ggplot
    calc.median <- function(x) {
      meds <- median(x)
      return(meds)
    }
    tmp <- ggplot(list.avail, aes(x=Stock, y=value, fill=Stock))
    tmp <- tmp + geom_violin(alpha = 0.5, lwd=0.1, scale='width') 
    tmp <- tmp + ylab('Relative Availability Values') + xlab('Stock')
    tmp <- tmp + stat_summary(fun.y=calc.median, geom='point')
    tmp <- tmp + facet_wrap(~District, ncol=1)
    
    if(side=='west') {
      tmp <- tmp + ggtitle('West Side Bristol Bay')	
    }else {
      tmp <- tmp + ggtitle('East Side Bristol Bay')	
    }
    plot(tmp)
    tmp <- tmp + coord_flip()
    plot(tmp)
    tmp <- tmp + theme(legend.position='none')
    plot(tmp)
        
    tmp <- ggplot(list.avail, aes(x=Stock, y=value, fill=Stock))
    tmp <- tmp + geom_violin(alpha = 0.5, lwd=0.1, scale='width') 
    tmp <- tmp + ylab('Relative Availability Values') + xlab('Stock')
    #tmp <- tmp + stat_summary(fun.y=calc.median, geom='point')
    tmp <- tmp + facet_wrap(~District, ncol=1)
    
    if(side=='west') {
      tmp <- tmp + ggtitle('West Side Bristol Bay')	
    }else {
      tmp <- tmp + ggtitle('East Side Bristol Bay')	
    }
    tmp <- tmp + geom_boxplot(width=0.2, lwd=0.5) #width=0.5
    plot(tmp)
    tmp <- tmp + coord_flip()
    plot(tmp)
    tmp <- tmp + theme(legend.position='none')
    plot(tmp)
    plot(tmp + scale_x_discrete(limit=rev(names.stocks)))    
    #Boxplots
    tmp <- ggplot(list.avail, aes(x=Stock, y=value, fill=Stock))
    tmp <- tmp + geom_boxplot(lwd=0.5) 
    tmp <- tmp + ylab('Relative Availability Values') + xlab('Stock')
    tmp <- tmp + facet_wrap(~District, ncol=1)
    
    if(side=='west') {
      tmp <- tmp + ggtitle('West Side Bristol Bay')	
    }else {
      tmp <- tmp + ggtitle('East Side Bristol Bay')	
    }
    plot(tmp)
    tmp <- tmp + theme(legend.position='none')
    plot(tmp)

    #tmp <- coord_flip()
    #plot(tmp)
    
    #### CONTINUE HERE!!!!
    #rel.avail
    #avail.est
    
    par(mfrow=c(n.districts,1), oma=c(2,2,2,1), mar=c(2,4,2,0))
    d <- 1
    for(d in 1:n.districts) {
      temp.dat <- t(avail.dat[d,,avail.est[d,]])
      caterplot(temp.dat, quantiles=list(outer=c(0.025,0.975),inner=c(0.25,0.75)), font.lab=2, denstrip=FALSE, width=3, lwd=c(2,4))
      mtext(paste(names.districts[d],'District'), side=3, font=2)
      caterpoints(apply(temp.dat, c(2), median), col='red', cex=1.5)
    }#next d
    mtext('Stock', side=2, outer=TRUE, font=2)
    mtext('Relative Availability', side=1, outer=TRUE, font=2, line=0.5)
    d <- 1
    for(d in 1:n.districts) {
      temp.dat <- t(avail.dat[d,,avail.est[d,]])
      caterplot(temp.dat, quantiles=list(outer=c(0.025,0.975),inner=c(0.25,0.75)), font.lab=2, denstrip=FALSE)
      mtext(paste(names.districts[d],'District'), side=3, font=2)
      caterpoints(apply(temp.dat, c(2), median), col='red')
    }#next d
    mtext('Stock', side=2, outer=TRUE, font=2)
    mtext('Relative Availability', side=1, outer=TRUE, font=2, line=0.5)
    #Density Strip
    d <- 1
    for(d in 1:n.districts) {
      temp.dat <- t(avail.dat[d,,avail.est[d,]])
      caterplot(temp.dat, quantiles=list(outer=c(0.025,0.975),inner=c(0.25,0.75)), font.lab=2, denstrip=TRUE)
      mtext(paste(names.districts[d],'District'), side=3, font=2)
      caterpoints(apply(temp.dat, c(2), median), col='red', pch=16, cex=1.5)
    }#next d
    mtext('Stock', side=2, outer=TRUE, font=2)
    mtext('Relative Availability', side=1, outer=TRUE, font=2, line=0.5)
       
    ### WRITE SELECTIVITY TABLE ###
    list.dist <- vector(length=0)
    list.stock <- vector(length=0)
    list.mean <- vector(length=0)
    list.sd <- vector(length=0)
    list.cv <- vector(length=0)
    
    d <- 1
    for(d in 1:n.districts) {
      dist <- names.districts[d]
      #Update lists
      list.dist <- append(list.dist, rep(dist, n.stocks))
      list.stock <- append(list.stock, names.stocks)
      temp.mean <- apply(avail.dat[d,,], c(1), mean, na.rm=TRUE)
      list.mean <- append(list.mean, temp.mean)
      temp.sd <- apply(avail.dat[d,,], c(1), sd, na.rm=TRUE)
      list.sd <- append(list.sd, temp.sd)
      temp.cv <- temp.sd/temp.mean
      list.cv <- append(list.cv, temp.cv)
      
      if(d==1) {
        temp.quant <- apply(avail.dat[d,,], c(1), quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm=TRUE)
        mtx.quant <- t(temp.quant)
      }else {
        temp.quant <- apply(avail.dat[d,,], c(1), quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm=TRUE)
        mtx.quant <- rbind(mtx.quant, t(temp.quant))
      }  
    }#next d
    avail.output <- cbind(list.dist, list.stock, list.mean, list.sd, list.cv, mtx.quant)    
    avail.output <- data.frame(avail.output)

    if(side=='west') {
      write.csv(avail.output, file='WestSide Figs/WestSide Availability Table.csv')
    }else {
      write.csv(avail.output, file='EastSide Figs/EastSide Availability Table.csv')
    }

    if(pdf == TRUE) { dev.off() }
  }
  ###########################################################  
  if(plot.fmort == TRUE) {
    #### NOT FINISHED - NOT TRUELY NECESSARY ####
    print(fmort)
    if(pdf == TRUE) {
      if(side == 'west') {
        pdf(file='WestSide Figs/WestSide Fmort Over Time.pdf', height=2, width=8)
      } else {
        pdf(file='EastSide Figs/EastSide Fmort Over Time.pdf', height=6, width=6)	
      }
    }
    par(mfrow=c(1,1), oma=c(1,1,1,1), mar=c(0,0,0,0))
    list.fmort <- melt(fmort)
    list.fmort <- data.frame(list.fmort)
    names(list.fmort) <- c('District','Year','value')
    list.fmort$District <- as.factor(list.fmort$District)

    #Lines
    g <- ggplot(list.fmort, aes(x=Year, y=value, fill=District))
    g <- g + geom_line(aes(col=District))
    g <- g + ylab('Estimated Fishing Mortality Rate')
    if(side=='west') {
      g <- g + ggtitle('West Side Bristol Bay')	
    }else {
      g <- g + ggtitle('East Side Bristol Bay')	
    }
    plot(g)
    
    #Distributions
    g <- ggplot(list.fmort, aes(x=District, y=value, fill=District))
    g <- g + geom_violin()
    g <- g + ylab('Estimated Fishing Mortality Rate')
    if(side=='west') {
      g <- g + ggtitle('West Side Bristol Bay')	
    }else {
      g <- g + ggtitle('East Side Bristol Bay')	
    }
    plot(g)
    
    #Boxplot
    g <- ggplot(list.fmort, aes(x=District, y=value, fill=District))
    g <- g + geom_boxplot(lwd=0.5)
    g <- g + ylab('Estimated Fishing Mortality Rate')
    if(side=='west') {
      g <- g + ggtitle('West Side Bristol Bay')	
    }else {
      g <- g + ggtitle('East Side Bristol Bay')	
    }
    plot(g)
    
    #Caterplot
    par(mfrow=c(1,1), oma=c(2,2,0,0), mar=c(2,6,2,1))
    caterplot(t(fmort))
    mtext('District', side=2, outer=TRUE, font=2, line=1)
    mtext('Estimated Fishing Mortality Rate', side=1, outer=TRUE, font=2, line=0.5)
    if(side=='west') {
      mtext('West Side Bristol Bay', side=3, line=0.5, font=2)
    }else {
      mtext('East Side Bristol Bay', side=3, line=0.5, font=2)
    }
        
    if(pdf==TRUE) { dev.off() }
    

  }#end if plot Fmort
  ###########################################################
  if(plot.hist == TRUE) {
    if(pdf == TRUE) {
      if(side == 'west') {
        pdf(file='WestSide Figs/WestSide Availability Histogram.pdf', height=6, width=5)
      } else {
        pdf(file='EastSide Figs/EastSide Availability Histogram.pdf', height=6, width=8)	
      }
    }
    par(mfcol=c(n.stocks,n.districts), mar=c(4,4,0,0), oma=c(4,4,4,4))
    n.breaks <- 10
    d <- 1
    for(d in 1:n.districts) {
      s <- 1
      for(s in 1:n.stocks) {
        xpos <- hist(avail[d,s,which(avail.est[d,])], main='', xlab='',ylab='', col='gray', breaks=n.breaks)
        abline(v=mean(avail[d,s,which(avail.est[d,])]), col='red', lwd=3)
        abline(v=median(avail[d,s,which(avail.est[d,])]), col='blue', lwd=3)
        if(d == 1) { mtext(paste(names.stocks[s],'River',sep=' '), side=2, line=2.5, outer=FALSE, cex=0.75)  }
        if(s == n.stocks) { mtext(paste(names.districts[d],'District',sep=' '), side=1, line=2.5, outer=FALSE, cex=0.75) }
        if(d == 1 & s == 1) { legend('topright', legend=c('Mean','Median'), bty='n', col=c('red','blue'), lty=c(1,1)) }
      }#next s
    	
    }#next d
    mtext('District', side=1, line=2, outer=TRUE, font=2)
    mtext('Stock', side=2, line=2, outer=TRUE, font=2)
    if(pdf == TRUE) { dev.off() }
  }
  
}

####### plot.apportioned.catch ######
# INPUTS: 1) side - "east" or "west"
#         2) years - years for which plots will be generated
#         3) pdf - T/F whether .pdf file will be generated
#		      4) separate - T/F whether to put different districts on the same or different pages
################################


plot.apportioned.catch <- function(side, years, pdf=FALSE, separate=FALSE, write.nush.data=FALSE, write.table=FALSE, wd=wd) {
  setwd(paste(wd, "//Syrah/outputFiles", sep=""))
  ### TESTING ###
#   side <- 'west'
#   years <- 1963:2014
#   pdf <- FALSE
#   separate <- FALSE
#   write.nush.data <- FALSE
#   write.table <- TRUE
  ###
  n.select.par <- 5
  n.agecomps <- 18
  
  n.years <- length(years)
  
  if(side == 'west') {
    n.districts <- 1
    n.stocks <- 3
    names.districts <- 'Nushagak'
    names.stocks <- c('Igushik', 'Wood', 'Nushagak')
  }
  if(side == 'east') {
    n.districts <- 3
    n.stocks <- 5
    names.districts <- c('Naknek-Kvichak','Egegik','Ugashik')
    names.stocks <- c('Kvichak','Alagnak','Naknek','Egegik','Ugashik')	
  }
  if(side != 'west' & side != 'east') { print('##### ERROR side selection is incorrect'); stop(); }
  
  #Catches by district and stock
  catches <- array(data=0, dim=c(n.districts, n.stocks, n.years), dimnames=list(names.districts, names.stocks, years))
  total.catches <- matrix(nrow=n.districts, ncol=n.years, dimnames=list(names.districts, years))
  
  #For Chuck Brazil - Nush catch and escapement by group
  nush.catch <- matrix(nrow=n.years, ncol=n.agecomps)
  nush.esc <- matrix(nrow=n.years, ncol=n.agecomps)
  
  y <- 1
  for(y in 1:n.years) {
    year <- years[y]
    
    if(side == 'west') {
      temp.data <- readList(paste('WestSide/WestSide_',year,'.out',sep='')) 
      
    }else {
      temp.data <- readList(paste('EastSide/EastSide_',year,'.out',sep=''))	
    } 
    
    #Retreive general info
    n.agecomps <- temp.data$nagecomps
    
    d <- 1
    for(d in 1:n.districts) {
      #Total catch by district
      total.catches[d,y] <- temp.data$predCatch[d]
      counter <- 1

      s <- 1
      for(s in 1:n.stocks) {
        if(side == 'west') {
          catches[d,s,y] <- sum(temp.data$catchByGroup[counter:(counter+n.agecomps-1)])
        } else {
          catches[d,s,y] <- sum(temp.data$catchByGroup[d,c(counter:(counter+n.agecomps-1))])
        }
        #Update counter
        counter <- counter+n.agecomps
      }#next s
    }#next d
    if(write.nush.data == TRUE & side == 'west') {
      nush.catch[y,] <- temp.data$catchByGroup[(2*n.agecomps+1):length(temp.data$catchByGroup)]
      nush.esc[y,] <- temp.data$escByGroup[(2*n.agecomps+1):length(temp.data$catchByGroup)]
    }
  }#next y  
  
  #PLOTTING
  if(pdf == TRUE) {
    if(side == 'west') {
      pdf(file='WestSide Figs/WestSide Apportioned Catches.pdf', height=8, width=8)
    } else {
      pdf(file='EastSide Figs/EastSide Apportioned Catches.pdf', height=8, width=8)
    }
  }
  
  if(separate == TRUE) { par(mfrow=c(1,1), mar=c(0.1,4,0,0), oma=c(5,2,1,1)) } else { par(mfrow=c(n.districts,1), mar=c(0.125,4,0,0), oma=c(5,2,1,1)) }
  #cols <- rainbow(n.stocks)
  cols <- topo.colors(n.stocks)
  if(side=='east') { cols[2] <- 'red' }
  #cols <- brewer.pal(n=n.stocks, name='Set2')  

  space <- 0.2
  x.limit <- c(0,n.years+(n.years+1)*space)
  
 # barplot(genSS[d,gen.est], las=2, xlim=xlim, ylim=ylim.ss, space=space, axes=FALSE, col='grey50', xaxs='i')
  
  d <- 1
  for(d in 1:n.districts) {
  	y.limit <- c(0, 1.1*max(total.catches[d,]/1e+6))
  	xpos <- barplot(catches[d,,]/1e+6, las=2, xlim=x.limit, ylim=y.limit, space=space, xaxt='n', xlab='', 
  	                  col=cols, ylab=paste(names.districts[d],'District', sep=' '))
  	abline(h=0, lwd=2)  	
  	abline(h=pretty(c(0,y.limit)), lty=2)
  	if(d == 1) { legend('top', legend=names.stocks, ncol=n.stocks, fill=cols, bty='n')}
  	if(separate == TRUE) { yrs.pty <- pretty(years)[-c(1,7)]
                           loc.pty <- which(years %in% yrs.pty)
  	                       axis(side=1, at=xpos[loc.pty], labels=yrs.pty, las=2, outer=TRUE)
                         axis(side=1, at=xpos, labels=FALSE, las=2, outer=TRUE, col='gray')
  		                   mtext('Year', side=1, line=3.5, outer=TRUE, font=2) 
  		                   mtext('Total Catch (millions)', side=2, line=0.5, outer=TRUE, font=2) }
  	if(separate == FALSE & d == n.districts) { axis(side=1, at=xpos, labels=years, las=2, outer=TRUE) 
  		                                       mtext('Year', side=1, line=3.5, outer=TRUE, font=2) 
  		                                       mtext('Total Catch (millions)', side=2, line=0.5, outer=TRUE, font=2)}
  }#next d
  
  #PLOT PROPORTIONAL CATCHES
  if(write.table==TRUE) {
    if(side=='west') {
      write.xlsx(x=catches, file='WestSide Figs/WestSide Apportioned Catches.xlsx', sheetName='catches', append=FALSE)
    }else {
      write.xlsx(x=catches, file='EastSide Figs/EastSide Apportioned Catches.xlsx', sheetName='catches', append=FALSE)
    }
  }

  par(mfrow=c(2,1), mar=c(0.5,4,0,0), oma=c(4.5,2,1,1)) 
  d <- 1
  for(d in 1:n.districts) {
    y.limit <- c(0, 1)
    temp.sum <- apply(catches[d,,], c(2), sum)
    temp.prop <- catches[d,,]
    s <- 1
    for(s in 1:n.stocks) {
      temp.prop[s,] <- temp.prop[s,]/temp.sum
    }
    xpos <- barplot(temp.prop, las=2, xlim=x.limit, ylim=y.limit, space=space, xaxt='n', xlab='', 
                    col=cols, ylab=paste('Proportion of Catch'))
    abline(h=0, lwd=2)  	
    abline(h=pretty(c(0,y.limit)), lty=2)
    
    #Catches
    y.limit <- c(0, 1.1*max(total.catches[d,]/1e+6))
    xpos <- barplot(catches[d,,]/1e+6, las=2, xlim=x.limit, ylim=y.limit, space=space, xaxt='n', xlab='', 
                col=cols, ylab='Total Catch (millions)')
    abline(h=0, lwd=2)  	
    abline(h=pretty(c(0,y.limit)), lty=2)
    legend('top', legend=names.stocks, ncol=n.stocks, fill=cols, bty='n')
    yrs.pty <- pretty(years)[-c(1,7)]
    loc.pty <- which(years %in% yrs.pty)
    axis(side=1, at=xpos[loc.pty], labels=yrs.pty, las=2, outer=TRUE)
    axis(side=1, at=xpos, labels=FALSE, las=2, outer=TRUE, col='gray')
    mtext('Year', side=1, line=3.5, outer=TRUE, font=2) 
    mtext(paste(names.districts[d],'District', sep=' '), side=2, line=0.5, outer=TRUE, font=2) 
    #For Table
    if(write.table==TRUE) {
      list.mean <- apply(temp.prop, c(1), mean)
      list.sd <- apply(temp.prop, c(1), sd)
      list.cv <-list.sd/list.mean
      list.props <- apply(temp.prop, c(1), quantile, probs=c(0.025,0.25,0.5,0.75,0.975))
      output <- cbind(names.stocks,list.mean,list.sd,list.cv,t(list.props))
      if(side=='west') {
        write.xlsx(x=output, file='WestSide Figs/WestSide Apportioned Catches.xlsx', sheetName=names.districts[d], append=TRUE)
      }else {
        write.xlsx(x=output, file='EastSide Figs/EastSide Apportioned Catches.xlsx', sheetName=names.districts[d], append=TRUE)
      }      
    }
  }#next d
 
  if(pdf == TRUE) { dev.off() }
  
  if(write.nush.data == TRUE){ 
    rownames(nush.catch) <- years; rownames(nush.esc) <- years
    colnames(nush.catch) <- temp.data$AgeCompLabels; colnames(nush.esc) <- temp.data$AgeCompLabels
    write.csv(nush.catch, file='WestSide Figs/Reconstructed Nushagak Catches.csv')
    write.csv(nush.esc, file='WestSide Figs/Reconstructed Nushagak Escapements.csv')
  }
}

# plot.apportioned.catch(side='west', years=1963:2014, pdf=FALSE, separate=FALSE, write.nush.data=FALSE, write.table=TRUE, wd=wd)
# plot.apportioned.catch(side='east', years=1963:2014, pdf=FALSE, separate=FALSE, write.nush.data=FALSE, write.table=TRUE, wd=wd)

##############################################################################################################
##### HELPER FUNCTIONS #####

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

##############################################################################################################
##### TABLES #####

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
    ## TESTING ##
#     side <- 'east'
#     years <- 1963:2016
#     reallocate <- TRUE
#     allocateOffshore <- TRUE
#     renorm.obs.err <- TRUE
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
      	write.xlsx(x=cbind(c('Return Year', years), rbind(temp.data$AgeCompLabels,return.data[,,s])),
      	           file=paste('WestSide Figs/ALL Return Table.xlsx', sep=''),
      	           sheetName=names.stocks[s], row.names=FALSE, append=FALSE)
      	           
      	write.xlsx(x=cbind(rbind(c('Brood Year', temp.data$AgeCompLabels),brood.data[,,s]), c('Escapement', rep(NA,max(offset)), escapements[,s]), 
                                                                                       c('Recruits', rep(NA,max(offset)), recruits[,s]), 
                                                                                       c('R/S', rep(NA,max(offset)), rps[,s])),
      	           file=paste('WestSide Figs/ALL Brood Table.xlsx', sep=''),
      	           sheetName=names.stocks[s], row.names=FALSE, append=FALSE)      	              	           	           
      }else {
      	write.xlsx(x=cbind(c('Return Year', years), rbind(temp.data$AgeCompLabels,return.data[,,s])),
      	           file=paste('WestSide Figs/ALL Return Table.xlsx', sep=''),
      	           sheetName=names.stocks[s], row.names=FALSE, append=TRUE)
      	           
      	write.xlsx(x=cbind(rbind(c('Brood Year', temp.data$AgeCompLabels),brood.data[,,s]), c('Escapement', rep(NA,max(offset)), escapements[,s]), 
                                                                                       c('Recruits', rep(NA,max(offset)), recruits[,s]), 
                                                                                       c('R/S', rep(NA,max(offset)), rps[,s])),
      	           file=paste('WestSide Figs/ALL Brood Table.xlsx', sep=''),
      	           sheetName=names.stocks[s], row.names=FALSE, append=TRUE)      	         
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
      	write.xlsx(x=cbind(c('Return Year', years), rbind(temp.data$AgeCompLabels,return.data[,,s])),
      	           file=paste('EastSide Figs/ALL Return Table.xlsx', sep=''),
      	           sheetName=names.stocks[s], row.names=FALSE, append=FALSE)
      	           
      	write.xlsx(x=cbind(rbind(c('Brood Year', temp.data$AgeCompLabels),brood.data[,,s]), c('Escapement', rep(NA,max(offset)), escapements[,s]), 
                                                                                       c('Recruits', rep(NA,max(offset)), recruits[,s]), 
                                                                                       c('R/S', rep(NA,max(offset)), rps[,s])),
      	           file=paste('EastSide Figs/ALL Brood Table.xlsx', sep=''),
      	           sheetName=names.stocks[s], row.names=FALSE, append=FALSE)      	              	           	           
      }else {
      	write.xlsx(x=cbind(c('Return Year', years), rbind(temp.data$AgeCompLabels,return.data[,,s])),
      	           file=paste('EastSide Figs/ALL Return Table.xlsx', sep=''),
      	           sheetName=names.stocks[s], row.names=FALSE, append=TRUE)
      	           
      	write.xlsx(x=cbind(rbind(c('Brood Year', temp.data$AgeCompLabels),brood.data[,,s]), c('Escapement', rep(NA,max(offset)), escapements[,s]), 
                                                                                       c('Recruits', rep(NA,max(offset)), recruits[,s]), 
                                                                                       c('R/S', rep(NA,max(offset)), rps[,s])),
      	           file=paste('EastSide Figs/ALL Brood Table.xlsx', sep=''),
      	           sheetName=names.stocks[s], row.names=FALSE, append=TRUE)      	         
      }
    
    }
  }#next s
}

# create.brood(side='east', years=1963:2016, reallocate=TRUE, allocateOffshore=TRUE, renorm.obs.err=TRUE, wd=wd)

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
  require(xlsx)
  
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
        write.xlsx(x=total.ret[,i,], file=paste('WestSide Figs/Extras/Daniel Summary_west.xlsx', sep=''), sheetName=dimnames(total.ret)[[2]][i], append=ifelse(i==1,FALSE,TRUE) ) 
      }else {
        write.xlsx(x=total.ret[,i,], file=paste('EastSide Figs/Extras/Daniel Summary_east.xlsx', sep=''), sheetName=dimnames(total.ret)[[2]][i], append=ifelse(i==1,FALSE,TRUE) ) 
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

######### fit.stock.recruit ########
# INPUTS: 1) side - "east" or "west"
#         2) stock
#
################################

fit.stock.recruit <- function(pdf=FALSE, wd=wd) {
  setwd(paste(wd, "/Syrah/outputFiles", sep=""))
  ### TESTING
  #side <- 'west'
  #stock <- 'Wood'
  ###
  
  n.stocks <- 8
  side.stocks <- c(rep('west',3), rep('east',5))
  name.stocks <- c('Igushik','Wood','Nushagak','Kvichak','Alagnak','Naknek','Egegik','Ugashik')
  
  if(pdf == TRUE) { pdf('RS Eval/Escapement Evaluation.pdf', height=9, width=8) }
  
  s <- 1
  for(s in 1:n.stocks) {
  #Stock specific information
  side <- side.stocks[s]
  stock <- name.stocks[s]	
  	
  if(side == 'west') {
    data <- data.frame(read.csv(paste("WestSide Figs/",stock," Brood Table.csv", sep=''), skip=1))
  }else {
  	data <- data.frame(read.csv(paste("EastSide Figs/",stock," Brood Table.csv", sep=''), skip=1))
  }
  
  data <- data[,c(2,21,22,23)]; names(data) <- c('broodYr','esc','rec','rps')
  data <- na.omit(data)  #Remove NA's from data
  
  #PLOTTING
  #start.alpha <- 1
  #start.beta <- 1e+6
  
  #par(mfrow=c(2,1))
  #plot(rec ~ esc, data=data, type='p'); 
  #points(x=data$esc, y=data$esc*exp(start.alpha*(1-(data$esc/start.beta))), col='red')
  #points(x=data$esc, y=(start.alpha*data$esc)/(start.beta+data$esc), col='red')
  #plot(rps ~ esc, data=data, type='p')
  
  
  
  #MODEL FITTING
  fit.ricker <- nls(rec ~ esc*exp(alpha*(1-(esc/beta))), data=data, start=list(alpha=1, beta=1e+6))
  fit.bh <- nls(rec ~ (alpha*esc)/(beta+esc), data=data, start=list(alpha=5e+6, beta=1e+6))
  
  #PLOT THE FIT
  par(mfrow=c(2,1), mar=c(0,4,0,4), oma=c(4,0,4,0))
  plot(rec~esc, data=data, type='p', pch=21, bg='gray', xlab='', ylab='Recruits', xaxt='n')
  trial <- seq(0,max(data$esc),100)
  
  lines(trial,predict(fit.bh, newdata=list(esc=trial)), col='blue', lwd=2)
  lines(trial,predict(fit.ricker, newdata=list(esc=trial)), col='red', lwd=2)
  legend('topright', title='AIC', legend=c(paste('B-H: ', round(AIC(fit.bh),0)), paste('Ricker: ', round(AIC(fit.ricker),0))), col=c('blue','red'), lwd=2, bty='n')
  #segments(0,0,1e+7,1e+7)
  #SURPLUS PRODUCTION
  plot((rec-esc)~esc, data=data, type='p', pch=21, bg='gray', xlab='', ylab='Surplus Production', xaxt='n')
  #legend('topright', title='Model', legend=c(paste('B-H: ', round(AIC(fit.bh),0)), paste('Ricker: ', round(AIC(fit.ricker),0))), col=c('blue','red'), lwd=2)
  lines(trial, predict(fit.bh, newdata=list(esc=trial))-trial, col='blue', lwd=2)
  max.sp <- max(predict(fit.bh, newdata=list(esc=trial))-trial)
  opt.bh <- trial[which((predict(fit.bh, newdata=list(esc=trial))-trial)==max.sp)]
  segments(opt.bh,-10e+6,opt.bh,max.sp, lty=1, col='blue')
  segments(0,max.sp,opt.bh,max.sp, lty=1, col='blue')
    
  lines(trial, predict(fit.ricker, newdata=list(esc=trial))-trial, col='red', lwd=2)
  max.sp <- max(predict(fit.ricker, newdata=list(esc=trial))-trial)
  opt.ricker <- trial[which((predict(fit.ricker, newdata=list(esc=trial))-trial)==max.sp)]
  segments(opt.ricker,-10e+6,opt.ricker,max.sp, lty=1, col='red')
  segments(0,max.sp,opt.ricker,max.sp, lty=1, col='red')
  
  legend('topright', title='MSY Esc', legend=c(paste('B-H: ',opt.bh), paste('Ricker: ', opt.ricker)), bty='n')
  
  mtext('Escapement', side=1, outer=TRUE, line=3)
  axis(side=1)
  mtext(paste(stock,'River',sep=' '), side=3, outer=2, font=2, line=2)
  
  }#next s
  if(pdf == TRUE) {dev.off() }
  
}

#fit.stock.recruit(pdf=TRUE, wd=wd)

######### create.annual.summary ########
# INPUTS: 1) year
#
################################

create.annual.summary <- function(year, side, wd=wd) {
  setwd(paste(wd, "/Syrah/outputFiles", sep=""))
  ### TESTING ###
   # year <- 2013
   # side <- 'east'
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
    s <- which(names.stocks=='Igushik')
    temp.stream <- esc.stream[s]
    temp.dist <- esc.dist[s]
    temp.subdistrict <- 'Igushik Set'
    sub.catch <- add.catch$number[add.catch$subdistrict==temp.subdistrict & add.catch$year==year]
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
      if(length(sub.catch)>0) {
  	    output[dim,,s] <- sub.catch*temp.esc.ac
  	  }else {
  	  	output[dim,,s] <- 0
  	  }
    }
    
    #WRSHA
    dim <- 5
    s <- which(names.stocks=='Wood')
    temp.stream <- esc.stream[s]
    temp.dist <- esc.dist[s]
    temp.subdistrict <- 'WRSHA'
    sub.catch <- add.catch$number[add.catch$subdistrict==temp.subdistrict & add.catch$year==year]
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
  	  if(length(sub.catch) > 0) {
  	    output[dim,,s] <- sub.catch*temp.esc.ac
  	  }else {
  	    output[dim,,s] <- 0
  	  }
    }

  }else {
  	#Kvichak Set
  	dim <- 4
  	temp.subdistrict <- 'Kvi Set'
  	temp.add.catch <- add.catch[add.catch$subdistrict==temp.subdistrict,]
  	n.catches <- nrow(temp.add.catch)
  	i <- 1
  	for(i in 1:n.catches) {
  	  s <- which(names.stocks==temp.add.catch$district[i])
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
  	  s <- which(names.stocks==temp.add.catch$district[i])
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
  	  s <- which(names.stocks==temp.add.catch$district[i])
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

#create.annual.summary(year=2012, side='west', wd=wd)

####### plot.agecomp.coord.all ######
# INPUTS: 1) side - "east" or "west"
#         2) years - years for which plots will be generated
#         3) all - whether all age classes should be plotted (if FALSE will plot 1.1 1.2 1.3 2.1 2.2 2.3)
#         4) pdf - T/F whether .pdf file will be generated
#         5) sz.pts - vector of sizes of points in plots 1)all 2) critical
#		  6) omit.est - omit fits to estimated age composition data
#
################################

plot.agecomp.coord.all <- function(side, years, all=TRUE, pdf=FALSE, sz.pts, omit.est=TRUE, wd=wd) {
  setwd(paste(wd, "/Syrah/outputFiles", sep=""))
  ### TESTING ###
  #side <- 'west'
  #years <- 1963:2014
  #pdf <- FALSE
  #sz.pts <- c(1,2)
  #omit.est <- TRUE
  ###############
  
  ac.data <- na.omit(read.csv(paste(wd, "/R/ageComp.annual.csv", sep=""), header=TRUE, stringsAsFactors=FALSE)[,-1])
  n.agecomps <- 18
  #Limits
  #space <- 0.2
  #x.limit <- c(0,n.agecomps+(n.agecomps+1)*space)
  
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
        if(nrow(ac.data[ac.data$catch.esc=='catch' & ac.data$year == years[y] & ac.data$district==cat.dist[d],]) == 0 & omit.est == TRUE) {
          pred.catch[d,ac,y] <- NA
          obs.catch[d,ac,y] <- NA	
        } else {
          pred.catch[d,ac,y] <- if(side == 'west') { temp.data$predAgeCompCatch[ac] }else { temp.data$predAgeCompCatch[d,ac] }
          obs.catch[d,ac,y] <- if(side == 'west') { temp.data$obsAgeCompCatch[ac] }else { temp.data$obsAgeCompCatch[d,ac] }	
        }
      }#next d
      s <- 1
      for(s in 1:n.stocks) {
      	if(nrow(ac.data[ac.data$catch.esc=='esc' & ac.data$year == years[y] & ac.data$district==esc.dist[s] & ac.data$stream==esc.stream[s],]) == 0 & omit.est == TRUE) {
      	  pred.esc[s,ac,y] <- NA
      	  obs.esc[s,ac,y] <- NA	
      	}else {
        pred.esc[s,ac,y] <- temp.data$predAgeCompEsc[s,ac]
        obs.esc[s,ac,y] <- temp.data$obsAgeCompEsc[s,ac]
        }
      }#next s
    }#next ac
    
    if(length(which(pred.catch[,,y] > 1))!=0) {print(paste('##### AgeComp ERROR in', year, 'catch', sep=' '))}
    if(length(which(pred.esc[,,y] > 1))!=0) {print(paste('##### AgeComp ERROR in', year, 'escapement', sep=' '))}
  	
    if(y == n.years) { ageComps <- temp.data$AgeCompLabels }  #Rereive ac labels	 
  }#next y
  
  #Location of critical age comps
  crit.ac <- c(6:8,11:13)
  n.crit.ac <- length(crit.ac)
  
  #Colors 
  # catch.pal <- colorRampPalette(c('red', 'orange'))
  # catch.cols.all <- catch.pal(n.agecomps+1)[1:n.agecomps]
  # catch.cols.crit <- catch.pal(n.crit.ac+1)[1:n.crit.ac]
  # esc.pal <- colorRampPalette(c('blue','green','yellow'))
  # esc.cols.all <- esc.pal(n.agecomps+1)[1:n.agecomps]
  # esc.cols.crit  <- esc.pal(n.crit.ac+1)[1:n.crit.ac]
  
  catch.cols.all <- colorRampAlpha(c('maroon','red', 'orange','yellow'), n=n.agecomps+1, alpha=0.3)[1:n.agecomps]
  catch.cols.crit <- colorRampAlpha(c('maroon', 'red', 'orange','yellow'), n=n.crit.ac+1, alpha=0.3)[1:n.crit.ac]
  esc.cols.all <- colorRampAlpha(c('blue','green','yellow'), n=n.agecomps+1, alpha=0.3)[1:n.agecomps]
  esc.cols.crit  <- colorRampAlpha(c('blue','green','yellow'), n=n.crit.ac+1, alpha=0.3)[1:n.crit.ac]
  
  ### PLOT IT OUT ###

  
  if(all == TRUE) {  #Plotting all age classes
    if(side == 'west') {  #WESTSIDE
      if(pdf == TRUE) { pdf(file='WestSide Figs/WestSide AgeComp Coord COMBINED - ALL.pdf', height=7, width=7)	} 
    } else {  #EASTSIDE
  	  if(pdf == TRUE) { pdf(file='EastSide Figs/EastSide AgeComp Coord COMBINED - ALL.pdf', height=7, width=7)	}
    }
    
    #Set up plotting parameters
    par(mfrow=c(2,2), mar=c(2,2,2,1), oma=c(4,4,1,1))

  	#CATCHES
    d <- 1
    for(d in 1:n.districts) {
  	  ac <- 1
  	  for(ac in 1:n.agecomps) {
  	    if(ac==1) {
  	      x.limit <- c(0,max(obs.catch[d,,],pred.catch[d,,], na.rm=TRUE))
  	      y.limit <- x.limit
  	      plot(x=obs.catch[d,ac,], y=pred.catch[d,ac,], type='p', pch=16, xlab='', ylab='', main='', xlim=x.limit, ylim=y.limit, col=catch.cols.all[ac], cex=sz.pts[1])
  	      lines(x=c(0,1), y=c(0,1), lty=2)
  	      mtext(paste(names.districts[d], 'Dist. Catch'), outer=FALSE, side=3, line=0.25, font=2)
  	    }else {
  	      points(x=obs.catch[d,ac,], y=pred.catch[d,ac,], pch=16, col=catch.cols.all[ac], cex=sz.pts[1])
  	    }
  	  }#next ac
  	  mtext('Observed Age Proportions', side=1, outer=TRUE, line=2)
  	  mtext('Predicted Age Proportions', side=2, outer=TRUE, line=2)
    }#next d
      
    #ESCAPEMENTS
    s <- 1
    for(s in 1:n.stocks) {
  	  ac <- 1
  	  for(ac in 1:n.agecomps) {
  	    if(ac==1) {
  	      x.limit <- c(0,max(obs.esc[s,,],pred.esc[s,,], na.rm=TRUE))
  	      y.limit <- x.limit
  	      plot(x=obs.esc[s,ac,], y=pred.esc[s,ac,], type='p', pch=16, xlab='', ylab='', main='', xlim=x.limit, ylim=y.limit, col=esc.cols.all[ac], cex=sz.pts[1])
  	      lines(x=c(0,1), y=c(0,1), lty=2)
  	      mtext(paste(names.stocks[s], 'Esc.'), outer=FALSE, side=3, line=0.25, font=2)
  	    }else {
  	      points(x=obs.esc[s,ac,], y=pred.esc[s,ac,], pch=16, col=esc.cols.all[ac], cex=sz.pts[1])
  	    }
  	  }#next ac
  	  mtext('Observed Age Proportions', side=1, outer=TRUE, line=2)
  	  mtext('Predicted Age Proportions', side=2, outer=TRUE, line=2)
  	}#next s

  
  } else {  #Only plotting main six age classes: 1.1, 1.2, 1.3, 2.1, 2.2, 2.3
    if(side == 'west') {  #WESTSIDE
      if(pdf == TRUE) { pdf(file='WestSide Figs/WestSide AgeComp Coord COMBINED - CRIT.pdf', height=7, width=7)	} 
    } else {  #EASTSIDE
  	  if(pdf == TRUE) { pdf(file='EastSide Figs/EastSide AgeComp Coord COMBINED - CRIT.pdf', height=7, width=7)	}
    }
    
    par(mfrow=c(2,2), mar=c(2,2,2,1), oma=c(4,4,1,1))	
    #CATCHES
    d <- 1
    for(d in 1:n.districts) {
  	  ac <- 1
  	  for(ac in 1:n.crit.ac) {
  	    if(ac==1) {
  	      x.limit <- c(0,max(obs.catch[d,,],pred.catch[d,,], na.rm=TRUE))
  	      y.limit <- x.limit
  	      plot(x=obs.catch[d,crit.ac[ac],], y=pred.catch[d,crit.ac[ac],], type='p', pch=16, xlab='', ylab='', main='', xlim=x.limit, ylim=y.limit, col=catch.cols.all[ac], cex=sz.pts[1])
  	      lines(x=c(0,1), y=c(0,1), lty=2)
  	      mtext(paste(names.districts[d], 'Dist. Catch'), outer=FALSE, side=3, line=0.25, font=2)
  	    }else {
  	      points(x=obs.catch[d,crit.ac[ac],], y=pred.catch[d,crit.ac[ac],], pch=16, col=catch.cols.all[ac], cex=sz.pts[1])
  	    }
  	  }#next ac
  	  mtext('Observed Age Proportions', side=1, outer=TRUE, line=2)
  	  mtext('Predicted Age Proportions', side=2, outer=TRUE, line=2)
    }#next d
      
    #ESCAPEMENTS
    s <- 1
    for(s in 1:n.stocks) {
  	  ac <- 1
  	  for(ac in 1:n.crit.ac) {
  	    if(ac==1) {
  	      x.limit <- c(0,max(obs.esc[s,,],pred.esc[s,,], na.rm=TRUE))
  	      y.limit <- x.limit
  	      plot(x=obs.esc[s,crit.ac[ac],], y=pred.esc[s,crit.ac[ac],], type='p', pch=16, xlab='', ylab='', main='', xlim=x.limit, ylim=y.limit, col=esc.cols.all[ac], cex=sz.pts[1])
  	      lines(x=c(0,1), y=c(0,1), lty=2)
  	      mtext(paste(names.stocks[s], 'Esc.'), outer=FALSE, side=3, line=0.25, font=2)
  	    }else {
  	      points(x=obs.esc[s,crit.ac[ac],], y=pred.esc[s,crit.ac[ac],], pch=16, col=esc.cols.all[ac], cex=sz.pts[1])
  	    }
  	  }#next ac
  	  mtext('Observed Age Proportions', side=1, outer=TRUE, line=2)
  	  mtext('Predicted Age Proportions', side=2, outer=TRUE, line=2)
  	}#next s
  }
  if(pdf == TRUE) { dev.off() }
  
  if(pdf==TRUE & side=='east') {
  	if(all==TRUE) {
  	  pdf(file='EastSide Figs/EastSide AgeComp Coord COMBINED together - ALL.pdf', height=8, width=6)
    }else {
      pdf(file='EastSide Figs/EastSide AgeComp Coord COMBINED together - CRIT.pdf', height=8, width=6)
    }
  	par(mfrow=c(4,2), mar=c(2,2,2,1), oma=c(4,4,1,1))
  	
  	if(all==TRUE) {
  	  #CATCHES
      d <- 1
      for(d in 1:n.districts) {
  	    ac <- 1
  	    for(ac in 1:n.agecomps) {
  	      if(ac==1) {
  	        x.limit <- c(0,max(obs.catch[d,,],pred.catch[d,,], na.rm=TRUE))
  	        y.limit <- x.limit
  	        plot(x=obs.catch[d,ac,], y=pred.catch[d,ac,], type='p', pch=16, xlab='', ylab='', main='', xlim=x.limit, ylim=y.limit, col=catch.cols.all[ac], cex=sz.pts[1])
  	        lines(x=c(0,1), y=c(0,1), lty=2)
  	        mtext(paste(names.districts[d], 'Dist. Catch'), outer=FALSE, side=3, line=0.25, font=2)
  	      }else {
  	        points(x=obs.catch[d,ac,], y=pred.catch[d,ac,], pch=16, col=catch.cols.all[ac], cex=sz.pts[1])
  	      }
  	    }#next ac
  	    mtext('Observed Age Proportions', side=1, outer=TRUE, line=2)
  	    mtext('Predicted Age Proportions', side=2, outer=TRUE, line=2)
      }#next d
      
      #ESCAPEMENTS
      s <- 1
      for(s in 1:n.stocks) {
  	    ac <- 1
  	    for(ac in 1:n.agecomps) {
  	      if(ac==1) {
  	        x.limit <- c(0,max(obs.esc[s,,],pred.esc[s,,], na.rm=TRUE))
  	        y.limit <- x.limit
  	        plot(x=obs.esc[s,ac,], y=pred.esc[s,ac,], type='p', pch=16, xlab='', ylab='', main='', xlim=x.limit, ylim=y.limit, col=esc.cols.all[ac], cex=sz.pts[1])
  	        lines(x=c(0,1), y=c(0,1), lty=2)
  	        mtext(paste(names.stocks[s], 'Esc.'), outer=FALSE, side=3, line=0.25, font=2)
  	      }else {
  	        points(x=obs.esc[s,ac,], y=pred.esc[s,ac,], pch=16, col=esc.cols.all[ac], cex=sz.pts[1])
  	      }
  	    }#next ac
  	    mtext('Observed Age Proportions', side=1, outer=TRUE, line=2)
  	    mtext('Predicted Age Proportions', side=2, outer=TRUE, line=2)
  	  }#next s
  	}else {
  	  #CATCHES
      d <- 1
      for(d in 1:n.districts) {
  	    ac <- 1
  	    for(ac in 1:n.crit.ac) {
  	      if(ac==1) {
  	        x.limit <- c(0,max(obs.catch[d,,],pred.catch[d,,], na.rm=TRUE))
  	        y.limit <- x.limit
  	        plot(x=obs.catch[d,crit.ac[ac],], y=pred.catch[d,crit.ac[ac],], type='p', pch=16, xlab='', ylab='', main='', xlim=x.limit, ylim=y.limit, col=catch.cols.all[ac], cex=sz.pts[1])
  	        lines(x=c(0,1), y=c(0,1), lty=2)
  	        mtext(paste(names.districts[d], 'Dist. Catch'), outer=FALSE, side=3, line=0.25, font=2)
  	      }else {
  	        points(x=obs.catch[d,crit.ac[ac],], y=pred.catch[d,crit.ac[ac],], pch=16, col=catch.cols.all[ac], cex=sz.pts[1])
  	      }
  	    }#next ac
  	    mtext('Observed Age Proportions', side=1, outer=TRUE, line=2)
  	    mtext('Predicted Age Proportions', side=2, outer=TRUE, line=2)
      }#next d
      
      #ESCAPEMENTS
      s <- 1
      for(s in 1:n.stocks) {
  	    ac <- 1
  	    for(ac in 1:n.crit.ac) {
  	      if(ac==1) {
  	        x.limit <- c(0,max(obs.esc[s,,],pred.esc[s,,], na.rm=TRUE))
  	        y.limit <- x.limit
  	        plot(x=obs.esc[s,crit.ac[ac],], y=pred.esc[s,crit.ac[ac],], type='p', pch=16, xlab='', ylab='', main='', xlim=x.limit, ylim=y.limit, col=esc.cols.all[ac], cex=sz.pts[1])
  	        lines(x=c(0,1), y=c(0,1), lty=2)
  	        mtext(paste(names.stocks[s], 'Esc.'), outer=FALSE, side=3, line=0.25, font=2)
  	      }else {
  	        points(x=obs.esc[s,crit.ac[ac],], y=pred.esc[s,crit.ac[ac],], pch=16, col=esc.cols.all[ac], cex=sz.pts[1])
  	      }
  	    }#next ac
  	    mtext('Observed Age Proportions', side=1, outer=TRUE, line=2)
  	    mtext('Predicted Age Proportions', side=2, outer=TRUE, line=2)
  	  }#next s
  	}
  	
  	dev.off()
  }
  
}#end fxn

#plot.agecomp.coord.all(side='east', years=1963:2014, all=TRUE, pdf=TRUE, sz.pts=c(1,2), omit.est=TRUE, wd=wd)
#plot.agecomp.coord.all(side='west', years=1963:2014, all=TRUE, pdf=TRUE, sz.pts=c(1,2), omit.est=TRUE, wd=wd)

####### plot.agecomp.coord.pub ######
# INPUTS: 1) side - "east" or "west"
#         2) years - years for which plots will be generated
#         3) pdf - T/F whether .pdf file will be generated
#         4) sz.pts - vector of sizes of points in plots 1)all 2) critical
#		  5) omit.est - omit fits to estimated age composition data
#
################################

plot.agecomp.coord.pub <- function(side, years, pdf=FALSE, sz.pts, omit.est=TRUE, wd=wd) {
  # side <- 'west'
  # years <- 1963:2014
  # pdf <- FALSE
  # sz.pts <- 1
  # omit.est <- TRUE
  setwd(paste(wd, "/Syrah/outputFiles", sep=""))

  ac.data <- na.omit(read.csv(paste(wd, "/R/ageComp.annual.csv", sep=""), header=TRUE, stringsAsFactors=FALSE)[,-1])
  n.agecomps <- 18
  #Limits
  #space <- 0.2
  #x.limit <- c(0,n.agecomps+(n.agecomps+1)*space)
  
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
        if(nrow(ac.data[ac.data$catch.esc=='catch' & ac.data$year == years[y] & ac.data$district==cat.dist[d],]) == 0 & omit.est == TRUE) {
          pred.catch[d,ac,y] <- NA
          obs.catch[d,ac,y] <- NA	
        } else {
          pred.catch[d,ac,y] <- if(side == 'west') { temp.data$predAgeCompCatch[ac] }else { temp.data$predAgeCompCatch[d,ac] }
          obs.catch[d,ac,y] <- if(side == 'west') { temp.data$obsAgeCompCatch[ac] }else { temp.data$obsAgeCompCatch[d,ac] }	
        }
      }#next d
      s <- 1
      for(s in 1:n.stocks) {
      	if(nrow(ac.data[ac.data$catch.esc=='esc' & ac.data$year == years[y] & ac.data$district==esc.dist[s] & ac.data$stream==esc.stream[s],]) == 0 & omit.est == TRUE) {
      	  pred.esc[s,ac,y] <- NA
      	  obs.esc[s,ac,y] <- NA	
      	}else {
        pred.esc[s,ac,y] <- temp.data$predAgeCompEsc[s,ac]
        obs.esc[s,ac,y] <- temp.data$obsAgeCompEsc[s,ac]
        }
      }#next s
    }#next ac
    
    if(length(which(pred.catch[,,y] > 1))!=0) {print(paste('##### AgeComp ERROR in', year, 'catch', sep=' '))}
    if(length(which(pred.esc[,,y] > 1))!=0) {print(paste('##### AgeComp ERROR in', year, 'escapement', sep=' '))}
  	
    if(y == n.years) { ageComps <- temp.data$AgeCompLabels }  #Rereive ac labels	 
  }#next y
  
  #Location of critical age comps
  crit.ac <- c(6:8,11:13)
  n.crit.ac <- length(crit.ac)
  
  catch.cols <- colorRampAlpha(c('yellow','orange','red','maroon'), n=n.years, alpha=0.5)
  esc.cols  <- colorRampAlpha(c('yellow','green','darkgreen','blue'), n=n.years, alpha=0.5)
  
  if(side == 'west') {  #WESTSIDE
    if(pdf == TRUE) { pdf(file='WestSide Figs/WestSide AgeComp Coord PUBLICATION.pdf', height=8, width=7)	}
    par(mfcol=c(n.crit.ac,4), mar=c(2,2,2,1), oma=c(4,4,2,1)) 
  } else {  #EASTSIDE
  	if(pdf == TRUE) { pdf(file='EastSide Figs/EastSide AgeComp Coord PUBLICATION.pdf', height=8, width=7)	}
  	par(mfcol=c(n.crit.ac,3), mar=c(2,2,2,1), oma=c(4,4,2,1))
  }
    
  	
  #CATCHES
  d <- 1
  for(d in 1:n.districts) {
    ac <- 1
    for(ac in 1:n.crit.ac) {
      x.limit <- c(0,max(obs.catch[d,crit.ac[ac],],pred.catch[d,crit.ac[ac],], na.rm=TRUE))
      y.limit <- x.limit
      plot(x=obs.catch[d,crit.ac[ac],], y=pred.catch[d,crit.ac[ac],], type='p', pch=16, xlab='', ylab='', main='', xlim=x.limit, ylim=y.limit, col=catch.cols, cex=sz.pts[1])
      lines(x=c(0,1), y=c(0,1), lty=2)
      mtext(ageComps[crit.ac[ac]], outer=FALSE, side=3, line=0.25, font=1)
      if(ac==1) { mtext(paste(names.districts[d], 'Dist. Catch'), side=3, line=1.75, font=2) }
    }#next ac
  }#next d
  mtext('Observed Age Proportions', side=1, outer=TRUE, line=2)
  mtext('Predicted Age Proportions', side=2, outer=TRUE, line=2)
  
  if(side=='east') { par(mfcol=c(n.crit.ac,5), mar=c(2,2,2,1), oma=c(4,4,1,1)) }
  
  #ESCAPEMENTS
  s <- 1
  for(s in 1:n.stocks) {
  	ac <- 1
  	for(ac in 1:n.crit.ac) {
  	  x.limit <- c(0,max(obs.esc[s,crit.ac[ac],],pred.esc[s,crit.ac[ac],], na.rm=TRUE))
  	  y.limit <- x.limit
  	  plot(x=obs.esc[s,crit.ac[ac],], y=pred.esc[s,crit.ac[ac],], type='p', pch=16, xlab='', ylab='', main='', xlim=x.limit, ylim=y.limit, col=esc.cols, cex=sz.pts[1])
  	  lines(x=c(0,1), y=c(0,1), lty=2)
  	  mtext(ageComps[crit.ac[ac]], outer=FALSE, side=3, line=0.25)
  	  if(ac==1) { mtext(paste(names.stocks[s], 'Esc.'), side=3, line=1.75, font=2) }
  	}#next ac
  }#next s
  mtext('Observed Age Proportions', side=1, outer=TRUE, line=2)
  mtext('Predicted Age Proportions', side=2, outer=TRUE, line=2)
  	
  if(pdf==TRUE) { dev.off() }
}

#plot.agecomp.coord.pub(side='west', years=1963:2014, pdf=TRUE, sz.pts=1.25, omit.est=TRUE, wd=wd)
#plot.agecomp.coord.pub(side='east', years=1963:2014, pdf=TRUE, sz.pts=1.25, omit.est=TRUE, wd=wd)

####### plot.catch.esc.sigma ######
# INPUTS: 1) side - "east" or "west"
#         2) years - years for which plots will be generated
#         3) pdf - T/F whether .pdf file will be generated
#         4) sz.pts - vector of sizes of points in plots 1)all 2) critical
#		  5) omit.est - omit fits to estimated age composition data
#
################################

plot.catch.esc.sigma <- function(side, years, pdf, wd=wd) {
  # side <- 'west'
  # years <- 1963:2014
  # pdf <- FALSE

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
  
  setwd(paste(wd, "/Syrah/outputFiles", sep=""))

  n.years <- length(years)
  
  sigma.cat <- vector(length=n.years)
  sigma.esc <- vector(length=n.years)
  
  #READ IN THE DATA
  y <- 1
  for(y in 1:n.years) {
    year <- years[y]
  	
    if(side == 'west') {
      temp.data <- readList(paste('WestSide/WestSide_',year,'.out',sep='')) 
  	}else {
  	  temp.data <- readList(paste('EastSide/EastSide_',year,'.out',sep=''))	
  	}
  	
  	sigma.cat[y] <- temp.data$sigmaCat
  	sigma.esc[y] <- temp.data$sigmaEsc
  		 
  }#next y
  
  if(side == 'west') {  #WESTSIDE
    if(pdf == TRUE) { pdf(file='WestSide Figs/WestSide OE Sigma.pdf', height=6, width=7)	}
  } else {  #EASTSIDE
  	if(pdf == TRUE) { pdf(file='EastSide Figs/EastSide OE Sigma.pdf', height=6, width=7)	}
  }
  
  par(mfrow=c(1,1), oma=rep(0,4), mar=c(4,4,3,1))
  plot(x=years, y=sigma.cat, type='l', col=rgb(1,0,0,alpha=0.5), ylim=c(0, max(sigma.cat,sigma.esc)), xlab='Year', ylab='Obs. Sigma')
  points(x=years, y=sigma.cat, pch=21, bg=rgb(1,0,0,alpha=0.5))
  lines(x=years, y=sigma.esc, col=rgb(0,0,1,alpha=0.5))
  points(x=years, y=sigma.esc, pch=21, bg=rgb(0,0,1,alpha=0.5))
  axis(side=1, at=years, labels=FALSE)
  
  if(side=='west') {
    mtext('West Side Bristol Bay', side=3, line=1, font=2)
  }else {
    mtext('East Side Bristol Bay', side=3, line=1, font=2)  	
  }
  
  if(pdf==TRUE) { dev.off() }
}
#plot.catch.esc.sigma(side='west', years=1963:2014, pdf=TRUE, wd=wd)
#plot.catch.esc.sigma(side='east', years=1963:2014, pdf=TRUE, wd=wd)


##############################################################################################################


# ##### WestSide #####
#plot.side <- 'west'
#PRELIM
#plot.years <- c(1965,1977,1980,1982,1983,1985,1993,1995,1999,2006:2011) #All years with genetics
#MAIN
#plot.years <- c(1963:1964,1966:1976,1978:1979,1981,1984,1986:1992,1994,1996:1998,2000:2003,2004,2005)
#plot.years <- 2006:2011
#plot.years <- 1963:2013

##### Eastside #####

#plot.side <- 'west'
#PRELIM
#plot.years <- c(1964,1965,1983,1985,1993,1995,1999,2002,2006:2011)
#MAIN
#plot.years <- c(1963,1966:1982,1984,1986:1992,1994,1996:1998,2000:2001,2003:2005)
#plot.years <- c(1990,2006:2011)
#plot.years <- 1963:2014

#Point size
#PRELIM
#cross <- 3
#MAIN
#cross <- 1.25

# wd <- "/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual"
############################
#  plot.years <- 1963:2017
#  plot.side <- 'east'
#  cross <- 1.25

plot.all <- function(plot.years, plot.side, cross, wd=wd) {
  
  plot.annual.catch.esc(side=plot.side, years=plot.years, pdf=TRUE, text.cex=0.6, cross.cex=cross, wd=wd)
  plot.annual.agecomp(side=plot.side, years=plot.years, pdf=TRUE, input.cex=2, plot.page=4, wd=wd)
  plot.agecomp.coord(side=plot.side, years=plot.years, all=FALSE, pdf=TRUE, sz.pts=c(1,2), omit.est=TRUE, wd=wd)
  plot.agecomp.coord(side=plot.side, years=plot.years, all=TRUE, pdf=TRUE, sz.pts=c(1,2), omit.est=TRUE, wd=wd)
  plot.annual.genComp(side=plot.side, years=plot.years, pdf=TRUE, text.cex.left=1, text.cex.right=0.75, wd=wd)
  plot.maxGradient(side=plot.side, years=plot.years, pdf=TRUE, wd=wd)
  plot.avail.sel.time(side=plot.side, years=plot.years, pdf=TRUE, plot.avail=TRUE, plot.sel=TRUE, plot.fmort=TRUE, plot.hist=TRUE, wd=wd)
  plot.apportioned.catch(side=plot.side, years=plot.years, pdf=TRUE, separate=FALSE, write.nush.data=TRUE, write.table=TRUE, wd=wd)
  #New Agecomp
  plot.agecomp.coord.all(side=plot.side, years=plot.years, all=TRUE, pdf=TRUE, sz.pts=c(1,2), omit.est=TRUE, wd=wd)
  plot.agecomp.coord.pub(side=plot.side, years=plot.years, pdf=TRUE, sz.pts=1.25, omit.est=TRUE, wd=wd)
  plot.catch.esc.sigma(side=plot.side, years=plot.years, pdf=TRUE, wd=wd)
}
#plot.all(plot.years=plot.years, plot.side=plot.side, cross=cross, wd=wd)

create.all <- function(plot.years, plot.side, wd=wd) {
  ###############################################################################################################
  #NOTE: This section cannot be run until both West and East side have results
  create.brood(side=plot.side, years=plot.years, reallocate=TRUE, allocateOffshore=TRUE, renorm.obs.err=TRUE, wd=wd)
  calc.avg.avail.sel(side=plot.side, years=plot.years, writeCsv=TRUE, wd=wd)
  create.total.run.table(side=plot.side, years=plot.years, writeCSV=TRUE, wd=wd)
  ##############################################################################################################
  create.annual.summary(year=max(plot.years), side=plot.side, wd=wd)
  #fit.stock.recruit(pdf=FALSE)
}
#create.all(plot.years=plot.years, plot.side=plot.side, wd=wd)



