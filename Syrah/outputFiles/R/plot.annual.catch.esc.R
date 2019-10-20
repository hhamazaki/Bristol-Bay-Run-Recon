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
    # print(year)
    
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