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