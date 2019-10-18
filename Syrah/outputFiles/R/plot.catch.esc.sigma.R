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
  require(PBSmodelling)
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
  legend('left', legend=c("Catch","Escapement"), pch=19, col=c(rgb(1,0,0,alpha=0.5),
                                                              rgb(0,0,1,alpha=0.5)),
         title="Fixed Obs. Error SD")
  
  if(side=='west') {
    mtext('West Side Bristol Bay', side=3, line=1, font=2)
  }else {
    mtext('East Side Bristol Bay', side=3, line=1, font=2)  	
  }
  
  if(pdf==TRUE) { dev.off() }
}
# plot.catch.esc.sigma(side='west', years=1963:2014, pdf=FALSE, wd=wd)
#plot.catch.esc.sigma(side='east', years=1963:2014, pdf=TRUE, wd=wd)