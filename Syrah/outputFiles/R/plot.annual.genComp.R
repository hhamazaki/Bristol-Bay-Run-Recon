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
	fdir <- 'WestSide/WestSide_'
	pdf.fn <- 'WestSide Figs/WestSide Genetic Comp.pdf'
  }
  if(side == 'east') {
    n.districts <- 3
    n.stocks <- 5
    names.districts <- c('Naknek-Kvichak','Egegik','Ugashik')
    names.stocks <- c('Kvichak','Alagnak','Naknek','Egegik','Ugashik')	
	fdir <- 'EastSide/EastSide_'
	pdf.fn <- 'EastSide Figs/EastSide Genetic Comp.pdf'
  }
  if(side != 'west' & side != 'east') { print('##### ERROR side selection is incorrect'); stop(); }
  
  #Retreive Data - GENETIC COMPOSITION OF CATCH
  predGenComp <- array(dim=c(n.districts, n.stocks, n.years))
  obsGenComp <- array(dim=c(n.districts, n.stocks, n.years))
  genSS <- matrix(nrow=n.districts, ncol=n.years)
  
  y <- 1
  for(y in 1:n.years) {
    year <- years[y]
    
    temp.data <- readList(paste(fdir,year,'.out',sep='')) 

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
  
  if(pdf == TRUE) {pdf(file=pdf.fn , height=6, width=8)}
 
  
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