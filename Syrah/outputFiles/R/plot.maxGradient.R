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