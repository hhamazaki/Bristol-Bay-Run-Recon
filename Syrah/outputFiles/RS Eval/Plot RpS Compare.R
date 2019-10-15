#**************************************************************************
#Project Name: SYRAH ANNUAL - Simply plotting differences between model-predicted R/S and ADFG standard
#Creator: Curry James Cunningham, SAFS, University of Washington
#Date: 10.27.12
#
#Purpose: Read RpS Comparison.csv - plot accordingly
#
#NOTE: At some point I should just have the adfg R/S data file and compare it to most recent brood table outputs. But I'll save that for later.
#**************************************************************************
setwd("/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah/outputFiles/RS Eval")

data <- read.csv('RpS Comparison.csv',header=TRUE, stringsAsFactors=FALSE)

systems <- unique(data$system)
n.systems <- length(systems)

years <- sort(unique(data$year))
n.years <- length(years)

#ALL STOCKS
pdf('All Systems.pdf', height=6, width=8)

space <- 0.2
y.limit <- c(0,n.years+(n.years+1)*space)

par(mfrow=c(1,n.systems), mar=c(4,1,4,0), oma=c(5,5,0,5))

cols <- topo.colors(n.systems)


#x.limit <- c(min(data$pct.diff)*100,max(data$pct.diff)*100)

s <- 1
for(s in 1:n.systems) {
  system <- systems[s]
  
  x.pos <- barplot(data$pct.diff[data$system==system]*100, las=2, horiz=TRUE, main=system, col=cols[s], ylim=y.limit, space=space)
  abline(v=pretty(data$pct.diff[data$system==system]*100), lty=3, col='black')
  abline(v=0, lwd=3)
  abline(v=mean(data$pct.diff[data$system==system]*100), col='red', lwd=2)
  print(paste('Average Productivity Deviation',system,'River:',mean(data$pct.diff[data$system==system]*100),"%",sep=' '))
}#next s
axis(2, at=x.pos, labels=years, las=2, outer=TRUE, line=1)
axis(4, at=x.pos, labels=years, las=2, outer=TRUE, line=1)
mtext('Percent Difference in R/S Estimate from ADFG Brood Table\n(By Brood Year)', side=1, outer=TRUE, line=2)
dev.off()

#WEST SIDE
west.systems <- c('Igushik', 'Wood', 'Nushagak')
n.west.systems <- length(west.systems)

pdf('WestSide.pdf', height=6, width=8)

space <- 0.2
limit <- c(0,n.years+(n.years+1)*space)

par(mfrow=c(1,n.west.systems), mar=c(4,1,4,0), oma=c(5,5,0,5))

cols <- topo.colors(n.west.systems)

s <- 1
for(s in 1:n.west.systems) {
  system <- west.systems[s]
  
  barplot(data$pct.diff[data$system==system]*100, las=2, horiz=TRUE, main=system, col=cols[s], ylim=limit, space=space)
  abline(v=pretty(data$pct.diff[data$system==system]*100), lty=3, col='black')
  abline(v=0, lwd=3)
  abline(v=mean(data$pct.diff[data$system==system]*100), col='red', lwd=2)
}#next s
axis(2, at=x.pos, labels=years, las=2, outer=TRUE, line=1)
axis(4, at=x.pos, labels=years, las=2, outer=TRUE, line=1)
mtext('Percent Difference in R/S Estimate from ADFG Brood Table\n(By Brood Year)', side=1, outer=TRUE, line=2)

dev.off()


#EAST SIDE
east.systems <- c('Kvichak','Alagnak','Naknek','Egegik','Ugashik')
n.east.systems <- length(east.systems)

pdf('EastSide.pdf', height=6, width=8)

space <- 0.2
limit <- c(0,n.years+(n.years+1)*space)

par(mfrow=c(1,n.east.systems), mar=c(4,1,4,0), oma=c(5,5,0,5))

cols <- topo.colors(n.east.systems)
cols[2] <- 'red'

s <- 1
for(s in 1:n.east.systems) {
  system <- east.systems[s]
  
  barplot(data$pct.diff[data$system==system]*100, las=2, horiz=TRUE, main=system, col=cols[s], ylim=limit, space=space)
  abline(v=pretty(data$pct.diff[data$system==system]*100), lty=3, col='black')
  abline(v=0, lwd=3)
  abline(v=mean(data$pct.diff[data$system==system]*100), col='red', lwd=2)
}#next s
axis(2, at=x.pos, labels=years, las=2, outer=TRUE, line=1)
axis(4, at=x.pos, labels=years, las=2, outer=TRUE, line=1)
mtext('Percent Difference in R/S Estimate from ADFG Brood Table\n(By Brood Year)', side=1, outer=TRUE, line=2)

dev.off()

#**************************************************************************
#ALL STOCKS
pdf('All Systems_extra.pdf', height=6, width=8)

space <- 0.2
y.limit <- c(0,n.years+(n.years+1)*space)
par(mar=c(2,1,2,0), oma=c(5,5,0,5))
mat <- matrix(c(1:n.systems,
				1:n.systems,
				1:n.systems,
				1:n.systems,
				(n.systems+1):(2*n.systems)), byrow=TRUE, ncol=n.systems)
layout(mat)				
				
cols <- topo.colors(n.systems)

#x.limit <- c(min(data$pct.diff)*100,max(data$pct.diff)*100)

s <- 1
for(s in 1:n.systems) {
  system <- systems[s]
  
  x.pos <- barplot(data$pct.diff[data$system==system]*100, las=2, horiz=TRUE, main=system, col=cols[s], ylim=y.limit, space=space)
  abline(v=pretty(data$pct.diff[data$system==system]*100), lty=3, col='black')
  abline(v=0, lwd=3)
  abline(v=mean(data$pct.diff[data$system==system]*100), col='red', lwd=2)
  print(paste('Average Productivity Deviation',system,'River:',mean(data$pct.diff[data$system==system]*100),"%",sep=' '))
}#next s
yr.pty <- pretty(years)
loc.pty <- which(years%in%yr.pty)
axis(2, at=x.pos[loc.pty], labels=years[loc.pty], las=2, outer=TRUE, line=1)
axis(2, at=x.pos, labels=FALSE, las=2, outer=TRUE, line=1)
axis(4, at=x.pos[loc.pty], labels=years[loc.pty], las=2, outer=TRUE, line=1)
axis(4, at=x.pos, labels=FALSE, las=2, outer=TRUE, line=1)
mtext('Percent Difference in R/S Estimate from ADFG Brood Table\n(By Brood Year)', side=1, outer=TRUE, line=2.5)

#Histogram
s <- 1
for(s in 1:n.systems) {
  system <- systems[s]
  hist(data$pct.diff[data$system==system]*100, breaks=15, main='', col=cols[s], xlab='', yaxt='n', ylab='')
  abline(v=0, lwd=3)
  abline(v=mean(data$pct.diff[data$system==system]*100), col='red', lwd=2)
}

dev.off()

#WEST SIDE
west.systems <- c('Igushik', 'Wood', 'Nushagak')
n.west.systems <- length(west.systems)

pdf('WestSide_extra.pdf', height=6, width=8)

space <- 0.2
limit <- c(0,n.years+(n.years+1)*space)

par(mar=c(2,1,2,0), oma=c(5,5,0,5))
mat <- matrix(c(1:n.west.systems,
				1:n.west.systems,
				1:n.west.systems,
				1:n.west.systems,
				(n.west.systems+1):(2*n.west.systems)), byrow=TRUE, ncol=n.west.systems)
layout(mat)		
cols <- topo.colors(n.west.systems)

s <- 1
for(s in 1:n.west.systems) {
  system <- west.systems[s]
  x.lim <- c(min(data$pct.diff[data$system==system]*100),max(data$pct.diff[data$system==system]*100))
  barplot(data$pct.diff[data$system==system]*100, las=2, horiz=TRUE, main=system, col=cols[s], ylim=limit, space=space, xlim=x.lim)
  abline(v=pretty(data$pct.diff[data$system==system]*100), lty=3, col='black')
  abline(v=0, lwd=3)
  abline(v=mean(data$pct.diff[data$system==system]*100), col='red', lwd=2)
}#next s
axis(2, at=x.pos[loc.pty], labels=years[loc.pty], las=2, outer=TRUE, line=1)
axis(2, at=x.pos, labels=FALSE, las=2, outer=TRUE, line=1)
axis(4, at=x.pos[loc.pty], labels=years[loc.pty], las=2, outer=TRUE, line=1)
axis(4, at=x.pos, labels=FALSE, las=2, outer=TRUE, line=1)
mtext('Percent Difference in R/S Estimate from ADFG Brood Table\n(By Brood Year)', side=1, outer=TRUE, line=2.5)
#Histogram
s <- 1
for(s in 1:n.west.systems) {
  system <- west.systems[s]
  x.lim <- c(min(data$pct.diff[data$system==system]*100),max(data$pct.diff[data$system==system]*100))
  hist(data$pct.diff[data$system==system]*100, breaks=15, main='', col=cols[s], xlab='', yaxt='n', ylab='', xlim=x.lim)
  abline(v=0, lwd=3)
  abline(v=mean(data$pct.diff[data$system==system]*100), col='red', lwd=2)
}


s <- 1
for(s in 1:n.west.systems) {
  system <- west.systems[s]
  x.lim <- c(min(data$pct.diff[data$system==system]*100),max(data$pct.diff[data$system==system]*100))
  # barplot(data$pct.diff[data$system==system]*100, las=2, horiz=TRUE, main=system, col=cols[s], ylim=limit, space=space, xlim=x.lim)
  barplot(data$pct.diff[data$system==system]*100, las=2, horiz=TRUE, main=system, col=rgb(1,0,0, alpha=0), ylim=limit, space=space, xlim=x.lim)
  abline(v=pretty(data$pct.diff[data$system==system]*100), lty=3, col='black')
  abline(v=0, lwd=3)
  #abline(v=mean(data$pct.diff[data$system==system]*100), col='red', lwd=2)
  polygon(x=c(min(x.lim),0,0,min(x.lim)), y=c(min(x.pos)-1,min(x.pos)-1,max(x.pos)+1,max(x.pos)+1), col=rgb(1,0,0, alpha=0.25), border=NA)
  polygon(x=c(max(x.lim),0,0,max(x.lim)), y=c(min(x.pos)-1,min(x.pos)-1,max(x.pos)+1,max(x.pos)+1), col=rgb(0,0,1, alpha=0.25), border=NA)
}#next s
axis(2, at=x.pos[loc.pty], labels=years[loc.pty], las=2, outer=TRUE, line=1)
axis(2, at=x.pos, labels=FALSE, las=2, outer=TRUE, line=1)
axis(4, at=x.pos[loc.pty], labels=years[loc.pty], las=2, outer=TRUE, line=1)
axis(4, at=x.pos, labels=FALSE, las=2, outer=TRUE, line=1)
mtext('Percent Difference in R/S Estimate from ADFG Brood Table\n(By Brood Year)', side=1, outer=TRUE, line=2.5)
#Histogram
s <- 1
for(s in 1:n.west.systems) {
  system <- west.systems[s]
  x.lim <- c(min(data$pct.diff[data$system==system]*100),max(data$pct.diff[data$system==system]*100))
  h <- hist(data$pct.diff[data$system==system]*100, breaks=15, main='', col=rgb(1,0,0, alpha=0), xlab='', yaxt='n', ylab='', xlim=x.lim)
  abline(v=0, lwd=3)
  #abline(v=mean(data$pct.diff[data$system==system]*100), col='red', lwd=2)
  polygon(x=c(min(x.lim),0,0,min(x.lim)), y=c(0,0,max(h$counts)+1,max(h$counts)+1), col=rgb(1,0,0, alpha=0.25), border=NA)
  polygon(x=c(max(x.lim),0,0,max(x.lim)), y=c(0,0,max(h$counts)+1,max(h$counts)+1), col=rgb(0,0,1, alpha=0.25), border=NA)
}





dev.off()


#EAST SIDE
east.systems <- c('Kvichak','Alagnak','Naknek','Egegik','Ugashik')
n.east.systems <- length(east.systems)

pdf('EastSide_extra.pdf', height=6, width=8)

space <- 0.2
limit <- c(0,n.years+(n.years+1)*space)

par(mar=c(2,1,2,0), oma=c(5,5,0,5))
mat <- matrix(c(1: n.east.systems,
				1: n.east.systems,
				1: n.east.systems,
				1: n.east.systems,
				(n.east.systems +1):(2* n.east.systems)), byrow=TRUE, ncol= n.east.systems)
layout(mat)		
cols <- topo.colors(n.east.systems)

s <- 1
for(s in 1:n.east.systems) {
  system <- east.systems[s]
  x.lim <- c(min(data$pct.diff[data$system==system]*100),max(data$pct.diff[data$system==system]*100))
  barplot(data$pct.diff[data$system==system]*100, las=2, horiz=TRUE, main=system, col=cols[s], ylim=limit, space=space, xlim=x.lim)
  abline(v=pretty(data$pct.diff[data$system==system]*100), lty=3, col='black')
  abline(v=0, lwd=3)
  abline(v=mean(data$pct.diff[data$system==system]*100), col='red', lwd=2)
}#next s
axis(2, at=x.pos[loc.pty], labels=years[loc.pty], las=2, outer=TRUE, line=1)
axis(2, at=x.pos, labels=FALSE, las=2, outer=TRUE, line=1)
axis(4, at=x.pos[loc.pty], labels=years[loc.pty], las=2, outer=TRUE, line=1)
axis(4, at=x.pos, labels=FALSE, las=2, outer=TRUE, line=1)
mtext('Percent Difference in R/S Estimate from ADFG Brood Table\n(By Brood Year)', side=1, outer=TRUE, line=2.5)
#Histogram
s <- 1
for(s in 1:n.east.systems) {
  system <- east.systems[s]
  x.lim <- c(min(data$pct.diff[data$system==system]*100),max(data$pct.diff[data$system==system]*100))
  hist(data$pct.diff[data$system==system]*100, breaks=15, main='', col=cols[s], xlab='', yaxt='n', ylab='', xlim=x.lim)
  abline(v=0, lwd=3)
  abline(v=mean(data$pct.diff[data$system==system]*100), col='red', lwd=2)
}
dev.off()

