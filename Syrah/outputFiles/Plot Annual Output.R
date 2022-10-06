#**************************************************************************
#Project Name: SYRAH ANNUAL - Evaluate output files
#Creator: Curry James Cunningham, SAFS, University of Washington
#Date: 10.20.18
#
#Purpose: Read output files for ADMB reconstruction and create descriptive plots
#**************************************************************************
require(PBSmodelling)
require(openxlsx)
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

source(file.path(wd,"Syrah", "outputFiles", "R","plot.annual.catch.esc.R"))

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

source(file.path(wd,"Syrah", "outputFiles", "R","plot.annual.agecomp.R"))

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

source(file.path(wd,"Syrah", "outputFiles", "R","plot.agecomp.coord.R"))


##### plot.annual.genComp ####
# INPUTS: 1) side - "east" or "west"
#         2) years - years for which plots will be generated
#         3) pdf - T/F whether .pdf file will be generated
#         4) text.cex.left - size of text on the LEFT side of plots (LARGER)
#         5) text.cex.right - size fo text on the RIGHT side of plots
#
#
################################

source(file.path(wd,"Syrah", "outputFiles", "R","plot.annual.genComp.R"))

####### plot.maxGradient ######
# INPUTS: 1) side - "east" or "west"
#         2) years - years for which plots will be generated
#         3) pdf - T/F whether .pdf file will be generated
#
#
################################

source(file.path(wd,"Syrah", "outputFiles", "R","plot.maxGradient.R"))


####### plot.avail.sel.time ######
# INPUTS: 1) side - "east" or "west"
#         2) years - years for which plots will be generated
#         3) pdf - T/F whether .pdf file will be generated
#		      4) plot.avail - whether to plot availability
#		      5) plot.sel  - whether to plot selectivity over time
#         6) plot.fmort - whether to plot fishing mortality over time
#		      7) plot.hist - whether to plot a histogram of the estimated availability values for each stock in each district
################################

source(file.path(wd,"Syrah", "outputFiles", "R","plot.avail.sel.time.R"))

####### plot.apportioned.catch ######
# INPUTS: 1) side - "east" or "west"
#         2) years - years for which plots will be generated
#         3) pdf - T/F whether .pdf file will be generated
#		      4) separate - T/F whether to put different districts on the same or different pages
################################

source(file.path(wd,"Syrah", "outputFiles", "R","plot.apportioned.catch.R"))

# plot.apportioned.catch(side='west', years=1963:2014, pdf=FALSE, separate=FALSE, write.nush.data=FALSE, write.table=TRUE, wd=wd)
# plot.apportioned.catch(side='east', years=1963:2014, pdf=FALSE, separate=FALSE, write.nush.data=FALSE, write.table=TRUE, wd=wd)

##############################################################################################################
##### HELPER FUNCTIONS #####

######### allocate.offshoreCatch ########
# ALLOCATES ANNUAL OFFSHORE CATCHES IN PROPORTION TO RECONSTRUCTED INSHORE RUNSIZE BY STOCK
#
# INPUTS: 1) years - years for which plots will be generated
################################

source(file.path(wd,"Syrah", "outputFiles", "R","allocate.offshoreCatch.R"))

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

source(file.path(wd,"Syrah", "outputFiles", "R","create.brood.R"))

# create.brood(side='east', years=1963:2016, reallocate=TRUE, allocateOffshore=TRUE, renorm.obs.err=TRUE, wd=wd)

######### calc.avg.avail.sel ########
# INPUTS: 1) side - "east" or "west"
#         2) years - years for which plots will be generated
#         3) writeCsv - T/F whether .csv will be generated
#
#
################################

source(file.path(wd,"Syrah", "outputFiles", "R","calc.avg.avail.sel.R"))

######### create.total.run.table ########
# INPUTS: 1) side - "east" or "west"
#         2) years - years for which plots will be generated
#
################################

source(file.path(wd,"Syrah", "outputFiles", "R","create.total.run.table.R"))

# create.total.run.table(side='west', years=c(1963:2017), writeCSV=TRUE, wd="/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual")
# create.total.run.table(side='east', years=c(1963:2017), writeCSV=TRUE, wd="/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual")

######### fit.stock.recruit ########
# INPUTS: 1) side - "east" or "west"
#         2) stock
#
################################

source(file.path(wd,"Syrah", "outputFiles", "R","fit.stock.recruit.R"))

#fit.stock.recruit(pdf=TRUE, wd=wd)

######### create.annual.summary ########
# INPUTS: 1) year
#
################################

source(file.path(wd,"Syrah", "outputFiles", "R", "create.annual.summary.R"))

# create.annual.summary(year=2018, side='west', wd=wd)

####### plot.agecomp.coord.all ######
# INPUTS: 1) side - "east" or "west"
#         2) years - years for which plots will be generated
#         3) all - whether all age classes should be plotted (if FALSE will plot 1.1 1.2 1.3 2.1 2.2 2.3)
#         4) pdf - T/F whether .pdf file will be generated
#         5) sz.pts - vector of sizes of points in plots 1)all 2) critical
#		  6) omit.est - omit fits to estimated age composition data
#
################################

source(file.path(wd,"Syrah", "outputFiles", "R", "plot.agecomp.coord.all.R"))

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

source(file.path(wd,"Syrah", "outputFiles", "R", "plot.agecomp.coord.pub.R"))

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

source(file.path(wd,"Syrah", "outputFiles", "R", "plot.catch.esc.sigma.R"))

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
#TESTING
 # plot.years <- 1963:2021
 # plot.side <- 'east'
 # cross <- 1.25


plot.all <- function(plot.years, plot.side, cross, wd=wd,pdf=FALSE) {
  
  plot.annual.catch.esc(side=plot.side, years=plot.years, pdf=pdf, text.cex=0.6, cross.cex=cross, wd=wd)
  plot.annual.agecomp(side=plot.side, years=plot.years, pdf=pdf, input.cex=2, plot.page=4, wd=wd)
  plot.agecomp.coord(side=plot.side, years=plot.years, all=FALSE, pdf=pdf, sz.pts=c(1,2), omit.est=TRUE, wd=wd)
  plot.annual.genComp(side=plot.side, years=plot.years, pdf=pdf, text.cex.left=1, text.cex.right=0.75, wd=wd)
  plot.maxGradient(side=plot.side, years=plot.years, pdf=pdf, wd=wd)
  plot.avail.sel.time(side=plot.side, years=plot.years, pdf=pdf, plot.avail=TRUE, plot.sel=TRUE, plot.fmort=TRUE, plot.hist=TRUE, wd=wd)
  plot.apportioned.catch(side=plot.side, years=plot.years, pdf=pdf, separate=FALSE, write.nush.data=TRUE, write.table=TRUE, wd=wd)
  #New Agecomp
  plot.agecomp.coord.all(side=plot.side, years=plot.years, all=TRUE, pdf=pdf, sz.pts=c(1,2), omit.est=TRUE, wd=wd)
  plot.agecomp.coord.pub(side=plot.side, years=plot.years, pdf=pdf, sz.pts=1.25, omit.est=TRUE, wd=wd)
  plot.catch.esc.sigma(side=plot.side, years=plot.years, pdf=pdf, wd=wd)
}

# plot.all(plot.years=plot.years, plot.side=plot.side, cross=cross, wd=wd)

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

# create.all(plot.years=plot.years, plot.side=plot.side, wd=wd)