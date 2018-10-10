#************************************************************************************************
#Project Name: SYRAH ANNUAL - Running the complete model
#Creator: Curry James Cunningham, SAFS, University of Washington
#Date: 11.4.13
#
#Purpose: Run the Bristol Bay, Alaska Run Reconstructions in one script.
#
#  1) Create ADMB input files
#  2) Call ADMB for East and West Size
#  3) Plot Output
#  4) Update variance parameters for error distribution
#  5) Re-run ADMB from from previous parameter estimates
#  6) Plot Output and create brood and return tables
#
#*************************************************************************************************
#Notes:
#  A)
#  
#*************************************************************************************************
require(PBSmodelling)
require(R2admb)
setwd("/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah")


##### Source Necessary Files #####
#Creat ADMB input
source("/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/R/Annual Reconstruction.r")
#Plot output
source("/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah/outputFiles/Plot Annual Output.r")
#plot.all(plot.years=plot.years, plot.side=plot.side, cross=cross)
#create.all(plot.years=plot.years, plot.side=plot.side)

#Helper Functions
source("/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah/Syrah Helper Functions.r")
#move(); cleanup(); both take side and year as inputs



#Compile ADMB CODE
#setup_admb("/Applications/ADMB-11 Terminal.app/Contents/admb-11")
Sys.setenv(ADMB_HOME='usr/local/bin/ADMB')
Sys.setenv(PATH='/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin')

system("admb syrah.tpl")

#######################################################################################################
max.year <- 2014
#######################################################################################################
#Plotting parameters
plot.years <- 1963:max.year
cross <- 1.25 
#######################################################################################################
#WEST SIDE
phz.run=1; phz.sel=1; phz.avail=1;

do.west <- FALSE
if(do.west==TRUE) {
  side <- 'west'
  
  #Get Average Availability
  west.prelim <- c(1965,1977,1980,1982,1983,1985,1993,1995,1999,2006:2013)
  #avg.avail <- avg.Avail.prelim(side='west', prelim.years=west.prelim)$avail.avgs
  y <- 1
  for(y in 1:length(west.prelim)) {
  year <- west.prelim[y]
  #Cleanup
  cleanup(side=side, year=year)

  #Create ADMB Input
  create.SYRAH.annual.input(side=side, district.codes=c(325,325,325), stream.codes=c(100,300,700), 
                            stream.district=c(1,1,1), year=year, cat.esc.div=1, 
                            #fixed.avail=avg.avail, #IF NO GENETIC DATA ARE AVAILABLE (March Reconstruction)
                            fixed.avail=rep(-1,3), #IF GENETIC DATA ARE AVAILABLE (June Reconstruction)
                            loc.prefix="/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah/datFiles/",
                            read.outs=FALSE, phz.run=phz.run, phz.sel=phz.sel, phz.avail=phz.avail,
                            temp_sigmaCat=0.5, temp_sigmaEsc=0.5) 
  #Run ADMB 1st Round                          
  setwd("/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah")
  system(paste("./syrah -ind datFiles/WestSide_",year,".dat -rs -nox", sep=''))
  move(side=side, year=year)
  #Plot
  #plot.all(plot.years=plot.years, plot.side=side, cross=cross)

  #Create ADMB Input
  create.SYRAH.annual.input(side=side, district.codes=c(325,325,325), stream.codes=c(100,300,700), 
                            stream.district=c(1,1,1), year=year, cat.esc.div=1, 
                            #fixed.avail=avg.avail, #IF NO GENETIC DATA ARE AVAILABLE (March Reconstruction)
                            fixed.avail=rep(-1,3), #IF GENETIC DATA ARE AVAILABLE (June Reconstruction)
                            loc.prefix="/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah/datFiles/",
                            read.outs=TRUE, phz.run=phz.run, phz.sel=phz.sel, phz.avail=phz.avail,
                            temp_sigmaCat=0.5, temp_sigmaEsc=0.05) 

  #Clear output objects
  cleanup(side=side, year=year)

  #Run ADMB 1st Round                          
  setwd("/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah")
  system(paste("./syrah -ind datFiles/WestSide_",year,".dat -rs -nox", sep=''))
  move(side=side, year=year)
  }#next y
  
  #MAIN YEARS
  avg.avail <- avg.Avail.prelim(side='west', prelim.years=west.prelim)$avail.avgs

  main.years <- plot.years[-which(plot.years%in%west.prelim)]
  y <- 1
  for(y in 1:length(main.years)) {
    year <- main.years[y]
      #Cleanup
  cleanup(side=side, year=year)

  #Create ADMB Input
  create.SYRAH.annual.input(side=side, district.codes=c(325,325,325), stream.codes=c(100,300,700), 
                            stream.district=c(1,1,1), year=year, cat.esc.div=1, 
                            fixed.avail=avg.avail, #IF NO GENETIC DATA ARE AVAILABLE (March Reconstruction)
                            #fixed.avail=rep(-1,3), #IF GENETIC DATA ARE AVAILABLE (June Reconstruction)
                            loc.prefix="/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah/datFiles/",
                            read.outs=FALSE, phz.run=phz.run, phz.sel=phz.sel, phz.avail=phz.avail,
                            temp_sigmaCat=0.5, temp_sigmaEsc=0.5) 
  #Run ADMB 1st Round                          
  setwd("/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah")
  system(paste("./syrah -ind datFiles/WestSide_",year,".dat -rs -nox", sep=''))
  move(side=side, year=year)
  #Plot
  #plot.all(plot.years=plot.years, plot.side=side, cross=cross)

  #Create ADMB Input
  create.SYRAH.annual.input(side=side, district.codes=c(325,325,325), stream.codes=c(100,300,700), 
                            stream.district=c(1,1,1), year=year, cat.esc.div=1, 
                            fixed.avail=avg.avail, #IF NO GENETIC DATA ARE AVAILABLE (March Reconstruction)
                            #fixed.avail=rep(-1,3), #IF GENETIC DATA ARE AVAILABLE (June Reconstruction)
                            loc.prefix="/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah/datFiles/",
                            read.outs=TRUE, phz.run=phz.run, phz.sel=phz.sel, phz.avail=phz.avail,
                            temp_sigmaCat=0.5, temp_sigmaEsc=0.05) 

  #Clear output objects
  cleanup(side=side, year=year)

  #Run ADMB 1st Round                          
  setwd("/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah")
  system(paste("./syrah -ind datFiles/WestSide_",year,".dat -rs -nox", sep=''))
  move(side=side, year=year)
    	
  }#next y
  
  #PLOT FINAL OUTPUT
  if(file.exists(paste("/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah/outputFiles/WestSide/COR/WestSide_", year, ".cor", sep=''))==TRUE) {
    plot.all(plot.years=plot.years, plot.side=side, cross=cross)
  }else {
    print(paste(side, 'SIDE RECONSTRUCTION DID NOT CONVERGE, RECHECK PHASING', sep=' '))	
  }
}

#######################################################################################################
#EAST SIDE
phz.run=1; phz.sel=2; phz.avail=2;

do.east <- TRUE
#if(do.east==TRUE) {
  side <- 'east'
  
  #Get Average Availability
  east.prelim <- c(1964,1965,1983,1985,1993,1995,1999,2002,2006:2013)
  #avg.avail <- avg.Avail.prelim(side='east', prelim.years=east.prelim)$avail.avgs
  y <- 1
  for(y in 1:length(east.prelim)) {
  year <- east.prelim[y]
  #Cleanup
  cleanup(side=side, year=year)

  #Create ADMB Input
  create.SYRAH.annual.input(side=side, district.codes=c(324,324,324,322,321), stream.codes=c(100,500,600,100,100), 
                            stream.district=c(1,1,1,2,3), year=year,  cat.esc.div=1,
                            #fixed.avail=avg.avail,  #IF NO GENETIC DATA ARE AVAILABLE (October/March Reconstruction)
                            fixed.avail=rep(-1,15), #IF GENETIC DATA ARE AVAILABLE (June Reconstruction)
                            loc.prefix="/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah/datFiles/",
                            read.outs=FALSE, phz.run=phz.run, phz.sel=phz.sel, phz.avail=phz.avail,
                            temp_sigmaCat=0.5, temp_sigmaEsc=0.5)  #0.5, 0.5
  #Run ADMB 1st Round                          
  setwd("/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah")
  system(paste("./syrah -ind datFiles/EastSide_",year,".dat -rs -nox", sep=''))
  #system(paste("./syrah -ainp syrah.par -ind datFiles/EastSide_",year,".dat", sep=''))
  move(side=side, year=year)
  #Plot
  #plot.all(plot.years=plot.years, plot.side=side, cross=cross)
  #plot.annual.catch.esc(side=side, years=plot.years, pdf=FALSE, text.cex=0.6, cross.cex=cross)
  #plot.maxGradient(side=side, years=plot.years, pdf=FALSE)
  
  #if(file.exists(paste("/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah/outputFiles/EastSide/COR/EastSide_", year, ".cor", sep=''))==FALSE) {
  #phz.run=1; phz.sel=2; phz.avail=1;

  #Create ADMB Input
  create.SYRAH.annual.input(side=side, district.codes=c(324,324,324,322,321), stream.codes=c(100,500,600,100,100), 
                            stream.district=c(1,1,1,2,3), year=year,  cat.esc.div=1,
                            #fixed.avail=avg.avail,  #IF NO GENETIC DATA ARE AVAILABLE (October/March Reconstruction)
                            fixed.avail=rep(-1,15), #IF GENETIC DATA ARE AVAILABLE (June Reconstruction)
                            loc.prefix="/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah/datFiles/",
                            read.outs=TRUE, phz.run=phz.run, phz.sel=phz.sel, phz.avail=phz.avail,
                            temp_sigmaCat=0.5, temp_sigmaEsc=0.05) #0.5, 0.05

  #Clear output objects
  cleanup(side=side, year=year)

  #Run ADMB 1st Round                          
  setwd("/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah")
  system(paste("./syrah -ind datFiles/EastSide_",year,".dat -rs -nox", sep=''))
  
  #system(paste("./syrah -ainp syrah.par -ind datFiles/EastSide_",year,".dat", sep=''))
  
  move(side=side, year=year)
  }#next y
  
  #RUN MAIN YEARS
  avg.avail <- avg.Avail.prelim(side='east', prelim.years=east.prelim)$avail.avgs
  
  main.years <- plot.years[-which(plot.years%in%east.prelim)]
  y <- 1
  for(y in 1:length(main.years)) {
    year <- main.years[y]
    
    #Cleanup
    cleanup(side=side, year=year)
    
    #Create ADMB Input
    create.SYRAH.annual.input(side=side, district.codes=c(324,324,324,322,321), stream.codes=c(100,500,600,100,100), 
                              stream.district=c(1,1,1,2,3), year=year,  cat.esc.div=1,
                              fixed.avail=avg.avail,  #IF NO GENETIC DATA ARE AVAILABLE (October/March Reconstruction)
                              #fixed.avail=rep(-1,15), #IF GENETIC DATA ARE AVAILABLE (June Reconstruction)
                              loc.prefix="/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah/datFiles/",
                              read.outs=FALSE, phz.run=phz.run, phz.sel=phz.sel, phz.avail=phz.avail,
                              temp_sigmaCat=0.5, temp_sigmaEsc=0.5)  #0.5, 0.5
    #Run ADMB 1st Round                          
    setwd("/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah")
    system(paste("./syrah -ind datFiles/EastSide_",year,".dat -rs -nox", sep=''))
    #system(paste("./syrah -ainp syrah.par -ind datFiles/EastSide_",year,".dat", sep=''))
    move(side=side, year=year)
    #Plot
    #plot.all(plot.years=plot.years, plot.side=side, cross=cross)
    #plot.annual.catch.esc(side=side, years=plot.years, pdf=FALSE, text.cex=0.6, cross.cex=cross)
    #plot.maxGradient(side=side, years=plot.years, pdf=FALSE)
    
    #if(file.exists(paste("/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah/outputFiles/EastSide/COR/EastSide_", year, ".cor", sep=''))==FALSE) {
    #phz.run=1; phz.sel=2; phz.avail=1;
    
    #Create ADMB Input
    create.SYRAH.annual.input(side=side, district.codes=c(324,324,324,322,321), stream.codes=c(100,500,600,100,100), 
                              stream.district=c(1,1,1,2,3), year=year,  cat.esc.div=1,
                              fixed.avail=avg.avail,  #IF NO GENETIC DATA ARE AVAILABLE (October/March Reconstruction)
                              #fixed.avail=rep(-1,15), #IF GENETIC DATA ARE AVAILABLE (June Reconstruction)
                              loc.prefix="/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah/datFiles/",
                              read.outs=TRUE, phz.run=phz.run, phz.sel=phz.sel, phz.avail=phz.avail,
                              temp_sigmaCat=0.5, temp_sigmaEsc=0.05) #0.5, 0.05
    
    #Clear output objects
    cleanup(side=side, year=year)
    
    #Run ADMB 1st Round                          
    setwd("/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah")
    system(paste("./syrah -ind datFiles/EastSide_",year,".dat -rs -nox", sep=''))
    
    #system(paste("./syrah -ainp syrah.par -ind datFiles/EastSide_",year,".dat", sep=''))
    
    move(side=side, year=year)
  }#next y  
  #TEMPORARY
  plot.annual.catch.esc(side=side, years=plot.years, pdf=FALSE, text.cex=0.6, cross.cex=cross)
  #plot.maxGradient(side=side, years=plot.years, pdf=FALSE)
  #}
  #Plot
  if(file.exists(paste("/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah/outputFiles/EastSide/COR/EastSide_", year, ".cor", sep=''))==TRUE) {
    plot.all(plot.years=plot.years, plot.side=side, cross=cross)
  }else {
    print(paste(side, 'SIDE RECONSTRUCTION DID NOT CONVERGE, RECHECK PHASING', sep=' '))	
  }
}

#######################################################################################################
#CREATE BROOD TABLES - SOME SORT 
#  Must be done last as both converged outputs must be ready so offshore can be allocated accordingly
do.broods <- TRUE
if(do.broods==TRUE) {
  west.converge <- file.exists(paste("/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah/outputFiles/WestSide/COR/WestSide_", year, ".cor", sep=''))
  east.converge <- file.exists(paste("/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah/outputFiles/EastSide/COR/EastSide_", year, ".cor", sep=''))
  if(west.converge==TRUE & east.converge==TRUE) {
    create.all(plot.years=plot.years, plot.side='west')
    create.all(plot.years=plot.years, plot.side='east')
    plot.all(plot.years=plot.years, plot.side='west', cross=cross)
    plot.all(plot.years=plot.years, plot.side='east', cross=cross)
  }else {
    print(paste('ERROR: CANNOT CREATE TABLES, CONVERGENCE FOR WestSide: ', west.converge, ' CONVERGENCE FOR EastSide: ', east.converge, sep=''))
  }
}







