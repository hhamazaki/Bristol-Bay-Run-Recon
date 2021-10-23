#************************************************************************************************
#Project Name: SYRAH ANNUAL - Helper Functions for File Movement and Removal
#Creator: Curry James Cunningham, SAFS, University of Washington
#Date: 11.4.13
#
#Purpose: To Move and Remove syrah output files
#
#
#*************************************************************************************************
#Notes:
#  A)
#  
#*************************************************************************************************
#Remove old files
cleanup <- function(side='west', year=2013, wd=wd) {
  #General Files
  file.remove(paste(wd, "/Syrah/syrah.cor", sep=""))
  file.remove(paste(wd, "/Syrah/syrah.par", sep=""))
  file.remove(paste(wd, "/Syrah/syrah.std", sep=""))
  file.remove(paste(wd, "/Syrah/SYRAH_annual.out", sep=""))
  
  file.remove(paste(wd, "/Syrah/syrah_v1.cor", sep=""))
  file.remove(paste(wd, "/Syrah/syrah_v1.par", sep=""))
  file.remove(paste(wd, "/Syrah/syrah_v1.std", sep=""))

  file.remove(paste(wd, "/Syrah/syrah_v2.cor", sep=""))
  file.remove(paste(wd, "/Syrah/syrah_v2.par", sep=""))
  file.remove(paste(wd, "/Syrah/syrah_v2.std", sep=""))

  #Side-specific files
  if(side=='west') {
    file.remove(paste(wd, "/Syrah/outputFiles/WestSide/WestSide_", year, ".out", sep=""))
    file.remove(paste(wd, "/Syrah/outputFiles/WestSide/COR/WestSide_", year, ".cor", sep=""))
    file.remove(paste(wd, "/Syrah/outputFiles/WestSide/STD/WestSide_", year, ".std", sep=""))
  }
  if(side=='east') {
    file.remove(paste(wd, "/Syrah/outputFiles/EastSide/EastSide_", year, ".out", sep=""))
    file.remove(paste(wd, "/Syrah/outputFiles/EastSide/COR/EastSide_", year, ".cor", sep=""))
    file.remove(paste(wd, "/Syrah/outputFiles/EastSide/STD/EastSide_", year, ".std", sep=""))  	
  }
  if(side!='west' & side!='east') { stop('ERROR: YOU ARE AN IDOT, side: ', side, sep="") }
}

#Move output files to specified folders
move <- function(side='west', year=2013, wd=wd, model.name=NULL) {
  if(side=='west') {
    file.copy(from=paste(wd, "/Syrah/SYRAH_annual.out", sep=""), 
              to=paste(wd, "/Syrah/outputFiles/WestSide/WestSide_",year, ".out", sep=""), 
              overwrite=TRUE)	
    # file.copy(from=paste(wd, "/Syrah/syrah.cor", sep=""),
    #           to=paste(wd, "/Syrah/outputFiles/WestSide/COR/WestSide_",year, ".cor", sep=""), 
    #           overwrite=TRUE)     
    # file.copy(from=paste(wd, "/Syrah/syrah.std", sep=""), 
    #           to=paste(wd, "/Syrah/outputFiles/WestSide/STD/WestSide_",year, ".std", sep=""), 
    #           overwrite=TRUE)         
    file.copy(from=file.path(wd, "Syrah", paste(model.name, ".cor", sep="")),
              to=paste(wd, "/Syrah/outputFiles/WestSide/COR/WestSide_",year, ".cor", sep=""), 
              overwrite=TRUE)     
    file.copy(from=file.path(wd, "Syrah", paste(model.name, ".std", sep="")), 
              to=paste(wd, "/Syrah/outputFiles/WestSide/STD/WestSide_",year, ".std", sep=""), 
              overwrite=TRUE)  
  }	
  if(side=='east') {
    file.copy(from=paste(wd, "/Syrah/SYRAH_annual.out", sep=""),
              to=paste(wd, "/Syrah/outputFiles/EastSide/EastSide_",year, ".out", sep=""), 
              overwrite=TRUE)	
    # file.copy(from=paste(wd, "/Syrah/syrah.cor", sep=""),
    #           to=paste(wd, "/Syrah/outputFiles/EastSide/COR/EastSide_",year, ".cor", sep=""), 
    #           overwrite=TRUE)     
    # file.copy(from=paste(wd, "/Syrah/syrah.std", sep=""),
    #           to=paste(wd, "/Syrah/outputFiles/EastSide/STD/EastSide_",year, ".std", sep=""), 
    #           overwrite=TRUE)  	
    file.copy(from=file.path(wd, "Syrah", paste(model.name, ".cor", sep="")),
              to=paste(wd, "/Syrah/outputFiles/EastSide/COR/EastSide_",year, ".cor", sep=""), 
              overwrite=TRUE)     
    file.copy(from=file.path(wd, "Syrah", paste(model.name, ".std", sep="")),
              to=paste(wd, "/Syrah/outputFiles/EastSide/STD/EastSide_",year, ".std", sep=""), 
              overwrite=TRUE)  	
  }
  if(side!='west' & side!='east') { stop('ERROR: YOU ARE AN IDOT, side: ', side, sep='') }
}


