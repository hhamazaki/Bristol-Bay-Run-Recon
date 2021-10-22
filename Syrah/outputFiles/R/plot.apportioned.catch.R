####### plot.apportioned.catch ######
# INPUTS: 1) side - "east" or "west"
#         2) years - years for which plots will be generated
#         3) pdf - T/F whether .pdf file will be generated
#		      4) separate - T/F whether to put different districts on the same or different pages
################################
plot.apportioned.catch <- function(side, years, pdf=FALSE, separate=FALSE, write.nush.data=FALSE, write.table=FALSE, wd=wd) {
  setwd(paste(wd, "/Syrah/outputFiles", sep=""))
  ### TESTING ###
  # side <- 'west'
  # years <- 1963:2018
  # pdf <- FALSE
  # separate <- FALSE
  # write.nush.data <- FALSE
  # write.table <- TRUE
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
      pdf(file='WestSide Figs/WestSide Apportioned Catches.pdf', height=8, width=10)
    } else {
      pdf(file='EastSide Figs/EastSide Apportioned Catches.pdf', height=8, width=10)
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
      # write.xlsx(x=catches, file='WestSide Figs/WestSide Apportioned Catches.xlsx', sheetName='catches', append=FALSE)
      
    }else {
      # write.xlsx(x=catches, file='EastSide Figs/EastSide Apportioned Catches.xlsx', sheetName='catches', append=FALSE)
      # writexl::write_xlsx(x=catches, path='EastSide Figs/EastSide Apportioned Catches.xlsx')
      # openxlsx::write.xlsx(x=catches, file='EastSide Figs/EastSide Apportioned Catches.xlsx', sheetName='catches')
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
    # yrs.pty <- pretty(years)[-c(1,7)]
    yrs.pty <- pretty(years)
    yrs.pty <- yrs.pty[yrs.pty>=min(years) & yrs.pty<=max(years)]
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
        # write.xlsx(x=output, file='WestSide Figs/WestSide Apportioned Catches.xlsx', sheetName=names.districts[d], append=TRUE)
      }else {
        # write.xlsx(x=output, file='EastSide Figs/EastSide Apportioned Catches.xlsx', sheetName=names.districts[d], append=TRUE)
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