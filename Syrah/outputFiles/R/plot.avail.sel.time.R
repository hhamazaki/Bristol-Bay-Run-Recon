####### plot.avail.sel.time ######
# INPUTS: 1) side - "east" or "west"
#         2) years - years for which plots will be generated
#         3) pdf - T/F whether .pdf file will be generated
#		      4) plot.avail - whether to plot availability
#		      5) plot.sel  - whether to plot selectivity over time
#         6) plot.fmort - whether to plot fishing mortality over time
#		      7) plot.hist - whether to plot a histogram of the estimated availability values for each stock in each district
################################

plot.avail.sel.time <- function(side, years, pdf=FALSE, plot.avail=FALSE, plot.sel=FALSE, plot.fmort=FALSE, plot.hist=FALSE, wd=wd) {
  setwd(paste(wd, "/Syrah/outputFiles", sep=""))
  ### TESTING ###
  #    side <- 'east'
  #    years <- 1963:2014
  #    pdf <- FALSE
  #    plot.avail <- TRUE
  #    plot.sel <- FALSE
  #    plot.fmort <- FALSE
  ###
  n.select.par <- 5
  
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
  
  #Availabiltiy
  avail <- array(dim=c(n.districts, n.stocks, n.years), dimnames=list(names.districts,names.stocks,years))
  avail.est <- matrix(nrow=n.districts, ncol=n.years, dimnames=list(names.districts, years)) #Boolean for whether availability is estimated from genetic data
  #Selectivity
  select <- matrix(nrow=n.select.par, ncol=n.years, dimnames=list(c(1:5), years))
  #Instantaneous fishing mortality rate
  fmort <- matrix(nrow=n.districts, ncol=n.years, dimnames=list(names.districts, years))
  
  y <- 1
  for(y in 1:n.years) {
    year <- years[y]
    
    if(side == 'west') {
      temp.data <- readList(paste('WestSide/WestSide_',year,'.out',sep='')) 
      
    }else {
      temp.data <- readList(paste('EastSide/EastSide_',year,'.out',sep=''))	
    } 
    
    d <- 1
    for(d in 1:n.districts) {
      #Model Predicted availability
      s <- 1
      for(s in 1:n.stocks) {
        if(side == 'west') {
          avail[d,s,y] <- temp.data$availability[s]
          if(temp.data$GENdata[d] == 1) { avail.est[d,y] <- TRUE }else { avail.est[d,y] <- FALSE } #1=TRUE
        } else {
          avail[d,s,y] <- temp.data$availability[d,s]
          if(temp.data$GENdata[d] == 1) { avail.est[d,y] <- TRUE }else { avail.est[d,y] <- FALSE } #1=TRUE
        }
      }#next s
      fmort[d,y] <- temp.data$Fmort[d]
    }#next d
    select[,y] <- temp.data$selectivity
  }#next y
  
  #WRITE OUT SELECTIVITY
  if(side=='west') {
    write.csv(select, file='WestSide Figs/WestSide Selectivity.csv')
  }else {
    write.csv(select, file='WestSide Figs/WestSide Selectivity.csv')
  }
  
  ###########################################################
  if(plot.sel == TRUE) {
    if(pdf == TRUE) {
      if(side == 'west') {
        pdf(file='WestSide Figs/WestSide Selectivity Over Time.pdf', height=6, width=8)
      } else {
        pdf(file='EastSide Figs/EastSide Selectivity Over Time.pdf', height=6, width=8)	
      }
    }
    
    #Plotting Selectivity - relative
    rel.select <- matrix(nrow=n.select.par, ncol=n.years)
    
    #     y <- 1
    #     for(y in 1:n.years) {
    #       rel.select[,y] <- select[,y]/sum(select[,y])
    #     }#next y
    rel.select <- select
    colnames(rel.select) <- years
    
    space <- 0.2
    x.limit <- c(0,n.years+(n.years+1)*space)
    y.limit <- c(0, 1.1*max(rel.select))
    
    par(mfrow=c(n.select.par,1), oma=c(5,3,4,1), mar=c(0,4,0,4))
    
    #cols <- rainbow(n.select.par)
    cols <- topo.colors(n.select.par)
    
    if(pdf == TRUE) { pdf }
    s <- 1
    for(s in 1:n.select.par) {
      xpos <- barplot(rel.select[s,], beside=FALSE, xlim=x.limit, ylim=y.limit, xlab='', xaxt='n', main='', ylab=paste('Marine Age:',s,sep=' '), 
                      col=cols[s], space=space)
      abline(h=0)
    }#next s
    axis(side=1, at=xpos, labels=years, las=2, outer=TRUE)
    mtext('Year', side=1, line=3.5, outer=TRUE)  
    mtext('Relative Selectivity Values', side=2, line=1, outer=TRUE)
    if(pdf == TRUE) { dev.off() }
    
    
    # temp.df <- data.frame(rel.select)
    # rownames(temp.df) <- paste('X.',row.names(select), sep='')
    # colnames(temp.df) <- years
    if(pdf == TRUE) {
      if(side == 'west') {
        pdf(file='WestSide Figs/WestSide Selectivity Dist.pdf', height=4, width=8)
      } else {
        pdf(file='EastSide Figs/EastSide Selectivity Dist.pdf', height=4, width=8)	
      }
    }
    par(mfrow=c(1,1), mar=c(3,3,2,1), oma=c(0,0,0,0))
    
    #DISTRIBUTIONS
    list.rel.select <- melt(rel.select)
    list.rel.select <- data.frame(list.rel.select)
    names(list.rel.select) <- c('OceanAge','Year','value')
    list.rel.select$OceanAge <- as.factor(list.rel.select$OceanAge)
    
    tmp <- ggplot(list.rel.select, aes(x=value, fill=OceanAge))
    tmp <- tmp + geom_density(alpha = 0.5, lwd=0.1) 
    tmp <- tmp + xlab('Relative Selectivity Values') + ylab('')
    
    if(side=='west') {
      tmp <- tmp + ggtitle('West Side Bristol Bay')	
    }else {
      tmp <- tmp + ggtitle('East Side Bristol Bay')	
    }
    #tmp <- tmp + scale_fill_hue(l=40)
    plot(tmp)
    tmp <- tmp + theme(legend.position='none')
    plot(tmp)
    
    age.cols <- colorRampPalette(c('blue','green','red'))(5)
    #CATERPILLAR PLOT
    #par(mfrow=c(2,1))
    t.rel.select <- data.frame(t(rel.select))
    colnames(t.rel.select) <- c(1:5) #paste('X.',c(1:5), sep='')
    
    caterplot(t.rel.select, denstrip=TRUE, quantiles=list(outer=c(0.025,0.975),inner=c(0.25,0.75)), font.lab=2)
    caterpoints(apply(t(rel.select), c(2), median), col='red', cex=2, pch=16)
    mtext('Relative Selectivity Values', side=1, line=2)
    mtext('Ocean Age', side=2, line=2)
    if(side=='west') {
      mtext('West Side Bristol Bay', side=3, font=2, outer=FALSE, line=0.5)
    }else {
      mtext('East Side Bristol Bay', side=3, font=2, outer=FALSE, line=0.5)
    }
    
    caterplot(t.rel.select, quantiles=list(outer=c(0.025,0.975),inner=c(0.25,0.75)), width=3, font.lab=2)
    caterpoints(apply(t(rel.select), c(2), median), col='red')
    mtext('Relative Selectivity Values', side=1, line=2)
    mtext('Ocean Age', side=2, line=2)
    if(side=='west') {
      mtext('West Side Bristol Bay', side=3, font=2, outer=FALSE, line=0.5)
    }else {
      mtext('East Side Bristol Bay', side=3, font=2, outer=FALSE, line=0.5)
    }
    
    #BEANPLOT
    beanplot(t.rel.select, lwd=1, col=c('blue', 'black', 'black', 'red'), ovalline='median', ll=0)
    mtext('Relative Selectivity Values', side=2, line=2)
    mtext('Ocean Age', side=1, line=2)
    if(side=='west') {
      mtext('West Side Bristol Bay', side=3, font=2, outer=FALSE, line=0.5)
    }else {
      mtext('East Side Bristol Bay', side=3, font=2, outer=FALSE, line=0.5)
    }
    beanplot(t.rel.select, lwd=1, col=c('blue', 'black', 'black', 'red'), ovalline='median', ll=0, horizontal=TRUE)
    mtext('Relative Selectivity Values', side=1, line=2)
    mtext('Ocean Age', side=2, line=2)
    if(side=='west') {
      mtext('West Side Bristol Bay', side=3, font=2, outer=FALSE, line=0.5)
    }else {
      mtext('East Side Bristol Bay', side=3, font=2, outer=FALSE, line=0.5)
    }
    
    
    #Violin ggplot
    calc.median <- function(x) {
      meds <- median(x)
      return(meds)
    }
    
    tmp <- ggplot(list.rel.select, aes(OceanAge, value, fill=OceanAge))
    tmp <- tmp + geom_violin(alpha = 0.5, lwd=0.1, scale='width') 
    tmp <- tmp + ylab('Relative Selectivity Values') + xlab('Ocean Age')
    tmp <- tmp + stat_summary(fun.y=calc.median, geom='point')
    if(side=='west') {
      tmp <- tmp + ggtitle('West Side Bristol Bay')	
    }else {
      tmp <- tmp + ggtitle('East Side Bristol Bay')	
    }
    plot(tmp)
    tmp <- tmp + coord_flip()
    plot(tmp)
    tmp <- tmp + theme(legend.position='none')
    plot(tmp)
    
    tmp <- ggplot(list.rel.select, aes(OceanAge, value, fill=OceanAge))
    tmp <- tmp + geom_violin(alpha = 0.5, lwd=0.1, scale='width') 
    tmp <- tmp + ylab('Relative Selectivity Values') + xlab('Ocean Age')
    #tmp <- tmp + stat_summary(fun.y=calc.median, geom='point')
    if(side=='west') {
      tmp <- tmp + ggtitle('West Side Bristol Bay')	
    }else {
      tmp <- tmp + ggtitle('East Side Bristol Bay')	
    }
    tmp <- tmp + geom_boxplot(width=0.25, lwd=0.5)#width=0.1, lwd=0.1
    plot(tmp)
    tmp <- tmp + coord_flip()
    plot(tmp)
    tmp <- tmp + theme(legend.position='none')
    plot(tmp)
    
    tmp <- ggplot(list.rel.select, aes(x=OceanAge, y=value, fill=OceanAge))
    tmp <- tmp + geom_boxplot() 
    tmp <- tmp + ylab('Relative Selectivity Values') + xlab('Stock')
    
    if(side=='west') {
      tmp <- tmp + ggtitle('West Side Bristol Bay')	
    }else {
      tmp <- tmp + ggtitle('East Side Bristol Bay')	
    }
    plot(tmp)
    tmp <- tmp + theme(legend.position='none')
    plot(tmp)
    
    if(pdf == TRUE) { dev.off() }
    
    ### WRITE SELECTIVITY TABLE ###
    sel.mean <- apply(rel.select, c(1), mean)
    sel.sd <- apply(rel.select, c(1), sd)
    sel.cv <- sel.sd/sel.mean
    sel.quants <- apply(rel.select, c(1), quantile, probs=c(0.025,0.25,0.5,0.75,0.975))
    sel.output <- cbind(sel.mean,sel.sd,sel.cv,t(sel.quants))
    if(side=='west') {
      write.csv(sel.output, file='WestSide Figs/WestSide Selectivity Table.csv')
    }else {
      write.csv(sel.output, file='EastSide Figs/EastSide Selectivity Table.csv')
    }
  }  
  ########################################################### 
  if(plot.avail == TRUE) {
    if(pdf == TRUE) {
      if(side == 'west') {
        pdf(file='WestSide Figs/WestSide Availability Over Time.pdf', height=6, width=8)
      } else {
        pdf(file='EastSide Figs/EastSide Availability Over Time.pdf', height=6, width=8)	
      }
    }
    
    #PLOTTING AVAILABILITY OVER TIME
    #avail #[d,s,y]
    #avail.est #[d,y]
    #Relative Availability
    rel.avail <- array(dim=c(n.districts, n.stocks, n.years), dimnames=list(names.districts,names.stocks,years))
    #Density for delineating non-estimated availability values
    temp.dens <- matrix(nrow=n.districts, ncol=n.years, dimnames=list(names.districts,years))
    
    y <- 1
    for(y in 1:n.years) {
      d <- 1
      for(d in 1:n.districts) {
        rel.avail[d,,y] <- avail[d,,y]/sum(avail[d,,y])
        if(avail.est[d,y] == TRUE) { temp.dens[d,y] <- 1000 }else { temp.dens[d,y] <- 30 }
      }#next d  	
    }#next y
    
    space <- 0.2
    x.limit <- c(0,n.years+(n.years+1)*space)
    y.limit <- c(0,1.1*max(rel.avail))
    #y.limit <- c(0,1.1*max(avail))
    #cols <- rainbow(n.stocks)
    cols <- topo.colors(n.stocks)
    
    par(mfrow=c(n.stocks,1), oma=c(5,3,4,1), mar=c(0,4,0,4))
    
    d <- 1
    for(d in 1:n.districts) {
      s <- 1
      for(s in 1:n.stocks) {
        xpos <- barplot(rel.avail[d,s,], beside=FALSE, xlim=x.limit, ylim=y.limit, xlab='', xaxt='n', main='', 
                        ylab=paste(names.stocks[s], 'River', sep=' '), 
                        col=cols[s], space=space, density=temp.dens[d,], angle=45)
      }#next s
      mtext('Year', side=1, line=3.5, outer=TRUE)  
      mtext('Relative Availability Values', side=2, line=1, outer=TRUE)
      axis(side=1, at=xpos, labels=years, las=2)
      mtext(paste(names.districts[d], 'District Availability', sep=' '), side=3, line=1.5, font=2, cex=1.5, outer=TRUE)
    } #next d 
    
    #NEW FIGURES FOR PUBLICATION
    
    if(pdf == TRUE) { dev.off() }
    #good to here
    if(pdf == TRUE) {
      if(side == 'west') {
        pdf(file='WestSide Figs/WestSide Availability Dist.pdf', height=6, width=6)
      } else {
        pdf(file='EastSide Figs/EastSide Availability Dist.pdf', height=6, width=6)	
      }
    }
    par(mfrow=c(1,1), mar=c(3,3,2,1), oma=c(0,0,0,0))
    
    #DISTRIBUTIONS
    #avail
    #rel.avail
    #avail.est
    
    #Replace assumed values with NA's
    dim(avail)
    dim(avail.est)
    
    avail.dat <- avail
    #avail.dat <- rel.avail
    
    d <- 1
    for(d in 1:n.districts) {
      y <- 1
      for(y in 1:n.years) {
        if(avail.est[d,y]==FALSE) {
          s <- 1
          for(s in 1:n.stocks) {
            avail.dat[d,s,y] <- NA
          }#next s
        }
      }#next y
    }#next d
    
    #Create List Object
    list.avail <- melt(avail.dat)
    list.avail <- data.frame(list.avail)
    names(list.avail) <- c('District','Stock','Year','value')
    list.avail$Stock <- as.factor(list.avail$Stock)
    
    #Plot Distribtuions
    tmp <- ggplot(list.avail, aes(x=value, fill=Stock))
    tmp <- tmp + geom_density(alpha = 0.5, lwd=0.1) 
    tmp <- tmp + xlab('Availability Values') + ylab('')
    tmp <- tmp + facet_wrap(~District, ncol=1)
    
    if(side=='west') {
      tmp <- tmp + ggtitle('West Side Bristol Bay')	
    }else {
      tmp <- tmp + ggtitle('East Side Bristol Bay')	
    }
    #tmp <- tmp + scale_fill_hue(l=40)
    plot(tmp)
    tmp <- tmp + theme(legend.position='none')
    plot(tmp)
    
    #Violin ggplot
    calc.median <- function(x) {
      meds <- median(x)
      return(meds)
    }
    tmp <- ggplot(list.avail, aes(x=Stock, y=value, fill=Stock))
    tmp <- tmp + geom_violin(alpha = 0.5, lwd=0.1, scale='width') 
    tmp <- tmp + ylab('Relative Availability Values') + xlab('Stock')
    tmp <- tmp + stat_summary(fun.y=calc.median, geom='point')
    tmp <- tmp + facet_wrap(~District, ncol=1)
    
    if(side=='west') {
      tmp <- tmp + ggtitle('West Side Bristol Bay')	
    }else {
      tmp <- tmp + ggtitle('East Side Bristol Bay')	
    }
    plot(tmp)
    tmp <- tmp + coord_flip()
    plot(tmp)
    tmp <- tmp + theme(legend.position='none')
    plot(tmp)
    
    tmp <- ggplot(list.avail, aes(x=Stock, y=value, fill=Stock))
    tmp <- tmp + geom_violin(alpha = 0.5, lwd=0.1, scale='width') 
    tmp <- tmp + ylab('Relative Availability Values') + xlab('Stock')
    #tmp <- tmp + stat_summary(fun.y=calc.median, geom='point')
    tmp <- tmp + facet_wrap(~District, ncol=1)
    
    if(side=='west') {
      tmp <- tmp + ggtitle('West Side Bristol Bay')	
    }else {
      tmp <- tmp + ggtitle('East Side Bristol Bay')	
    }
    tmp <- tmp + geom_boxplot(width=0.2, lwd=0.5) #width=0.5
    plot(tmp)
    tmp <- tmp + coord_flip()
    plot(tmp)
    tmp <- tmp + theme(legend.position='none')
    plot(tmp)
    plot(tmp + scale_x_discrete(limit=rev(names.stocks)))    
    #Boxplots
    tmp <- ggplot(list.avail, aes(x=Stock, y=value, fill=Stock))
    tmp <- tmp + geom_boxplot(lwd=0.5) 
    tmp <- tmp + ylab('Relative Availability Values') + xlab('Stock')
    tmp <- tmp + facet_wrap(~District, ncol=1)
    
    if(side=='west') {
      tmp <- tmp + ggtitle('West Side Bristol Bay')	
    }else {
      tmp <- tmp + ggtitle('East Side Bristol Bay')	
    }
    plot(tmp)
    tmp <- tmp + theme(legend.position='none')
    plot(tmp)
    
    #tmp <- coord_flip()
    #plot(tmp)
    
    #### CONTINUE HERE!!!!
    #rel.avail
    #avail.est
    
    par(mfrow=c(n.districts,1), oma=c(2,2,2,1), mar=c(2,4,2,0))
    d <- 1
    for(d in 1:n.districts) {
      temp.dat <- t(avail.dat[d,,avail.est[d,]])
      caterplot(temp.dat, quantiles=list(outer=c(0.025,0.975),inner=c(0.25,0.75)), font.lab=2, denstrip=FALSE, width=3, lwd=c(2,4))
      mtext(paste(names.districts[d],'District'), side=3, font=2)
      caterpoints(apply(temp.dat, c(2), median), col='red', cex=1.5)
    }#next d
    mtext('Stock', side=2, outer=TRUE, font=2)
    mtext('Relative Availability', side=1, outer=TRUE, font=2, line=0.5)
    d <- 1
    for(d in 1:n.districts) {
      temp.dat <- t(avail.dat[d,,avail.est[d,]])
      caterplot(temp.dat, quantiles=list(outer=c(0.025,0.975),inner=c(0.25,0.75)), font.lab=2, denstrip=FALSE)
      mtext(paste(names.districts[d],'District'), side=3, font=2)
      caterpoints(apply(temp.dat, c(2), median), col='red')
    }#next d
    mtext('Stock', side=2, outer=TRUE, font=2)
    mtext('Relative Availability', side=1, outer=TRUE, font=2, line=0.5)
    #Density Strip
    d <- 1
    for(d in 1:n.districts) {
      temp.dat <- t(avail.dat[d,,avail.est[d,]])
      caterplot(temp.dat, quantiles=list(outer=c(0.025,0.975),inner=c(0.25,0.75)), font.lab=2, denstrip=TRUE)
      mtext(paste(names.districts[d],'District'), side=3, font=2)
      caterpoints(apply(temp.dat, c(2), median), col='red', pch=16, cex=1.5)
    }#next d
    mtext('Stock', side=2, outer=TRUE, font=2)
    mtext('Relative Availability', side=1, outer=TRUE, font=2, line=0.5)
    
    ### WRITE SELECTIVITY TABLE ###
    list.dist <- vector(length=0)
    list.stock <- vector(length=0)
    list.mean <- vector(length=0)
    list.sd <- vector(length=0)
    list.cv <- vector(length=0)
    
    d <- 1
    for(d in 1:n.districts) {
      dist <- names.districts[d]
      #Update lists
      list.dist <- append(list.dist, rep(dist, n.stocks))
      list.stock <- append(list.stock, names.stocks)
      temp.mean <- apply(avail.dat[d,,], c(1), mean, na.rm=TRUE)
      list.mean <- append(list.mean, temp.mean)
      temp.sd <- apply(avail.dat[d,,], c(1), sd, na.rm=TRUE)
      list.sd <- append(list.sd, temp.sd)
      temp.cv <- temp.sd/temp.mean
      list.cv <- append(list.cv, temp.cv)
      
      if(d==1) {
        temp.quant <- apply(avail.dat[d,,], c(1), quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm=TRUE)
        mtx.quant <- t(temp.quant)
      }else {
        temp.quant <- apply(avail.dat[d,,], c(1), quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm=TRUE)
        mtx.quant <- rbind(mtx.quant, t(temp.quant))
      }  
    }#next d
    avail.output <- cbind(list.dist, list.stock, list.mean, list.sd, list.cv, mtx.quant)    
    avail.output <- data.frame(avail.output)
    
    if(side=='west') {
      write.csv(avail.output, file='WestSide Figs/WestSide Availability Table.csv')
    }else {
      write.csv(avail.output, file='EastSide Figs/EastSide Availability Table.csv')
    }
    
    if(pdf == TRUE) { dev.off() }
  }
  ###########################################################  
  if(plot.fmort == TRUE) {
    #### NOT FINISHED - NOT TRUELY NECESSARY ####
    print(fmort)
    if(pdf == TRUE) {
      if(side == 'west') {
        pdf(file='WestSide Figs/WestSide Fmort Over Time.pdf', height=2, width=8)
      } else {
        pdf(file='EastSide Figs/EastSide Fmort Over Time.pdf', height=6, width=6)	
      }
    }
    par(mfrow=c(1,1), oma=c(1,1,1,1), mar=c(0,0,0,0))
    list.fmort <- melt(fmort)
    list.fmort <- data.frame(list.fmort)
    names(list.fmort) <- c('District','Year','value')
    list.fmort$District <- as.factor(list.fmort$District)
    
    #Lines
    g <- ggplot(list.fmort, aes(x=Year, y=value, fill=District))
    g <- g + geom_line(aes(col=District))
    g <- g + ylab('Estimated Fishing Mortality Rate')
    if(side=='west') {
      g <- g + ggtitle('West Side Bristol Bay')	
    }else {
      g <- g + ggtitle('East Side Bristol Bay')	
    }
    plot(g)
    
    #Distributions
    g <- ggplot(list.fmort, aes(x=District, y=value, fill=District))
    g <- g + geom_violin()
    g <- g + ylab('Estimated Fishing Mortality Rate')
    if(side=='west') {
      g <- g + ggtitle('West Side Bristol Bay')	
    }else {
      g <- g + ggtitle('East Side Bristol Bay')	
    }
    plot(g)
    
    #Boxplot
    g <- ggplot(list.fmort, aes(x=District, y=value, fill=District))
    g <- g + geom_boxplot(lwd=0.5)
    g <- g + ylab('Estimated Fishing Mortality Rate')
    if(side=='west') {
      g <- g + ggtitle('West Side Bristol Bay')	
    }else {
      g <- g + ggtitle('East Side Bristol Bay')	
    }
    plot(g)
    
    #Caterplot
    par(mfrow=c(1,1), oma=c(2,2,0,0), mar=c(2,6,2,1))
    caterplot(t(fmort))
    mtext('District', side=2, outer=TRUE, font=2, line=1)
    mtext('Estimated Fishing Mortality Rate', side=1, outer=TRUE, font=2, line=0.5)
    if(side=='west') {
      mtext('West Side Bristol Bay', side=3, line=0.5, font=2)
    }else {
      mtext('East Side Bristol Bay', side=3, line=0.5, font=2)
    }
    
    if(pdf==TRUE) { dev.off() }
    
    
  }#end if plot Fmort
  ###########################################################
  if(plot.hist == TRUE) {
    if(pdf == TRUE) {
      if(side == 'west') {
        pdf(file='WestSide Figs/WestSide Availability Histogram.pdf', height=6, width=5)
      } else {
        pdf(file='EastSide Figs/EastSide Availability Histogram.pdf', height=6, width=8)	
      }
    }
    par(mfcol=c(n.stocks,n.districts), mar=c(4,4,0,0), oma=c(4,4,4,4))
    n.breaks <- 10
    d <- 1
    for(d in 1:n.districts) {
      s <- 1
      for(s in 1:n.stocks) {
        xpos <- hist(avail[d,s,which(avail.est[d,])], main='', xlab='',ylab='', col='gray', breaks=n.breaks)
        abline(v=mean(avail[d,s,which(avail.est[d,])]), col='red', lwd=3)
        abline(v=median(avail[d,s,which(avail.est[d,])]), col='blue', lwd=3)
        if(d == 1) { mtext(paste(names.stocks[s],'River',sep=' '), side=2, line=2.5, outer=FALSE, cex=0.75)  }
        if(s == n.stocks) { mtext(paste(names.districts[d],'District',sep=' '), side=1, line=2.5, outer=FALSE, cex=0.75) }
        if(d == 1 & s == 1) { legend('topright', legend=c('Mean','Median'), bty='n', col=c('red','blue'), lty=c(1,1)) }
      }#next s
      
    }#next d
    mtext('District', side=1, line=2, outer=TRUE, font=2)
    mtext('Stock', side=2, line=2, outer=TRUE, font=2)
    if(pdf == TRUE) { dev.off() }
  }
  
}