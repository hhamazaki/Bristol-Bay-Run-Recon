######### fit.stock.recruit ########
# INPUTS: 1) side - "east" or "west"
#         2) stock
#
################################

fit.stock.recruit <- function(pdf=FALSE, wd=wd) {
  setwd(paste(wd, "/Syrah/outputFiles", sep=""))
  ### TESTING
  #side <- 'west'
  #stock <- 'Wood'
  ###
  
  n.stocks <- 8
  side.stocks <- c(rep('west',3), rep('east',5))
  name.stocks <- c('Igushik','Wood','Nushagak','Kvichak','Alagnak','Naknek','Egegik','Ugashik')
  
  if(pdf == TRUE) { pdf('RS Eval/Escapement Evaluation.pdf', height=9, width=8) }
  
  s <- 1
  for(s in 1:n.stocks) {
    #Stock specific information
    side <- side.stocks[s]
    stock <- name.stocks[s]	
    
    if(side == 'west') {
      data <- data.frame(read.csv(paste("WestSide Figs/",stock," Brood Table.csv", sep=''), skip=1))
    }else {
      data <- data.frame(read.csv(paste("EastSide Figs/",stock," Brood Table.csv", sep=''), skip=1))
    }
    
    data <- data[,c(2,21,22,23)]; names(data) <- c('broodYr','esc','rec','rps')
    data <- na.omit(data)  #Remove NA's from data
    
    #PLOTTING
    #start.alpha <- 1
    #start.beta <- 1e+6
    
    #par(mfrow=c(2,1))
    #plot(rec ~ esc, data=data, type='p'); 
    #points(x=data$esc, y=data$esc*exp(start.alpha*(1-(data$esc/start.beta))), col='red')
    #points(x=data$esc, y=(start.alpha*data$esc)/(start.beta+data$esc), col='red')
    #plot(rps ~ esc, data=data, type='p')
    
    
    
    #MODEL FITTING
    fit.ricker <- nls(rec ~ esc*exp(alpha*(1-(esc/beta))), data=data, start=list(alpha=1, beta=1e+6))
    fit.bh <- nls(rec ~ (alpha*esc)/(beta+esc), data=data, start=list(alpha=5e+6, beta=1e+6))
    
    #PLOT THE FIT
    par(mfrow=c(2,1), mar=c(0,4,0,4), oma=c(4,0,4,0))
    plot(rec~esc, data=data, type='p', pch=21, bg='gray', xlab='', ylab='Recruits', xaxt='n')
    trial <- seq(0,max(data$esc),100)
    
    lines(trial,predict(fit.bh, newdata=list(esc=trial)), col='blue', lwd=2)
    lines(trial,predict(fit.ricker, newdata=list(esc=trial)), col='red', lwd=2)
    legend('topright', title='AIC', legend=c(paste('B-H: ', round(AIC(fit.bh),0)), paste('Ricker: ', round(AIC(fit.ricker),0))), col=c('blue','red'), lwd=2, bty='n')
    #segments(0,0,1e+7,1e+7)
    #SURPLUS PRODUCTION
    plot((rec-esc)~esc, data=data, type='p', pch=21, bg='gray', xlab='', ylab='Surplus Production', xaxt='n')
    #legend('topright', title='Model', legend=c(paste('B-H: ', round(AIC(fit.bh),0)), paste('Ricker: ', round(AIC(fit.ricker),0))), col=c('blue','red'), lwd=2)
    lines(trial, predict(fit.bh, newdata=list(esc=trial))-trial, col='blue', lwd=2)
    max.sp <- max(predict(fit.bh, newdata=list(esc=trial))-trial)
    opt.bh <- trial[which((predict(fit.bh, newdata=list(esc=trial))-trial)==max.sp)]
    segments(opt.bh,-10e+6,opt.bh,max.sp, lty=1, col='blue')
    segments(0,max.sp,opt.bh,max.sp, lty=1, col='blue')
    
    lines(trial, predict(fit.ricker, newdata=list(esc=trial))-trial, col='red', lwd=2)
    max.sp <- max(predict(fit.ricker, newdata=list(esc=trial))-trial)
    opt.ricker <- trial[which((predict(fit.ricker, newdata=list(esc=trial))-trial)==max.sp)]
    segments(opt.ricker,-10e+6,opt.ricker,max.sp, lty=1, col='red')
    segments(0,max.sp,opt.ricker,max.sp, lty=1, col='red')
    
    legend('topright', title='MSY Esc', legend=c(paste('B-H: ',opt.bh), paste('Ricker: ', opt.ricker)), bty='n')
    
    mtext('Escapement', side=1, outer=TRUE, line=3)
    axis(side=1)
    mtext(paste(stock,'River',sep=' '), side=3, outer=2, font=2, line=2)
    
  }#next s
  if(pdf == TRUE) {dev.off() }
  
}

#fit.stock.recruit(pdf=TRUE, wd=wd)