setwd("/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/R") 

catch.data <-  na.omit(read.csv('qry_Annual_CATCH.csv', header=TRUE, stringsAsFactors=FALSE))
esc.data <- na.omit(read.csv('qry_Annual_ESCAPEMENT_updated.csv', header=TRUE, stringsAsFactors=FALSE))

years <- 1963:2009
n.years <- length(years)

list.cat.nush <- catch.data$SumOfTotal[catch.data$DistrictID==325 & catch.data$Year>=min(years) & catch.data$Year<=max(years)]
list.cat.nk <- catch.data$SumOfTotal[catch.data$DistrictID==324 & catch.data$Year>=min(years) & catch.data$Year<=max(years)]
list.cat.eg <- catch.data$SumOfTotal[catch.data$DistrictID==322 & catch.data$Year>=min(years) & catch.data$Year<=max(years)]
list.cat.ug <- catch.data$SumOfTotal[catch.data$DistrictID==321 & catch.data$Year>=min(years) & catch.data$Year<=max(years)]

list.esc.igu <- esc.data$SumOfTotal[esc.data$DistrictID==325 & esc.data$Stream==100 & esc.data$Year>=min(years) & esc.data$Year<=max(years)]
list.esc.wood <- esc.data$SumOfTotal[esc.data$DistrictID==325 & esc.data$Stream==300 & esc.data$Year>=min(years) & esc.data$Year<=max(years)]
list.esc.nush <- esc.data$SumOfTotal[esc.data$DistrictID==325 & esc.data$Stream==700 & esc.data$Year>=min(years) & esc.data$Year<=max(years)]
list.esc.kvi <- esc.data$SumOfTotal[esc.data$DistrictID==324 & esc.data$Stream==100 & esc.data$Year>=min(years) & esc.data$Year<=max(years)]
list.esc.alag <- esc.data$SumOfTotal[esc.data$DistrictID==324 & esc.data$Stream==500 & esc.data$Year>=min(years) & esc.data$Year<=max(years)]
list.esc.nak <- esc.data$SumOfTotal[esc.data$DistrictID==324 & esc.data$Stream==600 & esc.data$Year>=min(years) & esc.data$Year<=max(years)]
list.esc.eg <- esc.data$SumOfTotal[esc.data$DistrictID==322 & esc.data$Stream==100 & esc.data$Year>=min(years) & esc.data$Year<=max(years)]
list.esc.ug <- esc.data$SumOfTotal[esc.data$DistrictID==321 & esc.data$Stream==100 & esc.data$Year>=min(years) & esc.data$Year<=max(years)]

catch.df <- data.frame(years, list.cat.nush, list.cat.nk, list.cat.eg, list.cat.ug)
names(catch.df) <- c('Year','Nushagak District', 'Naknek-Kvichak District', 'Egegik District', 'Ugashik District')
esc.df <- data.frame(years, list.esc.igu, list.esc.wood, list.esc.nush, list.esc.kvi, list.esc.alag, list.esc.nak, list.esc.eg, list.esc.ug)
names(esc.df) <- c('Year', 'Igushik Escapement', 'Wood Escapement', 'Nushagak Escapement', 'Kvichak Escapment', 'Alagnak Escapement', 'Naknek Escapement', 'Egegik Escapement', 'Ugashik Escapement')



write.csv(catch.df, 'For Fred 5.3.12/Catch Data.csv')
write.csv(esc.df, 'For Fred 5.3.12/Escapement Data.csv')
