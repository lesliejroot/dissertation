##first I downloaded from demogr.nes.ru and then converted the text file to CSV
##I used the files containing one-year age groups by birth order (Россия и регионы, по порядку рождения, однолетние возрастные группы)

#birth rates for 1989-2014:
fert <- read.csv("/Users/leslieroot/Documents/dissertation/data/CSVs/BRaO1989-2014.csv")

#because of the annexation of Crimea, newer data is separate, so birth rates for 2015-2017:
newfert <- read.csv("/Users/leslieroot/Documents/dissertation/data/CSVs/BRaO2015-2017.csv")

#Groups in the data are Total, Rural, Urban - here we want Total:
totfert <- subset(fert, fert$Group=="T")

totnewfert <- subset(newfert,newfert$Group=="T")

#to look at just overall fertility (all parities), we want 2 leading columns and 41 age groups:
totfert <- totfert[,c(1:2,4:44)] 
totnewfert <- totnewfert[,c(1:2,4:44)]


## Thanks to Monica Alexander for writing this part for me before I could do for-loops :)
##it divides everything by 1000000 to get a rate in units we can use:

ages <- 15:55

for (i in ages){
  colname <- paste0("rate", i)
  existing.colname <- paste0("BrOAa", i)
  totfert[, c(colname)] <- NA
  totfert[, c(colname)] <- (as.numeric(as.character(totfert[, c(existing.colname)])))/1000000
}

for (i in ages){
  colname <- paste0("rate", i)
  existing.colname <- paste0("BrOAa", i)
  totnewfert[, c(colname)] <- NA
  totnewfert[, c(colname)] <- (as.numeric(as.character(totnewfert[, c(existing.colname)])))/1000000
}

##throw out the per-million rates:
totfert <- totfert[,c(1:2,44:84)]
totnewfert <- totnewfert[,c(1:2,44:84)]

##combine the data from the two dataframes:
names <- paste0("rate",15:55)
total <- rbind(totfert,totnewfert)

#this sums the ASFRs to give us a period TFR for each year:
total$TFR <- rowSums(total[,colnames(total) %in% names])

#now to look at just the all-Russia data. Each region has a code, 1100 is the code for country-wide:
all <- subset(total, total$Reg == "1100")

##now let's look at yearly rates by transposing the dataframe
all <- all[,c(3:44)]
transpose <- as.data.frame(t(all))

##gotta name the columns of the transpose:
years <- 1989:2017
vec <-  NULL

for (i in years){
  vec <- c(vec, paste0("y",i))
}

colnames(transpose) <- vec

##gotta take out the TFR row, we'll put it back later though
transpose <- transpose[c(1:41),]

##the transpose lets us easily graph each year's ASFRs:
plot(transpose$y1989~ages, type ="l", col="black",ylab="fertility rate",xlab="age",
     main="Period age-specific fertility",lwd="3",ylim=c(0,.2))
lines(transpose$y2005~ages,col="red",lwd=3)
# lines(transpose$y2007~ages,col="orange",lwd=3)
lines(transpose$y2008~ages,col="darkgreen",lwd=3)
#lines(transpose$y2011~ages,col="darkolivegreen",lwd=3)
#lines(transpose$y2013~ages,col="hot pink",lwd=3)
lines(transpose$y2016~ages, type = "l",col="blue",lwd=3)
legend(49,.18, # places a legend at the appropriate place 
       c("1989","2005","2008","2016"), # puts text in the legend
       lty=c(1,1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(3,3,3,3),col=c("black","red","dark green","blue")) # gives the legend lines the correct color and width




#now make the cohort version:
ages <- 15:55
years <- 1989:2017
##oldest cohort is those who were 55 in 1989; youngest is those who were 15 in 2017
cohorts <- 1934:2002

##
all <- all[,1:41]
all <- as.matrix(all)

rownames(all) <- years
colnames(all) <- names

## Monica ABSOLUTELY wrote this part too:
upper <- matrix(NA, nrow = length(names), ncol = length(cohorts))
rownames(upper) <- names
colnames(upper) <- cohorts

for (i in 1:(ncol(all)-1)){
  dred <- all[,-(1:i)]
  age <- ages[-(1:i)][1]
  if(is.vector(dred)==FALSE){
    cohort <- as.numeric(rownames(dred)[1]) - age
    temp <- diag(dred)
    start.row <- which(ages ==age)
    end.row <- start.row + (length(temp)-1)
    upper[start.row:end.row, which(colnames(upper)==cohort)] <- temp
  }
  else{
    cohort <- as.numeric(names(dred)[1]) - age
    start.row <- which(ages ==age)
    upper[start.row, which(colnames(upper)==cohort)] <- dred[1]
  }
}

lower <- matrix(NA, nrow = length(names), ncol = length(cohorts))
rownames(lower) <- names
colnames(lower) <- cohorts

for (i in 1:(nrow(all)-1)){
  dred <- all[-(1:i),]
  if(is.vector(dred)==FALSE){
    temp <- diag(dred)
    age <- ages[-((length(ages)-1):length(ages))][1]
    cohort <- as.numeric(rownames(dred)[1]) - age
    start.row <- which(ages ==age)
    end.row <- start.row + (length(temp)-1)
    lower[start.row:end.row, which(colnames(upper)==cohort)] <- temp
  }
  else{
    cohort <- years[length(years)] - age
    start.row <- which(ages ==age)
    lower[start.row, which(colnames(upper)==cohort)] <- dred[1]
  }
}

#diag
cohort <- years[1] - ages[1]
temp <- diag(all)

#join
upper <- upper[,(sapply(1:ncol(upper), function(i) sum(is.na(upper[,i]))))!=nrow(upper)]
lower <- lower[,(sapply(1:ncol(lower), function(i) sum(is.na(lower[,i]))))!=nrow(lower)]

cohort.tfr <- cbind(upper, temp, lower)

colnames(cohort.tfr) <- cohorts
rownames(cohort.tfr) <- c(15:55)

cohorttfr <- as.data.frame(cohort.tfr)

#mistake in the joining - column 41, row 30-41 should be NA:
cohorttfr[30:41,41] <- NA

#plotting from cohorts:
plot(ages,cohorttfr$"1975",type ="l",col="black",lwd=2,ylab="TFR",xlab="age",xlim=c(15,45),
     main="Fertility, Russia, 1975, '80, and '85 birth cohorts",ylim=c(0,.15))
#lines(ages,cohorttfr$`1975`,col="red",lwd=2)
lines(ages,cohorttfr$"1980",col="red",lwd=2)
lines(ages,cohorttfr$"1985",col="blue",lwd=2)
legend(40,.15, # places a legend at the appropriate place 
       c("1975","1980","1985"), # puts text in the legend
       lty=c(1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(3,3,3),col=c("black","red","blue")) 


##we can get CFR for cohorts with complete fertility experiences in the observed years 
##(for everyone else the CFR will not be correct):

transposetfr <- as.data.frame(t(cohorttfr))
colnames(transposetfr) <- names

transposetfr$CFR <- rowSums(transposetfr[,colnames(transposetfr) %in% names],na.rm = TRUE)

all <- subset(total, total$Reg == "1100")
#now we have:
#rows are years, columns are ages: all
#rows are ages, columns are years: transpose
#rows are ages, columns are cohorts: cohorttfr
#rows are cohorts, columns are ages: transposetfr


#we have TFR and CFR, in all and transposetfr, respectively
#what don't we have? mean age at childbearing, mean age at first birth, portion of fertility that is first births



###mean age at childbearing - actually, technically, 
##mean age of the fertility schedule (excludes the effect of mortality).
##I did do mean age at childbearing originally but the two were so similar it did not seem
##worth it to me to add in the extra lx data needed to do a true MAC

years <- 1989:2017
means <- cbind(years,all)

for (i in ages){
  fx.colname <- paste0("rate",i)
  xfx.colname <- paste0("xfx",i)
  means[,c(xfx.colname)] <- NA
  means[,c(xfx.colname)] <- means[,c(fx.colname)]*(i+.5)
}

names <- paste0("xfx",15:55)
means$numerator2 <- rowSums(means[,colnames(means) %in% names])
names <- paste0("rate",15:55)
means$denominator2 <- rowSums(means[,colnames(means) %in% names])
means$schedmean <- means$numerator2/means$denominator2
plot(means$years,means$schedmean,type ="l",lwd="2",ylab="age",xlab="year",main="Period mean age at childbearing",ylim=c(24,29),xaxp  = c(1989, 2017,28),yaxp= c(24,29,10))

##variance
ages <- 15:55
for (i in ages){
  fx.colname <- paste0("rate",i)
  mean.colname<- paste0("mean",i)
  means[,c(mean.colname)] <- NA
  means[,c(mean.colname)] <- (i-means$schedmean)^2 * means[,c(fx.colname)]
}
names <- paste0("mean",15:55)
means$numerator3 <- rowSums(means[,colnames(means) %in% names])
means$variance <- means$numerator3/means$denominator2
means$stdev <- sqrt(means$variance)

##what about mean age at first birth??
##gotta re-read the data cause we deleted the parity-specific stuff :(
parity <- read.csv("/Users/leslieroot/Documents/dissertation/data/CSVs/BRaO1989-2014.csv")
newparity <- read.csv("/Users/leslieroot/Documents/dissertation/data/CSVs/BRaO2015-2017.csv")
parity <- rbind(parity, newparity)
parity <- subset(parity,parity$Reg=="1100" & Group =="T")

B1names <- paste0("BrO1a", 15:55)
parity <- parity[B1names]
parity <- cbind(years,parity)


for (i in ages){
  colname <- paste0("BrO1a", i)
  parity[, c(colname)] <- (as.numeric(as.character(parity[, c(colname)])))/1000000
}

parity$TFR <- rowSums(parity[,colnames(parity) %in% B1names])

##quick check of parity-specific TFR plot:
plot(parity$years,parity$TFR,type="l",ylim=c(0.5,2.2))
years <- c(1989:2017)
lines(all$TFR~years)

#got our first births, now we're ready for mean ages

mean1sts <- parity

for (i in ages){
  fx.colname <- paste0("BrO1a",i)
  xfx.colname <- paste0("xfx",i)
  mean1sts[,c(xfx.colname)] <- NA
  mean1sts[,c(xfx.colname)] <- mean1sts[,c(fx.colname)]*(i+.5)
}

names <- paste0("xfx",15:55)
mean1sts$numerator2 <- rowSums(mean1sts[,colnames(mean1sts) %in% names])
names <- paste0("BrO1a",15:55)
mean1sts$denominator2 <- rowSums(mean1sts[,colnames(mean1sts) %in% names])
mean1sts$schedmean <- mean1sts$numerator2/mean1sts$denominator2


##variance
ages <- 15:55
for (i in ages){
  fx.colname <- paste0("BrO1a",i)
  mean.colname<- paste0("mean",i)
  mean1sts[,c(mean.colname)] <- NA
  mean1sts[,c(mean.colname)] <- (i-mean1sts$schedmean)^2 * mean1sts[,c(fx.colname)]
}
names <- paste0("mean",15:55)
mean1sts$numerator3 <- rowSums(mean1sts[,colnames(mean1sts) %in% names])
mean1sts$variance <- mean1sts$numerator3/mean1sts$denominator2
mean1sts$stdev <- sqrt(mean1sts$variance)

##parity 2?
parity2 <- read.csv("/Users/leslieroot/Documents/dissertation/data/CSVs/BRaO1989-2014.csv")
newparity2 <- read.csv("/Users/leslieroot/Documents/dissertation/data/CSVs/BRaO2015-2017.csv")
parity2 <- rbind(parity2, newparity2)
parity2 <- subset(parity2,parity2$Reg=="1100" & Group =="T")

B2names <- paste0("BrO2a", 15:55)
parity2 <- parity2[B2names]
parity2 <- cbind(years,parity2)


for (i in ages){
  colname <- paste0("BrO2a", i)
  parity2[, c(colname)] <- (as.numeric(as.character(parity2[, c(colname)])))/1000000
}

parity2$TFR <- rowSums(parity2[,colnames(parity2) %in% B2names])

mean2nds <- parity2

for (i in ages){
  fx.colname <- paste0("BrO2a",i)
  xfx.colname <- paste0("xfx",i)
  mean2nds[,c(xfx.colname)] <- NA
  mean2nds[,c(xfx.colname)] <- mean2nds[,c(fx.colname)]*(i+.5)
}

names <- paste0("xfx",15:55)
mean2nds$numerator2 <- rowSums(mean2nds[,colnames(mean2nds) %in% names])
names <- paste0("BrO2a",15:55)
mean2nds$denominator2 <- rowSums(mean2nds[,colnames(mean2nds) %in% names])
mean2nds$schedmean <- mean2nds$numerator2/mean2nds$denominator2


##parity 3
parity3 <- read.csv("/Users/leslieroot/Documents/dissertation/data/CSVs/BRaO1989-2014.csv")
newparity3 <- read.csv("/Users/leslieroot/Documents/dissertation/data/CSVs/BRaO2015-2017.csv")
parity3 <- rbind(parity3, newparity3)
parity3 <- subset(parity3,parity3$Reg=="1100" & Group =="T")

B3names <- paste0("BrO3a", 15:55)
parity3 <- parity3[B3names]
parity3 <- cbind(years,parity3)


for (i in ages){
  colname <- paste0("BrO3a", i)
  parity3[, c(colname)] <- (as.numeric(as.character(parity3[, c(colname)])))/1000000
}

parity3$TFR <- rowSums(parity3[,colnames(parity3) %in% B3names])

mean3rds <- parity3

for (i in ages){
  fx.colname <- paste0("BrO3a",i)
  xfx.colname <- paste0("xfx",i)
  mean3rds[,c(xfx.colname)] <- NA
  mean3rds[,c(xfx.colname)] <- mean3rds[,c(fx.colname)]*(i+.5)
}

names <- paste0("xfx",15:55)
mean3rds$numerator2 <- rowSums(mean3rds[,colnames(mean3rds) %in% names])
names <- paste0("BrO3a",15:55)
mean3rds$denominator2 <- rowSums(mean3rds[,colnames(mean3rds) %in% names])
mean3rds$schedmean <- mean3rds$numerator2/mean3rds$denominator2


parity4 <- read.csv("/Users/leslieroot/Documents/dissertation/data/CSVs/BRaO1989-2014.csv")
newparity4 <- read.csv("/Users/leslieroot/Documents/dissertation/data/CSVs/BRaO2015-2017.csv")
parity4 <- rbind(parity4, newparity4)
parity4 <- subset(parity4,parity4$Reg=="1100" & Group =="T")

B4names <- paste0("BrO4a", 15:55)
parity4 <- parity4[B4names]
parity4 <- cbind(years,parity4)


for (i in ages){
  colname <- paste0("BrO4a", i)
  parity4[, c(colname)] <- (as.numeric(as.character(parity4[, c(colname)])))/1000000
}

parity4$TFR <- rowSums(parity4[,colnames(parity4) %in% B4names])

mean4ths <- parity4

for (i in ages){
  fx.colname <- paste0("BrO4a",i)
  xfx.colname <- paste0("xfx",i)
  mean4ths[,c(xfx.colname)] <- NA
  mean4ths[,c(xfx.colname)] <- mean4ths[,c(fx.colname)]*(i+.5)
}

names <- paste0("xfx",15:55)
mean4ths$numerator2 <- rowSums(mean4ths[,colnames(mean4ths) %in% names])
names <- paste0("BrO4a",15:55)
mean4ths$denominator2 <- rowSums(mean4ths[,colnames(mean4ths) %in% names])
mean4ths$schedmean <- mean4ths$numerator2/mean4ths$denominator2


#parity...5 and up
parity5 <- read.csv("/Users/leslieroot/Documents/dissertation/data/CSVs/BRaO1989-2014.csv")
newparity5 <- read.csv("/Users/leslieroot/Documents/dissertation/data/CSVs/BRaO2015-2017.csv")
parity5 <- rbind(parity5, newparity5)
parity5 <- subset(parity5,parity5$Reg=="1100" & Group =="T")

B5names <- paste0("BrO5a", 15:55)
parity5 <- parity5[B5names]
parity5 <- cbind(years,parity5)


for (i in ages){
  colname <- paste0("BrO5a", i)
  parity5[, c(colname)] <- (as.numeric(as.character(parity5[, c(colname)])))/1000000
}

parity5$TFR <- rowSums(parity5[,colnames(parity5) %in% B5names])

mean5ths <- parity5

for (i in ages){
  fx.colname <- paste0("BrO5a",i)
  xfx.colname <- paste0("xfx",i)
  mean5ths[,c(xfx.colname)] <- NA
  mean5ths[,c(xfx.colname)] <- mean5ths[,c(fx.colname)]*(i+.5)
}

names <- paste0("xfx",15:55)
mean5ths$numerator2 <- rowSums(mean5ths[,colnames(mean5ths) %in% names])
names <- paste0("BrO5a",15:55)
mean5ths$denominator2 <- rowSums(mean5ths[,colnames(mean5ths) %in% names])
mean5ths$schedmean <- mean5ths$numerator2/mean5ths$denominator2

##Great! Here's a nice plot of the change in mean age at childbearing by parity:
plot(means$years,means$schedmean,type ="l",lwd="3",ylab="age",xlab="year",main="Period mean age at childbearing",ylim=c(18,35),xaxp=c(1989,2017,28),yaxp=c(20,35,15))
lines(mean1sts$schedmean~years,type="l",col="red",lwd="2")
lines(mean2nds$schedmean~years,type="l",col="blue",lwd="2")
lines(mean3rds$schedmean~years,type="l",col="darkgreen",lwd="2")
lines(mean4ths$schedmean~years,type="l",col="magenta",lwd="2")
lines(mean5ths$schedmean~years,type="l",col="orange",lwd="2")
legend("bottomright", # places a legend at the appropriate place 
       c("Overall","Parity 1","Parity 2","Parity 3","Parity 4", "Parity 5+"), # puts text in the legend
       lty=c(1,1,1,1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(3,2,2,2,2,2),col=c("black","red","blue","dark green","magenta","orange")) # gives the legend lines the correct color and width





##Now a dataframe just of parity-specific TFR
TFRs <- as.data.frame(cbind(years, all$TFR, parity$TFR, parity2$TFR, parity3$TFR,parity4$TFR,parity5$TFR))
names(TFRs)[2] <- "total"
names(TFRs)[3] <- "parity1"
names(TFRs)[4] <- "parity2"
names(TFRs)[5] <- "parity3"
names(TFRs)[6] <- "parity4"
names(TFRs)[7] <- "parity5"
TFRs$fraction1 <- TFRs$parity1/TFRs$total
TFRs$fraction2 <- TFRs$parity2/TFRs$total
TFRs$fraction3 <- TFRs$parity3/TFRs$total
TFRs$fraction4 <- TFRs$parity4/TFRs$total
TFRs$fraction5 <- TFRs$parity5/TFRs$total

plot(TFRs$years,TFRs$total,type="l",ylim=c(0,2.5),main="Period TFR by parity",xlab="year",ylab="TFR")
lines(TFRs$years,TFRs$parity1,type="l",col="red")
lines(TFRs$years,TFRs$parity2,type="l",col="blue")
lines(TFRs$years,TFRs$parity3,type="l",col="darkgreen")
lines(TFRs$years,TFRs$parity4,type="l",col="magenta")
lines(TFRs$years,TFRs$parity5,type="l",col="orange")
legend("topright", # places a legend at the appropriate place 
       c("Total","Parity 1","Parity 2","Parity 3","Parity 4", "Parity 5+"), # puts text in the legend
       lty=c(1,1,1,1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2,2,2,2,2,2),col=c("black","red","blue","dark green","magenta","orange")) # gives the legend lines the correct color and width


props <- TFRs[,c(8:12)]
prop <- as.data.frame(t(props))

library(RColorBrewer) 
barplot(as.matrix(prop), main="Proportion of TFR by parity",
        xlab="year", col=brewer.pal(5,"GnBu")[5:1])

plot(mean1sts$years,mean1sts$schedmean,type ="l",lwd="3",ylab="age",xlab="year",main="Period mean age at first birth",xaxt="n",ylim=c(21,26),xaxp=c(1989,2015,26),yaxp=c(20,35,15))
at <- seq(from = 1989, to = 2017, by = 1)
axis(side = 1, at = at,las=2)
abline(h=21,lty=1,col="gray")
abline(h=22,lty=1,col="gray")
abline(h=23,lty=1,col="gray")
abline(h=24,lty=1,col="gray")
abline(h=25,lty=1,col="gray")
abline(h=26,lty=1,col="gray")
lines(mean1sts$years,mean1sts$schedmean)
# 
# legend("bottomright", # places a legend at the appropriate place 
#        c("Russia","Rostov Oblast","interpolated"), # puts text in the legend
#        lty=c(1,1,4), # gives the legend appropriate symbols (lines)
#        lwd=c(1,3,1)) # gives the legend lines the correct width



young <- as.data.frame(back[,1:5])
young$TFR <- rowSums(young)
underage <- c("rate15","rate16","rate17")
young$underageTFR <- rowSums(young[,colnames(young) %in% underage])
plot(young$underageTFR~years,type='l')
majority <- c("rate18","rate19")
young$majorityTFR <- rowSums(young[,colnames(young) %in% majority])
young$underageAVG <- young$underageTFR/3
young$majorityAVG <- young$majorityTFR/2


#####Background chapter plots

###Figure 1

plot(mean1sts$years,mean1sts$schedmean,type ="l",lwd="3",ylab="age",xlab="year",main="Period mean age at first birth, Russia",xaxt="n",ylim=c(22,26),xaxp=c(1989,2017,28),yaxp=c(20,35,15))
at <- seq(from = 1989, to = 2017, by = 2)
axis(side = 1, at = at,las=2)
abline(h=21,lty=1,col="gray")
abline(h=22,lty=1,col="gray")
abline(h=23,lty=1,col="gray")
abline(h=24,lty=1,col="gray")
abline(h=25,lty=1,col="gray")
abline(h=26,lty=1,col="gray")
lines(mean1sts$years,mean1sts$schedmean, lwd ="3")

plot(mean1sts$years, mean1sts$stdev,type ="l",lwd="3",ylab="standard deviation",xlab="year",main="Standard deviation in age at first birth, Russia",xaxt="n",ylim=c(4,5.1),xaxp=c(1989,2017,28),yaxp=c(4.0,5.1,11))
at <- seq(from = 1989, to = 2017, by = 2)
axis(side = 1, at = at,las=2)
abline(h=4.0,lty=1,col="gray")
abline(h=4.1,lty=1,col="gray")
abline(h=4.2,lty=1,col="gray")
abline(h=4.3,lty=1,col="gray")
abline(h=4.4,lty=1,col="gray")
abline(h=4.5,lty=1,col="gray")
abline(h=4.6,lty=1,col="gray")
abline(h=4.7,lty=1,col="gray")
abline(h=4.8,lty=1,col="gray")
abline(h=4.9,lty=1,col="gray")
abline(h=5.0,lty=1,col="gray")
abline(h=5.1,lty=1,col="gray")
lines(mean1sts$years, mean1sts$stdev, lwd="3")

