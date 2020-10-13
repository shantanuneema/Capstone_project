
# Program Eye-tracking Capstone Project
# Date: July 19th, 2017
# Author: Shantanu Neema
# Program to evaluate fixations for Tableau (Dispersion based algorithm)

library('cluster')
library('fpc')
library('png')
library('zoo')

subjects <- read.csv("EyetrackProject/SubjectDetails.csv",header=TRUE,sep=",")
Alldata <- read.csv("EyetrackProject/FixSac_wAOI_4.csv",header=TRUE,sep=",")

dat <- NULL

count <- 1

for (I in c(1,5,9,13,19,21,4,6,10,12,14,16,20,22,24,44,46,50,51,54,25,33,35,39,41,43)) {
  
  sdata <- Alldata[which(Alldata$trial == I),]
  sdata$count <- count
  
  sdata$x <- 1920 - sdata$x 
  sdata$y <- 1080 - sdata$y

  dat <- rbind(dat,sdata)
  count <- count + 1
  
}

dat$AOI <- ifelse(dat$x <= 760 & dat$y <= 280,"LDwn",dat$AOI)
dat$AOI <- ifelse(dat$x <= 760 & dat$y > 280,"Lup",dat$AOI)
dat$AOI <- ifelse(dat$x > 760 & dat$x <= 1230 & dat$y <= 280,"RdDwn",dat$AOI)
dat$AOI <- ifelse(dat$x > 760 & dat$x <= 1230 & dat$y > 280 & dat$y <= 540,"AtRoad",dat$AOI)
dat$AOI <- ifelse(dat$x > 760 & dat$x <= 1230 & dat$y > 540,"RdUp",dat$AOI)
dat$AOI <- ifelse(dat$x > 1230 & dat$y <= 280,"RDwn",dat$AOI)
dat$AOI <- ifelse(dat$x > 1230 & dat$y > 280,"Rup",dat$AOI)


dat$AOI <- ifelse(dat$x <= 640 & dat$y > 750,"L.top",dat$AOI)
dat$AOI <- ifelse(dat$x > 870 & dat$y <= 570,"R.bot",dat$AOI)
dat$AOI <- ifelse(dat$x > 870 & dat$y > 570 & dat$y <= 750,"R.cen",dat$AOI)
dat$AOI <- ifelse(dat$x > 870 & dat$y > 750,"R.top",dat$AOI)
dat$AOI <- ifelse(dat$x > 640 & dat$x <= 870 & dat$y < 390,"Sp.Mtr",dat$AOI)
dat$AOI <- ifelse(dat$x > 640 & dat$x <= 870 & dat$y < 570 & dat$y >= 390,"Rd.bot",dat$AOI)
dat$AOI <- ifelse(dat$x > 640 & dat$x <= 870 & dat$y < 750 & dat$y >= 570,"Rd.cen",dat$AOI)
dat$AOI <- ifelse(dat$x > 640 & dat$x <= 870 & dat$y < 930 & dat$y >= 750,"Rd.top",dat$AOI)
dat$AOI <- ifelse(dat$x > 640 & dat$x <= 870 & dat$y >= 930,"R.view",dat$AOI)

for (K in 1:dim(Alldata)[1]) {
  if (Alldata$x[K]+195 <= 760) {
    if (1080-Alldata$y[K] <= 280) {Alldata$AOI[K] <- "LDwn"}
    else {Alldata$AOI[K] <- "LUp"}
  } else if (Alldata$x[K]+195 > 760 & Alldata$x[K]+195 <= 1230) {
    if(1080-Alldata$y[K] <= 280) {Alldata$AOI[K] <- "RdDwn"}
    else if (1080-Alldata$y[K] > 280 & 1080-Alldata$y[K] <= 540) {Alldata$AOI[K] <- "AtRoad"}
    else if (1080-Alldata$y[K] > 540) {Alldata$AOI[K] <- "RdUp"}
  } else if (Alldata$x[K]+195 > 1230) {
    if(1080-Alldata$y[K] <= 280) {Alldata$AOI[K] <- "RDwn"}
    else if (1080-Alldata$y[K] > 280) {Alldata$AOI[K] <- "RUp"}
  }
}


# Subject 46 data modification
# str_x <- dat[which(dat$trial == 46),]$x
# str_y <- dat[which(dat$trial == 46),]$y
# dat[which(dat$trial == 46),]$x <- str_y
# dat[which(dat$trial == 46),]$y <- str_x

findata <- dat[-which(dat$Subst == "SC"),]
findata <- findata[-which(findata$Subst == "PC"),]
findata <- findata[-which(findata$Subst == "SE"),]
findata <- findata[-which(findata$Subst == "PE"),]
findata <- findata[-which(findata$Subst == "ST"),]
findata <- findata[-which(findata$Subst == "PT"),]

findata <- findata[-which(findata$Subst == "NSE"),]
findata <- findata[-which(findata$Subst == "NPE"),]
findata <- findata[-which(findata$Subst == "NST"),]
findata <- findata[-which(findata$Subst == "NPT"),]

outname <- paste("EyetrackProject/FixSacTableau.csv",sep="")
fsname <- paste("EyetrackProject/FinData_wAOI.csv",sep="")
write.csv(findata,file=outname,row.names=FALSE)
write.csv(dat,file=fsname,row.names=FALSE)

data <- findata[which(findata$Drive == 4),]
kfit <- kmeans(cbind(data$x,data$y),centers=5)
plot(cbind(data$x,data$y),col=kfit$cluster)




