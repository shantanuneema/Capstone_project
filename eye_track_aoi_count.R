
# Program Eye-tracking Capstone Project
# Date: July 24th, 2017
# Author: Shantanu Neema
# Program to generat AOI data (For Tableau)

library('animation')

AOIFunc <- function(subdat) {
  data <- NULL
  sts <- 1
  ste <- 0
  for (I in 1:(length(subdat$AOI)-1)) {
    if (subdat$AOI[I] != subdat$AOI[I+1]) {
      ste <- I 
      data <- rbind(data,c(subdat$start[sts],subdat$end[ste],as.character(subdat$AOI[I])))
      sts <- I+1
    }
  }
  data <- rbind(data,c(subdat$start[sts],subdat$end[I+1],as.character(subdat$AOI[I+1])))
  data <- as.data.frame(data)
  data$V1 <- as.numeric(as.character(data$V1))
  data$V2 <- as.numeric(as.character(data$V2))
  return(data)
}

modres <- function(res1) {
  if("Lane.Offset" %in% colnames(res1)) {res1 <- res1[,-7]}
  res1$dist <- 0
  
  for (K in 1:length(res1$Time)) {
    if (K == 1) {res1$dist[K] <- 0}
    else {res1$dist[K] <- 0.5*(res1$Speed[K]+res1$Speed[K-1])*
      (res1$Time[K]-res1$Time[K-1])*5/18000+res1$dist[K-1]}
  }
  return(res1)
}

trans <- function(data) {
  data <- cbind(data,V4=0)
  data$V4 <- as.character(data$V3)
  data$V4 <- ifelse(data$V4 == "LUp","Lft",data$V4)
  data$V4 <- ifelse(data$V4 == "RUp","Rgt",data$V4)
  data$V4 <- ifelse(data$V4 == "LDwn","Lft",data$V4)
  data$V4 <- ifelse(data$V4 == "RDwn","Rgt",data$V4)
  data$V4 <- ifelse(data$V4 == "RdUp","TopRd",data$V4)
  data$V4 <- ifelse(data$V4 == "RdDwn","BotRd",data$V4)
  data$V4 <- ifelse(data$V4 == "AtRoad","CenRd",data$V4)
  return(data)
}

trans2 <- function(data) {
  data$V5 <- ifelse(data$V5 == "CenRd-BotRd","BotRd-CenRd",data$V5)
  data$V5 <- ifelse(data$V5 == "Lft-CenRd","CenRd-Lft",data$V5)
  data$V5 <- ifelse(data$V5 == "Rgt-CenRd","CenRd-Rgt",data$V5)
  data$V5 <- ifelse(data$V5 == "TopRd-CenRd","CenRd-TopRd",data$V5)
  data$V5 <- ifelse(data$V5 == "TopRd-BotRd","BotRd-TopRd",data$V5)
  data$V5 <- ifelse(data$V5 == "Lft-TopRd","TopRd-Lft",data$V5)
  data$V5 <- ifelse(data$V5 == "Rgt-TopRd","TopRd-Rgt",data$V5)
  data$V5 <- ifelse(data$V5 == "Lft-BotRd","BotRd-Lft",data$V5)
  data$V5 <- ifelse(data$V5 == "Rgt-BotRd","BotRd-Rgt",data$V5)
  data$V5 <- ifelse(data$V5 == "Rgt-Lft","Lft-Rgt",data$V5)
  return(data)
}

getAOI <- function(subdat) {
  subdat$AOI <- as.character(subdat$AOI)
  data <- NULL
  sts <- 1
  ste <- 0
  for (I in 1:(length(subdat$AOI)-1)) {
    if (subdat$AOI[I] != subdat$AOI[I+1]) {
      ste <- I 
      data <- rbind(data,c(subdat$start[sts],subdat$end[ste],as.character(subdat$AOI[I])))
      sts <- I+1
    }
  }
  data <- rbind(data,c(subdat$start[sts],subdat$end[I+1],as.character(subdat$AOI[I+1])))
  data <- as.data.frame(data)
  data$V1 <- as.numeric(as.character(data$V1))
  data$V2 <- as.numeric(as.character(data$V2))
  
  data$V3 <- as.character(data$V3)
  data <- trans(data)
  
  data <- cbind(data,V5=0)
  for (I in 1:length(data$V4)) {
    if (I == 1) {data$V5[I] <- NA}
    else if (data$V4[I-1] != data$V4[I]) {
      data$V5[I] <- paste(data$V4[I-1],"-",data$V4[I],sep="")
    } else {data$V5[I] <- NA}
  }
  
  data <- trans2(data)
  vec <- as.numeric(paste(data$V2))-as.numeric(paste(data$V1))
  data <- cbind(data,Dur=vec)
  colnames(data) <- c("start","end","AOI","SubAOI","Shift","Dur")
  return(data)
}

Alldata <- read.csv("EyetrackProject/FinData_wAOI.csv",header=TRUE,sep=",")
subjects <- read.csv("EyetrackProject/SubjectDetails.csv",header=TRUE,sep=",")

Alldata <- subset(Alldata,!is.nan(Alldata[,5]))
Alldata$AOI <- as.character.factor(Alldata$AOI)

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

table(Alldata$AOI)


AOISfts <- as.data.frame(matrix(0,nrow=26,ncol=18))
colnames(AOISfts) <- c("Trial","Lft","Rgt","TopRd","CenRd","BotRd","BotRd-CenRd","CenRd-Lft",
                       "CenRd-Rgt","CenRd-TopRd","BotRd-TopRd","TopRd-Lft","TopRd-Rgt",
                       "BotRd-Lft","BotRd-Rgt","Lft-Rgt","Shifts","Duration")
AOINorm <- NULL
library('stringi')
count <- 1
for (I in c(1,5,9,13,19,21,4,6,10,12,14,16,20,22,24,44,46,50,51,54,25,33,35,39,41,43)) {
  subdat <- Alldata[which(Alldata$trial == I),]
  subdatn <- rbind(subdat[which(subdat$Subst == "NSC"),],
                   subdat[which(subdat$Subst == "NC"),],
                   subdat[which(subdat$Subst == "NPC"),])
  
  subdatn <- subdatn[order(subdatn$start),]
  AOIdata <- getAOI(subdatn)
  AOIdata <- cbind(trial = I,AOIdata,Drive=4)
  AOISfts[count,1] <- I
  AOISfts[count,2] <- sum(stri_count_fixed(AOIdata$SubAOI,"Lft"),na.rm=TRUE)
  AOISfts[count,3] <- sum(stri_count_fixed(AOIdata$SubAOI,"Rgt"),na.rm=TRUE)
  AOISfts[count,4] <- sum(stri_count_fixed(AOIdata$SubAOI,"TopRd"),na.rm=TRUE)
  AOISfts[count,5] <- sum(stri_count_fixed(AOIdata$SubAOI,"CenRd"),na.rm=TRUE)
  AOISfts[count,6] <- sum(stri_count_fixed(AOIdata$SubAOI,"BotRd"),na.rm=TRUE)
  
  AOISfts[count,7] <- sum(stri_count_fixed(AOIdata$Shift,"BotRd-CenRd"),na.rm=TRUE)
  AOISfts[count,8] <- sum(stri_count_fixed(AOIdata$Shift,"CenRd-Lft"),na.rm=TRUE)
  AOISfts[count,9] <- sum(stri_count_fixed(AOIdata$Shift,"CenRd-Rgt"),na.rm=TRUE)
  AOISfts[count,10] <- sum(stri_count_fixed(AOIdata$Shift,"CenRd-TopRd"),na.rm=TRUE)
  AOISfts[count,11] <- sum(stri_count_fixed(AOIdata$Shift,"BotRd-CenRd"),na.rm=TRUE)
  AOISfts[count,12] <- sum(stri_count_fixed(AOIdata$Shift,"TopRd-Lft"),na.rm=TRUE)
  AOISfts[count,13] <- sum(stri_count_fixed(AOIdata$Shift,"TopRd-Rgt"),na.rm=TRUE)
  AOISfts[count,14] <- sum(stri_count_fixed(AOIdata$Shift,"BotRd-Lft"),na.rm=TRUE)
  AOISfts[count,15] <- sum(stri_count_fixed(AOIdata$Shift,"BotRd-Rgt"),na.rm=TRUE)
  AOISfts[count,16] <- sum(stri_count_fixed(AOIdata$Shift,"Lft-Rgt"),na.rm=TRUE)
  AOISfts[count,17] <- length(AOIdata$SubAOI)
  AOISfts[count,18] <- sum(AOIdata$Dur)
  
  AOINorm <- rbind(AOINorm,AOIdata)
  count <- count + 1
}

AOISfts <- cbind(AOISfts,Driver.Type = NDrive$Clust)
write.csv(AOISfts,file="EyetrackProject/AOISfts_Norm.csv",row.names = FALSE)

AOI_NvsS <- NULL
count <- 1

for (I in c(1,5,9,13,19,21,4,6,10,12,14,16,20,22,24,44,46,50,51,54,25,33,35,39,41,43)) {
  subdat <- Alldata[which(Alldata$trial == I),]
  subdatn <- rbind(subdat[which(subdat$Subst == "NSC"),],
                   subdat[which(subdat$Subst == "NC"),],
                   subdat[which(subdat$Subst == "NPC"),])
  
  subdatn <- subdatn[order(subdatn$start),]
  
  fpath <- ifelse(I<10,"EyetrackProject/Subjects/T00","EyetrackProject/Subjects/T0")
  s1name <- paste(fpath,I,"-00",4,".res2",sep="")
  res1 <- read.csv(s1name,header=TRUE)
  res1 <- res1[,1:8]
  res1 <- modres(res1)
  
  for (J in c(5,6,7)) {
    sdata <- subdat[which(subdat$Drive == J),]
    
    sname <- paste(fpath,I,"-00",J,".stm",sep="")
    s2name <- paste(fpath,I,"-00",J,".res2",sep="")
    stm <- read.csv(sname,header=TRUE)
    res2 <- read.csv(s2name,header=TRUE)
    res2 <- modres(res2)
    
    ts <- c(stm[1,1],stm[1,2],stm[2,1],stm[2,2])
    tn <- ts
    for (L in 1:4) {tn[L] <- res1[max(which(res1$dist 
                                            <=res2[max(which(res2$Time <= ts[L])),8])),2]}
    
    if(J == 5){sti1 <- "NC"; sti2 <- "C"; sti_s1 <- "C1"; sti_s2 <- "C2"}
    if(J == 6){sti1 <- "NE"; sti2 <- "E"; sti_s1 <- "E1"; sti_s2 <- "E2"}
    if(J == 7){sti1 <- "NT"; sti2 <- "T"; sti_s1 <- "T1"; sti_s2 <- "T2"}
    
    aoin1 <- subdatn[which(as.numeric(paste(subdatn$start)) >= tn[1]),]
    aoin1 <- aoin1[which(as.numeric(paste(aoin1$end)) <= tn[2]),]
    
    aoin2 <- subdatn[which(as.numeric(paste(subdatn$start)) >= tn[3]),]
    aoin2 <- aoin2[which(as.numeric(paste(aoin2$end)) <= tn[4]),]
    
    AOI_Norm1 <- cbind(count = count,trial = I,getAOI(aoin1),Drive = 4,Stimuli = "N1",Subst=sti1)
    AOI_Norm2 <- cbind(count = count,trial = I,getAOI(aoin2),Drive = 4,Stimuli = "N2",Subst=sti1)
    
    aois1 <- sdata[which(as.numeric(paste(sdata$start)) >= ts[1]),]
    aois1 <- aois1[which(as.numeric(paste(aois1$end)) <= ts[2]),]
    
    aois2 <- sdata[which(as.numeric(paste(sdata$start)) >= ts[3]),]
    aois2 <- aois2[which(as.numeric(paste(aois2$end)) <= ts[4]),]
    
    AOI_Sti1 <- cbind(count = count,trial = I,getAOI(aois1),Drive = J,Stimuli = sti_s1,Subst=sti2)
    AOI_Sti2 <- cbind(count = count,trial = I,getAOI(aois2),Drive = J,Stimuli = sti_s2,Subst=sti2)

    AOI_NvsS <- rbind(AOI_NvsS,AOI_Norm1,AOI_Sti1,AOI_Norm2,AOI_Sti2)
    
  }
  cat("AOI data created for Trial ",I," # of trials = ",count,sep="","\n")
  count <- count + 1
}

write.csv(AOI_NvsS,file="EyetrackProject/AOIData_NvsS.csv",row.names = FALSE)






