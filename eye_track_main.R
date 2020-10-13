
# Program Eye-tracking Capstone Project
# Date: Aug 8th, 2017
# Author: Shantanu Neema
# Program to evaluate fixations for Tableau (Dispersion based algorithm)

library('emov')
library('psych')
library('fpc')
library('cluster')

# Readfile is a function to read Subject.csv files, apply Savitzky-Golay Filtering
# and to divide the data based on Drives mode (DM), no division is needed for DM = 4
Readfile <- function(tdata,DM) {
  colnames(tdata)[1] <- "Subject"
  Nd <- tdata[which(tdata$Stimuli=="N"),]
  if(DM==5){
    N1d <- tdata[which(tdata$Stimuli=="N1" & tdata$Drive==DM),]
    D1d <- tdata[which(tdata$Stimuli=="A" & tdata$Drive==DM),]
    N2d <- tdata[which(tdata$Stimuli=="N2" & tdata$Drive==DM),]
    D2d <- tdata[which(tdata$Stimuli=="M" & tdata$Drive==DM),]
    N3d <- tdata[which(tdata$Stimuli=="N3" & tdata$Drive==DM),]
  } else if(DM==6) {
    N1d <- tdata[which(tdata$Stimuli=="N1" & tdata$Drive==DM),]
    D1d <- tdata[which(tdata$Stimuli=="E1" & tdata$Drive==DM),]
    N2d <- tdata[which(tdata$Stimuli=="N2" & tdata$Drive==DM),]
    D2d <- tdata[which(tdata$Stimuli=="E2" & tdata$Drive==DM),]
    N3d <- tdata[which(tdata$Stimuli=="N3" & tdata$Drive==DM),]
  } else if(DM==7) {
    N1d <- tdata[which(tdata$Stimuli=="N1" & tdata$Drive==DM),]
    D1d <- tdata[which(tdata$Stimuli=="T1" & tdata$Drive==DM),]
    N2d <- tdata[which(tdata$Stimuli=="N2" & tdata$Drive==DM),]
    D2d <- tdata[which(tdata$Stimuli=="T2" & tdata$Drive==DM),]
    N3d <- tdata[which(tdata$Stimuli=="N3" & tdata$Drive==DM),]
  }
  
  # Factors for Savizky-Golay Filter
  sg5 = c(-3,12,17,12,-3)/35 
  
  Nd$GazeX <- filter(Nd$GazeX,sg5)
  Nd$GazeY <- filter(Nd$GazeY,sg5)
  
  N1d$GazeX <- filter(N1d$GazeX,sg5)
  N1d$GazeY <- filter(N1d$GazeY,sg5)
  
  D1d$GazeX <- filter(D1d$GazeX,sg5)
  D1d$GazeY <- filter(D1d$GazeY,sg5)
  
  N2d$GazeX <- filter(N2d$GazeX,sg5)
  N2d$GazeY <- filter(N2d$GazeY,sg5)
  
  D2d$GazeX <- filter(D2d$GazeX,sg5)
  D2d$GazeY <- filter(D2d$GazeY,sg5)
  
  N3d$GazeX <- filter(N3d$GazeX,sg5)
  N3d$GazeY <- filter(N3d$GazeY,sg5)
  
  return(list(Nd,N1d,D1d,N2d,D2d,N3d))
}

# modnfix is a function to Re-label the "sub-stimuli"
# (Mainly for tableau to distinguish post-stimuli)
modnfix <- function(Nfix,t,dtype) {
  col <- which(colnames(Nfix)=="Subst")
  for (K in 1:dim(Nfix)[1]) {
    nty <- substr(dtype,2,2)
    if (Nfix$start[K] < t[1]) {
      Nfix[K,col] <- paste("NS",nty,sep="")
    } else if (Nfix$start[K] >= t[1] & Nfix$start[K] < t[2]) {
      Nfix[K,col] <- dtype
    } else if (Nfix$start[K] >= t[2] & Nfix$start[K] < t[3]) {
      Nfix[K,col] <- paste("NP",nty,sep="")
    } else if (Nfix$start[K] >= t[3] & Nfix$start[K] < t[4]) {
      Nfix[K,col] <- dtype
    } else if (Nfix$start[K] >= t[4]) {
      Nfix[K,col] <- paste("NP",nty,sep="")
    }
  }
  return(Nfix)
}

# modres is a function to read and calculate the distance travelled
# As our comparison is for the same distance (Normal vs Stimuli)
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

# fsdata will add columns for saccade data like:
# duration, velocities, turn angles, etc
# This function will also combine the driving data by aggregate averaging
fsdata <- function(fdata,data) {
  
  tvec1 <- fdata$start
  tvec2 <- fdata$end
  n1 <- tvec1
  n2 <- tvec2
  
  for (I in 1:length(fdata$dur)) {
    n1[I] <- which(data$Time==tvec1[I])
    n2[I] <- which(data$Time==tvec2[I])
  }
  
  ndata <- matrix(0,ncol=10,nrow=(dim(fdata)[1]))
  ndata <- cbind(fdata,ndata)
  colnames(ndata)[3:15] <- c("fdur","x","y","sdur","Vx","Vy","turn","spd","accn",
                             "brkg","strg","lane","AOI") 
  for (I in 1:(dim(fdata)[1])) {
    if (I == 1) {
      ndata[I,6:9] <- NA
    } else {
      if ((fdata[I,1]-fdata[I-1,2]) <= 0.8) {ndata[I,6] <- fdata[I,1]-fdata[I-1,2]}
      else (ndata[I,6] <- NA)
      ndata[I,7] <- (fdata[I,4] - fdata[I-1,4])/ndata[I,6]
      ndata[I,8] <- (fdata[I,5] - fdata[I-1,5])/ndata[I,6]
      if (I == 2) {
        ndata[I,9] <- atan((fdata$y[I]-fdata$y[1])/(fdata$x[I]-fdata$x[1]))
      } else {
        ndata[I,9] <- (atan((fdata$y[I]-fdata$y[I-1])/(fdata$x[I]-fdata$x[I-1])) 
                       - atan((fdata$y[I-1]-fdata$y[I-2])/(fdata$x[I-1]-fdata$x[I-2])))
      }
    }
    ndata[I,10] <- mean(data[n1[I]:n2[I],9])
    ndata[I,11] <- mean(data[n1[I]:n2[I],10])
    ndata[I,12] <- mean(data[n1[I]:n2[I],11])
    ndata[I,13] <- mean(data[n1[I]:n2[I],12])
    ndata[I,14] <- mean(data[n1[I]:n2[I],13])
  }
  return(ndata)
}

# Below are the main parameters for I-DT algorithm
pixdis <- 20      # Pixels for fixation diameter (Max Size)
                  # Dispersion of 50 px in each x and y direction
mindur <- 0.20    # 200 ms as the minimum fixation duration
nsampl <- 60*mindur # Implies 12 samples (60 Hz x mindur)
Maxfix <- 2.5     # Maximum allowed Fixation duration (based on a paper)

subjects <- read.csv("EyetrackProject/SubjectDetails.csv",header=TRUE,sep=",")

Alldata <- NULL
NDrive <- as.data.frame(matrix(0,nrow=28,ncol=9))
colnames(NDrive) <- c("Trial","Dist,km","AvgSpd,km/h","fixs/s","mu(fdur,ms)","mu(sdur,ms)",
                      "mu(SacVel)","Gender","Age")
LDrive <- as.data.frame(matrix(0,nrow=28*3,ncol=16))
colnames(LDrive) <- c("Trial","Stimuli","Dist,km","AvgSpd,km/h","AvSpd_N","AvSpd_S","fixs/s_N",
                      "fixs/s_S","mu(fdurN,ms)","mu(fdurS,ms)","mu(sdurN,ms)","mu(sdurS,ms)",
                      "mu(sdisN,px)","mu(sdisS,px)","mu(svelN,px/s)","mu(svelS,px/s)")
count <- 1

# Loop will read all types of files saved in "EyetrackProject/Subjects/"
# a new user make to ensure the change in directory name if it is any different

for (I in c(1,5,9,13,19,21,4,6,10,12,14,16,20,22,24,44,46,50,51,54,25,33,35,39,41,43)) {
  fpath <- ifelse(I<10,"EyetrackProject/Subjects/T00","EyetrackProject/Subjects/T0")
  file.name <- paste("EyetrackProject/Subject",I,".csv",sep="")
  tdata <- read.csv(file.name,header=TRUE,sep=',')
  
  # readjust to bottom-left corner as origin
  tdata$GazeX <- 1920 - tdata$GazeX 
  tdata$GazeY <- 1080 - tdata$GazeY
  
  colnames(tdata)[1] <- "Subject"
  
  # read *.res file (non-dimensionalized)
  s1name <- paste(fpath,I,"-00",4,".res2",sep="")
  res1 <- read.csv(s1name,header=TRUE)
  res1 <- res1[,1:8]
  res1 <- modres(res1)
  
  # NDrive data is generated for the normal drives
  NDrive[count,1] <- LDrive[(1+3*(count-1)),1] <- I
  LDrive[(2+3*(count-1)),1] <- LDrive[(3+3*(count-1)),1] <- I
  
  NDrive[count,2] <- round(max(res1$dist),2)
  NDrive[count,3] <- round(3600*max(res1$dist)/max(res1$Time),2)
  
  for (J in c(5,6,7)) {
    
    # read *.res2 file (w/ dimensions) and time-stamp file (*.stm)
    # *.stm will read start and end time
    # *.res2 is used for distance calculation
    sname <- paste(fpath,I,"-00",J,".stm",sep="")
    s2name <- paste(fpath,I,"-00",J,".res2",sep="")
    stm <- read.csv(sname,header=TRUE)
    res2 <- read.csv(s2name,header=TRUE)
    res2 <- modres(res2)
    
    # LDrive data is generated for the stimuli drives
    LDrive[(J-4+3*(count-1)),3] <- round(max(res2$dist),2)
    LDrive[(J-4+3*(count-1)),4] <- round(3600*max(res2$dist)/max(res2$Time),2)
    
    t <- c(stm[1,1],stm[1,2],stm[2,1],stm[2,2])
    
    del1s <- max(res2[which(res2$Time <= t[2]),8])-max(res2[which(res2$Time <= t[1]),8])
    del2s <- max(res2[which(res2$Time <= t[4]),8])-max(res2[which(res2$Time <= t[3]),8])
    stime <- t[2]-t[1]+t[4]-t[3]
    LDrive[(J-4+3*(count-1)),6] <- round(3600*(del1s+del2s)/stime,2)
    
    d4 <- rep(0,4)
    for (L in 1:4) {t[L] <- res1[max(which(res1$dist 
                    <=res2[max(which(res2$Time <= t[L])),8])),2]}

    del1n <- max(res1[which(res1$Time <= t[2]),8])-max(res1[which(res1$Time <= t[1]),8])
    del2n <- max(res1[which(res1$Time <= t[4]),8])-max(res1[which(res1$Time <= t[3]),8])
    ntime <- t[2]-t[1]+t[4]-t[3]
    LDrive[(J-4+3*(count-1)),5] <- round(3600*(del1n+del2n)/ntime,2)
    
    # Use of "Readfile" function to get the data and then use idt algorithm
    outdata <- Readfile(tdata,J)
    Nd <- outdata[[1]]
    N1d <- outdata[[2]]
    D1d <- outdata[[3]]
    N2d <- outdata[[4]]
    D2d <- outdata[[5]]
    N3d <- outdata[[6]]
    
    # Use of package 'emov' starts here
    Nfs <- fsdata(emov.idt(Nd$Time,Nd$GazeX,Nd$GazeY,pixdis,nsampl),Nd)
    N1fs <- fsdata(emov.idt(N1d$Time,N1d$GazeX,N1d$GazeY,pixdis,nsampl),N1d)
    N2fs <- fsdata(emov.idt(N2d$Time,N2d$GazeX,N2d$GazeY,pixdis,nsampl),N2d)
    N3fs <- fsdata(emov.idt(N3d$Time,N3d$GazeX,N3d$GazeY,pixdis,nsampl),N3d)
    D1fs <- fsdata(emov.idt(D1d$Time,D1d$GazeX,D1d$GazeY,pixdis,nsampl),D1d)
    D2fs <- fsdata(emov.idt(D2d$Time,D2d$GazeX,D2d$GazeY,pixdis,nsampl),D2d)
    
    # Remove noise (of too long fixations, they are too few to get considered)
    Nfs <- Nfs[which(Nfs$fdur <= Maxfix),]
    N1fs <- N1fs[which(N1fs$fdur <= Maxfix),]
    N2fs <- N2fs[which(N2fs$fdur <= Maxfix),]
    N3fs <- N3fs[which(N3fs$fdur <= Maxfix),]
    D1fs <- D1fs[which(D1fs$fdur <= Maxfix),]
    D2fs <- D2fs[which(D2fs$fdur <= Maxfix),]
    
    # Add columns to NDrive file (Metafile for each driver's normal driving)
    # for LDrive the following if statements are used
    NDrive[count,4] <- round(length(Nfs$fdur)/max(Nd$Time),2)
    NDrive[count,5] <- 1000*round(geometric.mean(Nfs$fdur),3)
    NDrive[count,6] <- 1000*round(geometric.mean(na.omit(Nfs$sdur)),3)
    NDrive[count,7] <- round(geometric.mean(sqrt(na.omit(Nfs$Vx)^2+na.omit(Nfs$Vy)^2)),2)
    NDrive[count,8] <- subjects[count,2]
    NDrive[count,9] <- subjects[count,3]
    
    dstrg <- count

    Nfs <- cbind(trial=I,Nfs,Drive=4,Stimuli="N",Subst="N",count=dstrg,stringsAsFactors=FALSE)
    
    if (J == 5) {
      LDrive[(J-4+3*(count-1)),2] <- "Cognitive"
      Nfs <- modnfix(Nfs,t,"NC")
      D1fs <- cbind(trial=I,D1fs,Drive=J,Stimuli="A",Subst="C",count=dstrg)
      D2fs <- cbind(trial=I,D2fs,Drive=J,Stimuli="M",Subst="C",count=dstrg)
      N1fs <- cbind(trial=I,N1fs,Drive=J,Stimuli="N1",Subst="SC",count=dstrg)
      N2fs <- cbind(trial=I,N2fs,Drive=J,Stimuli="N2",Subst="PC",count=dstrg)
      N3fs <- cbind(trial=I,N3fs,Drive=J,Stimuli="N3",Subst="PC",count=dstrg)

      LDrive[(J-4+3*(count-1)),7] <- round(dim(Nfs[which(Nfs$Subst=="NC"),])[1]/ntime,2)
      LDrive[(J-4+3*(count-1)),8] <- round((length(D1fs$fdur)+length(D2fs$fdur))/stime,2)
      LDrive[(J-4+3*(count-1)),9] <- round(1000*geometric.mean(Nfs[which(Nfs$Subst=="NC"),4]),0)
      LDrive[(J-4+3*(count-1)),10] <- round(1000*geometric.mean(rbind(D1fs,D2fs)[,4]),0)
      LDrive[(J-4+3*(count-1)),11] <- round(1000*geometric.mean(Nfs[which(Nfs$Subst=="NC"),7],na.rm=TRUE),0)
      LDrive[(J-4+3*(count-1)),12] <- round(1000*geometric.mean(rbind(D1fs,D2fs)[,7],na.rm=TRUE),0)
      Sx <- Nfs[which(Nfs$Subst=="NC"),7]*Nfs[which(Nfs$Subst=="NC"),8]
      Sy <- Nfs[which(Nfs$Subst=="NC"),7]*Nfs[which(Nfs$Subst=="NC"),9]
      Sxs <- rbind(D1fs,D2fs)[,7]*rbind(D1fs,D2fs)[,8]
      Sys <- rbind(D1fs,D2fs)[,7]*rbind(D1fs,D2fs)[,9]
      LDrive[(J-4+3*(count-1)),13] <- round(geometric.mean(sqrt(Sx^2+Sy^2)),0)
      LDrive[(J-4+3*(count-1)),14] <- round(geometric.mean(sqrt(Sxs^2+Sys^2),na.rm=TRUE),0)
      LDrive[(J-4+3*(count-1)),15] <- round(geometric.mean(sqrt(Nfs[which(Nfs$Subst=="NC"),8]^2+
                                                          Nfs[which(Nfs$Subst=="NC"),9]^2)),0)
      LDrive[(J-4+3*(count-1)),16] <- round(geometric.mean(sqrt(rbind(D1fs,D2fs)[,8]^2+
                                                          rbind(D1fs,D2fs)[,9]^2),na.rm=TRUE))
      Alldata <- rbind(Alldata,Nfs,N1fs,D1fs,N2fs,D2fs,N3fs)
    } else if (J == 6) {
      Nfs <- modnfix(Nfs,t,"NE")
      LDrive[(J-4+3*(count-1)),2] <- "Emotional"
      D1fs <- cbind(trial=I,D1fs,Drive=J,Stimuli="E1",Subst="E",count=dstrg)
      D2fs <- cbind(trial=I,D2fs,Drive=J,Stimuli="E2",Subst="E",count=dstrg)
      N1fs <- cbind(trial=I,N1fs,Drive=J,Stimuli="N1",Subst="SE",count=dstrg)
      N2fs <- cbind(trial=I,N2fs,Drive=J,Stimuli="N2",Subst="PE",count=dstrg)
      N3fs <- cbind(trial=I,N3fs,Drive=J,Stimuli="N3",Subst="PE",count=dstrg)
      
      LDrive[(J-4+3*(count-1)),7] <- round(dim(Nfs[which(Nfs$Subst=="NE"),])[1]/ntime,2)
      LDrive[(J-4+3*(count-1)),8] <- round((length(D1fs$fdur)+length(D2fs$fdur))/stime,2)
      LDrive[(J-4+3*(count-1)),9] <- round(1000*geometric.mean(Nfs[which(Nfs$Subst=="NE"),4]),0)
      LDrive[(J-4+3*(count-1)),10] <- round(1000*geometric.mean(rbind(D1fs,D2fs)[,4]),0)
      LDrive[(J-4+3*(count-1)),11] <- round(1000*geometric.mean(Nfs[which(Nfs$Subst=="NE"),7],na.rm=TRUE),0)
      LDrive[(J-4+3*(count-1)),12] <- round(1000*geometric.mean(rbind(D1fs,D2fs)[,7],na.rm=TRUE),0)
      Sx <- Nfs[which(Nfs$Subst=="NE"),7]*Nfs[which(Nfs$Subst=="NE"),8]
      Sy <- Nfs[which(Nfs$Subst=="NE"),7]*Nfs[which(Nfs$Subst=="NE"),9]
      Sxs <- rbind(D1fs,D2fs)[,7]*rbind(D1fs,D2fs)[,8]
      Sys <- rbind(D1fs,D2fs)[,7]*rbind(D1fs,D2fs)[,9]
      LDrive[(J-4+3*(count-1)),13] <- round(geometric.mean(sqrt(Sx^2+Sy^2)),0)
      LDrive[(J-4+3*(count-1)),14] <- round(geometric.mean(sqrt(Sxs^2+Sys^2),na.rm=TRUE),0)
      LDrive[(J-4+3*(count-1)),15] <- round(geometric.mean(sqrt(Nfs[which(Nfs$Subst=="NE"),8]^2+
                                                          Nfs[which(Nfs$Subst=="NE"),9]^2)),0)
      LDrive[(J-4+3*(count-1)),16] <- round(geometric.mean(sqrt(rbind(D1fs,D2fs)[,8]^2+
                                                          rbind(D1fs,D2fs)[,9]^2),na.rm=TRUE))
      Alldata <- rbind(Alldata,Nfs,N1fs,D1fs,N2fs,D2fs,N3fs)
    } else if (J == 7) {
      Nfs <- modnfix(Nfs,t,"NT")
      LDrive[(J-4+3*(count-1)),2] <- "Texting"
      D1fs <- cbind(trial=I,D1fs,Drive=J,Stimuli="T1",Subst="T",count=dstrg)
      D2fs <- cbind(trial=I,D2fs,Drive=J,Stimuli="T2",Subst="T",count=dstrg)
      N1fs <- cbind(trial=I,N1fs,Drive=J,Stimuli="N1",Subst="ST",count=dstrg)
      N2fs <- cbind(trial=I,N2fs,Drive=J,Stimuli="N2",Subst="PT",count=dstrg)
      N3fs <- cbind(trial=I,N3fs,Drive=J,Stimuli="N3",Subst="PT",count=dstrg)

      LDrive[(J-4+3*(count-1)),7] <- round(dim(Nfs[which(Nfs$Subst=="NT"),])[1]/ntime,2)
      LDrive[(J-4+3*(count-1)),8] <- round((length(D1fs$fdur)+length(D2fs$fdur))/stime,2)
      LDrive[(J-4+3*(count-1)),9] <- round(1000*geometric.mean(Nfs[which(Nfs$Subst=="NT"),4]),0)
      LDrive[(J-4+3*(count-1)),10] <- round(1000*geometric.mean(rbind(D1fs,D2fs)[,4]),0)
      LDrive[(J-4+3*(count-1)),11] <- round(1000*geometric.mean(Nfs[which(Nfs$Subst=="NT"),7],na.rm=TRUE),0)
      LDrive[(J-4+3*(count-1)),12] <- round(1000*geometric.mean(rbind(D1fs,D2fs)[,7],na.rm=TRUE),0)
      Sx <- Nfs[which(Nfs$Subst=="NT"),7]*Nfs[which(Nfs$Subst=="NT"),8]
      Sy <- Nfs[which(Nfs$Subst=="NT"),7]*Nfs[which(Nfs$Subst=="NT"),9]
      Sxs <- rbind(D1fs,D2fs)[,7]*rbind(D1fs,D2fs)[,8]
      Sys <- rbind(D1fs,D2fs)[,7]*rbind(D1fs,D2fs)[,9]
      LDrive[(J-4+3*(count-1)),13] <- round(geometric.mean(sqrt(Sx^2+Sy^2)),0)
      LDrive[(J-4+3*(count-1)),14] <- round(geometric.mean(sqrt(Sxs^2+Sys^2),na.rm=TRUE),0)
      LDrive[(J-4+3*(count-1)),15] <- round(geometric.mean(sqrt(Nfs[which(Nfs$Subst=="NT"),8]^2+
                                                          Nfs[which(Nfs$Subst=="NT"),9]^2)),0)
      LDrive[(J-4+3*(count-1)),16] <- round(geometric.mean(sqrt(rbind(D1fs,D2fs)[,8]^2+
                                                          rbind(D1fs,D2fs)[,9]^2),na.rm=TRUE))
      Alldata <- rbind(Alldata,Nfs,N1fs,D1fs,N2fs,D2fs,N3fs)
    }
  }
  cat("Fixations & Saccades were created for Subject",I,",# of Trials",count,"\n")
  count <- count + 1
}

# Remove and NaN (generate due to numerial singularity errors)
Alldata <- subset(Alldata,!is.nan(Alldata[,5]))

# assign AOIs
Alldata$AOI <- ifelse(Alldata$x <= 760 & Alldata$y <= 280,"LDwn",Alldata$AOI)
Alldata$AOI <- ifelse(Alldata$x <= 760 & Alldata$y > 280,"Lup",Alldata$AOI)
Alldata$AOI <- ifelse(Alldata$x > 760 & Alldata$x <= 1230 & Alldata$y <= 280,"RdDwn",Alldata$AOI)
Alldata$AOI <- ifelse(Alldata$x > 760 & Alldata$x <= 1230 & Alldata$y > 280 & Alldata$y <= 540,"AtRoad",Alldata$AOI)
Alldata$AOI <- ifelse(Alldata$x > 760 & Alldata$x <= 1230 & Alldata$y > 540,"RdUp",Alldata$AOI)
Alldata$AOI <- ifelse(Alldata$x > 1230 & Alldata$y <= 280,"RDwn",Alldata$AOI)
Alldata$AOI <- ifelse(Alldata$x > 1230 & Alldata$y > 280,"Rup",Alldata$AOI)

# name the output file and write it
fsname <- paste("EyetrackProject/FFixSacData.csv",sep="")
write.csv(Alldata,file=fsname,row.names=FALSE)

# Name and write metadata driving files
fsname1 <- paste("EyetrackProject/NDrive4.csv",sep="")
fsname2 <- paste("EyetrackProject/LDrive5.csv",sep="")
fsname3 <- paste("EyetrackProject/LDrive6.csv",sep="")
fsname4 <- paste("EyetrackProject/LDrive7.csv",sep="")

write.csv(NDrive,file=fsname1,row.names=FALSE)
write.csv(LDrive[which(LDrive$Stimuli == "Cognitive"),],file=fsname2,row.names=FALSE)
write.csv(LDrive[which(LDrive$Stimuli == "Emotional"),],file=fsname3,row.names=FALSE)
write.csv(LDrive[which(LDrive$Stimuli == "Texting"),],file=fsname4,row.names=FALSE)

# End of the program with 5 different files created in directory
