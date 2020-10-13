
# Program Eye-tracking Capstone Project
# Date: Aug 1st, 2017
# Author: Shantanu Neema
# Program to generat AOI data (For Tableau)

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

interpol <- function(eye,tout,res) {
  dat2 <- as.data.frame(matrix(0,nrow=nrow(eye),ncol=7))
  tout <- seq(0,(nrow(eye)-1)/60,1/60)
  dat2[,1] <- tout*60+1
  dat2[,2] <- tout
  for (I in 3:7) {
    inter <- approx(res$Time,res[,I],tout,method="linear",rule=1)
    dat2[,I] <- inter$y
    rm(inter)
  }
  colnames(dat2) <- c("Frame","Time","Speed","Acceleration",
                      "Breaking","Steering","Lane.Position")
  return(dat2)
}

Allfix <- read.csv("EyetrackProject/FinData_wAOI.csv",header=TRUE,sep=",")

newdata <- NULL
count <- 1
for (I in c(1,5,9,13,19,21,4,6,10,12,14,16,20,22,24,44,46,50,51,54,25,33,35,39,41,43)) {
  subdat <- Allfix[which(Allfix$trial == I),]
  distvec <- rep(0,length(subdat$fdur))
  
  fpath <- ifelse(I<10,"EyetrackProject/Subjects/T00","EyetrackProject/Subjects/T0")
  s1name <- paste(fpath,I,"-00",4,".res2",sep="")
  res1 <- read.csv(s1name,header=TRUE)
  res1 <- res1[,1:8]
  res1 <- modres(res1)
  
  ndata <- subdat[which(subdat$Drive == 4),]
  ndata1 <- ndata[(which(ndata$start == min(ndata$start))[1]):
                     (which(ndata$start == max(ndata$start))[1]),]
  ndata2 <- ndata[(which(ndata$start == min(ndata$start))[2]):
                     (which(ndata$start == max(ndata$start))[2]),]
  ndata3 <- ndata[(which(ndata$start == min(ndata$start))[3]):
                     (which(ndata$start == max(ndata$start))[3]),]
  
  inter <- approx(res1$Time,res1$dist,ndata1$start,method="linear",rule=1)
  disvec <- inter$y
  
  ndata1 <- cbind(ndata1,dist = disvec)
  ndata2 <- cbind(ndata2,dist = disvec)
  ndata3 <- cbind(ndata3,dist = disvec)
  
  for (J in c(5,6,7)) {
    
    sdata <- subdat[which(subdat$Drive == J),]
    sname <- paste(fpath,I,"-00",J,".res2",sep="")
    res2 <- read.csv(sname,header=TRUE)
    res2 <- modres(res2)
    
    inter <- approx(res2$Time,res2$dist,sdata$start,method="linear",rule=1)
    disvec <- inter$y
    sdata <- cbind(sdata,dist = disvec)
    
    if (J == 5) {newdata <- rbind(newdata,ndata1,sdata)}
    if (J == 6) {newdata <- rbind(newdata,ndata2,sdata)}
    if (J == 7) {newdata <- rbind(newdata,ndata3,sdata)}

  }
  cat("AOI data created for Trial ",I," # of trials = ",count,sep="","\n")
  count <- count + 1
}

write.csv(newdata,file="EyetrackProject/DatawDist.csv",row.names = FALSE)
library('plotrix')
ddata <- read.csv("EyetrackProject/Subject43.csv",header=TRUE,sep=",")
ddata <- ddata[which(ddata$Drive == 5),]
sdata <- subdat[which(subdat$Drive == 5),]

plot(ddata$Time,ddata$GazeX+195,type='l',col='blue',lwd=1.5,xlim=c(510,540),ylim=c(0,1920),
     xlab="Time (s)",ylab="Horizontal Gaze (X)",main="Original vs Processed Data")
mtext("Horizontal Gaze (Participant # 43)")
legend("topleft",legend=c("Original","Processed"),col=c("blue","red"),
       lwd=2.5,bty="n",cex=0.85)
lines(sdata$start,sdata$x,col='red',lwd=2)

for (I in 1:dim(sdata)[1]) {
  draw.circle(sdata$start[I],sdata$x[I],sdata$fdur[I],
              border='red',col=adjustcolor('red',alpha.f=0.1))}

plot(ddata$Time,1080-ddata$GazeY,type='l',col='blue',lwd=1.5,xlim=c(510,540),ylim=c(0,1080),
     xlab="Time (s)",ylab="Vertical Gaze (Y)",main="Original vs Processed Data")
mtext("Vertical Gaze (Participant # 43)")
legend("topleft",legend=c("Original","Processed"),col=c("blue","red"),
       lwd=2.5,bty="n",cex=0.85)
lines(sdata$start,980-sdata$y,col='red',lwd=2)
# points(sdata$start,980-sdata$y,col='red3',pch=20)
for (I in 1:dim(sdata)[1]) {
  draw.circle(sdata$start[I],980-sdata$y[I],sdata$fdur[I],
              border='red',col=adjustcolor('red',alpha.f=0.1))}

plot(ddata$GazeX[30601:30801]+195,1080-ddata$GazeY[30601:30801],type='l',col='blue',lwd=1.5,
     xlab="Time (s)",ylab="Vertical Gaze (Y)",main="Original vs Processed Data")
mtext("Eye Gaze (Participant # 43)")
legend("topright",legend=c("Original","Processed"),col=c("blue","red"),
       lwd=2.5,bty="n",cex=0.85)
lines(sdata$x[512:530],980-sdata$y[512:530],col='red',lwd=2)
points(sdata$x[512:530],980-sdata$y[512:530],col='red3',pch=20)
for (I in c(512:530)) {
  draw.circle(sdata$x[I],980-sdata$y[I],sdata$fdur[I],
              border='red',col=adjustcolor('red',alpha.f=0.1))}

ddata <- read.csv("EyetrackProject/Subject43.csv",header=TRUE,sep=",")
ddata <- ddata[which(ddata$Drive == 5),]
sdata <- subdat[which(subdat$Drive == 5),]
plot(ddata$Time,ddata$Steering,type='l',col='blue',lwd=3,xlim=c(200,500),
     xlab="Time (s)",ylab="Lane Position",main="Original vs Processed Data")
mtext("Steering (Participant # 43)")
legend("topleft",legend=c("Original","Processed"),col=c("blue","red"),
       lwd=2.5,bty="n",cex=0.85)
lines(sdata$start,sdata$strg,col='red',lwd=2)

drdata <- read.csv("EyetrackProject/DatawDist.csv",header=TRUE,sep=",")

write.csv(Alldata,"EyetrackProject/DatawAOI.csv",row.names = FALSE)

fdata <- drdata[-which(drdata$Subst == "NSE"),]
fdata <- fdata[-which(fdata$Subst == "NPE"),]
fdata <- fdata[-which(fdata$Subst == "NST"),]
fdata <- fdata[-which(fdata$Subst == "NPT"),]
fdata <- fdata[-which(fdata$Subst == "NPC"),]
fdata <- fdata[-which(fdata$Subst == "NSC"),]
fdata <- fdata[-which(fdata$Subst == "PC"),]
fdata <- fdata[-which(fdata$Subst == "PE"),]
fdata <- fdata[-which(fdata$Subst == "PT"),]
fdata <- fdata[-which(fdata$Subst == "SC"),]
fdata <- fdata[-which(fdata$Subst == "SE"),]
fdata <- fdata[-which(fdata$Subst == "ST"),]


for (I in 1:26) {
  dat <- fdata[which(fdata$count == I),]
  datNC <- dat[which(dat$Subst == "NC"),]
  datC <- dat[which(dat$Subst == "C"),]
  datNE <- dat[which(dat$Subst == "NE"),]
  datE <- dat[which(dat$Subst == "E"),]
  datNT <- dat[which(dat$Drive == 7),]
  datT <- dat[which(dat$Drive == 7),]
}


dat <- drdata[which(drdata$count == 1),]
datNT <- drdata[which(dat$Drive == 4),]
datNT <- datNT[-which(datNT$Subst == "NE"),]
datNT <- datNT[-which(datNT$Subst == "NPE"),]
datNT <- datNT[-which(datNT$Subst == "NSE"),]
datNT <- datNT[-which(datNT$Subst == "NT"),]
datNT <- datNT[-which(datNT$Subst == "NST"),]
datNT <- datNT[-which(datNT$Subst == "NPT"),]
datT <- drdata[which(dat$Drive == 6),]

par(mfrow=c(2,1))
plot(datNT$dist,datNT$spd,type='l',col='red',lwd=2,ylim=c(-1,1.40),xlab="Distance (km)",
     ylab="",xlim=c(0,10))
lines(datNT$dist,datNT$accn,col='skyblue3',lwd=2)
lines(datNT$dist,datNT$brkg,col='orange',lwd=2)
lines(datNT$dist,datNT$strg,col='green',lwd=2)
lines(datNT$dist,datNT$lane,col='purple',lwd=2)
legend("topleft",legend=c("Speed","Acceleration","Breaking","Steering","Lane Postion"),
       col=c("red","skyblue3","orange","green","purple"),horiz=TRUE,
       lwd=2.5,bty="n",cex=0.85)

plot(datT$dist,datT$spd,type='l',col='red',lwd=2,ylim=c(-1,1.40),xlab="Distance (km)",
     ylab="",xlim=c(0,10))
lines(datT$dist,datT$accn,col='skyblue3',lwd=2)
lines(datT$dist,datT$brkg,col='orange',lwd=2)
lines(datT$dist,datT$strg,col='green',lwd=2)
lines(datT$dist,datT$lane,col='purple',lwd=2)
legend("topleft",legend=c("Speed","Acceleration","Breaking","Steering","Lane Postion"),
       col=c("red","skyblue3","orange","green","purple"),horiz=TRUE,
       lwd=2.5,bty="n",cex=0.85)

Allfix <- Alldata
table(Allfix$Subst)
Allfix <- Allfix[-which(Allfix$Subst == "NE"),]
Allfix <- Allfix[-which(Allfix$Subst == "NPE"),]
Allfix <- Allfix[-which(Allfix$Subst == "NSE"),]
Allfix <- Allfix[-which(Allfix$Subst == "NT"),]
Allfix <- Allfix[-which(Allfix$Subst == "NPT"),]
Allfix <- Allfix[-which(Allfix$Subst == "NST"),]

bw600 <- Allfix[which(Allfix$fdur <= 0.60),]
bw1000 <- Allfix[which(Allfix$fdur <= 1),]







