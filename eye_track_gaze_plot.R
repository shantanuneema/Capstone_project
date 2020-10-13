# Program Eye-tracking Capstone Project
# Date: August 30th, 2017
# Author: Shantanu Neema
# Program to evaluate fixations for Tableau (Dispersion based algorithm)

library('fpc')
library('png')
library('zoo')

subjects <- read.csv("EyetrackProject/SubjectDetails.csv",header=TRUE,sep=",")
Alldata <- read.csv("EyetrackProject/FFixSacData.csv",header=TRUE,sep=",")

dat <- NULL
count <- 1
for (I in c(1,5,9,13,19,21,4,6,10,12,14,16,20,22,24,44,46,50,51,54,25,33,35,39,41,43)) {
  file.name <- paste("EyetrackProject/Subject",I,".csv",sep="")
  sdata <- read.csv(file.name,header=TRUE,sep=',')
  sdata <- sdata[which(sdata$Drive == 5),]

  sdata$GazeX <- 1920 - sdata$GazeX
  sdata$GazeY <- 1080 - sdata$GazeY

  dat <- rbind(dat,sdata)
  cat("Data stored for ",count," Subjects","\n")
  count <- count + 1
}

library('png')
ima <- readPNG("EyetrackProject/Images.png")
plot(NULL,type="n",main="Normal Drive, Subject 14",
     xlab="GazeX",ylab="GazeY",xlim=c(0,1920),ylim=c(0,1080))
lim <- par()
rasterImage(ima,lim$usr[1],lim$usr[3],lim$usr[2],lim$usr[4])
points(sub14$GazeX,1080-sub14$GazeY,col='red',pch=20,cex=0.75)







