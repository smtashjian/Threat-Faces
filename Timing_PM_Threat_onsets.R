rm(list=ls())

setwd("~/Dropbox/Rachel/ThreatOnsets/")

subs=c("101","102","103", "105", "106", "107", "108", "109","110", "111", "112", "113",
      "114","115","116","117","118",
     "119","120","121","122","123","125","126","127",
    "128","129","130","131","132","134","135", "137", "138",
   "140","141","142","143","145","146","147","148",
  "149","150","151","152","156")

for(i in 1:length(subs)) {
  
  facesPM <- read.table(paste("AllFaces_",subs[i],"_PM.txt",sep=""), header=FALSE)
  
  sub <-append(sub, subs[i]) 
  
  attach(facesPM)
  
df1 <- facesPM[1:3,]
df2 <- facesPM[11:13,]
df3 <- facesPM[21:23,]
df4 <- facesPM[31:33,]
df5 <- facesPM[41:43,]
df6 <- facesPM[51:53,]
df7 <- facesPM[61:63,]

all_early = rbind(df1, df2, df3, df4, df5, df6, df7)

dfl1 <- facesPM[8:10,]
dfl2 <- facesPM[18:20,]
dfl3 <- facesPM[28:30,]
dfl4 <- facesPM[38:40,]
dfl5 <- facesPM[48:50,]
dfl6 <- facesPM[58:60,]
dfl7 <- facesPM[68:70,]

all_late = rbind(dfl1, dfl2, dfl3, dfl4, dfl5, dfl6, dfl7)

attach(all_early)
attach(all_late)

earlyface_onset=as.data.frame(all_early[[1]])
earlyface_duration=as.data.frame(all_early[[2]])
lateface_onset=as.data.frame(all_late[[1]])
lateface_duration=as.data.frame(all_late[[2]])

earlyfaces_1=cbind(earlyface_onset, earlyface_duration, c(rep(1, length(earlyface_onset))))
latefaces_1=cbind(lateface_onset, lateface_duration, c(rep(1, length(lateface_onset))))

#1 to 3 is early, 4 to 10 is the rest

dfre1 <- facesPM[4:10,]
dfre2 <- facesPM[14:20,]
dfre3 <- facesPM[24:30,]
dfre4 <- facesPM[34:40,]
dfre5 <- facesPM[44:50,]
dfre6 <- facesPM[54:60,]
dfre7 <- facesPM[64:70,]

all_restearly = rbind(dfre1, dfre2, dfre3, dfre4, dfre5, dfre6, dfre7)

attach(all_restearly)

restearlyface_onset=as.data.frame(all_restearly[[1]])
restearlyface_duration=as.data.frame(all_restearly[[2]])

restearlyfaces_1=cbind(restearlyface_onset, restearlyface_duration, c(rep(1, length(restearlyface_onset))))

#8 to 10 is late, 1 to 7 is the rest

dfrl1 <- facesPM[1:7,]
dfrl2 <- facesPM[11:17,]
dfrl3 <- facesPM[21:27,]
dfrl4 <- facesPM[31:37,]
dfrl5 <- facesPM[41:47,]
dfrl6 <- facesPM[51:57,]
dfrl7 <- facesPM[61:67,]

all_restlate = rbind(dfrl1, dfrl2, dfrl3, dfrl4, dfrl5, dfrl6, dfrl7)

attach(all_restlate)

restlateface_onset=as.data.frame(all_restlate[[1]])
restlateface_duration=as.data.frame(all_restlate[[2]])

restlatefaces_1=cbind(restlateface_onset, restlateface_duration, c(rep(1, length(restlateface_onset))))


#write.table(all_early, file=paste("EarlyFaces_",subs[i],"_PM", ".txt",sep=""), col.names=FALSE, row.names = FALSE, quote=FALSE)
#write.table(all_late, file=paste("LateFaces_",subs[i],"_PM", ".txt",sep=""), col.names=FALSE, row.names = FALSE, quote=FALSE)
write.table(latefaces_1, file=paste("LateFaces_1_",subs[i],"_PM", ".txt",sep=""), col.names=FALSE, row.names = FALSE, quote=FALSE)
write.table(earlyfaces_1, file=paste("EarlyFaces_1_",subs[i],"_PM", ".txt",sep=""), col.names=FALSE, row.names = FALSE, quote=FALSE)
write.table(restearlyfaces_1, file=paste("RestEarlyFaces_1_",subs[i],"_PM", ".txt",sep=""), col.names=FALSE, row.names = FALSE, quote=FALSE)
write.table(restlatefaces_1, file=paste("RestLateFaces_1_",subs[i],"_PM", ".txt",sep=""), col.names=FALSE, row.names = FALSE, quote=FALSE)

}


rm(list=ls())

setwd("~/Dropbox/Rachel/ThreatOnsets/")

subs=c("101","102","103", "105", "106", "107", "108", "109","110", "111", "112", "113",
       "114","115","116","117","118",
       "119","120","121","122","123","125","126","127",
       "128","129","130","131","132","134","135", "137", "138",
       "140","141","142","143","145","146","147","148",
       "149","150","151","152","156")



for(i in 1:length(subs)) {
  
  EarlyfacesPM <- read.table(paste("EarlyFaces_",subs[i],"_PM.txt",sep=""), header=FALSE)
  LatefacesPM <- read.table(paste("LateFaces_", subs[i], "_PM.txt", sep=""), header=FALSE)
  
  sub <-append(sub, subs[i]) 
  
 ## attach(EarlyfacesPM)
##  attach(LatefacesPM)
  
   EarlyfacesPM$V3[EarlyfacesPM$V3 < 2] <- -1.6335557
   EarlyfacesPM$V3[EarlyfacesPM$V3 < 3 & EarlyfacesPM$V3 > 1] <- -1.6920806
   EarlyfacesPM$V3[EarlyfacesPM$V3 < 4 & EarlyfacesPM$V3 >2] <- -1.5494455
   EarlyfacesPM$V3[EarlyfacesPM$V3 < 5 & EarlyfacesPM$V3 >3] <- -1.2056504
   EarlyfacesPM$V3[EarlyfacesPM$V3 < 6 & EarlyfacesPM$V3 > 4] <- -0.6606953
   EarlyfacesPM$V3[EarlyfacesPM$V3 < 7 & EarlyfacesPM$V3 > 5] <-  0.08541984
  EarlyfacesPM$V3[EarlyfacesPM$V3 > 6] <-  1.03269495
  
  LatefacesPM$V3[LatefacesPM$V3 < 2] <- -1.6335557
  LatefacesPM$V3[LatefacesPM$V3 < 3 & LatefacesPM$V3 >1] <- -1.6920806
  LatefacesPM$V3[LatefacesPM$V3 < 4 & LatefacesPM$V3> 2] <- -1.5494455
  LatefacesPM$V3[LatefacesPM$V3 < 5 & LatefacesPM$V3 >3] <- -1.2056504
  LatefacesPM$V3[LatefacesPM$V3 < 6 & LatefacesPM$V3 > 4] <- -0.6606953
  LatefacesPM$V3[LatefacesPM$V3 < 7 & LatefacesPM$V3 > 5] <-  0.08541984
  LatefacesPM$V3[LatefacesPM$V3 > 6] <-  1.03269495
  
  write.table(LatefacesPM, file=paste("LateFaces_quad_",subs[i],"_PM", ".txt",sep=""), col.names=FALSE, row.names = FALSE, quote=FALSE)
  write.table(EarlyfacesPM, file=paste("EarlyFaces_quad_",subs[i],"_PM", ".txt",sep=""), col.names=FALSE, row.names = FALSE, quote=FALSE)
  
}
 