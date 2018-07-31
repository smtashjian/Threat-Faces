library(readxl)
library(lattice)
library(lme4)
rm(list = ls())
setwd("~/Dropbox/Rachel/SAS_Threat/RT/")

data <- read.table(paste("allRT.txt",sep=""), header=FALSE)
colnames(data) <-c("rank","response","RT", "RTTIME", "id", "order")

data$id <- as.numeric(data$id)

data$RT <- data$RT/1000

str(data)

#center_colmeans <- function(x) {
#  xcenter = colMeans(x)
#  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
#}
#data2 <- center_colmeans(data)
#str(data2)

linear <- lmer(RT ~ order + (1 |id), data=data, control=lmerControl(optimizer="bobyqa"))
summary(linear)

cor.test(RT, order)

plot_model(linear, vline.color="red", show.values=TRUE, value.offset = .3)
quartz("Reaction time")
xyplot(RT~order |id , groups=rank, data=data, type=c('p','r'), auto.key=F)
xyplot(fitted(linear)~data$order |id , groups=id, data=data, type=c('p','r'), auto.key=F)
plot_model(linear)

library(languageR)
plotLMER.fnc(linear, ylimit=0:1, lockYlim=TRUE, linecolor="red", xlabel="Time on Task", ylabel="Reaction Time")

plot_model(linear, type="re", terms="order")




  setwd("~/Dropbox/Rachel/SAS_Threat/RT/earlyRT/")

dataearly <- read.table(paste("allearlyRT.txt",sep=""), header=FALSE)
colnames(dataearly) <-c("rank","response","RT", "RTTIME", "id", "order")

setwd("~/Dropbox/Rachel/SAS_Threat/RT/lateRT/")

datalate <- read.table(paste("alllateRT.txt",sep=""), header=FALSE)
colnames(datalate) <-c("rank","response","RT", "RTTIME", "id", "order")

dataearly$RT <- dataearly$RT/1000
datalate$RT <- datalate$RT/1000

#center_colmeans <- function(x) {
#  xcenter = colMeans(x)
#  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
#}
#data2 <- center_colmeans(data)
#str(data2)

linearearly <- lmer(RT ~ order + (1 |id), data=dataearly, control=lmerControl(optimizer="bobyqa"))
summary(linearearly)

linearlate <- lmer(RT ~ order + (1 |id), data=datalate, control=lmerControl(optimizer="bobyqa"))
summary(linearlate)

yearly<- as.data.frame((as.numeric(dataearly$RT)))
ylate<- as.data.frame((as.numeric(datalate$RT)))
earlyorder <-as.data.frame((as.numeric(dataearly$order)))
lateorder <-as.data.frame((as.numeric(datalate$order)))
colnames(yearly) <-c("col1")
colnames(ylate) <-c("col1")
colnames(earlyorder) <-c("col1")
colnames(lateorder) <-c("col1")
idearly <- as.data.frame(as.numeric(dataearly$id))
idlate <- as.data.frame(as.numeric(datalate$id))

df1 <- cbind(yearly, earlyorder, idearly)
colnames(df1) <-c ("RT", "order", "id")
df2 <- cbind(ylate, lateorder, idlate)
colnames(df2) <-c ("RT", "order", "id")

dftotal <- rbind(df1, df2)

#cor(df1, method="pearson")
#cor.test(df1, method="pearson")

library(nlme)
linear <- lme(RT ~order, random=~1 |id, data=df1)
anova(linear)
summary(linear)

linear2 <- lme(RT ~order, random=~1 |id, data=df2)
anova(linear2)
summary(linear2)

linear3 <- lme(RT ~order, random=~1 |id, data=dftotal)
anova(linear3)
summary(linear3)

library(miscTools)
mean(df1$RT)
mean(df2$RT)
t.test(df1$RT, df2$RT, paired=TRUE)

quadfirsthalf <- lme(RT ~order + I(order^2), random=~1 +order +I(order^2)| id,  data=df1)
summary(quadfirsthalf)

quadsecondhalf <- lme(RT ~order + I(order^2), random=~1 +order| id,  data=df2)
summary(quadsecondhalf)

lineartotal <-lme(RT ~order, random=~1|id, data=dftotal)
anova(lineartotal)

theme_set((base_size = 18))
xyplot(df1$RT~df1$order , groups=id, data=df1, type=c('p','r'), auto.key=F, xlab="Trial Number", ylab="Reaction Time (seconds)", main="Reaction Time and Order of Face Presentation")
xyplot(df2$RT~df2$order , groups=id, data=df2, type=c('p','r'), auto.key=F, xlab="Trial Number", ylab="Reaction Time (seconds)", main="Reaction Time and Order of Face Presentation")
xyplot(dftotal$RT~dftotal$order , groups=id, data=dftotal, type=c('p','r'), auto.key=F, xlab="Trial Number", ylab="Reaction Time (seconds)", main="Reaction Time and Order of Face Presentation")
xyplot(fitted(linear)~df1$order , groups=id, data=df1, type=c('p','r'), auto.key=F, xlab=list(label="Trial Number", cex=2.5), ylab=list(label="Reaction Time (seconds)", cex=2.5), scales=list(tck=c(1,1), x=list(cex=2), y=list(cex=2)))
xyplot(fitted(linear2)~df2$order , groups=id, data=df2, type=c('p','r'), auto.key=F,  xlab=list(label="Trial Number", cex=2.5), ylab=list(label="Reaction Time (seconds)", cex=2.5), scales=list(tck=c(1,1), x=list(cex=2), y=list(cex=2)))
xyplot(fitted(linear3)~dftotal$order , groups=id, data=dftotal, type=c('p','r'), auto.key=F,  xlab=list(label="Trial Number", cex=2.5), ylab=list(label="Reaction Time (seconds)", cex=2.5), scales=list(tck=c(1,1), x=list(cex=2), y=list(cex=2)))


#t.test (yearly, ylate)
#cor.test(yearly, dataearly$order, method=c("pearson"), conf.level=0.95)
#cor.test(ylate, datalate$order, method=c("pearson"), conf.level=0.95)

