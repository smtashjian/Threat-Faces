#install.packages("lme4")

#How can we geometrically interpret the various b's in the equation for a quadratic trend?
#b0 = expected value of Y when all predictors are at 0
#b1 = instantaneous linear change at X = 0
#b2 = instaneous linear change is changing at a rate of b2. b2 is acceleration of the quadratic relation between Y and X. 


library(lme4)
rm(list = ls())
setwd("~/Dropbox/Rachel/SAS_Threat/")

data <- read.table(paste("allthreat_agegender.txt",sep=""), header=FALSE)
colnames(data) <-c("rank","rating","id", "rankquad", "age", "gender")

str(data)

quad <- glmer(rating ~ rank + rankquad + (1 + rank + rankquad |id), family=binomial("logit"), data=data, control=glmerControl(optimizer="bobyqa"))
summary(quad)

quad_L2 <- glmer(rating ~ rank + rankquad + gender + (1 + rank + rankquad |id), family=binomial("logit"), data=data, control=glmerControl(optimizer="bobyqa"))
summary(quad_L2)

anova(quad, quad_L2)

quad3 <- glmer(rating ~ rank + I(rank^2)  + (1 + rank + I(rank^2) |id), family=binomial("logit"), data=data, control=glmerControl(optimizer="bobyqa"))
summary(quad3)

quartz(title="ranking")

plot(data3$rank,data3$rating,xlab="ranking",ylab="probability of threat") # plot with body size on x-axis and survival (0 or 1) on y-axis
g=glm(rating~rank,family=binomial,data3) # run a logistic regression model (in this case, generalized linear model with logit link). see ?glm

curve(predict(g,data.frame(rank=x),type="resp"),add=TRUE) # draws a curve based on prediction from logistic regression model

points(data3$rank,fitted(g),pch=20) # optional: you could skip this draws an invisible set of points of body size survival based on a 'fit' to glm model. pch= changes type of dots.

install.packages("popbio")
library(popbio)
quartz(title="histogram")
logi.hist.plot(data3$rank,data3$rating,boxp=FALSE,type="hist",col="gray")


#MAKING MEAN-CENTER ZERO POINT TO INTERPRET LINEAR TERM

data <- read.table(paste("allthreat_agegender.txt",sep=""), header=FALSE)
colnames(data) <-c("rank","rating","id", "rankquad", "age", "gender")

str(data)

center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
}
data2 <- center_colmeans(data)
str(data2)

data3 <- as.data.frame(cbind(data$rating, data2$rank, I(data2$rank^2), data$id))
str(data3)
colnames(data3) <-c("rating", "rank","rankquad","id")

quad4 <- glmer(rating ~ rank + rankquad + (1 + rank + rankquad |id), family=binomial("logit"), data=data3, control=glmerControl(optimizer="bobyqa"))
quartz("quadratic") 
summary(quad4)

linear <- glmer(rating ~ rank + (1 + rank |id), family=binomial("logit"), data=data3, control=glmerControl(optimizer="bobyqa"))
summary(linear)

quad5 <- glmer(rating ~ rank + I(rank^2) + (1 + rank + I(rank^2) |id), family=binomial("logit"), data=data3, control=glmerControl(optimizer="bobyqa"))
summary(quad5)


exponential <- glmer((log(rating))~rank (1|id), family=binomial("logit"), data=data3)

x = data3$rating
y = data3$rank

exp(x)

#install.packages("vcd")
library(minpack.lm)
library(vcd)
install.packages("MASS")
library(MASS)
plot(data3$rating~data3$rank)

x = as.data.frame(data3$rank)
xmatrix <- Matrix(c(x), nc=1)

exponents = x[,1] <- exp(x[,1])
exponents<-as.data.frame(exponents)

exponential <- glmer(data3$rating ~ exponents$exponents + (1 + exponents$exponents |data3$id), family=binomial("logit"), control=glmerControl(optimizer="bobyqa"))
summary(exponential)

anova(quad4,linear)

coef(exponential)
predprob <-fitted(exponential)

##########GET COEFFICIENTS##############
coef(quad4)$id
intercepts <-as.data.frame(coef(quad4)$id[,1])
slopes <-as.data.frame(coef(quad4)$id[,3])
rank<- as.data.frame(data$rank)
rating<- as.data.frame(data$rating)
id <-as.data.frame(data$id)

##########################################
install.packages("languageR")
library(languageR)
plotLMER.fnc(linear2, ylimit=0:1, lockYlim=TRUE, linecolor="red", xlabel="Valence", ylabel="Probability of Threat")

plot_model(linear2, type="re", terms="rank")

linear2 <- glmer(rating ~ rank + (rank+1  |id), family=binomial("logit"), data=data3, control=glmerControl(optimizer="bobyqa"))
summary(linear2)

xyplot( ~ rank, data = datapred, groups=id, type =c("p", "1", "g"), col="blue", xlim=c(0,8), ylim=c(-4,4))

predprob <-fitted(quad4)
predprob

#install.packages("VGAM")
library(VGAM)
#install.packages("lattice")
library(lattice)

#PLOT THE RESULTS WITH RANK ON THE X AND LOG-ODDS OF RATING A FACE AS THREATENING ON Y
#WITH EACH DATA POINT BEING A PARTICIPANT
#RESULTS INDICATE THE LOG-ODDS OF RATING A FACE AS THREATENING FOR VALENCE 7 RANGE FROM -2 TO 3.5
#INDICATING THERE ARE STRONG INDIVIDUAL EFFECTS

predprob <-fitted(quad4)
predlogit <-logit(predprob)
datapred <- unique(data.frame(cbind(predlogit = predlogit, id=data$id, rank=data$rank)))
quartz("quadratic")
xyplot(predlogit ~ rank, data = datapred, groups=id, type =c("p", "1", "g"), xlim=c(0,8), ylim=c(-8,8))


predprob2 <-fitted(quad4)
predlogit2 <- logit(predprob2)
datapred <- unique(data.frame(cbind(predlogit2 = predlogit2, id = data3$id, rank=data3$rank)))
xyplot(predlogit2 ~ rank, data = datapred, groups = id, type = c("p", "1", "g"), xlim = c(-4, 4), ylim = c (-8, 8))

anova(linear2, quad4)

##########################################
#PLOT LINEAR MODEL https://stats.stackexchange.com/questions/275103/visualizing-a-multilevel-model-hlm-in-ggplot2
coef(linear)$id
intercepts <-coef(linear)$id[,1]
slopes <-coef(linear)$id[,2]
geom_abline(slopes=slopes, intercepts=intercepts)
summary(linear)$coef

ggplot(data, aes(x=rank, y=rating, group=id)) +
  stat_smooth(method="lm", se=FALSE, size=.5, color="springgreen") +
  stat_smooth(aes(group=1), method="lm", color="blue", size=1.5) 

#PLOT QUADRATIC MODEL
library(ggplot2)
library(dplyr)
library(plyr)
library(ggplot2)
library(easyGgplot2)

coef(quad4)$id
intercepts <-as.data.frame(coef(quad4)$id[,1])
slopes <-as.data.frame(coef(quad4)$id[,3])
rank<- as.data.frame(data$rank)
rating<- as.data.frame(data$rating)
id <-as.data.frame(data$id)

p <- ggplot(mapping = aes(x= rank, y= rating, color=id)) 
geom_abline(data=p, aes(slope=slopes), aes(intercept=intercepts))



  geom_point(shape=20) +
  theme(legend.position="none")+
   
  #geom_abline(slope=(summary(quad4)$coef))

pXt <- expand.grid(rank=seq(min(data$rank), max(data$rank), by=1))
pYt <- modelPredictions(quad4, pXt)
agg <-aggregate(data,by=data[,c("id", "rank")], FUN=mean)
plot <- ggplot(data, aes(x=rank, y=Predicted))+
  geom_point(data=data, aes(y=rating), color=color1, position=position_jitter(width=1, heigh=.05), size=1.5, alpha=.4)+
  geom_smooth(aes(ymin=CILo, ymax=CIHi), stat="id", size=.9, color=color2)+
  theme_bw()+labs(x="Valence",y="Probability Responded 'Threatening'")+
  theme(text=element_text(size=12, family="Times"))+
  guides(linetype=guide_legend(override.aes=list(alpha=1)))+
  scale_x_continuous(breaks=seq(-50,50,10),labels=seq(0,100,10))+scale_y_continuous(limits=c(0,1.05),breaks=seq(0,1,.2))
plot

pXt <- expand.grid(StrengthG=seq(min(dtrain$StrengthG),max(dtrain$StrengthG),by=1))
pYt <-modelPredictions(modlogtrainG, pXt)
dtrainagg <-aggregate(dtrain,by=dtrain[,c("participant","StrengthG")],FUN=mean)
plot<- ggplot(data, aes(x=StrengthG,y=Predicted))+
  geom_point(data=dtrainagg, aes(y=EmoCod),color=color1, position=position_jitter(width=1, height=.05),size=1.5,alpha=.4)+
  geom_smooth(aes(ymin=CILo,ymax=CIHi), stat="identity",size=.9,color=color2)+
  theme_bw()+labs(x="Amount of Anger",y="Probability Responded 'Upset'")+
  theme(text=element_text(size=12, family="Times"))+
  guides(linetype=guide_legend(override.aes=list(alpha=1)))+
  scale_x_continuous(breaks=seq(-50,50,10),labels=seq(0,100,10))+scale_y_continuous(limits=c(0,1.05),breaks=seq(0,1,.2))
plot

#install.packages("sjPlot")
library(sjPlot)
library(sjmisc)
library(sjlabelled)

set_theme(theme = "forest", 
          geom.label.size = 3, 
          axis.textsize = .9, 
          axis.title.size = .9)

sjp.glmer(quad4, type="fe.prob")


plot_model(quad4, type = "re") 
all.plots<-plot_model(linear,type="re")
all.plots$plot.list[[1]] 

head(fitted(linear, level=0))
plot(fitted(linear, level=0))
plotDF <- data.frame(data = data$rank, fitted=fitted(quad4, level=0))
plot(plotDF)
plot(fitted(quad4))
par(mar=c(1,1,1,1))
###################################


#MAKING ZERO POINT AS LOWEST VALUE
data$rank[data$rank==1]<-0
data$rank[data$rank==2]<-1
data$rank[data$rank==3]<-2
data$rank[data$rank==4]<-3
data$rank[data$rank==5]<-4
data$rank[data$rank==6]<-5
data$rank[data$rank==7]<-6

quad5 <- glmer(rating ~ rank + I(rank^2) + (1 + rank + I(rank^2) |id), family=binomial("logit"), data=data, control=glmerControl(optimizer="bobyqa"))
summary(quad5)
