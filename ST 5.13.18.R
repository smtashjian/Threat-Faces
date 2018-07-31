#Tooth decay in 7257 children 12-14 years old in 21 communities as a function of floride concentration
#threat ratings in 70 images in 47 people as a function of threat rank

rm(list = ls())
setwd("~/Dropbox/Rachel/SAS_Threat/")

data <- read.table(paste("allthreat_agegender.txt",sep=""), header=FALSE)
colnames(data) <-c("rank","rating","id", "rankquad", "age", "gender")

str(data)

center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
}
data2 <- center_colmeans(data)
str(data2)

data3 <- as.data.frame(cbind(data$rating, data2$rank, I(data2$rank^2), data$id, data$rank))
data4 <- as.data.frame(cbind(data$rating, data2$rank, I(data2$rank^2), data$id, data2$age, data$gender))
data3[data3=="101"] <- 1
data3[data3=="102"] <- 2
data3[data3=="103"] <- 3
data3[data3=="105"] <- 4
data3[data3=="106"] <- 5
data3[data3=="107"] <- 6
data3[data3=="108"] <- 7
data3[data3=="109"] <- 8
data3[data3=="110"] <- 9
data3[data3=="111"] <- 10
data3[data3=="112"] <- 11
data3[data3=="113"] <- 12
data3[data3=="114"] <- 13
data3[data3=="115"] <- 14
data3[data3=="116"] <- 15
data3[data3=="117"] <- 16
data3[data3=="118"] <- 17
data3[data3=="119"] <- 18
data3[data3=="120"] <- 19
data3[data3=="121"] <- 20
data3[data3=="122"] <- 21
data3[data3=="123"] <- 22
data3[data3=="125"] <- 23
data3[data3=="126"] <- 24
data3[data3=="127"] <- 25
data3[data3=="128"] <- 26
data3[data3=="129"] <- 27
data3[data3=="130"] <- 28
data3[data3=="131"] <- 29
data3[data3=="132"] <- 30
data3[data3=="134"] <- 31
data3[data3=="135"] <- 32
data3[data3=="137"] <- 33
data3[data3=="138"] <- 34
data3[data3=="140"] <- 35
data3[data3=="141"] <- 36
data3[data3=="142"] <- 37
data3[data3=="143"] <- 38
data3[data3=="145"] <- 39
data3[data3=="146"] <- 40
data3[data3=="147"] <- 41
data3[data3=="148"] <- 42
data3[data3=="149"] <- 43
data3[data3=="150"] <- 44
data3[data3=="151"] <- 45
data3[data3=="152"] <- 46
data3[data3=="156"] <- 47
str(data3)
colnames(data3) <-c("Decisions", "Valence","ValenceSq","Participant", "rank17")
colnames(data4) <-c("Decisions", "Valence","ValenceSq","Participant", "Age", "Sex")


#data3["Participant"] <- NA
#data3$Participant <- rep(1:47, times=1, len=3148)

library(lme4)
linear <- glmer(Decisions ~ Valence + (1 + Valence |Participant), family=binomial("logit"), data=data3, control=glmerControl(optimizer="bobyqa"))
summary(linear)


quad4 <- glmer(Decisions ~ Valence + ValenceSq + (1 + Valence + ValenceSq |Participant), family=binomial("logit"), data=data3, control=glmerControl(optimizer="bobyqa"))
summary(quad4)

quad5 <-glmer(Decisions ~ Valence + ValenceSq  + Age + Sex + (1+ Valence + ValenceSq |Participant), family =binomial("logit"), data=data4, control=glmerControl(optimizer="bobyqa"))
summary(quad5)

anova(linear,quad4)

#https://www.statmethods.net/stats/frequencies.html
mytable <- table(data3$rating, data3$rank)
mytable
margin.table(mytable, 1)
#p = overall probability of rating threatening
#p = 107/447 for rank = 1 = .23937 (threat)
#p = 340/447 = .7606 (no threat)
#odds of threat = p/1-p.... .23937/(1-.23937) = .3146997
# odds of not threat = p/1-p .... .7606/(1-.7606) = 3.177109
#log odds of threat = log(.3146997) = -1.156136
#log odds of not threat = log(3.177109) = 1.155972
  #take logit to fix asymmetry in odds

#odds ratio for threat for face 7 versus face 1 = odds face 7/ odds face 1
  #2.338235/.3146997
  # odds of rating a face threatening are 7.43 times greater for faces ranked a 7 versus those ranked a 1

#p = 102/461 for rank = 2 = .22125
#log odds or logit is log(.22125) = -1.508462
#to transform log odds back to probability, p =exp(-1.508462)/(1+exp(-1.508462))
#p = 226/447 for rank = 6 = .505592
  #log odds log(.505592)=-0.6820253

#p threat = 318/454 for rank = 7 = .7004405
  #odds = .7004405/(1-.7004405) = 2.338235
  #log odds = -.3560459

library(ggplot2)
ggplot(data3, aes(x=rank, y=rating)) + geom_point() +
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)
plot(data3$rank, data3$rating)
curve(predict(linear, data.frame(rank=x), type="response"), add=TRUE)

library(ggplot2)
ggplot(data3, aes(x=Valence, y=Decisions)) + geom_point() +
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)
#plot(data3$Valence, data3$Decisions)
#curve(predict(quad4, data.frame(rank=x), type="response"), add=TRUE)

#intercept is interpreted as log odds of rating face a threatening with a rank of 0

library(lme4)
library(languageR)
linear <- glmer(rating ~ rank + (1 + rank |id), family=binomial("logit"), data=data3, control=glmerControl(optimizer="bobyqa"))
summary(linear)


quad4 <- glmer(rating ~ rank + rankquad + (1 + rank + rankquad |id), family=binomial("logit"), data=data3, control=glmerControl(optimizer="bobyqa"))
summary(quad4)
#verticle intercept line (vline) indicates no effect (x-axis position is 1 for most glms)
#https://strengejacke.wordpress.com/2017/10/23/one-function-to-rule-them-all-visualization-of-regression-models-in-rstats-w-sjplot/
plot_model(quad4, vline.color="red", transform=NULL, show.values=TRUE, value.offset = .3)
#if you delete transform = NULL you get odds ratios
plot_model(quad4, vline.color="red", show.values=TRUE, value.offset = .3)

quad5 <-glmer(rating ~rank + rankquad + age + gender + (1+rank + rankquad |id), family =binomial("logit"), data=data4, control=glmerControl(optimizer="bobyqa"))
summary(quad5)

quad6 <-glmer(rating ~rank + rankquad + age + gender + age*rank + gender*rank + (1+rank + rankquad |id), family =binomial("logit"), data=data4, control=glmerControl(optimizer="bobyqa"))
summary(quad6)

anova(linear,quad4)

exponential <-glmer(rating ~exp(rank17) + (1 + exp(rank17) | id), family=binomial("logit"),data=data3, control=glmerControl(optimizer="bobyqa"))
summary(exponential)
anova(quad4, exponential)

ident <- as.numeric(data3$id)
data3$Participant <- as.numeric(data3$Participant)

library(lattice)
ltheme <- standard.theme(color = FALSE) 
str(trellis.par.get(), max.level = 1)
coolNewPars <- list(superpose.symbol = list(pch = 21, cex = 2, col = "gray20"))

xyplot(data3$rating~data3$rank |id , groups=id, data=data3, type=c('p','r'), auto.key=F)
xyplot(fitted(quad4)~data3$rank |id , groups=id, data=data3, type=c('p','r'), auto.key=F)
xyplot(data4$rating~data4$rank |age , groups=id, data=data4, type=c('p','r'), auto.key=F)

xyplot(data3$Decisions~data3$Valence |Participant , groups=Participant, data=data3, type=c('p','r'), auto.key=F, color="gs", xlab=list(label="Valence", cex=2), ylab=list(label="Threat Decision Probability", cex=2), scales=list(tck=c(1,1), x=list(cex=1.2), y=list(cex=1.2)), col="black", transparent=TRUE)
xyplot(fitted(quad4)~data3$Valence |Participant , groups=Participant, data=data3, type=c('p','r'), color="gs", xlab=list(label="Valence", cex=2.5), ylab=list(label="Threat Decision Probability", cex=2.5), scales=list(tck=c(1,1), x=list(cex=1.2), y=list(cex=1.2)), par.settings = coolNewPars)
xyplot(data4$Decisions~data4$Valence |Age , groups=Participant, data=data4, type=c('p','r'), auto.key=F, xlab="Valence", ylab="Threat Decision Probability")

xyplot(fitted(quad4)~Valence + ValenceSq | Participant, groups=Participant,data=data3, type=c('p','r'),xlab="Valence", ylab="Threat Decision Probability")


My2Boxes(m=4,f1=data3$id,f2=data3$rating,x=data3$rank,color=c("red","yellow","green"))
install.packages("plotrix")
library(plotrix)
sizetree(data3[,c(1,2)])

library(popbio)
quartz("probabilities")
logi.hist.plot(data3$rank,data3$rating,logi.mod=1, boxp=TRUE,type="hist", col="white", xlabel="Mean Centered Face Valence", ylabel="Probability of Rating Threatening", ylabel2="Frequency")

with(data3,
     logi.hist.plot(rank, rating, type="hist", counts=TRUE,
                    ylabel="Probability of Rating Threatening", xlab="Mean Centered Face Valence",
                    col.cur="black", col.hist="grey", col.box="grey")
)


install.packages("coefplot2",
                 repos="http://www.math.mcmaster.ca/bolker/R",
                 type="source")
library(coefplot2)
coefplot2(quad4, intercept=TRUE)
par(new=TRUE)
coefplot2(linear)
coefplot2(quad4)

plot(data3$rank, data3$rating, xlab="rank", ylab="probability of threat rating")
g=glm(data3$rating ~ data3$rank, family=binomial)
curve(predict(g, data.frame(rank=x), type="resp"), add=TRUE)
points(data3$rank, fitted(quad4))

plot(data3$Decisions, data3$Valence, xlab="Valence", ylab="Probability of Threat Decision")
g=glm(data3$Decisions ~ data3$Valence, family=binomial)
curve(predict(g, data.frame(Valence=x), type="resp"), add=TRUE)
points(data3$Valence, fitted(quad4))

coef(quad4)
randoms <-ranef(quad4, condVar=TRUE)
qq <- attr(ranef(quad4, condVar=TRUE) [[1]], "condVar")
rand.interc<-randoms$Batch
df <- data.frame(Intercepts=randoms$Batch[,1], sd.interc=2*sqrt(qq[,,1:length(qq)]), lev.names=rownames(rand.interc))
qqmath(ranef(quad4, condVar=TRUE), strip=FALSE)$Batch

#install.packages("ggplot2")
#https://www.r-bloggers.com/visualizing-generalized-linear-mixed-effects-models-part-2-rstats-lme4/
library(ggplot2)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(RColorBrewer)
display.brewer.all()
display.brewer.pal(9, "Set3")
geom.colors1=list(color=colorRampPalette(brewer.pal(11,"Spectral"))(100))
point.color1=list(color=colorRampPalette(brewer.pal(11,"Spectral"))(100))

set_theme( geom.outline.color = "black",
             geom.outline.size = 1, 
               geom.label.size = 3,
             geom.label.color = "black",
             title.color = "black", 
             title.size = 2, 
             axis.title.size = 2,
             axis.title.color =  "black",
             axis.angle.x = 0,
             axis.textsize.x = 1.1,
             axis.textsize.y = 1.1,
            axis.textsize = 1.1,
             axis.textcolor = "black",
           legend.title.size=2.5,
           legend.size=2.5,
           legend.item.size = 2.5,
           legend.item.backcol = "white",
           plot.backcol = "white",
           axis.ticksmar=2.5)

set_theme( geom.outline.color = "black",
           geom.outline.size = 1, 
           geom.label.size = 1,
           geom.label.color = "black",
           axis.angle.x = 0,
           axis.textsize.x = 1,
           axis.textsize.y = 1,
           axis.textsize = 1,
           axis.textcolor = "black")


sjp.glmer(quad4, type="fe.cor")
sjp.glmer(quad4,type="re.qq")
plot_model(quad4, type="re.qq")
sjp.glmer(quad4, type="fe.pc")
quartz("test")
sjp.glmer(quad4, type="eff", show.ci=TRUE)
sjp.glmer(quad4, type="pred", vars=c("ValenceSq", "Participant"), geom.colors = "black")
sjp.glmer(quad4, type="pred", vars=c("ValenceSq"), geom.colors = "black")
sjp.glmer(quad4, type="pred", vars=c("Valence"), geom.colors = "black")

#plot probability curves for each covariate grouped by random intercepts
sjp.glmer(quad4, type="ri.pc", show.se=TRUE, title="Predicted Probabilities of Valence on Decisions")
sjp.glmer(data3$Decisions, type="ri.pc", show.se=TRUE)
#in one graph w probability curves
sjp.glmer(quad4, type="ri.pc", facet.grid=TRUE, show.intercept = TRUE, show.ci = TRUE, show.se=TRUE)
#PLOT RANDOM EFFECTS
sjp.glmer(quad4)
#sort random effects by rank or rank quad
sjp.glmer(quad4, colors="gs", axis.title = "", point.color="black")
plot_model(quad4, type="diag", colors="gs")
plot_model(quad4, type="re", colors=c("black", "black"))
#####################################

range(data3$rank)
xrank <- seq(-3,4,0.01)
y <-predict(quad4, list(rank=xrank), type="response")

plot(data3$rank, data3$rating, pch=16, xlab="Rank", ylab = "Rating")
lines(quad4)

attach(data3)
regression.out <-lm(rating ~ rank  + rank^2)
x<-seq(-3,3,by=1)
plot(rank, rating) 
points(x, (regression.out$coefficients[1] + regression.out$coefficients[2]*x))
regression.out$coefficients[1]
regression.out$coefficients[2]*x

hist(linear$residuals)

quad4 <- glmer(rating ~ rank + rankquad + (1 + rank + rankquad |id), family=binomial("logit"), data=data3, control=glmerControl(optimizer="bobyqa"))
quartz("quadratic") 
summary(quad4)

coefficients<-as.data.frame(coef(quad4))
intercept <- as.data.frame(coefficients[1])
points(x, (coefficients[1] + (coefficients[2]*x) + (coefficients[3]*(x^2))))
ranks <- data3$rank
ranksquad <- data3$rankquad
attach(quad4)



logi.hist.plot(data3$rank,data3$rating,boxp=FALSE,type="hist",col="gray") 
  points(rank, predlogit, groups=id, type =c("p", "1", "g"))

plot(x,y1,type="l",col="red")
lines(x,y2,col="green")

points(x, (coefficients$Intercepts + (coefficients$rank*x) + (coefficients$rankquad*(x^2))))
coefficients

predprob <-fitted(quad4)
predlogit <-logit(predprob)
datapred <- unique(data.frame(cbind(predlogit = predlogit, id=data3$id, rank=data3$rank)))
#quartz("quadratic")
par(new=TRUE)
xyplot(predlogit ~ rank, data = datapred, groups=id, type =c("p", "1", "g"), xlim=c(-4,4), ylim=c(-8,8))

xyplot(predlogit ~ rank, data = datapred, groups=id, type =c("p", "r", "r"), xlim=c(-4,4), ylim=c(-8,8))

xyplot(predlogit ~rank|id, data=datapred, type=c("p","r"))
