
library(AICcmodavg)
library(ggplot2)

survival<-read.csv("D:/backupJuly2017/Thesis/thesis analyses/Final Thesis Analyses/NestSurvival_All.csv", header=T)

dat <- subset(survival, Species %in% c("BEWR", "BTSP", "CARD", "CASP", "CBTH", "COGD", "EAME", "GRRO", "GREJ", 
                                       "LASP","LBTH", "MODO", "NOCA", "NOMO", "PABU", "PYRR",  "UNKN", "STFL", "YBCU"))

dat$FateDate <- as.Date(dat$FateDate, format=c("%m/%d/%Y"))
dat$Julian <- as.numeric(format(dat$FateDate, "%j"))

small <- subset(dat, NestType %in% "Small Cup")
med <- subset(dat, NestType %in% "Medium Cup")
large <- subset(dat, NestType %in% "Roadrunner")
exp <- subset(dat, NestType %in% "Exposed")

sm2015 <- subset(small, Year %in% "2015")
sm2016 <- subset(small, Year %in% "2016")

med2015 <- subset(med, Year %in% "2015")
med2016 <- subset(med, Year %in% "2016")

lg2015 <- subset(large, Year %in% "2015")
lg2016 <- subset(large, Year %in% "2016")

exp2015 <- subset(exp, Year %in% "2015")
exp2016 <- subset(exp, Year %in% "2016")

# All


shrub.all <-glm(Fate~TotalShrub, family=binomial(link=logit), data=dat)
edge.all <-glm(Fate~ClosestEdge, family=binomial(link=logit), data=dat)
global.all <-glm(Fate~TotalShrub + Concealment + ClosestEdge + Year + CameraOnNest + Julian + NestType, family=binomial(link=logit), data=dat) 
group.all <-glm(Fate~NestType, family=binomial(link=logit), data=dat)
camera.all <-glm(Fate~CameraOnNest, family=binomial(link=logit), data=dat)
julian.all <-glm(Fate~Julian, family=binomial(link=logit), data=dat)
year.all <-glm(Fate~Year, family=binomial(link=logit), data=dat)
null.all <-glm(Fate~1, family=binomial(link=logit), data=dat)
conceal.all <-glm(Fate~Concealment, family=binomial(link=logit), data=dat)
shrubxedge.all <-glm(Fate~TotalShrub * ClosestEdge, family=binomial(link=logit), data=dat)
shrubxgroup.all <-glm(Fate~TotalShrub*NestType, family=binomial(link=logit), data=dat)
edgexgroup.all <-glm(Fate~ClosestEdge*NestType, family=binomial(link=logit), data=dat)
concealxgroup.all <-glm(Fate~Concealment*NestType, family=binomial(link=logit), data=dat)
shrubxconceal.all <-glm(Fate~TotalShrub*Concealment, family=binomial(link=logit), data=dat)
shrubxedgexgroup.all <-glm(Fate~TotalShrub*ClosestEdge*NestType, family=binomial(link=logit), data=dat)
shrubxconcealxgroup.all <-glm(Fate~TotalShrub*Concealment*NestType, family=binomial(link=logit), data=dat)
concealxedgexgroup.all <-glm(Fate~Concealment*ClosestEdge*NestType, family=binomial(link=logit), data=dat)
julianxshrub.all <-glm(Fate~Julian*TotalShrub, family=binomial(link=logit), data=dat)

LL.all <- c(logLik(global.all), logLik(null.all), logLik(year.all), logLik(camera.all),logLik(shrub.all),
            logLik(edge.all), logLik(conceal.all), logLik(group.all),
            logLik(shrubxedge.all), logLik(shrubxgroup.all),logLik(edgexgroup.all),logLik(concealxgroup.all),
            logLik(shrubxconceal.all),logLik(julian.all))
K.all <- c(length(coef(global.all)),length(coef(null.all)),length(coef(year.all)),length(coef(camera.all)),length(coef(shrub.all)),
           length(coef(edge.all)),length(coef(conceal.all)),length(coef(group.all)),
           length(coef(shrubxedge.all)), length(coef(shrubxgroup.all)),length(coef(edgexgroup.all)),length(coef(concealxgroup.all)),
           length(coef(shrubxconceal.all)),length(coef(julian.all)))
Modnames.all <- c("Global", "Null", "Year", "Camera", "Shrub", "Edge", "Concealment","Nest Group" ,
                  "Shrub x Edge", "Shrub x Nest Group", "Edge x Nest Group", "Concealment x Nest Group", 
                  "Shrub x Concealment", "Julian")
modeltable.all <- aictabCustom(LL.all, K.all, modnames= Modnames.all, nobs = 14)
write.table(modeltable.all, file = "C:/Users/Helen Davis/Desktop/Manuscripts/all aic.csv", sep = ",")
                  
# Small Cup

global.sm <-glm(Fate~TotalShrub + Concealment + ClosestEdge + Year + CameraOnNest + Julian, family=binomial(link=logit), data=small)
null.sm <-glm(Fate~1, family=binomial(link=logit), data=small)
year.sm <-glm(Fate~Year, family=binomial(link=logit), data=small)
camera.sm <-glm(Fate~CameraOnNest, family=binomial(link=logit), data=small)
shrub.sm <-glm(Fate~TotalShrub, family=binomial(link=logit), data=small)
edge.sm <-glm(Fate~ClosestEdge, family=binomial(link=logit), data=small)
conceal.sm <-glm(Fate~Concealment, family=binomial(link=logit), data=small)
julian.sm <-glm(Fate~Julian, family=binomial(link=logit), data=small)
shrub.conceal.sm <-glm(Fate~TotalShrub + Concealment, family=binomial(link=logit), data=small)
shrub.edge.sm <-glm(Fate~TotalShrub + ClosestEdge, family=binomial(link=logit), data=small)
shrubxconcealment.sm <-glm(Fate~TotalShrub * Concealment, family=binomial(link=logit), data=small)
shrubxjulian.sm <-glm(Fate~TotalShrub * Julian, family=binomial(link=logit), data=small)

LL.sm <- c(logLik(global.sm), logLik(null.sm), logLik(year.sm), logLik(camera.sm),logLik(shrub.sm),
           logLik(edge.sm), logLik(conceal.sm), logLik(julian.sm), logLik(shrub.conceal.sm),logLik(shrub.edge.sm), 
           logLik(shrubxconcealment.sm))
K.sm <- c(length(coef(global.sm)),length(coef(null.sm)),
            length(coef(year.sm)),
            length(coef(camera.sm)),length(coef(shrub.sm)),
            length(coef(edge.sm)),length(coef(conceal.sm)),length(coef(julian.sm)),
            length(coef(shrub.conceal.sm)),length(coef(shrub.edge.sm)), length(coef(shrubxconcealment.sm)))
Modnames.sm <- c("Global", "Null", "Year", "Camera", "Shrub", "Edge", "Concealment", "Julian Date",  
                   "Shrub + Concealment", "Shrub + Edge", "Shrub x Concealment")
modeltable.sm <- aictabCustom(LL.sm, K.sm, modnames= Modnames.sm, nobs = 11)
write.table(modeltable.sm, file = "C:/Users/Helen Davis/Desktop/Manuscripts/sm aic.csv", sep = ",")


#Medium Cup

global.med <-glm(Fate~TotalShrub + Concealment + ClosestEdge + Year + CameraOnNest + Julian, family=binomial(link=logit), data=med)
null.med <-glm(Fate~1, family=binomial(link=logit), data=med)
year.med <-glm(Fate~Year, family=binomial(link=logit), data=med)
camera.med <-glm(Fate~CameraOnNest, family=binomial(link=logit), data=med)
shrub.med <-glm(Fate~TotalShrub, family=binomial(link=logit), data=med)
edge.med <-glm(Fate~ClosestEdge, family=binomial(link=logit), data=med)
conceal.med <-glm(Fate~Concealment, family=binomial(link=logit), data=med)
julian.med <-glm(Fate~Julian, family=binomial(link=logit), data=med)
shrub.conceal.med <-glm(Fate~TotalShrub + Concealment, family=binomial(link=logit), data=med)
shrub.edge.med <-glm(Fate~TotalShrub + ClosestEdge, family=binomial(link=logit), data=med)
shrubxconcealment.med <-glm(Fate~TotalShrub * Concealment, family=binomial(link=logit), data=med)
shrubxedge.med <-glm(Fate~TotalShrub * Concealment, family=binomial(link=logit), data=med)
julianxshrub.med <-glm(Fate~Julian*TotalShrub, family=binomial(link=logit), data=med)

LL.med <- c(logLik(global.med), logLik(null.med), logLik(year.med), logLik(camera.med),logLik(shrub.med),
            logLik(edge.med), logLik(conceal.med), logLik(julian.med), logLik(shrub.conceal.med),logLik(shrub.edge.med), 
            logLik(shrubxconcealment.med), logLik(shrubxedge.med))
K.med <- c(length(coef(global.med)),length(coef(null.med)),
           length(coef(year.med)),
           length(coef(camera.med)),length(coef(shrub.med)),
           length(coef(edge.med)),length(coef(conceal.med)),length(coef(julian.med)),
           length(coef(shrub.conceal.med)),length(coef(shrub.edge.med)), length(coef(shrubxconcealment.med)),length(coef(shrubxedge.med)))
Modnames.med <- c("Global", "Null", "Year", "Camera", "Shrub", "Edge", "Concealment", "Julian Date",  
                  "Shrub + Concealment", "Shrub + Edge", "Shrub x Concealment", "Shrub x Edge")
modeltable.med <- aictabCustom(LL.med, K.med, modnames= Modnames.med, nobs = 13)
write.table(modeltable.med, file = "C:/Users/Helen Davis/Desktop/Manuscripts/med aic.csv", sep = ",")

#Exposed Nests

global.exp <-glm(Fate~TotalShrub + Concealment + ClosestEdge + Year + CameraOnNest + Julian, family=binomial(link=logit), data=exp)
null.exp <-glm(Fate~1, family=binomial(link=logit), data=exp)
year.exp <-glm(Fate~Year, family=binomial(link=logit), data=exp)
camera.exp <-glm(Fate~CameraOnNest, family=binomial(link=logit), data=exp)
shrub.exp <-glm(Fate~TotalShrub, family=binomial(link=logit), data=exp)
edge.exp <-glm(Fate~ClosestEdge, family=binomial(link=logit), data=exp)
conceal.exp <-glm(Fate~Concealment, family=binomial(link=logit), data=exp)
julian.exp <-glm(Fate~Julian, family=binomial(link=logit), data=exp)
conceal.edge.exp <-glm(Fate~Concealment + ClosestEdge, family=binomial(link=logit), data=exp)
concealxedge.exp <-glm(Fate~Concealment * ClosestEdge, family=binomial(link=logit), data=exp)
julianxshrub.exp <-glm(Fate~Julian*TotalShrub, family=binomial(link=logit), data=exp)

LL.exp <- c(logLik(global.exp), logLik(null.exp), logLik(year.exp), logLik(camera.exp),logLik(shrub.exp),
            logLik(edge.exp), logLik(conceal.exp), logLik(julian.exp), logLik(conceal.edge.exp), 
            logLik(concealxedge.exp))
K.exp <- c(length(coef(global.exp)),length(coef(null.exp)),
           length(coef(year.exp)),
           length(coef(camera.exp)),length(coef(shrub.exp)),
           length(coef(edge.exp)),length(coef(conceal.exp)),length(coef(julian.exp)),
           length(coef(conceal.edge.exp)), length(coef(concealxedge.exp)))
Modnames.exp <- c("Global", "Null", "Year", "Camera", "Shrub", "Edge", "Concealment", "Julian Date",
                  "Concealment + Edge", "Concealment x Edge")
modeltable.exp <- aictabCustom(LL.exp, K.exp, modnames= Modnames.exp, nobs = 10)
write.table(modeltable.exp, file = "C:/Users/Helen Davis/Desktop/Manuscripts/exp aic.csv", sep = ",")
                  

ggplot(med, aes(x=Julian, y=Fate)) + stat_smooth(method="glm", method.args=list(family="binomial"), se=T, color="black")+
  ylim(0,1)+ ylab("Predicted Nest Success") +xlab("Julian Date")+
  theme(axis.text=element_text(size=12),  axis.title=element_text(size=16),panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(med, aes(x=TotalShrub, y=Fate)) + stat_smooth(method="glm", method.args=list(family="binomial"), se=T, color="black") +
   ylim(0,1)+ ylab("Predicted Nest Success") +xlab("Shrub Cover %")+
  theme(axis.text=element_text(size=12),  axis.title=element_text(size=16),panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
 
ggplot(med, aes(x=ClosestEdge, y=Fate)) + stat_smooth(method="glm", method.args=list(family="binomial"), se=T) +
  ylim(0,1)+ ylab("Predicted Nest Success") +xlab("Shrub Cover %")+
  theme(axis.text=element_text(size=12),  axis.title=element_text(size=16),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

