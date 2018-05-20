

library(AICcmodavg)
library(ggplot2)

survival<-read.csv("D:/backupJuly2017/Thesis/thesis analyses/Final Thesis Analyses/NestSurvival_All.csv", header=T)

preddat <- subset(survival, CameraOnNest %in% "Y")

preddat$FateDate <- as.Date(preddat$FateDate, format=c("%m/%d/%Y"))
preddat$Julian <- as.numeric(format(preddat$FateDate, "%j"))

predall <- subset(preddat, PredType %in% c("1", "2", "3", "4", "5", "6", "7"))
snake <- subset(preddat, PredType %in% c("1", "3"))

predall$Group = predall$PredType
predall$PredType[predall$PredType=="3"] <- "2"
predall$PredType[predall$PredType=="4"] <- "2"
predall$PredType[predall$PredType=="5"] <- "2"
predall$PredType[predall$PredType=="6"] <- "2"
predall$PredType[predall$PredType=="7"] <- "2"


shrub.pred <-glm(PredType~TotalShrub, family=binomial(link=logit), dat=predall)
edge.pred <-glm(PredType~ClosestEdge, family=binomial(link=logit), dat=predall)
global.pred <-glm(PredType~TotalShrub + Concealment + ClosestEdge + Year + Julian + Group, family=binomial(link=logit), dat=predall) 
group.pred <-glm(PredType~Group, family=binomial(link=logit), dat=predall)
julian.pred <-glm(PredType~Julian, family=binomial(link=logit), dat=predall)
year.pred <-glm(PredType~Year, family=binomial(link=logit), dat=predall)
null.pred <-glm(PredType~1, family=binomial(link=logit), dat=predall)
conceal.pred <-glm(PredType~Concealment, family=binomial(link=logit), dat=predall)
shrubxedge.pred <-glm(PredType~TotalShrub * ClosestEdge, family=binomial(link=logit), dat=predall)
shrubxgroup.pred <-glm(PredType~TotalShrub*Group, family=binomial(link=logit), dat=predall)
edgexgroup.pred <-glm(PredType~ClosestEdge*Group, family=binomial(link=logit), dat=predall)
concealxgroup.pred <-glm(PredType~Concealment*Group, family=binomial(link=logit), dat=predall)
shrubxconceal.pred <-glm(PredType~TotalShrub*Concealment, family=binomial(link=logit), dat=predall)
shrubxedgexgroup.pred <-glm(PredType~TotalShrub*ClosestEdge*Group, family=binomial(link=logit), dat=predall)
shrubxconcealxgroup.pred <-glm(PredType~TotalShrub*Concealment*Group, family=binomial(link=logit), dat=predall)
concealxedgexgroup.pred <-glm(PredType~Concealment*ClosestEdge*Group, family=binomial(link=logit), dat=predall)
julianxshrub.pred <-glm(PredType~Julian*TotalShrub, family=binomial(link=logit), dat=predall)

LL.pred <- c(logLik(global.pred), logLik(null.pred), logLik(year.pred),logLik(shrub.pred),
             logLik(edge.pred), logLik(conceal.pred), logLik(group.pred),
             logLik(shrubxedge.pred), logLik(shrubxgroup.pred),logLik(edgexgroup.pred),logLik(concealxgroup.pred),
             logLik(shrubxconceal.pred),logLik(julian.pred))
K.pred <- c(length(coef(global.pred)),length(coef(null.pred)),length(coef(year.pred)),length(coef(shrub.pred)),
            length(coef(edge.pred)),length(coef(conceal.pred)),length(coef(group.pred)),
            length(coef(shrubxedge.pred)), length(coef(shrubxgroup.pred)),length(coef(edgexgroup.pred)),length(coef(concealxgroup.pred)),
            length(coef(shrubxconceal.pred)),length(coef(julian.pred)))
Modnames.pred <- c("Global", "Null", "Year", "Shrub", "Edge", "Concealment","Nest Group" ,
                   "Shrub x Edge", "Shrub x Nest Group", "Edge x Nest Group", "Concealment x Nest Group", 
                   "Shrub x Concealment", "Julian")
modeltable.pred <- aictabCustom(LL.pred, K.pred, modnames= Modnames.pred, nobs = 13)
write.table(modeltable.pred, file = "C:/Users/Helen Davis/Desktop/Manuscripts/pred aic.csv", sep = ",")


