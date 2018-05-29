

library(AICcmodavg)
library(ggplot2)

survival<-read.csv("D:/backupJuly2017/Thesis/thesis analyses/Final Thesis Analyses/NestSurvival_All.csv", header=T)

preddat <- subset(dat, CameraOnNest %in% "Y")

preddat$FateDate <- as.Date(preddat$FateDate, format=c("%m/%d/%Y"))
preddat$Julian <- as.numeric(format(preddat$FateDate, "%j"))

predall <- subset(preddat, PredType %in% c("1", "2", "3", "4", "5", "6", "7"))

predall.cor <- subset(predall, select=c(TotalShrub, Concealment, ClosestEdge, Julian, Year))
vif(predall.cor)

snake <- subset(preddat, PredType %in% c("1", "3"))

predall$Group = predall$PredType
predall$PredType[predall$PredType=="3"] <- "2"
predall$PredType[predall$PredType=="4"] <- "2"
predall$PredType[predall$PredType=="5"] <- "2"
predall$PredType[predall$PredType=="6"] <- "2"
predall$PredType[predall$PredType=="7"] <- "2"


shrub.pred <-glm(PredType~TotalShrub, family=binomial(link=logit), dat=predall)
edge.pred <-glm(PredType~ClosestEdge, family=binomial(link=logit), dat=predall)
global.pred <-glm(PredType~TotalShrub + Concealment + ClosestEdge + Year + Julian, family=binomial(link=logit), dat=predall) 
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
             logLik(shrubxedge.pred), 
             logLik(shrubxconceal.pred),logLik(julian.pred))
K.pred <- c(length(coef(global.pred)),length(coef(null.pred)),length(coef(year.pred)),length(coef(shrub.pred)),
            length(coef(edge.pred)),length(coef(conceal.pred)),length(coef(group.pred)),
            length(coef(shrubxedge.pred)), 
            length(coef(shrubxconceal.pred)),length(coef(julian.pred)))
Modnames.pred <- c("Global", "Null", "Year", "Shrub", "Edge", "Concealment","Pred Group" ,
                   "Shrub x Edge", 
                   "Shrub x Concealment", "Julian")
modeltable.pred <- aictabCustom(LL.pred, K.pred, modnames= Modnames.pred, nobs = 11)
write.table(modeltable.pred, file = "C:/Users/Helen Davis/Desktop/Manuscripts/pred aic.csv", sep = ",")


shrub.snake <-glm(PredType~TotalShrub, family=binomial(link=logit), dat=snake)
edge.snake <-glm(PredType~ClosestEdge, family=binomial(link=logit), dat=snake)
global.snake <-glm(PredType~TotalShrub + Concealment + ClosestEdge + Year + Julian, family=binomial(link=logit), dat=snake) 
group.snake <-glm(PredType~Group, family=binomial(link=logit), dat=snake)
julian.snake <-glm(PredType~Julian, family=binomial(link=logit), dat=snake)
year.snake <-glm(PredType~Year, family=binomial(link=logit), dat=snake)
null.snake <-glm(PredType~1, family=binomial(link=logit), dat=snake)
conceal.snake <-glm(PredType~Concealment, family=binomial(link=logit), dat=snake)
shrubxedge.snake <-glm(PredType~TotalShrub * ClosestEdge, family=binomial(link=logit), dat=snake)
shrubxgroup.snake <-glm(PredType~TotalShrub*Group, family=binomial(link=logit), dat=snake)
edgexgroup.snake <-glm(PredType~ClosestEdge*Group, family=binomial(link=logit), dat=snake)
concealxgroup.snake <-glm(PredType~Concealment*Group, family=binomial(link=logit), dat=snake)
shrubxconceal.snake <-glm(PredType~TotalShrub*Concealment, family=binomial(link=logit), dat=snake)
shrubxedgexgroup.snake <-glm(PredType~TotalShrub*ClosestEdge*Group, family=binomial(link=logit), dat=snake)
shrubxconcealxgroup.snake <-glm(PredType~TotalShrub*Concealment*Group, family=binomial(link=logit), dat=snake)
concealxedgexgroup.snake <-glm(PredType~Concealment*ClosestEdge*Group, family=binomial(link=logit), dat=snake)
concealxjulian.snake <-glm(PredType~Julian*Concealment, family=binomial(link=logit), dat=snake)

LL.snake <- c(logLik(global.snake), logLik(null.snake), logLik(year.snake),logLik(shrub.snake),
              logLik(edge.snake), logLik(conceal.snake),logLik(concealxjulian.snake),
              logLik(shrubxedge.snake), 
              logLik(shrubxconceal.snake),logLik(julian.snake))
K.snake <- c(length(coef(global.snake)),length(coef(null.snake)),length(coef(year.snake)),length(coef(shrub.snake)),
             length(coef(edge.snake)),length(coef(conceal.snake)),length(coef(concealxjulian.snake)),
             length(coef(shrubxedge.snake)), 
             length(coef(shrubxconceal.snake)),length(coef(julian.snake)))
Modnames.snake <- c("Global", "Null", "Year", "Shrub", "Edge", "Concealment", "Julian x Concealment",
                    "Shrub x Edge", 
                    "Shrub x Concealment", "Julian")
modeltable.snake <- aictabCustom(LL.snake, K.snake, modnames= Modnames.snake, nobs = 10)
write.table(modeltable.snake, file = "C:/Users/Helen Davis/Desktop/Manuscripts/snake aic.csv", sep = ",")

ggplot(snake, aes(x=TotalShrub, y=Fate)) + stat_smooth(method="glm", method.args=list(family="binomial"), se=T, color="black")+
  ylim(0,1)+ ylab("Predicted Probability of Predation") +xlab("Shrub Cover %")+
  theme(axis.text=element_text(size=12),  axis.title=element_text(size=16),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))


df1=data.frame(med)
df2=data.frame(snake)

df2$PredType <- as.numeric(df2$PredType)

df2$PredType[df2$PredType=="1"] <- "0"
df2$PredType[df2$PredType=="3"] <- "1"

colnames(df1)[6] = "NewFate" 
colnames(df2)[25] = "NewFate" 



df1$model <- "A"
df2$model <- "B"

df1 <- df1[,c(6,20,29)]
df2 <- df2[,c(20,25,29)]

dfc <- rbind(df1, df2)

dfc <- na.omit(dfc)


dfc$NewFate <- as.numeric(dfc$NewFate)

library(ggplot2)
ggplot(dfc, aes(x=TotalShrub, y=NewFate, group=model)) + stat_smooth(method="glm", method.args=list(family="binomial"), color="black")+
  ylim(0,1)+ ylab("Predicted Probability") +xlab("Shrub Cover %")+
  theme(axis.text=element_text(size=12),  axis.title=element_text(size=16),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

