
library(ggplot2)

manuscriptact <- read.csv("D:/backupJuly2017/Thesis/thesis analyses/Final Thesis Analyses/PredAnalyses/manuscript2.csv" , header=TRUE)

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

manuscriptact$VegType <- factor(manuscriptact$VegType, levels = c("ES", "NG", "CC", "MW"))
manuscriptact <- summarySE(manuscriptact, measurevar="Activity", groupvars=c("VegType","CommonName"))


manuact2015<-subset(manuscriptact, Year=="2015")
manuact2016<-subset(manuscriptact, Year=="2016")

manuact2015$VegType <- factor(manuact2015$VegType, levels = c("ES", "NG", "CC", "MW"))
manuact2015 <- summarySE(manuact2015, measurevar="Activity", groupvars=c("VegType","CommonName"))

manuact2016$VegType <- factor(manuact2016$VegType, levels = c("ES", "NG", "CC", "MW"))
manuact2016 <- summarySE(manuact2016, measurevar="Activity", groupvars=c("VegType","CommonName"))


ggplot(manuact2015, aes(x=CommonName, y=Activity, fill=VegType)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=Activity-se, ymax=Activity+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Species") + ylab("Activity") +
  scale_fill_grey(name="Vegetation Type", # Legend label, use darker colors
  breaks=c("ES", "NG", "CC", "MW"),labels=c("Early Seral", "Native Grassland", "Catclaw Shrubland", "Mesquite Woodland")) +
  ylim(0,8)+theme_bw() +ggtitle("2015") +theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text.x = element_text(angle = 90, hjust = 1, size=12, colour="black",family="Times New Roman")) +
  theme(text=element_text(size=12, family="Times New Roman", colour="black"))


ggplot(manuact2016, aes(x=CommonName, y=Activity, fill=VegType)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=Activity-se, ymax=Activity+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Species") + ylab("Activity") +
  scale_fill_grey(name="Vegetation Type", # Legend label, use darker colors
                  breaks=c("ES", "NG", "CC", "MW"),labels=c("Early Seral", "Native Grassland", "Catclaw Shrubland", "Mesquite Woodland")) +
  ylim(0,8)+theme_bw() +ggtitle("2016") +theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=12, colour="black")) +
  theme(text=element_text(size=12, family="times", colour="black"))

sm<-read.csv("D:/backupJuly2017/Thesis/thesis analyses/Final Thesis Analyses/PredAnalyses/smammal_final.csv", header=T)
sm<- sm[,c(1:7)]
library(reshape2)
sm<-melt(sm, id.vars=c("Year", "Grid", "VegType"))
write.csv(sm,"D:/backupJuly2017/Thesis/thesis analyses/Final Thesis Analyses/PredAnalyses/smreshape.csv")

install.packages("extrafont")
install.packages("Rttf2pt1")
library(extrafont)
library(Rttf2pt1)
font_import()
loadfonts(quiet = T)

fonts()

install.packages("showtext")
library(showtext)

library(showtext) 
# If you have this font installed 
font_add("times", "Times New Roman.ttf") 
