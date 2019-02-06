
## predact.R
## R 3.5.1
## Intended use: Written for my thesis manuscript to generate figures of predator activity
## Inputs: Input data must be in .csv format. 
## Variables: common_name: species common name 
##            scientific_name: species scientific name
##            veg_type: vegetation type 
##            year: year sample was taken
##            activity: number of individuals detected per sampling period
## Outputs: Manuscript-quality figure of mean activity by species by vegetation type with standard error bars
## Instructions: Run script line-by-line from R studio

# Install required packages
install.packages("ggplot")

# Load required packages
require("ggplot")

#Create user inputs
workingdir <- function ()
{
  n <- readline(prompt = "Enter path to working directory: ")
  return(n)
}
predact <- function ()
{
  n <- readline(prompt = "Enter file name: ")
  return(n)
}
levels <- function ()
{
  n <- readline(prompt = "Enter order of variables as you want them listed on table (e.g. "skunk", "badger"): ")
  return(n)
}

# Activity summary function

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

## Set path for folder containing raster files
setwd(print(workingdir()))

## Verify successfully changed
print(getwd())

## Read in data file
dat <- read.csv(print(predact()) , header=TRUE)

## Enter list of species common names and vegetation types in the order you want them displayed in the figure
dat$common_name <- factor(dat$common_name, levels = c(print(levels())))
dat$vegtype <- factor(dat$vegtype, levels = c(print(levels())))

## Run summary analysis. This will create a dataframe of your values that you can also export if needed
datsum <- summarySE(dat, measurevar="activity", groupvars=c("vegtype","common_name"))

## Create figure in ggplot
## You will be prompted twice for lists
##    1. Enter where you want your column breaks to be (e.g. vegetation type)
##    2. Enter the full name of each vegetation type if you are using an abbreviation
ggplot(manuact2015, aes(x=CommonName, y=activity, fill=vegtype)) + 
  geom_bar(position=position_dodge(), stat="identity", colour="black", size=.3) +   
  geom_errorbar(aes(ymin=Activity-se, ymax=Activity+se), size=.3, width=.2, position=position_dodge(.9)) +
  xlab("Species") + ylab("Activity") + 
  scale_fill_grey(name="Vegetation Type", breaks=c(print(levels())), labels=c(print(levels()))) +
  ylim(0,8) + theme_bw() + ggtitle("Year") + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=12, colour="black",family="Times New Roman")) +
  theme(text=element_text(size=12, family="Times New Roman", colour="black"))


