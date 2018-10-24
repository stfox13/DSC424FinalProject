#DSC project


#import libraries
library(foreign)
library(corrplot)
library(car)
library(QuantPsyc)
library(leaps)
library(RColorBrewer)
library(Hmisc) 
library(psych)
library(ggplot2)
library(MASS)
library(reshape2)
library(RCurl)


family_data <- getURL("https://raw.githubusercontent.com/stfox13/DSC424FinalProject/master/family_modified_001.csv")

family_df <- read.csv(text = family_data)

View(family_df)




# Set the working directory
setwd("C:/Depaul_Win7/DSC 424 Advanced Data Analysis/Project/family/hm3")


#Read data
fam <- read.csv("family_modified_001.csv", sep=",", header=T)
dim(fam)
str(fam)
head(fam)


#put FM_SIZE in first column
fam <- fam[,c(7,1,2,3,4,5,6,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127)]
str(fam)


#describe
describe(fam$FM_SIZE)

#recode missing values -1 to NA
fam[fam==-1] <-NA

#recode missing values 99 to NA
fam[fam==99] <-NA

#recode missing values 98 to NA
fam[fam==98] <-NA

#recode missing values 97 to NA
fam[fam==97] <-NA

#recode missing values 96 to NA
fam[fam==96] <-NA

#check for missing values
sum(is.na(fam))


#drop variables which have too many NAs
sum(is.na(fam$WRKCELN))
fam$WRKCELN = NULL
sum(is.na(fam$PHONEUSE))
fam$PHONEUSE = NULL
fam$FSSKIP = NULL
fam$FSSKDAYS = NULL
fam$FSLESS = NULL
fam$FSHUNGRY = NULL
fam$FSWEIGHT = NULL
fam$FSNOTEAT = NULL
fam$FSNEDAYS = NULL
fam$FHDSTCT = NULL
fam$FWRKLWCT = NULL
fam$FCHLMYN = NULL
fam$FSPEDYN = NULL
fam$FCHLMCT = NULL
fam$FSPEDCT = NULL
fam$FGAH = NULL
fam$FSNAPMYR = NULL
fam$FWICYN = NULL
fam$FWICCT = NULL
fam$COVCONF = NULL
fam$FMEDBNOP = NULL
fam$FPRCOOH = NULL
fam$FHIEBCCT = NULL



fam[!complete.cases(fam),]

famcleaned <- na.omit(fam) 
head(famcleaned)
dim(fam)
dim(famcleaned)

# check na after cleaning
sum(is.na(famcleaned))



#check for correlation
cor.famcleaned = cor(famcleaned)
cor.famcleaned
#corrplot(cor.famcleaned, method = "number")
corrplot(cor.famcleaned, method = "ellipse")



# create a random sample of rows of 5000

famcleaned[sample(nrow(famcleaned),3000),]

famrandom = famcleaned[sample(nrow(famcleaned),3000),]
dim(famrandom)


#I was not able to finish this model using all abservations, either my computer was freezing or Rstudio could not finish
# I had to stop the process, I rerun it with famrandom, but Rstudio  kept freezing.

#famsubsets = regsubsets(FM_SIZE ~.,data=famrandom,really.big=T,nbest=8)
#summary(famsubsets)



# I was not able to finish forward selection using all observations
#my computer run for hours before I had to stop the process

#I rerun it with famrandom and it worked!

#automatic selections
null = lm(FM_SIZE ~ 1, data=famrandom)
null

full = lm(FM_SIZE ~ ., data=famrandom)
full


famForward = step(null, scope = list(lower=null, upper=full), direction="forward")
summary(famForward)


famfinal = lm(FM_SIZE ~ FHICOVCT + FM_KIDS + FM_TYPE + 
                FDGLWCT1 + FHSTATEX + FHSTATG + FHSTATVG + FHSTATFR + FHSTATPR +  
                FREMEMYN + FHCDVCT + FHCDVYN + RAT_CAT5 + RAT_CAT4 + 
                FWALKCT + FWALKYN, data = famrandom)

summary(famfinal)
vif(famfinal)
qqPlot(famfinal)

famfinal2 = lm(FM_SIZE ~ FHICOVCT + FM_KIDS + FM_TYPE + 
                FDGLWCT1 + FHSTATEX + FHSTATG + FHSTATVG + FHSTATFR + FHSTATPR +  
                FREMEMYN + FHCDVCT + FHCDVYN, data = famrandom)

vif(famfinal2)
summary(famfinal2)

famfinal3 = lm(FM_SIZE ~ FM_KIDS + 
                 FDGLWCT1 + FHSTATEX + FHSTATG + FHSTATVG + FHSTATFR + FHSTATPR +  
                 FREMEMYN + FHCDVCT, data = famrandom)

vif(famfinal3)
summary(famfinal3)





