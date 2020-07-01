###############################################################################
#THIS IS THE DEMOGRAPHICS TABLE FOR KWP
#IT WAS AN RMD
#DAVID PELL
#19/03/2018
###############################################################################
#LIBRARIES
###############################################################################

library(data.table)#FOR SPEED
library(reshape2)#WIDE TO LONG
library(officer)
library(questionr)

###############################################################################
#OPEN DATA
###############################################################################

KWP <- fread("/dph/sdil/KWPSEP19/KWPDemographics.csv")

###############################################################################
#RECODE HIGHEST QUALIFICATION
###############################################################################

KWP$Qualification <- ifelse(KWP$Qualification %in% 2, 1, KWP$Qualification)
KWP$Qualification <- ifelse(KWP$Qualification >4, 5, KWP$Qualification)

###############################################################################
#HOUSEHOLD WEIGHTING
###############################################################################

KWP$HouseholdWeight <- KWP$grossupweight * KWP$HouseholdWeeklyWeight

###############################################################################
#GET WEEKLY HOUSEHOLD SUM VOLUME
###############################################################################

HouseholdWeight <- KWP[, .(HouseholdWeight = mean(HouseholdWeight)),by = c("householdno","Weeks")]

###############################################################################
#GET MEAN WEEKLY WEIGHT
###############################################################################

meanWeight <- HouseholdWeight[, .(WeekWeight = mean(HouseholdWeight)), by = c("Weeks")]

###############################################################################
#MERGE WEEKLY WEIGHT
###############################################################################

HouseholdWeight <- merge(x=HouseholdWeight,y=meanWeight,by="Weeks",all.x=TRUE)

###############################################################################
#SCALE WEIGHTS
###############################################################################

HouseholdWeight$Weight <- HouseholdWeight$HouseholdWeight / HouseholdWeight$WeekWeight

###############################################################################
#TIDY UP
###############################################################################

rm(meanWeight)

###############################################################################
#SUBSET VARIABLES FROM KWP
###############################################################################

Demographics <- unique(KWP[,c("householdno","Weeks","Qualification","EducationGroups","Socialclass","Income","Children")])

###############################################################################
#MERGE DEMOGRAPHICS FROM HOUSEHOLD WEIGHTS
###############################################################################

HouseholdWeight <- merge(x=HouseholdWeight,y=Demographics,by=c("householdno","Weeks"),all.x=TRUE)

###############################################################################
#TIDY UP
###############################################################################

HouseholdWeight$HouseholdWeight <- NULL
HouseholdWeight$WeekWeight <- NULL

###############################################################################
#GET WEIGHTED VALUES FOR HOUSEHOLDS WITH CHILDREN PER WEEK
###############################################################################

Children <- data.frame(HouseholdWeight[,rep(1L,sum(Weight)),by=.(Children, Weeks)][,prop.table(table(Children, Weeks),2)])

# ggplot(Children,aes(x=Weeks,y=Freq, colour=Children)) +
#   geom_line() + 
#   geom_point() + 
#   geom_vline(xintercept=as.Date("2016-03-16")) 

Children <- aggregate(Children$Freq,list(Children$Children),FUN=mean)

###############################################################################
#GET WEIGHTED VALUES PER SOCIAL CLASS GROUP PER WEEK
###############################################################################

SocialClass <- data.frame(HouseholdWeight[,rep(1L,sum(Weight)),by=.(Socialclass, Weeks)][,prop.table(table(Socialclass, Weeks),2)])

# ggplot(SocialClass,aes(x=Weeks,y=Freq, colour=Socialclass)) +
#   geom_line() + 
#   geom_point() + 
#   geom_vline(xintercept=as.Date("2016-03-16")) 

SocialClass <- aggregate(SocialClass$Freq,list(SocialClass$Socialclass),FUN=mean)

###############################################################################
#GET WEIGHTED VALUES PER INCOME GROUP PER WEEK
###############################################################################

Income <- data.frame(HouseholdWeight[,rep(1L,sum(Weight)),by=.(Income, Weeks)][,prop.table(table(Income, Weeks),2)])

# IncomeUnweighted <- data.frame(HouseholdWeight[, ,by=.(Income, Weeks)][,prop.table(table(Income, Weeks),2)])

# ggplot(Income,aes(x=Weeks,y=Freq, colour=Income)) +
#   geom_line() + 
#   geom_point() + 
#   geom_vline(xintercept=as.Date("2016-03-16")) 

# ggplot(IncomeUnweighted,aes(x=Weeks,y=Freq, colour=Income)) +
#   geom_line() + 
#   geom_point() + 
#   geom_vline(xintercept=as.Date("2016-03-16")) 

Income <- aggregate(Income$Freq,list(Income$Income),FUN=mean)

###############################################################################
#GET WEIGHTED VALUES PER QUALIFICATION GROUP PER WEEK
###############################################################################

Qualification <- data.frame(HouseholdWeight[,rep(1L,sum(Weight)),by=.(Qualification, Weeks)][,prop.table(table(Qualification, Weeks),2)])

# ggplot(Qualification,aes(x=Weeks,y=Freq, colour=Qualification)) +
#   geom_line() + 
#   geom_point() + 
#   geom_vline(xintercept=as.Date("2016-03-16")) 

Qualification <- aggregate(Qualification$Freq,list(Qualification$Qualification),FUN=mean)

###############################################################################
#TIDY UP DATA FRAMES
###############################################################################

Children <- data.frame(Group=Children$Group.1,Percent=round(Children$x*100,1))
SocialClass <- data.frame(Group=SocialClass$Group.1,Percent=round(SocialClass$x*100,1))
Income <- data.frame(Group=Income$Group.1,Percent=round(Income$x*100,1))
Qualification <- data.frame(Group=Qualification$Group.1,Percent=round(Qualification$x*100,1))

###############################################################################
#CONSTRUCT TABLE
###############################################################################

rbind(Children,SocialClass, Income,Qualification)

###############################################################################
#TIDY UP
###############################################################################

rm(SocialClassCount,SocialClass,ChildrenCount,Children,
   IncomeCount,Income,HighestQualificationCount,HighestQualification)