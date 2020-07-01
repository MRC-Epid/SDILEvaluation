###############################################################################
#REGRESSION TABLE
#DAVID PELL
#11/11/2019
###############################################################################
#RUN ARMA MODELS RETURNING OBJECTS NAMED BY DRINK/CONFECTIONERY CATEGORY 
#RUN FUNCTIONS FOR ABSOLUTE AND RELATIVE DIFFERENCES
#RUN FUNCTIONS TO EXTRACT ANNOUNCEMENT THEN IMPLEMENTATION TERMS IF A QUADRATIC 
#TERM IS INCLUDED IN THE MODEL
#RUN FUNCTION TO EXTRACT COEFFICIENTS FROM MODEL IN THE ORDER THEY WILL BE INCLUDE
#IN THE TABLE
#RUN FUNCTION TO FORMAT TABLES
#CREATE TABLES
#EXPORT TABLES
#TIDY UP
###############################################################################
#LIBRARIES
###############################################################################

library(officer)

###############################################################################
#OPEN DATA
###############################################################################

if(!exists("WeeklyVolume")){
  source("/dph/sdil/KWP221/Cleaning/ITSData19.R")
}

###############################################################################
#RUN REGRESSION MODELS - VOLUME
###############################################################################

source("/dph/sdil/KWP221/Analysis/Volume/HigherTierVolume.R")
source("/dph/sdil/KWP221/Analysis/Volume/LowerTierVolume.R")
source("/dph/sdil/KWP221/Analysis/Volume/NoLevyVolume.R")
source("/dph/sdil/KWP221/Analysis/Volume/LowSugarVolume.R")
source("/dph/sdil/KWP221/Analysis/Volume/NoSugarVolume.R")
source("/dph/sdil/KWP221/Analysis/Volume/BottledWaterVolume.R")
source("/dph/sdil/KWP221/Analysis/Volume/AlcoholVolume.R")
source("/dph/sdil/KWP221/Analysis/Volume/MilkBasedVolume.R")
source("/dph/sdil/KWP221/Analysis/Volume/NASFruitJuiceVolume.R")
source("/dph/sdil/KWP221/Analysis/Volume/PowderVolume.R")
source("/dph/sdil/KWP221/Analysis/Volume/ConfectioneryVolume.R")

###############################################################################
#RUN REGRESSION MODELS - SUGAR
###############################################################################

source("/dph/sdil/KWP221/Analysis/Sugar/HigherTierSugar.R")
source("/dph/sdil/KWP221/Analysis/Sugar/LowerTierSugar.R")
source("/dph/sdil/KWP221/Analysis/Sugar/NoLevySugar.R")
source("/dph/sdil/KWP221/Analysis/Sugar/LowSugarSugar.R")      
source("/dph/sdil/KWP221/Analysis/Sugar/MilkBasedSugar.R")  
source("/dph/sdil/KWP221/Analysis/Sugar/NASFruitJuiceSugar.R")
source("/dph/sdil/KWP221/Analysis/Sugar/PowderSugar.R")  
source("/dph/sdil/KWP221/Analysis/Sugar/ConfectionerySugar.R")

###############################################################################
#RUN ABSOLUTE DIFFERENCES FUNCTIONS
###############################################################################

source("/dph/sdil/KWP221/Analysis/RegressionTable/AbsoluteDifferencesFunctions19.R")

###############################################################################
#RUN ABSOLUTE DIFFERENCE FUNCTIONS FOR LOWER TIER IMPLEMENTATION
#THIS IS VERY SIMILAR TO THE RELATIVE DIFFERENCE FUNCTION IN THAT IT EXTRACTS ALL
#COEFFICIENTS AND VARIANCE FROM THE MODEL ESTIMATES AND SUBTRACTS THE DIFFERENCE
#ALLOWING DIFFERENT TIME POINTS TO BE SPECIFIED
#IN THE FUTURE THE ABSOLUTE AND RELATIVE FUNCTIONS COULD BE COMBINED THIS WAY
###############################################################################

source("/dph/sdil/KWP221/Analysis/RegressionTable/AbsoluteDifferencesImplementationLowerTier.R")

###############################################################################
#RUN ABSOLUTE DIFFERENCE FUNCTIONS FOR LOWER TIER ANNOUNCEMENT AND IMPLEMENTATION
###############################################################################

source("/dph/sdil/KWP221/Analysis/RegressionTable/AbsoluteDifferencesAnnouncementImplementationLowerTier.R")

###############################################################################
#RUN RELATIVE DIFFERENCES POST ANNOUNCEMENT FUNCTIONS
###############################################################################

source("/dph/sdil/KWP221/Analysis/RegressionTable/RelativeDifferencesFunctionsAnnouncement19.R")

###############################################################################
#RUN RELATIVE DIFFERENCES PRE-ANNOUNCEMENT TO POST IMPLEMENTATION FUNCTIONS
###############################################################################

source("/dph/sdil/KWP221/Analysis/RegressionTable/RelativeDifferencesFunctionsAnnouncementImplementation19.R")

###############################################################################
#RUN RELATIVE DIFFERENCES POST IMPLEMENTATION FUNCTIONS
###############################################################################

source("/dph/sdil/KWP221/Analysis/RegressionTable/RelativeDifferencesFunctionsImplementation19.R")

###############################################################################
#QUADRATIC TREND FUNCTIONS - ANNOUNCEMENT
#COULD COMBINE ANNOUNCEMENT AND IMPLEMENTATION FUNCTIONS THOUGH MIGHT STILL BE MESSY
###############################################################################

ValueA.fun <- function(model){
  model.object = eval(parse(text=paste0(model)))
  ifelse(sum(names(coef(model.object)) %in% "I(AnnouncementIndicatorTrend^2)")==1,
         summary(model.object)$tTable["I(AnnouncementIndicatorTrend^2)","Value"],
         summary(model.object)$tTable["AnnouncementIndicatorTrend","Value"])
}

LowerCIA.fun <- function(model){
  model.object = eval(parse(text=paste0(model)))
  ifelse(sum(names(coef(model.object)) %in% "I(AnnouncementIndicatorTrend^2)")==1,
         confint(model.object)["I(AnnouncementIndicatorTrend^2)","2.5 %"],
         confint(model.object)["AnnouncementIndicatorTrend","2.5 %"])
}

UpperCIA.fun <- function(model){
  model.object = eval(parse(text=paste0(model)))
  ifelse(sum(names(coef(model.object)) %in% "I(AnnouncementIndicatorTrend^2)")==1,
         confint(model.object)["I(AnnouncementIndicatorTrend^2)","97.5 %"],
         confint(model.object)["AnnouncementIndicatorTrend","97.5 %"])
}

###############################################################################
#ANNOUNCEMENT AND IMPLEMENTATION LEVEL CHANGE
###############################################################################

ValueAILevel.fun <- function(model){
  model = eval(parse(text=paste0(model)))
  est <- unname(coef(model)["AnnouncementIndicatorLevel"] + coef(model)["ImplementationIndicatorLevel"])
  #VARIANCE
  se <- sqrt(
    vcov(model)["AnnouncementIndicatorLevel","AnnouncementIndicatorLevel"] +
      vcov(model)["ImplementationIndicatorLevel","ImplementationIndicatorLevel"] +
      (vcov(model)["AnnouncementIndicatorLevel","ImplementationIndicatorLevel"] * 2)
  )
  
  list(est,
       (est + (-1 * qnorm(0.975)) * se),
       (est + ( 1 * qnorm(0.975)) * se))
}

###############################################################################
#ANNOUNCEMENT AND IMPLEMENTATION TREND CHANGE
###############################################################################

ValueAITrend.fun <- function(model){
  model.object = eval(parse(text=paste0(model)))
  if(sum(names(coef(model.object)) %in% "I(AnnouncementIndicatorTrend^2)")==1) {
    
    est <- unname(coef(model.object)["I(AnnouncementIndicatorTrend^2)"] + coef(model.object)["ImplementationIndicatorTrend"])
    se <- sqrt(vcov(model.object)["I(AnnouncementIndicatorTrend^2)","I(AnnouncementIndicatorTrend^2)"] +
                 vcov(model.object)["ImplementationIndicatorTrend","ImplementationIndicatorTrend"] +
                 (vcov(model.object)["I(AnnouncementIndicatorTrend^2)","ImplementationIndicatorTrend"] * 2))
    LowerCI <- est + (-1 * qnorm(0.975)) * se
    UpperCI <- est + ( 1 * qnorm(0.975)) * se
    return(list(est,LowerCI,UpperCI));
  } else{
    est <- unname(coef(model.object)["AnnouncementIndicatorTrend"] + coef(model.object)["ImplementationIndicatorTrend"])
    se <- sqrt(vcov(model.object)["AnnouncementIndicatorTrend","AnnouncementIndicatorTrend"] +
                 vcov(model.object)["ImplementationIndicatorTrend","ImplementationIndicatorTrend"] +
                 (vcov(model.object)["AnnouncementIndicatorTrend","ImplementationIndicatorTrend"] * 2))
    LowerCI <- est + (-1 * qnorm(0.975)) * se
    UpperCI <- est + ( 1 * qnorm(0.975)) * se
    return(list(est,LowerCI,UpperCI))
  }
}

###############################################################################
#LEVEL AND TREND CHANGES TABLE FUNCTION
###############################################################################

LevelTrendTable <- function(model,n=1, m=1000){
  df <- setNames(data.frame(
    #Announcement
    Announcement.Level=summary(eval(parse(text=paste0(model))))$tTable["AnnouncementIndicatorLevel","Value"]*m,
    Announcement.Level.LowerCI=confint(eval(parse(text=paste0(model))))["AnnouncementIndicatorLevel","2.5 %"]*m,
    Announcement.Level.UpperCI=confint(eval(parse(text=paste0(model))))["AnnouncementIndicatorLevel","97.5 %"]*m,
    Announcement.Trend=ValueA.fun(model)*m,
    Announcement.Trend.LowerCI=LowerCIA.fun(model)*m,
    Announcement.Trend.UpperCI=UpperCIA.fun(model)*m,
    #Implementation
    Implementation.Level=summary(eval(parse(text=paste0(model))))$tTable["ImplementationIndicatorLevel",c("Value")]*m,
    Implementation.Level.LowerCI=confint(eval(parse(text=paste0(model))))["ImplementationIndicatorLevel","2.5 %"]*m,
    Implementation.Level.UpperCI=confint(eval(parse(text=paste0(model))))["ImplementationIndicatorLevel","97.5 %"]*m,
    Implementation.Trend=summary(eval(parse(text=paste0(model))))$tTable["ImplementationIndicatorTrend",c("Value")]*m,
    Implementation.Trend.LowerCI=confint(eval(parse(text=paste0(model))))["ImplementationIndicatorTrend","2.5 %"]*m,
    Implementation.Trend.UpperCI=confint(eval(parse(text=paste0(model))))["ImplementationIndicatorTrend","97.5 %"]*m,
    #AnnouncementImplementation
    AnnouncementImplementation.Level=ValueAILevel.fun(model)[[1]]*m,
    AnnouncementImplementation.Level.LowerCAI=ValueAILevel.fun(model)[[2]]*m,
    AnnouncementImplementation.Level.UpperCAI=ValueAILevel.fun(model)[[3]]*m,
    AnnouncementImplementation.Trend=ValueAITrend.fun(model)[[1]]*m,
    AnnouncementImplementation.Trend.LowerCAI=ValueAITrend.fun(model)[[2]]*m,
    AnnouncementImplementation.Trend.UpperCAI=ValueAITrend.fun(model)[[3]]*m
  ),c("AnnouncementLevelChange","AnnouncementLevelChangeLowerCI","AnnouncementLevelChangeUpperCI",
      "AnnouncementTrendChange","AnnouncementTrendChangeLowerCI","AnnouncementTrendChangeUpperCI",
      "ImplementationLevelChange","ImplementationLevelChangeLowerCI","ImplementationLevelChangeUpperCI",
      "ImplementationTrendChange","ImplementationTrendChangeLowerCI","ImplementationTrendChangeUpperCI",
      "AnnouncementImplementationLevelChange","AnnouncementImplementationLevelChangeLowerCI","AnnouncementImplementationLevelChangeUpperCI",
      "AnnouncementImplementationTrendChange","AnnouncementImplementationTrendChangeLowerCI","AnnouncementImplementationTrendChangeUpperCI"))
  return(df)
}

###############################################################################
#REGRESSION COEFFICIENTS TABLE FUNCTION
#POST-ANNOUNCEMENT TO IMPLEMENTATION
###############################################################################

PrePostTable <- function(model,n=1,m=1000){
    df <-setNames(data.frame(
      #Pre-announcement to pre-implementation - change
      AbsoluteChangeAnnouncement.fun(model,n)[1],
      AbsoluteChangeAnnouncement.fun(model,n)[2],
      AbsoluteChangeAnnouncement.fun(model,n)[3],
      RelativeChangeAnnouncement.fun(model,n)[[1]][1],
      RelativeChangeAnnouncement.fun(model,n)[[1]][2],
      RelativeChangeAnnouncement.fun(model,n)[[1]][3],
      #Post-announcement to post-implementation - change
      AbsoluteChangeImplementation.fun(model,n)[1],
      AbsoluteChangeImplementation.fun(model,n)[2],
      AbsoluteChangeImplementation.fun(model,n)[3],
      RelativeChangeImplementation.fun(model,n)[[1]][1],
      RelativeChangeImplementation.fun(model,n)[[1]][2],
      RelativeChangeImplementation.fun(model,n)[[1]][3],
      #Pre-announcement to post-implementation - change
      AbsoluteChangeAnnouncementImplementation.fun(model,n)[1],
      AbsoluteChangeAnnouncementImplementation.fun(model,n)[2],
      AbsoluteChangeAnnouncementImplementation.fun(model,n)[3],
      RelativeChangeAnnouncementImplementation.fun(model,n)[[1]][1],
      RelativeChangeAnnouncementImplementation.fun(model,n)[[1]][2],
      RelativeChangeAnnouncementImplementation.fun(model,n)[[1]][3]
      ),c(
        "PreAPostA.Absolute.Change","PreAPostA.Absolute.Change.LowerCI","PreAPostA.Absolute.Change.UpperCI",
        "PreAPostA.Relative.Change","PreAPostA.Relative.Change.LowerCI","PreAPostA.Relative.Change.UpperCI",
        "PreIPostI.Absolute.Change","PreIPostI.Absolute.Change.LowerCI","PreIPostI.Absolute.Change.UpperCI",
        "PreIPostI.Relative.Change","PreIPostI.Relative.Change.LowerCI","PreIPostI.Relative.Change.UpperCI",
        "PreAPostI.Absolute.Change","PreAPostI.Absolute.Change.LowerCI","PreAPostI.Absolute.Change.UpperCI",
        "PreAPostI.Relative.Change","PreAPostI.Relative.Change.LowerCI","PreAPostI.Relative.Change.UpperCI"))
    return(df)
}

###############################################################################
#REGRESSION COEFFICIENTS TABLE FUNCTION
#POST-ANNOUNCEMENT TO IMPLEMENTATION - LOWER TIER
###############################################################################

PrePostTableLT <- function(model,n=1,TimePointI1=264,TimePointI2=257, m=1000){
  df <-setNames(data.frame(
    #Pre-announcement to pre-implementation - change
    AbsoluteChangeAnnouncement.fun(model,n)[1],
    AbsoluteChangeAnnouncement.fun(model,n)[2],
    AbsoluteChangeAnnouncement.fun(model,n)[3],
    RelativeChangeAnnouncement.fun(model,n)[[1]][1],
    RelativeChangeAnnouncement.fun(model,n)[[1]][2],
    RelativeChangeAnnouncement.fun(model,n)[[1]][3],
    #Post-announcement to post-implementation - change
    AbsoluteChangeILowerTier.fun(model,n,TimePointI1,TimePointI2)[1],
    AbsoluteChangeILowerTier.fun(model,n,TimePointI1,TimePointI2)[2],
    AbsoluteChangeILowerTier.fun(model,n,TimePointI1,TimePointI2)[3],
    RelativeChangeImplementation.fun(model,n,TimePointI1,TimePointI2)[[1]][1],
    RelativeChangeImplementation.fun(model,n,TimePointI1,TimePointI2)[[1]][2],
    RelativeChangeImplementation.fun(model,n,TimePointI1,TimePointI2)[[1]][3],
    #Pre-announcement to post-implementation - change
    AbsoluteChangeAILowerTier.fun(model,n,TimePointI1,TimePointI2)[1],
    AbsoluteChangeAILowerTier.fun(model,n,TimePointI1,TimePointI2)[2],
    AbsoluteChangeAILowerTier.fun(model,n,TimePointI1,TimePointI2)[3],
    RelativeChangeAnnouncementImplementation.fun(model,n,TimePointI1,TimePointI2)[[1]][1],
    RelativeChangeAnnouncementImplementation.fun(model,n,TimePointI1,TimePointI2)[[1]][2],
    RelativeChangeAnnouncementImplementation.fun(model,n,TimePointI1,TimePointI2)[[1]][3]
  ),c(
    "PreAPostA.Absolute.Change","PreAPostA.Absolute.Change.LowerCI","PreAPostA.Absolute.Change.UpperCI",
    "PreAPostA.Relative.Change","PreAPostA.Relative.Change.LowerCI","PreAPostA.Relative.Change.UpperCI",
    "PreIPostI.Absolute.Change","PreIPostI.Absolute.Change.LowerCI","PreIPostI.Absolute.Change.UpperCI",
    "PreIPostI.Relative.Change","PreIPostI.Relative.Change.LowerCI","PreIPostI.Relative.Change.UpperCI",
    "PreAPostI.Absolute.Change","PreAPostI.Absolute.Change.LowerCI","PreAPostI.Absolute.Change.UpperCI",
    "PreAPostI.Relative.Change","PreAPostI.Relative.Change.LowerCI","PreAPostI.Relative.Change.UpperCI"))
  return(df)
}

###############################################################################
#FORMAT TABLE FUNCTION
###############################################################################

FormatLevelTrendTable.fun <- function(Categories,Tab) {
  setNames(data.frame(c(Categories),
                      paste0(Tab$AnnouncementLevelChange," (",
                             Tab$AnnouncementLevelChangeLowerCI,",",
                             Tab$AnnouncementLevelChangeUpperCI,")"),
                      
                      paste0(Tab$AnnouncementTrendChange," (",
                             Tab$AnnouncementTrendChangeLowerCI,",",
                             Tab$AnnouncementTrendChangeUpperCI,")"),
                      
                      paste0(Tab$ImplementationLevelChange," (",
                             Tab$ImplementationLevelChangeLowerCI,",",
                             Tab$ImplementationLevelChangeUpperCI,")"),
                      
                      paste0(Tab$ImplementationTrendChange," (",
                             Tab$ImplementationTrendChangeLowerCI,",",
                             Tab$ImplementationTrendChangeUpperCI,")"),
                      
                      paste0(Tab$AnnouncementImplementationLevelChange," (",
                             Tab$AnnouncementImplementationLevelChangeLowerCI,",",
                             Tab$AnnouncementImplementationLevelChangeUpperCI,")"),
                      
                      paste0(Tab$AnnouncementImplementationTrendChange," (",
                             Tab$AnnouncementImplementationTrendChangeLowerCI,",",
                             Tab$AnnouncementImplementationTrendChangeUpperCI,")")),
                      
           c("Categories",
             "Announcement Level change (ml/g)",
             "Announcement Trend change (ml/g per week)",
             "Implementation Level change (ml/g)",
             "Implementation Trend change (ml/g per week)",
             "Announcement Implementation Level change (ml/g)",
             "Announcement Implementation Trend change (ml/g per week)"))
}

###############################################################################
#FORMAT TABLE FUNCTION
###############################################################################

FormatPrePostTable.fun <- function(Categories,Tab) {
  setNames(data.frame(c(Categories),
                      paste0(Tab$PreAPostA.Absolute.Change," (",
                             Tab$PreAPostA.Absolute.Change.LowerCI,",",
                             Tab$PreAPostA.Absolute.Change.UpperCI,")"),
                      
                      paste0(Tab$PreAPostA.Relative.Change," (",
                             Tab$PreAPostA.Relative.Change.LowerCI,",",
                             Tab$PreAPostA.Relative.Change.UpperCI,")"),
                                         
                      paste0(Tab$PreIPostI.Absolute.Change," (",
                             Tab$PreIPostI.Absolute.Change.LowerCI,",",
                             Tab$PreIPostI.Absolute.Change.UpperCI,")"),
                      
                      paste0(Tab$PreIPostI.Relative.Change," (",
                             Tab$PreIPostI.Relative.Change.LowerCI,",",
                             Tab$PreIPostI.Relative.Change.UpperCI,")"),
                      
                      paste0(Tab$PreAPostI.Absolute.Change," (",
                             Tab$PreAPostI.Absolute.Change.LowerCI,",",
                             Tab$PreAPostI.Absolute.Change.UpperCI,")"),
                      
                      paste0(Tab$PreAPostI.Relative.Change," (",
                             Tab$PreAPostI.Relative.Change.LowerCI,",",
                             Tab$PreAPostI.Relative.Change.UpperCI,")")),
           c("Categories",
             "Pre-announcement to post-announcement absolute change (ml/g)",
             "Pre-announcement to post-announcement relative change (%)",
             "Pre-implementation to post-implementation absolute change (ml)",
             "Pre-implementation to post-implementation relative change (%)",
             "Pre-announcement to post-implementation absolute change (ml)",
             "Pre-announcement to post-implementation relative change (%)"))
}

###############################################################################
#TURN OFF SCIENTIFIC NOTATION
###############################################################################

options(scipen=999)

###############################################################################
#TABLE 2. LEVEL AND TREND CHANGES: VOLUME
###############################################################################

Table2 <- data.frame(lapply(rbind(
  "HigherTierVolume"    =LevelTrendTable("HigherTier"),
  "LowerTierVolume"     =LevelTrendTable("LowerTier"),
  "NoLevyVolume"        =LevelTrendTable("NoLevy"),
  "LowSugarVolume"      =LevelTrendTable("LowSugar"),
  "NoSugarVolume"       =LevelTrendTable("NoSugar"),
  "BottledWaterVolume"  =LevelTrendTable("BottledWater"),
  "AlcoholVolume"       =LevelTrendTable("Alcohol"),
  "MilkBasedVolume"     =LevelTrendTable("MilkBased"),
  "NASFruitJuiceVolume" =LevelTrendTable("NASFruitJuice"),
  "PowderVolume"        =LevelTrendTable("Powder"),
  "ConfectioneryVolume" =LevelTrendTable("Confectionery")),round,1))

###############################################################################
#TABLE 3. CHANGES BETWEEN COUNTERFACTUAL: VOLUME
###############################################################################

Table3 <- data.frame(lapply(rbind(
"HigherTierVolume"    =PrePostTable("HigherTier"),
"LowerTierVolume"     =PrePostTableLT("LowerTier", TimePointI2=257),
"NoLevyVolume"        =PrePostTable("NoLevy"),
"LowSugarVolume"      =PrePostTable("LowSugar"),
"NoSugarVolume"       =PrePostTable("NoSugar"),
"BottledWaterVolume"  =PrePostTable("BottledWater"),
"AlcoholVolume"       =PrePostTable("Alcohol"),
"MilkBasedVolume"     =PrePostTable("MilkBased"),
"NASFruitJuiceVolume" =PrePostTable("NASFruitJuice"),
"PowderVolume"        =PrePostTable("Powder"),
"ConfectioneryVolume" =PrePostTable("Confectionery")),round,1))

###############################################################################
#TABLE 4. LEVEL AND TREND CHANGES: SUGAR
###############################################################################

Table4 <- data.frame(lapply(rbind(
  "HigherTierSugar"    =LevelTrendTable(model="HigherTierSugar",    n=1000, m=1),
  "LowerTierSugar"     =LevelTrendTable(model="LowerTierSugar",     n=1000, m=1), #^2
  "NoLevySugar"        =LevelTrendTable(model="NoLevySugar",        n=1000, m=1), #^2
  "LowSugarSugar"      =LevelTrendTable(model="LowSugarSugar",      n=1000, m=1), #^2
  "MilkBasedSugar"     =LevelTrendTable(model="MilkBasedSugar",     n=1000, m=1),
  "NASFruitJuiceSugar" =LevelTrendTable(model="NASFruitJuiceSugar", n=1000, m=1),
  "PowderSugar"        =LevelTrendTable(model="PowderSugar",        n=1000, m=1),
  "ConfectionerySugar" =LevelTrendTable(model="ConfectionerySugar", n=1000, m=1)),round,1))

###############################################################################
#TABLE 5. CHANGES BETWEEN COUNTERFACTUAL: SUGAR
#THE FINAL POSITIVE VALUE FOR LOWER TIER SUGAR OCCURED IN WEEK 253
###############################################################################

Table5 <- data.frame(lapply(rbind(
  "HigherTierSugar"    =PrePostTable(model="HigherTierSugar",    n=1000, m=1),
  "LowerTierSugar"     =PrePostTableLT(model="LowerTierSugar",   n=1000,TimePointI2=253, m=1), #^2
  "NoLevySugar"        =PrePostTable(model="NoLevySugar",        n=1000, m=1), #^2
  "LowSugarSugar"      =PrePostTable(model="LowSugarSugar",      n=1000, m=1), #^2
  "MilkBasedSugar"     =PrePostTable(model="MilkBasedSugar",     n=1000, m=1),
  "NASFruitJuiceSugar" =PrePostTable(model="NASFruitJuiceSugar", n=1000, m=1),
  "PowderSugar"        =PrePostTable(model="PowderSugar",        n=1000, m=1),
  "ConfectionerySugar" =PrePostTable(model="ConfectionerySugar", n=1000, m=1)),round,1))

###############################################################################
#CONSTRUCT TABLE OUTPUT
###############################################################################

doc <- read_docx() %>% 
       body_add_table(#TABLE 2
       FormatLevelTrendTable.fun(Categories=c("HigherTierVolume","LowerTierVolume","NoLevyVolume","LowSugarVolume","NoSugarVolume","BottledWaterVolume",
                                      "AlcoholVolume","MilkBasedVolume","NASFruitJuiceVolume","PowderVolume","ConfectioneryVolume"),Tab=Table2))

doc <- body_add_par(doc, value="") #CREATE GAP BETWEEN TABLES

doc <- body_add_table(doc, #TABLE 3
       FormatPrePostTable.fun(Categories=c("HigherTierVolume","LowerTierVolume","NoLevyVolume","LowSugarVolume","NoSugarVolume","BottledWaterVolume",
                                    "AlcoholVolume","MilkBasedVolume","NASFruitJuiceVolume","PowderVolume","ConfectioneryVolume"),Tab=Table3))

doc <- body_add_par(doc, value="") #CREATE GAP BETWEEN TABLES

doc <- body_add_table(doc, #TABLE 4
       FormatLevelTrendTable.fun(Categories=c("HigherTierSugar","LowerTierSugar","NoLevySugar","LowSugarSugar",
                                                          "MilkBasedSugar","NASFruitJuiceSugar","PowderSugar","ConfectionerySugar"),Tab=Table4))
doc <- body_add_par(doc, value="") #CREATE GAP BETWEEN TABLES

doc <- body_add_table(doc, #TABLE 5
       FormatPrePostTable.fun(Categories=c("HigherTierSugar","LowerTierSugar","NoLevySugar","LowSugarSugar",
                                "MilkBasedSugar","NASFruitJuiceSugar","PowderSugar","ConfectionerySugar"),Tab=Table5))

print(doc, target = "/dph/sdil/KWP221/Analysis/RegressionTable/RegressionTables.docx") #EXPORT TABLES

##############################################################################
#TIDY UP
##############################################################################

# rm(HigherTier,LowerTier,NoLevy,Confectionery,NoSugar,LowSugar,Alcohol,BottledWater,MilkBased,NASFruitJuice,
#    Powder,HigherTierSugar,LowerTierSugar,NoLevySugar,ConfectionerySugar,MilkBasedSugar,
#    NASFruitJuiceSugar,PowderSugar,SDIL,AbsDiff.fun,AbsDiffQuad.fun, AbsDiffQuadSugar.fun,
#    AbsDiffSugar.fun,AbsoluteChange.fun, AbsoluteChangeSugar.fun,LowerCI.fun, LowSugarSugar,                     
#    RelativeChange.fun, RelativeChangeSugar.fun, RelativeDifferencesEaster.fun,
#    RelativeDifferencesEasterSugar.fun,RelativeDifferences.fun,RelativeDifferencesQuad.fun,
#    RelativeDifferencesQuadSugar.fun,RelativeDifferencesSugar.fun,Table,UpperCI.fun,
#    Value.fun,Table,WeeklyVolume)
