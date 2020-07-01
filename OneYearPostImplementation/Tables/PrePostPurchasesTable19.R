###############################################################################
#TABLE OF MEAN INTAKES BEFORE AND AFTER CONSUMPTION
#DAVID PELL
#06/11/19
###############################################################################
#LIBRARIES
###############################################################################

library(reshape2)
library(officer)

###############################################################################
#OPEN DATA
###############################################################################

source("/dph/sdil/KWP221/Analysis/PrePostPurchases/CleanKWP19.R")

###############################################################################
#OPEN WEEKLY VOLUME - THIS IS NOT THE STANDARD ONE AS EXTRA GROUPS ARE CREATED
###############################################################################

source("/dph/sdil/KWP221/Analysis/PrePostPurchases/ITSData19.R")

###############################################################################
#GET MEANS PER WEEK
###############################################################################

MeanValues <- WeeklyVolume[, .(volume = mean(volume), volumesd = sd(volume), 
                        sugar = mean(sugar), sugarsd= sd(sugar)), by = c("SDIL2","SDILAnnouncement","SDILImplementation")]

###############################################################################
#CONVERT VOLUME TO ML
###############################################################################

MeanValues$volume <- MeanValues$volume*1000
MeanValues$volumesd <- MeanValues$volumesd*1000

###############################################################################
#CREATE ID VARIABLE
###############################################################################

MeanValues$Period <- ifelse(MeanValues$SDILAnnouncement == 0 & MeanValues$SDILImplementation == 0, "Pre-SDIL",
                            ifelse(MeanValues$SDILAnnouncement == 1 & MeanValues$SDILImplementation == 0, "Pre-Implementation","Post-Implementation"))

###############################################################################
#ORDER FACTOR LEVELS
###############################################################################

MeanValues$Period <- factor(MeanValues$Period, levels=c("Pre-SDIL","Pre-Implementation","Post-Implementation"))

###############################################################################
#REARRANGE DATA FRAME
###############################################################################

MeanValues <- MeanValues[,c("SDIL2","Period","volume","volumesd","sugar","sugarsd")]

###############################################################################
#ROUND VALUES
###############################################################################

MeanValues[,c(3:6)] <- round(MeanValues[,c(3:6)],1)

###############################################################################
#ORDER SDIL2 GROUPS
###############################################################################

RowOrder =c("HigherTier","LowerTier","NoLevy","LowSugar","NoSugar",
            "BottledWater","Alcohol","MilkBased","Milk","Milkshakes","NASFruitJuice",
            "Powder","Confectionery","Toiletries")

###############################################################################
#CONSTRUCT TABLE
###############################################################################

Table <- 
setNames(
  data.frame(
    dcast(MeanValues, factor(SDIL2, levels=RowOrder) ~ Period, value.var=c("volume"))[,1:2],
    dcast(MeanValues, factor(SDIL2, levels=RowOrder) ~ Period, value.var=c("volumesd"))[,2],
    dcast(MeanValues, factor(SDIL2, levels=RowOrder) ~ Period, value.var=c("volume"))[,3],
    dcast(MeanValues, factor(SDIL2, levels=RowOrder) ~ Period, value.var=c("volumesd"))[,3],
    dcast(MeanValues, factor(SDIL2, levels=RowOrder) ~ Period, value.var=c("volume"))[,4],
    dcast(MeanValues, factor(SDIL2, levels=RowOrder) ~ Period, value.var=c("volumesd"))[,4],
    dcast(MeanValues, factor(SDIL2, levels=RowOrder) ~ Period, value.var=c("sugar"))[,2],
    dcast(MeanValues, factor(SDIL2, levels=RowOrder) ~ Period, value.var=c("sugarsd"))[,2],
    dcast(MeanValues, factor(SDIL2, levels=RowOrder) ~ Period, value.var=c("sugar"))[,3],
    dcast(MeanValues, factor(SDIL2, levels=RowOrder) ~ Period, value.var=c("sugarsd"))[,3],
    dcast(MeanValues, factor(SDIL2, levels=RowOrder) ~ Period, value.var=c("sugar"))[,4],
    dcast(MeanValues, factor(SDIL2, levels=RowOrder) ~ Period, value.var=c("sugarsd"))[,4]),
  c("Category","Pre-SDIL-volume","Pre-SDIL-volume sd",
    "Pre-Implementation-volume","Pre-Implementation-volume sd",
    "Post-Implementation-volume","Post-Implementation-volume sd",
    "Pre-SDIL-sugar","Pre-SDIL-sugar sd",
    "Pre-Implementation-SDIL-sugar","Pre-Implementation-SDIL-sugar sd",
    "Post-Implementation-sugar","Post-Implementation-sugar sd"
  ))

###############################################################################
#ADD EMPTY LINES TO TABLE #IGNORE WARNINGS
###############################################################################

Table <- rbind(
  c("Liable drinks","","","","","","","","","","","",""),
  Table[1:6,],
  c("Exempt drinks","","","","","","","","","","","",""),
  Table[7:13,])

###############################################################################
#OUTPUT TABLE
###############################################################################

doc <- read_docx() %>%
  body_add_table(Table,
    style = "table_template")
print(doc, target = "/dph/sdil/KWP221/TablePrePostIntake.docx")

