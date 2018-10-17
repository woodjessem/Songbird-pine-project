#load package
library(vegan)
library(dplyr)

setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")
CSV <- read.csv("17_18_acdata_dels.csv")
#CSV
str(CSV)

#Species Accumulation Curve - Species AREA relationship - accumulation by number of independent camera location
# edited for 2018 PC data - all sites treated as independent_cam_loc, Species is column
ICS <- tapply(CSV$Total,list(CSV$Site, CSV$Species), sum)  #create data frame of species sum by independent camera location - all sites
ICS
ICS[is.na(ICS)]<-0
class(ICS)
#convert matrix to dataframe by exporting table and importing CSV
write.table(ICS,"ICS.csv", sep=',', col.names=TRUE)
ICSdata <- read.csv('ICS.csv', header = TRUE)
class(ICSdata) #if it is a dataframe you are good to go
sa1 <- specaccum(ICSdata) #species accumulation curve - "exact"
sa2 <- specaccum(ICSdata, "random") #species accumulation curve for boxplot
plot(sa1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue", 
     ylab="Number of Species", xlab="Independent Sites",
     main="SAC for area sampled across all sites")
boxplot(sa2, col="yellow", add=TRUE, pch="+")

plot(sa2, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue", 
     ylab="Number of Species", xlab="Independent Sites (random)",
     main="SAC for area sampled across all sites")

#SHSS <- diversity(ICSdata, index="shannon")
#SHSS
#SISS <- diversity(ICSdata, "simpson")
#SISS


#Species Accumulation Curve - Sampling Occasion /Species Richness relationship - accumulation by visits - all study sites/all photos
#2018 work/edits!  Spp Accum Curve - Sampling Occasion/Species Richness relationship - accumulation by VISITS - all study sites/all photos
VS <- tapply(CSV$Total, list(CSV$Survey, CSV$Species), sum)
VS                   #check out data - should have species total counts for each day surveyed
VS[is.na(VS)]<-0                                                    #convert NA's to 0
class(VS)            #if "matrix" need to convert to "data.frame" (following 2 lines)
write.table(VS,"VS.csv", sep=',', col.names=TRUE)
VSdata <- read.csv('VS.csv', header = TRUE)
class(VSdata)
sav1 <- specaccum(VSdata)
sav2 <- specaccum(VSdata, "random", permutations = 100)
plot(sav1, xaxt="n",ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue",
     ylab="Number of Species", xlab="Visits",
     main="SAC for visits (across all sites)")
axis(1, at=seq(1, 3, by=1) , cex.axis=1)
boxplot(sav2, col="yellow", add=TRUE, pch="+")

results <- with(sav2, data.frame(sites, richness, sd))
results

plot(sav2, xaxt="n",ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue",
     ylab="Number of Species", xlab="Visits (random across sites)",
     main="SAC for visits (across all sites)")
axis(1, at=seq(1, 3, by=1) , cex.axis=1)

#rarefaction plot - what would happen if I had fewer visits - look backward
#https://www.researchgate.net/post/Does_anyone_know_what_the_difference_is_between_an_accumulation_and_rarefaction_curve
sav3 <- specaccum(VSdata, method="rarefaction")
plot(sav3, xaxt="n",ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue",
     ylab="Number of Species", xlab="Visits",
     main="Rarefaction curve for visits (across all sites)")
axis(1, at=seq(1, 3, by=1) , cex.axis=1)

#Species Accumulation Curve - Sampling Time/Species Richness relationship - accumulation by time surveyed - all study sites/all photos
#2018 work/edits!  Spp Accum Curve - Sampling Time/Species Richness relationship - accumulation by time surveyed - all study sites/all photos
DS <- tapply(CSV$Total, list(CSV$Interval, CSV$Species), sum)
DS                   #check out data - should have species total counts for each time interval surveyed
DS[is.na(DS)]<-0                                                    #convert NA's to 0
class(DS)            #if "matrix" need to convert to "data.frame" (following 2 lines)
write.table(DS,"DS.csv", sep=',', col.names=TRUE)
DSdata <- read.csv('DS.csv', header = TRUE)
class(DSdata)
sat1 <- specaccum(DSdata)
sat2 <- specaccum(DSdata, "random")
plot(sat1, xaxt="n",ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue",
     ylab="Number of Species", xlab="Survey Intervals",
     main="SAC for time intervals sampled across all sites")
axis(1, at=seq(1, 4, by=1) , cex.axis=1)
boxplot(sat2, col="yellow", add=TRUE, pch="+")

plot(sat2, xaxt="n",ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue",
     ylab="Number of Species", xlab="Survey Intervals (random across sites)",
     main="SAC for time intervals sampled across all sites")
axis(1, at=seq(1, 4, by=1) , cex.axis=1)

#work backwards with rarefaction?
sat3 <- specaccum(DSdata, method="rarefaction")
plot(sat3, xaxt="n",ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue",
     ylab="Number of Species", xlab="Survey Intervals",
     main="rarefaction curve for time intervals (all sites)")
axis(1, at=seq(1, 4, by=1) , cex.axis=1)

######################################################################################################
#ignore lines 99-195 right now
#Species Accumulation Curve - BDA - Sampling Time accumulation
#2018 editing - call out one individual site
BLEASES <- with(CSV[CSV$Site=="Blease_3B_6",], tapply(Total, list(Interval, Species), sum))
BLEASES[is.na(BLEASES)] <- 0
class(BLEASES)                    #check to make sure it is a data.frame
write.table(BLEASES,"BLEASES.csv", sep=',', col.names=TRUE)
BLEASESdata <- read.csv('BLEASES.csv', header = TRUE)
class(BLEASESdata)
BLEASE1 <- specaccum(BLEASESdata)
BLEASE2 <- specaccum(BLEASESdata, "random")
jpeg(filename='Blease3B6specaccum.jpg', 
     width=1600, 
     height=1200, 
     units='px', 
     res=100)
par(mai=c(1,1.5,1,1),
    cex.lab=2, 
    cex.axis=1.5, 
    cex.main=2)
plot(BLEASE1, xaxt="n", ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue", ylab="Number of Species", xlab="Survey Intervals", main="SAC for Intervals Sampled on Blease 3B6")
axis(1, at=seq(1, 4, by=1) , cex.axis=1)


#experimenting with multiple sites, 1 plot

CSV <- read.csv("17_18_acdata_dels.csv")

#Species Accumulation Curve - Sampling Time/Species Richness relationship - accumulation by time surveyed - all study sites/all photos
#2018 work/edits!  Spp Accum Curve - Sampling Time/Species Richness relationship - accumulation by time surveyed - all study sites/all photos
DS <- tapply(CSV$Total, list(CSV$Interval, CSV$Species), sum)
DS                   #check out data - should have species total counts for each day surveyed
DS[is.na(DS)]<-0                                                    #convert NA's to 0
class(DS)            #if "matrix" need to convert to "data.frame" (following 2 lines)
write.table(DS,"DS.csv", sep=',', col.names=TRUE)
DSdata <- read.csv('DS.csv', header = TRUE)
class(DSdata)
sat1 <- specaccum(DSdata)
sat2 <- specaccum(DSdata, "random")
plot(sat1, xaxt="n",ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue", ylab="Number of Species", xlab="Survey Intervals", main="Species Accumulation Curve for time intervals sampled across all study sites")
axis(1, at=seq(1, 4, by=1) , cex.axis=1)


curve_all <- specaccum(DSdata, method="random")

###subset sites
#DSdata %>% filter(Site == "Blease_3B_6") -> "3B6"
#DSdata %>% filter(Site == "Blease_3B_9") -> "3B9"
#DSdata %>% filter(Site == "Timberhaven_2B_1") -> "2B1"
###calc species accumulation curve for each habitat
#curve_3B6 = specaccum("3B6", method = "random")
#curve_3B9 = specaccum(rubble[, 2:76], method = "random")
#curve_2B1 = specaccum(sand[, 2:76], method = "random")

#all individual sites - separate data and generate curves!
BLEASE3B6 <- with(CSV[CSV$Site=="Blease_3B_6",], tapply(Total, list(Interval, Species), sum))
BLEASE3B6[is.na(BLEASE3B6)] <- 0
class(BLEASE3B6)                    #check to make sure it is a data.frame
write.table(BLEASE3B6,"BLEASE3B6.csv", sep=',', col.names=TRUE)
BLEASE3B6data <- read.csv('BLEASE3B6.csv', header = TRUE)
class(BLEASE3B6data)
curve_3B6 <- specaccum(BLEASE3B6data, "random")

BLEASE3B9 <- with(CSV[CSV$Site=="Blease_3B_9",], tapply(Total, list(Interval, Species), sum))
BLEASE3B9[is.na(BLEASE3B9)] <- 0
class(BLEASE3B9)                    #check to make sure it is a data.frame
write.table(BLEASE3B9,"BLEASE3B9.csv", sep=',', col.names=TRUE)
BLEASE3B9data <- read.csv('BLEASE3B9.csv', header = TRUE)
class(BLEASE3B9data)
curve_3B9 <- specaccum(BLEASE3B9data, "random")

SHEALY2B3 <- with(CSV[CSV$Site=="Shealy_2B_3",], tapply(Total, list(Interval, Species), sum))
SHEALY2B3[is.na(SHEALY2B3)] <- 0
class(SHEALY2B3)                    #check to make sure it is a data.frame
write.table(SHEALY2B3,"SHEALY2B3.csv", sep=',', col.names=TRUE)
SHEALY2B3data <- read.csv('SHEALY2B3.csv', header = TRUE)
class(SHEALY2B3data)
curve_2B3 <- specaccum(SHEALY2B3data, "random")

HUDSON2B7 <- with(CSV[CSV$Site=="Hudson_2B_7",], tapply(Total, list(Interval, Species), sum))
HUDSON2B7[is.na(HUDSON2B7)] <- 0
class(HUDSON2B7)                    #check to make sure it is a data.frame
write.table(HUDSON2B7,"HUDSON2B7.csv", sep=',', col.names=TRUE)
HUDSON2B7data <- read.csv('HUDSON2B7.csv', header = TRUE)
class(HUDSON2B7data)
curve_2B7 <- specaccum(HUDSON2B7data, "random")


#plot curve_all first
plot(curve_all, xaxt="n",ylab="Number of Species", xlab="Survey Intervals",
     main="SAC for time intervals sampled across all sites")
axis(1, at=seq(1, 4, by=1) , cex.axis=1)
#then plot the rest
plot(curve_3B6, add = TRUE, col = 2) #col is COLOUR setting, so change it to something else if you want
plot(curve_3B9, add = TRUE, col = 3)
plot(curve_2B3, add = TRUE, col = 4)
plot(curve_2B7, add = TRUE, col = 5)
######################################################################################################




### NEW TRY ####   - working best! for TIME

setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")
CSV <- read.csv("17_18_acdata_dels.csv")
DS <- tapply(CSV$Total, list(CSV$Interval, CSV$Species), sum)
#DS                   #check out data - should have species total counts for each time interval surveyed
DS[is.na(DS)]<-0                                                    #convert NA's to 0
class(DS)            #if "matrix" need to convert to "data.frame" (following 2 lines)
write.table(DS,"DS.csv", sep=',', col.names=TRUE)
DSdata <- read.csv('DS.csv', header = TRUE)
class(DSdata)
sat1 <- specaccum(DSdata)
sat2 <- specaccum(DSdata, "random")
#draw the plot for an overall frame
plot(sat1, xaxt="n",ci.type="poly", col="purple", lwd=2, ci.lty=0, ci.col="thistle",
     ylab="Number of Species", xlab="Survey Intervals",
     main="SAC for time intervals sampled across all sites")
axis(1, at=seq(1, 4, by=1) , cex.axis=1)
#boxplot(sat2, col="yellow", add=TRUE, pch="+")

### or, instead of manually, make a dataframe csv of each property and just call that in (skip lines 1-4),
## then name the curve, and plot all the curves together
setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/ac_properties")
#Tdat <- read.csv("17_18_acdata_all_property_spp_totals.csv")

#if have loaded overall SR curve, do NOT run lines 202-205, run just 2016 instead!

cc <- palette()
palette(c(cc,"purple","brown"))
palette()

Battlewood_1B_8data <- read.csv('Battlewood_1B_8.csv', header = TRUE)
curve_1B8 <- specaccum(Battlewood_1B_8data, "exact")
#plot(curve_1B8, xaxt="n",ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue",
#     ylab="Number of Species", xlab="Survey Intervals",
#     main="SAC for time intervals sampled across all sites")
#  axis(1, at=seq(1, 4, by=1) , cex.axis=1)
plot(curve_1B8, add = TRUE, col = "purple")

Blease_3B_6data <- read.csv('Blease_3B_6.csv', header = TRUE)
curve_3B6 <- specaccum(Blease_3B_6data, "exact")
plot(curve_3B6, add = TRUE, col = "purple")

Blease_3B_9data <- read.csv('Blease_3B_9.csv', header = TRUE)
curve_3B9 <- specaccum(Blease_3B_9data, "exact")
plot(curve_3B9, add = TRUE, col = "purple")

Bryson_2B_9data <- read.csv('Bryson_2B_9.csv', header = TRUE)
curve_2B9 <- specaccum(Bryson_2B_9data, "exact")
plot(curve_2B9, add = TRUE, col = "purple")

Burnett_1B_5data <- read.csv('Burnett_1B_5.csv', header = TRUE)
curve_1B5 <- specaccum(Burnett_1B_5data, "exact")
plot(curve_1B5, add = TRUE, col = "purple")

Creswell_0B_8data <- read.csv('Creswell_0B_8.csv', header = TRUE)
curve_0B8 <- specaccum(Creswell_0B_8data, "exact")
plot(curve_0B8, add = TRUE, col = "purple")

Creswell_2B_10data <- read.csv('Creswell_2B_10.csv', header = TRUE)
curve_2B10 <- specaccum(Creswell_2B_10data, "exact")
plot(curve_2B10, add = TRUE, col = "purple")

Gosnell_0B_5Bdata <- read.csv('Gosnell_0B_5B.csv', header = TRUE)
curve_0B5B <- specaccum(Gosnell_0B_5Bdata, "exact")
plot(curve_0B5B, add = TRUE, col = "purple")

Hood_Creek_Rd_0B_7data <- read.csv('Hood_Creek_Rd_0B_7.csv', header = TRUE)
curve_0B7 <- specaccum(Hood_Creek_Rd_0B_7data, "exact")
plot(curve_0B7, add = TRUE, col = "purple")

Hudson_2B_7data <- read.csv('Hudson_2B_7.csv', header = TRUE)
curve_2B7 <- specaccum(Hudson_2B_7data, "exact")
plot(curve_2B7, add = TRUE, col = "purple")

Kemp_0B_10data <- read.csv('Kemp_0B_10.csv', header = TRUE)
curve_0B10 <- specaccum(Kemp_0B_10data, "exact")
plot(curve_0B10, add = TRUE, col = "purple")

Kemp_1B_1data <- read.csv('Kemp_1B_1.csv', header = TRUE)
curve_1B1 <- specaccum(Kemp_1B_1data, "exact")
plot(curve_1B1, add = TRUE, col = "purple")

Kemp_2B_5data <- read.csv('Kemp_2B_5.csv', header = TRUE)
curve_2B5 <- specaccum(Kemp_2B_5data, "exact")
plot(curve_2B5, add = TRUE, col = "purple")

Kessler_3B_4data <- read.csv('Kessler_3B_4.csv', header = TRUE)
curve_3B4 <- specaccum(Kessler_3B_4data, "exact")
plot(curve_3B4, add = TRUE, col = "purple")

Kessler_3B_5Bdata <- read.csv('Kessler_3B_5B.csv', header = TRUE)
curve_3B5B <- specaccum(Kessler_3B_5Bdata, "exact")
plot(curve_3B5B, add = TRUE, col = "purple")

Mathis_1B_6data <- read.csv('Mathis_1B_6.csv', header = TRUE)
curve_1B6 <- specaccum(Mathis_1B_6data, "exact")
plot(curve_1B6, add = TRUE, col = "purple")

Mills_0B_9data <- read.csv('Mills_0B_9.csv', header = TRUE)
curve_0B9 <- specaccum(Mills_0B_9data, "exact")
plot(curve_0B9, add = TRUE, col = "purple")

Mills_1B_45data <- read.csv('Mills_1B_45.csv', header = TRUE)
curve_1B45 <- specaccum(Mills_1B_45data, "exact")
plot(curve_1B45, add = TRUE, col = "purple")

Shealy_0B_2data <- read.csv('Shealy_0B_2.csv', header = TRUE)
curve_0B2 <- specaccum(Shealy_0B_2data, "exact")
plot(curve_0B2, add = TRUE, col = "purple")

Shealy_0B_4data <- read.csv('Shealy_0B_4.csv', header = TRUE)
curve_0B4 <- specaccum(Shealy_0B_4data, "exact")
plot(curve_0B4, add = TRUE, col = "purple")

Shealy_1B_3Bdata <- read.csv('Shealy_1B_3B.csv', header = TRUE)
curve_1B3B <- specaccum(Shealy_1B_3Bdata, "exact")
plot(curve_1B3B, add = TRUE, col = "purple")

Shealy_1B_E_Sdata <- read.csv('Shealy_1B_E_S.csv', header = TRUE)
curve_1BES <- specaccum(Shealy_1B_E_Sdata, "exact")
plot(curve_1BES, add = TRUE, col = "purple")

Shealy_2B_3data <- read.csv('Shealy_2B_3.csv', header = TRUE)
curve_2B3 <- specaccum(Shealy_2B_3data, "exact")
plot(curve_2B3, add = TRUE, col = "purple")

Suggs_0B_E_Sdata <- read.csv('Suggs_0B_E_S.csv', header = TRUE)
curve_0BES <- specaccum(Suggs_0B_E_Sdata, "exact")
plot(curve_0BES, add = TRUE, col = "purple")

Swanson_3B_2data <- read.csv('Swanson_3B_2.csv', header = TRUE)
curve_3B2 <- specaccum(Swanson_3B_2data, "exact")
plot(curve_3B2, add = TRUE, col = "purple")

Timberhaven_2B_1data <- read.csv('Timberhaven_2B_1.csv', header = TRUE)
curve_2B1 <- specaccum(Timberhaven_2B_1data, "exact")
plot(curve_2B1, add = TRUE, col = "purple")

Turkey_Rd_2B_6Adata <- read.csv('Turkey_Rd_2B_6A.csv', header = TRUE)
curve_2B6A <- specaccum(Turkey_Rd_2B_6Adata, "exact")
plot(curve_2B6A, add = TRUE, col = "purple")

Abercrombie_Rd_0B_E_ABdata <- read.csv('Turner_Abercrombie_Rd_0B_E_AB.csv', header = TRUE)
curve_0BEAB <- specaccum(Abercrombie_Rd_0B_E_ABdata, "exact")
plot(curve_0BEAB, add = TRUE, col = "purple")

Abercrombie_Rd_1B_2data <- read.csv('Turner_Abercrombie_Rd_1B_2.csv', header = TRUE)
curve_1B2 <- specaccum(Abercrombie_Rd_1B_2data, "exact")
plot(curve_1B2, add = TRUE, col = "purple")

Honea_Path_0B_1data <- read.csv('Turner_Honea_Path_0B_1.csv', header = TRUE)
curve_0B1 <- specaccum(Honea_Path_0B_1data, "exact")
plot(curve_0B1, add = TRUE, col = "purple")

Honea_Path_3B_1data <- read.csv('Turner_Honea_Path_3B_1.csv', header = TRUE)
curve_3B1 <- specaccum(Honea_Path_3B_1data, "exact")
plot(curve_3B1, add = TRUE, col = "purple")

Honea_Path_3B_3data <- read.csv('Turner_Honea_Path_3B_3.csv', header = TRUE)
curve_3B3 <- specaccum(Honea_Path_3B_3data, "exact")
plot(curve_3B3, add = TRUE, col = "purple")

###########################################################################
###########################################################################
##########################################################################################
#SAC for visits now ---- both 2017 and 2018 together next lines

setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")
CSV <- read.csv("17_18_acdata_dels.csv")
VS <- tapply(CSV$Total, list(CSV$Survey, CSV$Species), sum)
VS                   #check out data - should have species total counts for each day surveyed
VS[is.na(VS)]<-0                                                    #convert NA's to 0
class(VS)            #if "matrix" need to convert to "data.frame" (following 2 lines)
write.table(VS,"VS.csv", sep=',', col.names=TRUE)
VSdata <- read.csv('VS.csv', header = TRUE)
class(VSdata)
sav1 <- specaccum(VSdata)
sav2 <- specaccum(VSdata, "random")
plot(sav1, xaxt="n",ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue",
     ylab="Number of Species", xlab="Visits",
     main="SAC for visits (across all sites)")
axis(1, at=seq(1, 3, by=1) , cex.axis=1)
#boxplot(sav2, col="yellow", add=TRUE, pch="+")

setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/ac_properties")

Battlewood_1B8_datav <- read.csv('Battlewood1B8v.csv', header = TRUE)
Battlewood_1B8_datav <- Battlewood_1B8_datav[]
Battlewood_1B8_datav
curve_1B8v <- specaccum(Battlewood_1B8_datav, "exact")
#plot(curve_1B8v, xaxt="n",ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue",
#     ylab="Number of Species", xlab="Survey Visits",
#     main="SAC for survey visits sampled across all sites")
#  axis(1, at=seq(1, 4, by=1) , cex.axis=1)
plot(curve_1B8v, add = TRUE, col = 4)
results <- with(curve_1B8v, data.frame(sites, richness, sd))
results

Blease_3B6_datav <- read.csv('Blease3B6v.csv', header = TRUE)
curve_3B6v <- specaccum(Blease_3B6_datav, "exact")
plot(curve_3B6v, add = TRUE, col = 4)
results <- with(curve_3B6v, data.frame(sites, richness, sd))
results

Blease_3B9_datav <- read.csv('Blease3B9v.csv', header = TRUE)
curve_3B9v <- specaccum(Blease_3B9_datav, "exact")
plot(curve_3B9v, add = TRUE, col = 4)

Bryson_2B9_datav <- read.csv('Bryson2B9v.csv', header = TRUE)
curve_2B9v <- specaccum(Bryson_2B9_datav, "exact")
plot(curve_2B9v, add = TRUE, col = 4)

Burnett_1B_5datav <- read.csv('Burnett1B5v.csv', header = TRUE)
curve_1B5v <- specaccum(Burnett_1B_5datav, "exact")
plot(curve_1B5v, add = TRUE, col = 4)

Creswell_0B_8datav <- read.csv('Creswell0B8v.csv', header = TRUE)
curve_0B8v <- specaccum(Creswell_0B_8datav, "exact")
plot(curve_0B8v, add = TRUE, col = 4)

Creswell_2B_10datav <- read.csv('Creswell2B10v.csv', header = TRUE)
curve_2B10v <- specaccum(Creswell_2B_10datav, "exact")
plot(curve_2B10v, add = TRUE, col = 4)

Gosnell_0B_5Bdatav <- read.csv('Gosnell0B5Bv.csv', header = TRUE)
curve_0B5Bv <- specaccum(Gosnell_0B_5Bdatav, "exact")
plot(curve_0B5Bv, add = TRUE, col = 4)

Hood_Creek_Rd_0B_7datav <- read.csv('HoodCreekRd0B7v.csv', header = TRUE)
curve_0B7v <- specaccum(Hood_Creek_Rd_0B_7datav, "exact")
plot(curve_0B7v, add = TRUE, col = 4)

Hudson_2B_7datav <- read.csv('Hudson2B7v.csv', header = TRUE)
curve_2B7v <- specaccum(Hudson_2B_7datav, "exact")
plot(curve_2B7v, add = TRUE, col = 4)

Kemp_0B_10datav <- read.csv('Kemp0B10v.csv', header = TRUE)
curve_0B10v <- specaccum(Kemp_0B_10datav, "exact")
plot(curve_0B10v, add = TRUE, col = 4)

Kemp_1B_1datav <- read.csv('Kemp1B1v.csv', header = TRUE)
curve_1B1v <- specaccum(Kemp_1B_1datav, "exact")
plot(curve_1B1v, add = TRUE, col = 4)

Kemp_2B_5datav <- read.csv('Kemp2B5v.csv', header = TRUE)
curve_2B5v <- specaccum(Kemp_2B_5datav, "exact")
plot(curve_2B5v, add = TRUE, col = 4)

Kessler_3B_4datav <- read.csv('Kessler3B4v.csv', header = TRUE)
curve_3B4v <- specaccum(Kessler_3B_4datav, "exact")
plot(curve_3B4v, add = TRUE, col = 4)

Kessler_3B_5Bdatav <- read.csv('Kessler3BB5v.csv', header = TRUE)
curve_3B5Bv <- specaccum(Kessler_3B_5Bdatav, "exact")
plot(curve_3B5Bv, add = TRUE, col = 4)

Mathis_1B_6datav <- read.csv('Mathis1B6v.csv', header = TRUE)
curve_1B6v <- specaccum(Mathis_1B_6datav, "exact")
plot(curve_1B6v, add = TRUE, col = 4)

Mills_0B_9datav <- read.csv('Mills0B9v.csv', header = TRUE)
curve_0B9v <- specaccum(Mills_0B_9datav, "exact")
plot(curve_0B9v, add = TRUE, col = 4)

Mills_1B_45datav <- read.csv('Mills1B45v.csv', header = TRUE)
curve_1B45v <- specaccum(Mills_1B_45datav, "exact")
plot(curve_1B45v, add = TRUE, col = 4)

Shealy_0B_2datav <- read.csv('Shealy0B2v.csv', header = TRUE)
curve_0B2v <- specaccum(Shealy_0B_2datav, "exact")
plot(curve_0B2v, add = TRUE, col = 4)

Shealy_0B_4datav <- read.csv('Shealy0B4v.csv', header = TRUE)
curve_0B4v <- specaccum(Shealy_0B_4datav, "exact")
plot(curve_0B4v, add = TRUE, col = 4)

Shealy_1B_3Bdatav <- read.csv('Shealy1B3Bv.csv', header = TRUE)
curve_1B3Bv <- specaccum(Shealy_1B_3Bdatav, "exact")
plot(curve_1B3Bv, add = TRUE, col = 4)

Shealy_1B_E_Sdatav <- read.csv('Shealy1BESv.csv', header = TRUE)
curve_1BESv <- specaccum(Shealy_1B_E_Sdatav, "exact")
plot(curve_1BESv, add = TRUE, col = 4)

Shealy_2B_3datav <- read.csv('Shealy2B3v.csv', header = TRUE)
curve_2B3v <- specaccum(Shealy_2B_3datav, "exact")
plot(curve_2B3v, add = TRUE, col = 4)

Suggs_0B_E_Sdatav <- read.csv('Suggs0BESv.csv', header = TRUE)
curve_0BESv <- specaccum(Suggs_0B_E_Sdatav, "exact")
plot(curve_0BESv, add = TRUE, col = 4)

Swanson_3B_2datav <- read.csv('Swanson3B2v.csv', header = TRUE)
curve_3B2v <- specaccum(Swanson_3B_2datav, "exact")
plot(curve_3B2v, add = TRUE, col = 4)

Timberhaven_2B_1datav <- read.csv('Timberhaven2B1v.csv', header = TRUE)
curve_2B1v <- specaccum(Timberhaven_2B_1datav, "exact")
plot(curve_2B1v, add = TRUE, col = 4)

Turkey_Rd_2B_6Adatav <- read.csv('TurkeyRd2B6Av.csv', header = TRUE)
curve_2B6Av <- specaccum(Turkey_Rd_2B_6Adatav, "exact")
plot(curve_2B6Av, add = TRUE, col = 4)

Abercrombie_Rd_0B_E_ABdatav <- read.csv('Abercrombie0BEABv.csv', header = TRUE)
curve_0BEABv <- specaccum(Abercrombie_Rd_0B_E_ABdatav, "exact")
plot(curve_0BEABv, add = TRUE, col = 4)

Abercrombie_Rd_1B_2datav <- read.csv('Abercrombie1B2v.csv', header = TRUE)
curve_1B2v <- specaccum(Abercrombie_Rd_1B_2datav, "exact")
plot(curve_1B2v, add = TRUE, col = 4)

Honea_Path_0B_1datav <- read.csv('HoneaPath0B1v.csv', header = TRUE)
curve_0B1v <- specaccum(Honea_Path_0B_1datav, "exact")
plot(curve_0B1v, add = TRUE, col = 4)

Honea_Path_3B_1datav <- read.csv('HoneaPath3B1v.csv', header = TRUE)
curve_3B1v <- specaccum(Honea_Path_3B_1datav, "exact")
plot(curve_3B1v, add = TRUE, col = 4)

Honea_Path_3B_3datav <- read.csv('HoneaPath3B3v.csv', header = TRUE)
curve_3B3v <- specaccum(Honea_Path_3B_3datav, "exact")
plot(curve_3B3v, add = TRUE, col = 4)


results <- with(sav2, data.frame(sites, richness, sd))
results


############################################################################
#2017 only - visits
setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")
CSV <- read.csv("17_acdata_dels.csv")
VS <- tapply(CSV$Total, list(CSV$Survey, CSV$Species), sum)
VS                   #check out data - should have species total counts for each day surveyed
VS[is.na(VS)]<-0                                                    #convert NA's to 0
class(VS)            #if "matrix" need to convert to "data.frame" (following 2 lines)
write.table(VS,"VS.csv", sep=',', col.names=TRUE)
VSdata <- read.csv('VS.csv', header = TRUE)
class(VSdata)
sav1 <- specaccum(VSdata)
sav2 <- specaccum(VSdata, "random")
plot(sav1, xaxt="n",ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue",
     ylab="Number of Species", xlab="Audio Survey Visits",
     main="")
axis(1, at=seq(1, 3, by=1) , cex.axis=1)
#boxplot(sav2, col="yellow", add=TRUE, pch="+")

setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/ac_properties/2017 visits")

#Battlewood_1B8_datav <- read.csv('Battlewood1B8v.csv', header = TRUE)
#Battlewood_1B8_datav <- Battlewood_1B8_datav[]
#Battlewood_1B8_datav
#curve_1B8v <- specaccum(Battlewood_1B8_datav, "exact")
#plot(curve_1B8v, xaxt="n",ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue",
#     ylab="Number of Species", xlab="Survey Visits",
#     main="SAC for survey visits sampled across all sites")
#  axis(1, at=seq(1, 4, by=1) , cex.axis=1)
#plot(curve_1B8v, add = TRUE, col = 4)
#results <- with(curve_1B8v, data.frame(sites, richness, sd))
#results

Blease_3B6_datav <- read.csv('Blease3B6v.csv', header = TRUE)
curve_3B6v <- specaccum(Blease_3B6_datav, "exact")
plot(curve_3B6v, add = TRUE, col = 4)
results <- with(curve_3B6v, data.frame(sites, richness, sd))
results

Blease_3B9_datav <- read.csv('Blease3B9v.csv', header = TRUE)
curve_3B9v <- specaccum(Blease_3B9_datav, "exact")
plot(curve_3B9v, add = TRUE, col = 4)
results <- with(curve_3B9v, data.frame(sites, richness, sd))
results

Bryson_2B9_datav <- read.csv('Bryson2B9v.csv', header = TRUE)
curve_2B9v <- specaccum(Bryson_2B9_datav, "exact")
plot(curve_2B9v, add = TRUE, col = 4)
results <- with(curve_2B9v, data.frame(sites, richness, sd))
results

Burnett_1B_5datav <- read.csv('Burnett1B5v.csv', header = TRUE)
curve_1B5v <- specaccum(Burnett_1B_5datav, "exact")
plot(curve_1B5v, add = TRUE, col = 4)
results <- with(curve_1B5v, data.frame(sites, richness, sd))
results

Creswell_0B_8datav <- read.csv('Creswell0B8v.csv', header = TRUE)
curve_0B8v <- specaccum(Creswell_0B_8datav, "exact")
plot(curve_0B8v, add = TRUE, col = 4)
results <- with(curve_0B8v, data.frame(sites, richness, sd))
results

Creswell_2B_10datav <- read.csv('Creswell2B10v.csv', header = TRUE)
curve_2B10v <- specaccum(Creswell_2B_10datav, "exact")
plot(curve_2B10v, add = TRUE, col = 4)
results <- with(curve_2B10v, data.frame(sites, richness, sd))
results

#Gosnell_0B_5Bdatav <- read.csv('Gosnell0B5Bv.csv', header = TRUE)
#curve_0B5Bv <- specaccum(Gosnell_0B_5Bdatav, "exact")
#plot(curve_0B5Bv, add = TRUE, col = 4)
#results <- with(curve_0B5Bv, data.frame(sites, richness, sd))
#results

#Hood_Creek_Rd_0B_7datav <- read.csv('HoodCreekRd0B7v.csv', header = TRUE)
#curve_0B7v <- specaccum(Hood_Creek_Rd_0B_7datav, "exact")
#plot(curve_0B7v, add = TRUE, col = 4)
#results <- with(curve_0B7v, data.frame(sites, richness, sd))
#results

Hudson_2B_7datav <- read.csv('Hudson2B7v.csv', header = TRUE)
curve_2B7v <- specaccum(Hudson_2B_7datav, "exact")
plot(curve_2B7v, add = TRUE, col = 4)
results <- with(curve_2B7v, data.frame(sites, richness, sd))
results

Kemp_0B_10datav <- read.csv('Kemp0B10v.csv', header = TRUE)
curve_0B10v <- specaccum(Kemp_0B_10datav, "exact")
plot(curve_0B10v, add = TRUE, col = 4)
results <- with(curve_0B10v, data.frame(sites, richness, sd))
results

Kemp_1B_1datav <- read.csv('Kemp1B1v.csv', header = TRUE)
curve_1B1v <- specaccum(Kemp_1B_1datav, "exact")
plot(curve_1B1v, add = TRUE, col = 4)
results <- with(curve_1B1v, data.frame(sites, richness, sd))
results

Kemp_2B_5datav <- read.csv('Kemp2B5v.csv', header = TRUE)
curve_2B5v <- specaccum(Kemp_2B_5datav, "exact")
plot(curve_2B5v, add = TRUE, col = 4)
results <- with(curve_2B5v, data.frame(sites, richness, sd))
results

Kessler_3B_4datav <- read.csv('Kessler3B4v.csv', header = TRUE)
curve_3B4v <- specaccum(Kessler_3B_4datav, "exact")
plot(curve_3B4v, add = TRUE, col = 4)
results <- with(curve_3B4v, data.frame(sites, richness, sd))
results

Kessler_3B_5Bdatav <- read.csv('Kessler3B5Bv.csv', header = TRUE)
curve_3B5Bv <- specaccum(Kessler_3B_5Bdatav, "exact")
plot(curve_3B5Bv, add = TRUE, col = 4)
results <- with(curve_3B5Bv, data.frame(sites, richness, sd))
results

Mathis_1B_6datav <- read.csv('Mathis1B6v.csv', header = TRUE)
curve_1B6v <- specaccum(Mathis_1B_6datav, "exact")
plot(curve_1B6v, add = TRUE, col = 4)
results <- with(curve_3B6v, data.frame(sites, richness, sd))
results

Mills_0B_9datav <- read.csv('Mills0B9v.csv', header = TRUE)
curve_0B9v <- specaccum(Mills_0B_9datav, "exact")
plot(curve_0B9v, add = TRUE, col = 4)
results <- with(curve_0B9v, data.frame(sites, richness, sd))
results

Mills_1B_45datav <- read.csv('Mills1B45v.csv', header = TRUE)
curve_1B45v <- specaccum(Mills_1B_45datav, "exact")
plot(curve_1B45v, add = TRUE, col = 4)
results <- with(curve_1B45v, data.frame(sites, richness, sd))
results

Shealy_0B_2datav <- read.csv('Shealy0B2v.csv', header = TRUE)
curve_0B2v <- specaccum(Shealy_0B_2datav, "exact")
plot(curve_0B2v, add = TRUE, col = 4)
results <- with(curve_0B2v, data.frame(sites, richness, sd))
results

Shealy_0B_4datav <- read.csv('Shealy0B4v.csv', header = TRUE)
curve_0B4v <- specaccum(Shealy_0B_4datav, "exact")
plot(curve_0B4v, add = TRUE, col = 4)
results <- with(curve_0B4v, data.frame(sites, richness, sd))
results

Shealy_1B_3Bdatav <- read.csv('Shealy1B3Bv.csv', header = TRUE)
curve_1B3Bv <- specaccum(Shealy_1B_3Bdatav, "exact")
plot(curve_1B3Bv, add = TRUE, col = 4)
results <- with(curve_1B3Bv, data.frame(sites, richness, sd))
results

Shealy_1B_E_Sdatav <- read.csv('Shealy1BESv.csv', header = TRUE)
curve_1BESv <- specaccum(Shealy_1B_E_Sdatav, "exact")
plot(curve_1BESv, add = TRUE, col = 4)
results <- with(curve_1BESv, data.frame(sites, richness, sd))
results

Shealy_2B_3datav <- read.csv('Shealy2B3v.csv', header = TRUE)
curve_2B3v <- specaccum(Shealy_2B_3datav, "exact")
plot(curve_2B3v, add = TRUE, col = 4)
results <- with(curve_2B3v, data.frame(sites, richness, sd))
results

Suggs_0B_E_Sdatav <- read.csv('Suggs0BESv.csv', header = TRUE)
curve_0BESv <- specaccum(Suggs_0B_E_Sdatav, "exact")
plot(curve_0BESv, add = TRUE, col = 4)
results <- with(curve_0BESv, data.frame(sites, richness, sd))
results

Swanson_3B_2datav <- read.csv('Swanson3B2v.csv', header = TRUE)
curve_3B2v <- specaccum(Swanson_3B_2datav, "exact")
plot(curve_3B2v, add = TRUE, col = 4)
results <- with(curve_3B2v, data.frame(sites, richness, sd))
results

Timberhaven_2B_1datav <- read.csv('Timberhaven2B1v.csv', header = TRUE)
curve_2B1v <- specaccum(Timberhaven_2B_1datav, "exact")
plot(curve_2B1v, add = TRUE, col = 4)
results <- with(curve_2B1v, data.frame(sites, richness, sd))
results

Turkey_Rd_2B_6Adatav <- read.csv('TurkeyRd2B6Av.csv', header = TRUE)
curve_2B6Av <- specaccum(Turkey_Rd_2B_6Adatav, "exact")
plot(curve_2B6Av, add = TRUE, col = 4)
results <- with(curve_2B6Av, data.frame(sites, richness, sd))
results

Abercrombie_Rd_0B_E_ABdatav <- read.csv('Abercrombie0BEABv.csv', header = TRUE)
curve_0BEABv <- specaccum(Abercrombie_Rd_0B_E_ABdatav, "exact")
plot(curve_0BEABv, add = TRUE, col = 4)
results <- with(curve_0BEABv, data.frame(sites, richness, sd))
results

Abercrombie_Rd_1B_2datav <- read.csv('Abercrombie1B2v.csv', header = TRUE)
curve_1B2v <- specaccum(Abercrombie_Rd_1B_2datav, "exact")
plot(curve_1B2v, add = TRUE, col = 4)
results <- with(curve_1B2v, data.frame(sites, richness, sd))
results

Honea_Path_0B_1datav <- read.csv('HoneaPath0B1v.csv', header = TRUE)
curve_0B1v <- specaccum(Honea_Path_0B_1datav, "exact")
plot(curve_0B1v, add = TRUE, col = 4)
results <- with(curve_0B1v, data.frame(sites, richness, sd))
results

Honea_Path_3B_1datav <- read.csv('HoneaPath3B1v.csv', header = TRUE)
curve_3B1v <- specaccum(Honea_Path_3B_1datav, "exact")
plot(curve_3B1v, add = TRUE, col = 4)
results <- with(curve_3B1v, data.frame(sites, richness, sd))
results

Honea_Path_3B_3datav <- read.csv('HoneaPath3B3v.csv', header = TRUE)
curve_3B3v <- specaccum(Honea_Path_3B_3datav, "exact")
plot(curve_3B3v, add = TRUE, col = 4)
results <- with(curve_3B3v, data.frame(sites, richness, sd))
results


#results <- with(sav2, data.frame(sites, richness, sd))
#results


#################################################################
# 2018 visits only
setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")
CSV <- read.csv("18_acdata_dels.csv")
VS <- tapply(CSV$Total, list(CSV$Survey, CSV$Species), sum)
VS                   #check out data - should have species total counts for each day surveyed
VS[is.na(VS)]<-0                                                    #convert NA's to 0
class(VS)            #if "matrix" need to convert to "data.frame" (following 2 lines)
write.table(VS,"VS.csv", sep=',', col.names=TRUE)
VSdata <- read.csv('VS.csv', header = TRUE)
class(VSdata)
sav1 <- specaccum(VSdata)
sav2 <- specaccum(VSdata, "random")
plot(sav1, xaxt="n",ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue",
     ylab="Number of Species", xlab="Audio Survey Visits",
     main="")
axis(1, at=seq(1, 3, by=1) , cex.axis=1)
#boxplot(sav2, col="yellow", add=TRUE, pch="+")

setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/ac_properties/2018 visits")

Battlewood_1B8_datav <- read.csv('Battlewood1B8v.csv', header = TRUE)
#Battlewood_1B8_datav <- Battlewood_1B8_datav[]
#Battlewood_1B8_datav
curve_1B8v <- specaccum(Battlewood_1B8_datav, "exact")
#plot(curve_1B8v, xaxt="n",ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue",
#     ylab="Number of Species", xlab="Survey Visits",
#     main="SAC for survey visits sampled across all sites")
#  axis(1, at=seq(1, 4, by=1) , cex.axis=1)
plot(curve_1B8v, add = TRUE, col = 4)
results <- with(curve_1B8v, data.frame(sites, richness, sd))
results

Blease_3B6_datav <- read.csv('Blease3B6v.csv', header = TRUE)
curve_3B6v <- specaccum(Blease_3B6_datav, "exact")
plot(curve_3B6v, add = TRUE, col = 4)
results <- with(curve_3B6v, data.frame(sites, richness, sd))
results

Blease_3B9_datav <- read.csv('Blease3B9v.csv', header = TRUE)
curve_3B9v <- specaccum(Blease_3B9_datav, "exact")
plot(curve_3B9v, add = TRUE, col = 4)
results <- with(curve_3B9v, data.frame(sites, richness, sd))
results

Bryson_2B9_datav <- read.csv('Bryson2B9v.csv', header = TRUE)
curve_2B9v <- specaccum(Bryson_2B9_datav, "exact")
plot(curve_2B9v, add = TRUE, col = 4)
results <- with(curve_2B9v, data.frame(sites, richness, sd))
results

Burnett_1B_5datav <- read.csv('Burnett1B5v.csv', header = TRUE)
curve_1B5v <- specaccum(Burnett_1B_5datav, "exact")
plot(curve_1B5v, add = TRUE, col = 4)
results <- with(curve_1B5v, data.frame(sites, richness, sd))
results

Creswell_0B_8datav <- read.csv('Creswell0B8v.csv', header = TRUE)
curve_0B8v <- specaccum(Creswell_0B_8datav, "exact")
plot(curve_0B8v, add = TRUE, col = 4)
results <- with(curve_0B8v, data.frame(sites, richness, sd))
results

Creswell_2B_10datav <- read.csv('Creswell2B10v.csv', header = TRUE)
curve_2B10v <- specaccum(Creswell_2B_10datav, "exact")
plot(curve_2B10v, add = TRUE, col = 4)
results <- with(curve_2B10v, data.frame(sites, richness, sd))
results

Gosnell_0B_5Bdatav <- read.csv('Gosnell0B5Bv.csv', header = TRUE)
curve_0B5Bv <- specaccum(Gosnell_0B_5Bdatav, "exact")
plot(curve_0B5Bv, add = TRUE, col = 4)
results <- with(curve_0B5Bv, data.frame(sites, richness, sd))
results

Hood_Creek_Rd_0B_7datav <- read.csv('HoodCreekRd0B7v.csv', header = TRUE)
curve_0B7v <- specaccum(Hood_Creek_Rd_0B_7datav, "exact")
plot(curve_0B7v, add = TRUE, col = 4)
results <- with(curve_0B7v, data.frame(sites, richness, sd))
results

Hudson_2B_7datav <- read.csv('Hudson2B7v.csv', header = TRUE)
curve_2B7v <- specaccum(Hudson_2B_7datav, "exact")
plot(curve_2B7v, add = TRUE, col = 4)
results <- with(curve_2B7v, data.frame(sites, richness, sd))
results

#Kemp_0B_10datav <- read.csv('Kemp0B10v.csv', header = TRUE)
#curve_0B10v <- specaccum(Kemp_0B_10datav, "exact")
#plot(curve_0B10v, add = TRUE, col = 4)
#results <- with(curve_0B10v, data.frame(sites, richness, sd))
#results

Kemp_1B_1datav <- read.csv('Kemp1B1v.csv', header = TRUE)
curve_1B1v <- specaccum(Kemp_1B_1datav, "exact")
plot(curve_1B1v, add = TRUE, col = 4)
results <- with(curve_1B1v, data.frame(sites, richness, sd))
results

Kemp_2B_5datav <- read.csv('Kemp2B5v.csv', header = TRUE)
curve_2B5v <- specaccum(Kemp_2B_5datav, "exact")
plot(curve_2B5v, add = TRUE, col = 4)
results <- with(curve_2B5v, data.frame(sites, richness, sd))
results

Kessler_3B_4datav <- read.csv('Kessler3B4v.csv', header = TRUE)
curve_3B4v <- specaccum(Kessler_3B_4datav, "exact")
plot(curve_3B4v, add = TRUE, col = 4)
results <- with(curve_3B4v, data.frame(sites, richness, sd))
results

Kessler_3B_5Bdatav <- read.csv('Kessler3B5Bv.csv', header = TRUE)
curve_3B5Bv <- specaccum(Kessler_3B_5Bdatav, "exact")
plot(curve_3B5Bv, add = TRUE, col = 4)
results <- with(curve_3B5Bv, data.frame(sites, richness, sd))
results

Mathis_1B_6datav <- read.csv('Mathis1B6v.csv', header = TRUE)
curve_1B6v <- specaccum(Mathis_1B_6datav, "exact")
plot(curve_1B6v, add = TRUE, col = 4)
results <- with(curve_3B6v, data.frame(sites, richness, sd))
results

Mills_0B_9datav <- read.csv('Mills0B9v.csv', header = TRUE)
curve_0B9v <- specaccum(Mills_0B_9datav, "exact")
plot(curve_0B9v, add = TRUE, col = 4)
results <- with(curve_0B9v, data.frame(sites, richness, sd))
results

Mills_1B_45datav <- read.csv('Mills1B45v.csv', header = TRUE)
curve_1B45v <- specaccum(Mills_1B_45datav, "exact")
plot(curve_1B45v, add = TRUE, col = 4)
results <- with(curve_1B45v, data.frame(sites, richness, sd))
results

Shealy_0B_2datav <- read.csv('Shealy0B2v.csv', header = TRUE)
curve_0B2v <- specaccum(Shealy_0B_2datav, "exact")
plot(curve_0B2v, add = TRUE, col = 4)
results <- with(curve_0B2v, data.frame(sites, richness, sd))
results

Shealy_0B_4datav <- read.csv('Shealy0B4v.csv', header = TRUE)
curve_0B4v <- specaccum(Shealy_0B_4datav, "exact")
plot(curve_0B4v, add = TRUE, col = 4)
results <- with(curve_0B4v, data.frame(sites, richness, sd))
results

Shealy_1B_3Bdatav <- read.csv('Shealy1B3Bv.csv', header = TRUE)
curve_1B3Bv <- specaccum(Shealy_1B_3Bdatav, "exact")
plot(curve_1B3Bv, add = TRUE, col = 4)
results <- with(curve_1B3Bv, data.frame(sites, richness, sd))
results

Shealy_1B_E_Sdatav <- read.csv('Shealy1BESv.csv', header = TRUE)
curve_1BESv <- specaccum(Shealy_1B_E_Sdatav, "exact")
plot(curve_1BESv, add = TRUE, col = 4)
results <- with(curve_1BESv, data.frame(sites, richness, sd))
results

Shealy_2B_3datav <- read.csv('Shealy2B3v.csv', header = TRUE)
curve_2B3v <- specaccum(Shealy_2B_3datav, "exact")
plot(curve_2B3v, add = TRUE, col = 4)
results <- with(curve_2B3v, data.frame(sites, richness, sd))
results

Suggs_0B_E_Sdatav <- read.csv('Suggs0BESv.csv', header = TRUE)
curve_0BESv <- specaccum(Suggs_0B_E_Sdatav, "exact")
plot(curve_0BESv, add = TRUE, col = 4)
results <- with(curve_0BESv, data.frame(sites, richness, sd))
results

Swanson_3B_2datav <- read.csv('Swanson3B2v.csv', header = TRUE)
curve_3B2v <- specaccum(Swanson_3B_2datav, "exact")
plot(curve_3B2v, add = TRUE, col = 4)
results <- with(curve_3B2v, data.frame(sites, richness, sd))
results

Timberhaven_2B_1datav <- read.csv('Timberhaven2B1v.csv', header = TRUE)
curve_2B1v <- specaccum(Timberhaven_2B_1datav, "exact")
plot(curve_2B1v, add = TRUE, col = 4)
results <- with(curve_2B1v, data.frame(sites, richness, sd))
results

#Turkey_Rd_2B_6Adatav <- read.csv('TurkeyRd2B6Av.csv', header = TRUE)
#curve_2B6Av <- specaccum(Turkey_Rd_2B_6Adatav, "exact")
#plot(curve_2B6Av, add = TRUE, col = 4)
#results <- with(curve_2B6Av, data.frame(sites, richness, sd))
#results

Abercrombie_Rd_0B_E_ABdatav <- read.csv('Abercrombie0BEABv.csv', header = TRUE)
curve_0BEABv <- specaccum(Abercrombie_Rd_0B_E_ABdatav, "exact")
plot(curve_0BEABv, add = TRUE, col = 4)
results <- with(curve_0BEABv, data.frame(sites, richness, sd))
results

Abercrombie_Rd_1B_2datav <- read.csv('Abercrombie1B2v.csv', header = TRUE)
curve_1B2v <- specaccum(Abercrombie_Rd_1B_2datav, "exact")
plot(curve_1B2v, add = TRUE, col = 4)
results <- with(curve_1B2v, data.frame(sites, richness, sd))
results

Honea_Path_0B_1datav <- read.csv('HoneaPath0B1v.csv', header = TRUE)
curve_0B1v <- specaccum(Honea_Path_0B_1datav, "exact")
plot(curve_0B1v, add = TRUE, col = 4)
results <- with(curve_0B1v, data.frame(sites, richness, sd))
results

Honea_Path_3B_1datav <- read.csv('HoneaPath3B1v.csv', header = TRUE)
curve_3B1v <- specaccum(Honea_Path_3B_1datav, "exact")
plot(curve_3B1v, add = TRUE, col = 4)
results <- with(curve_3B1v, data.frame(sites, richness, sd))
results

Honea_Path_3B_3datav <- read.csv('HoneaPath3B3v.csv', header = TRUE)
curve_3B3v <- specaccum(Honea_Path_3B_3datav, "exact")
plot(curve_3B3v, add = TRUE, col = 4)
results <- with(curve_3B3v, data.frame(sites, richness, sd))
results


#results <- with(sav2, data.frame(sites, richness, sd))
#results

##################################################################
##################################################################
##################################################################
# TIME 2017 only

setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")
CSV <- read.csv("17_acdata_dels.csv")
DS <- tapply(CSV$Total, list(CSV$Interval, CSV$Species), sum)
#DS                   #check out data - should have species total counts for each time interval surveyed
DS[is.na(DS)]<-0                                                    #convert NA's to 0
class(DS)            #if "matrix" need to convert to "data.frame" (following 2 lines)
write.table(DS,"DS.csv", sep=',', col.names=TRUE)
DSdata <- read.csv('DS.csv', header = TRUE)
class(DSdata)
sat1 <- specaccum(DSdata)
sat2 <- specaccum(DSdata, "random")
#draw the plot for an overall frame
plot(sat1, xaxt="n",ci.type="poly", col="purple", lwd=2, ci.lty=0, ci.col="thistle",
     ylab="Number of Species", xlab="Time Intervals in Audio Surveys",
     main="")
axis(1, at=seq(1, 4, by=1) , cex.axis=1)
#boxplot(sat2, col="yellow", add=TRUE, pch="+")

setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/ac_properties/2017 time")

cc <- palette()
palette(c(cc,"purple","brown"))
palette()

#Battlewood_1B_8data <- read.csv('Battlewood_1B_8.csv', header = TRUE)
#curve_1B8 <- specaccum(Battlewood_1B_8data, "exact")
#plot(curve_1B8, xaxt="n",ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue",
#     ylab="Number of Species", xlab="Survey Intervals",
#     main="SAC for time intervals sampled across all sites")
#  axis(1, at=seq(1, 4, by=1) , cex.axis=1)
#plot(curve_1B8, add = TRUE, col = "purple")
#results <- with(curve_1B8, data.frame(sites, richness, sd))
#results

Blease_3B_6data <- read.csv('Blease_3B_6.csv', header = TRUE)
curve_3B6 <- specaccum(Blease_3B_6data, "exact")
plot(curve_3B6, add = TRUE, col = "purple")
results <- with(curve_3B6, data.frame(sites, richness, sd))
results

Blease_3B_9data <- read.csv('Blease_3B_9.csv', header = TRUE)
curve_3B9 <- specaccum(Blease_3B_9data, "exact")
plot(curve_3B9, add = TRUE, col = "purple")
results <- with(curve_3B9, data.frame(sites, richness, sd))
results

Bryson_2B_9data <- read.csv('Bryson_2B_9.csv', header = TRUE)
curve_2B9 <- specaccum(Bryson_2B_9data, "exact")
plot(curve_2B9, add = TRUE, col = "purple")
results <- with(curve_2B9, data.frame(sites, richness, sd))
results

Burnett_1B_5data <- read.csv('Burnett_1B_5.csv', header = TRUE)
curve_1B5 <- specaccum(Burnett_1B_5data, "exact")
plot(curve_1B5, add = TRUE, col = "purple")
results <- with(curve_1B5, data.frame(sites, richness, sd))
results

Creswell_0B_8data <- read.csv('Creswell_0B_8.csv', header = TRUE)
curve_0B8 <- specaccum(Creswell_0B_8data, "exact")
plot(curve_0B8, add = TRUE, col = "purple")
results <- with(curve_0B8, data.frame(sites, richness, sd))
results

Creswell_2B_10data <- read.csv('Creswell_2B_10.csv', header = TRUE)
curve_2B10 <- specaccum(Creswell_2B_10data, "exact")
plot(curve_2B10, add = TRUE, col = "purple")
results <- with(curve_2B10, data.frame(sites, richness, sd))
results

#Gosnell_0B_5Bdata <- read.csv('Gosnell_0B_5B.csv', header = TRUE)
#curve_0B5B <- specaccum(Gosnell_0B_5Bdata, "exact")
#plot(curve_0B5B, add = TRUE, col = "purple")
#results <- with(curve_0B5B, data.frame(sites, richness, sd))
#results

#Hood_Creek_Rd_0B_7data <- read.csv('Hood_Creek_Rd_0B_7.csv', header = TRUE)
#curve_0B7 <- specaccum(Hood_Creek_Rd_0B_7data, "exact")
#plot(curve_0B7, add = TRUE, col = "purple")
#results <- with(curve_0B7, data.frame(sites, richness, sd))
#results

Hudson_2B_7data <- read.csv('Hudson_2B_7.csv', header = TRUE)
curve_2B7 <- specaccum(Hudson_2B_7data, "exact")
plot(curve_2B7, add = TRUE, col = "purple")
results <- with(curve_2B7, data.frame(sites, richness, sd))
results

Kemp_0B_10data <- read.csv('Kemp_0B_10.csv', header = TRUE)
curve_0B10 <- specaccum(Kemp_0B_10data, "exact")
plot(curve_0B10, add = TRUE, col = "purple")
results <- with(curve_0B10, data.frame(sites, richness, sd))
results

Kemp_1B_1data <- read.csv('Kemp_1B_1.csv', header = TRUE)
curve_1B1 <- specaccum(Kemp_1B_1data, "exact")
plot(curve_1B1, add = TRUE, col = "purple")
results <- with(curve_1B1, data.frame(sites, richness, sd))
results

Kemp_2B_5data <- read.csv('Kemp_2B_5.csv', header = TRUE)
curve_2B5 <- specaccum(Kemp_2B_5data, "exact")
plot(curve_2B5, add = TRUE, col = "purple")
results <- with(curve_2B5, data.frame(sites, richness, sd))
results

Kessler_3B_4data <- read.csv('Kessler_3B_4.csv', header = TRUE)
curve_3B4 <- specaccum(Kessler_3B_4data, "exact")
plot(curve_3B4, add = TRUE, col = "purple")
results <- with(curve_3B4, data.frame(sites, richness, sd))
results

Kessler_3B_5Bdata <- read.csv('Kessler_3B_5B.csv', header = TRUE)
curve_3B5B <- specaccum(Kessler_3B_5Bdata, "exact")
plot(curve_3B5B, add = TRUE, col = "purple")
results <- with(curve_3B5B, data.frame(sites, richness, sd))
results

Mathis_1B_6data <- read.csv('Mathis_1B_6.csv', header = TRUE)
curve_1B6 <- specaccum(Mathis_1B_6data, "exact")
plot(curve_1B6, add = TRUE, col = "purple")
results <- with(curve_1B6, data.frame(sites, richness, sd))
results

Mills_0B_9data <- read.csv('Mills_0B_9.csv', header = TRUE)
curve_0B9 <- specaccum(Mills_0B_9data, "exact")
plot(curve_0B9, add = TRUE, col = "purple")
results <- with(curve_0B9, data.frame(sites, richness, sd))
results

Mills_1B_45data <- read.csv('Mills_1B_45.csv', header = TRUE)
curve_1B45 <- specaccum(Mills_1B_45data, "exact")
plot(curve_1B45, add = TRUE, col = "purple")
results <- with(curve_1B45, data.frame(sites, richness, sd))
results

Shealy_0B_2data <- read.csv('Shealy_0B_2.csv', header = TRUE)
curve_0B2 <- specaccum(Shealy_0B_2data, "exact")
plot(curve_0B2, add = TRUE, col = "purple")
results <- with(curve_0B2, data.frame(sites, richness, sd))
results

Shealy_0B_4data <- read.csv('Shealy_0B_4.csv', header = TRUE)
curve_0B4 <- specaccum(Shealy_0B_4data, "exact")
plot(curve_0B4, add = TRUE, col = "purple")
results <- with(curve_0B4, data.frame(sites, richness, sd))
results

Shealy_1B_3Bdata <- read.csv('Shealy_1B_3B.csv', header = TRUE)
curve_1B3B <- specaccum(Shealy_1B_3Bdata, "exact")
plot(curve_1B3B, add = TRUE, col = "purple")
results <- with(curve_1B3B, data.frame(sites, richness, sd))
results

Shealy_1B_E_Sdata <- read.csv('Shealy_1B_E_S.csv', header = TRUE)
curve_1BES <- specaccum(Shealy_1B_E_Sdata, "exact")
plot(curve_1BES, add = TRUE, col = "purple")
results <- with(curve_1BES, data.frame(sites, richness, sd))
results

Shealy_2B_3data <- read.csv('Shealy_2B_3.csv', header = TRUE)
curve_2B3 <- specaccum(Shealy_2B_3data, "exact")
plot(curve_2B3, add = TRUE, col = "purple")
results <- with(curve_2B3, data.frame(sites, richness, sd))
results

Suggs_0B_E_Sdata <- read.csv('Suggs_0B_E_S.csv', header = TRUE)
curve_0BES <- specaccum(Suggs_0B_E_Sdata, "exact")
plot(curve_0BES, add = TRUE, col = "purple")
results <- with(curve_0BES, data.frame(sites, richness, sd))
results

Swanson_3B_2data <- read.csv('Swanson_3B_2.csv', header = TRUE)
curve_3B2 <- specaccum(Swanson_3B_2data, "exact")
plot(curve_3B2, add = TRUE, col = "purple")
results <- with(curve_3B2, data.frame(sites, richness, sd))
results

Timberhaven_2B_1data <- read.csv('Timberhaven_2B_1.csv', header = TRUE)
curve_2B1 <- specaccum(Timberhaven_2B_1data, "exact")
plot(curve_2B1, add = TRUE, col = "purple")
results <- with(curve_2B1, data.frame(sites, richness, sd))
results

Turkey_Rd_2B_6Adata <- read.csv('Turkey_Rd_2B_6A.csv', header = TRUE)
curve_2B6A <- specaccum(Turkey_Rd_2B_6Adata, "exact")
plot(curve_2B6A, add = TRUE, col = "purple")
results <- with(curve_2B6A, data.frame(sites, richness, sd))
results

Abercrombie_Rd_0B_E_ABdata <- read.csv('Turner_Abercrombie_Rd_0B_E_AB.csv', header = TRUE)
curve_0BEAB <- specaccum(Abercrombie_Rd_0B_E_ABdata, "exact")
plot(curve_0BEAB, add = TRUE, col = "purple")
results <- with(curve_0BEAB, data.frame(sites, richness, sd))
results

Abercrombie_Rd_1B_2data <- read.csv('Turner_Abercrombie_Rd_1B_2.csv', header = TRUE)
curve_1B2 <- specaccum(Abercrombie_Rd_1B_2data, "exact")
plot(curve_1B2, add = TRUE, col = "purple")
results <- with(curve_1B2, data.frame(sites, richness, sd))
results

Honea_Path_0B_1data <- read.csv('Turner_Honea_Path_0B_1.csv', header = TRUE)
curve_0B1 <- specaccum(Honea_Path_0B_1data, "exact")
plot(curve_0B1, add = TRUE, col = "purple")
results <- with(curve_0B1, data.frame(sites, richness, sd))
results

Honea_Path_3B_1data <- read.csv('Turner_Honea_Path_3B_1.csv', header = TRUE)
curve_3B1 <- specaccum(Honea_Path_3B_1data, "exact")
plot(curve_3B1, add = TRUE, col = "purple")
results <- with(curve_3B1, data.frame(sites, richness, sd))
results

Honea_Path_3B_3data <- read.csv('Turner_Honea_Path_3B_3.csv', header = TRUE)
curve_3B3 <- specaccum(Honea_Path_3B_3data, "exact")
plot(curve_3B3, add = TRUE, col = "purple")
results <- with(curve_3B3, data.frame(sites, richness, sd))
results


##################################################################
# TIME 2018 only

setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")
CSV <- read.csv("18_acdata_dels.csv")
DS <- tapply(CSV$Total, list(CSV$Interval, CSV$Species), sum)
#DS                   #check out data - should have species total counts for each time interval surveyed
DS[is.na(DS)]<-0                                                    #convert NA's to 0
class(DS)            #if "matrix" need to convert to "data.frame" (following 2 lines)
write.table(DS,"DS.csv", sep=',', col.names=TRUE)
DSdata <- read.csv('DS.csv', header = TRUE)
class(DSdata)
sat1 <- specaccum(DSdata)
sat2 <- specaccum(DSdata, "random")
#draw the plot for an overall frame
plot(sat1, xaxt="n",ci.type="poly", col="purple", lwd=2, ci.lty=0, ci.col="thistle",
     ylab="Number of Species", xlab="Time Intervals in Audio Surveys",
     main="")
axis(1, at=seq(1, 4, by=1) , cex.axis=1)
#boxplot(sat2, col="yellow", add=TRUE, pch="+")

setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/ac_properties/2018 time")

cc <- palette()
palette(c(cc,"purple","brown"))
palette()

Battlewood_1B_8data <- read.csv('Battlewood_1B_8.csv', header = TRUE)
curve_1B8 <- specaccum(Battlewood_1B_8data, "exact")
#plot(curve_1B8, xaxt="n",ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue",
#     ylab="Number of Species", xlab="Survey Intervals",
#     main="SAC for time intervals sampled across all sites")
#  axis(1, at=seq(1, 4, by=1) , cex.axis=1)
plot(curve_1B8, add = TRUE, col = "purple")
results <- with(curve_1B8, data.frame(sites, richness, sd))
results

Blease_3B_6data <- read.csv('Blease_3B_6.csv', header = TRUE)
curve_3B6 <- specaccum(Blease_3B_6data, "exact")
plot(curve_3B6, add = TRUE, col = "purple")
results <- with(curve_3B6, data.frame(sites, richness, sd))
results

Blease_3B_9data <- read.csv('Blease_3B_9.csv', header = TRUE)
curve_3B9 <- specaccum(Blease_3B_9data, "exact")
plot(curve_3B9, add = TRUE, col = "purple")
results <- with(curve_3B9, data.frame(sites, richness, sd))
results

Bryson_2B_9data <- read.csv('Bryson_2B_9.csv', header = TRUE)
curve_2B9 <- specaccum(Bryson_2B_9data, "exact")
plot(curve_2B9, add = TRUE, col = "purple")
results <- with(curve_2B9, data.frame(sites, richness, sd))
results

Burnett_1B_5data <- read.csv('Burnett_1B_5.csv', header = TRUE)
curve_1B5 <- specaccum(Burnett_1B_5data, "exact")
plot(curve_1B5, add = TRUE, col = "purple")
results <- with(curve_1B5, data.frame(sites, richness, sd))
results

Creswell_0B_8data <- read.csv('Creswell_0B_8.csv', header = TRUE)
curve_0B8 <- specaccum(Creswell_0B_8data, "exact")
plot(curve_0B8, add = TRUE, col = "purple")
results <- with(curve_0B8, data.frame(sites, richness, sd))
results

Creswell_2B_10data <- read.csv('Creswell_2B_10.csv', header = TRUE)
curve_2B10 <- specaccum(Creswell_2B_10data, "exact")
plot(curve_2B10, add = TRUE, col = "purple")
results <- with(curve_2B10, data.frame(sites, richness, sd))
results

Gosnell_0B_5Bdata <- read.csv('Gosnell_0B_5B.csv', header = TRUE)
curve_0B5B <- specaccum(Gosnell_0B_5Bdata, "exact")
plot(curve_0B5B, add = TRUE, col = "purple")
results <- with(curve_0B5B, data.frame(sites, richness, sd))
results

Hood_Creek_Rd_0B_7data <- read.csv('Hood_Creek_Rd_0B_7.csv', header = TRUE)
curve_0B7 <- specaccum(Hood_Creek_Rd_0B_7data, "exact")
plot(curve_0B7, add = TRUE, col = "purple")
results <- with(curve_0B7, data.frame(sites, richness, sd))
results

Hudson_2B_7data <- read.csv('Hudson_2B_7.csv', header = TRUE)
curve_2B7 <- specaccum(Hudson_2B_7data, "exact")
plot(curve_2B7, add = TRUE, col = "purple")
results <- with(curve_2B7, data.frame(sites, richness, sd))
results

#Kemp_0B_10data <- read.csv('Kemp_0B_10.csv', header = TRUE)
#curve_0B10 <- specaccum(Kemp_0B_10data, "exact")
#plot(curve_0B10, add = TRUE, col = "purple")
#results <- with(curve_0B10, data.frame(sites, richness, sd))
#results

Kemp_1B_1data <- read.csv('Kemp_1B_1.csv', header = TRUE)
curve_1B1 <- specaccum(Kemp_1B_1data, "exact")
plot(curve_1B1, add = TRUE, col = "purple")
results <- with(curve_1B1, data.frame(sites, richness, sd))
results

Kemp_2B_5data <- read.csv('Kemp_2B_5.csv', header = TRUE)
curve_2B5 <- specaccum(Kemp_2B_5data, "exact")
plot(curve_2B5, add = TRUE, col = "purple")
results <- with(curve_2B5, data.frame(sites, richness, sd))
results

Kessler_3B_4data <- read.csv('Kessler_3B_4.csv', header = TRUE)
curve_3B4 <- specaccum(Kessler_3B_4data, "exact")
plot(curve_3B4, add = TRUE, col = "purple")
results <- with(curve_3B4, data.frame(sites, richness, sd))
results

Kessler_3B_5Bdata <- read.csv('Kessler_3B_5B.csv', header = TRUE)
curve_3B5B <- specaccum(Kessler_3B_5Bdata, "exact")
plot(curve_3B5B, add = TRUE, col = "purple")
results <- with(curve_3B5B, data.frame(sites, richness, sd))
results

Mathis_1B_6data <- read.csv('Mathis_1B_6.csv', header = TRUE)
curve_1B6 <- specaccum(Mathis_1B_6data, "exact")
plot(curve_1B6, add = TRUE, col = "purple")
results <- with(curve_1B6, data.frame(sites, richness, sd))
results

Mills_0B_9data <- read.csv('Mills_0B_9.csv', header = TRUE)
curve_0B9 <- specaccum(Mills_0B_9data, "exact")
plot(curve_0B9, add = TRUE, col = "purple")
results <- with(curve_0B9, data.frame(sites, richness, sd))
results

Mills_1B_45data <- read.csv('Mills_1B_45.csv', header = TRUE)
curve_1B45 <- specaccum(Mills_1B_45data, "exact")
plot(curve_1B45, add = TRUE, col = "purple")
results <- with(curve_1B45, data.frame(sites, richness, sd))
results

Shealy_0B_2data <- read.csv('Shealy_0B_2.csv', header = TRUE)
curve_0B2 <- specaccum(Shealy_0B_2data, "exact")
plot(curve_0B2, add = TRUE, col = "purple")
results <- with(curve_0B2, data.frame(sites, richness, sd))
results

Shealy_0B_4data <- read.csv('Shealy_0B_4.csv', header = TRUE)
curve_0B4 <- specaccum(Shealy_0B_4data, "exact")
plot(curve_0B4, add = TRUE, col = "purple")
results <- with(curve_0B4, data.frame(sites, richness, sd))
results

Shealy_1B_3Bdata <- read.csv('Shealy_1B_3B.csv', header = TRUE)
curve_1B3B <- specaccum(Shealy_1B_3Bdata, "exact")
plot(curve_1B3B, add = TRUE, col = "purple")
results <- with(curve_1B3B, data.frame(sites, richness, sd))
results

Shealy_1B_E_Sdata <- read.csv('Shealy_1B_E_S.csv', header = TRUE)
curve_1BES <- specaccum(Shealy_1B_E_Sdata, "exact")
plot(curve_1BES, add = TRUE, col = "purple")
results <- with(curve_1BES, data.frame(sites, richness, sd))
results

Shealy_2B_3data <- read.csv('Shealy_2B_3.csv', header = TRUE)
curve_2B3 <- specaccum(Shealy_2B_3data, "exact")
plot(curve_2B3, add = TRUE, col = "purple")
results <- with(curve_2B3, data.frame(sites, richness, sd))
results

Suggs_0B_E_Sdata <- read.csv('Suggs_0B_E_S.csv', header = TRUE)
curve_0BES <- specaccum(Suggs_0B_E_Sdata, "exact")
plot(curve_0BES, add = TRUE, col = "purple")
results <- with(curve_0BES, data.frame(sites, richness, sd))
results

Swanson_3B_2data <- read.csv('Swanson_3B_2.csv', header = TRUE)
curve_3B2 <- specaccum(Swanson_3B_2data, "exact")
plot(curve_3B2, add = TRUE, col = "purple")
results <- with(curve_3B2, data.frame(sites, richness, sd))
results

Timberhaven_2B_1data <- read.csv('Timberhaven_2B_1.csv', header = TRUE)
curve_2B1 <- specaccum(Timberhaven_2B_1data, "exact")
plot(curve_2B1, add = TRUE, col = "purple")
results <- with(curve_2B1, data.frame(sites, richness, sd))
results

#Turkey_Rd_2B_6Adata <- read.csv('Turkey_Rd_2B_6A.csv', header = TRUE)
#curve_2B6A <- specaccum(Turkey_Rd_2B_6Adata, "exact")
#plot(curve_2B6A, add = TRUE, col = "purple")
#results <- with(curve_2B6A, data.frame(sites, richness, sd))
#results

Abercrombie_Rd_0B_E_ABdata <- read.csv('Turner_Abercrombie_Rd_0B_E_AB.csv', header = TRUE)
curve_0BEAB <- specaccum(Abercrombie_Rd_0B_E_ABdata, "exact")
plot(curve_0BEAB, add = TRUE, col = "purple")
results <- with(curve_0BEAB, data.frame(sites, richness, sd))
results

Abercrombie_Rd_1B_2data <- read.csv('Turner_Abercrombie_Rd_1B_2.csv', header = TRUE)
curve_1B2 <- specaccum(Abercrombie_Rd_1B_2data, "exact")
plot(curve_1B2, add = TRUE, col = "purple")
results <- with(curve_1B2, data.frame(sites, richness, sd))
results

Honea_Path_0B_1data <- read.csv('Turner_Honea_Path_0B_1.csv', header = TRUE)
curve_0B1 <- specaccum(Honea_Path_0B_1data, "exact")
plot(curve_0B1, add = TRUE, col = "purple")
results <- with(curve_0B1, data.frame(sites, richness, sd))
results

Honea_Path_3B_1data <- read.csv('Turner_Honea_Path_3B_1.csv', header = TRUE)
curve_3B1 <- specaccum(Honea_Path_3B_1data, "exact")
plot(curve_3B1, add = TRUE, col = "purple")
results <- with(curve_3B1, data.frame(sites, richness, sd))
results

Honea_Path_3B_3data <- read.csv('Turner_Honea_Path_3B_3.csv', header = TRUE)
curve_3B3 <- specaccum(Honea_Path_3B_3data, "exact")
plot(curve_3B3, add = TRUE, col = "purple")
results <- with(curve_3B3, data.frame(sites, richness, sd))
results
