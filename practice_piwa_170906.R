
library("unmarked")
setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")

test <-read.csv("piwa_abund.csv")
summary(test)
str(test)
#stringsAsFactors=FALSE, but this only works for reading, not for below.
var(test[2:5])
mean(test[2:4])
mean(test$y.3)

piwa.abund<- csvToUMF("piwa_abund.csv", long = FALSE, type = "unmarkedFramePCount")
    ##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##

#library(plyr)
#piwa.abund$Treatment2<-revalue(piwa.abund$Treatment, c("0B"="0","1B"="1","2B"="2","3B"="3"))
summary(piwa.abund)
str(piwa.abund)
#scale all observation covariates (covs of detection)
obsCovs(piwa.abund)= scale(obsCovs(piwa.abund))
#only need to scale SOME site covariates
  #(NOT siteCovs(cawr.abund)= scale (siteCovs(cawr.abund)))
#maybe the following can be used instead:
sc <- siteCovs(piwa.abund)
sc[,c(2:9,11,13)] <- scale(sc[, c(2:9,11,13)])
siteCovs(piwa.abund) <- sc

#test for NB or Poisson
testP.piwa <- pcount(~1 ~1, piwa.abund, mixture="P", K=15)
testNB.piwa <- pcount(~1 ~1, piwa.abund, mixture="NB", K=15)
fmsTEST <- fitList(testP.piwa, testNB.piwa)
msTEST.piwa <- modSel(fmsTEST)
msTEST.piwa

#detection covariates first
det.null.piwa <- pcount(~1 ~1, piwa.abund, mixture="P", K=15)
det.weather.piwa <- pcount(~ Wind + Sky ~1, piwa.abund, mixture="P", K=15)
det.global.piwa <- pcount(~ Jdate + Wind + Sky + Noise ~1, piwa.abund, mixture="P", K=15)
det.sound.piwa <- pcount(~ Noise + Wind ~1, piwa.abund, mixture="P", K=15)
det.date.piwa <- pcount(~ Jdate ~1, piwa.abund, mixture="P", K=15)
det.detect.piwa <- pcount(~ Jdate + Noise ~1, piwa.abund, mixture="P", K=15)
det.notdate.piwa <-pcount(~ Wind + Sky + Noise ~1, piwa.abund, mixture="P", K=15)

fms <- fitList(det.null.piwa, det.weather.piwa, det.global.piwa,
               det.sound.piwa, det.date.piwa, det.detect.piwa, det.notdate.piwa)
ms1.piwa <- modSel(fms)
ms1.piwa@Full
ms1.piwa

#on August 23, WITHOUT scaling the covariates:
#found that weather (wind + sky) was top model
# and sound (wind + noise) was really close behind (delta 0.76)
#  but global was least, and date was really not that relevant!
# added "not date" which is wind + sky + noise & it was the third highest model (1.29)

#then, on Aug 29, I did "obsCovs(piwa.abund)= scale(obsCovs(piwa.abund))"
#and I got a different model fit list.
#SO now indicates that I should be doing, at the very least, date. maybe noise.

~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
#site covariates next - * NOT using the covariate knowledge we have
null.piwa <- pcount(~1 ~1, piwa.abund, mixture="P", K=15)
global.piwa <- pcount(~ 1 ~ Treatment + BA + Nsnags
                      + Ccover + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Herbicide
                      , piwa.abund, mixture="P", K=15)
local.piwa <- pcount(~ 1 ~ BA + Ccover + TreeHt + Ldepth, piwa.abund, mixture="P")
lh.piwa <- pcount(~ 1 ~ Ccover + TreeHt + BA, piwa.abund, mixture="P", K=15)
#landscape.piwa <- pcount(~ 1 ~ cov 5 + 6, piwa.abund, mixture="P", K=15)
treatment.piwa <- pcount(~ 1 ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide, piwa.abund, mixture="P", K=15)
disturbance3.piwa <- pcount(~ 1 ~ TimeSinceB + TimeSinceT, piwa.abund,mixture = "P", K=15)

fms2 <- fitList(null.piwa, global.piwa, local.piwa, lh.piwa, treatment.piwa)
ms2.piwa <- modSel(fms2)
ms2.piwa@Full
ms2.piwa
 ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
 ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
##OLD: wind, sky, noise ## ## ## ## DO NOT RUN THROUGH LINE 86!! ## ## ## ## ##
#no K and no mixture type set (NB or P or ZIP) yet
null2.piwa <- pcount(~ Wind + Sky + Noise ~1, piwa.abund)
global2.piwa <- pcount(~ Wind + Sky + Noise ~ Treatment + BA + Nsnags
                      + Ccover + Ldepth + TreeHt + Age + TimeSinceB + Herbicide
                      , piwa.abund)
local2.piwa <- pcount(~ Wind + Sky + Noise ~ BA + Ccover + TreeHt + Ldepth, piwa.abund)
lh2.piwa <- pcount(~ Wind + Sky + Noise ~ Ccover + TreeHt + BA, piwa.abund)
#landscape.piwa <- pcount(~ Wind + Sky + Noise ~ cov 5 + 6, piwa.abund)
treatment2.piwa <- pcount(~ Wind + Sky + Noise ~ Treatment + BA + TimeSinceB + Herbicide, piwa.abund)
disturbance3.piwa <- pcount(~ Wind + Sky + Noise ~ TimeSinceB + TimeSinceT,
                            piwa.abund, mixture = "P", K=15)

fms3 <- fitList(null2.piwa, global2.piwa, local2.piwa, lh2.piwa, treatment2.piwa)
ms3.piwa <- modSel(fms3)
ms3.piwa@Full
ms3.piwa
#summary: all AIC values are lower in the set that includes detection covariates - yay!
#but cumulative weight is lower also... weird.
# life history has a slightly lower deltaAIC value... now just over 2.
~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

##NEW: jdate only
null3.piwa <- pcount(~ Jdate ~1, piwa.abund, K=15)
global3.piwa <- pcount(~ Jdate ~ Treatment + BA + Nsnags
                       + Ccover + Ldepth + TreeHt + Age + TimeSinceB +
                         TimeSinceT + Herbicide
                       , piwa.abund, K=15)
local3.piwa <- pcount(~ Jdate ~ BA + Ccover + TreeHt + Ldepth, piwa.abund, K=15)
lh3.piwa <- pcount(~ Jdate ~ Ccover + TreeHt + BA, piwa.abund, K=15)
#landscape.piwa <- pcount(~ Jdate ~ cov 5 + 6, piwa.abund, K=15)
treatment3.piwa <- pcount(~ Jdate ~ Treatment + BA + TimeSinceB +
                            TimeSinceT + Herbicide, piwa.abund, K=15)
disturbance3.piwa <- pcount(~ Jdate ~ TimeSinceB + TimeSinceT,
                             piwa.abund, mixture = "P", K=15)

fms4 <- fitList(null3.piwa, global3.piwa, local3.piwa, lh3.piwa, treatment3.piwa, disturbance3.piwa)
ms4.piwa <- modSel(fms4)
ms4.piwa
#ms4.piwa@Full
#same order as unscaled covariates, but now lower overall AIC values; higher weights.


#see help for package "xlsReadWrite" in old notes, if need be#
#write.table(ms1.cawr@Full, file="C:/Users/path.type",sep="\t")