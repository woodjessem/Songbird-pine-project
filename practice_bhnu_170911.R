#- BHNU (cavity-nester, bark forager, insects, forest habitat )
#- snags, tree height, age, ccover, - density, stand area, (open understory)

library("unmarked")
setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")

test <-read.csv("bhnu_abund.csv")
summary(test)
str(test)
var(test[2:5])
#mean(test[2:4])
mean(test$y.3)

bhnu.abund<- csvToUMF("bhnu_abund.csv", long = FALSE, type = "unmarkedFramePCount")
##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(bhnu.abund)
str(bhnu.abund)
#scale all observation covariates (covs of detection)
obsCovs(bhnu.abund)= scale (obsCovs(bhnu.abund))
#siteCovs(bhnu.abund)= scale (siteCovs(bhnu.abund))
#select particular site covariates to scale below
#(note: NOT ALL - not treatment, herbicide, or years ones)
sc <- siteCovs(bhnu.abund)
sc[,c(2:9,11,13)] <- scale(sc[, c(2:9,11,13)])
siteCovs(bhnu.abund) <- sc

#test for NB or Poisson - most should use Poisson
testP.bhnu <- pcount(~1 ~1, bhnu.abund, mixture="P", K=4)
testNB.bhnu <- pcount(~1 ~1, bhnu.abund, mixture="NB", K=4)
fmsTEST <- fitList(testP.bhnu, testNB.bhnu)
msTEST.bhnu <- modSel(fmsTEST)
msTEST.bhnu
#Poisson is best for bhnu.

?pcount

#detection covariates first
det.null.bhnu <- pcount(~1 ~1, bhnu.abund, mixture="P", K=15)
det.weather.bhnu <- pcount(~ Wind + Sky ~1, bhnu.abund, mixture="P", K=15)
det.global.bhnu <- pcount(~ Jdate + Wind + Sky + Noise ~1, bhnu.abund, mixture="P", K=15)
det.sound.bhnu <- pcount(~ Noise + Wind ~1, bhnu.abund, mixture="P", K=15)
det.date.bhnu <- pcount(~ Jdate ~1, bhnu.abund, mixture="P", K=15)
det.detect.bhnu <- pcount(~ Jdate + Noise ~1, bhnu.abund, mixture="P", K=15)
det.notdate.bhnu <-pcount(~ Wind + Sky + Noise ~1, bhnu.abund, mixture="P", K=15)

fms <- fitList(det.null.bhnu, det.weather.bhnu, det.global.bhnu,
               det.sound.bhnu, det.date.bhnu, det.detect.bhnu, det.notdate.bhnu)
ms1.bhnu <- modSel(fms)
ms1.bhnu
ms1.bhnu@Full
#summary: 1st null, 2nd date (Jdate),
#    next closest is d2.49 and is weather (wind + sky)

##site covariates next

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#null detection covariates (actually best one from above)
null.bhnu <- pcount(~1 ~1, bhnu.abund, mixture="P", K=15)
global.bhnu <- pcount(~ 1 ~ Treatment + BA + Nsnags
                      + Ccover + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Herbicide
                      , bhnu.abund, mixture="P", K=15)
local.bhnu <- pcount(~ 1 ~ BA + Ccover + TreeHt + Ldepth, bhnu.abund, mixture="P", K=15)
lh.bhnu <- pcount(~ 1 ~ TreeHt + Ccover + BA + Nsnags + Age, bhnu.abund, mixture="P", K=15)
##landscape.bhnu <- pcount(~ 1 ~ cov 5 + 6, bhnu.abund, mixture="P")
treatment.bhnu <- pcount(~ 1 ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide, bhnu.abund, mixture="P", K=15)
disturbance.bhnu <- pcount(~ 1 ~ TimeSinceB + TimeSinceT + Herbicide,
                           bhnu.abund, mixture = "P", K=15)

fms2 <- fitList(null.bhnu, global.bhnu, local.bhnu, lh.bhnu, treatment.bhnu, disturbance.bhnu)
ms2.bhnu <- modSel(fms2)
#ms2.bhnu@Full
ms2.bhnu
#ms2 summary: null highest,
# local (BA + Ccover + TreeHt + Ldepth) next @ 2.02.


# SOME detection covariates (Jdate, based on second best det model)
null2.bhnu <- pcount(~ Jdate ~1, bhnu.abund, mixture="P", K=15)
global2.bhnu <- pcount(~ Jdate ~ Treatment + BA + Nsnags
                       + Ccover + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Herbicide
                       , bhnu.abund, mixture="P", K=15)
local2.bhnu <- pcount(~ Jdate ~ BA + Ccover + TreeHt + Ldepth, bhnu.abund, mixture="P", K=15)
lh2.bhnu <- pcount(~ Jdate~ TreeHt + Ccover + BA + Nsnags + Age, bhnu.abund, mixture="P", K=15)
#landscape.bhnu <- pcount(~ Jdate ~ cov 5 + 6, bhnu.abund, mixture="P", K=15)
treatment2.bhnu <- pcount(~ Jdate ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide, bhnu.abund, mixture="P", K=15)
disturbance2.bhnu <- pcount(~ Jdate ~ TimeSinceB + TimeSinceT,
                            bhnu.abund, mixture = "P", K=15)

fms3 <- fitList(null2.bhnu, global2.bhnu, local2.bhnu, lh2.bhnu, treatment2.bhnu, disturbance2.bhnu)
ms3.bhnu <- modSel(fms3)
ms3.bhnu
#ms3.bhnu@Full

disturbance2.bhnu
confint(disturbance2.bhnu,type="state",method="normal")
confint(disturbance2.bhnu,type="det",method="normal")

#ms3 summary (using Jdate): null highest, disturbance second @ 0.82
#  next closest local @ 2.12.

#see help for package "xlsReadWrite" in old notes, if need be#
write.table(ms2.bhnu@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/bhnu_top_models_ms2.xls",sep="\t")
write.table(ms3.bhnu@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/bhnu_top_models_ms3.xls",sep="\t")
