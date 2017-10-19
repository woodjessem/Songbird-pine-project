#PRWA (foliage gleaner, insects, shrub/tree nester 1-45', open wood warbler, ESS/second growth brushy/bushy habitat)
# covariates: age, time since B, grasses, understory growth, shrub density, 
library("unmarked")
setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")

test <-read.csv("prwa_abund.csv")
summary(test)
str(test)
var(test[2:5])
#mean(test[2:4])
mean(test$y.3)

prwa.abund<- csvToUMF("prwa_abund.csv", long = FALSE, type = "unmarkedFramePCount")
##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(prwa.abund)
str(prwa.abund)
#scale all observation covariates (covs of detection)
obsCovs(prwa.abund)= scale (obsCovs(prwa.abund))
#siteCovs(prwa.abund)= scale (siteCovs(prwa.abund))
#select particular site covariates to scale below
#(note: NOT ALL - not treatment, herbicide, or years ones)
sc <- siteCovs(prwa.abund)
sc[,c(4:23)] <- scale(sc[, c(4:23)])
siteCovs(prwa.abund) <- sc

#test for NB or Poisson - most should use Poisson
testP.prwa <- pcount(~1 ~1, prwa.abund, mixture="P", K=4)
testNB.prwa <- pcount(~1 ~1, prwa.abund, mixture="NB", K=4)
fmsTEST <- fitList(testP.prwa, testNB.prwa)
msTEST.prwa <- modSel(fmsTEST)
msTEST.prwa
#NB is best for prwa. Changed below to correspond!

?pcount

#detection covariates first
det.null.prwa <- pcount(~1 ~1, prwa.abund, mixture="NB", K=15)
det.weather.prwa <- pcount(~ Wind + Sky ~1, prwa.abund, mixture="NB", K=15)
det.global.prwa <- pcount(~ Jdate + Wind + Sky + Noise ~1, prwa.abund, mixture="NB", K=15)
det.sound.prwa <- pcount(~ Noise + Wind ~1, prwa.abund, mixture="NB", K=15)
det.date.prwa <- pcount(~ Jdate ~1, prwa.abund, mixture="NB", K=15)
det.detect.prwa <- pcount(~ Jdate + Noise ~1, prwa.abund, mixture="NB", K=15)
det.notdate.prwa <-pcount(~ Wind + Sky + Noise ~1, prwa.abund, mixture="NB", K=15)

fms <- fitList(det.null.prwa, det.weather.prwa, det.global.prwa,
               det.sound.prwa, det.date.prwa, det.detect.prwa, det.notdate.prwa)
ms1.prwa <- modSel(fms)
ms1.prwa
#ms1.prwa@Full
#summary: 1st date, 2nd weather (wind + sky) @ 0.25, 3rd global @ 0.96,
# 4th detect (Jdate + Noise) at 1.52
#    next closest is d2.07 and is notdate.

##site covariates next

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#null detection covariates (aka NOT using the knowledge gained above)
null.prwa <- pcount(~1 ~1, prwa.abund, mixture="NB", K=15)
global.prwa <- pcount(~ 1 ~ Treatment + BA + Nsnags
                      + Ccover + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Herbicide
                      , prwa.abund, mixture="NB", K=15)
local.prwa <- pcount(~ 1 ~ BA + Ccover + TreeHt + Ldepth, prwa.abund, mixture="NB", K=15)
lh.prwa <- pcount(~ 1 ~ Age + TimeSinceB + BA, prwa.abund, mixture="NB", K=15)
##landscape.prwa <- pcount(~ 1 ~ cov 5 + 6, prwa.abund, mixture="NB")
treatment.prwa <- pcount(~ 1 ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide, prwa.abund, mixture="NB", K=15)
disturbance.prwa <- pcount(~ 1 ~ TimeSinceB + TimeSinceT, prwa.abund, mixture="NB", K=15)

fms2 <- fitList(null.prwa, global.prwa, local.prwa, lh.prwa, treatment.prwa, disturbance.prwa)
ms2.prwa <- modSel(fms2)
ms2.prwa@Full
ms2.prwa
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#more appropriate detection covariates (JDate first)
null2.prwa <- pcount(~ Jdate + Noise ~1, prwa.abund, mixture="NB", K=15)
global2.prwa <- pcount(~ Jdate + Noise ~ Treatment + BA + Nsnags
                       + Ccover + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Herbicide
                       + HWdens_10 + HWdens_50 + HWdens_100 + FG_herb + FG_shrub + NHW_saplings
                       + NP_over_20cm + Rel_HW2P_canopy + Rel_HW2P_shrubcover
                       , prwa.abund, mixture="NB", K=15)
local2.prwa <- pcount(~ Jdate + Noise ~ BA + Ccover + TreeHt + Ldepth, prwa.abund, mixture="NB", K=15)
lh2.prwa <- pcount(~ Jdate + Noise ~ Age + TimeSinceB + BA + FG_herb + HWdens_10 + HWdens_50, prwa.abund, mixture="NB", K=15)
#landscape.prwa <- pcount(~ Jdate + Noise ~ cov 5 + 6, prwa.abund, mixture="NB", K=15)
treatment2.prwa <- pcount(~ Jdate + Noise ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide, prwa.abund, mixture="NB", K=15)
disturbance2.prwa <- pcount(~ Jdate + Noise ~ TimeSinceB + TimeSinceT, prwa.abund, mixture="NB", K=15)

fms3 <- fitList(null2.prwa, global2.prwa, local2.prwa, lh2.prwa, treatment2.prwa, disturbance2.prwa)
ms3.prwa <- modSel(fms3)
ms3.prwa
#ms3.prwa@Full
lh2.prwa

#ms3 summary:(with Jdate only): null was best, life history second best but @ 2.87
## ms3 update 10/11/2017 (added in 9 new variables to global & FG_herb to lh):
# now, life history (-Age, +TimeSinceB, -BA, +FG_herb, +HWdens_10, +HWdens_50) is best,
# local is next best but at 3.3!

#just as good detection covariates (global instead! from third best model)
null3.prwa <- pcount(~ Jdate + Wind + Sky + Noise ~1, prwa.abund, mixture="NB", K=15)
global3.prwa <- pcount(~ Jdate + Wind + Sky + Noise ~ Treatment + BA + Nsnags
                       + Ccover + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Herbicide
                       + HWdens_10 + HWdens_50 + HWdens_100 + FG_herb + FG_shrub + NHW_saplings
                       + NP_over_20cm + Rel_HW2P_canopy + Rel_HW2P_shrubcover
                       , prwa.abund, mixture="NB", K=15)
local3.prwa <- pcount(~ Jdate + Wind + Sky + Noise ~ BA + Ccover + TreeHt + Ldepth, prwa.abund, mixture="NB", K=15)
lh3.prwa <- pcount(~ Jdate + Wind + Sky + Noise ~ Age + TimeSinceB + BA + FG_herb + HWdens_10 + HWdens_50, prwa.abund, mixture="NB", K=15)
#landscape3.prwa <- pcount(~ Jdate + Wind + Sky + Noise ~ cov 5 + 6, prwa.abund, mixture="NB", K=15)
treatment3.prwa <- pcount(~ Jdate + Wind + Sky + Noise ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide, prwa.abund, mixture="NB", K=15)
disturbance3.prwa <- pcount(~ Jdate + Wind + Sky + Noise ~ TimeSinceB + TimeSinceT, 
                            prwa.abund, mixture="NB", K=15)

fms4 <- fitList(null3.prwa, local3.prwa, lh3.prwa, treatment3.prwa, disturbance3.prwa) #had to take out global
ms4.prwa <- modSel(fms4)
ms4.prwa

#ms4: null best, life history second best @ 0.73, but this excluded global.

#see help for package "xlsReadWrite" in old notes, if need be#
#write.table(ms1.prwa@Full, file="C:/Users/path.type",sep="\t")