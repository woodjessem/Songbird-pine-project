#PRWA (foliage gleaner, insects, shrub/tree nester 1-45', open wood warbler, ESS/second growth brushy/bushy habitat)
# covariates: grasses, understory growth, midstory shrub density, 
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
sc[,c(5:26)] <- scale(sc[, c(5:26)])
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


#more appropriate detection covariates (JDate first)  #NB
null2.prwa <- pcount(~ Jdate ~1, prwa.abund, mixture="NB", K=20)
global2.prwa <- pcount(~ Jdate ~ Treatment + Herbicide + BA + Nsnags +Ccover
                       + Ldepth + TreeHt + Age + Nburns + Nthins + TimeSinceB + TimeSinceT
                       + HWdens_50 + FG_herb + FG_shrub + NHW_saplings + NP_over_20cm
                       + Rel_HW2P_canopy + LCR
                       , prwa.abund, mixture="NB", K=20)
local2.prwa <- pcount(~ Jdate ~ BA + Ccover + TreeHt + Ldepth, prwa.abund, mixture="NB", K=20)
lh2.prwa <- pcount(~ Jdate ~ Age + TimeSinceB + FG_herb + HWdens_50 + NHW_saplings, prwa.abund, mixture="NB", K=20)
#landscape.prwa <- pcount(~ Jdate ~ cov 5 + 6, prwa.abund, mixture="NB", K=20)
treatment2.prwa <- pcount(~ Jdate ~ Treatment + Nthins, prwa.abund, mixture ="NB", K=20)
management2.prwa <- pcount(~ Jdate ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide, prwa.abund, mixture="NB", K=20)
disturbance2.prwa <- pcount(~ Jdate ~ TimeSinceB + TimeSinceT, prwa.abund, mixture="NB", K=20)

fms3 <- fitList(null2.prwa, local2.prwa, lh2.prwa, treatment2.prwa, management2.prwa, disturbance2.prwa)
ms3.prwa <- modSel(fms3) #note this does not include global
ms3.prwa
#ms3.prwa@Full
lh2.prwa
null.prwa

## ms3 update 10/20/2017 (added new variables to global & FG_herb + midstory to lh):
# now, life history (-Age, +TimeSinceB, +FG_herb, +HWdens_50, -NHW_saplings) is best,
# dispersion is 0.173, SE 0.989, z 0.175, and P > 0.861
 # null second best at 0.53

#just as good detection covariates (global instead! from third best model)
null3.prwa <- pcount(~ Jdate + Wind + Sky + Noise ~1, prwa.abund, mixture="NB", K=20)
global3.prwa <- pcount(~ Jdate + Wind + Sky + Noise ~ Treatment + Herbicide + BA + Nsnags +Ccover
                       + Ldepth + TreeHt + Age + Nburns + Nthins + TimeSinceB + TimeSinceT
                       + HWdens_50 + FG_herb + FG_shrub + NHW_saplings + NP_over_20cm
                       + Rel_HW2P_canopy + LCR
                       , prwa.abund, mixture="NB", K=20)
local3.prwa <- pcount(~ Jdate + Wind + Sky + Noise ~ BA + Ccover + TreeHt + Ldepth, prwa.abund, mixture="NB", K=20)
lh3.prwa <- pcount(~ Jdate + Wind + Sky + Noise ~ Age + TimeSinceB + FG_herb + HWdens_50 + NHW_saplings, prwa.abund, mixture="NB", K=20)
#landscape3.prwa <- pcount(~ Jdate + Wind + Sky + Noise ~ cov 5 + 6, prwa.abund, mixture="NB", K=20)
treatment3.prwa <- pcount(~ Jdate + Wind + Sky + Noise ~ Treatment + Nthins, prwa.abund, mixture ="NB", K=20)
management3.prwa <- pcount(~ Jdate + Wind + Sky + Noise ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide, prwa.abund, mixture="NB", K=20)
disturbance3.prwa <- pcount(~ Jdate + Wind + Sky + Noise ~ TimeSinceB + TimeSinceT, 
                            prwa.abund, mixture="NB", K=20)

fms4 <- fitList(null3.prwa, local3.prwa, lh3.prwa, treatment3.prwa, management3.prwa, disturbance3.prwa) #had to take out global
ms4.prwa <- modSel(fms4) #no global
ms4.prwa

#ms4: null best, life history second best @ 0.96, but this excluded global.
lh3.prwa
# (-Age, +TimeSinceB, +FG_herb, +HWdens_50, -NHW_saplings)
#dispersion: 0.172 1.05 0.164    0.87

#see help for package "xlsReadWrite" in old notes, if need be#
write.table(ms3.prwa@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/prwa_top_models_ms3.xls",sep="\t")
write.table(ms4.prwa@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/prwa_top_models_ms4.xls",sep="\t")
