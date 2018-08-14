# NOBO (grasslands - open woodlands, ground nester, ground forager, plants) 
# covariates: low BA, fires/disturbance, litter, low-medium HW cover?
library("unmarked")
setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")

test <-read.csv("nobo_abund.csv") #spp CSV file!
summary(test)
str(test)
var(test[2:5])  #what were these testing for? variance>mean?
#mean(test[2:4])
mean(test$y.3)

nobo.abund<- csvToUMF("nobo_abund.csv", long = FALSE, type = "unmarkedFramePCount")
##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(nobo.abund)
str(nobo.abund)
#scale all observation covariates (covs of detection)
obsCovs(nobo.abund)= scale (obsCovs(nobo.abund))
#siteCovs(nobo.abund)= scale (siteCovs(nobo.abund))
#select particular site covariates to scale below
#(note: NOT ALL - not treatment, herbicide, or years ones)
sc <- siteCovs(nobo.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)]) #from 26 to 74 +landscape+soils
siteCovs(nobo.abund) <- sc

#test for NB or Poisson - most should use Poisson ... 
testP.nobo <- pcount(~1 ~1, nobo.abund, mixture="P", K=4)
testNB.nobo <- pcount(~1 ~1, nobo.abund, mixture="NB", K=4)
fmsTEST <- fitList(testP.nobo, testNB.nobo)
msTEST.nobo <- modSel(fmsTEST)
msTEST.nobo
## NB is best for this species. Change below to correspond!

?pcount

#detection covariates first
det.null.nobo <- pcount(~1 ~1, nobo.abund, mixture="NB", K=15)
det.weather.nobo <- pcount(~ Wind + Sky ~1, nobo.abund, mixture="NB", K=15)
det.global.nobo <- pcount(~ Jdate + Wind + Sky + Noise +Time ~1, nobo.abund, mixture="NB", K=15)
det.sound.nobo <- pcount(~ Noise + Wind ~1, nobo.abund, mixture="NB", K=15)
det.date.nobo <- pcount(~ Jdate ~1, nobo.abund, mixture="NB", K=15)
det.detect.nobo <- pcount(~ Jdate + Noise + Time ~1, nobo.abund, mixture="NB", K=15)
det.notdate.nobo <-pcount(~ Wind + Sky + Noise ~1, nobo.abund, mixture="NB", K=15)
det.time.nobo <-pcount(~ Time ~1, nobo.abund, mixture="NB",K=15)
det.timing.nobo <-pcount(~ Time + Jdate ~1, nobo.abund, mixture="NB", K=15)

fmsDC <- fitList(det.null.nobo, det.weather.nobo, det.global.nobo,
                 det.sound.nobo, det.date.nobo, det.detect.nobo, det.notdate.nobo,
                 det.time.nobo, det.timing.nobo)
msDC.nobo <- modSel(fmsDC)
msDC.nobo
#msDC.nobo@Full
#summary: NB is better fit than Poisson. Global is best detection model.
##     date only is second best model.

det.global.nobo  # positive w date, neg with sky
confint(det.global.nobo, type="state",method="normal")  #?
write.table(msDC.nobo@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/nobo_top_models_msDC.xls",sep="\t")

det.date.nobo  #positive with date
confint(det.date.nobo, type="state", method="normal")   #overlaps 0 though

##site covariates next

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# covariates: low BA, fires/disturbance, litter, low-medium HW cover?

#more appropriate detection covariates (global)
null.nobo <- pcount(~ Jdate + Wind + Sky + Noise +Time ~1, nobo.abund, mixture="NB", K=80)
global.nobo <- pcount(~ Jdate + Wind + Sky + Noise +Time
                      ~ Treatment + Herbicide + BA +Ccover + TreeHt
                       + Ldepth + Age + TimeSinceB + TimeSinceT + Nthins
                       + HW_dens_1050 + FG_shrub
                       + PISoils + NSoilTypes
                       + Parea + ShapeIndex
                       , nobo.abund, mixture="NB", K=80)
#FPSiteIndex taken out, TreeHt, NHW_saplings, NP_over_20cm, FG_herb, Rel_HW2P_canopy, Nsnags
local.nobo <- pcount(~ Jdate + Wind + Sky + Noise +Time
                     ~ Ccover + TreeHt + Ldepth
                     , nobo.abund, mixture="NB", K=80) #can only include BA OR CCover
lh.nobo <- pcount(~ Jdate + Wind + Sky + Noise +Time
                  ~ BA + Ldepth + HW_dens_1050 + FG_shrub + Age
                  , nobo.abund, mixture="NB", K=80)
landmetrics.nobo <- pcount (~ Jdate + Wind + Sky + Noise +Time
                          ~ Parea + ShapeIndex
                        , nobo.abund, mixture="NB",K=80)
landscape500.nobo <- pcount(~ Jdate + Wind + Sky + Noise +Time
                        ~ Grass500m + HighDev500m + Schrubs500m + Evergreen500m + Ag500m
                            , nobo.abund, mixture="NB", K=80)
landscape1.nobo <- pcount(~ Jdate + Wind + Sky + Noise +Time
                       ~  Grass1km + HighDev1km + Schrubs1km + Evergreen1km + Ag1km
                          , nobo.abund, mixture="NB", K=80)
landscape5.nobo <- pcount(~ Jdate + Wind + Sky + Noise +Time
                       ~ Grass5km + HighDev5km + Schrubs5km + Evergreen5km
                          , nobo.abund, mixture="NB", K=80)
                  # - can't use Evergreen&Ag,
                  #+ can't use HighDev&OpenDev together
landscape30.nobo <- pcount(~ Jdate + Wind + Sky + Noise +Time
                       ~ Grass30km + HighDev30km + Evergreen30km + Protected30km
                           , nobo.abund, mixture="NB", K=80)
                  #- can't use Protected&Ag together,
                  #- can't use Ag&HighDev together
                  #- can't use Evergreen&Ag together
                  #- can't use HighDev&OpenDev together
                  #- can't use Schrubs&OpenDev together
                  #+ can't use Grass&Ag together
                  #+ can't use Ag&OpenDev together
                  #+ can't use Water&Protected together
                  #+ can't use Schrubs&HighDev together
treatment.nobo <- pcount(~ Jdate + Wind + Sky + Noise +Time
                         ~ Nburns + Nthins
                         , nobo.abund, mixture ="NB", K=80)  #this one I made Nburns instead of treatment for ones with many many burns
management.nobo <- pcount(~ Jdate + Wind + Sky + Noise +Time
                          ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide
                          , nobo.abund, mixture="NB", K=80)
disturbance.nobo <- pcount(~ Jdate + Wind + Sky + Noise +Time
                           ~ TimeSinceB + TimeSinceT
                           , nobo.abund, mixture="NB", K=80)
siteprod.nobo <- pcount(~ Jdate + Wind + Sky + Noise +Time ~ PISoils + NSoilTypes
                        , nobo.abund, mixture="NB", K=80)  #no FPSiteIndex
#upstate.nobo <- pcount(~ Jdate + Wind + Sky + Noise +Time ~ X + Y + Z, nobo.abund, mixture="NB", K=40)


fms <- fitList(null.nobo, local.nobo, lh.nobo, landmetrics.nobo,
               landscape500.nobo, landscape1.nobo, landscape5.nobo, landscape30.nobo,
               treatment.nobo, management.nobo, disturbance.nobo,
                siteprod.nobo)
ms.nobo <- modSel(fms) #note this does not include FPSiteIndex or upstate OR GLOBAL
ms.nobo
ms.nobo@Full

lh.nobo
#dispersion & abundance summary:
#Abundance:
#  Estimate    SE      z P(>|z|)
#(Intercept)    0.0173 0.559  0.031  0.9753
#BA            -0.4922 0.216 -2.281  0.0226
#Ldepth        -0.1166 0.252 -0.462  0.6439
#HW_dens_1050   0.3123 0.222  1.407  0.1593
#FG_shrub       0.5488 0.212  2.586  0.0097
#Age            0.0773 0.192  0.404  0.6865

#Detection:
#          Estimate    SE       z  P(>|z|)
#(Intercept)  -2.2880 0.760 -3.0105 2.61e-03
#Jdate         1.2972 0.331  3.9196 8.87e-05
#Wind          0.4707 0.262  1.7995 7.19e-02
#Sky          -0.6892 0.334 -2.0642 3.90e-02
#Noise        -0.0248 0.264 -0.0942 9.25e-01
#Time         -0.3358 0.319 -1.0518 2.93e-01

#Dispersion:
#  Estimate  SE      z P(>|z|)
#    10.5 107 0.0983   0.922

confint(lh.nobo, type="state",method="normal")
#summary of output:
#0.025       0.975
#lam(Int)          -1.0792112  1.11387443
#lam(BA)           -0.9152859 -0.06920571
#lam(Ldepth)       -0.6112370  0.37795119
#lam(HW_dens_1050) -0.1226556  0.74725458
#lam(FG_shrub)      0.1329231  0.96466267
#lam(Age)          -0.2982293  0.45289993

local.nobo
confint(local.nobo, type="state",method="normal")

landscape1.nobo
confint(landscape1.nobo, type="state",method="normal")

treatment.nobo
confint(treatment.nobo, type="state",method="normal")

landscape5.nobo
confint(landscape5.nobo, type="state",method="normal")
write.table(ms.nobo@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/nobo_top_models_ms.xls",sep="\t")

#quick test between some correlated
testShrub.nobo <- pcount(~1 ~ FG_shrub, nobo.abund, mixture="NB", K=10)
testNHWsaps.nobo <- pcount(~1 ~ NHW_saplings, nobo.abund, mixture="NB", K=10)
testHW1050.nobo <- pcount(~1 ~ HW_dens_1050, nobo.abund, mixture="NB", K=10)
fmsveg.test <- fitList(testShrub.nobo, testHW1050.nobo, testNHWsaps.nobo)
msT.nobo <- modSel(fmsveg.test)
msT.nobo

testAg5km.nobo <- pcount(~1 ~ Ag5km, nobo.abund, mixture="NB", K=10)
testEv5km.nobo <- pcount(~1 ~ Evergreen5km, nobo.abund, mixture="NB", K=10)
fmshab.test <- fitList(testAg5km.nobo, testEv5km.nobo)
msHT.nobo <- modSel(fmshab.test)
msHT.nobo

################### paste parboot stuff below when figured out ####
prwa.abund<- csvToUMF("prwa_abund.csv", long = FALSE, type = "unmarkedFramePCount")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#next best detection covariates ()
#modelname+2

fms2 <- fitList(null2.prwa, global2.prwa, local2.prwa, lh2.prwa, landmetrics2.prwa, landscape500_2.prwa, landscape1_2.prwa, 
               landscape5_2.prwa, landscape30_2.prwa, treatment2.prwa, management2.prwa, disturbance2.prwa, siteprod2.prwa, upstate2.prwa)
ms2.prwa <- modSel(fms2) #note this does not include 
ms2.prwa
ms2.prwa@Full

landscape5_2.prwa
#dispersion & abundance summary:

confint(landscape5_2.prwa, type="state",method="normal")
#summary of output:

#write.table(ms2.prwa@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/XXX_top_models_ms2.xls",sep="\t")
