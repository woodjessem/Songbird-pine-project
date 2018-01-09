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
sc[,c(5:74)] <- scale(sc[, c(5:74)]) #from 26 to 74 +landscape+soils
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
null2.prwa <- pcount(~ Jdate ~1, prwa.abund, mixture="NB", K=40)
global2.prwa <- pcount(~ Jdate ~ Treatment + Herbicide + BA + Nsnags +Ccover
                       + Ldepth + TreeHt + Age + Nburns + Nthins + TimeSinceB + TimeSinceT
                       + HWdens_50 + FG_herb + FG_shrub + NHW_saplings + NP_over_20cm
                       + Rel_HW2P_canopy + LCR + PISoils + FPSiteIndex + NSoilTypes
                       , prwa.abund, mixture="NB", K=40)
local2.prwa <- pcount(~ Jdate ~ BA + Ccover + TreeHt + Ldepth, prwa.abund, mixture="NB", K=40)
lh2.prwa <- pcount(~ Jdate ~ Age + TimeSinceB + FG_herb + HWdens_50 + NHW_saplings, prwa.abund, mixture="NB", K=40)
landmetrics.prwa <- pcount (~ Jdate ~ Parea + ShapeIndex + PAratio + FracDimIndex + CoreAreaIndex, prwa.abund, mixture="NB",K=40)
landscape500.prwa <- pcount(~ Jdate ~ Evergreen500m + Grass500m + HighDev500m + Schrubs500m, prwa.abund, mixture="NB", K=40)
landscape1.prwa <- pcount(~ Jdate ~ Evergreen1km + Grass1km + HighDev1km + Schrubs1km, prwa.abund, mixture="NB", K=40)
landscape5.prwa <- pcount(~ Jdate ~ Evergreen5km + Grass5km + HighDev5km + Schrubs5km, prwa.abund, mixture="NB", K=40)
landscape30.prwa <- pcount(~ Jdate ~ Evergreen30km + Grass30km + HighDev30km + Schrubs30km, prwa.abund, mixture="NB", K=40)
treatment2.prwa <- pcount(~ Jdate ~ Treatment + Nthins, prwa.abund, mixture ="NB", K=40)
management2.prwa <- pcount(~ Jdate ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide, prwa.abund, mixture="NB", K=40)
disturbance2.prwa <- pcount(~ Jdate ~ TimeSinceB + TimeSinceT, prwa.abund, mixture="NB", K=40)
#siteprod.prwa <- pcount(~ Jdate ~ PISoils + FPSiteIndex + NSoilTypes, prwa.abund, mixture="NB", K=40)

fms3 <- fitList(null2.prwa, local2.prwa, lh2.prwa, landmetrics.prwa, landscape500.prwa, landscape1.prwa, 
                landscape5.prwa, landscape30.prwa, treatment2.prwa, management2.prwa, disturbance2.prwa)
ms3.prwa <- modSel(fms3) #note this does not include global or siteprod
ms3.prwa
ms3.prwa@Full
landscape5.prwa #only one that is below delta2
landscape1.prwa #d7.70
landscape500.prwa #11.15
lh2.prwa #18.27
null.prwa #18.75

landscape5.prwa
#dispersion: estimate 8.02, SE 38.2, z 0.21, P>z 0.834
# Abundance:
#             Estimate    SE      z P(>|z|)
#(Intercept)    -4.304 1.758 -2.449  0.0143
#Evergreen5km   -0.352 0.505 -0.698  0.4852
#Grass5km       -0.704 0.413 -1.705  0.0882
#HighDev5km     -8.043 3.414 -2.356  0.0185
#Schrubs5km      0.167 0.266  0.626  0.5315
confint(landscape5.prwa, type="state",method="normal")
#                       0.025      0.975
#lam(Int)           -7.7486058 -0.8590123     #sig
#lam(Evergreen5km)  -1.3411178  0.6368127
#lam(Grass5km)      -1.5137667  0.1052621
#lam(HighDev5km)   -14.7343936 -1.3512654     #sig
#lam(Schrubs5km)    -0.3551819  0.6883344

#12/04 update added landscape models and 3 now come before life history! exported see below.
write.table(ms3.prwa@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/prwa_top_models_ms3_with_landscape.xls",sep="\t")
#write.table(##, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/prwa_top_models_ms3_with_landscape.xls",sep="\t")


## ms3 update 10/20/2017 (added new variables to global & FG_herb + midstory to lh):
# now, life history (-Age, +TimeSinceB, +FG_herb, +HWdens_50, -NHW_saplings) is best,
# dispersion is 0.173, SE 0.989, z 0.175, and P > 0.861
 # null second best at 0.53

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
