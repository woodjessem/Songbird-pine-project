# NESTING GUILD #
                 # cavity   #tree ~ 150  #shrub ~ 300   #ground ~ 450
library("unmarked")
setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")

## Cavity Nesters   n = 16 ##
# covariates: tree height, age, BA, big trees, snags, open space  #burns based on Greenberg paper!

cavity.abund<- csvToUMF("Nesting_cavity_pcount.csv", long = FALSE, type = "unmarkedFramePCount")
##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(cavity.abund)
str(cavity.abund)
#scale all observation covariates (covs of detection)
obsCovs(cavity.abund)= scale (obsCovs(cavity.abund))
#siteCovs(cavity.abund)= scale (siteCovs(cavity.abund))
#select particular site covariates to scale below
#(note: NOT ALL - not treatment, herbicide, or years ones)
sc <- siteCovs(cavity.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)]) #from 26 to 74 +landscape+soils
siteCovs(cavity.abund) <- sc

#test for NB or Poisson - most should use Poisson
testP.cavity <- pcount(~1 ~1, cavity.abund, mixture="P", K=50)
testNB.cavity <- pcount(~1 ~1, cavity.abund, mixture="NB", K=50)
fmsTEST <- fitList(testP.cavity, testNB.cavity)
msTEST.cavity <- modSel(fmsTEST)
msTEST.cavity
## P is best for cavity nesters group.

?pcount

#detection covariates first
det.null.cavity <- pcount(~1 ~1, cavity.abund, mixture="P", K=50)
det.weather.cavity <- pcount(~ Wind + Sky ~1, cavity.abund, mixture="P", K=50)
det.global.cavity <- pcount(~ Jdate + Wind + Sky + Noise +Time ~1, cavity.abund, mixture="P", K=50)
det.sound.cavity <- pcount(~ Noise + Wind ~1, cavity.abund, mixture="P", K=50)
det.date.cavity <- pcount(~ Jdate ~1, cavity.abund, mixture="P", K=50)
det.detect.cavity <- pcount(~ Jdate + Noise + Time ~1, cavity.abund, mixture="P", K=50)
det.notdate.cavity <-pcount(~ Wind + Sky + Noise ~1, cavity.abund, mixture="P", K=50)
det.time.cavity <-pcount(~ Time ~1, cavity.abund, mixture="P",K=50)
det.timing.cavity <-pcount(~ Time + Jdate ~1, cavity.abund, mixture="P", K=50)

fmsDC <- fitList(det.null.cavity, det.weather.cavity, det.global.cavity,
                 det.sound.cavity, det.date.cavity, det.detect.cavity, det.notdate.cavity,
                 det.time.cavity, det.timing.cavity)
msDC.cavity <- modSel(fmsDC)
msDC.cavity
#msDC.cavity@Full
#summary: weather, time, date, timing, global, null, notdate all under 2.0

det.weather.cavity
confint(det.weather.cavity, type="state",method="normal")
write.table(msDC.cavity@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/Nest_cavity_top_models_msDC.xls",sep="\t")


##site covariates next

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#null detection covariates (aka NOT using the knowledge gained above - NOT RUN)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#more appropriate detection covariates (Wind + Sky)
null.cavity <- pcount(~ Wind + Sky ~1, cavity.abund, mixture="P", K=80)
global.cavity <- pcount(~ Wind + Sky
                      ~ Treatment + Herbicide + BA + Nsnags +Ccover
                       + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Nthins + Nburns
                       + HW_dens_1050 + NP_over_20cm
                       + Rel_HW2P_canopy + PISoils  + NSoilTypes
                       + Parea + ShapeIndex
                       , cavity.abund, mixture="P", K=80) #+ FPSiteIndex
local.cavity <- pcount(~ Wind + Sky
                     ~ Ccover + TreeHt + Ldepth
                     , cavity.abund, mixture="P", K=80) #can only include BA OR CCover
lh.cavity <- pcount(~ Wind + Sky
                  ~ Age + TreeHt + BA + NP_over_20cm + Nsnags + Rel_HW2P_canopy
                  , cavity.abund, mixture="P", K=80)
#tree height, age, BA, big trees, snags, open space  #burns based on Greenberg paper!
landmetrics.cavity <- pcount (~ Wind + Sky
                          ~ Parea + ShapeIndex
                        , cavity.abund, mixture="P",K=80)
landscape500.cavity <- pcount(~ Wind + Sky
                        ~ Evergreen500m + HighDev500m + Schrubs500m
                            , cavity.abund, mixture="P", K=80)
landscape1.cavity <- pcount(~ Wind + Sky
                       ~ Evergreen1km + HighDev1km + Schrubs1km
                          , cavity.abund, mixture="P", K=80)
landscape5.cavity <- pcount(~ Wind + Sky
                       ~ Evergreen5km + HighDev5km + Schrubs5km
                          , cavity.abund, mixture="P", K=80)
                  # - can't use Evergreen&Ag,
                  #+ can't use HighDev&OpenDev together
landscape30.cavity <- pcount(~ Wind + Sky
                       ~ Evergreen30km + HighDev30km + Protected30km
                           , cavity.abund, mixture="P", K=80)
                  #- can't use Protected&Ag together,
                  #- can't use Ag&HighDev together
                  #- can't use Evergreen&Ag together
                  #- can't use HighDev&OpenDev together
                  #- can't use Schrubs&OpenDev together
                  #+ can't use Grass&Ag together
                  #+ can't use Ag&OpenDev together
                  #+ can't use Water&Protected together
                  #+ can't use Schrubs&HighDev together
treatment.cavity <- pcount(~ Wind + Sky
                         ~ Treatment + Nthins
                         , cavity.abund, mixture ="P", K=80)
management.cavity <- pcount(~ Wind + Sky
                          ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide
                          , cavity.abund, mixture="P", K=80)
disturbance.cavity <- pcount(~ Wind + Sky
                           ~ TimeSinceB + TimeSinceT
                           , cavity.abund, mixture="P", K=80)
siteprod.cavity <- pcount(~ Wind + Sky ~ PISoils + NSoilTypes
                        , cavity.abund, mixture="P", K=80)    #FPSiteIndex
greenberg.cavity <- pcount(~ Wind + Sky ~ BA + Nsnags + Nburns
                           , cavity.abund, mixture="P", K=80)

fmsCN <- fitList(null.cavity, global.cavity, local.cavity, lh.cavity, landmetrics.cavity,
               landscape500.cavity, landscape1.cavity, landscape5.cavity, landscape30.cavity,
               treatment.cavity, management.cavity, disturbance.cavity,
                siteprod.cavity, greenberg.cavity)
ms.cavity <- modSel(fmsCN)
ms.cavity
#ms.cavity@Full

siteprod.cavity
confint(siteprod.cavity, type="state",method="normal")

write.table(ms.cavity@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/Nest_cavity_top_models_ms.xls",sep="\t")

#quick test between some correlated
testHigh.cawr <- pcount(~Jdate ~ HighDev30km, cawr.abund, mixture="P", K=10)
testOpen.cawr <- pcount(~Jdate ~ OpenDev30km, cawr.abund, mixture="P", K=10)
testSchrubs.cawr <- pcount(~Jdate ~ Schrubs30km, cawr.abund, mixture="P", K=10)
fmsctest <- fitList(testHigh.cawr, testOpen.cawr, testSchrubs.cawr)
msT.cawr <- modSel(fmsctest)
msT.cawr


################### paste parboot stuff below when figured out ####
cavity.abund<- csvToUMF("Nesting_cavity_pcount.csv", long = FALSE, type = "unmarkedFramePCount")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#next best detection covariates () for cavity-nesters
#modelname+2


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##    TREE NESTERS  n = 25  #
#covariates: 


cavity.abund<- csvToUMF("Nesting_cavity_pcount.csv", long = FALSE, type = "unmarkedFramePCount")
##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(cavity.abund)
str(cavity.abund)
#scale all observation covariates (covs of detection)
obsCovs(cavity.abund)= scale (obsCovs(cavity.abund))
#siteCovs(cavity.abund)= scale (siteCovs(cavity.abund))
#select particular site covariates to scale below
#(note: NOT ALL - not treatment, herbicide, or years ones)
sc <- siteCovs(cavity.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)]) #from 26 to 74 +landscape+soils
siteCovs(cavity.abund) <- sc

#test for NB or Poisson - most should use Poisson
testP.cavity <- pcount(~1 ~1, cavity.abund, mixture="P", K=50)
testNB.cavity <- pcount(~1 ~1, cavity.abund, mixture="NB", K=50)
fmsTEST <- fitList(testP.cavity, testNB.cavity)
msTEST.cavity <- modSel(fmsTEST)
msTEST.cavity
## P is best for cavity nesters group.

?pcount

#detection covariates first
det.null.cavity <- pcount(~1 ~1, cavity.abund, mixture="P", K=50)
det.weather.cavity <- pcount(~ Wind + Sky ~1, cavity.abund, mixture="P", K=50)
det.global.cavity <- pcount(~ Jdate + Wind + Sky + Noise +Time ~1, cavity.abund, mixture="P", K=50)
det.sound.cavity <- pcount(~ Noise + Wind ~1, cavity.abund, mixture="P", K=50)
det.date.cavity <- pcount(~ Jdate ~1, cavity.abund, mixture="P", K=50)
det.detect.cavity <- pcount(~ Jdate + Noise + Time ~1, cavity.abund, mixture="P", K=50)
det.notdate.cavity <-pcount(~ Wind + Sky + Noise ~1, cavity.abund, mixture="P", K=50)
det.time.cavity <-pcount(~ Time ~1, cavity.abund, mixture="P",K=50)
det.timing.cavity <-pcount(~ Time + Jdate ~1, cavity.abund, mixture="P", K=50)

fmsDC <- fitList(det.null.cavity, det.weather.cavity, det.global.cavity,
                 det.sound.cavity, det.date.cavity, det.detect.cavity, det.notdate.cavity,
                 det.time.cavity, det.timing.cavity)
msDC.cavity <- modSel(fmsDC)
msDC.cavity
#msDC.cavity@Full
#summary: weather, time, date, timing, global, null, notdate all under 2.0

det.weather.cavity
confint(det.weather.cavity, type="state",method="normal")
write.table(msDC.cavity@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/Nest_cavity_top_models_msDC.xls",sep="\t")


##site covariates next

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#null detection covariates (aka NOT using the knowledge gained above - NOT RUN)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

null.cavity <- pcount(~ Wind + Sky ~1, cavity.abund, mixture="P", K=80)
global.cavity <- pcount(~ Wind + Sky
                        ~ Treatment + Herbicide + BA + Nsnags +Ccover
                        + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Nthins
                        + HW_dens_1050 + FG_herb + FG_shrub + NHW_saplings + NP_over_20cm
                        + Rel_HW2P_canopy + PISoils + FPSiteIndex + NSoilTypes
                        + Parea + ShapeIndex
                        , cavity.abund, mixture="P", K=80) #FPSiteIndex
local.cavity <- pcount(~ Wind + Sky
                       ~ Ccover + TreeHt + Ldepth
                       , cavity.abund, mixture="P", K=80) #can only include BA OR CCover
lh.cavity <- pcount(~ Wind + Sky
                    ~ Age + TreeHt + BA + NP_over_20cm + Nsnags + Rel_HW2P_canopy
                    , cavity.abund, mixture="P", K=80)
#tree height, age, BA, big trees, snags, open space  #burns based on Greenberg paper!
landmetrics.cavity <- pcount (~ Wind + Sky
                              ~ Parea + ShapeIndex
                              , cavity.abund, mixture="P",K=80)
landscape500.cavity <- pcount(~ Wind + Sky
                              ~ Evergreen500m + HighDev500m + Schrubs500m
                              , cavity.abund, mixture="P", K=80)
landscape1.cavity <- pcount(~ Wind + Sky
                            ~ Evergreen1km + HighDev1km + Schrubs1km
                            , cavity.abund, mixture="P", K=80)
landscape5.cavity <- pcount(~ Wind + Sky
                            ~ Evergreen5km + HighDev5km + Schrubs5km
                            , cavity.abund, mixture="P", K=80)
# - can't use Evergreen&Ag,
#+ can't use HighDev&OpenDev together
landscape30.cavity <- pcount(~ Wind + Sky
                             ~ Evergreen30km + HighDev30km + Protected30km
                             , cavity.abund, mixture="P", K=80)
#- can't use Protected&Ag together,
#- can't use Ag&HighDev together
#- can't use Evergreen&Ag together
#- can't use HighDev&OpenDev together
#- can't use Schrubs&OpenDev together
#+ can't use Grass&Ag together
#+ can't use Ag&OpenDev together
#+ can't use Water&Protected together
#+ can't use Schrubs&HighDev together
treatment.cavity <- pcount(~ Wind + Sky
                           ~ Treatment + Nthins
                           , cavity.abund, mixture ="P", K=80)
management.cavity <- pcount(~ Wind + Sky
                            ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide
                            , cavity.abund, mixture="P", K=80)
disturbance.cavity <- pcount(~ Wind + Sky
                             ~ TimeSinceB + TimeSinceT
                             , cavity.abund, mixture="P", K=80)
siteprod.cavity <- pcount(~ Wind + Sky ~ PISoils + NSoilTypes
                          , cavity.abund, mixture="P", K=80)    #FPSiteIndex
greenberg.cavity <- pcount(~ Wind + Sky ~ BA + Nsnags + Nburns
                           , cavity.abund, mixture="P", K=80)

fmsCN <- fitList(null.cavity, global.cavity, local.cavity, lh.cavity, landmetrics.cavity,
                 landscape500.cavity, landscape1.cavity, landscape5.cavity, landscape30.cavity,
                 treatment.cavity, management.cavity, disturbance.cavity,
                 siteprod.cavity, greenberg.cavity)
ms.cavity <- modSel(fmsCN)
ms.cavity
#ms.cavity@Full

siteprod.cavity
confint(siteprod.cavity, type="state",method="normal")

write.table(ms.cavity@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/Nest_cavity_top_models_ms.xls",sep="\t")

#quick test between some correlated
testHigh.cawr <- pcount(~Jdate ~ HighDev30km, cawr.abund, mixture="P", K=10)
testOpen.cawr <- pcount(~Jdate ~ OpenDev30km, cawr.abund, mixture="P", K=10)
testSchrubs.cawr <- pcount(~Jdate ~ Schrubs30km, cawr.abund, mixture="P", K=10)
fmsctest <- fitList(testHigh.cawr, testOpen.cawr, testSchrubs.cawr)
msT.cawr <- modSel(fmsctest)
msT.cawr


################### paste parboot stuff below when figured out ####
cavity.abund<- csvToUMF("Nesting_cavity_pcount.csv", long = FALSE, type = "unmarkedFramePCount")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#next best detection covariates () for cavity-nesters
#modelname+2


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##    SHRUB NESTERS  n = 16  #
#covariates: 

cavity.abund<- csvToUMF("Nesting_cavity_pcount.csv", long = FALSE, type = "unmarkedFramePCount")
##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(cavity.abund)
str(cavity.abund)
#scale all observation covariates (covs of detection)
obsCovs(cavity.abund)= scale (obsCovs(cavity.abund))
#siteCovs(cavity.abund)= scale (siteCovs(cavity.abund))
#select particular site covariates to scale below
#(note: NOT ALL - not treatment, herbicide, or years ones)
sc <- siteCovs(cavity.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)]) #from 26 to 74 +landscape+soils
siteCovs(cavity.abund) <- sc

#test for NB or Poisson - most should use Poisson
testP.cavity <- pcount(~1 ~1, cavity.abund, mixture="P", K=50)
testNB.cavity <- pcount(~1 ~1, cavity.abund, mixture="NB", K=50)
fmsTEST <- fitList(testP.cavity, testNB.cavity)
msTEST.cavity <- modSel(fmsTEST)
msTEST.cavity
## P is best for cavity nesters group.

?pcount

#detection covariates first
det.null.cavity <- pcount(~1 ~1, cavity.abund, mixture="P", K=50)
det.weather.cavity <- pcount(~ Wind + Sky ~1, cavity.abund, mixture="P", K=50)
det.global.cavity <- pcount(~ Jdate + Wind + Sky + Noise +Time ~1, cavity.abund, mixture="P", K=50)
det.sound.cavity <- pcount(~ Noise + Wind ~1, cavity.abund, mixture="P", K=50)
det.date.cavity <- pcount(~ Jdate ~1, cavity.abund, mixture="P", K=50)
det.detect.cavity <- pcount(~ Jdate + Noise + Time ~1, cavity.abund, mixture="P", K=50)
det.notdate.cavity <-pcount(~ Wind + Sky + Noise ~1, cavity.abund, mixture="P", K=50)
det.time.cavity <-pcount(~ Time ~1, cavity.abund, mixture="P",K=50)
det.timing.cavity <-pcount(~ Time + Jdate ~1, cavity.abund, mixture="P", K=50)

fmsDC <- fitList(det.null.cavity, det.weather.cavity, det.global.cavity,
                 det.sound.cavity, det.date.cavity, det.detect.cavity, det.notdate.cavity,
                 det.time.cavity, det.timing.cavity)
msDC.cavity <- modSel(fmsDC)
msDC.cavity
#msDC.cavity@Full
#summary: weather, time, date, timing, global, null, notdate all under 2.0

det.weather.cavity
confint(det.weather.cavity, type="state",method="normal")
write.table(msDC.cavity@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/Nest_cavity_top_models_msDC.xls",sep="\t")


##site covariates next

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#null detection covariates (aka NOT using the knowledge gained above - NOT RUN)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

null.cavity <- pcount(~ Wind + Sky ~1, cavity.abund, mixture="P", K=80)
global.cavity <- pcount(~ Wind + Sky
                        ~ Treatment + Herbicide + BA + Nsnags +Ccover
                        + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Nthins
                        + HW_dens_1050 + FG_herb + FG_shrub + NHW_saplings + NP_over_20cm
                        + Rel_HW2P_canopy + PISoils + FPSiteIndex + NSoilTypes
                        + Parea + ShapeIndex
                        , cavity.abund, mixture="P", K=80) #FPSiteIndex
local.cavity <- pcount(~ Wind + Sky
                       ~ Ccover + TreeHt + Ldepth
                       , cavity.abund, mixture="P", K=80) #can only include BA OR CCover
lh.cavity <- pcount(~ Wind + Sky
                    ~ Age + TreeHt + BA + NP_over_20cm + Nsnags + Rel_HW2P_canopy
                    , cavity.abund, mixture="P", K=80)
#tree height, age, BA, big trees, snags, open space  #burns based on Greenberg paper!
landmetrics.cavity <- pcount (~ Wind + Sky
                              ~ Parea + ShapeIndex
                              , cavity.abund, mixture="P",K=80)
landscape500.cavity <- pcount(~ Wind + Sky
                              ~ Evergreen500m + HighDev500m + Schrubs500m
                              , cavity.abund, mixture="P", K=80)
landscape1.cavity <- pcount(~ Wind + Sky
                            ~ Evergreen1km + HighDev1km + Schrubs1km
                            , cavity.abund, mixture="P", K=80)
landscape5.cavity <- pcount(~ Wind + Sky
                            ~ Evergreen5km + HighDev5km + Schrubs5km
                            , cavity.abund, mixture="P", K=80)
# - can't use Evergreen&Ag,
#+ can't use HighDev&OpenDev together
landscape30.cavity <- pcount(~ Wind + Sky
                             ~ Evergreen30km + HighDev30km + Protected30km
                             , cavity.abund, mixture="P", K=80)
#- can't use Protected&Ag together,
#- can't use Ag&HighDev together
#- can't use Evergreen&Ag together
#- can't use HighDev&OpenDev together
#- can't use Schrubs&OpenDev together
#+ can't use Grass&Ag together
#+ can't use Ag&OpenDev together
#+ can't use Water&Protected together
#+ can't use Schrubs&HighDev together
treatment.cavity <- pcount(~ Wind + Sky
                           ~ Treatment + Nthins
                           , cavity.abund, mixture ="P", K=80)
management.cavity <- pcount(~ Wind + Sky
                            ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide
                            , cavity.abund, mixture="P", K=80)
disturbance.cavity <- pcount(~ Wind + Sky
                             ~ TimeSinceB + TimeSinceT
                             , cavity.abund, mixture="P", K=80)
siteprod.cavity <- pcount(~ Wind + Sky ~ PISoils + NSoilTypes
                          , cavity.abund, mixture="P", K=80)    #FPSiteIndex
greenberg.cavity <- pcount(~ Wind + Sky ~ BA + Nsnags + Nburns
                           , cavity.abund, mixture="P", K=80)

fmsCN <- fitList(null.cavity, global.cavity, local.cavity, lh.cavity, landmetrics.cavity,
                 landscape500.cavity, landscape1.cavity, landscape5.cavity, landscape30.cavity,
                 treatment.cavity, management.cavity, disturbance.cavity,
                 siteprod.cavity, greenberg.cavity)
ms.cavity <- modSel(fmsCN)
ms.cavity
#ms.cavity@Full

siteprod.cavity
confint(siteprod.cavity, type="state",method="normal")

write.table(ms.cavity@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/Nest_cavity_top_models_ms.xls",sep="\t")

#quick test between some correlated
testHigh.cawr <- pcount(~Jdate ~ HighDev30km, cawr.abund, mixture="P", K=10)
testOpen.cawr <- pcount(~Jdate ~ OpenDev30km, cawr.abund, mixture="P", K=10)
testSchrubs.cawr <- pcount(~Jdate ~ Schrubs30km, cawr.abund, mixture="P", K=10)
fmsctest <- fitList(testHigh.cawr, testOpen.cawr, testSchrubs.cawr)
msT.cawr <- modSel(fmsctest)
msT.cawr


################### paste parboot stuff below when figured out ####
cavity.abund<- csvToUMF("Nesting_cavity_pcount.csv", long = FALSE, type = "unmarkedFramePCount")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#next best detection covariates () for cavity-nesters
#modelname+2



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##    GROUND NESTERS  n = 10  #
#covariates: 

cavity.abund<- csvToUMF("Nesting_cavity_pcount.csv", long = FALSE, type = "unmarkedFramePCount")
##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(cavity.abund)
str(cavity.abund)
#scale all observation covariates (covs of detection)
obsCovs(cavity.abund)= scale (obsCovs(cavity.abund))
#siteCovs(cavity.abund)= scale (siteCovs(cavity.abund))
#select particular site covariates to scale below
#(note: NOT ALL - not treatment, herbicide, or years ones)
sc <- siteCovs(cavity.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)]) #from 26 to 74 +landscape+soils
siteCovs(cavity.abund) <- sc

#test for NB or Poisson - most should use Poisson
testP.cavity <- pcount(~1 ~1, cavity.abund, mixture="P", K=50)
testNB.cavity <- pcount(~1 ~1, cavity.abund, mixture="NB", K=50)
fmsTEST <- fitList(testP.cavity, testNB.cavity)
msTEST.cavity <- modSel(fmsTEST)
msTEST.cavity
## P is best for cavity nesters group.

?pcount

#detection covariates first
det.null.cavity <- pcount(~1 ~1, cavity.abund, mixture="P", K=50)
det.weather.cavity <- pcount(~ Wind + Sky ~1, cavity.abund, mixture="P", K=50)
det.global.cavity <- pcount(~ Jdate + Wind + Sky + Noise +Time ~1, cavity.abund, mixture="P", K=50)
det.sound.cavity <- pcount(~ Noise + Wind ~1, cavity.abund, mixture="P", K=50)
det.date.cavity <- pcount(~ Jdate ~1, cavity.abund, mixture="P", K=50)
det.detect.cavity <- pcount(~ Jdate + Noise + Time ~1, cavity.abund, mixture="P", K=50)
det.notdate.cavity <-pcount(~ Wind + Sky + Noise ~1, cavity.abund, mixture="P", K=50)
det.time.cavity <-pcount(~ Time ~1, cavity.abund, mixture="P",K=50)
det.timing.cavity <-pcount(~ Time + Jdate ~1, cavity.abund, mixture="P", K=50)

fmsDC <- fitList(det.null.cavity, det.weather.cavity, det.global.cavity,
                 det.sound.cavity, det.date.cavity, det.detect.cavity, det.notdate.cavity,
                 det.time.cavity, det.timing.cavity)
msDC.cavity <- modSel(fmsDC)
msDC.cavity
#msDC.cavity@Full
#summary: weather, time, date, timing, global, null, notdate all under 2.0

det.weather.cavity
confint(det.weather.cavity, type="state",method="normal")
write.table(msDC.cavity@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/Nest_cavity_top_models_msDC.xls",sep="\t")


##site covariates next

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#null detection covariates (aka NOT using the knowledge gained above - NOT RUN)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

null.cavity <- pcount(~ Wind + Sky ~1, cavity.abund, mixture="P", K=80)
global.cavity <- pcount(~ Wind + Sky
                        ~ Treatment + Herbicide + BA + Nsnags +Ccover
                        + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Nthins
                        + HW_dens_1050 + FG_herb + FG_shrub + NHW_saplings + NP_over_20cm
                        + Rel_HW2P_canopy + PISoils + FPSiteIndex + NSoilTypes
                        + Parea + ShapeIndex
                        , cavity.abund, mixture="P", K=80) #FPSiteIndex
local.cavity <- pcount(~ Wind + Sky
                       ~ Ccover + TreeHt + Ldepth
                       , cavity.abund, mixture="P", K=80) #can only include BA OR CCover
lh.cavity <- pcount(~ Wind + Sky
                    ~ Age + TreeHt + BA + NP_over_20cm + Nsnags + Rel_HW2P_canopy
                    , cavity.abund, mixture="P", K=80)
#tree height, age, BA, big trees, snags, open space  #burns based on Greenberg paper!
landmetrics.cavity <- pcount (~ Wind + Sky
                              ~ Parea + ShapeIndex
                              , cavity.abund, mixture="P",K=80)
landscape500.cavity <- pcount(~ Wind + Sky
                              ~ Evergreen500m + HighDev500m + Schrubs500m
                              , cavity.abund, mixture="P", K=80)
landscape1.cavity <- pcount(~ Wind + Sky
                            ~ Evergreen1km + HighDev1km + Schrubs1km
                            , cavity.abund, mixture="P", K=80)
landscape5.cavity <- pcount(~ Wind + Sky
                            ~ Evergreen5km + HighDev5km + Schrubs5km
                            , cavity.abund, mixture="P", K=80)
# - can't use Evergreen&Ag,
#+ can't use HighDev&OpenDev together
landscape30.cavity <- pcount(~ Wind + Sky
                             ~ Evergreen30km + HighDev30km + Protected30km
                             , cavity.abund, mixture="P", K=80)
#- can't use Protected&Ag together,
#- can't use Ag&HighDev together
#- can't use Evergreen&Ag together
#- can't use HighDev&OpenDev together
#- can't use Schrubs&OpenDev together
#+ can't use Grass&Ag together
#+ can't use Ag&OpenDev together
#+ can't use Water&Protected together
#+ can't use Schrubs&HighDev together
treatment.cavity <- pcount(~ Wind + Sky
                           ~ Treatment + Nthins
                           , cavity.abund, mixture ="P", K=80)
management.cavity <- pcount(~ Wind + Sky
                            ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide
                            , cavity.abund, mixture="P", K=80)
disturbance.cavity <- pcount(~ Wind + Sky
                             ~ TimeSinceB + TimeSinceT
                             , cavity.abund, mixture="P", K=80)
siteprod.cavity <- pcount(~ Wind + Sky ~ PISoils + NSoilTypes
                          , cavity.abund, mixture="P", K=80)    #FPSiteIndex
greenberg.cavity <- pcount(~ Wind + Sky ~ BA + Nsnags + Nburns
                           , cavity.abund, mixture="P", K=80)

fmsCN <- fitList(null.cavity, global.cavity, local.cavity, lh.cavity, landmetrics.cavity,
                 landscape500.cavity, landscape1.cavity, landscape5.cavity, landscape30.cavity,
                 treatment.cavity, management.cavity, disturbance.cavity,
                 siteprod.cavity, greenberg.cavity)
ms.cavity <- modSel(fmsCN)
ms.cavity
#ms.cavity@Full

siteprod.cavity
confint(siteprod.cavity, type="state",method="normal")

write.table(ms.cavity@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/Nest_cavity_top_models_ms.xls",sep="\t")

#quick test between some correlated
testHigh.cawr <- pcount(~Jdate ~ HighDev30km, cawr.abund, mixture="P", K=10)
testOpen.cawr <- pcount(~Jdate ~ OpenDev30km, cawr.abund, mixture="P", K=10)
testSchrubs.cawr <- pcount(~Jdate ~ Schrubs30km, cawr.abund, mixture="P", K=10)
fmsctest <- fitList(testHigh.cawr, testOpen.cawr, testSchrubs.cawr)
msT.cawr <- modSel(fmsctest)
msT.cawr


################### paste parboot stuff below when figured out ####
cavity.abund<- csvToUMF("Nesting_cavity_pcount.csv", long = FALSE, type = "unmarkedFramePCount")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#next best detection covariates () for cavity-nesters
#modelname+2
