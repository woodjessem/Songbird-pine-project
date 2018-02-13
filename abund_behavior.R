# NESTING GUILD #
                 # bark forager   #foliage gleaner ~ 150   #ground forager ~ 300
library("unmarked")
setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")

## Bark forager    n=9 ##
# covariates: tree height, age, BA, big trees, snags, open space  #burns based on Greenberg paper!

bf.abund<- csvToUMF("Behavior_bf_pcount.csv", long = FALSE, type = "unmarkedFramePCount")
##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(bf.abund)
str(bf.abund)
#scale all observation covariates (covs of detection)
obsCovs(bf.abund)= scale (obsCovs(bf.abund))
#siteCovs(bf.abund)= scale (siteCovs(bf.abund))
#select particular site covariates to scale below
#(note: NOT ALL - not treatment, herbicide, or years ones)
sc <- siteCovs(bf.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)]) #from 26 to 74 +landscape+soils
siteCovs(bf.abund) <- sc

#test for NB or Poisson - most should use Poisson
testP.bf <- pcount(~1 ~1, bf.abund, mixture="P", K=50)
testNB.bf <- pcount(~1 ~1, bf.abund, mixture="NB", K=50)
fmsTEST <- fitList(testP.bf, testNB.bf)
msTEST.bf <- modSel(fmsTEST)
msTEST.bf
## P is best for bf behavior group.

?pcount

#detection covariates first
det.null.bf <- pcount(~1 ~1, bf.abund, mixture="P", K=50)
det.weather.bf <- pcount(~ Wind + Sky ~1, bf.abund, mixture="P", K=50)
det.global.bf <- pcount(~ Jdate + Wind + Sky + Noise +Time ~1, bf.abund, mixture="P", K=50)
det.sound.bf <- pcount(~ Noise + Wind ~1, bf.abund, mixture="P", K=50)
det.date.bf <- pcount(~ Jdate ~1, bf.abund, mixture="P", K=50)
det.detect.bf <- pcount(~ Jdate + Noise + Time ~1, bf.abund, mixture="P", K=50)
det.notdate.bf <-pcount(~ Wind + Sky + Noise ~1, bf.abund, mixture="P", K=50)
det.time.bf <-pcount(~ Time ~1, bf.abund, mixture="P",K=50)
det.timing.bf <-pcount(~ Time + Jdate ~1, bf.abund, mixture="P", K=50)

fmsDC <- fitList(det.null.bf, det.weather.bf, det.global.bf,
                 det.sound.bf, det.date.bf, det.detect.bf, det.notdate.bf,
                 det.time.bf, det.timing.bf)
msDC.bf <- modSel(fmsDC)
msDC.bf
#msDC.bf@Full
#summary: time+date best, date, global, detect all under 2.0

det.timing.bf
confint(det.timing.bf, type="state",method="normal")
write.table(msDC.bf@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/Behavior_bf_top_models_msDC.xls",sep="\t")


##site covariates next

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#null detection covariates (aka NOT using the knowledge gained above - NOT RUN)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#more appropriate detection covariates (Time+Date from best model)
null.bf <- pcount(~ Time + Jdate ~1, bf.abund, mixture="P", K=80)
global.bf <- pcount(~ Time + Jdate
                      ~ Treatment + Herbicide + BA + Nsnags +Ccover
                       + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Nthins
                       + NP_over_20cm
                       + Rel_HW2P_canopy + PISoils  + NSoilTypes
                       + Parea + ShapeIndex
                       , bf.abund, mixture="P", K=80) #+ FPSiteIndex
local.bf <- pcount(~ Time + Jdate
                     ~ Ccover + TreeHt + Ldepth
                     , bf.abund, mixture="P", K=80) #can only include BA OR CCover
lh.bf <- pcount(~ Time + Jdate
                  ~ Age + TreeHt + BA + NP_over_20cm + Nsnags + Rel_HW2P_canopy
                  , bf.abund, mixture="P", K=80)
#tree height, age, BA, big trees, snags, open space  #burns based on Greenberg paper!
#note: these are same as cavity-nesters right now
landmetrics.bf <- pcount (~ Time + Jdate
                          ~ Parea + ShapeIndex
                        , bf.abund, mixture="P",K=80)
landscape500.bf <- pcount(~ Time + Jdate
                        ~ Evergreen500m + HighDev500m + Schrubs500m
                            , bf.abund, mixture="P", K=80)
landscape1.bf <- pcount(~ Time + Jdate
                       ~ Evergreen1km + HighDev1km + Schrubs1km
                          , bf.abund, mixture="P", K=80)
landscape5.bf <- pcount(~ Time + Jdate
                       ~ Evergreen5km + HighDev5km + Schrubs5km
                          , bf.abund, mixture="P", K=80)
                  # - can't use Evergreen&Ag,
                  #+ can't use HighDev&OpenDev together
landscape30.bf <- pcount(~ Time + Jdate
                       ~ Evergreen30km + HighDev30km + Protected30km
                           , bf.abund, mixture="P", K=80)
                  #- can't use Protected&Ag together,
                  #- can't use Ag&HighDev together
                  #- can't use Evergreen&Ag together
                  #- can't use HighDev&OpenDev together
                  #- can't use Schrubs&OpenDev together
                  #+ can't use Grass&Ag together
                  #+ can't use Ag&OpenDev together
                  #+ can't use Water&Protected together
                  #+ can't use Schrubs&HighDev together
treatment.bf <- pcount(~ Time + Jdate
                         ~ Treatment + Nthins
                         , bf.abund, mixture ="P", K=80)
management.bf <- pcount(~ Time + Jdate
                          ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide
                          , bf.abund, mixture="P", K=80)
disturbance.bf <- pcount(~ Time + Jdate
                           ~ TimeSinceB + TimeSinceT
                           , bf.abund, mixture="P", K=80)
siteprod.bf <- pcount(~ Time + Jdate ~ PISoils + NSoilTypes
                        , bf.abund, mixture="P", K=80)    #FPSiteIndex
#greenberg.bf <- pcount(~ Time + Jdate ~ BA + Nsnags + Nburns, bf.abund, mixture="P", K=80)

fmsBF <- fitList(null.bf, global.bf, local.bf, lh.bf, landmetrics.bf,
               landscape500.bf, landscape1.bf, landscape5.bf, landscape30.bf,
               treatment.bf, management.bf, disturbance.bf,
                siteprod.bf)  #no greenberg
ms.bf <- modSel(fmsBF)
ms.bf
#ms.bf@Full
# null is best :/ and landscape30km is OVER 2.0

landscape30.bf
confint(landscape30.bf, type="state",method="normal")

write.table(ms.bf@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/Behavior_bf_top_models_ms.xls",sep="\t")

#quick test between some correlated
testHigh.cawr <- pcount(~Jdate ~ HighDev30km, cawr.abund, mixture="P", K=10)
testOpen.cawr <- pcount(~Jdate ~ OpenDev30km, cawr.abund, mixture="P", K=10)
testSchrubs.cawr <- pcount(~Jdate ~ Schrubs30km, cawr.abund, mixture="P", K=10)
fmsctest <- fitList(testHigh.cawr, testOpen.cawr, testSchrubs.cawr)
msT.cawr <- modSel(fmsctest)
msT.cawr


################### paste parboot stuff below when figured out ####
bf.abund<- csvToUMF("Behavior_bf_pcount.csv", long = FALSE, type = "unmarkedFramePCount")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#next best detection covariates () for bark-foragers



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Foliage gleaner n=20 ##
# covariates: tree height, age, BA, big trees, snags, open space  #burns based on Greenberg paper!

fg.abund<- csvToUMF("Behavior_fg_pcount.csv", long = FALSE, type = "unmarkedFramePCount")
##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(fg.abund)
str(fg.abund)
#scale all observation covariates (covs of detection)
obsCovs(fg.abund)= scale (obsCovs(fg.abund))
#siteCovs(fg.abund)= scale (siteCovs(fg.abund))
#select particular site covariates to scale below
#(note: NOT ALL - not treatment, herbicide, or years ones)
sc <- siteCovs(fg.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)]) #from 26 to 74 +landscape+soils
siteCovs(fg.abund) <- sc

#test for NB or Poisson - most should use Poisson
testP.fg <- pcount(~1 ~1, fg.abund, mixture="P", K=50)
testNB.fg <- pcount(~1 ~1, fg.abund, mixture="NB", K=50)
fmsTEST <- fitList(testP.fg, testNB.fg)
msTEST.fg <- modSel(fmsTEST)
msTEST.fg
## P is best for fg behavior group.

?pcount

#detection covariates first
det.null.fg <- pcount(~1 ~1, fg.abund, mixture="P", K=50)
det.weather.fg <- pcount(~ Wind + Sky ~1, fg.abund, mixture="P", K=50)
det.global.fg <- pcount(~ Jdate + Wind + Sky + Noise +Time ~1, fg.abund, mixture="P", K=50)
det.sound.fg <- pcount(~ Noise + Wind ~1, fg.abund, mixture="P", K=50)
det.date.fg <- pcount(~ Jdate ~1, fg.abund, mixture="P", K=50)
det.detect.fg <- pcount(~ Jdate + Noise + Time ~1, fg.abund, mixture="P", K=50)
det.notdate.fg <-pcount(~ Wind + Sky + Noise ~1, fg.abund, mixture="P", K=50)
det.time.fg <-pcount(~ Time ~1, fg.abund, mixture="P",K=50)
det.timing.fg <-pcount(~ Time + Jdate ~1, fg.abund, mixture="P", K=50)

fmsDC <- fitList(det.null.fg, det.weather.fg, det.global.fg,
                 det.sound.fg, det.date.fg, det.detect.fg, det.notdate.fg,
                 det.time.fg, det.timing.fg)
msDC.fg <- modSel(fmsDC)
msDC.fg
#msDC.fg@Full
#summary: notdate (Wind, Sky, Noise) best, weather, sound all under 2.0

det.notdate.fg
confint(det.notdate.fg, type="state",method="normal")
write.table(msDC.fg@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/Behavior_fg_top_models_msDC.xls",sep="\t")


##site covariates next

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#null detection covariates (aka NOT using the knowledge gained above - NOT RUN)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#more appropriate detection covariates (Wind+Sky+Noise from best model)
null.fg <- pcount(~ Wind + Sky + Noise ~1, fg.abund, mixture="P", K=80)
global.fg <- pcount(~ Wind + Sky + Noise
                    ~ Treatment + Herbicide + BA + Nsnags +Ccover
                    + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Nthins + Nburns
                    + HW_dens_1050 + NP_over_20cm
                    + Rel_HW2P_canopy + PISoils  + NSoilTypes
                    + Parea + ShapeIndex
                    , fg.abund, mixture="P", K=80) #+ FPSiteIndex
local.fg <- pcount(~ Wind + Sky + Noise
                   ~ Ccover + TreeHt + Ldepth
                   , fg.abund, mixture="P", K=80) #can only include BA OR CCover
lh.fg <- pcount(~ Wind + Sky + Noise
                ~ TreeHt + Ccover + Nsnags + Rel_HW2P_canopy + NHW_saplings + HW_dens_1050
                , fg.abund, mixture="P", K=80)
#tree height, ccover, NOT snags, hardwood ratio (tree diversity)
#note: similar to tree nesters
landmetrics.fg <- pcount (~ Wind + Sky + Noise
                          ~ Parea + ShapeIndex
                          , fg.abund, mixture="P",K=80)
landscape500.fg <- pcount(~ Wind + Sky + Noise
                          ~ Evergreen500m + OpenDev500m + Schrubs500m + Ag500m
                          , fg.abund, mixture="P", K=80)
landscape1.fg <- pcount(~ Wind + Sky + Noise
                        ~ Evergreen1km + OpenDev1km + Schrubs1km + Ag1km
                        , fg.abund, mixture="P", K=80)
landscape5.fg <- pcount(~ Wind + Sky + Noise
                        ~ Evergreen5km + OpenDev5km + Schrubs5km + Water1km
                        , fg.abund, mixture="P", K=80)
# - can't use Evergreen&Ag,
#+ can't use HighDev&OpenDev together
landscape30.fg <- pcount(~ Wind + Sky + Noise
                         ~ Evergreen30km + OpenDev30km + Protected30km
                         , fg.abund, mixture="P", K=80)
#- can't use Protected&Ag together,
#- can't use Ag&HighDev together
#- can't use Evergreen&Ag together
#- can't use HighDev&OpenDev together
#- can't use Schrubs&OpenDev together
#+ can't use Grass&Ag together
#+ can't use Ag&OpenDev together
#+ can't use Water&Protected together
#+ can't use Schrubs&HighDev together
treatment.fg <- pcount(~ Wind + Sky + Noise
                       ~ Treatment + Nthins
                       , fg.abund, mixture ="P", K=80)
management.fg <- pcount(~ Wind + Sky + Noise
                        ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide
                        , fg.abund, mixture="P", K=80)
disturbance.fg <- pcount(~ Wind + Sky + Noise
                         ~ TimeSinceB + TimeSinceT
                         , fg.abund, mixture="P", K=80)
siteprod.fg <- pcount(~ Wind + Sky + Noise ~ PISoils + NSoilTypes
                      , fg.abund, mixture="P", K=80)    #FPSiteIndex
#greenberg.fg <- pcount(~ Wind + Sky + Noise ~ BA + Nsnags + Nburns, fg.abund, mixture="P", K=80)

fmsFG <- fitList(null.fg, global.fg, local.fg, lh.fg, landmetrics.fg,
                 landscape500.fg, landscape1.fg, landscape5.fg, landscape30.fg,
                 treatment.fg, management.fg, disturbance.fg,
                 siteprod.fg)  #no greenberg
ms.fg <- modSel(fmsFG)
ms.fg
#ms.fg@Full
# landscape @ 500m is best and disturbance is well OVER 2.0

landscape500.fg
confint(landscape500.fg, type="state",method="normal")

write.table(ms.fg@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/Behavior_fg_top_models_ms.xls",sep="\t")

#quick test between some correlated
testHigh.cawr <- pcount(~Jdate ~ HighDev30km, cawr.abund, mixture="P", K=10)
testOpen.cawr <- pcount(~Jdate ~ OpenDev30km, cawr.abund, mixture="P", K=10)
testSchrubs.cawr <- pcount(~Jdate ~ Schrubs30km, cawr.abund, mixture="P", K=10)
fmsctest <- fitList(testHigh.cawr, testOpen.cawr, testSchrubs.cawr)
msT.cawr <- modSel(fmsctest)
msT.cawr


################### paste parboot stuff below when figured out ####
fg.abund<- csvToUMF("Behavior_fg_pcount.csv", long = FALSE, type = "unmarkedFramePCount")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#next best detection covariates () for bark-foragers


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## GROUND FORAGERS    n=27 ##
#covariates: midstory, HW saplings, BA, greenberg - Nburns, - tree density, + shrub stem density

gf.abund<- csvToUMF("Behavior_gf_pcount.csv", long = FALSE, type = "unmarkedFramePCount")
##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(gf.abund)
str(gf.abund)
#scale all observation covariates (covs of detection)
obsCovs(gf.abund)= scale (obsCovs(gf.abund))
#siteCovs(gf.abund)= scale (siteCovs(gf.abund))
#select particular site covariates to scale below
#(note: NOT ALL - not treatment, herbicide, or years ones)
sc <- siteCovs(gf.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)]) #from 26 to 74 +landscape+soils
siteCovs(gf.abund) <- sc

#test for NB or Poisson - most should use Poisson
testP.gf <- pcount(~1 ~1, gf.abund, mixture="P", K=50)
testNB.gf <- pcount(~1 ~1, gf.abund, mixture="NB", K=50)
fmsTEST <- fitList(testP.gf, testNB.gf)
msTEST.gf <- modSel(fmsTEST)
msTEST.gf
## P is best for gf behavior group.

?pcount

#detection covariates first
det.null.gf <- pcount(~1 ~1, gf.abund, mixture="P", K=50)
det.weather.gf <- pcount(~ Wind + Sky ~1, gf.abund, mixture="P", K=50)
det.global.gf <- pcount(~ Jdate + Wind + Sky + Noise +Time ~1, gf.abund, mixture="P", K=50)
det.sound.gf <- pcount(~ Noise + Wind ~1, gf.abund, mixture="P", K=50)
det.date.gf <- pcount(~ Jdate ~1, gf.abund, mixture="P", K=50)
det.detect.gf <- pcount(~ Jdate + Noise + Time ~1, gf.abund, mixture="P", K=50)
det.notdate.gf <-pcount(~ Wind + Sky + Noise ~1, gf.abund, mixture="P", K=50)
det.time.gf <-pcount(~ Time ~1, gf.abund, mixture="P",K=50)
det.timing.gf <-pcount(~ Time + Jdate ~1, gf.abund, mixture="P", K=50)

fmsDC <- fitList(det.null.gf, det.weather.gf, det.global.gf,
                 det.sound.gf, det.date.gf, det.detect.gf, det.notdate.gf,
                 det.time.gf, det.timing.gf)
msDC.gf <- modSel(fmsDC)
msDC.gf
#msDC.gf@Full
#summary: date best, but timing is also right around 2.0

det.date.gf
confint(det.date.gf, type="state",method="normal")
write.table(msDC.gf@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/Behavior_gf_top_models_msDC.xls",sep="\t")


##site covariates next

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#null detection covariates (aka NOT using the knowledge gained above - NOT RUN)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#more appropriate detection covariates (only Date from best model)
null.gf <- pcount(~ Jdate ~1, gf.abund, mixture="P", K=80)
global.gf <- pcount(~ Jdate
                    ~ Treatment + Herbicide + BA + Nsnags +Ccover
                    + Ldepth + TreeHt + TimeSinceB + TimeSinceT + Nthins
                    + HW_dens_1050 + NP_over_20cm + FG_herb + FG_shrub
                    + Rel_HW2P_canopy + PISoils  + NSoilTypes
                    + Parea + ShapeIndex
                    , gf.abund, mixture="P", K=80) #+ FPSiteIndex
local.gf <- pcount(~ Jdate
                   ~ Ccover + TreeHt + Ldepth
                   , gf.abund, mixture="P", K=80) #can only include BA OR CCover
lh.gf <- pcount(~ Jdate
                ~ FG_herb + FG_shrub + HW_dens_1050 + Ldepth + Rel_HW2P_canopy + BA + NHW_saplings
                , gf.abund, mixture="P", K=80)
#note: these are similar to ground NESTERS now (kinda to shrubs too)
#covariates: forbes & grasses at 2 low heights, HW_dens_1050, leaf litter depth,
#  Greenberg: -Nburns, -TimeSinceB, leaf litter depth, - Nsnags
landmetrics.gf <- pcount (~ Jdate
                          ~ Parea + ShapeIndex
                          , gf.abund, mixture="P",K=80)
landscape500.gf <- pcount(~ Jdate
                          ~ Evergreen500m + HighDev500m + Schrubs500m + Ag500m
                          , gf.abund, mixture="P", K=80)
landscape1.gf <- pcount(~ Jdate
                        ~ Evergreen1km + HighDev1km + Schrubs1km + Ag1km
                        , gf.abund, mixture="P", K=80)
landscape5.gf <- pcount(~ Jdate
                        ~ Evergreen5km + HighDev5km + Schrubs5km
                        , gf.abund, mixture="P", K=80)
# - can't use Evergreen&Ag,
#+ can't use HighDev&OpenDev together
landscape30.gf <- pcount(~ Jdate
                         ~ Evergreen30km + HighDev30km + Protected30km
                         , gf.abund, mixture="P", K=80)
#- can't use Protected&Ag together,
#- can't use Ag&HighDev together
#- can't use Evergreen&Ag together
#- can't use HighDev&OpenDev together
#- can't use Schrubs&OpenDev together
#+ can't use Grass&Ag together
#+ can't use Ag&OpenDev together
#+ can't use Water&Protected together
#+ can't use Schrubs&HighDev together
treatment.gf <- pcount(~ Jdate
                       ~ Treatment + Nthins
                       , gf.abund, mixture ="P", K=80)
management.gf <- pcount(~ Jdate
                        ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide
                        , gf.abund, mixture="P", K=80)
disturbance.gf <- pcount(~ Jdate
                         ~ TimeSinceB + TimeSinceT
                         , gf.abund, mixture="P", K=80)
siteprod.gf <- pcount(~ Jdate ~ PISoils + NSoilTypes
                      , gf.abund, mixture="P", K=80)    #FPSiteIndex
#greenberg.gf <- pcount(~ Jdate ~ BA + Nsnags + Nburns, gf.abund, mixture="P", K=80)

fmsGF <- fitList(null.gf, global.gf, local.gf, lh.gf, landmetrics.gf,
                 landscape500.gf, landscape1.gf, landscape5.gf, landscape30.gf,
                 treatment.gf, management.gf, disturbance.gf,
                 siteprod.gf)  #no greenberg
ms.gf <- modSel(fmsGF)
ms.gf
#ms.gf@Full
# local model is only top model

local.gf
confint(local.gf, type="state",method="normal")

write.table(ms.gf@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/Behavior_gf_top_models_ms.xls",sep="\t")

#quick test between some correlated
testHigh.cawr <- pcount(~Jdate ~ HighDev30km, cawr.abund, mixture="P", K=10)
testOpen.cawr <- pcount(~Jdate ~ OpenDev30km, cawr.abund, mixture="P", K=10)
testSchrubs.cawr <- pcount(~Jdate ~ Schrubs30km, cawr.abund, mixture="P", K=10)
fmsctest <- fitList(testHigh.cawr, testOpen.cawr, testSchrubs.cawr)
msT.cawr <- modSel(fmsctest)
msT.cawr


################### paste parboot stuff below when figured out ####
gf.abund<- csvToUMF("Behavior_gf_pcount.csv", long = FALSE, type = "unmarkedFramePCount")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#next best detection covariates () for ground-foragers


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
