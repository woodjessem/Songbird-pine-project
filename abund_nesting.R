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
#covariates: tree ht, age, density?, canopy? idk else Greenberg: shrubs stem density, maybe burns

tree.abund<- csvToUMF("Nesting_tree_pcount.csv", long = FALSE, type = "unmarkedFramePCount")
##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(tree.abund)
str(tree.abund)
#scale all observation covariates (covs of detection)
obsCovs(tree.abund)= scale (obsCovs(tree.abund))
#siteCovs(tree.abund)= scale (siteCovs(tree.abund))
#select particular site covariates to scale below
#(note: NOT ALL - not treatment, herbicide, or years ones)
sc <- siteCovs(tree.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)]) #from 26 to 74 +landscape+soils
siteCovs(tree.abund) <- sc

#test for NB or Poisson - most should use Poisson
testP.tree <- pcount(~1 ~1, tree.abund, mixture="P", K=50)
testNB.tree <- pcount(~1 ~1, tree.abund, mixture="NB", K=50)
fmsTEST <- fitList(testP.tree, testNB.tree)
msTEST.tree <- modSel(fmsTEST)
msTEST.tree
## P is best for tree nesters group.

?pcount

#detection covariates first
det.null.tree <- pcount(~1 ~1, tree.abund, mixture="P", K=50)
det.weather.tree <- pcount(~ Wind + Sky ~1, tree.abund, mixture="P", K=50)
det.global.tree <- pcount(~ Jdate + Wind + Sky + Noise +Time ~1, tree.abund, mixture="P", K=50)
det.sound.tree <- pcount(~ Noise + Wind ~1, tree.abund, mixture="P", K=50)
det.date.tree <- pcount(~ Jdate ~1, tree.abund, mixture="P", K=50)
det.detect.tree <- pcount(~ Jdate + Noise + Time ~1, tree.abund, mixture="P", K=50)
det.notdate.tree <-pcount(~ Wind + Sky + Noise ~1, tree.abund, mixture="P", K=50)
det.time.tree <-pcount(~ Time ~1, tree.abund, mixture="P",K=50)
det.timing.tree <-pcount(~ Time + Jdate ~1, tree.abund, mixture="P", K=50)

fmsDC <- fitList(det.null.tree, det.weather.tree, det.global.tree,
                 det.sound.tree, det.date.tree, det.detect.tree, det.notdate.tree,
                 det.time.tree, det.timing.tree)
msDC.tree <- modSel(fmsDC)
msDC.tree
#msDC.tree@Full
#summary: null, sound, date, weather, time under 2.0

det.sound.tree
confint(det.sound.tree, type="state",method="normal")
write.table(msDC.tree@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/Nest_tree_top_models_msDC.xls",sep="\t")


##site covariates next

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#null detection covariates (best model, technically - NOT RUN YET)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   #sound model was second best, run here:
null.tree <- pcount(~ Wind + Noise ~1, tree.abund, mixture="P", K=80)
global.tree <- pcount(~ Wind + Noise
                        ~ Treatment + Herbicide + BA +Ccover
                        + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Nthins
                        + HW_dens_1050 + NHW_saplings
                        + Rel_HW2P_canopy + PISoils + NSoilTypes
                        + Parea + ShapeIndex
                        , tree.abund, mixture="P", K=80) #FPSiteIndex, middle row, snags
local.tree <- pcount(~ Wind + Noise
                       ~ Ccover + TreeHt + Ldepth
                       , tree.abund, mixture="P", K=80) #can only include BA OR CCover
lh.tree <- pcount(~ Wind + Noise
                    ~ Age + TreeHt + BA + NHW_saplings + Rel_HW2P_canopy
                    , tree.abund, mixture="P", K=80)
#covariates: tree ht, age, density?, canopy? idk else Greenberg: shrubs stem density, maybe burns
landmetrics.tree <- pcount (~ Wind + Noise
                              ~ Parea + ShapeIndex
                              , tree.abund, mixture="P",K=80)
landscape500.tree <- pcount(~ Wind + Noise
                              ~ Evergreen500m + HighDev500m + Schrubs500m + OpenDev500m
                              , tree.abund, mixture="P", K=80)
landscape1.tree <- pcount(~ Wind + Noise
                            ~ Evergreen1km + HighDev1km + Schrubs1km + OpenDev1km
                            , tree.abund, mixture="P", K=80)
landscape5.tree <- pcount(~ Wind + Noise
                            ~ Evergreen5km + HighDev5km + Schrubs5km
                            , tree.abund, mixture="P", K=80)
# - can't use Evergreen&Ag,
#+ can't use HighDev&OpenDev together
landscape30.tree <- pcount(~ Wind + Noise
                             ~ Evergreen30km + HighDev30km + Protected30km
                             , tree.abund, mixture="P", K=80)
#- can't use Protected&Ag together,
#- can't use Ag&HighDev together
#- can't use Evergreen&Ag together
#- can't use HighDev&OpenDev together
#- can't use Schrubs&OpenDev together
#+ can't use Grass&Ag together
#+ can't use Ag&OpenDev together
#+ can't use Water&Protected together
#+ can't use Schrubs&HighDev together
treatment.tree <- pcount(~ Wind + Noise
                           ~ Treatment + Nthins
                           , tree.abund, mixture ="P", K=80)
management.tree <- pcount(~ Wind + Noise
                            ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide
                            , tree.abund, mixture="P", K=80)
disturbance.tree <- pcount(~ Wind + Noise
                             ~ TimeSinceB + TimeSinceT
                             , tree.abund, mixture="P", K=80)
siteprod.tree <- pcount(~ Wind + Noise ~ PISoils + NSoilTypes
                          , tree.abund, mixture="P", K=80)    #FPSiteIndex
greenberg.tree <- pcount(~ Wind + Noise ~ Ccover + NHW_saplings + HW_dens_1050 + Nburns
                           , tree.abund, mixture="P", K=80)

fmsTN <- fitList(null.tree, global.tree, local.tree, lh.tree, landmetrics.tree,
                 landscape500.tree, landscape1.tree, landscape5.tree, landscape30.tree,
                 treatment.tree, management.tree, disturbance.tree,
                 siteprod.tree, greenberg.tree)
ms.tree <- modSel(fmsTN)
ms.tree
#ms.tree@Full

landmetrics.tree
confint(landmetrics.tree, type="state",method="normal")
write.table(ms.tree@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/Nest_tree_top_models_ms.xls",sep="\t")

#quick test between some correlated
testHigh.cawr <- pcount(~Jdate ~ HighDev30km, cawr.abund, mixture="P", K=10)
testOpen.cawr <- pcount(~Jdate ~ OpenDev30km, cawr.abund, mixture="P", K=10)
testSchrubs.cawr <- pcount(~Jdate ~ Schrubs30km, cawr.abund, mixture="P", K=10)
fmsctest <- fitList(testHigh.cawr, testOpen.cawr, testSchrubs.cawr)
msT.cawr <- modSel(fmsctest)
msT.cawr


################### paste parboot stuff below when figured out ####
tree.abund<- csvToUMF("Nesting_tree_pcount.csv", long = FALSE, type = "unmarkedFramePCount")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#next best detection covariates () for tree-nesters
#modelname+2


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##    SHRUB NESTERS  n = 16  #
#covariates: midstory, HW saplings, BA, greenberg - Nburns, - tree density, + shrub stem density

shrub.abund<- csvToUMF("Nesting_shrub_pcount.csv", long = FALSE, type = "unmarkedFramePCount")
##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(shrub.abund)
str(shrub.abund)
#scale all observation covariates (covs of detection)
obsCovs(shrub.abund)= scale (obsCovs(shrub.abund))
#siteCovs(shrub.abund)= scale (siteCovs(shrub.abund))
#select particular site covariates to scale below
#(note: NOT ALL - not treatment, herbicide, or years ones)
sc <- siteCovs(shrub.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)]) #from 26 to 74 +landscape+soils
siteCovs(shrub.abund) <- sc

#test for NB or Poisson - most should use Poisson
testP.shrub <- pcount(~1 ~1, shrub.abund, mixture="P", K=50)
testNB.shrub <- pcount(~1 ~1, shrub.abund, mixture="NB", K=50)
fmsTEST <- fitList(testP.shrub, testNB.shrub)
msTEST.shrub <- modSel(fmsTEST)
msTEST.shrub
## P is best for shrub nesters group.

?pcount

#detection covariates first
det.null.shrub <- pcount(~1 ~1, shrub.abund, mixture="P", K=50)
det.weather.shrub <- pcount(~ Wind + Sky ~1, shrub.abund, mixture="P", K=50)
det.global.shrub <- pcount(~ Jdate + Wind + Sky + Noise +Time ~1, shrub.abund, mixture="P", K=50)
det.sound.shrub <- pcount(~ Noise + Wind ~1, shrub.abund, mixture="P", K=50)
det.date.shrub <- pcount(~ Jdate ~1, shrub.abund, mixture="P", K=50)
det.detect.shrub <- pcount(~ Jdate + Noise + Time ~1, shrub.abund, mixture="P", K=50)
det.notdate.shrub <-pcount(~ Wind + Sky + Noise ~1, shrub.abund, mixture="P", K=50)
det.time.shrub <-pcount(~ Time ~1, shrub.abund, mixture="P",K=50)
det.timing.shrub <-pcount(~ Time + Jdate ~1, shrub.abund, mixture="P", K=50)

fmsDC <- fitList(det.null.shrub, det.weather.shrub, det.global.shrub,
                 det.sound.shrub, det.date.shrub, det.detect.shrub, det.notdate.shrub,
                 det.time.shrub, det.timing.shrub)
msDC.shrub <- modSel(fmsDC)
msDC.shrub
#msDC.shrub@Full
#summary: detection is best model and only under 2.0

det.detect.shrub
confint(det.detect.shrub, type="state",method="normal")
write.table(msDC.shrub@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/Nest_shrub_top_models_msDC.xls",sep="\t")


##site covariates next

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#null detection covariates (aka NOT using the knowledge gained above - NOT RUN)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

null.shrub <- pcount(~ Jdate + Noise + Time ~1, shrub.abund, mixture="P", K=80)
global.shrub <- pcount(~ Jdate + Noise + Time 
                        ~ Treatment + Herbicide + BA + Ccover
                        + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Nthins + Nburns
                        + HW_dens_1050 + FG_shrub + NHW_saplings
                        + PISoils + NSoilTypes
                        + Parea + ShapeIndex
                        , shrub.abund, mixture="P", K=80) #FPSiteIndex
local.shrub <- pcount(~ Jdate + Noise + Time 
                       ~ Ccover + TreeHt + Ldepth
                       , shrub.abund, mixture="P", K=80) #can only include BA OR CCover
lh.shrub <- pcount(~ Jdate + Noise + Time 
                    ~ BA + NHW_saplings + FG_shrub + HW_dens_1050
                    , shrub.abund, mixture="P", K=80)
#covariates: midstory, HW saplings, BA, greenberg - Nburns, - tree density, + shrub stem density
landmetrics.shrub <- pcount (~ Jdate + Noise + Time 
                              ~ Parea + ShapeIndex
                              , shrub.abund, mixture="P",K=80)
landscape500.shrub <- pcount(~ Jdate + Noise + Time 
                              ~ Evergreen500m + HighDev500m + OpenDev500m+ + Schrubs500m + Ag500m
                              , shrub.abund, mixture="P", K=80)
landscape1.shrub <- pcount(~ Jdate + Noise + Time 
                            ~ Evergreen1km + HighDev1km + OpenDev1km + Schrubs1km + Ag1km
                            , shrub.abund, mixture="P", K=80)
landscape5.shrub <- pcount(~ Jdate + Noise + Time 
                            ~ OpenDev5km + Schrubs5km + Ag5km
                            , shrub.abund, mixture="P", K=80)
# - can't use Evergreen&Ag,
#+ can't use HighDev&OpenDev together
landscape30.shrub <- pcount(~ Jdate + Noise + Time 
                             ~ Schrubs30km + Evergreen30km + Protected30km
                             , shrub.abund, mixture="P", K=80)
#- can't use Protected&Ag together,
#- can't use Ag&HighDev together
#- can't use Evergreen&Ag together
#- can't use HighDev&OpenDev together
#- can't use Schrubs&OpenDev together
#+ can't use Grass&Ag together
#+ can't use Ag&OpenDev together
#+ can't use Water&Protected together
#+ can't use Schrubs&HighDev together
treatment.shrub <- pcount(~ Jdate + Noise + Time 
                           ~ Treatment + Nthins
                           , shrub.abund, mixture ="P", K=80)
management.shrub <- pcount(~ Jdate + Noise + Time 
                            ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide
                            , shrub.abund, mixture="P", K=80)
disturbance.shrub <- pcount(~ Jdate + Noise + Time 
                             ~ TimeSinceB + TimeSinceT
                             , shrub.abund, mixture="P", K=80)
siteprod.shrub <- pcount(~ Jdate + Noise + Time ~ PISoils + NSoilTypes
                          , shrub.abund, mixture="P", K=80)    #FPSiteIndex
greenberg.shrub <- pcount(~ Jdate + Noise + Time ~ BA + HW_dens_1050 + Nburns
                           , shrub.abund, mixture="P", K=80)

fmsSN <- fitList(null.shrub, global.shrub, local.shrub, lh.shrub, landmetrics.shrub,
                 landscape500.shrub, landscape1.shrub, landscape5.shrub, landscape30.shrub,
                 treatment.shrub, management.shrub, disturbance.shrub,
                 siteprod.shrub, greenberg.shrub)
ms.shrub <- modSel(fmsSN)
ms.shrub
#ms.shrub@Full

greenberg.shrub
confint(greenberg.shrub, type="state",method="normal")

lh.shrub
confint(lh.shrub, type="state",method="normal")

write.table(ms.shrub@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/Nest_shrub_top_models_ms.xls",sep="\t")

#quick test between some correlated
testHigh.cawr <- pcount(~Jdate ~ HighDev30km, cawr.abund, mixture="P", K=10)
testOpen.cawr <- pcount(~Jdate ~ OpenDev30km, cawr.abund, mixture="P", K=10)
testSchrubs.cawr <- pcount(~Jdate ~ Schrubs30km, cawr.abund, mixture="P", K=10)
fmsctest <- fitList(testHigh.cawr, testOpen.cawr, testSchrubs.cawr)
msT.cawr <- modSel(fmsctest)
msT.cawr


################### paste parboot stuff below when figured out ####
shrub.abund<- csvToUMF("Nesting_shrub_pcount.csv", long = FALSE, type = "unmarkedFramePCount")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#next best detection covariates () for shrub-nesters
#modelname+2



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##    GROUND NESTERS  n = 10  #
#covariates: forbes & grasses at 2 low heights, HW_dens_1050, leaf litter depth,
#  Greenberg: -Nburns, -TimeSinceB, leaf litter depth, - Nsnags

ground.abund<- csvToUMF("Nesting_ground_pcount.csv", long = FALSE, type = "unmarkedFramePCount")
##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(ground.abund)
str(ground.abund)
#scale all observation covariates (covs of detection)
obsCovs(ground.abund)= scale (obsCovs(ground.abund))
#siteCovs(ground.abund)= scale (siteCovs(ground.abund))
#select particular site covariates to scale below
#(note: NOT ALL - not treatment, herbicide, or years ones)
sc <- siteCovs(ground.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)]) #from 26 to 74 +landscape+soils
siteCovs(ground.abund) <- sc

#test for NB or Poisson - most should use Poisson
testP.ground <- pcount(~1 ~1, ground.abund, mixture="P", K=50)
testNB.ground <- pcount(~1 ~1, ground.abund, mixture="NB", K=50)
fmsTEST <- fitList(testP.ground, testNB.ground)
msTEST.ground <- modSel(fmsTEST)
msTEST.ground
## P is best for ground nesters group.

?pcount

#detection covariates first
det.null.ground <- pcount(~1 ~1, ground.abund, mixture="P", K=50)
det.weather.ground <- pcount(~ Wind + Sky ~1, ground.abund, mixture="P", K=50)
det.global.ground <- pcount(~ Jdate + Wind + Sky + Noise +Time ~1, ground.abund, mixture="P", K=50)
det.sound.ground <- pcount(~ Noise + Wind ~1, ground.abund, mixture="P", K=50)
det.date.ground <- pcount(~ Jdate ~1, ground.abund, mixture="P", K=50)
det.detect.ground <- pcount(~ Jdate + Noise + Time ~1, ground.abund, mixture="P", K=50)
det.notdate.ground <-pcount(~ Wind + Sky + Noise ~1, ground.abund, mixture="P", K=50)
det.time.ground <-pcount(~ Time ~1, ground.abund, mixture="P",K=50)
det.timing.ground <-pcount(~ Time + Jdate ~1, ground.abund, mixture="P", K=50)

fmsDC <- fitList(det.null.ground, det.weather.ground, det.global.ground,
                 det.sound.ground, det.date.ground, det.detect.ground, det.notdate.ground,
                 det.time.ground, det.timing.ground)
msDC.ground <- modSel(fmsDC)
msDC.ground
#msDC.ground@Full
#summary: date and then timing (date+time) under 2.0

det.date.ground
confint(det.date.ground, type="state",method="normal")
write.table(msDC.ground@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/Nest_ground_top_models_msDC.xls",sep="\t")

##site covariates next

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#null detection covariates (aka NOT using the knowledge gained above - NOT RUN)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

null.ground <- pcount(~ Jdate ~1, ground.abund, mixture="P", K=80)
global.ground <- pcount(~ Jdate
                        ~ Treatment + Herbicide + BA + Nsnags +Ccover
                        + Ldepth + TreeHt + TimeSinceB + TimeSinceT + Nthins + Nburns
                        + HW_dens_1050 + FG_herb + FG_shrub
                        + Rel_HW2P_canopy + PISoils + NSoilTypes
                        + Parea + ShapeIndex
                        , ground.abund, mixture="P", K=80) #FPSiteIndex
local.ground <- pcount(~ Jdate
                       ~ Ccover + TreeHt + Ldepth
                       , ground.abund, mixture="P", K=80) #can only include BA OR CCover
lh.ground <- pcount(~ Jdate
                    ~ FG_herb + FG_shrub + HW_dens_1050 + Ldepth + Rel_HW2P_canopy + BA
                    , ground.abund, mixture="P", K=80)
#covariates: forbes & grasses at 2 low heights, HW_dens_1050, leaf litter depth,
landmetrics.ground <- pcount (~ Jdate
                              ~ Parea + ShapeIndex
                              , ground.abund, mixture="P",K=80)
landscape500.ground <- pcount(~ Jdate
                              ~ Evergreen500m + HighDev500m + Schrubs500m + OpenDev500m
                              , ground.abund, mixture="P", K=80)
landscape1.ground <- pcount(~ Jdate
                            ~ Evergreen1km + HighDev1km + Schrubs1km + OpenDev1km
                            , ground.abund, mixture="P", K=80)
landscape5.ground <- pcount(~ Jdate
                            ~ Evergreen5km + HighDev5km + Schrubs5km
                            , ground.abund, mixture="P", K=80)
# - can't use Evergreen&Ag,
#+ can't use HighDev&OpenDev together
landscape30.ground <- pcount(~ Jdate
                             ~ Evergreen30km + HighDev30km + Protected30km
                             , ground.abund, mixture="P", K=80)
#- can't use Protected&Ag together,
#- can't use Ag&HighDev together
#- can't use Evergreen&Ag together
#- can't use HighDev&OpenDev together
#- can't use Schrubs&OpenDev together
#+ can't use Grass&Ag together
#+ can't use Ag&OpenDev together
#+ can't use Water&Protected together
#+ can't use Schrubs&HighDev together
treatment.ground <- pcount(~ Jdate
                           ~ Treatment + Nthins
                           , ground.abund, mixture ="P", K=80)
management.ground <- pcount(~ Jdate
                            ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide
                            , ground.abund, mixture="P", K=80)
disturbance.ground <- pcount(~ Jdate
                             ~ TimeSinceB + TimeSinceT
                             , ground.abund, mixture="P", K=80)
siteprod.ground <- pcount(~ Jdate ~ PISoils + NSoilTypes
                          , ground.abund, mixture="P", K=80)    #FPSiteIndex
greenberg.ground <- pcount(~ Jdate ~ Ccover + Nsnags + Nburns + TimeSinceB + Ldepth
                           , ground.abund, mixture="P", K=80)
#  Greenberg: -Nburns, -TimeSinceB, leaf litter depth, - Nsnags

fmsGN <- fitList(null.ground, global.ground, local.ground, lh.ground, landmetrics.ground,
                 landscape500.ground, landscape1.ground, landscape5.ground, landscape30.ground,
                 treatment.ground, management.ground, disturbance.ground,
                 siteprod.ground, greenberg.ground)
ms.ground <- modSel(fmsGN)
ms.ground
#ms.ground@Full

local.ground
confint(local.ground, type="state",method="normal")

landscape1.ground
confint(landscape1.ground, type="state",method="normal")

landmetrics.ground
confint(landmetrics.ground, type="state",method="normal")

greenberg.ground
confint(greenberg.ground, type="state",method="normal")

write.table(ms.ground@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/Nest_ground_top_models_ms.xls",sep="\t")

#quick test between some correlated
testHigh.cawr <- pcount(~Jdate ~ HighDev30km, cawr.abund, mixture="P", K=10)
testOpen.cawr <- pcount(~Jdate ~ OpenDev30km, cawr.abund, mixture="P", K=10)
testSchrubs.cawr <- pcount(~Jdate ~ Schrubs30km, cawr.abund, mixture="P", K=10)
fmsctest <- fitList(testHigh.cawr, testOpen.cawr, testSchrubs.cawr)
msT.cawr <- modSel(fmsctest)
msT.cawr


################### paste parboot stuff below when figured out ####
ground.abund<- csvToUMF("Nesting_ground_pcount.csv", long = FALSE, type = "unmarkedFramePCount")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#next best detection covariates () for ground-nesters
#modelname+2
