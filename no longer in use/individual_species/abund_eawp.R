# EAWP (forests (prefer deciduious or more edgy?), tree-nester, flycatching, insectivore) 
# covariates: less canopy cover? less dense stand? hardwoody?
library("unmarked")
setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")

test <-read.csv("eawp_abund.csv") #spp CSV file!
summary(test)
str(test)
var(test[2:5])  #what were these testing for? variance>mean?
#mean(test[2:4])
mean(test$y.3)

eawp.abund<- csvToUMF("eawp_abund.csv", long = FALSE, type = "unmarkedFramePCount")
##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(eawp.abund)
str(eawp.abund)
#scale all observation covariates (covs of detection)
obsCovs(eawp.abund)= scale (obsCovs(eawp.abund))
#siteCovs(eawp.abund)= scale (siteCovs(eawp.abund))
#select particular site covariates to scale below
#(note: NOT ALL - not treatment, herbicide, or years ones)
sc <- siteCovs(eawp.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)]) #from 26 to 74 +landscape+soils
siteCovs(eawp.abund) <- sc

#test for NB or Poisson - most should use Poisson
testP.eawp <- pcount(~1 ~1, eawp.abund, mixture="P", K=4)
testNB.eawp <- pcount(~1 ~1, eawp.abund, mixture="NB", K=4)
fmsTEST <- fitList(testP.eawp, testNB.eawp)
msTEST.eawp <- modSel(fmsTEST)
msTEST.eawp
## P is best for this species.

?pcount

#detection covariates first
det.null.eawp <- pcount(~1 ~1, eawp.abund, mixture="P", K=15)
det.weather.eawp <- pcount(~ Wind + Sky ~1, eawp.abund, mixture="P", K=15)
det.global.eawp <- pcount(~ Jdate + Wind + Sky + Noise +Time ~1, eawp.abund, mixture="P", K=15)
det.sound.eawp <- pcount(~ Noise + Wind ~1, eawp.abund, mixture="P", K=15)
det.date.eawp <- pcount(~ Jdate ~1, eawp.abund, mixture="P", K=15)
det.detect.eawp <- pcount(~ Jdate + Noise + Time ~1, eawp.abund, mixture="P", K=15)
det.notdate.eawp <-pcount(~ Wind + Sky + Noise ~1, eawp.abund, mixture="P", K=15)
det.time.eawp <-pcount(~ Time ~1, eawp.abund, mixture="P",K=15)
det.timing.eawp <-pcount(~ Time + Jdate ~1, eawp.abund, mixture="P", K=15)

fmsDC <- fitList(det.null.eawp, det.weather.eawp, det.global.eawp,
                 det.sound.eawp, det.date.eawp, det.detect.eawp, det.notdate.eawp,
                 det.time.eawp, det.timing.eawp)
msDC.eawp <- modSel(fmsDC)
msDC.eawp
#msDC.ybch@Full
#summary: weather best model (wind, sky), sound (noise, wind)
#  null is third best, (wind, sky, noise) also under 2.0

det.weather.eawp 
confint(det.weather.eawp, type="state",method="normal")
det.sound.eawp 
confint(det.sound.eawp, type="state",method="normal")
det.notdate.eawp 
confint(det.notdate.eawp, type="state",method="normal")
write.table(msDC.eawp@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/eawp_top_models_msDC.xls",sep="\t")


##site covariates next

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# covariates: less canopy cover? less dense stand? hardwoody?

#more appropriate detection covariates (weather - wind + sky)
null.eawp <- pcount(~ Wind + Sky ~1, eawp.abund, mixture="P", K=40)
global.eawp <- pcount(~ Wind + Sky
                      ~ Treatment + Herbicide + BA +Ccover
                       + Ldepth + TreeHt  + TimeSinceB + TimeSinceT + Nthins
                        + NHW_saplings + Rel_HW2P_canopy
                        + PISoils + NSoilTypes
                       + Parea + ShapeIndex
                       , eawp.abund, mixture="P", K=40)
#took out: Nsnags, Age, FPSiteIndex + HW_dens_1050 + FG_herb + FG_shrub + NP_over_20cm
local.eawp <- pcount(~ Wind + Sky
                     ~ Ccover + TreeHt + Ldepth
                     , eawp.abund, mixture="P", K=40) #can only include BA OR CCover
lh.eawp <- pcount(~ Wind + Sky
                  ~ Rel_HW2P_canopy + Ccover + NHW_saplings
                  , eawp.abund, mixture="P", K=40)
landmetrics.eawp <- pcount (~ Wind + Sky
                          ~ Parea + ShapeIndex
                        , eawp.abund, mixture="P",K=40)
landscape500.eawp <- pcount(~ Wind + Sky
                        ~ Evergreen500m + Ag500m + HighDev500m
                            , eawp.abund, mixture="P", K=40)
landscape1.eawp <- pcount(~ Wind + Sky
                       ~ Evergreen1km + Ag1km + HighDev1km
                          , eawp.abund, mixture="P", K=40)
landscape5.eawp <- pcount(~ Wind + Sky
                       ~ Evergreen5km + HighDev5km
                          , eawp.abund, mixture="P", K=40)
                  # - can't use Evergreen&Ag,
                  #+ can't use HighDev&OpenDev together
landscape30.eawp <- pcount(~ Wind + Sky
                       ~ Evergreen30km + HighDev30km + Protected30km
                           , eawp.abund, mixture="P", K=40)
                  #- can't use Protected&Ag together,
                  #- can't use Ag&HighDev together
                  #- can't use Evergreen&Ag together
                  #- can't use HighDev&OpenDev together
                  #- can't use Schrubs&OpenDev together
                  #+ can't use Grass&Ag together
                  #+ can't use Ag&OpenDev together
                  #+ can't use Water&Protected together
                  #+ can't use Schrubs&HighDev together
treatment.eawp <- pcount(~ Wind + Sky
                         ~ Treatment + Nthins
                         , eawp.abund, mixture ="P", K=40)
management.eawp <- pcount(~ Wind + Sky
                          ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide
                          , eawp.abund, mixture="P", K=40)
disturbance.eawp <- pcount(~ Wind + Sky
                           ~ TimeSinceB + TimeSinceT
                           , eawp.abund, mixture="P", K=40)
siteprod.eawp <- pcount(~ Wind + Sky ~ PISoils + NSoilTypes
                        , eawp.abund, mixture="P", K=40) #FPSiteIndex out
#upstate.eawp <- pcount(~ Wind + Sky ~ X + Y + Z, eawp.abund, mixture="P", K=40)


fms <- fitList(null.eawp, global.eawp, local.eawp, lh.eawp, landmetrics.eawp,
               landscape500.eawp, landscape1.eawp, landscape5.eawp, landscape30.eawp,
               treatment.eawp, management.eawp, disturbance.eawp,
                siteprod.eawp)
ms.eawp <- modSel(fms) #note this does not include upstate or FPSiteIndex
ms.eawp
ms.eawp@Full

landscape500.eawp
#dispersion & abundance summary:
#Abundance:
#  Estimate     SE        z P(>|z|)
#(Intercept)    -0.0547 12.580 -0.00435  0.9965
#Evergreen500m   0.4705  0.164  2.86251  0.0042
#Ag500m          0.2149  0.161  1.33483  0.1819
#HighDev500m    -1.7839 89.829 -0.01986  0.9842

#Detection:
#  Estimate    SE      z P(>|z|)
#(Intercept)   -0.136 0.314 -0.432  0.6661
#Wind          -0.305 0.149 -2.046  0.0407
#Sky            0.178 0.157  1.131  0.2579
confint(landscape500.eawp, type="state",method="normal")
#summary of output:

local.eawp
confint(local.eawp, type="state",method="normal")

write.table(ms.eawp@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/eawp_top_models_ms.xls",sep="\t")

#quick test between some correlated
testHigh.cawr <- pcount(~Jdate ~ HighDev30km, cawr.abund, mixture="P", K=10)
testOpen.cawr <- pcount(~Jdate ~ OpenDev30km, cawr.abund, mixture="P", K=10)
testSchrubs.cawr <- pcount(~Jdate ~ Schrubs30km, cawr.abund, mixture="P", K=10)
fmsctest <- fitList(testHigh.cawr, testOpen.cawr, testSchrubs.cawr)
msT.cawr <- modSel(fmsctest)
msT.cawr


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
