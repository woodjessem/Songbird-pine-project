# EABL (insectivore, cavity-nesting, ground foraging, grassland habitat) 
# covariates: BA, canopy cover, low height and mid-height veg (lack thereof)
library("unmarked")
setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")

test <-read.csv("eabl_abund.csv") #spp CSV file!
summary(test)
str(test)
var(test[2:5])  #what were these testing for? variance>mean?
#mean(test[2:4])
mean(test$y.3)

eabl.abund<- csvToUMF("eabl_abund.csv", long = FALSE, type = "unmarkedFramePCount")
##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(eabl.abund)
str(eabl.abund)
#scale all observation covariates (covs of detection)
obsCovs(eabl.abund)= scale (obsCovs(eabl.abund))
#siteCovs(eabl.abund)= scale (siteCovs(eabl.abund))
#select particular site covariates to scale below
#(note: NOT ALL - not treatment, herbicide, or years ones)
sc <- siteCovs(eabl.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)]) #from 26 to 74 +landscape+soils
siteCovs(eabl.abund) <- sc

#test for NB or Poisson - most should use Poisson
testP.eabl <- pcount(~1 ~1, eabl.abund, mixture="P", K=4)
testNB.eabl <- pcount(~1 ~1, eabl.abund, mixture="NB", K=4)
fmsTEST <- fitList(testP.eabl, testNB.eabl)
msTEST.eabl <- modSel(fmsTEST)
msTEST.eabl
## P is best for this species.

?pcount

#detection covariates first
det.null.eabl <- pcount(~1 ~1, eabl.abund, mixture="P", K=15)
det.weather.eabl <- pcount(~ Wind + Sky ~1, eabl.abund, mixture="P", K=15)
det.global.eabl <- pcount(~ Jdate + Wind + Sky + Noise +Time ~1, eabl.abund, mixture="P", K=15)
det.sound.eabl <- pcount(~ Noise + Wind ~1, eabl.abund, mixture="P", K=15)
det.date.eabl <- pcount(~ Jdate ~1, eabl.abund, mixture="P", K=15)
det.detect.eabl <- pcount(~ Jdate + Noise + Time ~1, eabl.abund, mixture="P", K=15)
det.notdate.eabl <-pcount(~ Wind + Sky + Noise ~1, eabl.abund, mixture="P", K=15)
det.time.eabl <-pcount(~ Time ~1, eabl.abund, mixture="P",K=15)
det.timing.eabl <-pcount(~ Time + Jdate ~1, eabl.abund, mixture="P", K=15)

fmsDC <- fitList(det.null.eabl, det.weather.eabl, det.global.eabl,
                 det.sound.eabl, det.date.eabl, det.detect.eabl, det.notdate.eabl,
                 det.time.eabl, det.timing.eabl)
msDC.eabl <- modSel(fmsDC)
msDC.eabl
#msDC.eabl@Full
#summary: date is best, closely followed by null.

det.date.eabl   #non-sig with Jdate but kinda positive 
confint(det.date.eabl, type="det",method="normal")  #non-sig #changed FROM "state"
write.table(msDC.eabl@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/eabl_top_models_msDC.xls",sep="\t")


##site covariates next

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#null detection covariates (aka NOT using the knowledge gained above)
#not adj yet#
Nnull.ybch <- pcount(~1 ~1
                     ,ybch.abund, mixture="P", K=40)
Nglobal.ybch <- pcount(~ 1
                       ~ Treatment + Herbicide + BA + Nsnags +Ccover
                       + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Nthins
                       + HW_dens_1050 + FG_herb + FG_shrub + NHW_saplings + NP_over_20cm
                      + Rel_HW2P_canopy + PISoils + FPSiteIndex + NSoilTypes
                       + Parea + ShapeIndex
                      , ybch.abund, mixture="P", K=40)
Nlocal.ybch <- pcount(~ 1
                      ~ Ccover + TreeHt + Ldepth
                      , ybch.abund, mixture="P", K=40)
                          #can only include BA OR CCover
Nlh.ybch <- pcount(~ 1
                    ~ XXXX
                   , ybch.abund, mixture="P", K=40)
# covariates: BA, canopy cover, low height and mid-height veg (lack thereof)
Nlandmetrics.ybch <- pcount (~ 1 ~ Parea + ShapeIndex
                             , ybch.abund, mixture="P",K=40)
Nlandscape500.ybch <- pcount(~ 1 ~ Evergreen500m + Grass500m + HighDev500m + Schrubs500m
                             , ybch.abund, mixture="P", K=40)
Nlandscape1.ybch <- pcount(~ 1 ~ Evergreen1km + Grass1km + HighDev1km + Schrubs1km
                           , ybch.abund, mixture="P", K=40)
Nlandscape5.ybch <- pcount(~ 1 ~ Evergreen5km + Grass5km + HighDev5km + Schrubs5km
                           , ybch.abund, mixture="P", K=40)
                      # - can't use Evergreen&Ag,
                      #+ can't use HighDev&OpenDev together
Nlandscape30.ybch <- pcount(~ 1 ~ Evergreen30km + Grass30km + HighDev30km + Schrubs30km
                            , ybch.abund, mixture="P", K=40)
                      #- can't use Protected&Ag together,
                      #- can't use Ag&HighDev together
                      #- can't use Evergreen&Ag together
                      #- can't use HighDev&OpenDev together
                      #- can't use Schrubs&OpenDev together
                      #+ can't use Grass&Ag together
                      #+ can't use Ag&OpenDev together
                      #+ can't use Water&Protected together
                      #+ can't use Schrubs&HighDev together
Ntreatment.ybch <- pcount(~ 1 ~ Treatment + Nthins
                          , ybch.abund, mixture ="P", K=40)
Nmanagement.ybch <- pcount(~ 1 ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide
                           , ybch.abund, mixture="P", K=40)
Ndisturbance.ybch <- pcount(~ 1 ~ TimeSinceB + TimeSinceT
                            , ybch.abund, mixture="P", K=40)
Nsiteprod.ybch <- pcount(~ 1 ~ PISoils + FPSiteIndex + NSoilTypes
                         , ybch.abund, mixture="P", K=40)
#Nupstate.ybch <- pcount(~ 1 ~ X + Y + Z, ybch.abund, mixture="P", K=40)

fmsN <- fitList(Nnull.ybch, Nglobal.ybch, Nlocal.ybch, Nlh.ybch,
                Nlandmetrics.ybch,
                Nlandscape500.ybch, Nlandscape1.ybch, Nlandscape5.ybch,
                Nlandscape30.ybch,
                Ntreatment.ybch, Nmanagement.ybch, Ndisturbance.ybch,
                Nsiteprod.ybch, Nupstate.ybch)

msN.ybch <- modSel(fmsN)
msN.ybch@Full
msN.ybch
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#more appropriate detection covariates ()
null.eabl <- pcount(~ Jdate ~1, eabl.abund, mixture="P", K=40)
global.eabl <- pcount(~ Jdate
                      ~ Treatment + Herbicide + BA + Nsnags +Ccover
                       + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Nthins
                       + HW_dens_1050 + FG_herb + NHW_saplings
                       + PISoils + NSoilTypes
                       + Parea + ShapeIndex
                       , eabl.abund, mixture="P", K=40)
local.eabl <- pcount(~ Jdate
                     ~ Ccover + TreeHt + Ldepth
                     , eabl.abund, mixture="P", K=40) #can only include BA OR CCover
lh.eabl <- pcount(~ Jdate
                  ~ BA + FG_herb + HW_dens_1050 + NHW_saplings + Nsnags
                  , eabl.abund, mixture="P", K=40)
# covariates: BA, canopy cover, low height and mid-height veg (lack thereof), cavities
landmetrics.eabl <- pcount (~ Jdate
                          ~ Parea + ShapeIndex
                        , eabl.abund, mixture="P",K=40)
landscape500.eabl <- pcount(~ Jdate
                        ~ Grass500m + OpenDev500m + Schrubs500m + Ag500m
                            , eabl.abund, mixture="P", K=40)
landscape1.eabl <- pcount(~ Jdate
                       ~ Grass1km + OpenDev1km + Schrubs1km + Ag1km
                          , eabl.abund, mixture="P", K=40)
landscape5.eabl <- pcount(~ Jdate
                       ~ Grass5km + OpenDev5km + Schrubs5km + Ag5km
                          , eabl.abund, mixture="P", K=40)
                  # - can't use Evergreen&Ag,
                  #+ can't use HighDev&OpenDev together
landscape30.eabl <- pcount(~ Jdate
                       ~ Grass30km + OpenDev30km
                           , eabl.abund, mixture="P", K=40)
                  #- can't use Protected&Ag together,
                  #- can't use Ag&HighDev together
                  #- can't use Evergreen&Ag together
                  #- can't use HighDev&OpenDev together
                  #- can't use Schrubs&OpenDev together
                  #+ can't use Grass&Ag together
                  #+ can't use Ag&OpenDev together
                  #+ can't use Water&Protected together
                  #+ can't use Schrubs&HighDev together
treatment.eabl <- pcount(~ Jdate
                         ~ Treatment + Nthins
                         , eabl.abund, mixture ="P", K=40)
management.eabl <- pcount(~ Jdate
                          ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide
                          , eabl.abund, mixture="P", K=40)
disturbance.eabl <- pcount(~ Jdate
                           ~ TimeSinceB + TimeSinceT
                           , eabl.abund, mixture="P", K=40)
siteprod.eabl <- pcount(~ Jdate ~ PISoils + NSoilTypes
                        , eabl.abund, mixture="P", K=40) # + FPSiteIndex
#upstate.eabl <- pcount(~ Jdate ~ X + Y + Z, eabl.abund, mixture="P", K=40)


fmsEABL <- fitList(null.eabl, global.eabl, local.eabl, lh.eabl, landmetrics.eabl,
               landscape500.eabl, landscape1.eabl, landscape5.eabl, landscape30.eabl,
               treatment.eabl, management.eabl, disturbance.eabl,
                siteprod.eabl)  
ms.eabl <- modSel(fmsEABL) #note this does not include upstate
ms.eabl
#ms.eabl@Full
#landmetrics best and only under 2.0  (null is 2.5)

landmetrics.eabl
#dispersion & abundance summary:
#Abundance:
#  Estimate    SE      z P(>|z|)
#(Intercept)  -0.0232 0.228 -0.102  0.9188
#Parea        -0.2251 0.185 -1.217  0.2237
#ShapeIndex    0.3815 0.153  2.501  0.0124

#Detection:
#  Estimate    SE     z P(>|z|)
#(Intercept)   -0.410 0.332 -1.23   0.217
#Jdate          0.227 0.161  1.41   0.159

confint(landmetrics.eabl, type="state",method="normal")
#summary of output:  non-sig with patch area (overlaps 0 widely) -
#  but sig with shape index!

#write.table(ms.eabl@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/XXX_top_models_msX.xls",sep="\t")

#quick test between some correlated
testBA.eabl <- pcount(~Jdate ~ BA, eabl.abund, mixture="P", K=10)
testCcover.eabl <- pcount(~Jdate ~ Ccover, eabl.abund, mixture="P", K=10)
fmsctest <- fitList(testBA.eabl, testCcover.eabl)
msT.eabl <- modSel(fmsctest)
msT.eabl

testSchrubs.eabl <- pcount(~Jdate ~ Schrubs30km, eabl.abund, mixture="P", K=10)




################### paste parboot stuff below when figured out ####
eabl.abund<- csvToUMF("eabl_abund.csv", long = FALSE, type = "unmarkedFramePCount")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#next best detection covariates ()    #not run yet
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
