#PIWA (forest, insects, tree nesting, bark forager, nest high in pines) 
# covariates: HW2P ratio, canopy, tree ht, BA, 
library("unmarked")
setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")

test <-read.csv("piwa_abund.csv") #spp CSV file!
summary(test)
str(test)
var(test[2:5])  #what were these testing for? variance>mean?
#mean(test[2:4])
mean(test$y.3)

piwa.abund<- csvToUMF("piwa_abund.csv", long = FALSE, type = "unmarkedFramePCount")
##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(piwa.abund)
str(piwa.abund)
#scale all observation covariates (covs of detection)
obsCovs(piwa.abund)= scale (obsCovs(piwa.abund))
#siteCovs(piwa.abund)= scale (siteCovs(piwa.abund))
#select particular site covariates to scale below
#(note: NOT ALL - not treatment, herbicide, or years ones)
sc <- siteCovs(piwa.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)]) #from 26 to 74 +landscape+soils
siteCovs(piwa.abund) <- sc

#test for NB or Poisson - most should use Poisson
testP.piwa <- pcount(~1 ~1, piwa.abund, mixture="P", K=30)
testNB.piwa <- pcount(~1 ~1, piwa.abund, mixture="NB", K=30)
fmsTEST <- fitList(testP.piwa, testNB.piwa)
msTEST.piwa <- modSel(fmsTEST)
msTEST.piwa
## P is best for this species.

?pcount

#detection covariates first
det.null.piwa <- pcount(~1 ~1, piwa.abund, mixture="P", K=15)
det.weather.piwa <- pcount(~ Wind + Sky ~1, piwa.abund, mixture="P", K=15)
det.global.piwa <- pcount(~ Jdate + Wind + Sky + Noise +Time ~1, piwa.abund, mixture="P", K=15)
det.sound.piwa <- pcount(~ Noise + Wind ~1, piwa.abund, mixture="P", K=15)
det.date.piwa <- pcount(~ Jdate ~1, piwa.abund, mixture="P", K=15)
det.detect.piwa <- pcount(~ Jdate + Noise + Time ~1, piwa.abund, mixture="P", K=15)
det.notdate.piwa <-pcount(~ Wind + Sky + Noise ~1, piwa.abund, mixture="P", K=15)
det.time.piwa <-pcount(~ Time ~1, piwa.abund, mixture="P",K=15)
det.timing.piwa <-pcount(~ Time + Jdate ~1, piwa.abund, mixture="P", K=15)

fmsDC <- fitList(det.null.piwa, det.weather.piwa, det.global.piwa,
                 det.sound.piwa, det.date.piwa, det.detect.piwa, det.notdate.piwa,
                 det.time.piwa, det.timing.piwa)
msDC.piwa <- modSel(fmsDC)
msDC.piwa
#msDC.piwa@Full
#summary: timing is best model, Jdate second, global next!

det.timing.piwa 
confint(det.timing.piwa, type="state",method="normal")
write.table(msDC.piwa@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/PIWA_top_models_msDC.xls",sep="\t")


##site covariates next

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#null detection covariates (aka NOT using the knowledge gained above - not yet adjusted!!)
Nnull.piwa <- pcount(~1 ~1
                     ,piwa.abund, mixture="P", K=40)
Nglobal.piwa <- pcount(~ 1
                       ~ Treatment + Herbicide + BA + Nsnags +Ccover
                       + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Nthins
                       + HW_dens_1050 + FG_herb + FG_shrub + NHW_saplings + NP_over_20cm
                      + Rel_HW2P_canopy + PISoils + FPSiteIndex + NSoilTypes
                       + Parea + ShapeIndex
                      , piwa.abund, mixture="P", K=40)
Nlocal.piwa <- pcount(~ 1
                      ~ Ccover + TreeHt + Ldepth
                      , piwa.abund, mixture="P", K=40)
                          #can only include BA OR CCover
Nlh.piwa <- pcount(~ 1
                    ~ XXXX
                   , piwa.abund, mixture="P", K=40)
Nlandmetrics.piwa <- pcount (~ 1 ~ Parea + ShapeIndex
                             , piwa.abund, mixture="P",K=40)
Nlandscape500.piwa <- pcount(~ 1 ~ Evergreen500m + Grass500m + HighDev500m + Schrubs500m
                             , piwa.abund, mixture="P", K=40)
Nlandscape1.piwa <- pcount(~ 1 ~ Evergreen1km + Grass1km + HighDev1km + Schrubs1km
                           , piwa.abund, mixture="P", K=40)
Nlandscape5.piwa <- pcount(~ 1 ~ Evergreen5km + Grass5km + HighDev5km + Schrubs5km
                           , piwa.abund, mixture="P", K=40)
                      # - can't use Evergreen&Ag,
                      #+ can't use HighDev&OpenDev together
Nlandscape30.piwa <- pcount(~ 1 ~ Evergreen30km + Grass30km + HighDev30km + Schrubs30km
                            , piwa.abund, mixture="P", K=40)
                      #- can't use Protected&Ag together,
                      #- can't use Ag&HighDev together
                      #- can't use Evergreen&Ag together
                      #- can't use HighDev&OpenDev together
                      #- can't use Schrubs&OpenDev together
                      #+ can't use Grass&Ag together
                      #+ can't use Ag&OpenDev together
                      #+ can't use Water&Protected together
                      #+ can't use Schrubs&HighDev together
Ntreatment.piwa <- pcount(~ 1 ~ Treatment + Nthins
                          , piwa.abund, mixture ="P", K=40)
Nmanagement.piwa <- pcount(~ 1 ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide
                           , piwa.abund, mixture="P", K=40)
Ndisturbance.piwa <- pcount(~ 1 ~ TimeSinceB + TimeSinceT
                            , piwa.abund, mixture="P", K=40)
Nsiteprod.piwa <- pcount(~ 1 ~ PISoils + FPSiteIndex + NSoilTypes
                         , piwa.abund, mixture="P", K=40)
#Nupstate.piwa <- pcount(~ 1 ~ X + Y + Z, piwa.abund, mixture="P", K=40)

fmsN <- fitList(Nnull.piwa, Nglobal.piwa, Nlocal.piwa, Nlh.piwa,
                Nlandmetrics.piwa,
                Nlandscape500.piwa, Nlandscape1.piwa, Nlandscape5.piwa,
                Nlandscape30.piwa,
                Ntreatment.piwa, Nmanagement.piwa, Ndisturbance.piwa,
                Nsiteprod.piwa, Nupstate.piwa)

msN.piwa <- modSel(fmsN)
msN.piwa@Full
msN.piwa
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#more appropriate detection covariates (Using Jdate+Time)
null.piwa <- pcount(~ Jdate + Time ~1, piwa.abund, mixture="P", K=40)
global.piwa <- pcount(~ Jdate + Time 
                      ~ Treatment + Herbicide + BA + Ccover
                       + Ldepth + TreeHt + TimeSinceB + TimeSinceT + Nthins
                       + NP_over_20cm
                       + Rel_HW2P_canopy + PISoils + NSoilTypes
                       + Parea + ShapeIndex
                       , piwa.abund, mixture="P", K=40)  #FPSiteIndex taken out, Age, HWs, snags
local.piwa <- pcount(~ Jdate + Time 
                     ~ Ccover + TreeHt + Ldepth
                     , piwa.abund, mixture="P", K=40) #can only include BA OR CCover
lh.piwa <- pcount(~ Jdate + Time 
                  ~ Ccover + TreeHt + Rel_HW2P_canopy + NP_over_20cm
                  , piwa.abund, mixture="P", K=40)
landmetrics.piwa <- pcount (~ Jdate + Time 
                          ~ Parea + ShapeIndex
                        , piwa.abund, mixture="P",K=40)
landscape500.piwa <- pcount(~ Jdate + Time 
                        ~ Evergreen500m + HighDev500m
                            , piwa.abund, mixture="P", K=40)
landscape1.piwa <- pcount(~ Jdate + Time 
                       ~ Evergreen1km + HighDev1km
                          , piwa.abund, mixture="P", K=40)
landscape5.piwa <- pcount(~ Jdate + Time 
                       ~ Evergreen5km + HighDev5km
                          , piwa.abund, mixture="P", K=40)
                  # - can't use Evergreen&Ag,
                  #+ can't use HighDev&OpenDev together
landscape30.piwa <- pcount(~ Jdate + Time 
                       ~ Evergreen30km + HighDev30km + Protected30km
                           , piwa.abund, mixture="P", K=40)
                  #- can't use Protected&Ag together,
                  #- can't use Ag&HighDev together
                  #- can't use Evergreen&Ag together
                  #- can't use HighDev&OpenDev together
                  #- can't use Schrubs&OpenDev together
                  #+ can't use Grass&Ag together
                  #+ can't use Ag&OpenDev together
                  #+ can't use Water&Protected together
                  #+ can't use Schrubs&HighDev together
treatment.piwa <- pcount(~ Jdate + Time 
                         ~ Treatment + Nthins
                         , piwa.abund, mixture ="P", K=40)
management.piwa <- pcount(~ Jdate + Time 
                          ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide
                          , piwa.abund, mixture="P", K=40)
disturbance.piwa <- pcount(~ Jdate + Time 
                           ~ TimeSinceB + TimeSinceT
                           , piwa.abund, mixture="P", K=40)
siteprod.piwa <- pcount(~ Jdate + Time ~ PISoils + NSoilTypes
                        , piwa.abund, mixture="P", K=40)  #Site Index out
#upstate.piwa <- pcount(~ Jdate + Time ~ X + Y + Z, piwa.abund, mixture="P", K=40)


fms <- fitList(null.piwa, global.piwa, local.piwa, lh.piwa, landmetrics.piwa,
               landscape500.piwa, landscape1.piwa, landscape5.piwa, landscape30.piwa,
               treatment.piwa, management.piwa, disturbance.piwa,
                siteprod.piwa)
ms.piwa <- modSel(fms) #note this does not include upstate
ms.piwa
ms.piwa@Full

landscape5.piwa  #best
#dispersion & abundance summary:
#Abundance:
#  Estimate     SE     z  P(>|z|)
#(Intercept)    1.3585 0.4028  3.37 0.000743
#Evergreen5km  -0.3453 0.1115 -3.10 0.001954
#HighDev5km    -0.0317 0.0961 -0.33 0.741402

#Detection:
#  Estimate     SE     z P(>|z|)
#(Intercept)   -0.963 0.5711 -1.69 0.09174
#Jdate         -0.314 0.1079 -2.91 0.00359
#Time          -0.137 0.0928 -1.47 0.14098
confint(landscape5.piwa, type="state",method="normal")  #Evergreen significant
#summary of output:
#0.025      0.975
#lam(Int)           0.5691392  2.1479336
#lam(Evergreen5km) -0.5637566 -0.1267728
#lam(HighDev5km)   -0.2201306  0.1566863

write.table(ms.piwa@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/piwa_top_models_ms.xls",sep="\t")

landscape30.piwa  #second best
confint(landscape30.piwa, type="state",method="normal")
#significantly negative w High Dev, positive (but non-sig) with Evergreen & Protected

#quick test between some correlated
testHigh.cawr <- pcount(~Jdate ~ HighDev30km, cawr.abund, mixture="P", K=10)
testOpen.cawr <- pcount(~Jdate ~ OpenDev30km, cawr.abund, mixture="P", K=10)
testSchrubs.cawr <- pcount(~Jdate ~ Schrubs30km, cawr.abund, mixture="P", K=10)
fmsctest <- fitList(testHigh.cawr, testOpen.cawr, testSchrubs.cawr)
msT.cawr <- modSel(fmsctest)
msT.cawr


################### paste parboot stuff below when figured out ####
piwa.abund<- csvToUMF("piwa_abund.csv", long = FALSE, type = "unmarkedFramePCount")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#next best detection covariates ()
#modelname+2

fms2 <- fitList(null2.piwa, global2.piwa, local2.piwa, lh2.piwa, landmetrics2.piwa, landscape500_2.piwa, landscape1_2.piwa, 
               landscape5_2.piwa, landscape30_2.piwa, treatment2.piwa, management2.piwa, disturbance2.piwa, siteprod2.piwa, upstate2.piwa)
ms2.piwa <- modSel(fms2) #note this does not include 
ms2.piwa
ms2.piwa@Full

landscape5_2.piwa
#dispersion & abundance summary:

confint(landscape5_2.piwa, type="state",method="normal")
#summary of output:

#write.table(ms2.piwa@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/XXX_top_models_ms2.xls",sep="\t")
