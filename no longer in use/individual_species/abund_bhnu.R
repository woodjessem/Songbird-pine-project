#BHNU (cavity-nester, bark forager, insects, forest habitat )
#snags, tree height, age, ccover, - density, stand area, (open understory)

library("unmarked")
setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")

test <-read.csv("bhnu_abund.csv")
summary(test)
str(test)
var(test[2:5])
#mean(test[2:4])
mean(test$y.3)

bhnu.abund<- csvToUMF("bhnu_abund.csv", long = FALSE, type = "unmarkedFramePCount")
##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(bhnu.abund)
str(bhnu.abund)
#scale all observation covariates (covs of detection)
obsCovs(bhnu.abund)= scale (obsCovs(bhnu.abund))
#siteCovs(bhnu.abund)= scale (siteCovs(bhnu.abund))
#select particular site covariates to scale below
#(note: NOT ALL - not treatment, herbicide, or years ones)
sc <- siteCovs(bhnu.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)]) #from 26 to 74 +landscape+soils
siteCovs(bhnu.abund) <- sc

#test for NB or Poisson - most should use Poisson
testP.bhnu <- pcount(~1 ~1, bhnu.abund, mixture="P", K=4)
testNB.bhnu <- pcount(~1 ~1, bhnu.abund, mixture="NB", K=4)
fmsTEST <- fitList(testP.bhnu, testNB.bhnu)
msTEST.bhnu <- modSel(fmsTEST)
msTEST.bhnu
#Poisson is best for bhnu.

?pcount

#detection covariates first
det.null.bhnu <- pcount(~1 ~1, bhnu.abund, mixture="P", K=15)
det.weather.bhnu <- pcount(~ Wind + Sky ~1, bhnu.abund, mixture="P", K=15)
det.global.bhnu <- pcount(~ Jdate + Wind + Sky + Noise +Time ~1, bhnu.abund, mixture="P", K=15)
det.sound.bhnu <- pcount(~ Noise + Wind ~1, bhnu.abund, mixture="P", K=15)
det.date.bhnu <- pcount(~ Jdate ~1, bhnu.abund, mixture="P", K=15)
det.detect.bhnu <- pcount(~ Jdate + Noise + Time ~1, bhnu.abund, mixture="P", K=15)
det.notdate.bhnu <-pcount(~ Wind + Sky + Noise ~1, bhnu.abund, mixture="P", K=15)
det.time.bhnu <-pcount(~ Time ~1, bhnu.abund, mixture="P",K=15)

fmsDC <- fitList(det.null.bhnu, det.weather.bhnu, det.global.bhnu,
               det.sound.bhnu, det.date.bhnu, det.detect.bhnu, det.notdate.bhnu, det.time.bhnu)
msDC.bhnu <- modSel(fmsDC)
msDC.bhnu
#msDC.bhnu@Full
#summary: 1st is Time, 2nd null, 3rd detect (Jdate,Noise,Time), 4th date (Jdate)
#    next closest is d2.87 and is weather (wind + sky)

det.time.bhnu   #positive relationship w time and 
confint(det.time.bhnu, type="state",method="normal")  #significant
write.table(msDC.bhnu@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/bhnu_top_models_msDC.xls",sep="\t")


##site covariates next

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#null detection covariates (actually 2nd best from above)
Nnull.bhnu <- pcount(~1 ~1
                     ,bhnu.abund, mixture="P", K=40)
Nglobal.bhnu <- pcount(~ 1
                       ~ Treatment + Herbicide + BA + Nsnags +Ccover
                       + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Nthins
                       + NP_over_20cm + Rel_HW2P_canopy 
                      + PISoils + NSoilTypes
                       + Parea + ShapeIndex
                      , bhnu.abund, mixture="P", K=40)  #FPSiteIndex removed
Nlocal.bhnu <- pcount(~ 1
                      ~ Ccover + TreeHt + Ldepth
                      , bhnu.abund, mixture="P", K=40)
                          #can only include BA OR CCover
Nlh.bhnu <- pcount(~ 1
                    ~ Ccover + Age + Nsnags + TreeHt + NP_over_20cm +Rel_HW2P_canopy
                   , bhnu.abund, mixture="P", K=40)
Nlandmetrics.bhnu <- pcount (~ 1 ~ Parea + ShapeIndex
                             , bhnu.abund, mixture="P",K=40)
Nlandscape500.bhnu <- pcount(~ 1 ~ Evergreen500m + HighDev500m + OpenDev500m
                             , bhnu.abund, mixture="P", K=40)
Nlandscape1.bhnu <- pcount(~ 1 ~ Evergreen1km + OpenDev1km
                           , bhnu.abund, mixture="P", K=40)
Nlandscape5.bhnu <- pcount(~ 1 ~ Evergreen5km + OpenDev5km
                           , bhnu.abund, mixture="P", K=40)
                      # - can't use Evergreen&Ag,
                      #+ can't use HighDev&OpenDev together
Nlandscape30.bhnu <- pcount(~ 1 ~ Evergreen30km + OpenDev30km + Protected30km
                            , bhnu.abund, mixture="P", K=40)
                      #- can't use Protected&Ag together,
                      #- can't use Ag&HighDev together
                      #- can't use Evergreen&Ag together
                      #- can't use HighDev&OpenDev together
                      #- can't use Schrubs&OpenDev together
                      #+ can't use Grass&Ag together
                      #+ can't use Ag&OpenDev together
                      #+ can't use Water&Protected together
                      #+ can't use Schrubs&HighDev together
Ntreatment.bhnu <- pcount(~ 1 ~ Treatment + Nthins
                          , bhnu.abund, mixture ="P", K=40)
Nmanagement.bhnu <- pcount(~ 1 ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide
                           , bhnu.abund, mixture="P", K=40)
Ndisturbance.bhnu <- pcount(~ 1 ~ TimeSinceB + TimeSinceT
                            , bhnu.abund, mixture="P", K=40)
Nsiteprod.bhnu <- pcount(~ 1 ~ PISoils + NSoilTypes
                         , bhnu.abund, mixture="P", K=40) #FPSiteIndex removed
Nupstate.bhnu <- pcount(~ 1 ~ Parea + HighDev5km + BA + TreeHt
                        , bhnu.abund, mixture="P", K=40)

fmsN <- fitList(Nnull.bhnu, Nglobal.bhnu, Nlocal.bhnu, Nlh.bhnu,
                Nlandmetrics.bhnu,
                Nlandscape500.bhnu, Nlandscape1.bhnu, Nlandscape5.bhnu,
                Nlandscape30.bhnu,
                Ntreatment.bhnu, Nmanagement.bhnu, Ndisturbance.bhnu,
                Nsiteprod.bhnu, Nupstate.bhnu)

msN.bhnu <- modSel(fmsN)  #does not include some of site.prod
#msN.bhnu@Full
msN.bhnu

Nlh.bhnu  #best model and only one below 2 (landscape@1km is 2.87)
#dispersion & abundance summary:
#Abundance:
#                Estimate    SE     z P(>|z|)
#(Intercept)        2.080 0.776  2.68 0.00739
#Ccover            -0.192 0.128 -1.50 0.13367
#Age               -0.343 0.197 -1.74 0.08218
#Nsnags             0.215 0.124  1.73 0.08344
#TreeHt             0.254 0.191  1.33 0.18337
#NP_over_20cm      -0.359 0.166 -2.17 0.03031
#Rel_HW2P_canopy   -0.516 0.173 -2.99 0.00281

#Detection:
#  Estimate    SE    z  P(>|z|)
#      -3.07 0.807 -3.8 0.000144

confint(Nlh.bhnu, type="state",method="normal")
#summary of output:
#                           0.025       0.975
#lam(Int)              0.55816395  3.60190334    #sig
#lam(Ccover)          -0.44278144  0.05889276
#lam(Age)             -0.72953461  0.04375042
#lam(Nsnags)          -0.02836109  0.45737930
#lam(TreeHt)          -0.12000964  0.62727637
#lam(NP_over_20cm)    -0.68350182 -0.03413843     #sig
#lam(Rel_HW2P_canopy) -0.85519073 -0.17764091     #sig

write.table(msN.bhnu@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/bhnu_top_models_msN.xls",sep="\t")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#other detection covariates (Time best model given msDC.bhnu)
null.bhnu <- pcount(~ Time ~1, bhnu.abund, mixture="P", K=40)
global.bhnu <- pcount(~ Time
                      ~ Treatment + Herbicide + BA + Nsnags +Ccover
                      + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Nthins
                      + NP_over_20cm + Rel_HW2P_canopy 
                      + PISoils + NSoilTypes
                      + Parea + ShapeIndex
                      , bhnu.abund, mixture="P", K=40)  #FPSiteIndex removed
local.bhnu <- pcount(~ Time
                     ~ Ccover + TreeHt + Ldepth
                     , bhnu.abund, mixture="P", K=40) #can only include BA OR CCover
lh.bhnu <- pcount(~ Time
                  ~ Ccover + Age + Nsnags + TreeHt + NP_over_20cm + Rel_HW2P_canopy
                  , bhnu.abund, mixture="P", K=40)
landmetrics.bhnu <- pcount (~ Time
                          ~ Parea + ShapeIndex
                        , bhnu.abund, mixture="P",K=40)
landscape500.bhnu <- pcount(~ Time
                        ~ Evergreen500m + HighDev500m + OpenDev500m
                            , bhnu.abund, mixture="P", K=40)
landscape1.bhnu <- pcount(~ Time
                       ~ Evergreen1km + OpenDev1km
                          , bhnu.abund, mixture="P", K=40)
landscape5.bhnu <- pcount(~ Time
                       ~ Evergreen5km + OpenDev5km
                          , bhnu.abund, mixture="P", K=40)
                  # - can't use Evergreen&Ag,
                  #+ can't use HighDev&OpenDev together
landscape30.bhnu <- pcount(~ Time
                       ~ Evergreen30km + OpenDev30km + Protected30km
                           , bhnu.abund, mixture="P", K=40)
                  #- can't use Protected&Ag together,
                  #- can't use Ag&HighDev together
                  #- can't use Evergreen&Ag together
                  #- can't use HighDev&OpenDev together
                  #- can't use Schrubs&OpenDev together
                  #+ can't use Grass&Ag together
                  #+ can't use Ag&OpenDev together
                  #+ can't use Water&Protected together
                  #+ can't use Schrubs&HighDev together
treatment.bhnu <- pcount(~ Time
                         ~ Treatment + Nthins
                         , bhnu.abund, mixture ="P", K=40)
management.bhnu <- pcount(~ Time
                          ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide
                          , bhnu.abund, mixture="P", K=40)
disturbance.bhnu <- pcount(~ Time
                           ~ TimeSinceB + TimeSinceT
                           , bhnu.abund, mixture="P", K=40)
siteprod.bhnu <- pcount(~ Time ~ PISoils + NSoilTypes
                        , bhnu.abund, mixture="P", K=40) #FPSiteIndex removed
upstate.bhnu <- pcount(~ Time ~ Parea + HighDev5km + BA + TreeHt
                       , bhnu.abund, mixture="P", K=40)

fms <- fitList(null.bhnu, global.bhnu, local.bhnu, lh.bhnu, landmetrics.bhnu,
               landscape500.bhnu, landscape1.bhnu, landscape5.bhnu, landscape30.bhnu,
               treatment.bhnu, management.bhnu, disturbance.bhnu,
                siteprod.bhnu, upstate.bhnu)
ms.bhnu <- modSel(fms) #note this does not include some of site.prod
ms.bhnu
ms.bhnu@Full

lh.bhnu   #best model and only one below 2.0, landscape@1km is 3.33
#dispersion & abundance summary:
#                Estimate    SE     z P(>|z|)
#(Intercept)        2.029 0.853  2.38  0.0174
#Ccover            -0.200 0.128 -1.56  0.1189
#Age               -0.332 0.197 -1.68  0.0920
#Nsnags             0.223 0.126  1.78  0.0752
#TreeHt             0.239 0.190  1.26  0.2068
#NP_over_20cm      -0.350 0.165 -2.12  0.0343
#Rel_HW2P_canopy   -0.533 0.175 -3.05  0.0023

#Detection:
#           Estimate    SE     z  P(>|z|)
#(Intercept)   -3.035 0.892 -3.40 0.000665
#Time           0.231 0.132  1.76 0.078431

confint(lh.bhnu, type="state",method="normal")
#summary of output:
#                        0.025       0.975
#lam(Int)              0.3566512  3.70135992     #sig
#lam(Ccover)          -0.4513112  0.05138292
#lam(Age)             -0.7184945  0.05423311
#lam(Nsnags)          -0.0227385  0.46972797
#lam(TreeHt)          -0.1322787  0.61105709
#lam(NP_over_20cm)    -0.6738699 -0.02596831    #sig
#lam(Rel_HW2P_canopy) -0.8762021 -0.19039375    #sig


write.table(ms.bhnu@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/bhnu_top_models_ms.xls",sep="\t")

################### paste parboot stuff below when figured out ####
bhnu.abund<- csvToUMF("bhnu_abund.csv", long = FALSE, type = "unmarkedFramePCount")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#next best ()
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
