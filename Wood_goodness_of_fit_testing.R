install("installr")
# installing/loading the package:
if(!require(installr)) {
  install.packages("installr"); 
  require(installr)
} #load / install+load installr
updateR()

library("unmarked")
setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")

prwa.abund<- csvToUMF("prwa_abund.csv", long = FALSE, type = "unmarkedFramePCount")

obsCovs(prwa.abund)= scale (obsCovs(prwa.abund))
sc <- siteCovs(prwa.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)]) #from 26 to 74 +landscape+soils
siteCovs(prwa.abund) <- sc

# Full list of candidate models   #NB best type  #JDate best det cov
null2.prwa <- pcount(~ Jdate ~1, prwa.abund, mixture="NB", K=80)
#global2.prwa <- pcount(~ Jdate ~ Treatment + Herbicide + BA + Nsnags +Ccover
#                       + Ldepth + TreeHt + Age + Nburns + Nthins + TimeSinceB + TimeSinceT
#                       + HWdens_50 + FG_herb + NHW_saplings
#                       + Parea + ShapeIndex,
#                      prwa.abund, mixture="NB", K=80)
                  #error Hessian is singular, exclude global for now
                  #unused: FG_shrub, NP_over_20cm, Rel_HW2P_canopy, LCR, PISoils, FPSiteIndex, NSoilTypes
local2.prwa <- pcount(~ Jdate ~ BA + Ccover + TreeHt + Ldepth, prwa.abund, mixture="NB", K=80)
lh2.prwa <- pcount(~ Jdate ~ Age + TimeSinceB + FG_herb + HWdens_50 + NHW_saplings, prwa.abund, mixture="NB", K=80)
landmetrics.prwa <- pcount (~ Jdate ~ Parea + ShapeIndex, prwa.abund, mixture="NB",K=80)
landscape500.prwa <- pcount(~ Jdate ~ Evergreen500m + Grass500m + HighDev500m + Schrubs500m, prwa.abund, mixture="NB", K=80)
landscape1.prwa <- pcount(~ Jdate ~ Evergreen1km + Grass1km + HighDev1km + Schrubs1km, prwa.abund, mixture="NB", K=80)
landscape5.prwa <- pcount(~ Jdate ~ Evergreen5km + Grass5km + HighDev5km + Schrubs5km, prwa.abund, mixture="NB", K=80)
landscape30.prwa <- pcount(~ Jdate ~ Evergreen30km + Grass30km + HighDev30km + Schrubs30km, prwa.abund, mixture="NB", K=80)
treatment2.prwa <- pcount(~ Jdate ~ Treatment + Nthins, prwa.abund, mixture ="NB", K=80)
management2.prwa <- pcount(~ Jdate ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide, prwa.abund, mixture="NB", K=80)
disturbance2.prwa <- pcount(~ Jdate ~ TimeSinceB + TimeSinceT, prwa.abund, mixture="NB", K=80)
#siteprod.prwa <- pcount(~ Jdate ~ PISoils + FPSiteIndex + NSoilTypes, prwa.abund, mixture="NB", K=80)

fms3 <- fitList(null2.prwa, local2.prwa, lh2.prwa, landmetrics.prwa, landscape500.prwa, landscape1.prwa, 
                landscape5.prwa, landscape30.prwa, treatment2.prwa, management2.prwa, disturbance2.prwa)
ms3.prwa <- modSel(fms3) #note this fitList does not include global or siteprod
ms3.prwa
ms3.prwa@Full
landscape5.prwa   #only model
confint(landscape5.prwa, type="state",method="normal")

# Function returning three fit-statistics.  #see line 60 for extra not run
fitstats <- function(landscape5.prwa) {
  observed <- getY(landscape5.prwa@data)
  expected <- fitted(landscape5.prwa)
  resids <- residuals(landscape5.prwa)
  sse <- sum(resids^2, na.rm=TRUE)
  chisq <- sum((observed[,1:3] - expected[,1:3])^2 / expected[,1:3])
  freeTuke <- sum((sqrt(observed[,1:3]) - sqrt(expected[,1:3]))^2)
  out <- c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
  return(out)
}
pb <- parboot(landscape5.prwa, fitstats, nsim=25, report=1)
plot(pb, main="")








install.packages("AICcmodavg", dependencies = TRUE)
install.packages("VGAM", dependencies=TRUE)
library("AICcmodavg")
Nmix.gof.test(landscape5.prwa, nsim = 25, plot.hist = TRUE, report = NULL) #increase nsim
#?Nmix.gof.test()
Nmix.chisq(landscape5.prwa)


## from line 39, this was code in the parboot example that I did NOT modify or run ##
#data(linetran)
#(dbreaksLine <- c(0, 5, 10, 15, 20))
#lengths <- linetran$Length

#ltUMF <- with(prwa.abund, {
#  unmarkedFrameDS(y = cbind(dc1, dc2, dc3, dc4),
#                  siteCovs = data.frame(Length, area, habitat), dist.breaks = dbreaksLine,
#                  tlength = lengths*1000, survey = "line", unitsIn = "m")
#})
# Fit a model
#(fm <- distsamp(~area ~habitat, ltUMF))


  

#parboot function within unmarked (section 3.6, page 20)
    # https://cals.arizona.edu/classes/wfsc578/Fiske%20and%20Chandler%202011.pdf

    # https://groups.google.com/forum/#!topic/unmarked/ICKiDUp_sZM 


#Nmix.gof.test within AICcmodavg  (page 164-167)
    # https://cran.r-project.org/web/packages/AICcmodavg/AICcmodavg.pdf

    # #https://www.rdocumentation.org/packages/AICcmodavg/versions/2.1-1/topics/Nmix.gof.test 

#forum on using 1 vs other:
    # https://groups.google.com/forum/#!topic/unmarked/vVvpDr123lI 
