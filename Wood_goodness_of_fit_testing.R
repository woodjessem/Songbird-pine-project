library("unmarked")
setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")

prwa.abund<- csvToUMF("prwa_abund.csv", long = FALSE, type = "unmarkedFramePCount")

obsCovs(prwa.abund)= scale (obsCovs(prwa.abund))
sc <- siteCovs(prwa.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)]) #from 26 to 74 +landscape+soils
siteCovs(prwa.abund) <- sc


# Full list of candidate models   #NB best type  #JDate best det cov
null2.prwa <- pcount(~ Jdate ~1, prwa.abund, mixture="NB", K=40)
#global2.prwa <- pcount(~ Jdate ~ Treatment + Herbicide + BA + Nsnags +Ccover
                       + Ldepth + TreeHt + Age + Nburns + Nthins + TimeSinceB + TimeSinceT
                       + HWdens_50 + FG_herb + FG_shrub + NHW_saplings + NP_over_20cm
                       + Rel_HW2P_canopy + LCR + PISoils + FPSiteIndex + NSoilTypes
                       , prwa.abund, mixture="NB", K=40) #error Hessian is singular
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
ms3.prwa <- modSel(fms3) #note this fitList does not include global or siteprod
ms3.prwa
ms3.prwa@Full
landscape5.prwa #lowest AIC model
confint(landscape5.prwa, type="state",method="normal")

# Function returning three fit-statistics.  #see line 59 for extra not run
fitstats <- function(landscape5.prwa) {
  observed <- getY(landscape5.prwa@data)    #abund.prwa inside () not recommended
  expected <- fitted(landscape5.prwa)
  resids <- residuals(landscape5.prwa)
  sse <- sum(resids^2, na.rm=TRUE)
  chisq <- sum((observed - expected)^2 / expected)
  freeTuke <- sum((sqrt(observed) - sqrt(expected))^2)
  out <- c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
  return(out)
}
pb <- parboot(landscape5.prwa, fitstats, nsim=25, report=1)
plot(pb, main="")



#use above instead of the following way, because AICcmodavg package not loading:
# Nmix.gof.test(landscape5.prwa, nsim = 25, plot.hist = TRUE, report = NULL)


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
