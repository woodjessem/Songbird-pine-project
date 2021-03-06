
library("unmarked")
setwd("~/GRAD SCHOOL - CLEMSON/Project-Specific/R_Work")

test <-read.csv("piwa_abund.csv")
summary(test)
str(test) #y.4 and Noise.4 and Wind.4 and Sky.4 JDate.4 are factors and shouldn't be
#stringsAsFactors=FALSE, but this only works for reading, not for below.

piwa.abund<- csvToUMF("piwa_abund.csv", long = FALSE, type = "unmarkedFramePCount")
    ##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(piwa.abund)
#obsCovs(piwa.abund)= scale(obsCovs(piwa.abund))
#siteCovs(piwa.abund)= scale(siteCovs(piwa.abund))

?pcount

#detection covariates first
det.null.piwa <- pcount(~1 ~1, piwa.abund)
det.weather.piwa <- pcount(~ Wind + Sky ~1, piwa.abund)
det.global.piwa <- pcount(~ Jdate + Wind + Sky + Noise ~1, piwa.abund)
det.sound.piwa <- pcount(~ Noise + Wind ~1, piwa.abund)
det.date.piwa <- pcount(~ Jdate ~1, piwa.abund)
det.detect.piwa <- pcount(~ Jdate + Noise ~1, piwa.abund)
det.notdate.piwa <-pcount(~ Wind + Sky + Noise ~1, piwa.abund)

fms <- fitList(det.null.piwa, det.weather.piwa, det.global.piwa,
               det.sound.piwa, det.date.piwa, det.detect.piwa, det.notdate.piwa)
ms1.piwa <- modSel(fms)
ms1.piwa@Full
ms1.piwa

#found that weather (wind + sky) was top model
# and sound (wind + noise) was really close behind (delta 0.76)
#  but global was least, and date was really not that relevant!
# added "not date" which is wind + sky + noise & it was the third highest model (1.29)


#site covariates next

#no K and no mixture type set (NB or P or ZIP) yet
null.piwa <- pcount(~1 ~1, piwa.abund)
global.piwa <- pcount(~ 1 ~ Treatment + BA + Nsnags
                      + Ccover + Ldepth + TreeHt + Age + TimeSinceB + Herbicide
                      , piwa.abund)
local.piwa <- pcount(~ 1 ~ BA + Ccover + TreeHt + Ldepth, piwa.abund)
lh.piwa <- pcount(~ 1 ~ Ccover + TreeHt + BA, piwa.abund)
#landscape.piwa <- pcount(~ 1 ~ cov 5 + 6, piwa.abund)
treatment.piwa <- pcount(~ 1 ~ Treatment + BA + TimeSinceB + Herbicide, piwa.abund)

fms2 <- fitList(null.piwa, global.piwa, local.piwa, lh.piwa, treatment.piwa)
ms2.piwa <- modSel(fms2)
ms2.piwa@Full
ms2.piwa

#no K and no mixture type set (NB or P or ZIP) yet
null2.piwa <- pcount(~ Wind + Sky + Noise ~1, piwa.abund)
global2.piwa <- pcount(~ Wind + Sky + Noise ~ Treatment + BA + Nsnags
                      + Ccover + Ldepth + TreeHt + Age + TimeSinceB + Herbicide
                      , piwa.abund)
local2.piwa <- pcount(~ Wind + Sky + Noise ~ BA + Ccover + TreeHt + Ldepth, piwa.abund)
lh2.piwa <- pcount(~ Wind + Sky + Noise ~ Ccover + TreeHt + BA, piwa.abund)
#landscape.piwa <- pcount(~ Wind + Sky + Noise ~ cov 5 + 6, piwa.abund)
treatment2.piwa <- pcount(~ Wind + Sky + Noise ~ Treatment + BA + TimeSinceB + Herbicide, piwa.abund)

fms3 <- fitList(null.piwa, global.piwa, local.piwa, lh.piwa, treatment.piwa)
ms3.piwa <- modSel(fms3)
ms3.piwa@Full
ms3.piwa

#for some reason, those ones are no different at all from ms2...

#see help for package "xlsReadWrite" in old notes, if need be#
#write.table(ms1.cawr@Full, file="C:/Users/path.type",sep="\t")