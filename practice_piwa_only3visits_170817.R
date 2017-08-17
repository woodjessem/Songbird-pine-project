library("unmarked")
setwd("~/GRAD SCHOOL - CLEMSON/Project-Specific/R_Work")

test3 <-read.csv("piwa_abund3.csv")
summary(test3)
str(test3)

piwa.abund3<- csvToUMF("piwa_abund3.csv", long = FALSE, type = "unmarkedFramePCount")
##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(piwa.abund3)

?pcount

null.piwa <- pcount(~1 ~1, piwa.abund3)
global.piwa <- pcount(~ Noise + Wind + Sky + Jdate ~ Treatment + BA + Nsnags
                      + Ccover + Ldepth + TreeHt + Age + TimeSinceB + Herbicide
                      , piwa.abund3)
local.piwa <- pcount(~ 1 ~ BA + Ccover + TreeHt + Ldepth, piwa.abund3)
lh.piwa <- pcount(~ 1 ~ Ccover + TreeHt + BA, piwa.abund3)
#landscape.piwa <- pcount(~ 1 ~ cov 5 + 6, piwa.abund3)
treatment.piwa <- pcount(~ 1 ~ Treatment + BA + TimeSinceB + Herbicide, piwa.abund3)

fms <- fitList(null.piwa, local.piwa, lh.piwa, treatment.piwa)
ms1.piwa <- modSel(fms)
ms1.piwa@Full
ms1.piwa

#see help for package "xlsReadWrite" in old notes, if need be#
#write.table(ms1.cawr@Full, file="C:/Users/path.type",sep="\t")
