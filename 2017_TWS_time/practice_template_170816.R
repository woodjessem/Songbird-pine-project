
library("unmarked")
setwd("~/GRAD SCHOOL - CLEMSON/Project-Specific/R_Work")

#test <-read.csv("cawr_abundance.csv")
#summary(test)
#str(test)

#cawr.abund <- csvToUMF("cawr_abundance.csv", long = FALSE, type = "unmarkedFramePCount")
    ## type may need to change for occupancy (occuRN, pcount or whichever nMixture) ##
#summary(cawr.abund)

?pcount

#null.cawr(~1,~1, data=cawr.abund)
#global.cawr(~ detection covariates 1 + 2 ~ covariates 1 + 2 + 3 + 4, cawr.abund)
#local.cawr(~ detection covariates 1 + 2 ~ covariates 2 + 3 + 4, data)
#lh.cawr(~ det cov ~ covariates 3 + 4, data)
#landscape.cawr (~ det cov ~ cov 5 + 6, data)
#treatment.cawr (~ det cov ~ cov 7, data)

#fms <- fitList(names of all models)
#ms1.cawr <- modSel(fms)
#ms1.cawr@Full
#ms1.cawr

#see help for package "xlsReadWrite" in old notes, if need be#
#write.table(ms1.cawr@Full, file="C:/Users/path.type",sep="\t")
