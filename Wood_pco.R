library("unmarked")
library("ggplot2")
setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")

#smaller file to test with
inbu.test<-read.csv("test.pco.csv")
umf.t<- unmarkedFramePCO(inbu.test,
                    y=inbu.test[,2:9],            #not first column, next 8
                    siteCovs=inbu.test[,10],    #only 1 variable to test with
                    obsCovs=inbu.test[,11:18],   #left out yearly for now
                    numPrimary=2)
str(umf.t)

#help from hotstetler and chandler 2015 ovenbird data -with my small file to start
ydata <- as.matrix(read.csv('test.pco.csv', row.names=1))
umf.dt<- unmarkedFramePCO(y=ydata[,2:9],            #not first column, next 8
                         siteCovs=ydata[,10],    #only 1 variable for now to test with
                         obsCovs=ydata[,11:18],   
                         numPrimary=2)           #left out yearly for now


#real file - perhaps not formatted right yet - see below for different importing approach
inbu.umf<- csvToUMF("inbu_abund3.csv", long = FALSE, type = "unmarkedFramePCO",
                    y=inbu.umf[,2:5],            #not first column, next 4
                    siteCovs=inbu.umf[,6:17],    #6 to 17 now site level (don't change w year)
                    obsCovs=inbu.umf[,83:102],    
                    yearlySiteCovs=inbu.umf[,18:82],
                    numPrimary=2)
str(inbu.umf)

#different reading in approach - seems to work a little better?
inbu.abund<-read.csv("inbu_abund3.csv")

inbu.umf<- unmarkedFramePCO(inbu.abund,
                    y=inbu.abund[,2:5],          #51 rows are primary 1, 50 rows are primary 2
                    siteCovs=inbu.abund[,6:17],    #6 to 17 now site level (don't change w year)
                    obsCovs=inbu.abund[,83:102],    
                    yearlySiteCovs=inbu.abund[,18:82],
                    numPrimary=2)
str(inbu.umf)


#MINE - 2 primary x 4 secondary (only 3 for some) = 2X4 = 8 
inbu.umf <-unmarkedFramePCO(y=inbu.umf[,2:5],            #not first column, next 4
                            siteCovs=inbu.umf[,6:17],    #6 to 17 now site level (don't change w year)
                            obsCovs=inbu.umf[,83:102],    
                            yearlySiteCovs=inbu.umf[,18:82],   #original site covariates
                            numPrimary=2)

obsCovs(inbu.umf)= scale(obsCovs(inbu.umf))  #scale all OBS
sc <- siteCovs(inbu.umf)
siteCovs(inbu.umf)= scale(siteCovs(inbu.umf)) #scale all SITE
ysc<-yearlySiteCovs(inbu.umf)
ysc[,c(23:82)] <- scale(ysc[, c(23:82)])  #scale only SOME yearly site covs
yearlySiteCovs(inbu.umf) <- ysc
                                
  pcountOpen(lambdaformula = ~ Ccover + TreeHt + Ldepth + YearCat,
             gammaformula = ~1, omegaformula = ~1,
             pformula = ~ Jdate + Noise + Time + I(Time^2),
             data = inbu.abund, mixture = "P", K = 70, dynamics = "trend", se = T)


#other example - he wanted to specify column names - but what does his data LOOK like?!
#x.umf <- unmarkedFramePCO(y=x.dat[,1:64], 
#               siteCovs=        data.frame(x.dat[,c("BLOCK", "TREATMENT")]), 
#               obsCovs=         list(OBSEXP = x.dat[,names(x.dat)[83:146]]), 
#               yearlySiteCovs = list(LOGGED = x.dat[,names(x.dat)[67:74]], 
#                                 YEAR = x.dat[,names(x.dat)[75:82]]), numPrimary=8) 
#
#(fm1 <- pcountOpen(~BLOCK, ~LOGGED + TREATMENT + YEAR, ~LOGGED + 
#                     TREATMENT + YEAR, ~OBSEXP, x.umf, K=30)) 



#My own data has dimensions M = 53 sites, T = 2 seasons, and J = 3-4 visits per season
#will need something like:
#     
#      season1v1 season1v2 season1v3 season2v1 season2v2 season2v3
#  site1 0 0 0 0 0 0
#  site2 1 1 1 1 1 1
#  site3 0 0 0 0 0 0
#  site4 1 1 1 1 1 1
#         umf <- unmarkedMultFrame(y = y, numPrimary = 2)



                            
                            
                            
                            
                            
                            
                            
                            
                            
                            Repeated count data with 4 primary periods and
                            # no 2 secondary sampling periods (ie J==2)
                            y2 <- matrix(c(
                              0,0, 2,2, 3,2, 2,2,
                              2,2, 2,1, 3,2, 1,1,
                              1,0, 1,1, 0,0, 0,0,
                              0,0, 0,0, 0,0, 0,0), nrow=4, ncol=8, byrow=TRUE)
                            # Site-specific covariates
                            sc2 <- data.frame(x1 = 1:4, x2 = c('A','A','B','B'))
                            # Observation-specific covariates
                            oc2 <- list(
                              x3 = matrix(1:8, nrow=4, ncol=8, byrow=TRUE),
                              x4 = matrix(letters[1:8], nrow=4, ncol=8, byrow=TRUE))
                            # Yearly-site covariates
                            ysc2 <- list(
                              x5 = matrix(c(
                                1,2,3,4,
                                1,2,3,4,
                                1,2,3,4,
                                1,2,3,4), nrow=4, ncol=4, byrow=TRUE))
                            # Primary periods of surveys
                            primaryPeriod2 <- matrix(as.integer(c(
                              1,2,5,7,
                              1,2,3,4,
                              1,2,4,5,
                              1,3,5,6)), nrow=4, ncol=4, byrow=TRUE)
                            # Create the unmarkedFrame
                            umf2 <- unmarkedFramePCO(y=y2, siteCovs=sc2, obsCovs=oc2,
                                                     yearlySiteCovs=ysc2,
                                                     numPrimary=4, primaryPeriod=primaryPeriod2)
                            # Take a look
                            umf2
                            summary(umf2)