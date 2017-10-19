library("unmarked")
setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")

test <-read.csv("ybch_abund.csv")
summary(test)
str(test)
var(test[2:5])
#mean(test[2:4])
mean(test$y.3)

## DON'T SCALE YET ##

#scale all observation covariates (covs of detection)
obsCovs(test)= scale (obsCovs(test))
#siteCovs(ybch.abund)= scale (siteCovs(ybch.abund))
#select particular site covariates to scale below
#(note: NOT ALL - not treatment, herbicide, or years ones)
sc <- siteCovs(test)
sc[,c(4:23)] <- scale(sc[, c(4:23)])
siteCovs(test) <- sc

#cor.test(data$columnvariable1, data$columnvariable2)
#Herbicide by each
#cor.test(test$Herbicide, test$Treatment) #non-numeric
cor.test(test$Herbicide, test$LastB) #r is 0.164
cor.test(test$Herbicide, test$LastT) #r is -0.169
cor.test(test$Herbicide, test$BA)     #r is -0.173
cor.test(test$Herbicide, test$Nsnags) #r is 0.007
cor.test(test$Herbicide, test$Ccover) #r is -0.209
cor.test(test$Herbicide, test$Ldepth) #r is -0.059
cor.test(test$Herbicide, test$TreeHt) #r is -0.167 
cor.test(test$Herbicide, test$Age)    #r is 0.102
cor.test(test$Herbicide, test$Nburns) #r is 0.350   #high-ISH
cor.test(test$Herbicide, test$Nthins)  #r is 0.101
cor.test(test$Herbicide, test$TimeSinceB) #r is -0.300
cor.test(test$Herbicide, test$TimeSinceT) #r is -0.064
cor.test(test$Herbicide, test$HWdens_10)  #r is -0.137
cor.test(test$Herbicide, test$HWdens_50) #r is -0.041
cor.test(test$Herbicide, test$HWdens_100) #r is -0.135
cor.test(test$Herbicide, test$FG_herb)   #r is 0.221
cor.test(test$Herbicide, test$FG_shrub)  #r is -0.038
cor.test(test$Herbicide, test$NHW_saplings)  #r is -0.205
cor.test(test$Herbicide, test$NP_over_20cm)  #r is -0.192
cor.test(test$Herbicide, test$Rel_HW2P_canopy) #r is -0.390   #highest but still not 0.5 #cool
cor.test(test$Herbicide, test$Rel_HW2P_shrubcover) #r is -0.118

#LastBurn by each
#cor.test(test$LastB, test$Treatment) #non-numeric
cor.test(test$LastB, test$Herbicide) # r is 0.164
#cor.test(test$LastB, test$LastB) # r is 
cor.test(test$LastB, test$LastT) #r is -0.170
cor.test(test$LastB, test$BA)     #r is -0.101
cor.test(test$LastB, test$Nsnags) #r is -0.090
cor.test(test$LastB, test$Ccover) #r is -0.164
cor.test(test$LastB, test$Ldepth) #r is -0.469  # this is higher but not quite 0.5 - encouraging proxy!
cor.test(test$LastB, test$TreeHt) #r is -0.380   #ish
cor.test(test$LastB, test$Age)    #r is -0.023
cor.test(test$LastB, test$Nburns) #r is 0.297
cor.test(test$LastB, test$Nthins)  #r is -0.096
cor.test(test$LastB, test$TimeSinceB) #r is -1  # duh makes sense - redundant variable
cor.test(test$LastB, test$TimeSinceT) #r is 0.191
cor.test(test$LastB, test$HWdens_10)  #r is -0.127
cor.test(test$LastB, test$HWdens_50) #r is -0.035
cor.test(test$LastB, test$HWdens_100) #r is -0.029
cor.test(test$LastB, test$FG_herb)   #r is 0.437 # higher but not quite 0.5 - encouraging trend!
cor.test(test$LastB, test$FG_shrub)  #r is 0.203 
cor.test(test$LastB, test$NHW_saplings)  #r is -0.120
cor.test(test$LastB, test$NP_over_20cm)  #r is -0.067
cor.test(test$LastB, test$Rel_HW2P_canopy) #r is -0.232
cor.test(test$LastB, test$Rel_HW2P_shrubcover) #r is -0.106

#LastThin by each
#cor.test(test$LastT, test$Treatment) #non-numeric
cor.test(test$LastT, test$Herbicide) # r is -0.170
cor.test(test$LastT, test$LastB) # r is -0.170
#cor.test(test$LastT, test$LastT) #r is 
cor.test(test$LastT, test$BA)     #r is -0.235
cor.test(test$LastT, test$Nsnags) #r is -0.053
cor.test(test$LastT, test$Ccover) #r is -0.203
cor.test(test$LastT, test$Ldepth) #r is -0.087
cor.test(test$LastT, test$TreeHt) #r is -0.199
cor.test(test$LastT, test$Age)    #r is -0.332 # high-ISH but not worrisome
cor.test(test$LastT, test$Nburns) #r is -0.224
cor.test(test$LastT, test$Nthins)  #r is -0.083
cor.test(test$LastT, test$TimeSinceB) #r is -0.005
cor.test(test$LastT, test$TimeSinceT) #r is -1 # duh correlated with time since thinned
cor.test(test$LastT, test$HWdens_10)  #r is 0.037
cor.test(test$LastT, test$HWdens_50) #r is -0.094
cor.test(test$LastT, test$HWdens_100) #r is 0.037
cor.test(test$LastT, test$FG_herb)   #r is -0.013
cor.test(test$LastT, test$FG_shrub)  #r is 0.142
cor.test(test$LastT, test$NHW_saplings)  #r is -0.114
cor.test(test$LastT, test$NP_over_20cm)  #r is 0.104
cor.test(test$LastT, test$Rel_HW2P_canopy) #r is 0.040
cor.test(test$LastT, test$Rel_HW2P_shrubcover) #r is -0.067

#BA by each
#cor.test(test$BA, test$Treatment) #non-numeric
cor.test(test$BA, test$Herbicide) # r is -0.173
cor.test(test$BA, test$LastB) # r is -0.101
cor.test(test$BA, test$LastT) #r is -0.235
#cor.test(test$BA, test$BA)     #r is 
cor.test(test$BA, test$Nsnags) #r is 0.174
cor.test(test$BA, test$Ccover) #r is 0.825 # YES THERE IS CORRELATION HERE ! ACKNOWLEDDGE!
cor.test(test$BA, test$Ldepth) #r is 0.224
cor.test(test$BA, test$TreeHt) #r is -0.119
cor.test(test$BA, test$Age)    #r is -0.470 #high but just under 0.5 - should acknowledge !
cor.test(test$BA, test$Nburns) #r is -0.494 #high but just under 0.5 - should acknowledge !
cor.test(test$BA, test$Nthins)  #r is -0.536 #high but just under 0.5 - should acknowledge !
cor.test(test$BA, test$TimeSinceB) #r is 0.410 #high but just under 0.5 - should acknowledge !
cor.test(test$BA, test$TimeSinceT) #r is 0.208
cor.test(test$BA, test$HWdens_10)  #r is -0.462 #high but just under 0.5 - should acknowledge !
cor.test(test$BA, test$HWdens_50) #r is -0.392 #high-ISH but under 0.5 - should acknowledge !
cor.test(test$BA, test$HWdens_100) #r is -0.272
cor.test(test$BA, test$FG_herb)   #r is -0.391 #high-ISH but under 0.5 - should acknowledge !
cor.test(test$BA, test$FG_shrub)  #r is 0.043
cor.test(test$BA, test$NHW_saplings)  #r is -0.035
cor.test(test$BA, test$NP_over_20cm)  #r is 0.608 #high - over 0.5 ! makes sense !
cor.test(test$BA, test$Rel_HW2P_canopy) #r is 0.213
cor.test(test$BA, test$Rel_HW2P_shrubcover) #r is -0.070

#Nsnags by each
#cor.test(test$Nsnags, test$Treatment) #non-numeric
cor.test(test$Nsnags, test$Herbicide) # r is 0.007
cor.test(test$Nsnags, test$LastB) # r is -0.090
cor.test(test$Nsnags, test$LastT) #r is -0.053
cor.test(test$Nsnags, test$BA)     #r is 0.174
#cor.test(test$Nsnags, test$Nsnags) #r is 
cor.test(test$Nsnags, test$Ccover) #r is 0.192
cor.test(test$Nsnags, test$Ldepth) #r is -0.075
cor.test(test$Nsnags, test$TreeHt) #r is -0.051
cor.test(test$Nsnags, test$Age)    #r is 0.078 # interesting that NOT
cor.test(test$Nsnags, test$Nburns) #r is -0.040
cor.test(test$Nsnags, test$Nthins)  #r is -0.296
cor.test(test$Nsnags, test$TimeSinceB) #r is 0.160
cor.test(test$Nsnags, test$TimeSinceT) #r is 0.164
cor.test(test$Nsnags, test$HWdens_10)  #r is -0.282
cor.test(test$Nsnags, test$HWdens_50) #r is -0.139
cor.test(test$Nsnags, test$HWdens_100) #r is -0.150
cor.test(test$Nsnags, test$FG_herb)   #r is -0.011
cor.test(test$Nsnags, test$FG_shrub)  #r is 0.106
cor.test(test$Nsnags, test$NHW_saplings)  #r is -0.171
cor.test(test$Nsnags, test$NP_over_20cm)  #r is 0.184
cor.test(test$Nsnags, test$Rel_HW2P_canopy) #r is -0.021
cor.test(test$Nsnags, test$Rel_HW2P_shrubcover) #r is -0.158


#Ccover by each
#cor.test(test$Ccover, test$Treatment) #non-numeric
cor.test(test$Ccover, test$Herbicide) # r is -0.209
cor.test(test$Ccover, test$LastB) # r is -0.164
cor.test(test$Ccover, test$LastT) #r is -0.203
cor.test(test$Ccover, test$BA)     #r is 0.825 # HIGHLY CORR - MUST ACKNOWLEDGE! BA & COVER
cor.test(test$Ccover, test$Nsnags) #r is 0.192
#cor.test(test$Ccover, test$Ccover) #r is 
cor.test(test$Ccover, test$Ldepth) #r is 0.305 #maybe somewhat high? but well under 0.5
cor.test(test$Ccover, test$TreeHt) #r is 0.139
cor.test(test$Ccover, test$Age)    #r is -0.321 #maybe somewhat high? but well under 0.5
cor.test(test$Ccover, test$Nburns) #r is -0.485 #high but just under 0.5 - should acknowledge !
cor.test(test$Ccover, test$Nthins)  #r is -0.467 #high but just under 0.5 - should acknowledge !
cor.test(test$Ccover, test$TimeSinceB) #r is 0.391 #high-ISH but still under 0.5 - acknowledge !
cor.test(test$Ccover, test$TimeSinceT) #r is 0.200 # interesting that less than burning
cor.test(test$Ccover, test$HWdens_10)  #r is -0.385 #high-ISH but still under 0.5 - acknowledge !
cor.test(test$Ccover, test$HWdens_50) #r is -0.344 #ditto above
cor.test(test$Ccover, test$HWdens_100) #r is -0.196
cor.test(test$Ccover, test$FG_herb)   #r is -0.376 #high-ISH but still under 0.5 - acknowledge !
cor.test(test$Ccover, test$FG_shrub)  #r is -0.122
cor.test(test$Ccover, test$NHW_saplings)  #r is 0.056
cor.test(test$Ccover, test$NP_over_20cm)  #r is 0.508 # it's 0.5 - acknowledge but makes sense
cor.test(test$Ccover, test$Rel_HW2P_canopy) #r is 0.258 # as should be
cor.test(test$Ccover, test$Rel_HW2P_shrubcover) #r is -0.089

#Ldepth by each
#cor.test(test$Ldepth, test$Treatment) #non-numeric
cor.test(test$Ldepth, test$Herbicide) # r is -0.060
cor.test(test$Ldepth, test$LastB) # r is -0.465  # high but under 0.5 - acknowledge
cor.test(test$Ldepth, test$LastT) #r is 0.087
cor.test(test$Ldepth, test$BA)     #r is 0.224
cor.test(test$Ldepth, test$Nsnags) #r is -0.075
cor.test(test$Ldepth, test$Ccover) #r is 0.305 #high-ISH but still under 0.5 
#cor.test(test$Ldepth, test$Ldepth) #r is 
cor.test(test$Ldepth, test$TreeHt) #r is 0.000
cor.test(test$Ldepth, test$Age)    #r is -0.267
cor.test(test$Ldepth, test$Nburns) #r is -0.484 # high but just under 0.5 - correct proxy
cor.test(test$Ldepth, test$Nthins)  #r is -0.192
cor.test(test$Ldepth, test$TimeSinceB) #r is 0.366 # high-ISH but under 0.5 -makes sense
cor.test(test$Ldepth, test$TimeSinceT) #r is 0.152
cor.test(test$Ldepth, test$HWdens_10)  #r is -0.105
cor.test(test$Ldepth, test$HWdens_50) #r is -0.151
cor.test(test$Ldepth, test$HWdens_100) #r is 0.025
cor.test(test$Ldepth, test$FG_herb)   #r is -0.509 # AHA! indirect rel bw fire and forbes/grasses?!
cor.test(test$Ldepth, test$FG_shrub)  #r is -0.267 #maybe above?
cor.test(test$Ldepth, test$NHW_saplings)  #r is 0.260 #
cor.test(test$Ldepth, test$NP_over_20cm)  #r is 0.174
cor.test(test$Ldepth, test$Rel_HW2P_canopy) #r is 0.183
cor.test(test$Ldepth, test$Rel_HW2P_shrubcover) #r is -0.131

#TreeHt by each
#cor.test(test$TreeHt, test$Treatment) #non-numeric
cor.test(test$TreeHt, test$Herbicide) # r is -0.167
cor.test(test$TreeHt, test$LastB) # r is -0.380 #high-ISH but under 0.5
cor.test(test$TreeHt, test$LastT) #r is -0.199
cor.test(test$TreeHt, test$BA)     #r is -0.119
cor.test(test$TreeHt, test$Nsnags) #r is -0.051
cor.test(test$TreeHt, test$Ccover) #r is 0.139
cor.test(test$TreeHt, test$Ldepth) #r is 0.000
#cor.test(test$TreeHt, test$TreeHt) #r is 
cor.test(test$TreeHt, test$Age)    #r is 0.594 # over 0.5 - highly correlated - ACKNOWLEDGE
cor.test(test$TreeHt, test$Nburns) #r is 0.266
cor.test(test$TreeHt, test$Nthins)  #r is 0.438 # close to 0.5 - ACKNOWLEDGE THIS AGING corr
cor.test(test$TreeHt, test$TimeSinceB) #r is -0.082
cor.test(test$TreeHt, test$TimeSinceT) #r is 0.083
cor.test(test$TreeHt, test$HWdens_10)  #r is 0.123
cor.test(test$TreeHt, test$HWdens_50) #r is 0.235
cor.test(test$TreeHt, test$HWdens_100) #r is 0.229
cor.test(test$TreeHt, test$FG_herb)   #r is -0.156
cor.test(test$TreeHt, test$FG_shrub)  #r is -0.331 # close to 0.5 - huh?
cor.test(test$TreeHt, test$NHW_saplings)  #r is 0.215
cor.test(test$TreeHt, test$NP_over_20cm)  #r is -0.187
cor.test(test$TreeHt, test$Rel_HW2P_canopy) #r is 0.252
cor.test(test$TreeHt, test$Rel_HW2P_shrubcover) #r is 0.092

#Age by each
#cor.test(test$Age, test$Treatment) #non-numeric
cor.test(test$Age, test$Herbicide) # r is 0.102
cor.test(test$Age, test$LastB) # r is -0.023
cor.test(test$Age, test$LastT) #r is -0.332
cor.test(test$Age, test$BA)     #r is -0.470 #close to 0.5 - acknowledge
cor.test(test$Age, test$Nsnags) #r is 0.078
cor.test(test$Age, test$Ccover) #r is -0.321  #under 0.5 but high-ish
cor.test(test$Age, test$Ldepth) #r is -0.267
cor.test(test$Age, test$TreeHt) #r is 0.594  #over 0.5 - acknowledge, duh
#cor.test(test$Age, test$Age)    #r is 
cor.test(test$Age, test$Nburns) #r is 0.530  #over 0.5 - acknowledge *AGE SAMPLING ISSUE
cor.test(test$Age, test$Nthins)  #r is 0.702 #product of management timing!! ACKNOWLEDGE
cor.test(test$Age, test$TimeSinceB) #r is -0.340  #high-ISH but not yet 0.5
cor.test(test$Age, test$TimeSinceT) #r is 0.157
cor.test(test$Age, test$HWdens_10)  #r is 0.250
cor.test(test$Age, test$HWdens_50) #r is 0.384 #high-ISH but not yet 0.5 - acknowledge
cor.test(test$Age, test$HWdens_100) #r is 0.214
cor.test(test$Age, test$FG_herb)   #r is 0.200
cor.test(test$Age, test$FG_shrub)  #r is -0.081
cor.test(test$Age, test$NHW_saplings)  #r is 0.164
cor.test(test$Age, test$NP_over_20cm)  #r is -0.487 # high - makes sense though - redundant proxy?
cor.test(test$Age, test$Rel_HW2P_canopy) #r is -0.020
cor.test(test$Age, test$Rel_HW2P_shrubcover) #r is -0.101

#Nburns by each
#cor.test(test$Nburns, test$Treatment) #non-numeric
cor.test(test$Nburns, test$Herbicide) # r is 0.350  #high-ish
cor.test(test$Nburns, test$LastB) # r is 0.297
cor.test(test$Nburns, test$LastT) #r is -0.224
cor.test(test$Nburns, test$BA)     #r is -0.494  #close to 0.5
cor.test(test$Nburns, test$Nsnags) #r is -0.040
cor.test(test$Nburns, test$Ccover) #r is -0.485  #close to 0.5
cor.test(test$Nburns, test$Ldepth) #r is -0.484  #close to 0.5 - fire proxy
cor.test(test$Nburns, test$TreeHt) #r is 0.266
cor.test(test$Nburns, test$Age)    #r is 0.530
#cor.test(test$Nburns, test$Nburns) #r is 
cor.test(test$Nburns, test$Nthins)  #r is 0.494  #close to 0.5
cor.test(test$Nburns, test$TimeSinceB) #r is -0.515  #close to 0.5
cor.test(test$Nburns, test$TimeSinceT) #r is -0.001
cor.test(test$Nburns, test$HWdens_10)  #r is 0.306  #close to 0.5
cor.test(test$Nburns, test$HWdens_50) #r is 0.504  #0.5  - rel with HW mid height density
cor.test(test$Nburns, test$HWdens_100) #r is 0.158
cor.test(test$Nburns, test$FG_herb)   #r is 0.542  #0.5 - rel with forbs at low height
cor.test(test$Nburns, test$FG_shrub)  #r is -0.081
cor.test(test$Nburns, test$NHW_saplings)  #r is -0.168
cor.test(test$Nburns, test$NP_over_20cm)  #r is -0.332
cor.test(test$Nburns, test$Rel_HW2P_canopy) #r is -0.402  #high-ish, close to 0.5
cor.test(test$Nburns, test$Rel_HW2P_shrubcover) #r is 0.060

#Nthins by each
#cor.test(test$Nthins, test$Treatment) #non-numeric
cor.test(test$Nthins, test$Herbicide) # r is 0.101
cor.test(test$Nthins, test$LastB) # r is -0.096
cor.test(test$Nthins, test$LastT) #r is -0.083
cor.test(test$Nthins, test$BA)     #r is -0.536  #over 0.5 - acknowledge this duh link
cor.test(test$Nthins, test$Nsnags) #r is -0.296 
cor.test(test$Nthins, test$Ccover) #r is -0.467 #close - acknowledge this man. link
cor.test(test$Nthins, test$Ldepth) #r is -0.192
cor.test(test$Nthins, test$TreeHt) #r is 0.438 #close - acknowledge this timing link
cor.test(test$Nthins, test$Age)    #r is 0.702
cor.test(test$Nthins, test$Nburns) #r is 0.494  #close - acknowledge
#cor.test(test$Nthins, test$Nthins)  #r is 
cor.test(test$Nthins, test$TimeSinceB) #r is -0.387  #close-ish
cor.test(test$Nthins, test$TimeSinceT) #r is -0.189
cor.test(test$Nthins, test$HWdens_10)  #r is 0.323  #high-ish
cor.test(test$Nthins, test$HWdens_50) #r is 0.331  #-high-ish
cor.test(test$Nthins, test$HWdens_100) #r is 0.096
cor.test(test$Nthins, test$FG_herb)   #r is 0.171
cor.test(test$Nthins, test$FG_shrub)  #r is -0.105
cor.test(test$Nthins, test$NHW_saplings)  #r is 0.080
cor.test(test$Nthins, test$NP_over_20cm)  #r is -0.374  #high-ISH
cor.test(test$Nthins, test$Rel_HW2P_canopy) #r is -0.202
cor.test(test$Nthins, test$Rel_HW2P_shrubcover) #r is -0.063

# ---------- pause and recover! this was a lot! ---------- #

#TimeSinceB by each
#cor.test(test$TimeSinceB, test$Treatment) #non-numeric
cor.test(test$TimeSinceB, test$Herbicide) # r is -0.300  #high-ISH
cor.test(test$TimeSinceB, test$LastB) # r is -1  #duh! timing since burn
cor.test(test$TimeSinceB, test$LastT) #r is -0.005
cor.test(test$TimeSinceB, test$BA)     #r is 0.413  #high close to 0.5
cor.test(test$TimeSinceB, test$Nsnags) #r is 0.160
cor.test(test$TimeSinceB, test$Ccover) #r is 0.391  #high-ISH
cor.test(test$TimeSinceB, test$Ldepth) #r is 0.366  #high-ISH
cor.test(test$TimeSinceB, test$TreeHt) #r is -0.082
cor.test(test$TimeSinceB, test$Age)    #r is -0.340  #high-ISH
cor.test(test$TimeSinceB, test$Nburns) #r is -0.515  #0.5 - makes sense
cor.test(test$TimeSinceB, test$Nthins)  #r is -0.387  #high-ISH
#cor.test(test$TimeSinceB, test$TimeSinceB) #r is 
cor.test(test$TimeSinceB, test$TimeSinceT) #r is 0.220
cor.test(test$TimeSinceB, test$HWdens_10)  #r is -0.296  #check out later and think
cor.test(test$TimeSinceB, test$HWdens_50) #r is -0.364  # "
cor.test(test$TimeSinceB, test$HWdens_100) #r is -0.337  # "
cor.test(test$TimeSinceB, test$FG_herb)   #r is -0.495  #high
cor.test(test$TimeSinceB, test$FG_shrub)  #r is 0.071
cor.test(test$TimeSinceB, test$NHW_saplings)  #r is 0.025
cor.test(test$TimeSinceB, test$NP_over_20cm)  #r is 0.317  #high-ISH
cor.test(test$TimeSinceB, test$Rel_HW2P_canopy) #r is 0.419   #high-ISH 
cor.test(test$TimeSinceB, test$Rel_HW2P_shrubcover) #r is -0.010
  
#TimeSinceT by each
#cor.test(test$TimeSinceT, test$Treatment) #non-numeric
cor.test(test$TimeSinceT, test$Herbicide) # r is -0.064
cor.test(test$TimeSinceT, test$LastB) # r is 0.191
cor.test(test$TimeSinceT, test$LastT) #r is -1     #duh
cor.test(test$TimeSinceT, test$BA)     #r is 0.210
cor.test(test$TimeSinceT, test$Nsnags) #r is 0.164
cor.test(test$TimeSinceT, test$Ccover) #r is 0.199
cor.test(test$TimeSinceT, test$Ldepth) #r is 0.152
cor.test(test$TimeSinceT, test$TreeHt) #r is 0.083
cor.test(test$TimeSinceT, test$Age)    #r is 0.157
cor.test(test$TimeSinceT, test$Nburns) #r is -0.001
cor.test(test$TimeSinceT, test$Nthins)  #r is -0.189
cor.test(test$TimeSinceT, test$TimeSinceB) #r is 0.220
#cor.test(test$TimeSinceT, test$TimeSinceT) #r is 
cor.test(test$TimeSinceT, test$HWdens_10)  #r is -0.115
cor.test(test$TimeSinceT, test$HWdens_50) #r is 0.053
cor.test(test$TimeSinceT, test$HWdens_100) #r is -0.084
cor.test(test$TimeSinceT, test$FG_herb)   #r is -0.156
cor.test(test$TimeSinceT, test$FG_shrub)  #r is -0.161
cor.test(test$TimeSinceT, test$NHW_saplings)  #r is -0.031
cor.test(test$TimeSinceT, test$NP_over_20cm)  #r is -0.029
cor.test(test$TimeSinceT, test$Rel_HW2P_canopy) #r is 0.164
cor.test(test$TimeSinceT, test$Rel_HW2P_shrubcover) #r is 0.041

#HWdens_10 by each
#cor.test(test$HWdens_10, test$Treatment) #non-numeric
cor.test(test$HWdens_10, test$Herbicide) # r is -0.137
cor.test(test$HWdens_10, test$LastB) # r is -0.127
cor.test(test$HWdens_10, test$LastT) #r is 0.037
cor.test(test$HWdens_10, test$BA)     #r is -0.462  #high-ISH - close to 0.5
cor.test(test$HWdens_10, test$Nsnags) #r is -0.282
cor.test(test$HWdens_10, test$Ccover) #r is -0.385  #high-ISH - under 0.5
cor.test(test$HWdens_10, test$Ldepth) #r is -0.105
cor.test(test$HWdens_10, test$TreeHt) #r is 0.128
cor.test(test$HWdens_10, test$Age)    #r is 0.250
cor.test(test$HWdens_10, test$Nburns) #r is 0.306  #high-ISH - under 0.5
cor.test(test$HWdens_10, test$Nthins)  #r is 0.323  #high-ISH - under 0.5
cor.test(test$HWdens_10, test$TimeSinceB) #r is -0.296
cor.test(test$HWdens_10, test$TimeSinceT) #r is -0.115
#cor.test(test$HWdens_10, test$HWdens_10)  #r is 
cor.test(test$HWdens_10, test$HWdens_50) #r is 0.670   # ACKNOWLEDGE !!
cor.test(test$HWdens_10, test$HWdens_100) #r is 0.428  #slightly less so, but still high
cor.test(test$HWdens_10, test$FG_herb)   #r is 0.343  #high-ISH
cor.test(test$HWdens_10, test$FG_shrub)  #r is -0.271
cor.test(test$HWdens_10, test$NHW_saplings)  #r is 0.189
cor.test(test$HWdens_10, test$NP_over_20cm)  #r is -0.278
cor.test(test$HWdens_10, test$Rel_HW2P_canopy) #r is 0.046
cor.test(test$HWdens_10, test$Rel_HW2P_shrubcover) #r is 0.327  #high-ISH; good! should be

#HWdens_50 by each
#cor.test(test$HWdens_50, test$Treatment) #non-numeric
cor.test(test$HWdens_50, test$Herbicide) # r is -0.041
cor.test(test$HWdens_50, test$LastB) # r is -0.035
cor.test(test$HWdens_50, test$LastT) #r is -0.094
cor.test(test$HWdens_50, test$BA)     #r is -0.392  #high-ISH
cor.test(test$HWdens_50, test$Nsnags) #r is -0.139
cor.test(test$HWdens_50, test$Ccover) #r is -0.344  #high-ISH
cor.test(test$HWdens_50, test$Ldepth) #r is -0.151
cor.test(test$HWdens_50, test$TreeHt) #r is 0.235
cor.test(test$HWdens_50, test$Age)    #r is 0.384  #high-ISH
cor.test(test$HWdens_50, test$Nburns) #r is 0.504  #high
cor.test(test$HWdens_50, test$Nthins)  #r is 0.331  #high-ISH
cor.test(test$HWdens_50, test$TimeSinceB) #r is -0.364  #high-ISH
cor.test(test$HWdens_50, test$TimeSinceT) #r is 0.053
cor.test(test$HWdens_50, test$HWdens_10)  #r is 0.670  #YES - corr with lower level
#cor.test(test$HWdens_50, test$HWdens_50) #r is 
cor.test(test$HWdens_50, test$HWdens_100) #r is 0.705  # YES - corr with highest level
cor.test(test$HWdens_50, test$FG_herb)   #r is 0.400  #high
cor.test(test$HWdens_50, test$FG_shrub)  #r is -0.265
cor.test(test$HWdens_50, test$NHW_saplings)  #r is 0.293  #high-ISH
cor.test(test$HWdens_50, test$NP_over_20cm)  #r is 0.285
cor.test(test$HWdens_50, test$Rel_HW2P_canopy) #r is -0.003
cor.test(test$HWdens_50, test$Rel_HW2P_shrubcover) #r is 0.416  #high

#HWdens_100 by each
#cor.test(test$HWdens_100, test$Treatment) #non-numeric
cor.test(test$HWdens_100, test$Herbicide) # r is -0.135
#cor.test(test$HWdens_100, test$LastB) # r is 
cor.test(test$HWdens_100, test$LastT) #r is 0.037
cor.test(test$HWdens_100, test$BA)     #r is -0.272
cor.test(test$HWdens_100, test$Nsnags) #r is -0.150
cor.test(test$HWdens_100, test$Ccover) #r is -0.196
cor.test(test$HWdens_100, test$Ldepth) #r is 0.025
cor.test(test$HWdens_100, test$TreeHt) #r is 0.229
cor.test(test$HWdens_100, test$Age)    #r is 0.214
cor.test(test$HWdens_100, test$Nburns) #r is 0.158
cor.test(test$HWdens_100, test$Nthins)  #r is 0.096
cor.test(test$HWdens_100, test$TimeSinceB) #r is -0.337  #high-ISH
cor.test(test$HWdens_100, test$TimeSinceT) #r is -0.084
cor.test(test$HWdens_100, test$HWdens_10)  #r is 0.428  #high/ish
cor.test(test$HWdens_100, test$HWdens_50) #r is 0.705  #YES - corr with mid-level
#cor.test(test$HWdens_100, test$HWdens_100) #r is 
cor.test(test$HWdens_100, test$FG_herb)   #r is 0.267
cor.test(test$HWdens_100, test$FG_shrub)  #r is -0.229
cor.test(test$HWdens_100, test$NHW_saplings)  #r is 0.523
cor.test(test$HWdens_100, test$NP_over_20cm)  #r is -0.218
cor.test(test$HWdens_100, test$Rel_HW2P_canopy) #r is 0.088
cor.test(test$HWdens_100, test$Rel_HW2P_shrubcover) #r is 0.493  #high - close to 0.5


#FG_herb by each
#cor.test(test$FG_herb, test$Treatment) #non-numeric
cor.test(test$FG_herb, test$Herbicide) # r is 0.221
cor.test(test$FG_herb, test$LastB) # r is 0.437  #high - close to 0.5
cor.test(test$FG_herb, test$LastT) #r is -0.013
cor.test(test$FG_herb, test$BA)     #r is -0.391  #high-ISH
cor.test(test$FG_herb, test$Nsnags) #r is -0.011
cor.test(test$FG_herb, test$Ccover) #r is -0.375  #high-ISH
cor.test(test$FG_herb, test$Ldepth) #r is -0.510  #high
cor.test(test$FG_herb, test$TreeHt) #r is -0.156
cor.test(test$FG_herb, test$Age)    #r is 0.200
cor.test(test$FG_herb, test$Nburns) #r is 0.542  #high
cor.test(test$FG_herb, test$Nthins)  #r is 0.171
cor.test(test$FG_herb, test$TimeSinceB) #r is -0.495  #high
cor.test(test$FG_herb, test$TimeSinceT) #r is -0.156
cor.test(test$FG_herb, test$HWdens_10)  #r is 0.343  #high-ISH
cor.test(test$FG_herb, test$HWdens_50) #r is 0.400  #high-ISH
cor.test(test$FG_herb, test$HWdens_100) #r is 0.267
#cor.test(test$FG_herb, test$FG_herb)   #r is 
cor.test(test$FG_herb, test$FG_shrub)  #r is 0.227
cor.test(test$FG_herb, test$NHW_saplings)  #r is -0.065
cor.test(test$FG_herb, test$NP_over_20cm)  #r is -0.206
cor.test(test$FG_herb, test$Rel_HW2P_canopy) #r is -0.268
cor.test(test$FG_herb, test$Rel_HW2P_shrubcover) #r is 0.128

#FG_shrub by each
#cor.test(test$FG_shrub, test$Treatment) #non-numeric
cor.test(test$FG_shrub, test$Herbicide) # r is -0.038
cor.test(test$FG_shrub, test$LastB) # r is 0.203
cor.test(test$FG_shrub, test$LastT) #r is 0.142
cor.test(test$FG_shrub, test$BA)     #r is 0.043
cor.test(test$FG_shrub, test$Nsnags) #r is 0.106
cor.test(test$FG_shrub, test$Ccover) #r is -0.122
cor.test(test$FG_shrub, test$Ldepth) #r is -0.267
cor.test(test$FG_shrub, test$TreeHt) #r is -0.332
cor.test(test$FG_shrub, test$Age)    #r is -0.081
cor.test(test$FG_shrub, test$Nburns) #r is -0.081
cor.test(test$FG_shrub, test$Nthins)  #r is -0.105
cor.test(test$FG_shrub, test$TimeSinceB) #r is 0.071
cor.test(test$FG_shrub, test$TimeSinceT) #r is 0.161
cor.test(test$FG_shrub, test$HWdens_10)  #r is -0.271
cor.test(test$FG_shrub, test$HWdens_50) #r is -0.265
cor.test(test$FG_shrub, test$HWdens_100) #r is -0.230
cor.test(test$FG_shrub, test$FG_herb)   #r is 0.227
#cor.test(test$FG_shrub, test$FG_shrub)  #r is 
cor.test(test$FG_shrub, test$NHW_saplings)  #r is -0.224
cor.test(test$FG_shrub, test$NP_over_20cm)  #r is 0.054
cor.test(test$FG_shrub, test$Rel_HW2P_canopy) #r is -0.123
cor.test(test$FG_shrub, test$Rel_HW2P_shrubcover) #r is -0.223

#NHW_saplings by each
#cor.test(test$NHW_saplings, test$Treatment) #non-numeric
cor.test(test$NHW_saplings, test$Herbicide) # r is -0.205
cor.test(test$NHW_saplings, test$LastB) # r is -0.120
cor.test(test$NHW_saplings, test$LastT) #r is -0.114
cor.test(test$NHW_saplings, test$BA)     #r is -0.035
cor.test(test$NHW_saplings, test$Nsnags) #r is -0.171
cor.test(test$NHW_saplings, test$Ccover) #r is 0.056
cor.test(test$NHW_saplings, test$Ldepth) #r is 0.259
cor.test(test$NHW_saplings, test$TreeHt) #r is 0.215
cor.test(test$NHW_saplings, test$Age)    #r is 0.164
cor.test(test$NHW_saplings, test$Nburns) #r is -0.168
cor.test(test$NHW_saplings, test$Nthins)  #r is 0.080
cor.test(test$NHW_saplings, test$TimeSinceB) #r is 0.025
cor.test(test$NHW_saplings, test$TimeSinceT) #r is -0.031
cor.test(test$NHW_saplings, test$HWdens_10)  #r is 0.189
cor.test(test$NHW_saplings, test$HWdens_50) #r is 0.293
cor.test(test$NHW_saplings, test$HWdens_100) #r is 0.522  #high
cor.test(test$NHW_saplings, test$FG_herb)   #r is -0.065
cor.test(test$NHW_saplings, test$FG_shrub)  #r is -0.224
#cor.test(test$NHW_saplings, test$NHW_saplings)  #r is 
cor.test(test$NHW_saplings, test$NP_over_20cm)  #r is -0.187
cor.test(test$NHW_saplings, test$Rel_HW2P_canopy) #r is 0.501  #high
cor.test(test$NHW_saplings, test$Rel_HW2P_shrubcover) #r is 0.305


#NP_over_20cm by each
#cor.test(test$NP_over_20cm, test$Treatment) #non-numeric
cor.test(test$NP_over_20cm, test$Herbicide) # r is 0.192
cor.test(test$NP_over_20cm, test$LastB) # r is -0.067
cor.test(test$NP_over_20cm, test$LastT) #r is 0.104
cor.test(test$NP_over_20cm, test$BA)     #r is 0.608  #high
cor.test(test$NP_over_20cm, test$Nsnags) #r is 0.184
cor.test(test$NP_over_20cm, test$Ccover) #r is 0.508  #high
cor.test(test$NP_over_20cm, test$Ldepth) #r is 0.174
cor.test(test$NP_over_20cm, test$TreeHt) #r is -0.187
cor.test(test$NP_over_20cm, test$Age)    #r is -0.487  #high
cor.test(test$NP_over_20cm, test$Nburns) #r is -0.332  #high-ISH
cor.test(test$NP_over_20cm, test$Nthins)  #r is -0.374  #high-ISH
cor.test(test$NP_over_20cm, test$TimeSinceB) #r is 0.317  #high-ISH
cor.test(test$NP_over_20cm, test$TimeSinceT) #r is -0.029
cor.test(test$NP_over_20cm, test$HWdens_10)  #r is -0.278
cor.test(test$NP_over_20cm, test$HWdens_50) #r is -0.285
cor.test(test$NP_over_20cm, test$HWdens_100) #r is -0.218
cor.test(test$NP_over_20cm, test$FG_herb)   #r is -0.206
cor.test(test$NP_over_20cm, test$FG_shrub)  #r is 0.054
cor.test(test$NP_over_20cm, test$NHW_saplings)  #r is -0.187
#cor.test(test$NP_over_20cm, test$NP_over_20cm)  #r is 
cor.test(test$NP_over_20cm, test$Rel_HW2P_canopy) #r is 0.027
cor.test(test$NP_over_20cm, test$Rel_HW2P_shrubcover) #r is -0.213


#Rel_HW2P_canopy by each
#cor.test(test$Rel_HW2P_canopy, test$Treatment) #non-numeric
cor.test(test$Rel_HW2P_canopy, test$Herbicide) # r is -0.390
cor.test(test$Rel_HW2P_canopy, test$LastB) # r is -0.232
cor.test(test$Rel_HW2P_canopy, test$LastT) #r is 0.040
cor.test(test$Rel_HW2P_canopy, test$BA)     #r is 0.213
cor.test(test$Rel_HW2P_canopy, test$Nsnags) #r is -0.021
cor.test(test$Rel_HW2P_canopy, test$Ccover) #r is 0.258
cor.test(test$Rel_HW2P_canopy, test$Ldepth) #r is 0.183
cor.test(test$Rel_HW2P_canopy, test$TreeHt) #r is 0.252
cor.test(test$Rel_HW2P_canopy, test$Age)    #r is -0.020
cor.test(test$Rel_HW2P_canopy, test$Nburns) #r is -0.402  #high-ISH
cor.test(test$Rel_HW2P_canopy, test$Nthins)  #r is -0.202
cor.test(test$Rel_HW2P_canopy, test$TimeSinceB) #r is 0.419  #high-ISH
cor.test(test$Rel_HW2P_canopy, test$TimeSinceT) #r is 0.164 
cor.test(test$Rel_HW2P_canopy, test$HWdens_10)  #r is 0.046
cor.test(test$Rel_HW2P_canopy, test$HWdens_50) #r is -0.003
cor.test(test$Rel_HW2P_canopy, test$HWdens_100) #r is 0.088
cor.test(test$Rel_HW2P_canopy, test$FG_herb)   #r is -0.268
cor.test(test$Rel_HW2P_canopy, test$FG_shrub)  #r is -0.123
cor.test(test$Rel_HW2P_canopy, test$NHW_saplings)  #r is 0.501  #high
cor.test(test$Rel_HW2P_canopy, test$NP_over_20cm)  #r is 0.027
#cor.test(test$Rel_HW2P_canopy, test$Rel_HW2P_canopy) #r is 
cor.test(test$Rel_HW2P_canopy, test$Rel_HW2P_shrubcover) #r is 0.290

#Rel_HW2P_shrubcover by each
#cor.test(test$Rel_HW2P_shrubcover, test$Treatment) #non-numeric
cor.test(test$Rel_HW2P_shrubcover, test$Herbicide) # r is -0.118
cor.test(test$Rel_HW2P_shrubcover, test$LastB) # r is -0.106
cor.test(test$Rel_HW2P_shrubcover, test$LastT) #r is -0.067
cor.test(test$Rel_HW2P_shrubcover, test$BA)     #r is -0.070
cor.test(test$Rel_HW2P_shrubcover, test$Nsnags) #r is -0.158
cor.test(test$Rel_HW2P_shrubcover, test$Ccover) #r is -0.089
cor.test(test$Rel_HW2P_shrubcover, test$Ldepth) #r is -0.132
cor.test(test$Rel_HW2P_shrubcover, test$TreeHt) #r is 0.092
cor.test(test$Rel_HW2P_shrubcover, test$Age)    #r is -0.101
cor.test(test$Rel_HW2P_shrubcover, test$Nburns) #r is 0.060
cor.test(test$Rel_HW2P_shrubcover, test$Nthins)  #r is -0.063
cor.test(test$Rel_HW2P_shrubcover, test$TimeSinceB) #r is -0.010
cor.test(test$Rel_HW2P_shrubcover, test$TimeSinceT) #r is 0.041
cor.test(test$Rel_HW2P_shrubcover, test$HWdens_10)  #r is 0.327
cor.test(test$Rel_HW2P_shrubcover, test$HWdens_50) #r is 0.416  #high-ISH
cor.test(test$Rel_HW2P_shrubcover, test$HWdens_100) #r is 0.493  #high
cor.test(test$Rel_HW2P_shrubcover, test$FG_herb)   #r is 0.128
cor.test(test$Rel_HW2P_shrubcover, test$FG_shrub)  #r is -0.223
cor.test(test$Rel_HW2P_shrubcover, test$NHW_saplings)  #r is 0.305
cor.test(test$Rel_HW2P_shrubcover, test$NP_over_20cm)  #r is -0.213
cor.test(test$Rel_HW2P_shrubcover, test$Rel_HW2P_canopy) #r is 0.290
#cor.test(test$Rel_HW2P_shrubcover, test$Rel_HW2P_shrubcover) #r is 