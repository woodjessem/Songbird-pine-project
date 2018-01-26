library("unmarked")
setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")

test <-read.csv("ybch_abund.csv")
summary(test)
SStable <- summary(test)
?summary
write.table(SStable, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/covariate_summary_stats.xls",sep="\t")
str(test)
var(test[2:5])
#mean(test[2:4])
mean(test$y.3)

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
cor.test(test$Herbicide, test$LCR) #r is 0.006
cor.test(test$Herbicide, test$HW_dens_1050) #r is -0.974
cor.test(test$Herbicide, test$HW_shrub) #r is -0.366
cor.test(test$Herbicide, test$Parea) #r is -0.277
cor.test(test$Herbicide, test$ShapeIndex) #r is -0.260
cor.test(test$Herbicide, test$PAratio) #r is 0.169
cor.test(test$Herbicide, test$FracDimIndex) #r is -0.136
cor.test(test$Herbicide, test$CoreAreaIndex) #r is -0.228
cor.test(test$Herbicide, test$Ag500m) #r is 0.062
cor.test(test$Herbicide, test$Ag1km) #r is -0.074
cor.test(test$Herbicide, test$Ag5km) #r is -0.085
cor.test(test$Herbicide, test$Ag30km) #r is -0.055
cor.test(test$Herbicide, test$Evergreen500m) #r is 0.076
cor.test(test$Herbicide, test$Evergreen1km) #r is 0.321
cor.test(test$Herbicide, test$Evergreen5km) #r is 0.235
cor.test(test$Herbicide, test$Evergreen30km) #r is 0.270
cor.test(test$Herbicide, test$Imperv500m) #r is -0.075
cor.test(test$Herbicide, test$Imperv1km) #r is 0.078
cor.test(test$Herbicide, test$Imperv5km) #r is 0.125
cor.test(test$Herbicide, test$Imperv30km) #r is -0.323
cor.test(test$Herbicide, test$Protected30km) #r is 0.073
cor.test(test$Herbicide, test$HighDev500m) #r is -0.169
cor.test(test$Herbicide, test$HighDev1km) #r is 0.079
cor.test(test$Herbicide, test$HighDev5km) #r is 0.126
cor.test(test$Herbicide, test$HighDev30km) #r is 0.169
cor.test(test$Herbicide, test$LowDev500m) #r is 0.179
cor.test(test$Herbicide, test$LowDev1km) #r is 0.168
cor.test(test$Herbicide, test$LowDev5km) #r is 0.151
cor.test(test$Herbicide, test$LowDev30km) #r is -0.340
cor.test(test$Herbicide, test$OpenDev500m) #r is 0.390
cor.test(test$Herbicide, test$OpenDev1km) #r is 0.352
cor.test(test$Herbicide, test$OpenDev5km) #r is 0.158
cor.test(test$Herbicide, test$OpenDev30km) #r is -0.247
cor.test(test$Herbicide, test$Grass500m) #r is 0
cor.test(test$Herbicide, test$Grass1km) #r is -0.338
cor.test(test$Herbicide, test$Grass5km) #r is -0.335
cor.test(test$Herbicide, test$Grass30km) #r is -0.237
cor.test(test$Herbicide, test$Schrubs500m) #r is -0.135
cor.test(test$Herbicide, test$Schrubs1km) #r is -0.049
cor.test(test$Herbicide, test$Schrubs5km) #r is 0.037
cor.test(test$Herbicide, test$Schrubs30km) #r is -0.009
cor.test(test$Herbicide, test$Water500m) #r is 0.050
cor.test(test$Herbicide, test$Water1km) #r is 0.100
cor.test(test$Herbicide, test$Water5km) #r is 0.170
cor.test(test$Herbicide, test$Water30km) #r is -0.002
cor.test(test$Herbicide, test$NSoilTypes) #r is -0.267
cor.test(test$Herbicide, test$FPSiteIndex)  # r is -0.226
cor.test(test$Herbicide, test$SiteIndexPrimaryS)  # r is -0.203
cor.test(test$Herbicide, test$PISoils)  # r is 0.229
cor.test(test$Herbicide, test$SISoils)  # r is 0.070
cor.test(test$Herbicide, test$HydricSoils)  # r is -0.130


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
cor.test(test$LastB, test$LCR) #r is 0.312
cor.test(test$LastB, test$HW_dens_1050) #r is -0.089
cor.test(test$LastB, test$HW_shrub) #r is -0.112
cor.test(test$LastB, test$Parea) #r is 0.062
cor.test(test$LastB, test$ShapeIndex) #r is 0.249
cor.test(test$LastB, test$PAratio) #r is 0.022
cor.test(test$LastB, test$FracDimIndex) #r is 0.254
cor.test(test$LastB, test$CoreAreaIndex) #r is -0.062
cor.test(test$LastB, test$Ag500m) #r is 0.037
cor.test(test$LastB, test$Ag1km) #r is -0.173
cor.test(test$LastB, test$Ag5km) #r is 0.416
cor.test(test$LastB, test$Ag30km) #r is -0.154
cor.test(test$LastB, test$Evergreen500m) #r is 0.040
cor.test(test$LastB, test$Evergreen1km) #r is 0.090
cor.test(test$LastB, test$Evergreen5km) #r is 0.224
cor.test(test$LastB, test$Evergreen30km) #r is 0.191
cor.test(test$LastB, test$Imperv500m) #r is -0.048
cor.test(test$LastB, test$Imperv1km) #r is -0.215
cor.test(test$LastB, test$Imperv5km) #r is 0.050
cor.test(test$LastB, test$Imperv30km) #r is 0.124
cor.test(test$LastB, test$Protected30km) #r is 0.091
cor.test(test$LastB, test$HighDev500m) #r is ???   - there is only 1 value for HighDev - IGNORE
cor.test(test$LastB, test$HighDev1km) #r is -0.194
cor.test(test$LastB, test$HighDev5km) #r is 0.060
cor.test(test$LastB, test$HighDev30km) #r is 0.065
cor.test(test$LastB, test$LowDev500m) #r is -0.008
cor.test(test$LastB, test$LowDev1km) #r is -0.067
cor.test(test$LastB, test$LowDev5km) #r is 0.016
cor.test(test$LastB, test$LowDev30km) #r is 0.069
cor.test(test$LastB, test$OpenDev500m) #r is -0.139
cor.test(test$LastB, test$OpenDev1km) #r is -0.141
cor.test(test$LastB, test$OpenDev5km) #r is 0.175
cor.test(test$LastB, test$OpenDev30km) #r is 0.129
cor.test(test$LastB, test$Grass500m) #r is 0.143
cor.test(test$LastB, test$Grass1km) #r is 0.002
cor.test(test$LastB, test$Grass5km) #r is -0.382
cor.test(test$LastB, test$Grass30km) #r is -0.559
cor.test(test$LastB, test$Schrubs500m) #r is 0.207
cor.test(test$LastB, test$Schrubs1km) #r is 0.283
cor.test(test$LastB, test$Schrubs5km) #r is 0.344
cor.test(test$LastB, test$Schrubs30km) #r is -0.033
cor.test(test$LastB, test$Water500m) #r is -0.114
cor.test(test$LastB, test$Water1km) #r is 0.048
cor.test(test$LastB, test$Water5km) #r is 0.159
cor.test(test$LastB, test$Water30km) #r is 0.132
cor.test(test$LastB, test$NSoilTypes) #r is -0.044
cor.test(test$LastB, test$FPSiteIndex)  # r is -.108
cor.test(test$LastB, test$SiteIndexPrimaryS)  # r is -0.019
cor.test(test$LastB, test$PISoils)  # r is 0.251
cor.test(test$LastB, test$SISoils)  # r is 0.061
cor.test(test$LastB, test$HydricSoils)  # r is -0.205

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
cor.test(test$LastT, test$LCR) #r is 0.053
cor.test(test$LastT, test$HW_dens_1050) #r is -0.0255
cor.test(test$LastT, test$HW_shrub) #r is 0.131
cor.test(test$LastT, test$Parea) #r is -0.089
cor.test(test$LastT, test$ShapeIndex) #r is 0.089
cor.test(test$LastT, test$PAratio) #r is 0.171
cor.test(test$LastT, test$FracDimIndex) #r is 0.116
cor.test(test$LastT, test$CoreAreaIndex) #r is -0.113
cor.test(test$LastT, test$Ag500m) #r is 0.064
cor.test(test$LastT, test$Ag1km) #r is 0.047
cor.test(test$LastT, test$Ag5km) #r is 0.059
cor.test(test$LastT, test$Ag30km) #r is -0.048
cor.test(test$LastT, test$Evergreen500m) #r is -0.390
cor.test(test$LastT, test$Evergreen1km) #r is -0.299
cor.test(test$LastT, test$Evergreen5km) #r is -0.161
cor.test(test$LastT, test$Evergreen30km) #r is -0.094
cor.test(test$LastT, test$Imperv500m) #r is -0.199
cor.test(test$LastT, test$Imperv1km) #r is 0.081
cor.test(test$LastT, test$Imperv5km) #r is 0.312
cor.test(test$LastT, test$Imperv30km) #r is 0.260
cor.test(test$LastT, test$Protected30km) #r is -0.024
cor.test(test$LastT, test$HighDev500m) #r is -0.210
cor.test(test$LastT, test$HighDev1km) #r is 0.106
cor.test(test$LastT, test$HighDev5km) #r is 0.311
cor.test(test$LastT, test$HighDev30km) #r is 0.017
cor.test(test$LastT, test$LowDev500m) #r is -0.228
cor.test(test$LastT, test$LowDev1km) #r is -0.003
cor.test(test$LastT, test$LowDev5km) #r is 0.270
cor.test(test$LastT, test$LowDev30km) #r is 0.270
cor.test(test$LastT, test$OpenDev500m) #r is -0.065
cor.test(test$LastT, test$OpenDev1km) #r is -0.030
cor.test(test$LastT, test$OpenDev5km) #r is 0.202
cor.test(test$LastT, test$OpenDev30km) #r is 0.109
cor.test(test$LastT, test$Grass500m) #r is 0.352
cor.test(test$LastT, test$Grass1km) #r is 0.306
cor.test(test$LastT, test$Grass5km) #r is 0.180
cor.test(test$LastT, test$Grass30km) #r is 0.279
cor.test(test$LastT, test$Schrubs500m) #r is 0.050
cor.test(test$LastT, test$Schrubs1km) #r is -0.016
cor.test(test$LastT, test$Schrubs5km) #r is -0.111
cor.test(test$LastT, test$Schrubs30km) #r is -0.171
cor.test(test$LastT, test$Water500m) #r is 0.256
cor.test(test$LastT, test$Water1km) #r is 0.221
cor.test(test$LastT, test$Water5km) #r is 0.109
cor.test(test$LastT, test$Water30km) #r is -0.197
cor.test(test$LastT, test$NSoilTypes) #r is -0.238
cor.test(test$LastT, test$FPSiteIndex)  # r is -0.060
cor.test(test$LastT, test$SiteIndexPrimaryS)  # r is 0.016
cor.test(test$LastT, test$PISoils)  # r is -0.102
cor.test(test$LastT, test$SISoils)  # r is 0.348
cor.test(test$LastT, test$HydricSoils)  # r is -0.031

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
cor.test(test$BA, test$LCR) #r is 0.142
cor.test(test$BA, test$HW_dens_1050) #r is -0.470
cor.test(test$BA, test$HW_shrub) #r is -0.145
cor.test(test$BA, test$Parea) #r is 0.204
cor.test(test$BA, test$ShapeIndex) #r is -0.067
cor.test(test$BA, test$PAratio) #r is -0.152
cor.test(test$BA, test$FracDimIndex) #r is -0.118
cor.test(test$BA, test$CoreAreaIndex) #r is 0.166
cor.test(test$BA, test$Ag500m) #r is 0.232
cor.test(test$BA, test$Ag1km) #r is 0.298
cor.test(test$BA, test$Ag5km) #r is 0.324
cor.test(test$BA, test$Ag30km) #r is 0.427
cor.test(test$BA, test$Evergreen500m) #r is -0.239
cor.test(test$BA, test$Evergreen1km) #r is -0.372
cor.test(test$BA, test$Evergreen5km) #r is -0.462
cor.test(test$BA, test$Evergreen30km) #r is -0.149
cor.test(test$BA, test$Imperv500m) #r is -0.112
cor.test(test$BA, test$Imperv1km) #r is 0.018
cor.test(test$BA, test$Imperv5km) #r is 0.179
cor.test(test$BA, test$Imperv30km) #r is -0.053
cor.test(test$BA, test$Protected30km) #r is -0.380
cor.test(test$BA, test$HighDev500m) #r is 0.099
cor.test(test$BA, test$HighDev1km) #r is 0.028
cor.test(test$BA, test$HighDev5km) #r is 0.178
cor.test(test$BA, test$HighDev30km) #r is -0.477
cor.test(test$BA, test$LowDev500m) #r is 0.065
cor.test(test$BA, test$LowDev1km) #r is 0.106
cor.test(test$BA, test$LowDev5km) #r is 0.302
cor.test(test$BA, test$LowDev30km) #r is -0.026
cor.test(test$BA, test$OpenDev500m) #r is 0.203
cor.test(test$BA, test$OpenDev1km) #r is 0.304
cor.test(test$BA, test$OpenDev5km) #r is 0.379
cor.test(test$BA, test$OpenDev30km) #r is 0.296
cor.test(test$BA, test$Grass500m) #r is -0.080
cor.test(test$BA, test$Grass1km) #r is 0.186
cor.test(test$BA, test$Grass5km) #r is 0.367
cor.test(test$BA, test$Grass30km) #r is 0.440
cor.test(test$BA, test$Schrubs500m) #r is -0.023
cor.test(test$BA, test$Schrubs1km) #r is 0.064
cor.test(test$BA, test$Schrubs5km) #r is -0.074
cor.test(test$BA, test$Schrubs30km) #r is -0.223
cor.test(test$BA, test$Water500m) #r is -0.347
cor.test(test$BA, test$Water1km) #r is -0.137
cor.test(test$BA, test$Water5km) #r is 0.292
cor.test(test$BA, test$Water30km) #r is -0.152
cor.test(test$BA, test$NSoilTypes) #r is 0.137
cor.test(test$BA, test$FPSiteIndex)  # r is -0.171
cor.test(test$BA, test$SiteIndexPrimaryS)  # r is -0.110
cor.test(test$BA, test$PISoils)  # r is 0.037
cor.test(test$BA, test$SISoils)  # r is -0.121
cor.test(test$BA, test$HydricSoils)  # r is 0.340

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
cor.test(test$Nsnags, test$LCR) #r is -0.091
cor.test(test$Nsnags, test$HW_dens_1050) #r is -0.232
cor.test(test$Nsnags, test$HW_shrub) #r is -0.062
cor.test(test$Nsnags, test$Parea) #r is 0.034
cor.test(test$Nsnags, test$ShapeIndex) #r is 0.013
cor.test(test$Nsnags, test$PAratio) #r is 0.052
cor.test(test$Nsnags, test$FracDimIndex) #r is -0.019
cor.test(test$Nsnags, test$CoreAreaIndex) #r is 0.028
cor.test(test$Nsnags, test$Ag500m) #r is 0.110
cor.test(test$Nsnags, test$Ag1km) #r is 0.070
cor.test(test$Nsnags, test$Ag5km) #r is 0.079
cor.test(test$Nsnags, test$Ag30km) #r is 0.075
cor.test(test$Nsnags, test$Evergreen500m) #r is -0.216
cor.test(test$Nsnags, test$Evergreen1km) #r is -0.223
cor.test(test$Nsnags, test$Evergreen5km) #r is -0.056
cor.test(test$Nsnags, test$Evergreen30km) #r is 0.142
cor.test(test$Nsnags, test$Imperv500m) #r is -0.054
cor.test(test$Nsnags, test$Imperv1km) #r is -0.096
cor.test(test$Nsnags, test$Imperv5km) #r is -0.149
cor.test(test$Nsnags, test$Imperv30km) #r is -0.097
cor.test(test$Nsnags, test$Protected30km) #r is 0.208
cor.test(test$Nsnags, test$HighDev500m) #r is -0.061
cor.test(test$Nsnags, test$HighDev1km) #r is -0.075
cor.test(test$Nsnags, test$HighDev5km) #r is -0.150
cor.test(test$Nsnags, test$HighDev30km) #r is -0.108
cor.test(test$Nsnags, test$LowDev500m) #r is -0.088
cor.test(test$Nsnags, test$LowDev1km) #r is -0.006
cor.test(test$Nsnags, test$LowDev5km) #r is -0.129
cor.test(test$Nsnags, test$LowDev30km) #r is -0.076
cor.test(test$Nsnags, test$OpenDev500m) #r is 0.158
cor.test(test$Nsnags, test$OpenDev1km) #r is 0.127
cor.test(test$Nsnags, test$OpenDev5km) #r is -0.052
cor.test(test$Nsnags, test$OpenDev30km) #r is 0.067
cor.test(test$Nsnags, test$Grass500m) #r is -0.049
cor.test(test$Nsnags, test$Grass1km) #r is 0.051
cor.test(test$Nsnags, test$Grass5km) #r is 0.173
cor.test(test$Nsnags, test$Grass30km) #r is 0.116
cor.test(test$Nsnags, test$Schrubs500m) #r is 0.087
cor.test(test$Nsnags, test$Schrubs1km) #r is 0.083
cor.test(test$Nsnags, test$Schrubs5km) #r is 0.202
cor.test(test$Nsnags, test$Schrubs30km) #r is -0.029
cor.test(test$Nsnags, test$Water500m) #r is -0.155
cor.test(test$Nsnags, test$Water1km) #r is -0.161
cor.test(test$Nsnags, test$Water5km) #r is 0.141
cor.test(test$Nsnags, test$Water30km) #r is 0.417
cor.test(test$Nsnags, test$NSoilTypes) #r is 0.174
cor.test(test$Nsnags, test$FPSiteIndex)  # r is 0.100
cor.test(test$Nsnags, test$SiteIndexPrimaryS)  # r is 0.074
cor.test(test$Nsnags, test$PISoils)  # r is 0.131
cor.test(test$Nsnags, test$SISoils)  # r is -0.207
cor.test(test$Nsnags, test$HydricSoils)  # r is 0.146

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
cor.test(test$Ccover, test$LCR) #r is -0.041  #haha... so weird that not correlated...
cor.test(test$Ccover, test$HW_dens_1050) #r is -0.401
cor.test(test$Ccover, test$HW_shrub) #r is -0.026
cor.test(test$Ccover, test$Parea) #r is 0.104
cor.test(test$Ccover, test$ShapeIndex) #r is -0.108
cor.test(test$Ccover, test$PAratio) #r is -0.058
cor.test(test$Ccover, test$FracDimIndex) #r is -0.102
cor.test(test$Ccover, test$CoreAreaIndex) #r is 0.054
cor.test(test$Ccover, test$Ag500m) #r is 0.258
cor.test(test$Ccover, test$Ag1km) #r is 0.294
cor.test(test$Ccover, test$Ag5km) #r is 0.392
cor.test(test$Ccover, test$Ag30km) #r is 0.428
cor.test(test$Ccover, test$Evergreen500m) #r is -0.217
cor.test(test$Ccover, test$Evergreen1km) #r is -0.421
cor.test(test$Ccover, test$Evergreen5km) #r is -0.517
cor.test(test$Ccover, test$Evergreen30km) #r is -0.224
cor.test(test$Ccover, test$Imperv500m) #r is -0.087
cor.test(test$Ccover, test$Imperv1km) #r is 0.044
cor.test(test$Ccover, test$Imperv5km) #r is 0.126
cor.test(test$Ccover, test$Imperv30km) #r is 0.095
cor.test(test$Ccover, test$Protected30km) #r is -0.356
cor.test(test$Ccover, test$HighDev500m) #r is 0.116
cor.test(test$Ccover, test$HighDev1km) #r is 0.060
cor.test(test$Ccover, test$HighDev5km) #r is 0.120
cor.test(test$Ccover, test$HighDev30km) #r is -0.533
cor.test(test$Ccover, test$LowDev500m) #r is -0.019
cor.test(test$Ccover, test$LowDev1km) #r is 0.098
cor.test(test$Ccover, test$LowDev5km) #r is 0.215
cor.test(test$Ccover, test$LowDev30km) #r is 0.123
cor.test(test$Ccover, test$OpenDev500m) #r is 0.108
cor.test(test$Ccover, test$OpenDev1km) #r is 0.224
cor.test(test$Ccover, test$OpenDev5km) #r is 0.280
cor.test(test$Ccover, test$OpenDev30km) #r is 0.379
cor.test(test$Ccover, test$Grass500m) #r is -0.209
cor.test(test$Ccover, test$Grass1km) #r is 0.097
cor.test(test$Ccover, test$Grass5km) #r is 0.388
cor.test(test$Ccover, test$Grass30km) #r is 0.472
cor.test(test$Ccover, test$Schrubs500m) #r is -0.058
cor.test(test$Ccover, test$Schrubs1km) #r is -0.019
cor.test(test$Ccover, test$Schrubs5km) #r is -0.233
cor.test(test$Ccover, test$Schrubs30km) #r is -0.304
cor.test(test$Ccover, test$Water500m) #r is -0.219
cor.test(test$Ccover, test$Water1km) #r is -0.149
cor.test(test$Ccover, test$Water5km) #r is 0.141
cor.test(test$Ccover, test$Water30km) #r is -0.107
cor.test(test$Ccover, test$NSoilTypes) #r is 0.109
cor.test(test$Ccover, test$FPSiteIndex)  # r is -0.203
cor.test(test$Ccover, test$SiteIndexPrimaryS)  # r is -0.122
cor.test(test$Ccover, test$PISoils)  # r is -0.048
cor.test(test$Ccover, test$SISoils)  # r is -0.115
cor.test(test$Ccover, test$HydricSoils)  # r is 0.220

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
cor.test(test$Ldepth, test$Nburns) #r is -0.484 # high but just under 0.5 - right proxy?
cor.test(test$Ldepth, test$Nthins)  #r is -0.192
cor.test(test$Ldepth, test$TimeSinceB) #r is 0.366 # high-ISH but under 0.5 -makes sense
cor.test(test$Ldepth, test$TimeSinceT) #r is 0.152
cor.test(test$Ldepth, test$HWdens_10)  #r is -0.105
cor.test(test$Ldepth, test$HWdens_50) #r is -0.151
cor.test(test$Ldepth, test$HWdens_100) #r is 0.025
cor.test(test$Ldepth, test$FG_herb)   #r is -0.509 # AHA! indirect rel bw fire and forbes/grasses?!
cor.test(test$Ldepth, test$FG_shrub)  #r is -0.267 #maybe above?
cor.test(test$Ldepth, test$NHW_saplings)  #r is 0.260
cor.test(test$Ldepth, test$NP_over_20cm)  #r is 0.174
cor.test(test$Ldepth, test$Rel_HW2P_canopy) #r is 0.183
cor.test(test$Ldepth, test$Rel_HW2P_shrubcover) #r is -0.131
cor.test(test$Ldepth, test$LCR) #r is -0.207
cor.test(test$Ldepth, test$HW_dens_1050) #r is -0.139
cor.test(test$Ldepth, test$HW_shrub) #r is 0.004
cor.test(test$Ldepth, test$Parea) #r is -0.184
cor.test(test$Ldepth, test$ShapeIndex) #r is 0.072
cor.test(test$Ldepth, test$PAratio) #r is 0.211
cor.test(test$Ldepth, test$FracDimIndex) #r is 0.145
cor.test(test$Ldepth, test$CoreAreaIndex) #r is -0.156
cor.test(test$Ldepth, test$Ag500m) #r is 0.222
cor.test(test$Ldepth, test$Ag1km) #r is 0.160
cor.test(test$Ldepth, test$Ag5km) #r is 0.215
cor.test(test$Ldepth, test$Ag30km) #r is 0.195
cor.test(test$Ldepth, test$Evergreen500m) #r is -0.272
cor.test(test$Ldepth, test$Evergreen1km) #r is -0.257
cor.test(test$Ldepth, test$Evergreen5km) #r is -0.243
cor.test(test$Ldepth, test$Evergreen30km) #r is -0.452
cor.test(test$Ldepth, test$Imperv500m) #r is 0.094
cor.test(test$Ldepth, test$Imperv1km) #r is 0.149
cor.test(test$Ldepth, test$Imperv5km) #r is 0.338
cor.test(test$Ldepth, test$Imperv30km) #r is 0.110
cor.test(test$Ldepth, test$Protected30km) #r is 0.009
cor.test(test$Ldepth, test$HighDev500m) #r is 0.166
cor.test(test$Ldepth, test$HighDev1km) #r is 0.143
cor.test(test$Ldepth, test$HighDev5km) #r is 0.333
cor.test(test$Ldepth, test$HighDev30km) #r is -0.180
cor.test(test$Ldepth, test$LowDev500m) #r is 0.144
cor.test(test$Ldepth, test$LowDev1km) #r is 0.183
cor.test(test$Ldepth, test$LowDev5km) #r is 0.335
cor.test(test$Ldepth, test$LowDev30km) #r is 0.200
cor.test(test$Ldepth, test$OpenDev500m) #r is 0.185
cor.test(test$Ldepth, test$OpenDev1km) #r is 0.170
cor.test(test$Ldepth, test$OpenDev5km) #r is 0.111
cor.test(test$Ldepth, test$OpenDev30km) #r is 0.154
cor.test(test$Ldepth, test$Grass500m) #r is -0.102
cor.test(test$Ldepth, test$Grass1km) #r is 0.099
cor.test(test$Ldepth, test$Grass5km) #r is 0.057
cor.test(test$Ldepth, test$Grass30km) #r is 0.221
cor.test(test$Ldepth, test$Schrubs500m) #r is -0.084
cor.test(test$Ldepth, test$Schrubs1km) #r is -0.119
cor.test(test$Ldepth, test$Schrubs5km) #r is -0.344
cor.test(test$Ldepth, test$Schrubs30km) #r is -0.380
cor.test(test$Ldepth, test$Water500m) #r is -0.135
cor.test(test$Ldepth, test$Water1km) #r is -0.162
cor.test(test$Ldepth, test$Water5km) #r is -0.127
cor.test(test$Ldepth, test$Water30km) #r is 0.043
cor.test(test$Ldepth, test$NSoilTypes) #r is 0.062
cor.test(test$Ldepth, test$FPSiteIndex)  # r is 0.006
cor.test(test$Ldepth, test$SiteIndexPrimaryS)  # r is -0.016
cor.test(test$Ldepth, test$PISoils)  # r is -0.296
cor.test(test$Ldepth, test$SISoils)  # r is 0.280
cor.test(test$Ldepth, test$HydricSoils)  # r is 0.006

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
cor.test(test$TreeHt, test$Age)    #r is 0.594     # over 0.5 - high - ACKNOWLEDGE
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
cor.test(test$TreeHt, test$LCR) #r is -0.630  #YES - CORRELATED - ACKNOWLEDGE
cor.test(test$TreeHt, test$HW_dens_1050) #r is 0.195
cor.test(test$TreeHt, test$HW_shrub) #r is 0.332
cor.test(test$TreeHt, test$Parea) #r is 0.085
cor.test(test$TreeHt, test$ShapeIndex) #r is 0.034
cor.test(test$TreeHt, test$PAratio) #r is -0.068
cor.test(test$TreeHt, test$FracDimIndex) #r is 0.033
cor.test(test$TreeHt, test$CoreAreaIndex) #r is 0.036
cor.test(test$TreeHt, test$Ag500m) #r is 0.106
cor.test(test$TreeHt, test$Ag1km) #r is 0.175
cor.test(test$TreeHt, test$Ag5km) #r is 0.286
cor.test(test$TreeHt, test$Ag30km) #r is 0.113
cor.test(test$TreeHt, test$Evergreen500m) #r is 0.233
cor.test(test$TreeHt, test$Evergreen1km) #r is -0.051
cor.test(test$TreeHt, test$Evergreen5km) #r is -0.198
cor.test(test$TreeHt, test$Evergreen30km) #r is -0.151
cor.test(test$TreeHt, test$Imperv500m) #r is 0.204
cor.test(test$TreeHt, test$Imperv1km) #r is 0.109
cor.test(test$TreeHt, test$Imperv5km) #r is -0.279
cor.test(test$TreeHt, test$Imperv30km) #r is -0.026
cor.test(test$TreeHt, test$Protected30km) #r is -0.106
cor.test(test$TreeHt, test$HighDev500m) #r is 0.131
cor.test(test$TreeHt, test$HighDev1km) #r is 0.085
cor.test(test$TreeHt, test$HighDev5km) #r is -0.282
cor.test(test$TreeHt, test$HighDev30km) #r is -0.112
cor.test(test$TreeHt, test$LowDev500m) #r is 0.119
cor.test(test$TreeHt, test$LowDev1km) #r is -0.064
cor.test(test$TreeHt, test$LowDev5km) #r is -0.296
cor.test(test$TreeHt, test$LowDev30km) #r is 0.034
cor.test(test$TreeHt, test$OpenDev500m) #r is -0.040
cor.test(test$TreeHt, test$OpenDev1km) #r is -0.150
cor.test(test$TreeHt, test$OpenDev5km) #r is -0.398
cor.test(test$TreeHt, test$OpenDev30km) #r is -0.060
cor.test(test$TreeHt, test$Grass500m) #r is -0.380
cor.test(test$TreeHt, test$Grass1km) #r is -0.237
cor.test(test$TreeHt, test$Grass5km) #r is 0.261
cor.test(test$TreeHt, test$Grass30km) #r is 0.272
cor.test(test$TreeHt, test$Schrubs500m) #r is -0.319
cor.test(test$TreeHt, test$Schrubs1km) #r is -0.340
cor.test(test$TreeHt, test$Schrubs5km) #r is -0.430
cor.test(test$TreeHt, test$Schrubs30km) #r is -0.099
cor.test(test$TreeHt, test$Water500m) #r is 0.050
cor.test(test$TreeHt, test$Water1km) #r is -0.146
cor.test(test$TreeHt, test$Water5km) #r is -0.532
cor.test(test$TreeHt, test$Water30km) #r is -0.188
cor.test(test$TreeHt, test$NSoilTypes) #r is 0.128
cor.test(test$TreeHt, test$FPSiteIndex)  # r is 0.198
cor.test(test$TreeHt, test$SiteIndexPrimaryS)  # r is 0.268
cor.test(test$TreeHt, test$PISoils)  # r is -0.221
cor.test(test$TreeHt, test$SISoils)  # r is -0.143
cor.test(test$TreeHt, test$HydricSoils)  # r is -0.100

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
cor.test(test$Age, test$LCR) #r is -0.383  #high-ISH
cor.test(test$Age, test$HW_dens_1050) #r is 0.345
cor.test(test$Age, test$HW_shrub) #r is 0.139
cor.test(test$Age, test$Parea) #r is 0.003
cor.test(test$Age, test$ShapeIndex) #r is 0.091
cor.test(test$Age, test$PAratio) #r is -0.020
cor.test(test$Age, test$FracDimIndex) #r is 0.082
cor.test(test$Age, test$CoreAreaIndex) #r is -0.027
cor.test(test$Age, test$Ag500m) #r is -0.060
cor.test(test$Age, test$Ag1km) #r is -0.094
cor.test(test$Age, test$Ag5km) #r is -0.096
cor.test(test$Age, test$Ag30km) #r is -0.224
cor.test(test$Age, test$Evergreen500m) #r is 0.401
cor.test(test$Age, test$Evergreen1km) #r is 0.234
cor.test(test$Age, test$Evergreen5km) #r is 0.222
cor.test(test$Age, test$Evergreen30km) #r is 0.160
cor.test(test$Age, test$Imperv500m) #r is 0.237
cor.test(test$Age, test$Imperv1km) #r is 0.046
cor.test(test$Age, test$Imperv5km) #r is -0.288
cor.test(test$Age, test$Imperv30km) #r is -0.070
cor.test(test$Age, test$Protected30km) #r is 0.186
cor.test(test$Age, test$HighDev500m) #r is -0.092
cor.test(test$Age, test$HighDev1km) #r is 0.011
cor.test(test$Age, test$HighDev5km) #r is -0.285
cor.test(test$Age, test$HighDev30km) #r is 0.229
cor.test(test$Age, test$LowDev500m) #r is 0.127
cor.test(test$Age, test$LowDev1km) #r is 0.009
cor.test(test$Age, test$LowDev5km) #r is -0.336
cor.test(test$Age, test$LowDev30km) #r is -0.067
cor.test(test$Age, test$OpenDev500m) #r is -0.027
cor.test(test$Age, test$OpenDev1km) #r is -0.129
cor.test(test$Age, test$OpenDev5km) #r is -0.408
cor.test(test$Age, test$OpenDev30km) #r is -0.209
cor.test(test$Age, test$Grass500m) #r is -0.195
cor.test(test$Age, test$Grass1km) #r is -0.193
cor.test(test$Age, test$Grass5km) #r is -0.094
cor.test(test$Age, test$Grass30km) #r is -0.218
cor.test(test$Age, test$Schrubs500m) #r is -0.071
cor.test(test$Age, test$Schrubs1km) #r is -0.151
cor.test(test$Age, test$Schrubs5km) #r is -0.052
cor.test(test$Age, test$Schrubs30km) #r is 0.182
cor.test(test$Age, test$Water500m) #r is -0.011
cor.test(test$Age, test$Water1km) #r is -0.145
cor.test(test$Age, test$Water5km) #r is -0.328
cor.test(test$Age, test$Water30km) #r is 0.095
cor.test(test$Age, test$NSoilTypes) #r is 0.132
cor.test(test$Age, test$FPSiteIndex)  # r is 0.143
cor.test(test$Age, test$SiteIndexPrimaryS)  # r is 0.178
cor.test(test$Age, test$PISoils)  # r is -0.120
cor.test(test$Age, test$SISoils)  # r is -0.252
cor.test(test$Age, test$HydricSoils)  # r is -0.180

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
cor.test(test$Nburns, test$LCR) #r is -0.106
cor.test(test$Nburns, test$HW_dens_1050) #r is 0.440
cor.test(test$Nburns, test$HW_shrub) #r is -0.018
cor.test(test$Nburns, test$Parea) #r is -0.064
cor.test(test$Nburns, test$ShapeIndex) #r is -0.122
cor.test(test$Nburns, test$PAratio) #r is -0.014
cor.test(test$Nburns, test$FracDimIndex) #r is -0.110
cor.test(test$Nburns, test$CoreAreaIndex) #r is -0.056
cor.test(test$Nburns, test$Ag500m) #r is -0.092
cor.test(test$Nburns, test$Ag1km) #r is -0.059
cor.test(test$Nburns, test$Ag5km) #r is -0.156
cor.test(test$Nburns, test$Ag30km) #r is -0.141
cor.test(test$Nburns, test$Evergreen500m) #r is 0.250
cor.test(test$Nburns, test$Evergreen1km) #r is 0.335
cor.test(test$Nburns, test$Evergreen5km) #r is 0.287
cor.test(test$Nburns, test$Evergreen30km) #r is 0.266
cor.test(test$Nburns, test$Imperv500m) #r is -0.005
cor.test(test$Nburns, test$Imperv1km) #r is 0.044
cor.test(test$Nburns, test$Imperv5km) #r is -0.186
cor.test(test$Nburns, test$Imperv30km) #r is -0.155
cor.test(test$Nburns, test$Protected30km) #r is -0.016
cor.test(test$Nburns, test$HighDev500m) #r is -0.125
cor.test(test$Nburns, test$HighDev1km) #r is 0.038
cor.test(test$Nburns, test$HighDev5km) #r is -0.184
cor.test(test$Nburns, test$HighDev30km) #r is 0.277
cor.test(test$Nburns, test$LowDev500m) #r is -0.074
cor.test(test$Nburns, test$LowDev1km) #r is -0.203
cor.test(test$Nburns, test$LowDev5km) #r is -0.238
cor.test(test$Nburns, test$LowDev30km) #r is -0.188
cor.test(test$Nburns, test$OpenDev500m) #r is -0.196
cor.test(test$Nburns, test$OpenDev1km) #r is -0.227
cor.test(test$Nburns, test$OpenDev5km) #r is -0.186
cor.test(test$Nburns, test$OpenDev30km) #r is -0.257
cor.test(test$Nburns, test$Grass500m) #r is 0.035
cor.test(test$Nburns, test$Grass1km) #r is -0.187
cor.test(test$Nburns, test$Grass5km) #r is -0.010
cor.test(test$Nburns, test$Grass30km) #r is -0.284
cor.test(test$Nburns, test$Schrubs500m) #r is 0.110
cor.test(test$Nburns, test$Schrubs1km) #r is -0.016
cor.test(test$Nburns, test$Schrubs5km) #r is 0.049
cor.test(test$Nburns, test$Schrubs30km) #r is 0.226
cor.test(test$Nburns, test$Water500m) #r is 0.213
cor.test(test$Nburns, test$Water1km) #r is 0.004
cor.test(test$Nburns, test$Water5km) #r is -0.242
cor.test(test$Nburns, test$Water30km) #r is -0.117
cor.test(test$Nburns, test$NSoilTypes) #r is -0.122
cor.test(test$Nburns, test$FPSiteIndex)  # r is 0.177
cor.test(test$Nburns, test$SiteIndexPrimaryS)  # r is 0.235
cor.test(test$Nburns, test$PISoils)  # r is 0.278
cor.test(test$Nburns, test$SISoils)  # r is -0.062
cor.test(test$Nburns, test$HydricSoils)  # r is -0.191

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
cor.test(test$Nthins, test$Age)    #r is 0.702  #ACKNOWLEDGE - unavoidable
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
cor.test(test$Nthins, test$LCR) #r is -0.359
cor.test(test$Nthins, test$HW_dens_1050) #r is 0.358
cor.test(test$Nthins, test$HW_shrub) #r is -0.036
cor.test(test$Nthins, test$Parea) #r is 0.065
cor.test(test$Nthins, test$ShapeIndex) #r is 0.144
cor.test(test$Nthins, test$PAratio) #r is -0.014
cor.test(test$Nthins, test$FracDimIndex) #r is 0.111
cor.test(test$Nthins, test$CoreAreaIndex) #r is 0.040
cor.test(test$Nthins, test$Ag500m) #r is -0.146
cor.test(test$Nthins, test$Ag1km) #r is -0.023
cor.test(test$Nthins, test$Ag5km) #r is 0.085
cor.test(test$Nthins, test$Ag30km) #r is -0.063
cor.test(test$Nthins, test$Evergreen500m) #r is 0.381
cor.test(test$Nthins, test$Evergreen1km) #r is 0.223
cor.test(test$Nthins, test$Evergreen5km) #r is 0.066
cor.test(test$Nthins, test$Evergreen30km) #r is -0.133
cor.test(test$Nthins, test$Imperv500m) #r is 0.250
cor.test(test$Nthins, test$Imperv1km) #r is 0.082
cor.test(test$Nthins, test$Imperv5km) #r is -0.037
cor.test(test$Nthins, test$Imperv30km) #r is 0.152
cor.test(test$Nthins, test$Protected30km) #r is -0.088
cor.test(test$Nthins, test$HighDev500m) #r is -0.114
cor.test(test$Nthins, test$HighDev1km) #r is 0.041
cor.test(test$Nthins, test$HighDev5km) #r is -0.032
cor.test(test$Nthins, test$HighDev30km) #r is 0.192
cor.test(test$Nthins, test$LowDev500m) #r is 0.178
cor.test(test$Nthins, test$LowDev1km) #r is 0.075
cor.test(test$Nthins, test$LowDev5km) #r is -0.079
cor.test(test$Nthins, test$LowDev30km) #r is 0.160
cor.test(test$Nthins, test$OpenDev500m) #r is -0.060
cor.test(test$Nthins, test$OpenDev1km) #r is -0.148
cor.test(test$Nthins, test$OpenDev5km) #r is -0.278
cor.test(test$Nthins, test$OpenDev30km) #r is -0.075
cor.test(test$Nthins, test$Grass500m) #r is -0.102
cor.test(test$Nthins, test$Grass1km) #r is -0.138
cor.test(test$Nthins, test$Grass5km) #r is 0.040
cor.test(test$Nthins, test$Grass30km) #r is 0.004
cor.test(test$Nthins, test$Schrubs500m) #r is -0.074
cor.test(test$Nthins, test$Schrubs1km) #r is -0.183
cor.test(test$Nthins, test$Schrubs5km) #r is -0.295
cor.test(test$Nthins, test$Schrubs30km) #r is 0.099
cor.test(test$Nthins, test$Water500m) #r is 0.151
cor.test(test$Nthins, test$Water1km) #r is 0.038
cor.test(test$Nthins, test$Water5km) #r is -0.310
cor.test(test$Nthins, test$Water30km) #r is -0.195
cor.test(test$Nthins, test$NSoilTypes) #r is -0.058
cor.test(test$Nthins, test$FPSiteIndex)  # r is 0.276
cor.test(test$Nthins, test$SiteIndexPrimaryS)  # r is 0.261
cor.test(test$Nthins, test$PISoils)  # r is -0.134
cor.test(test$Nthins, test$SISoils)  # r is -0.100
cor.test(test$Nthins, test$HydricSoils)  # r is -0.173

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
cor.test(test$TimeSinceB, test$LCR) #r is 0.103
cor.test(test$TimeSinceB, test$HW_dens_1050) #r is -0.362
cor.test(test$TimeSinceB, test$HW_shrub) #r is -0.044
cor.test(test$TimeSinceB, test$Parea) #r is 0.030
cor.test(test$TimeSinceB, test$ShapeIndex) #r is 0.042
cor.test(test$TimeSinceB, test$PAratio) #r is -0.104
cor.test(test$TimeSinceB, test$FracDimIndex) #r is 0.030
cor.test(test$TimeSinceB, test$CoreAreaIndex) #r is 0.096
cor.test(test$TimeSinceB, test$Ag500m) #r is 0.159
cor.test(test$TimeSinceB, test$Ag1km) #r is 0.206
cor.test(test$TimeSinceB, test$Ag5km) #r is 0.149
cor.test(test$TimeSinceB, test$Ag30km) #r is 0.366
cor.test(test$TimeSinceB, test$Evergreen500m) #r is -0.238
cor.test(test$TimeSinceB, test$Evergreen1km) #r is -0.340
cor.test(test$TimeSinceB, test$Evergreen5km) #r is -0.308
cor.test(test$TimeSinceB, test$Evergreen30km) #r is -0.313
cor.test(test$TimeSinceB, test$Imperv500m) #r is 0.171
cor.test(test$TimeSinceB, test$Imperv1km) #r is -0.087
cor.test(test$TimeSinceB, test$Imperv5km) #r is 0.140
cor.test(test$TimeSinceB, test$Imperv30km) #r is -0.055
cor.test(test$TimeSinceB, test$Protected30km) #r is -0.193
cor.test(test$TimeSinceB, test$HighDev500m) #r is 0.254
cor.test(test$TimeSinceB, test$HighDev1km) #r is -0.070
cor.test(test$TimeSinceB, test$HighDev5km) #r is 0.144
cor.test(test$TimeSinceB, test$HighDev30km) #r is -0.302
cor.test(test$TimeSinceB, test$LowDev500m) #r is -0.013
cor.test(test$TimeSinceB, test$LowDev1km) #r is 0.030
cor.test(test$TimeSinceB, test$LowDev5km) #r is 0.168
cor.test(test$TimeSinceB, test$LowDev30km) #r is 0.002
cor.test(test$TimeSinceB, test$OpenDev500m) #r is 0.130
cor.test(test$TimeSinceB, test$OpenDev1km) #r is 0.206
cor.test(test$TimeSinceB, test$OpenDev5km) #r is 0.201
cor.test(test$TimeSinceB, test$OpenDev30km) #r is 0.168
cor.test(test$TimeSinceB, test$Grass500m) #r is -0.022
cor.test(test$TimeSinceB, test$Grass1km) #r is -0.032
cor.test(test$TimeSinceB, test$Grass5km) #r is 0.242
cor.test(test$TimeSinceB, test$Grass30km) #r is 0.322
cor.test(test$TimeSinceB, test$Schrubs500m) #r is 0.102
cor.test(test$TimeSinceB, test$Schrubs1km) #r is 0.067
cor.test(test$TimeSinceB, test$Schrubs5km) #r is -0.074
cor.test(test$TimeSinceB, test$Schrubs30km) #r is -0.184
cor.test(test$TimeSinceB, test$Water500m) #r is -0.220
cor.test(test$TimeSinceB, test$Water1km) #r is -0.007
cor.test(test$TimeSinceB, test$Water5km) #r is 0.150
cor.test(test$TimeSinceB, test$Water30km) #r is 0.087
cor.test(test$TimeSinceB, test$NSoilTypes) #r is 0.030
cor.test(test$TimeSinceB, test$FPSiteIndex)  # r is 0.046
cor.test(test$TimeSinceB, test$SiteIndexPrimaryS)  # r is 0.060
cor.test(test$TimeSinceB, test$PISoils)  # r is -0.171
cor.test(test$TimeSinceB, test$SISoils)  # r is 0.041
cor.test(test$TimeSinceB, test$HydricSoils)  # r is 0.098
  
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
cor.test(test$TimeSinceT, test$LCR) #r is 0.137
cor.test(test$TimeSinceT, test$HW_dens_1050) #r is -0.038
cor.test(test$TimeSinceT, test$HW_shrub) #r is -0.081
cor.test(test$TimeSinceT, test$Parea) #r is -0.071
cor.test(test$TimeSinceT, test$ShapeIndex) #r is -0.066
cor.test(test$TimeSinceT, test$PAratio) #r is 0.105
cor.test(test$TimeSinceT, test$FracDimIndex) #r is -0.016
cor.test(test$TimeSinceT, test$CoreAreaIndex) #r is -0.065
cor.test(test$TimeSinceT, test$Ag500m) #r is 0.348
cor.test(test$TimeSinceT, test$Ag1km) #r is 0.058
cor.test(test$TimeSinceT, test$Ag5km) #r is -0.173
cor.test(test$TimeSinceT, test$Ag30km) #r is -0.209
cor.test(test$TimeSinceT, test$Evergreen500m) #r is 0.014
cor.test(test$TimeSinceT, test$Evergreen1km) #r is 0.084
cor.test(test$TimeSinceT, test$Evergreen5km) #r is 0.207
cor.test(test$TimeSinceT, test$Evergreen30km) #r is 0.138
cor.test(test$TimeSinceT, test$Imperv500m) #r is 0.065
cor.test(test$TimeSinceT, test$Imperv1km) #r is -0.071
cor.test(test$TimeSinceT, test$Imperv5km) #r is -0.180
cor.test(test$TimeSinceT, test$Imperv30km) #r is -0.150
cor.test(test$TimeSinceT, test$Protected30km) #r is 0.313
cor.test(test$TimeSinceT, test$HighDev500m) #r is 0.078
cor.test(test$TimeSinceT, test$HighDev1km) #r is -0.079
cor.test(test$TimeSinceT, test$HighDev5km) #r is -0.178
cor.test(test$TimeSinceT, test$HighDev30km) #r is 0.174
cor.test(test$TimeSinceT, test$LowDev500m) #r is 0.116
cor.test(test$TimeSinceT, test$LowDev1km) #r is 0.012
cor.test(test$TimeSinceT, test$LowDev5km) #r is -0.210
cor.test(test$TimeSinceT, test$LowDev30km) #r is -0.122
cor.test(test$TimeSinceT, test$OpenDev500m) #r is 0.019
cor.test(test$TimeSinceT, test$OpenDev1km) #r is -0.024
cor.test(test$TimeSinceT, test$OpenDev5km) #r is -0.202
cor.test(test$TimeSinceT, test$OpenDev30km) #r is -0.131
cor.test(test$TimeSinceT, test$Grass500m) #r is -0.153
cor.test(test$TimeSinceT, test$Grass1km) #r is 0.058
cor.test(test$TimeSinceT, test$Grass5km) #r is -0.150
cor.test(test$TimeSinceT, test$Grass30km) #r is -0.320
cor.test(test$TimeSinceT, test$Schrubs500m) #r is -0.091
cor.test(test$TimeSinceT, test$Schrubs1km) #r is -0.062
cor.test(test$TimeSinceT, test$Schrubs5km) #r is 0.106
cor.test(test$TimeSinceT, test$Schrubs30km) #r is 0.171
cor.test(test$TimeSinceT, test$Water500m) #r is -0.175
cor.test(test$TimeSinceT, test$Water1km) #r is -0.184
cor.test(test$TimeSinceT, test$Water5km) #r is -0.114
cor.test(test$TimeSinceT, test$Water30km) #r is 0.443
cor.test(test$TimeSinceT, test$NSoilTypes) #r is 0.140
cor.test(test$TimeSinceT, test$FPSiteIndex)  # r is 0.105
cor.test(test$TimeSinceT, test$SiteIndexPrimaryS)  # r is 0.066
cor.test(test$TimeSinceT, test$PISoils)  # r is 0.156
cor.test(test$TimeSinceT, test$SISoils)  # r is -0.216
cor.test(test$TimeSinceT, test$HydricSoils)  # r is -0.048

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
cor.test(test$HWdens_10, test$LCR) #r is -0.125
cor.test(test$HWdens_10, test$HW_dens_1050) #r is 0.921
cor.test(test$HWdens_10, test$HW_shrub) #r is 0.382
cor.test(test$HWdens_10, test$Parea) #r is -0.017
cor.test(test$HWdens_10, test$ShapeIndex) #r is 0.168
cor.test(test$HWdens_10, test$PAratio) #r is -0.045
cor.test(test$HWdens_10, test$FracDimIndex) #r is 0.149
cor.test(test$HWdens_10, test$CoreAreaIndex) #r is -0.048
cor.test(test$HWdens_10, test$Ag500m) #r is -0.295
cor.test(test$HWdens_10, test$Ag1km) #r is -0.291
cor.test(test$HWdens_10, test$Ag5km) #r is -0.266
cor.test(test$HWdens_10, test$Ag30km) #r is -0.430
cor.test(test$HWdens_10, test$Evergreen500m) #r is 0.330
cor.test(test$HWdens_10, test$Evergreen1km) #r is 0.356
cor.test(test$HWdens_10, test$Evergreen5km) #r is 0.432
cor.test(test$HWdens_10, test$Evergreen30km) #r is 0.228
cor.test(test$HWdens_10, test$Imperv500m) #r is -0.0355
cor.test(test$HWdens_10, test$Imperv1km) #r is -0.051
cor.test(test$HWdens_10, test$Imperv5km) #r is -0.068
cor.test(test$HWdens_10, test$Imperv30km) #r is -0.076
cor.test(test$HWdens_10, test$Protected30km) #r is 0.315
cor.test(test$HWdens_10, test$HighDev500m) #r is -0.081
cor.test(test$HWdens_10, test$HighDev1km) #r is -0.051
cor.test(test$HWdens_10, test$HighDev5km) #r is -0.059
cor.test(test$HWdens_10, test$HighDev30km) #r is 0.378
cor.test(test$HWdens_10, test$LowDev500m) #r is -0.203
cor.test(test$HWdens_10, test$LowDev1km) #r is -0.292
cor.test(test$HWdens_10, test$LowDev5km) #r is -0.233
cor.test(test$HWdens_10, test$LowDev30km) #r is -0.093
cor.test(test$HWdens_10, test$OpenDev500m) #r is -0.236
cor.test(test$HWdens_10, test$OpenDev1km) #r is -0.346
cor.test(test$HWdens_10, test$OpenDev5km) #r is -0.392
cor.test(test$HWdens_10, test$OpenDev30km) #r is -0.395
cor.test(test$HWdens_10, test$Grass500m) #r is -0.122
cor.test(test$HWdens_10, test$Grass1km) #r is -0.072
cor.test(test$HWdens_10, test$Grass5km) #r is -0.208
cor.test(test$HWdens_10, test$Grass30km) #r is -0.210
cor.test(test$HWdens_10, test$Schrubs500m) #r is -0.026
cor.test(test$HWdens_10, test$Schrubs1km) #r is -0.093
cor.test(test$HWdens_10, test$Schrubs5km) #r is -0.064
cor.test(test$HWdens_10, test$Schrubs30km) #r is 0.303
cor.test(test$HWdens_10, test$Water500m) #r is 0.278
cor.test(test$HWdens_10, test$Water1km) #r is 0.203
cor.test(test$HWdens_10, test$Water5km) #r is -0.375
cor.test(test$HWdens_10, test$Water30km) #r is 0.043
cor.test(test$HWdens_10, test$NSoilTypes) #r is -0.071
cor.test(test$HWdens_10, test$FPSiteIndex)  # r is 0.332
cor.test(test$HWdens_10, test$SiteIndexPrimaryS)  # r is 0.228
cor.test(test$HWdens_10, test$PISoils)  # r is -0.118
cor.test(test$HWdens_10, test$SISoils)  # r is 0.111
cor.test(test$HWdens_10, test$HydricSoils)  # r is -0.234

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
cor.test(test$HWdens_50, test$LCR) #r is -0.274
cor.test(test$HWdens_50, test$HW_dens_1050) #r is 0.907
cor.test(test$HWdens_50, test$HW_shrub) #r is 0.563
cor.test(test$HWdens_50, test$Parea) #r is -0.083
cor.test(test$HWdens_50, test$ShapeIndex) #r is 0.024
cor.test(test$HWdens_50, test$PAratio) #r is -0.023
cor.test(test$HWdens_50, test$FracDimIndex) #r is 0.033
cor.test(test$HWdens_50, test$CoreAreaIndex) #r is -0.082
cor.test(test$HWdens_50, test$Ag500m) #r is -0.236
cor.test(test$HWdens_50, test$Ag1km) #r is -0.323
cor.test(test$HWdens_50, test$Ag5km) #r is -0.284
cor.test(test$HWdens_50, test$Ag30km) #r is -0.446
cor.test(test$HWdens_50, test$Evergreen500m) #r is 0.250
cor.test(test$HWdens_50, test$Evergreen1km) #r is 0.344
cor.test(test$HWdens_50, test$Evergreen5km) #r is 0.456
cor.test(test$HWdens_50, test$Evergreen30km) #r is 0.287
cor.test(test$HWdens_50, test$Imperv500m) #r is -0.062
cor.test(test$HWdens_50, test$Imperv1km) #r is -0.002
cor.test(test$HWdens_50, test$Imperv5km) #r is -0.221
cor.test(test$HWdens_50, test$Imperv30km) #r is -0.142
cor.test(test$HWdens_50, test$Protected30km) #r is 0.323
cor.test(test$HWdens_50, test$HighDev500m) #r is -0.122
cor.test(test$HWdens_50, test$HighDev1km) #r is -0.001
cor.test(test$HWdens_50, test$HighDev5km) #r is -0.212
cor.test(test$HWdens_50, test$HighDev30km) #r is 0.372
cor.test(test$HWdens_50, test$LowDev500m) #r is -0.152
cor.test(test$HWdens_50, test$LowDev1km) #r is -0.318
cor.test(test$HWdens_50, test$LowDev5km) #r is -0.325
cor.test(test$HWdens_50, test$LowDev30km) #r is -0.132
cor.test(test$HWdens_50, test$OpenDev500m) #r is -0.294
cor.test(test$HWdens_50, test$OpenDev1km) #r is -0.410
cor.test(test$HWdens_50, test$OpenDev5km) #r is -0.438
cor.test(test$HWdens_50, test$OpenDev30km) #r is -0.466
cor.test(test$HWdens_50, test$Grass500m) #r is -0.217
cor.test(test$HWdens_50, test$Grass1km) #r is -0.215
cor.test(test$HWdens_50, test$Grass5km) #r is -0.180
cor.test(test$HWdens_50, test$Grass30km) #r is -0.276
cor.test(test$HWdens_50, test$Schrubs500m) #r is 0.195
cor.test(test$HWdens_50, test$Schrubs1km) #r is 0.031
cor.test(test$HWdens_50, test$Schrubs5km) #r is 0.079
cor.test(test$HWdens_50, test$Schrubs30km) #r is 0.284
cor.test(test$HWdens_50, test$Water500m) #r is -0.029
cor.test(test$HWdens_50, test$Water1km) #r is -0.078
cor.test(test$HWdens_50, test$Water5km) #r is -0.313
cor.test(test$HWdens_50, test$Water30km) #r is 0.119
cor.test(test$HWdens_50, test$NSoilTypes) #r is -0.054
cor.test(test$HWdens_50, test$FPSiteIndex)  # r is 0.286
cor.test(test$HWdens_50, test$SiteIndexPrimaryS)  # r is 0.221
cor.test(test$HWdens_50, test$PISoils)  # r is -0.027
cor.test(test$HWdens_50, test$SISoils)  # r is 0.004
cor.test(test$HWdens_50, test$HydricSoils)  # r is -0.245

#NOT USING ANYMORE - inserted test of combined 10_50 layer:
#basically, this would replace using just the 50cm layer alone, but is it necessary?
cor.test(test$combo_1050, test$Herbicide) # r is -0.097
cor.test(test$combo_1050, test$LastB) # r is -0.089
cor.test(test$combo_1050, test$LastT) #r is -0.025
cor.test(test$combo_1050, test$BA) #r is -0.470  **  #high
cor.test(test$combo_1050, test$Nsnags) #r is -0.232
cor.test(test$combo_1050, test$Ccover) #r is -0.401  #high
cor.test(test$combo_1050, test$Ldepth) #r is -0.139
cor.test(test$combo_1050, test$TreeHt) #r is 0.195
cor.test(test$combo_1050, test$Age) #r is 0.345  #ish
cor.test(test$combo_1050, test$Nburns) #r is 0.440  #high
cor.test(test$combo_1050, test$Nthins) #r is 0.358  #high
cor.test(test$combo_1050, test$TimeSinceB) #r is -0.362 #high
cor.test(test$combo_1050, test$TimeSinceT) #r is -0.038
cor.test(test$combo_1050, test$HWdens_10) #r is 0.92  #TOTALLY but wouldnt include
cor.test(test$combo_1050, test$HWdens_50) #r is 0.91  #TOTALLY but wouldnt include
cor.test(test$combo_1050, test$HWdens_100) #r is 0.614  #yes, but wouldn't include
cor.test(test$combo_1050, test$FG_herb) #r is 0.4 but need to differentiate
cor.test(test$combo_1050, test$FG_shrub) #r is -0.292
cor.test(test$combo_1050, test$NHW_saplings) #r is 0.260
cor.test(test$combo_1050, test$NP_over_20cm) #r is -0.308
cor.test(test$combo_1050, test$Rel_HW2P_canopy) #r is 0.022
cor.test(test$combo_1050, test$Rel_HW2P_shrubcover) #r is 0.402 # high
cor.test(test$combo_1050, test$LCR) #r is -0.215
cor.test(test$combo_1050, test$HW_dens_1050) #r is ???
cor.test(test$combo_1050, test$HW_shrub) #r is 
cor.test(test$combo_1050, test$Parea) #r is 
cor.test(test$combo_1050, test$ShapeIndex) #r is 
cor.test(test$combo_1050, test$PAratio) #r is 
cor.test(test$combo_1050, test$FracDimIndex) #r is 
cor.test(test$combo_1050, test$CoreAreaIndex) #r is
cor.test(test$combo_1050, test$Ag500m) #r is 
cor.test(test$combo_1050, test$Ag1km) #r is 
cor.test(test$combo_1050, test$Ag5km) #r is 
cor.test(test$combo_1050, test$Ag30km) #r is 
cor.test(test$combo_1050, test$Evergreen500m) #r is 
cor.test(test$combo_1050, test$Evergreen1km) #r is 
cor.test(test$combo_1050, test$Evergreen5km) #r is 
cor.test(test$combo_1050, test$Evergreen30km) #r is 
cor.test(test$combo_1050, test$Imperv500m) #r is 
cor.test(test$combo_1050, test$Imperv1km) #r is 
cor.test(test$combo_1050, test$Imperv5km) #r is 
cor.test(test$combo_1050, test$Imperv30km) #r is 
cor.test(test$combo_1050, test$Protected30km) #r is 
cor.test(test$combo_1050, test$HighDev500m) #r is 
cor.test(test$combo_1050, test$HighDev1km) #r is 
cor.test(test$combo_1050, test$HighDev5km) #r is 
cor.test(test$combo_1050, test$HighDev30km) #r is 
cor.test(test$combo_1050, test$LowDev500m) #r is 
cor.test(test$combo_1050, test$LowDev1km) #r is 
cor.test(test$combo_1050, test$LowDev5km) #r is 
cor.test(test$combo_1050, test$LowDev30km) #r is 
cor.test(test$combo_1050, test$OpenDev500m) #r is 
cor.test(test$combo_1050, test$OpenDev1km) #r is 
cor.test(test$combo_1050, test$OpenDev5km) #r is 
cor.test(test$combo_1050, test$OpenDev30km) #r is 
cor.test(test$combo_1050, test$Grass500m) #r is 
cor.test(test$combo_1050, test$Grass1km) #r is 
cor.test(test$combo_1050, test$Grass5km) #r is 
cor.test(test$combo_1050, test$Grass30km) #r is
cor.test(test$combo_1050, test$Schrubs500m) #r is 
cor.test(test$combo_1050, test$Schrubs1km) #r is 
cor.test(test$combo_1050, test$Schrubs5km) #r is 
cor.test(test$combo_1050, test$Schrubs30km) #r is 
cor.test(test$combo_1050, test$Water500m) #r is 
cor.test(test$combo_1050, test$Water1km) #r is 
cor.test(test$combo_1050, test$Water5km) #r is 
cor.test(test$combo_1050, test$Water30km) #r is 
cor.test(test$combo_1050, test$NSoilTypes) #r is 
cor.test(test$combo_1050, test$FPSiteIndex)  # r is 
cor.test(test$combo_1050, test$SiteIndexPrimaryS)  # r is 
cor.test(test$combo_1050, test$PISoils)  # r is 
cor.test(test$combo_1050, test$SISoils)  # r is 
cor.test(test$combo_1050, test$HydricSoils)  # r is 
##################################################################

#HWdens_100 by each
#cor.test(test$HWdens_100, test$Treatment) #non-numeric
cor.test(test$HWdens_100, test$Herbicide) # r is -0.135
cor.test(test$HWdens_100, test$LastB) # r is -0.029
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
cor.test(test$HWdens_100, test$LCR) #r is -0.360  #odd?
cor.test(test$HWdens_100, test$HW_dens_1050) #r is 0.615
cor.test(test$HWdens_100, test$HW_shrub) #r is 0.779   #duh
cor.test(test$HWdens_100, test$Parea) #r is 0.007
cor.test(test$HWdens_100, test$ShapeIndex) #r is 0.212
cor.test(test$HWdens_100, test$PAratio) #r is -0.001
cor.test(test$HWdens_100, test$FracDimIndex) #r is 0.192
cor.test(test$HWdens_100, test$CoreAreaIndex) #r is -0.014
cor.test(test$HWdens_100, test$Ag500m) #r is -0.121
cor.test(test$HWdens_100, test$Ag1km) #r is -0.250
cor.test(test$HWdens_100, test$Ag5km) #r is -0.213
cor.test(test$HWdens_100, test$Ag30km) #r is -0.398
cor.test(test$HWdens_100, test$Evergreen500m) #r is 0.142
cor.test(test$HWdens_100, test$Evergreen1km) #r is 0.164
cor.test(test$HWdens_100, test$Evergreen5km) #r is 0.341
cor.test(test$HWdens_100, test$Evergreen30km) #r is 0.236
cor.test(test$HWdens_100, test$Imperv500m) #r is -0.114
cor.test(test$HWdens_100, test$Imperv1km) #r is -0.106
cor.test(test$HWdens_100, test$Imperv5km) #r is -0.286
cor.test(test$HWdens_100, test$Imperv30km) #r is -0.080
cor.test(test$HWdens_100, test$Protected30km) #r is 0.319
cor.test(test$HWdens_100, test$HighDev500m) #r is -0.058
cor.test(test$HWdens_100, test$HighDev1km) #r is -0.113
cor.test(test$HWdens_100, test$HighDev5km) #r is -0.280
cor.test(test$HWdens_100, test$HighDev30km) #r is 0.261
cor.test(test$HWdens_100, test$LowDev500m) #r is 0.007
cor.test(test$HWdens_100, test$LowDev1km) #r is -0.179
cor.test(test$HWdens_100, test$LowDev5km) #r is -0.298
cor.test(test$HWdens_100, test$LowDev30km) #r is -0.069
cor.test(test$HWdens_100, test$OpenDev500m) #r is -0.279
cor.test(test$HWdens_100, test$OpenDev1km) #r is -0.386
cor.test(test$HWdens_100, test$OpenDev5km) #r is -0.398
cor.test(test$HWdens_100, test$OpenDev30km) #r is -0.357
cor.test(test$HWdens_100, test$Grass500m) #r is -0.314
cor.test(test$HWdens_100, test$Grass1km) #r is -0.117
cor.test(test$HWdens_100, test$Grass5km) #r is -0.214
cor.test(test$HWdens_100, test$Grass30km) #r is -0.240
cor.test(test$HWdens_100, test$Schrubs500m) #r is 0.051
cor.test(test$HWdens_100, test$Schrubs1km) #r is 0.097
cor.test(test$HWdens_100, test$Schrubs5km) #r is 0.146
cor.test(test$HWdens_100, test$Schrubs30km) #r is 0.186
cor.test(test$HWdens_100, test$Water500m) #r is -0.034
cor.test(test$HWdens_100, test$Water1km) #r is -0.040
cor.test(test$HWdens_100, test$Water5km) #r is -0.203
cor.test(test$HWdens_100, test$Water30km) #r is 0.055
cor.test(test$HWdens_100, test$NSoilTypes) #r is 0.118
cor.test(test$HWdens_100, test$FPSiteIndex)  # r is 0.056
cor.test(test$HWdens_100, test$SiteIndexPrimaryS)  # r is -0.029
cor.test(test$HWdens_100, test$PISoils)  # r is -0.191
cor.test(test$HWdens_100, test$SISoils)  # r is 0.045
cor.test(test$HWdens_100, test$HydricSoils)  # r is -0.082

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
cor.test(test$FG_herb, test$LCR) #r is 0.079
cor.test(test$FG_herb, test$HW_dens_1050) #r is 0.406
cor.test(test$FG_herb, test$HW_shrub) #r is 0.212
cor.test(test$FG_herb, test$Parea) #r is 0.210
cor.test(test$FG_herb, test$ShapeIndex) #r is 0.031
cor.test(test$FG_herb, test$PAratio) #r is -0.147
cor.test(test$FG_herb, test$FracDimIndex) #r is -0.015
cor.test(test$FG_herb, test$CoreAreaIndex) #r is 0.129
cor.test(test$FG_herb, test$Ag500m) #r is -0.242
cor.test(test$FG_herb, test$Ag1km) #r is -0.387
cor.test(test$FG_herb, test$Ag5km) #r is -0.585
cor.test(test$FG_herb, test$Ag30km) #r is -0.523
cor.test(test$FG_herb, test$Evergreen500m) #r is 0.287
cor.test(test$FG_herb, test$Evergreen1km) #r is 0.509
cor.test(test$FG_herb, test$Evergreen5km) #r is 0.601
cor.test(test$FG_herb, test$Evergreen30km) #r is 0.630
cor.test(test$FG_herb, test$Imperv500m) #r is -0.163
cor.test(test$FG_herb, test$Imperv1km) #r is -0.289
cor.test(test$FG_herb, test$Imperv5km) #r is -0.349
cor.test(test$FG_herb, test$Imperv30km) #r is -0.104
cor.test(test$FG_herb, test$Protected30km) #r is 0.313
cor.test(test$FG_herb, test$HighDev500m) #r is -0.206
cor.test(test$FG_herb, test$HighDev1km) #r is -0.273
cor.test(test$FG_herb, test$HighDev5km) #r is -0.338
cor.test(test$FG_herb, test$HighDev30km) #r is 0.401
cor.test(test$FG_herb, test$LowDev500m) #r is -0.328
cor.test(test$FG_herb, test$LowDev1km) #r is -0.393
cor.test(test$FG_herb, test$LowDev5km) #r is -0.386
cor.test(test$FG_herb, test$LowDev30km) #r is -0.236
cor.test(test$FG_herb, test$OpenDev500m) #r is -0.347
cor.test(test$FG_herb, test$OpenDev1km) #r is -0.397
cor.test(test$FG_herb, test$OpenDev5km) #r is -0.204
cor.test(test$FG_herb, test$OpenDev30km) #r is -0.291
cor.test(test$FG_herb, test$Grass500m) #r is 0.019
cor.test(test$FG_herb, test$Grass1km) #r is -0.023
cor.test(test$FG_herb, test$Grass5km) #r is -0.401
cor.test(test$FG_herb, test$Grass30km) #r is -0.518
cor.test(test$FG_herb, test$Schrubs500m) #r is 0.228
cor.test(test$FG_herb, test$Schrubs1km) #r is 0.178
cor.test(test$FG_herb, test$Schrubs5km) #r is 0.419
cor.test(test$FG_herb, test$Schrubs30km) #r is 0.514
cor.test(test$FG_herb, test$Water500m) #r is 0.164
cor.test(test$FG_herb, test$Water1km) #r is 0.042
cor.test(test$FG_herb, test$Water5km) #r is 0.223
cor.test(test$FG_herb, test$Water30km) #r is 0.224
cor.test(test$FG_herb, test$NSoilTypes) #r is 0.041
cor.test(test$FG_herb, test$FPSiteIndex)  # r is 0.110
cor.test(test$FG_herb, test$SiteIndexPrimaryS)  # r is 0.075
cor.test(test$FG_herb, test$PISoils)  # r is 0.106
cor.test(test$FG_herb, test$SISoils)  # r is 0.038
cor.test(test$FG_herb, test$HydricSoils)  # r is 0.093

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
cor.test(test$FG_shrub, test$LCR) #r is 0.254
cor.test(test$FG_shrub, test$HW_dens_1050) #r is 0.093
cor.test(test$FG_shrub, test$HW_shrub) #r is -0.167
cor.test(test$FG_shrub, test$Parea) #r is 0.337
cor.test(test$FG_shrub, test$ShapeIndex) #r is 0.103
cor.test(test$FG_shrub, test$PAratio) #r is -0.199
cor.test(test$FG_shrub, test$FracDimIndex) #r is 0.049
cor.test(test$FG_shrub, test$CoreAreaIndex) #r is 0.261
cor.test(test$FG_shrub, test$Ag500m) #r is -0.181
cor.test(test$FG_shrub, test$Ag1km) #r is -0.098
cor.test(test$FG_shrub, test$Ag5km) #r is -0.158
cor.test(test$FG_shrub, test$Ag30km) #r is 0.036
cor.test(test$FG_shrub, test$Evergreen500m) #r is 0.111
cor.test(test$FG_shrub, test$Evergreen1km) #r is 0.069
cor.test(test$FG_shrub, test$Evergreen5km) #r is -0.062
cor.test(test$FG_shrub, test$Evergreen30km) #r is 0.221
cor.test(test$FG_shrub, test$Imperv500m) #r is -0.052
cor.test(test$FG_shrub, test$Imperv1km) #r is -0.160
cor.test(test$FG_shrub, test$Imperv5km) #r is -0.186
cor.test(test$FG_shrub, test$Imperv30km) #r is -0.050
cor.test(test$FG_shrub, test$Protected30km) #r is -0.079
cor.test(test$FG_shrub, test$HighDev500m) #r is -0.100
cor.test(test$FG_shrub, test$HighDev1km) #r is -0.139
cor.test(test$FG_shrub, test$HighDev5km) #r is -0.183
cor.test(test$FG_shrub, test$HighDev30km) #r is -0.139
cor.test(test$FG_shrub, test$LowDev500m) #r is -0.184
cor.test(test$FG_shrub, test$LowDev1km) #r is -0.089
cor.test(test$FG_shrub, test$LowDev5km) #r is -0.125
cor.test(test$FG_shrub, test$LowDev30km) #r is -0.014
cor.test(test$FG_shrub, test$OpenDev500m) #r is 0.030
cor.test(test$FG_shrub, test$OpenDev1km) #r is 0.068
cor.test(test$FG_shrub, test$OpenDev5km) #r is 0.200
cor.test(test$FG_shrub, test$OpenDev30km) #r is 0.161
cor.test(test$FG_shrub, test$Grass500m) #r is 0.251
cor.test(test$FG_shrub, test$Grass1km) #r is 0.137
cor.test(test$FG_shrub, test$Grass5km) #r is -0.048
cor.test(test$FG_shrub, test$Grass30km) #r is -0.057
cor.test(test$FG_shrub, test$Schrubs500m) #r is 0.140
cor.test(test$FG_shrub, test$Schrubs1km) #r is 0.175
cor.test(test$FG_shrub, test$Schrubs5km) #r is 0.372
cor.test(test$FG_shrub, test$Schrubs30km) #r is 0.097
cor.test(test$FG_shrub, test$Water500m) #r is -0.250
cor.test(test$FG_shrub, test$Water1km) #r is -0.077
cor.test(test$FG_shrub, test$Water5km) #r is 0.508
cor.test(test$FG_shrub, test$Water30km) #r is -0.008
cor.test(test$FG_shrub, test$NSoilTypes) #r is 0.190
cor.test(test$FG_shrub, test$FPSiteIndex)  # r is -0.168
cor.test(test$FG_shrub, test$SiteIndexPrimaryS)  # r is -0.055
cor.test(test$FG_shrub, test$PISoils)  # r is 0.041
cor.test(test$FG_shrub, test$SISoils)  # r is -0.046
cor.test(test$FG_shrub, test$HydricSoils)  # r is 0.202

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
cor.test(test$NHW_saplings, test$HWdens_100) #r is 0.522   #high
cor.test(test$NHW_saplings, test$FG_herb)   #r is -0.065
cor.test(test$NHW_saplings, test$FG_shrub)  #r is -0.224
#cor.test(test$NHW_saplings, test$NHW_saplings)  #r is 
cor.test(test$NHW_saplings, test$NP_over_20cm)  #r is -0.187
cor.test(test$NHW_saplings, test$Rel_HW2P_canopy) #r is 0.501   #high
cor.test(test$NHW_saplings, test$Rel_HW2P_shrubcover) #r is 0.305
cor.test(test$NHW_saplings, test$LCR) #r is -0.360 #ish
cor.test(test$NHW_saplings, test$HW_dens_1050) #r is 0.260
cor.test(test$NHW_saplings, test$HW_shrub) #r is 0.526
cor.test(test$NHW_saplings, test$Parea) #r is 0.012
cor.test(test$NHW_saplings, test$ShapeIndex) #r is 0.283
cor.test(test$NHW_saplings, test$PAratio) #r is -0.039
cor.test(test$NHW_saplings, test$FracDimIndex) #r is 0.227
cor.test(test$NHW_saplings, test$CoreAreaIndex) #r is -0.041
cor.test(test$NHW_saplings, test$Ag500m) #r is -0.053
cor.test(test$NHW_saplings, test$Ag1km) #r is -0.150
cor.test(test$NHW_saplings, test$Ag5km) #r is 0.013
cor.test(test$NHW_saplings, test$Ag30km) #r is -0.002
cor.test(test$NHW_saplings, test$Evergreen500m) #r is 0.051
cor.test(test$NHW_saplings, test$Evergreen1km) #r is -0.051
cor.test(test$NHW_saplings, test$Evergreen5km) #r is 0.064
cor.test(test$NHW_saplings, test$Evergreen30km) #r is -0.116
cor.test(test$NHW_saplings, test$Imperv500m) #r is -0.041
cor.test(test$NHW_saplings, test$Imperv1km) #r is -0.039
cor.test(test$NHW_saplings, test$Imperv5km) #r is -0.160
cor.test(test$NHW_saplings, test$Imperv30km) #r is -0.097
cor.test(test$NHW_saplings, test$Protected30km) #r is 0.036
cor.test(test$NHW_saplings, test$HighDev500m) #r is 0.018
cor.test(test$NHW_saplings, test$HighDev1km) #r is -0.057
cor.test(test$NHW_saplings, test$HighDev5km) #r is -0.157
cor.test(test$NHW_saplings, test$HighDev30km) #r is 0.021
cor.test(test$NHW_saplings, test$LowDev500m) #r is 0.057
cor.test(test$NHW_saplings, test$LowDev1km) #r is -0.135
cor.test(test$NHW_saplings, test$LowDev5km) #r is -0.143
cor.test(test$NHW_saplings, test$LowDev30km) #r is -0.079
cor.test(test$NHW_saplings, test$OpenDev500m) #r is -0.100
cor.test(test$NHW_saplings, test$OpenDev1km) #r is -0.247
cor.test(test$NHW_saplings, test$OpenDev5km) #r is -0.257
cor.test(test$NHW_saplings, test$OpenDev30km) #r is -0.141
cor.test(test$NHW_saplings, test$Grass500m) #r is -0.264
cor.test(test$NHW_saplings, test$Grass1km) #r is 0.017
cor.test(test$NHW_saplings, test$Grass5km) #r is -0.076
cor.test(test$NHW_saplings, test$Grass30km) #r is -0.094
cor.test(test$NHW_saplings, test$Schrubs500m) #r is 0.039
cor.test(test$NHW_saplings, test$Schrubs1km) #r is 0.159
cor.test(test$NHW_saplings, test$Schrubs5km) #r is 0.112
cor.test(test$NHW_saplings, test$Schrubs30km) #r is 0.112
cor.test(test$NHW_saplings, test$Water500m) #r is -0.046
cor.test(test$NHW_saplings, test$Water1km) #r is 0.087
cor.test(test$NHW_saplings, test$Water5km) #r is -0.126
cor.test(test$NHW_saplings, test$Water30km) #r is 0.027
cor.test(test$NHW_saplings, test$NSoilTypes) #r is 0.178
cor.test(test$NHW_saplings, test$FPSiteIndex)  # r is -0.138
cor.test(test$NHW_saplings, test$SiteIndexPrimaryS)  # r is -0.241
cor.test(test$NHW_saplings, test$PISoils)  # r is -0.172
cor.test(test$NHW_saplings, test$SISoils)  # r is -0.001
cor.test(test$NHW_saplings, test$HydricSoils)  # r is -0.065

#inserted test of HW_Shrub(2) layer:
#basically, this would be a cover-based alternative to the count-based NHW_saplings layer
cor.test(test$HW_shrub, test$Herbicide) # r is -0.366  #ish
cor.test(test$HW_shrub, test$LastB) # r is -0.112
cor.test(test$HW_shrub, test$LastT) #r is 0.131
cor.test(test$HW_shrub, test$BA) #r is -0.145
cor.test(test$HW_shrub, test$Nsnags) #r is -0.062
cor.test(test$HW_shrub, test$Ccover) #r is -0.026
cor.test(test$HW_shrub, test$Ldepth) #r is 0.003
cor.test(test$HW_shrub, test$TreeHt) #r is 0.332  #ish
cor.test(test$HW_shrub, test$Age) #r is 0.139
cor.test(test$HW_shrub, test$Nburns) #r is -0.018
cor.test(test$HW_shrub, test$Nthins) #r is -0.036
cor.test(test$HW_shrub, test$TimeSinceB) #r is -0.044
cor.test(test$HW_shrub, test$TimeSinceT) #r is -0.081
cor.test(test$HW_shrub, test$HWdens_10) #r is 0.382  #ish but wouldnt use
cor.test(test$HW_shrub, test$HWdens_50) #r is 0.563  #high
cor.test(test$HW_shrub, test$HWdens_100) #r is 0.778  #highly - but wouldnt use
cor.test(test$HW_shrub, test$HW_dens_1050)  #r is 0.511 #high
cor.test(test$HW_shrub, test$FG_herb) #r is 0.212
cor.test(test$HW_shrub, test$FG_shrub) #r is -0.167
cor.test(test$HW_shrub, test$NHW_saplings) #r is 0.526 #wouldn't include
cor.test(test$HW_shrub, test$NP_over_20cm) #r is -0.096
cor.test(test$HW_shrub, test$Rel_HW2P_canopy) #r is 0.442 #high
cor.test(test$HW_shrub, test$Rel_HW2P_shrubcover) #r is 0.477 #high but scrapped variable
cor.test(test$HW_shrub, test$LCR) #r is -0.328  #ish
cor.test(test$HW_shrub, test$HW_dens_1050) #r is 0.512
#cor.test(test$HW_shrub, test$HW_shrub) #r is 
cor.test(test$HW_shrub, test$Parea) #r is 0.254
cor.test(test$HW_shrub, test$ShapeIndex) #r is 0.205
cor.test(test$HW_shrub, test$PAratio) #r is -0.186
cor.test(test$HW_shrub, test$FracDimIndex) #r is 0.150
cor.test(test$HW_shrub, test$CoreAreaIndex) #r is 0.203
cor.test(test$HW_shrub, test$Ag500m) #r is -0.113
cor.test(test$HW_shrub, test$Ag1km) #r is -0.210
cor.test(test$HW_shrub, test$Ag5km) #r is -0.165
cor.test(test$HW_shrub, test$Ag30km) #r is -0.233
cor.test(test$HW_shrub, test$Evergreen500m) #r is 0.108
cor.test(test$HW_shrub, test$Evergreen1km) #r is 0.096
cor.test(test$HW_shrub, test$Evergreen5km) #r is 0.190
cor.test(test$HW_shrub, test$Evergreen30km) #r is 0.154
cor.test(test$HW_shrub, test$Imperv500m) #r is -0.038
cor.test(test$HW_shrub, test$Imperv1km) #r is -0.175
cor.test(test$HW_shrub, test$Imperv5km) #r is -0.284
cor.test(test$HW_shrub, test$Imperv30km) #r is -0.167
cor.test(test$HW_shrub, test$Protected30km) #r is 0.195
cor.test(test$HW_shrub, test$HighDev500m) #r is 0.043
cor.test(test$HW_shrub, test$HighDev1km) #r is -0.181
cor.test(test$HW_shrub, test$HighDev5km) #r is -0.278
cor.test(test$HW_shrub, test$HighDev30km) #r is 0.117
cor.test(test$HW_shrub, test$LowDev500m) #r is -0.104
cor.test(test$HW_shrub, test$LowDev1km) #r is -0.283
cor.test(test$HW_shrub, test$LowDev5km) #r is -0.321
cor.test(test$HW_shrub, test$LowDev30km) #r is -0.175
cor.test(test$HW_shrub, test$OpenDev500m) #r is -0.303
cor.test(test$HW_shrub, test$OpenDev1km) #r is -0.457
cor.test(test$HW_shrub, test$OpenDev5km) #r is -0.387
cor.test(test$HW_shrub, test$OpenDev30km) #r is -0.271
cor.test(test$HW_shrub, test$Grass500m) #r is -0.318
cor.test(test$HW_shrub, test$Grass1km) #r is -0.143
cor.test(test$HW_shrub, test$Grass5km) #r is -0.071
cor.test(test$HW_shrub, test$Grass30km) #r is -0.031
cor.test(test$HW_shrub, test$Schrubs500m) #r is 0.070
cor.test(test$HW_shrub, test$Schrubs1km) #r is 0.021
cor.test(test$HW_shrub, test$Schrubs5km) #r is 0.127
cor.test(test$HW_shrub, test$Schrubs30km) #r is 0.189
cor.test(test$HW_shrub, test$Water500m) #r is -0.062
cor.test(test$HW_shrub, test$Water1km) #r is -0.087
cor.test(test$HW_shrub, test$Water5km) #r is -0.211
cor.test(test$HW_shrub, test$Water30km) #r is -0.014
cor.test(test$HW_shrub, test$NSoilTypes) #r is 0.252
cor.test(test$HW_shrub, test$FPSiteIndex)  # r is 0.087
cor.test(test$HW_shrub, test$SiteIndexPrimaryS)  # r is 0.052
cor.test(test$HW_shrub, test$PISoils)  # r is -0.187
cor.test(test$HW_shrub, test$SISoils)  # r is -0.034
cor.test(test$HW_shrub, test$HydricSoils)  # r is 0.078


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
cor.test(test$NP_over_20cm, test$LCR) #r is -0.021
cor.test(test$NP_over_20cm, test$HW_dens_1050) #r is -0.308
cor.test(test$NP_over_20cm, test$HW_shrub) #r is -0.097
cor.test(test$NP_over_20cm, test$Parea) #r is 0.244
cor.test(test$NP_over_20cm, test$ShapeIndex) #r is 0.016
cor.test(test$NP_over_20cm, test$PAratio) #r is -0.130
cor.test(test$NP_over_20cm, test$FracDimIndex) #r is -0.034
cor.test(test$NP_over_20cm, test$CoreAreaIndex) #r is 0.210
cor.test(test$NP_over_20cm, test$Ag500m) #r is 0.192
cor.test(test$NP_over_20cm, test$Ag1km) #r is 0.340
cor.test(test$NP_over_20cm, test$Ag5km) #r is 0.345
cor.test(test$NP_over_20cm, test$Ag30km) #r is 0.379
cor.test(test$NP_over_20cm, test$Evergreen500m) #r is -0.294
cor.test(test$NP_over_20cm, test$Evergreen1km) #r is -0.402
cor.test(test$NP_over_20cm, test$Evergreen5km) #r is -0.394
cor.test(test$NP_over_20cm, test$Evergreen30km) #r is -0.249
cor.test(test$NP_over_20cm, test$Imperv500m) #r is -0.114
cor.test(test$NP_over_20cm, test$Imperv1km) #r is -0.078
cor.test(test$NP_over_20cm, test$Imperv5km) #r is 0.313
cor.test(test$NP_over_20cm, test$Imperv30km) #r is 0.121
cor.test(test$NP_over_20cm, test$Protected30km) #r is -0.290
cor.test(test$NP_over_20cm, test$HighDev500m) #r is 0.021
cor.test(test$NP_over_20cm, test$HighDev1km) #r is -0.051
cor.test(test$NP_over_20cm, test$HighDev5km) #r is 0.313
cor.test(test$NP_over_20cm, test$HighDev30km) #r is -0.371
cor.test(test$NP_over_20cm, test$LowDev500m) #r is -0.171
cor.test(test$NP_over_20cm, test$LowDev1km) #r is -0.007
cor.test(test$NP_over_20cm, test$LowDev5km) #r is 0.373
cor.test(test$NP_over_20cm, test$LowDev30km) #r is 0.147
cor.test(test$NP_over_20cm, test$OpenDev500m) #r is -0.085
cor.test(test$NP_over_20cm, test$OpenDev1km) #r is -0.038
cor.test(test$NP_over_20cm, test$OpenDev5km) #r is -0.303
cor.test(test$NP_over_20cm, test$OpenDev30km) #r is 0.324
cor.test(test$NP_over_20cm, test$Grass500m) #r is 0.013
cor.test(test$NP_over_20cm, test$Grass1km) #r is 0.147
cor.test(test$NP_over_20cm, test$Grass5km) #r is 0.350
cor.test(test$NP_over_20cm, test$Grass30km) #r is 0.501
cor.test(test$NP_over_20cm, test$Schrubs500m) #r is -0.115
cor.test(test$NP_over_20cm, test$Schrubs1km) #r is -0.172
cor.test(test$NP_over_20cm, test$Schrubs5km) #r is -0.177
cor.test(test$NP_over_20cm, test$Schrubs30km) #r is -0.223
cor.test(test$NP_over_20cm, test$Water500m) #r is -0.158
cor.test(test$NP_over_20cm, test$Water1km) #r is -0.122
cor.test(test$NP_over_20cm, test$Water5km) #r is 0.152
cor.test(test$NP_over_20cm, test$Water30km) #r is -0.132
cor.test(test$NP_over_20cm, test$NSoilTypes) #r is 0.030
cor.test(test$NP_over_20cm, test$FPSiteIndex)  # r is -0.027
cor.test(test$NP_over_20cm, test$SiteIndexPrimaryS)  # r is 0.104
cor.test(test$NP_over_20cm, test$PISoils)  # r is 0.033
cor.test(test$NP_over_20cm, test$SISoils)  # r is -0.043
cor.test(test$NP_over_20cm, test$HydricSoils)  # r is 0.267


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
cor.test(test$Rel_HW2P_canopy, test$LCR) #r is -0.120
cor.test(test$Rel_HW2P_canopy, test$HW_dens_1050) #r is 0.022
cor.test(test$Rel_HW2P_canopy, test$HW_shrub) #r is 0.442
cor.test(test$Rel_HW2P_canopy, test$Parea) #r is 0.264
cor.test(test$Rel_HW2P_canopy, test$ShapeIndex) #r is 0.118
cor.test(test$Rel_HW2P_canopy, test$PAratio) #r is -0.289
cor.test(test$Rel_HW2P_canopy, test$FracDimIndex) #r is 0.046
cor.test(test$Rel_HW2P_canopy, test$CoreAreaIndex) #r is 0.240
cor.test(test$Rel_HW2P_canopy, test$Ag500m) #r is -0.053
cor.test(test$Rel_HW2P_canopy, test$Ag1km) #r is -0.214
cor.test(test$Rel_HW2P_canopy, test$Ag5km) #r is -0.104
cor.test(test$Rel_HW2P_canopy, test$Ag30km) #r is 0.072
cor.test(test$Rel_HW2P_canopy, test$Evergreen500m) #r is -0.083
cor.test(test$Rel_HW2P_canopy, test$Evergreen1km) #r is -0.184
cor.test(test$Rel_HW2P_canopy, test$Evergreen5km) #r is -0.075
cor.test(test$Rel_HW2P_canopy, test$Evergreen30km) #r is -0.042
cor.test(test$Rel_HW2P_canopy, test$Imperv500m) #r is 0.149
cor.test(test$Rel_HW2P_canopy, test$Imperv1km) #r is -0.087
cor.test(test$Rel_HW2P_canopy, test$Imperv5km) #r is -0.130
cor.test(test$Rel_HW2P_canopy, test$Imperv30km) #r is -0.035
cor.test(test$Rel_HW2P_canopy, test$Protected30km) #r is -0.052
cor.test(test$Rel_HW2P_canopy, test$HighDev500m) #r is 0.268
cor.test(test$Rel_HW2P_canopy, test$HighDev1km) #r is -0.110
cor.test(test$Rel_HW2P_canopy, test$HighDev5km) #r is -0.130
cor.test(test$Rel_HW2P_canopy, test$HighDev30km) #r is -0.090
cor.test(test$Rel_HW2P_canopy, test$LowDev500m) #r is 0.018
cor.test(test$Rel_HW2P_canopy, test$LowDev1km) #r is -0.168
cor.test(test$Rel_HW2P_canopy, test$LowDev5km) #r is -0.147
cor.test(test$Rel_HW2P_canopy, test$LowDev30km) #r is -0.008
cor.test(test$Rel_HW2P_canopy, test$OpenDev500m) #r is -0.011
cor.test(test$Rel_HW2P_canopy, test$OpenDev1km) #r is -0.060
cor.test(test$Rel_HW2P_canopy, test$OpenDev5km) #r is -0.090
cor.test(test$Rel_HW2P_canopy, test$OpenDev30km) #r is -0.020
cor.test(test$Rel_HW2P_canopy, test$Grass500m) #r is -0.007
cor.test(test$Rel_HW2P_canopy, test$Grass1km) #r is 0.172
cor.test(test$Rel_HW2P_canopy, test$Grass5km) #r is 0.159
cor.test(test$Rel_HW2P_canopy, test$Grass30km) #r is 0.160
cor.test(test$Rel_HW2P_canopy, test$Schrubs500m) #r is -0.050
cor.test(test$Rel_HW2P_canopy, test$Schrubs1km) #r is 0.042
cor.test(test$Rel_HW2P_canopy, test$Schrubs5km) #r is 0.065
cor.test(test$Rel_HW2P_canopy, test$Schrubs30km) #r is 0.048
cor.test(test$Rel_HW2P_canopy, test$Water500m) #r is -0.063
cor.test(test$Rel_HW2P_canopy, test$Water1km) #r is 0.122
cor.test(test$Rel_HW2P_canopy, test$Water5km) #r is 0.109
cor.test(test$Rel_HW2P_canopy, test$Water30km) #r is 0.100
cor.test(test$Rel_HW2P_canopy, test$NSoilTypes) #r is 0.228
cor.test(test$Rel_HW2P_canopy, test$FPSiteIndex)  # r is -0.073
cor.test(test$Rel_HW2P_canopy, test$SiteIndexPrimaryS)  # r is -0.079
cor.test(test$Rel_HW2P_canopy, test$PISoils)  # r is -0.252
cor.test(test$Rel_HW2P_canopy, test$SISoils)  # r is -0.132
cor.test(test$Rel_HW2P_canopy, test$HydricSoils)  # r is 0.270

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
cor.test(test$Rel_HW2P_shrubcover, test$LCR) #r is -0.097
cor.test(test$Rel_HW2P_shrubcover, test$HW_dens_1050) #r is 0.403
cor.test(test$Rel_HW2P_shrubcover, test$HW_shrub) #r is 0.477
cor.test(test$Rel_HW2P_shrubcover, test$Parea) #r is 0.187
cor.test(test$Rel_HW2P_shrubcover, test$ShapeIndex) #r is -0.040
cor.test(test$Rel_HW2P_shrubcover, test$PAratio) #r is -0.228
cor.test(test$Rel_HW2P_shrubcover, test$FracDimIndex) #r is -0.105
cor.test(test$Rel_HW2P_shrubcover, test$CoreAreaIndex) #r is 0.259
cor.test(test$Rel_HW2P_shrubcover, test$Ag500m) #r is -0.225
cor.test(test$Rel_HW2P_shrubcover, test$Ag1km) #r is -0.266
cor.test(test$Rel_HW2P_shrubcover, test$Ag5km) #r is -0.162
cor.test(test$Rel_HW2P_shrubcover, test$Ag30km) #r is -0.260
cor.test(test$Rel_HW2P_shrubcover, test$Evergreen500m) #r is 0.112
cor.test(test$Rel_HW2P_shrubcover, test$Evergreen1km) #r is 0.165
cor.test(test$Rel_HW2P_shrubcover, test$Evergreen5km) #r is 0.234
cor.test(test$Rel_HW2P_shrubcover, test$Evergreen30km) #r is 0.186
cor.test(test$Rel_HW2P_shrubcover, test$Imperv500m) #r is -0.096
cor.test(test$Rel_HW2P_shrubcover, test$Imperv1km) #r is -0.124
cor.test(test$Rel_HW2P_shrubcover, test$Imperv5km) #r is -0.349
cor.test(test$Rel_HW2P_shrubcover, test$Imperv30km) #r is -0.140
cor.test(test$Rel_HW2P_shrubcover, test$Protected30km) #r is 0.063
cor.test(test$Rel_HW2P_shrubcover, test$HighDev500m) #r is 0.108
cor.test(test$Rel_HW2P_shrubcover, test$HighDev1km) #r is -0.133
cor.test(test$Rel_HW2P_shrubcover, test$HighDev5km) #r is -0.345
cor.test(test$Rel_HW2P_shrubcover, test$HighDev30km) #r is 0.304
cor.test(test$Rel_HW2P_shrubcover, test$LowDev500m) #r is 0.087
cor.test(test$Rel_HW2P_shrubcover, test$LowDev1km) #r is -0.152
cor.test(test$Rel_HW2P_shrubcover, test$LowDev5km) #r is -0.372
cor.test(test$Rel_HW2P_shrubcover, test$LowDev30km) #r is -0.151
cor.test(test$Rel_HW2P_shrubcover, test$OpenDev500m) #r is -0.236
cor.test(test$Rel_HW2P_shrubcover, test$OpenDev1km) #r is -0.236
cor.test(test$Rel_HW2P_shrubcover, test$OpenDev5km) #r is -0.330
cor.test(test$Rel_HW2P_shrubcover, test$OpenDev30km) #r is -0.347
cor.test(test$Rel_HW2P_shrubcover, test$Grass500m) #r is -0.050
cor.test(test$Rel_HW2P_shrubcover, test$Grass1km) #r is -0.028
cor.test(test$Rel_HW2P_shrubcover, test$Grass5km) #r is -0.007
cor.test(test$Rel_HW2P_shrubcover, test$Grass30km) #r is -0.108
cor.test(test$Rel_HW2P_shrubcover, test$Schrubs500m) #r is 0.112
cor.test(test$Rel_HW2P_shrubcover, test$Schrubs1km) #r is 0.149
cor.test(test$Rel_HW2P_shrubcover, test$Schrubs5km) #r is 0.155
cor.test(test$Rel_HW2P_shrubcover, test$Schrubs30km) #r is 0.378
cor.test(test$Rel_HW2P_shrubcover, test$Water500m) #r is 0.185
cor.test(test$Rel_HW2P_shrubcover, test$Water1km) #r is 0.369
cor.test(test$Rel_HW2P_shrubcover, test$Water5km) #r is 0.024
cor.test(test$Rel_HW2P_shrubcover, test$Water30km) #r is -0.007
cor.test(test$Rel_HW2P_shrubcover, test$NSoilTypes) #r is -0.008
cor.test(test$Rel_HW2P_shrubcover, test$FPSiteIndex)  # r is 0.119
cor.test(test$Rel_HW2P_shrubcover, test$SiteIndexPrimaryS)  # r is -0.026
cor.test(test$Rel_HW2P_shrubcover, test$PISoils)  # r is -0.005
cor.test(test$Rel_HW2P_shrubcover, test$SISoils)  # r is -0.131
cor.test(test$Rel_HW2P_shrubcover, test$HydricSoils)  # r is 0.115

#LCR by each
#cor.test(test$LCR, test$Treatment) #non-numeric
cor.test(test$LCR, test$Herbicide) # r is 0.006
cor.test(test$LCR, test$LastB) # r is 0.312
cor.test(test$LCR, test$LastT) #r is 0.053
cor.test(test$LCR, test$BA)     #r is 0.142
cor.test(test$LCR, test$Nsnags) #r is -0.091
cor.test(test$LCR, test$Ccover) #r is -0.041
cor.test(test$LCR, test$Ldepth) #r is -0.207
cor.test(test$LCR, test$TreeHt) #r is -0.630  #CORRELATED ! ACK!
cor.test(test$LCR, test$Age)    #r is -0.383  #ish
cor.test(test$LCR, test$Nburns) #r is -0.106
cor.test(test$LCR, test$Nthins)  #r is -0.359  #ish
cor.test(test$LCR, test$TimeSinceB) #r is 0.103
cor.test(test$LCR, test$TimeSinceT) #r is 0.137
cor.test(test$LCR, test$HWdens_10)  #r is -0.125
cor.test(test$LCR, test$HWdens_50) #r is -0.274
cor.test(test$LCR, test$HWdens_100) #r is -0.360  #ish
cor.test(test$LCR, test$FG_herb)   #r is 0.079
cor.test(test$LCR, test$FG_shrub)  #r is 0.254
cor.test(test$LCR, test$NHW_saplings)  #r is -0.360  #ish
cor.test(test$LCR, test$NP_over_20cm)  #r is -0.021
cor.test(test$LCR, test$Rel_HW2P_canopy) #r is -0.120
cor.test(test$LCR, test$Rel_HW2P_shrubcover) #r is -0.097
#cor.test(test$LCR, test$LCR) #r is 
cor.test(test$LCR, test$HW_dens_1050) #r is -0.215
cor.test(test$LCR, test$HW_shrub) #r is -0.328
cor.test(test$LCR, test$Parea) #r is -0.125
cor.test(test$LCR, test$ShapeIndex) #r is -0.244
cor.test(test$LCR, test$PAratio) #r is 0.066
cor.test(test$LCR, test$FracDimIndex) #r is -0.228
cor.test(test$LCR, test$CoreAreaIndex) #r is -0.066
cor.test(test$LCR, test$Ag500m) #r is -0.037
cor.test(test$LCR, test$Ag1km) #r is -0.061
cor.test(test$LCR, test$Ag5km) #r is -0.249
cor.test(test$LCR, test$Ag30km) #r is -0.029
cor.test(test$LCR, test$Evergreen500m) #r is -0.222
cor.test(test$LCR, test$Evergreen1km) #r is 0.059
cor.test(test$LCR, test$Evergreen5km) #r is 0.070
cor.test(test$LCR, test$Evergreen30km) #r is 0.270
cor.test(test$LCR, test$Imperv500m) #r is 0.020
cor.test(test$LCR, test$Imperv1km) #r is -0.090
cor.test(test$LCR, test$Imperv5km) #r is -0.105
cor.test(test$LCR, test$Imperv30km) #r is 0.007
cor.test(test$LCR, test$Protected30km) #r is -0.112
cor.test(test$LCR, test$HighDev500m) #r is 0.059
cor.test(test$LCR, test$HighDev1km) #r is -0.092
cor.test(test$LCR, test$HighDev5km) #r is 0.101
cor.test(test$LCR, test$HighDev30km) #r is 0.021
cor.test(test$LCR, test$LowDev500m) #r is -0.045
cor.test(test$LCR, test$LowDev1km) #r is 0.125
cor.test(test$LCR, test$LowDev5km) #r is 0.151
cor.test(test$LCR, test$LowDev30km) #r is -0.078
cor.test(test$LCR, test$OpenDev500m) #r is -0.043
cor.test(test$LCR, test$OpenDev1km) #r is 0.182
cor.test(test$LCR, test$OpenDev5km) #r is 0.446
cor.test(test$LCR, test$OpenDev30km) #r is 0.142
cor.test(test$LCR, test$Grass500m) #r is 0.518
cor.test(test$LCR, test$Grass1km) #r is 0.326
cor.test(test$LCR, test$Grass5km) #r is -0.148
cor.test(test$LCR, test$Grass30km) #r is -0.283
cor.test(test$LCR, test$Schrubs500m) #r is 0.381
cor.test(test$LCR, test$Schrubs1km) #r is 0.367
cor.test(test$LCR, test$Schrubs5km) #r is 0.370
cor.test(test$LCR, test$Schrubs30km) #r is 0.077
cor.test(test$LCR, test$Water500m) #r is 0.043
cor.test(test$LCR, test$Water1km) #r is 0.227
cor.test(test$LCR, test$Water5km) #r is 0.455
cor.test(test$LCR, test$Water30km) #r is -0.083
cor.test(test$LCR, test$NSoilTypes) #r is -0.115
cor.test(test$LCR, test$FPSiteIndex)  # r is -0.379
cor.test(test$LCR, test$SiteIndexPrimaryS)  # r is -0.387
cor.test(test$LCR, test$PISoils)  # r is 0.217
cor.test(test$LCR, test$SISoils)  # r is -0.111
cor.test(test$LCR, test$HydricSoils)  # r is 0.062

#HW_dens_1050 by each
#cor.test(test$HW_dens_1050, test$Treatment) #non-numeric
cor.test(test$HW_dens_1050, test$Herbicide) # r is -0.097
cor.test(test$HW_dens_1050, test$LastB) # r is -0.089
cor.test(test$HW_dens_1050, test$LastT) #r is -0.025
cor.test(test$HW_dens_1050, test$BA)     #r is -0.470
cor.test(test$HW_dens_1050, test$Nsnags) #r is -0.232
cor.test(test$HW_dens_1050, test$Ccover) #r is -0.401
cor.test(test$HW_dens_1050, test$Ldepth) #r is -0.139
cor.test(test$HW_dens_1050, test$TreeHt) #r is 0.195
cor.test(test$HW_dens_1050, test$Age)    #r is 0.345
cor.test(test$HW_dens_1050, test$Nburns) #r is 0.440
cor.test(test$HW_dens_1050, test$Nthins)  #r is 0.358
cor.test(test$HW_dens_1050, test$TimeSinceB) #r is -0.362
cor.test(test$HW_dens_1050, test$TimeSinceT) #r is -0.038
cor.test(test$HW_dens_1050, test$HWdens_10)  #r is 0.921
cor.test(test$HW_dens_1050, test$HWdens_50) #r is 0.907
cor.test(test$HW_dens_1050, test$HWdens_100) #r is 0.615
cor.test(test$HW_dens_1050, test$FG_herb)   #r is 0.406
cor.test(test$HW_dens_1050, test$FG_shrub)  #r is -0.292
cor.test(test$HW_dens_1050, test$NHW_saplings)  #r is 0.260
cor.test(test$HW_dens_1050, test$NP_over_20cm)  #r is -0.308
cor.test(test$HW_dens_1050, test$Rel_HW2P_canopy) #r is 0.022
cor.test(test$HW_dens_1050, test$Rel_HW2P_shrubcover) #r is 0.403
cor.test(test$HW_dens_1050, test$LCR) #r is -0.215
#cor.test(test$HW_dens_1050, test$HW_dens_1050) #r is 
cor.test(test$HW_dens_1050, test$HW_shrub) #r is 0.512
cor.test(test$HW_dens_1050, test$Parea) #r is -0.055
cor.test(test$HW_dens_1050, test$ShapeIndex) #r is 0.108
cor.test(test$HW_dens_1050, test$PAratio) #r is -0.036
cor.test(test$HW_dens_1050, test$FracDimIndex) #r is 0.103
cor.test(test$HW_dens_1050, test$CoreAreaIndex) #r is -0.073
cor.test(test$HW_dens_1050, test$Ag500m) #r is -0.290
cor.test(test$HW_dens_1050, test$Ag1km) #r is -0.336
cor.test(test$HW_dens_1050, test$Ag5km) #r is -0.300
cor.test(test$HW_dens_1050, test$Ag30km) #r is -0.479
cor.test(test$HW_dens_1050, test$Evergreen500m) #r is 0.320
cor.test(test$HW_dens_1050, test$Evergreen1km) #r is 0.383
cor.test(test$HW_dens_1050, test$Evergreen5km) #r is 0.486
cor.test(test$HW_dens_1050, test$Evergreen30km) #r is 0.282
cor.test(test$HW_dens_1050, test$Imperv500m) #r is -0.053
cor.test(test$HW_dens_1050, test$Imperv1km) #r is -0.030
cor.test(test$HW_dens_1050, test$Imperv5km) #r is -0.154
cor.test(test$HW_dens_1050, test$Imperv30km) #r is -0.118
cor.test(test$HW_dens_1050, test$Protected30km) #r is 0.350
cor.test(test$HW_dens_1050, test$HighDev500m) #r is -0.110
cor.test(test$HW_dens_1050, test$HighDev1km) #r is -0.029
cor.test(test$HW_dens_1050, test$HighDev5km) #r is -0.144
cor.test(test$HW_dens_1050, test$HighDev30km) #r is 0.411
cor.test(test$HW_dens_1050, test$LowDev500m) #r is -0.194
cor.test(test$HW_dens_1050, test$LowDev1km) #r is -0.332
cor.test(test$HW_dens_1050, test$LowDev5km) #r is -0.302
cor.test(test$HW_dens_1050, test$LowDev30km) #r is -0.122
cor.test(test$HW_dens_1050, test$OpenDev500m) #r is -0.287
cor.test(test$HW_dens_1050, test$OpenDev1km) #r is -0.411
cor.test(test$HW_dens_1050, test$OpenDev5km) #r is -0.453
cor.test(test$HW_dens_1050, test$OpenDev30km) #r is -0.470
cor.test(test$HW_dens_1050, test$Grass500m) #r is -0.184
cor.test(test$HW_dens_1050, test$Grass1km) #r is -0.153
cor.test(test$HW_dens_1050, test$Grass5km) #r is -0.212
cor.test(test$HW_dens_1050, test$Grass30km) #r is -0.265
cor.test(test$HW_dens_1050, test$Schrubs500m) #r is 0.088
cor.test(test$HW_dens_1050, test$Schrubs1km) #r is -0.037
cor.test(test$HW_dens_1050, test$Schrubs5km) #r is 0.005
cor.test(test$HW_dens_1050, test$Schrubs30km) #r is 0.322
cor.test(test$HW_dens_1050, test$Water500m) #r is 0.143
cor.test(test$HW_dens_1050, test$Water1km) #r is 0.074
cor.test(test$HW_dens_1050, test$Water5km) #r is -0.377
cor.test(test$HW_dens_1050, test$Water30km) #r is 0.087
cor.test(test$HW_dens_1050, test$NSoilTypes) #r is -0.068
cor.test(test$HW_dens_1050, test$FPSiteIndex)  # r is 0.339
cor.test(test$HW_dens_1050, test$SiteIndexPrimaryS)  # r is 0.245
cor.test(test$HW_dens_1050, test$PISoils)  # r is -0.080
cor.test(test$HW_dens_1050, test$SISoils)  # r is 0.067
cor.test(test$HW_dens_1050, test$HydricSoils)  # r is -0.263


#HW_shrub by each
#cor.test(test$HW_shrub, test$Treatment) #non-numeric
cor.test(test$HW_shrub, test$Herbicide) # r is -0.366
cor.test(test$HW_shrub, test$LastB) # r is -0.112
cor.test(test$HW_shrub, test$LastT) #r is 0.131
cor.test(test$HW_shrub, test$BA)     #r is -0.145
cor.test(test$HW_shrub, test$Nsnags) #r is -0.062
cor.test(test$HW_shrub, test$Ccover) #r is -0.026
cor.test(test$HW_shrub, test$Ldepth) #r is 0.004
cor.test(test$HW_shrub, test$TreeHt) #r is 0.332
cor.test(test$HW_shrub, test$Age)    #r is 0.139
cor.test(test$HW_shrub, test$Nburns) #r is -0.018
cor.test(test$HW_shrub, test$Nthins)  #r is -0.036
cor.test(test$HW_shrub, test$TimeSinceB) #r is -0.044
cor.test(test$HW_shrub, test$TimeSinceT) #r is -0.081
cor.test(test$HW_shrub, test$HWdens_10)  #r is 0.382
cor.test(test$HW_shrub, test$HWdens_50) #r is 0.563
cor.test(test$HW_shrub, test$HWdens_100) #r is 0.779
cor.test(test$HW_shrub, test$FG_herb)   #r is 0.212
cor.test(test$HW_shrub, test$FG_shrub)  #r is -0.167
cor.test(test$HW_shrub, test$NHW_saplings)  #r is 0.526
cor.test(test$HW_shrub, test$NP_over_20cm)  #r is -0.097
cor.test(test$HW_shrub, test$Rel_HW2P_canopy) #r is 0.442
cor.test(test$HW_shrub, test$Rel_HW2P_shrubcover) #r is 0.477
cor.test(test$HW_shrub, test$LCR) #r is -0.328
cor.test(test$HW_shrub, test$HW_dens_1050) #r is 0.512
#cor.test(test$HW_shrub, test$HW_shrub) #r is 
cor.test(test$HW_shrub, test$Parea) #r is 0.254
cor.test(test$HW_shrub, test$ShapeIndex) #r is 0.205
cor.test(test$HW_shrub, test$PAratio) #r is -0.186
cor.test(test$HW_shrub, test$FracDimIndex) #r is 0.150
cor.test(test$HW_shrub, test$CoreAreaIndex) #r is 0.203
cor.test(test$HW_shrub, test$Ag500m) #r is -0.113
cor.test(test$HW_shrub, test$Ag1km) #r is -0.210
cor.test(test$HW_shrub, test$Ag5km) #r is -0.165
cor.test(test$HW_shrub, test$Ag30km) #r is -0.233
cor.test(test$HW_shrub, test$Evergreen500m) #r is 0.108
cor.test(test$HW_shrub, test$Evergreen1km) #r is 0.096
cor.test(test$HW_shrub, test$Evergreen5km) #r is 0.190
cor.test(test$HW_shrub, test$Evergreen30km) #r is 0.154
cor.test(test$HW_shrub, test$Imperv500m) #r is -0.038
cor.test(test$HW_shrub, test$Imperv1km) #r is -0.175
cor.test(test$HW_shrub, test$Imperv5km) #r is -0.284
cor.test(test$HW_shrub, test$Imperv30km) #r is -0.167
cor.test(test$HW_shrub, test$Protected30km) #r is 0.195
cor.test(test$HW_shrub, test$HighDev500m) #r is 0.043
cor.test(test$HW_shrub, test$HighDev1km) #r is -0.181
cor.test(test$HW_shrub, test$HighDev5km) #r is -0.278
cor.test(test$HW_shrub, test$HighDev30km) #r is 0.117
cor.test(test$HW_shrub, test$LowDev500m) #r is -0.104
cor.test(test$HW_shrub, test$LowDev1km) #r is -0.283
cor.test(test$HW_shrub, test$LowDev5km) #r is -0.321
cor.test(test$HW_shrub, test$LowDev30km) #r is -0.175
cor.test(test$HW_shrub, test$OpenDev500m) #r is -0.303
cor.test(test$HW_shrub, test$OpenDev1km) #r is -0.457
cor.test(test$HW_shrub, test$OpenDev5km) #r is -0.387
cor.test(test$HW_shrub, test$OpenDev30km) #r is -0.271
cor.test(test$HW_shrub, test$Grass500m) #r is -0.318
cor.test(test$HW_shrub, test$Grass1km) #r is -0.143
cor.test(test$HW_shrub, test$Grass5km) #r is -0.071
cor.test(test$HW_shrub, test$Grass30km) #r is -0.031
cor.test(test$HW_shrub, test$Schrubs500m) #r is 0.070
cor.test(test$HW_shrub, test$Schrubs1km) #r is 0.021
cor.test(test$HW_shrub, test$Schrubs5km) #r is 0.127
cor.test(test$HW_shrub, test$Schrubs30km) #r is 0.189
cor.test(test$HW_shrub, test$Water500m) #r is -0.062
cor.test(test$HW_shrub, test$Water1km) #r is -0.087
cor.test(test$HW_shrub, test$Water5km) #r is -0.211
cor.test(test$HW_shrub, test$Water30km) #r is -0.014
cor.test(test$HW_shrub, test$NSoilTypes) #r is 0.252
cor.test(test$HW_shrub, test$FPSiteIndex)  # r is 0.087
cor.test(test$HW_shrub, test$SiteIndexPrimaryS)  # r is 0.052
cor.test(test$HW_shrub, test$PISoils)  # r is -0.187
cor.test(test$HW_shrub, test$SISoils)  # r is -0.034
cor.test(test$HW_shrub, test$HydricSoils)  # r is 0.078

#Parea by each
#cor.test(test$Parea, test$Treatment) #non-numeric
#cor.test(test$Parea, test$Parea) #r is 
cor.test(test$Parea, test$ShapeIndex) #r is 0.271
cor.test(test$Parea, test$PAratio) #r is -0.775
cor.test(test$Parea, test$FracDimIndex) #r is 0.042
cor.test(test$Parea, test$CoreAreaIndex) #r is 0.926
cor.test(test$Parea, test$Ag500m) #r is -0.252
cor.test(test$Parea, test$Ag1km) #r is -0.179
cor.test(test$Parea, test$Ag5km) #r is -0.084
cor.test(test$Parea, test$Ag30km) #r is 0.091
cor.test(test$Parea, test$Evergreen500m) #r is 0.336
cor.test(test$Parea, test$Evergreen1km) #r is 0.149
cor.test(test$Parea, test$Evergreen5km) #r is -0.074
cor.test(test$Parea, test$Evergreen30km) #r is 0.097
cor.test(test$Parea, test$Imperv500m) #r is 0.106
cor.test(test$Parea, test$Imperv1km) #r is -0.113
cor.test(test$Parea, test$Imperv5km) #r is -0.200
cor.test(test$Parea, test$Imperv30km) #r is -0.026
cor.test(test$Parea, test$Protected30km) #r is -0.166
cor.test(test$Parea, test$HighDev500m) #r is 0.196
cor.test(test$Parea, test$HighDev1km) #r is -0.111
cor.test(test$Parea, test$HighDev5km) #r is -0.195
cor.test(test$Parea, test$HighDev30km) #r is -0.141
cor.test(test$Parea, test$LowDev500m) #r is -0.183
cor.test(test$Parea, test$LowDev1km) #r is -0.358
cor.test(test$Parea, test$LowDev5km) #r is -0.178
cor.test(test$Parea, test$LowDev30km) #r is -0.101
cor.test(test$Parea, test$OpenDev500m) #r is -0.268
cor.test(test$Parea, test$OpenDev1km) #r is -0.278
cor.test(test$Parea, test$OpenDev5km) #r is -0.031
cor.test(test$Parea, test$OpenDev30km) #r is 0.120
cor.test(test$Parea, test$Grass500m) #r is -0.040
cor.test(test$Parea, test$Grass1km) #r is 0.184
cor.test(test$Parea, test$Grass5km) #r is 0.180
cor.test(test$Parea, test$Grass30km) #r is 0.142
cor.test(test$Parea, test$Schrubs500m) #r is -0.116
cor.test(test$Parea, test$Schrubs1km) #r is -0.135
cor.test(test$Parea, test$Schrubs5km) #r is 0.142
cor.test(test$Parea, test$Schrubs30km) #r is 0.257
cor.test(test$Parea, test$Water500m) #r is -0.147
cor.test(test$Parea, test$Water1km) #r is -0.201
cor.test(test$Parea, test$Water5km) #r is 0.171
cor.test(test$Parea, test$Water30km) #r is -0.080
cor.test(test$Parea, test$NSoilTypes) #r is 0.561
cor.test(test$Parea, test$FPSiteIndex)  # r is 0.223
cor.test(test$Parea, test$SiteIndexPrimaryS)  # r is 0.145
cor.test(test$Parea, test$PISoils)  # r is 0.029
cor.test(test$Parea, test$SISoils)  # r is -0.373
cor.test(test$Parea, test$HydricSoils)  # r is 0.617


#ShapeIndex by each
#cor.test(test$ShapeIndex, test$Treatment) #non-numeric
#cor.test(test$ShapeIndex, test$ShapeIndex)
cor.test(test$ShapeIndex, test$PAratio) #r is 0.058
cor.test(test$ShapeIndex, test$FracDimIndex) #r is 0.936   # can't use these both!
cor.test(test$ShapeIndex, test$CoreAreaIndex) #r is 0.025
cor.test(test$ShapeIndex, test$Ag500m) #r is 0.136
cor.test(test$ShapeIndex, test$Ag1km) #r is -0.004
cor.test(test$ShapeIndex, test$Ag5km) #r is -0.091
cor.test(test$ShapeIndex, test$Ag30km) #r is -0.001
cor.test(test$ShapeIndex, test$Evergreen500m) #r is 0.152
cor.test(test$ShapeIndex, test$Evergreen1km) #r is -0.061
cor.test(test$ShapeIndex, test$Evergreen5km) #r is -0.003
cor.test(test$ShapeIndex, test$Evergreen30km) #r is -0.087
cor.test(test$ShapeIndex, test$Imperv500m) #r is 0.019
cor.test(test$ShapeIndex, test$Imperv1km) #r is -0.212
cor.test(test$ShapeIndex, test$Imperv5km) #r is 0.126
cor.test(test$ShapeIndex, test$Imperv30km) #r is 0.108
cor.test(test$ShapeIndex, test$Protected30km) #r is 0.013
cor.test(test$ShapeIndex, test$HighDev500m) #r is -0.051
cor.test(test$ShapeIndex, test$HighDev1km) #r is -0.223
cor.test(test$ShapeIndex, test$HighDev5km) #r is 0.140
cor.test(test$ShapeIndex, test$HighDev30km) #r is -0.029
cor.test(test$ShapeIndex, test$LowDev500m) #r is 0.114
cor.test(test$ShapeIndex, test$LowDev1km) #r is -0.080
cor.test(test$ShapeIndex, test$LowDev5km) #r is 0.130
cor.test(test$ShapeIndex, test$LowDev30km) #r is 0.106
cor.test(test$ShapeIndex, test$OpenDev500m) #r is 0.015
cor.test(test$ShapeIndex, test$OpenDev1km) #r is -0.124
cor.test(test$ShapeIndex, test$OpenDev5km) #r is 0.028
cor.test(test$ShapeIndex, test$OpenDev30km) #r is 0.037
cor.test(test$ShapeIndex, test$Grass500m) #r is -0.225
cor.test(test$ShapeIndex, test$Grass1km) #r is 0.204
cor.test(test$ShapeIndex, test$Grass5km) #r is 0.023
cor.test(test$ShapeIndex, test$Grass30km) #r is -0.043
cor.test(test$ShapeIndex, test$Schrubs500m) #r is -0.071
cor.test(test$ShapeIndex, test$Schrubs1km) #r is 0.082
cor.test(test$ShapeIndex, test$Schrubs5km) #r is 0.112
cor.test(test$ShapeIndex, test$Schrubs30km) #r is 0.045
cor.test(test$ShapeIndex, test$Water500m) #r is 0.036
cor.test(test$ShapeIndex, test$Water1km) #r is 0.154
cor.test(test$ShapeIndex, test$Water5km) #r is -0.036
cor.test(test$ShapeIndex, test$Water30km) #r is 0.065
cor.test(test$ShapeIndex, test$NSoilTypes) #r is 0.427
cor.test(test$ShapeIndex, test$FPSiteIndex)  # r is 0.100
cor.test(test$ShapeIndex, test$SiteIndexPrimaryS)  # r is 0.023
cor.test(test$ShapeIndex, test$PISoils)  # r is -0.176
cor.test(test$ShapeIndex, test$SISoils)  # r is -0.010
cor.test(test$ShapeIndex, test$HydricSoils)  # r is 0.052

#PAratio by each
#cor.test(test$PAratio, test$Treatment) #non-numeric
#cor.test(test$PAratio, test$PAratio)
cor.test(test$PAratio, test$FracDimIndex) #r is 0.330
cor.test(test$PAratio, test$CoreAreaIndex) #r is -0.825
cor.test(test$PAratio, test$Ag500m) #r is 0.478
cor.test(test$PAratio, test$Ag1km) #r is 0.325
cor.test(test$PAratio, test$Ag5km) #r is 0.072
cor.test(test$PAratio, test$Ag30km) #r is -0.135
cor.test(test$PAratio, test$Evergreen500m) #r is -0.331
cor.test(test$PAratio, test$Evergreen1km) #r is -0.176
cor.test(test$PAratio, test$Evergreen5km) #r is 0.081
cor.test(test$PAratio, test$Evergreen30km) #r is -0.141
cor.test(test$PAratio, test$Imperv500m) #r is -0.002
cor.test(test$PAratio, test$Imperv1km) #r is 0.0140
cor.test(test$PAratio, test$Imperv5km) #r is 0.159
cor.test(test$PAratio, test$Imperv30km) #r is 0.031
cor.test(test$PAratio, test$Protected30km) #r is 0.128
cor.test(test$PAratio, test$HighDev500m) #r is -0.186
cor.test(test$PAratio, test$HighDev1km) #r is 0.009
cor.test(test$PAratio, test$HighDev5km) #r is 0.162
cor.test(test$PAratio, test$HighDev30km) #r is 0.222
cor.test(test$PAratio, test$LowDev500m) #r is 0.329
cor.test(test$PAratio, test$LowDev1km) #r is 0.407
cor.test(test$PAratio, test$LowDev5km) #r is 0.109
cor.test(test$PAratio, test$LowDev30km) #r is 0.100
cor.test(test$PAratio, test$OpenDev500m) #r is 0.264
cor.test(test$PAratio, test$OpenDev1km) #r is 0.151
cor.test(test$PAratio, test$OpenDev5km) #r is -0.090
cor.test(test$PAratio, test$OpenDev30km) #r is -0.125
cor.test(test$PAratio, test$Grass500m) #r is -0.053
cor.test(test$PAratio, test$Grass1km) #r is -0.055
cor.test(test$PAratio, test$Grass5km) #r is -0.158
cor.test(test$PAratio, test$Grass30km) #r is -0.097
cor.test(test$PAratio, test$Schrubs500m) #r is -0.026
cor.test(test$PAratio, test$Schrubs1km) #r is 0.048
cor.test(test$PAratio, test$Schrubs5km) #r is -0.145
cor.test(test$PAratio, test$Schrubs30km) #r is -0.174
cor.test(test$PAratio, test$Water500m) #r is 0.197
cor.test(test$PAratio, test$Water1km) #r is 0.258
cor.test(test$PAratio, test$Water5km) #r is -0.168
cor.test(test$PAratio, test$Water30km) #r is 0.044
cor.test(test$PAratio, test$NSoilTypes) #r is -0.459
cor.test(test$PAratio, test$FPSiteIndex)  # r is -0.183
cor.test(test$PAratio, test$SiteIndexPrimaryS)  # r is -0.070
cor.test(test$PAratio, test$PISoils)  # r is -0.011
cor.test(test$PAratio, test$SISoils)  # r is 0.313
cor.test(test$PAratio, test$HydricSoils)  # r is -0.502

#FracDimIndex by each
#cor.test(test$FracDimIndex, test$Treatment) #non-numeric
#cor.test(test$FracDimIndex, test$FracDimIndex) 
cor.test(test$FracDimIndex, test$CoreAreaIndex) #r is -0.185
cor.test(test$FracDimIndex, test$Ag500m) #r is 0.255
cor.test(test$FracDimIndex, test$Ag1km) #r is 0.044
cor.test(test$FracDimIndex, test$Ag5km) #r is -0.101
cor.test(test$FracDimIndex, test$Ag30km) #r is -0.052
cor.test(test$FracDimIndex, test$Evergreen500m) #r is 0.110
cor.test(test$FracDimIndex, test$Evergreen1km) #r is -0.042
cor.test(test$FracDimIndex, test$Evergreen5km) #r is 0.042
cor.test(test$FracDimIndex, test$Evergreen30km) #r is -0.076
cor.test(test$FracDimIndex, test$Imperv500m) #r is 0.028
cor.test(test$FracDimIndex, test$Imperv1km) #r is -0.184
cor.test(test$FracDimIndex, test$Imperv5km) #r is 0.150
cor.test(test$FracDimIndex, test$Imperv30km) #r is 0.091
cor.test(test$FracDimIndex, test$Protected30km) #r is 0.060
cor.test(test$FracDimIndex, test$HighDev500m) #r is -0.095
cor.test(test$FracDimIndex, test$HighDev1km) #r is -0.189
cor.test(test$FracDimIndex, test$HighDev5km) #r is 0.165
cor.test(test$FracDimIndex, test$HighDev30km) #r is 0.031
cor.test(test$FracDimIndex, test$LowDev500m) #r is 0.181
cor.test(test$FracDimIndex, test$LowDev1km) #r is 0.020
cor.test(test$FracDimIndex, test$LowDev5km) #r is 0.119
cor.test(test$FracDimIndex, test$LowDev30km) #r is 0.110
cor.test(test$FracDimIndex, test$OpenDev500m) #r is 0.112
cor.test(test$FracDimIndex, test$OpenDev1km) #r is -0.058
cor.test(test$FracDimIndex, test$OpenDev5km) #r is -0.012
cor.test(test$FracDimIndex, test$OpenDev30km) #r is -0.022
cor.test(test$FracDimIndex, test$Grass500m) #r is -0.273
cor.test(test$FracDimIndex, test$Grass1km) #r is 0.087
cor.test(test$FracDimIndex, test$Grass5km) #r is -0.064
cor.test(test$FracDimIndex, test$Grass30km) #r is -0.066
cor.test(test$FracDimIndex, test$Schrubs500m) #r is -0.086
cor.test(test$FracDimIndex, test$Schrubs1km) #r is 0.068
cor.test(test$FracDimIndex, test$Schrubs5km) #r is 0.046
cor.test(test$FracDimIndex, test$Schrubs30km) #r is -0.036
cor.test(test$FracDimIndex, test$Water500m) #r is 0.098
cor.test(test$FracDimIndex, test$Water1km) #r is 0.175
cor.test(test$FracDimIndex, test$Water5km) #r is -0.088
cor.test(test$FracDimIndex, test$Water30km) #r is 0.066
cor.test(test$FracDimIndex, test$NSoilTypes) #r is 0.280
cor.test(test$FracDimIndex, test$FPSiteIndex)  # r is 0.026
cor.test(test$FracDimIndex, test$SiteIndexPrimaryS)  # r is -0.006
cor.test(test$FracDimIndex, test$PISoils)  # r is -0.160
cor.test(test$FracDimIndex, test$SISoils)  # r is 0.105
cor.test(test$FracDimIndex, test$HydricSoils)  # r is -0.091

#CoreAreaIndex by each
#cor.test(test$CoreAreaIndex, test$Treatment) #non-numeric
#cor.test(test$CoreAreaIndex, test$CoreAreaIndex) #r is 
cor.test(test$CoreAreaIndex, test$Ag500m) #r is -0.321
cor.test(test$CoreAreaIndex, test$Ag1km) #r is -0.201
cor.test(test$CoreAreaIndex, test$Ag5km) #r is -0.050
cor.test(test$CoreAreaIndex, test$Ag30km) #r is 0.086
cor.test(test$CoreAreaIndex, test$Evergreen500m) #r is 0.316
cor.test(test$CoreAreaIndex, test$Evergreen1km) #r is 0.186
cor.test(test$CoreAreaIndex, test$Evergreen5km) #r is -0.054
cor.test(test$CoreAreaIndex, test$Evergreen30km) #r is 0.101
cor.test(test$CoreAreaIndex, test$Imperv500m) #r is 0.131
cor.test(test$CoreAreaIndex, test$Imperv1km) #r is -0.020
cor.test(test$CoreAreaIndex, test$Imperv5km) #r is -0.238
cor.test(test$CoreAreaIndex, test$Imperv30km) #r is -0.045
cor.test(test$CoreAreaIndex, test$Protected30km) #r is -0.156
cor.test(test$CoreAreaIndex, test$HighDev500m) #r is 0.241
cor.test(test$CoreAreaIndex, test$HighDev1km) #r is -0.016
cor.test(test$CoreAreaIndex, test$HighDev5km) #r is -0.237
cor.test(test$CoreAreaIndex, test$HighDev30km) #r is -0.111
cor.test(test$CoreAreaIndex, test$LowDev500m) #r is -0.172
cor.test(test$CoreAreaIndex, test$LowDev1km) #r is -0.306
cor.test(test$CoreAreaIndex, test$LowDev5km) #r is -0.212
cor.test(test$CoreAreaIndex, test$LowDev30km) #r is -0.110
cor.test(test$CoreAreaIndex, test$OpenDev500m) #r is -0.280
cor.test(test$CoreAreaIndex, test$OpenDev1km) #r is -0.244
cor.test(test$CoreAreaIndex, test$OpenDev5km) #r is -0.066
cor.test(test$CoreAreaIndex, test$OpenDev30km) #r is 0.810
cor.test(test$CoreAreaIndex, test$Grass500m) #r is -0.000
cor.test(test$CoreAreaIndex, test$Grass1km) #r is 0.065
cor.test(test$CoreAreaIndex, test$Grass5km) #r is 0.184
cor.test(test$CoreAreaIndex, test$Grass30km) #r is 0.168
cor.test(test$CoreAreaIndex, test$Schrubs500m) #r is -0.070
cor.test(test$CoreAreaIndex, test$Schrubs1km) #r is -0.150
cor.test(test$CoreAreaIndex, test$Schrubs5km) #r is 0.106
cor.test(test$CoreAreaIndex, test$Schrubs30km) #r is 0.261
cor.test(test$CoreAreaIndex, test$Water500m) #r is -0.104
cor.test(test$CoreAreaIndex, test$Water1km) #r is -0.228
cor.test(test$CoreAreaIndex, test$Water5km) #r is 0.158
cor.test(test$CoreAreaIndex, test$Water30km) #r is -0.086 
cor.test(test$CoreAreaIndex, test$NSoilTypes) #r is 0.481
cor.test(test$CoreAreaIndex, test$FPSiteIndex)  # r is 0.266
cor.test(test$CoreAreaIndex, test$SiteIndexPrimaryS)  # r is 0.150
cor.test(test$CoreAreaIndex, test$PISoils)  # r is 0.071
cor.test(test$CoreAreaIndex, test$SISoils)  # r is -0.382
cor.test(test$CoreAreaIndex, test$HydricSoils)  # r is 0.629

#Ag500m by each
#cor.test(test$Ag500m, test$Treatment) #non-numeric
#cor.test(test$Ag500m, test$Ag500m)
cor.test(test$Ag500m, test$Ag1km) #r is 0.761 # duh
cor.test(test$Ag500m, test$Ag5km) #r is 0.263 #huh...
cor.test(test$Ag500m, test$Ag30km) #r is 0.133
cor.test(test$Ag500m, test$Evergreen500m) #r is -0.433 
cor.test(test$Ag500m, test$Evergreen1km) #r is -0.330
cor.test(test$Ag500m, test$Evergreen5km) #r is -0.161
cor.test(test$Ag500m, test$Evergreen30km) #r is -0.194
cor.test(test$Ag500m, test$Imperv500m) #r is -0.111
cor.test(test$Ag500m, test$Imperv1km) #r is -0.025
cor.test(test$Ag500m, test$Imperv5km) #r is 0.218
cor.test(test$Ag500m, test$Imperv30km) #r is -0.026
cor.test(test$Ag500m, test$Protected30km) #r is -0.019
cor.test(test$Ag500m, test$HighDev500m) #r is -0.073
cor.test(test$Ag500m, test$HighDev1km) #r is -0.040
cor.test(test$Ag500m, test$HighDev5km) #r is 0.222
cor.test(test$Ag500m, test$HighDev30km) #r is -0.049
cor.test(test$Ag500m, test$LowDev500m) #r is 0.480
cor.test(test$Ag500m, test$LowDev1km) #r is 0.427
cor.test(test$Ag500m, test$LowDev5km) #r is 0.264
cor.test(test$Ag500m, test$LowDev30km) #r is 0.059
cor.test(test$Ag500m, test$OpenDev500m) #r is 0.219
cor.test(test$Ag500m, test$OpenDev1km) #r is 0.153
cor.test(test$Ag500m, test$OpenDev5km) #r is 0.022
cor.test(test$Ag500m, test$OpenDev30km) #r is 0.086
cor.test(test$Ag500m, test$Grass500m) #r is -0.146
cor.test(test$Ag500m, test$Grass1km) #r is 0.050
cor.test(test$Ag500m, test$Grass5km) #r is 0.084
cor.test(test$Ag500m, test$Grass30km) #r is 0.101
cor.test(test$Ag500m, test$Schrubs500m) #r is -0.211
cor.test(test$Ag500m, test$Schrubs1km) #r is -0.280
cor.test(test$Ag500m, test$Schrubs5km) #r is -0.269
cor.test(test$Ag500m, test$Schrubs30km) #r is -0.167
cor.test(test$Ag500m, test$Water500m) #r is 0.131
cor.test(test$Ag500m, test$Water1km) #r is 0.009
cor.test(test$Ag500m, test$Water5km) #r is -0.247
cor.test(test$Ag500m, test$Water30km) #r is 0.021
cor.test(test$Ag500m, test$NSoilTypes) #r is -0.122
cor.test(test$Ag500m, test$FPSiteIndex)  # r is 0.009
cor.test(test$Ag500m, test$SiteIndexPrimaryS)  # r is 0.034
cor.test(test$Ag500m, test$PISoils)  # r is 0.075
cor.test(test$Ag500m, test$SISoils)  # r is 0.037
cor.test(test$Ag500m, test$HydricSoils)  # r is -0.226


#Ag1km by each
#cor.test(test$Ag1km, test$Treatment) #non-numeric
#cor.test(test$Ag1km, test$Ag1km)
cor.test(test$Ag1km, test$Ag5km) #r is 0.623
cor.test(test$Ag1km, test$Ag30km) #r is 0.425
cor.test(test$Ag1km, test$Evergreen500m) #r is -0.440
cor.test(test$Ag1km, test$Evergreen1km) #r is -0.552
cor.test(test$Ag1km, test$Evergreen5km) #r is -0.465
cor.test(test$Ag1km, test$Evergreen30km) #r is -0.455
cor.test(test$Ag1km, test$Imperv500m) #r is 0.013
cor.test(test$Ag1km, test$Imperv1km) #r is -0.003
cor.test(test$Ag1km, test$Imperv5km) #r is 0.263
cor.test(test$Ag1km, test$Imperv30km) #r is 0.075
cor.test(test$Ag1km, test$Protected30km) #r is -0.348
cor.test(test$Ag1km, test$HighDev500m) #r is 0.077
cor.test(test$Ag1km, test$HighDev1km) #r is -0.007
cor.test(test$Ag1km, test$HighDev5km) #r is 0.263
cor.test(test$Ag1km, test$HighDev30km) #r is -0.263
cor.test(test$Ag1km, test$LowDev500m) #r is 0.369
cor.test(test$Ag1km, test$LowDev1km) #r is 0.452
cor.test(test$Ag1km, test$LowDev5km) #r is 0.334
cor.test(test$Ag1km, test$LowDev30km) #r is 0.153
cor.test(test$Ag1km, test$OpenDev500m) #r is 0.154
cor.test(test$Ag1km, test$OpenDev1km) #r is 0.193
cor.test(test$Ag1km, test$OpenDev5km) #r is 0.084
cor.test(test$Ag1km, test$OpenDev30km) #r is 0.282
cor.test(test$Ag1km, test$Grass500m) #r is -0.057
cor.test(test$Ag1km, test$Grass1km) #r is 0.042
cor.test(test$Ag1km, test$Grass5km) #r is 0.340
cor.test(test$Ag1km, test$Grass30km) #r is 0.366
cor.test(test$Ag1km, test$Schrubs500m) #r is -0.213
cor.test(test$Ag1km, test$Schrubs1km) #r is -0.324
cor.test(test$Ag1km, test$Schrubs5km) #r is -0.427
cor.test(test$Ag1km, test$Schrubs30km) #r is -0.288
cor.test(test$Ag1km, test$Water500m) #r is 0.167
cor.test(test$Ag1km, test$Water1km) #r is -0.034
cor.test(test$Ag1km, test$Water5km) #r is -0.350
cor.test(test$Ag1km, test$Water30km) #r is -0.285
cor.test(test$Ag1km, test$NSoilTypes) #r is -0.191
cor.test(test$Ag1km, test$FPSiteIndex)  # r is 0.057
cor.test(test$Ag1km, test$SiteIndexPrimaryS)  # r is 0.179
cor.test(test$Ag1km, test$PISoils)  # r is 0.186
cor.test(test$Ag1km, test$SISoils)  # r is -0.001
cor.test(test$Ag1km, test$HydricSoils)  # r is -0.203

#Ag5km by each
#cor.test(test$Ag5km, test$Treatment) #non-numeric
#cor.test(test$Ag5km, test$Ag5km)
cor.test(test$Ag5km, test$Ag30km) #r is 0.739
cor.test(test$Ag5km, test$Evergreen500m) #r is -0.303
cor.test(test$Ag5km, test$Evergreen1km) #r is -0.578
cor.test(test$Ag5km, test$Evergreen5km) #r is -0.829  #high
cor.test(test$Ag5km, test$Evergreen30km) #r is -0.628
cor.test(test$Ag5km, test$Imperv500m) #r is -0.096
cor.test(test$Ag5km, test$Imperv1km) #r is 0.252
cor.test(test$Ag5km, test$Imperv5km) #r is 0.251
cor.test(test$Ag5km, test$Imperv30km) #r is 0.211
cor.test(test$Ag5km, test$Protected30km) #r is -0.562
cor.test(test$Ag5km, test$HighDev500m) #r is -0.030
cor.test(test$Ag5km, test$HighDev1km) #r is 0.249
cor.test(test$Ag5km, test$HighDev5km) #r is 0.235
cor.test(test$Ag5km, test$HighDev30km) #r is -0.665
cor.test(test$Ag5km, test$LowDev500m) #r is 0.266
cor.test(test$Ag5km, test$LowDev1km) #r is 0.461
cor.test(test$Ag5km, test$LowDev5km) #r is 0.395
cor.test(test$Ag5km, test$LowDev30km) #r is 0.260
cor.test(test$Ag5km, test$OpenDev500m) #r is 0.263
cor.test(test$Ag5km, test$OpenDev1km) #r is 0.299
cor.test(test$Ag5km, test$OpenDev5km) #r is 0.244
cor.test(test$Ag5km, test$OpenDev30km) #r is 0.532
cor.test(test$Ag5km, test$Grass500m) #r is -0.084
cor.test(test$Ag5km, test$Grass1km) #r is -0.032
cor.test(test$Ag5km, test$Grass5km) #r is 0.510
cor.test(test$Ag5km, test$Grass30km) #r is 0.670
cor.test(test$Ag5km, test$Schrubs500m) #r is -0.194
cor.test(test$Ag5km, test$Schrubs1km) #r is -0.315
cor.test(test$Ag5km, test$Schrubs5km) #r is -0.573
cor.test(test$Ag5km, test$Schrubs30km) #r is -0.571
cor.test(test$Ag5km, test$Water500m) #r is -0.048
cor.test(test$Ag5km, test$Water1km) #r is -0.080
cor.test(test$Ag5km, test$Water5km) #r is -0.244
cor.test(test$Ag5km, test$Water30km) #r is -0.515
cor.test(test$Ag5km, test$NSoilTypes) #r is -0.114
cor.test(test$Ag5km, test$FPSiteIndex)  # r is -0.154
cor.test(test$Ag5km, test$SiteIndexPrimaryS)  # r is -0.050
cor.test(test$Ag5km, test$PISoils)  # r is 0.198
cor.test(test$Ag5km, test$SISoils)  # r is -0.186
cor.test(test$Ag5km, test$HydricSoils)  # r is -0.093


#Ag30km by each
#cor.test(test$Ag30km, test$Treatment) #non-numeric
#cor.test(test$Ag30km, test$Ag30km)
cor.test(test$Ag30km, test$Evergreen500m) #r is -0.299
cor.test(test$Ag30km, test$Evergreen1km) #r is -0.515
cor.test(test$Ag30km, test$Evergreen5km) #r is -0.846
cor.test(test$Ag30km, test$Evergreen30km) #r is -0.622
cor.test(test$Ag30km, test$Imperv500m) #r is 0.022
cor.test(test$Ag30km, test$Imperv1km) #r is 0.275
cor.test(test$Ag30km, test$Imperv5km) #r is 0.320
cor.test(test$Ag30km, test$Imperv30km) #r is 0.074
cor.test(test$Ag30km, test$Protected30km) #r is -0.825
cor.test(test$Ag30km, test$HighDev500m) #r is 0.114
cor.test(test$Ag30km, test$HighDev1km) #r is 0.270
cor.test(test$Ag30km, test$HighDev5km) #r is 0.303
cor.test(test$Ag30km, test$HighDev30km) #r is -0.803
cor.test(test$Ag30km, test$LowDev500m) #r is 0.167
cor.test(test$Ag30km, test$LowDev1km) #r is 0.271
cor.test(test$Ag30km, test$LowDev5km) #r is 0.464
cor.test(test$Ag30km, test$LowDev30km) #r is 0.098
cor.test(test$Ag30km, test$OpenDev500m) #r is 0.203
cor.test(test$Ag30km, test$OpenDev1km) #r is 0.330
cor.test(test$Ag30km, test$OpenDev5km) #r is 0.551
cor.test(test$Ag30km, test$OpenDev30km) #r is 0.614
cor.test(test$Ag30km, test$Grass500m) #r is 0.098
cor.test(test$Ag30km, test$Grass1km) #r is 0.089
cor.test(test$Ag30km, test$Grass5km) #r is 0.603
cor.test(test$Ag30km, test$Grass30km) #r is 0.620
cor.test(test$Ag30km, test$Schrubs500m) #r is 0.019
cor.test(test$Ag30km, test$Schrubs1km) #r is -0.015
cor.test(test$Ag30km, test$Schrubs5km) #r is -0.263
cor.test(test$Ag30km, test$Schrubs30km) #r is -0.556
cor.test(test$Ag30km, test$Water500m) #r is -0.102
cor.test(test$Ag30km, test$Water1km) #r is -0.010
cor.test(test$Ag30km, test$Water5km) #r is 0.082
cor.test(test$Ag30km, test$Water30km) #r is -0.560
cor.test(test$Ag30km, test$NSoilTypes) #r is 0.047
cor.test(test$Ag30km, test$FPSiteIndex)  # r is -0.311
cor.test(test$Ag30km, test$SiteIndexPrimaryS)  # r is -0.199
cor.test(test$Ag30km, test$PISoils)  # r is 0.161
cor.test(test$Ag30km, test$SISoils)  # r is -0.189
cor.test(test$Ag30km, test$HydricSoils)  # r is 0.124

#Evergreen500m by each
#cor.test(test$Evergreen500m, test$Treatment) #non-numeric
#cor.test(test$Evergreen500m, test$Evergreen500m)
cor.test(test$Evergreen500m, test$Evergreen1km) #r is 0.779
cor.test(test$Evergreen500m, test$Evergreen5km) #r is 0.394
cor.test(test$Evergreen500m, test$Evergreen30km) #r is 0.353
cor.test(test$Evergreen500m, test$Imperv500m) #r is 0.042
cor.test(test$Evergreen500m, test$Imperv1km) #r is -0.139
cor.test(test$Evergreen500m, test$Imperv5km) #r is -0.399
cor.test(test$Evergreen500m, test$Imperv30km) #r is -0.327
cor.test(test$Evergreen500m, test$Protected30km) #r is 0.205
cor.test(test$Evergreen500m, test$HighDev500m) #r is -0.055
cor.test(test$Evergreen500m, test$HighDev1km) #r is -0.139
cor.test(test$Evergreen500m, test$HighDev5km) #r is -0.388
cor.test(test$Evergreen500m, test$HighDev30km) #r is 0.308
cor.test(test$Evergreen500m, test$LowDev500m) #r is -0.154
cor.test(test$Evergreen500m, test$LowDev1km) #r is -0.413
cor.test(test$Evergreen500m, test$LowDev5km) #r is -0.459
cor.test(test$Evergreen500m, test$LowDev30km) #r is -0.365
cor.test(test$Evergreen500m, test$OpenDev500m) #r is -0.139
cor.test(test$Evergreen500m, test$OpenDev1km) #r is -0.267
cor.test(test$Evergreen500m, test$OpenDev5km) #r is -0.402
cor.test(test$Evergreen500m, test$OpenDev30km) #r is -0.455
cor.test(test$Evergreen500m, test$Grass500m) #r is -0.363
cor.test(test$Evergreen500m, test$Grass1km) #r is -0.416
cor.test(test$Evergreen500m, test$Grass5km) #r is -0.171
cor.test(test$Evergreen500m, test$Grass30km) #r is -0.228
cor.test(test$Evergreen500m, test$Schrubs500m) #r is -0.238
cor.test(test$Evergreen500m, test$Schrubs1km) #r is -0.161
cor.test(test$Evergreen500m, test$Schrubs5km) #r is 0.159
cor.test(test$Evergreen500m, test$Schrubs30km) #r is 0.495
cor.test(test$Evergreen500m, test$Water500m) #r is 0.007
cor.test(test$Evergreen500m, test$Water1km) #r is -0.120
cor.test(test$Evergreen500m, test$Water5km) #r is -0.137
cor.test(test$Evergreen500m, test$Water30km) #r is 0.095
cor.test(test$Evergreen500m, test$NSoilTypes) #r is 0.417
cor.test(test$Evergreen500m, test$FPSiteIndex)  # r is 0.176
cor.test(test$Evergreen500m, test$SiteIndexPrimaryS)  # r is 0.111
cor.test(test$Evergreen500m, test$PISoils)  # r is -0.110
cor.test(test$Evergreen500m, test$SISoils)  # r is -0.077
cor.test(test$Evergreen500m, test$HydricSoils)  # r is 0.032

#Evergreen1km by each
#cor.test(test$Evergreen1km, test$Treatment) #non-numeric
#cor.test(test$Evergreen1km, test$Evergreen1km)
cor.test(test$Evergreen1km, test$Evergreen5km) #r is 0.695
cor.test(test$Evergreen1km, test$Evergreen30km) #r is 0.599
cor.test(test$Evergreen1km, test$Imperv500m) #r is 0.008
cor.test(test$Evergreen1km, test$Imperv1km) #r is -0.148
cor.test(test$Evergreen1km, test$Imperv5km) #r is -0.395
cor.test(test$Evergreen1km, test$Imperv30km) #r is -0.505
cor.test(test$Evergreen1km, test$Protected30km) #r is 0.421
cor.test(test$Evergreen1km, test$HighDev500m) #r is -0.090
cor.test(test$Evergreen1km, test$HighDev1km) #r is -0.149
cor.test(test$Evergreen1km, test$HighDev5km) #r is -0.383
cor.test(test$Evergreen1km, test$HighDev30km) #r is 0.530
cor.test(test$Evergreen1km, test$LowDev500m) #r is -0.126
cor.test(test$Evergreen1km, test$LowDev1km) #r is -0.400
cor.test(test$Evergreen1km, test$LowDev5km) #r is -0.484
cor.test(test$Evergreen1km, test$LowDev30km) #r is -0.550
cor.test(test$Evergreen1km, test$OpenDev500m) #r is -0.140
cor.test(test$Evergreen1km, test$OpenDev1km) #r is -0.300
cor.test(test$Evergreen1km, test$OpenDev5km) #r is -0.398
cor.test(test$Evergreen1km, test$OpenDev30km) #r is -0.648
cor.test(test$Evergreen1km, test$Grass500m) #r is -0.198
cor.test(test$Evergreen1km, test$Grass1km) #r is -0.444
cor.test(test$Evergreen1km, test$Grass5km) #r is -0.474
cor.test(test$Evergreen1km, test$Grass30km) #r is -0.482
cor.test(test$Evergreen1km, test$Schrubs500m) #r is -0.101
cor.test(test$Evergreen1km, test$Schrubs1km) #r is -0.091
cor.test(test$Evergreen1km, test$Schrubs5km) #r is 0.326
cor.test(test$Evergreen1km, test$Schrubs30km) #r is 0.565
cor.test(test$Evergreen1km, test$Water500m) #r is 0.062
cor.test(test$Evergreen1km, test$Water1km) #r is -0.091
cor.test(test$Evergreen1km, test$Water5km) #r is -0.027
cor.test(test$Evergreen1km, test$Water30km) #r is 0.203
cor.test(test$Evergreen1km, test$NSoilTypes) #r is 0.243
cor.test(test$Evergreen1km, test$FPSiteIndex)  # r is 0.142
cor.test(test$Evergreen1km, test$SiteIndexPrimaryS)  # r is -0.004
cor.test(test$Evergreen1km, test$PISoils)  # r is -0.025
cor.test(test$Evergreen1km, test$SISoils)  # r is 0.034
cor.test(test$Evergreen1km, test$HydricSoils)  # r is 0.062

#Evergreen5km by each
#cor.test(test$Evergreen5km, test$Treatment) #non-numeric
#cor.test(test$Evergreen5km, test$Evergreen5km)
cor.test(test$Evergreen5km, test$Evergreen30km) #r is 0.666
cor.test(test$Evergreen5km, test$Imperv500m) #r is 0.018
cor.test(test$Evergreen5km, test$Imperv1km) #r is -0.288
cor.test(test$Evergreen5km, test$Imperv5km) #r is -0.345
cor.test(test$Evergreen5km, test$Imperv30km) #r is -0.385
cor.test(test$Evergreen5km, test$Protected30km) #r is 0.709
cor.test(test$Evergreen5km, test$HighDev500m) #r is -0.081
cor.test(test$Evergreen5km, test$HighDev1km) #r is -0.281
cor.test(test$Evergreen5km, test$HighDev5km) #r is -0.326
cor.test(test$Evergreen5km, test$HighDev30km) #r is 0.854
cor.test(test$Evergreen5km, test$LowDev500m) #r is -0.178
cor.test(test$Evergreen5km, test$LowDev1km) #r is -0.390
cor.test(test$Evergreen5km, test$LowDev5km) #r is -0.511
cor.test(test$Evergreen5km, test$LowDev30km) #r is -0.417
cor.test(test$Evergreen5km, test$OpenDev500m) #r is -0.252
cor.test(test$Evergreen5km, test$OpenDev1km) #r is -0.400
cor.test(test$Evergreen5km, test$OpenDev5km) #r is -0.545
cor.test(test$Evergreen5km, test$OpenDev30km) #r is -0.774
cor.test(test$Evergreen5km, test$Grass500m) #r is -0.140
cor.test(test$Evergreen5km, test$Grass1km) #r is -0.216
cor.test(test$Evergreen5km, test$Grass5km) #r is -0.601
cor.test(test$Evergreen5km, test$Grass30km) #r is -0.726   #high
cor.test(test$Evergreen5km, test$Schrubs500m) #r is 0.045
cor.test(test$Evergreen5km, test$Schrubs1km) #r is 0.133
cor.test(test$Evergreen5km, test$Schrubs5km) #r is 0.491
cor.test(test$Evergreen5km, test$Schrubs30km) #r is 0.728  #high
cor.test(test$Evergreen5km, test$Water500m) #r is 0.137
cor.test(test$Evergreen5km, test$Water1km) #r is 0.063
cor.test(test$Evergreen5km, test$Water5km) #r is -0.024
cor.test(test$Evergreen5km, test$Water30km) #r is 0.058
cor.test(test$Evergreen5km, test$NSoilTypes) #r is 0.023
cor.test(test$Evergreen5km, test$FPSiteIndex)  # r is 0.292
cor.test(test$Evergreen5km, test$SiteIndexPrimaryS)  # r is 0.121
cor.test(test$Evergreen5km, test$PISoils)  # r is -0.017
cor.test(test$Evergreen5km, test$SISoils)  # r is 0.155
cor.test(test$Evergreen5km, test$HydricSoils)  # r is -0.054

#Evergreen30km by each
#cor.test(test$Evergreen30km, test$Treatment) #non-numeric
#cor.test(test$Evergreen30km, test$Evergreen30km)
cor.test(test$Evergreen30km, test$Imperv500m) #r is -0.140
cor.test(test$Evergreen30km, test$Imperv1km) #r is -0.318
cor.test(test$Evergreen30km, test$Imperv5km) #r is -0.463
cor.test(test$Evergreen30km, test$Imperv30km) #r is -0.393
cor.test(test$Evergreen30km, test$Protected30km) #r is 0.446
cor.test(test$Evergreen30km, test$HighDev500m) #r is -0.150
cor.test(test$Evergreen30km, test$HighDev1km) #r is -0.307
cor.test(test$Evergreen30km, test$HighDev5km) #r is -0.453
cor.test(test$Evergreen30km, test$HighDev30km) #r is 0.457
cor.test(test$Evergreen30km, test$LowDev500m) #r is -0.242
cor.test(test$Evergreen30km, test$LowDev1km) #r is -0.371
cor.test(test$Evergreen30km, test$LowDev5km) #r is -0.477
cor.test(test$Evergreen30km, test$LowDev30km) #r is -0.489
cor.test(test$Evergreen30km, test$OpenDev500m) #r is -0.095
cor.test(test$Evergreen30km, test$OpenDev1km) #r is -0.157
cor.test(test$Evergreen30km, test$OpenDev5km) #r is -0.223
cor.test(test$Evergreen30km, test$OpenDev30km) #r is -0.527
cor.test(test$Evergreen30km, test$Grass500m) #r is -0.044
cor.test(test$Evergreen30km, test$Grass1km) #r is -0.092
cor.test(test$Evergreen30km, test$Grass5km) #r is -0.394
cor.test(test$Evergreen30km, test$Grass30km) #r is -0.445
cor.test(test$Evergreen30km, test$Schrubs500m) #r is 0.134
cor.test(test$Evergreen30km, test$Schrubs1km) #r is 0.213
cor.test(test$Evergreen30km, test$Schrubs5km) #r is 0.495
cor.test(test$Evergreen30km, test$Schrubs30km) #r is 0.541
cor.test(test$Evergreen30km, test$Water500m) #r is 0.078
cor.test(test$Evergreen30km, test$Water1km) #r is 0.064
cor.test(test$Evergreen30km, test$Water5km) #r is 0.315
cor.test(test$Evergreen30km, test$Water30km) #r is 0.256
cor.test(test$Evergreen30km, test$NSoilTypes) #r is 0.196
cor.test(test$Evergreen30km, test$FPSiteIndex)  # r is -0.014
cor.test(test$Evergreen30km, test$SiteIndexPrimaryS)  # r is -0.056
cor.test(test$Evergreen30km, test$PISoils)  # r is 0.007
cor.test(test$Evergreen30km, test$SISoils)  # r is -0.047
cor.test(test$Evergreen30km, test$HydricSoils)  # r is 0.169

#Imperv500m by each
#cor.test(test$Imperv500m, test$Treatment) #non-numeric
#cor.test(test$Imperv500m, test$Imperv500m)
cor.test(test$Imperv500m, test$Imperv1km) #r is -0.011
cor.test(test$Imperv500m, test$Imperv5km) #r is -0.035
cor.test(test$Imperv500m, test$Imperv30km) #r is -0.036
cor.test(test$Imperv500m, test$Protected30km) #r is -0.072
cor.test(test$Imperv500m, test$HighDev500m) #r is 0.825   #high, makes sense but don't combine
cor.test(test$Imperv500m, test$HighDev1km) #r is -0.028
cor.test(test$Imperv500m, test$HighDev5km) #r is -0.028
cor.test(test$Imperv500m, test$HighDev30km) #r is 0.048
cor.test(test$Imperv500m, test$LowDev500m) #r is 0.156
cor.test(test$Imperv500m, test$LowDev1km) #r is 0.038
cor.test(test$Imperv500m, test$LowDev5km) #r is -0.092
cor.test(test$Imperv500m, test$LowDev30km) #r is -0.003
cor.test(test$Imperv500m, test$OpenDev500m) #r is 0.011
cor.test(test$Imperv500m, test$OpenDev1km) #r is 0.029
cor.test(test$Imperv500m, test$OpenDev5km) #r is -0.108
cor.test(test$Imperv500m, test$OpenDev30km) #r is -0.048
cor.test(test$Imperv500m, test$Grass500m) #r is -0.073
cor.test(test$Imperv500m, test$Grass1km) #r is -0.018
cor.test(test$Imperv500m, test$Grass5km) #r is -0.050
cor.test(test$Imperv500m, test$Grass30km) #r is 0.029
cor.test(test$Imperv500m, test$Schrubs500m) #r is -0.066
cor.test(test$Imperv500m, test$Schrubs1km) #r is 0.042
cor.test(test$Imperv500m, test$Schrubs5km) #r is -0.057
cor.test(test$Imperv500m, test$Schrubs30km) #r is -0.051
cor.test(test$Imperv500m, test$Water500m) #r is -0.083
cor.test(test$Imperv500m, test$Water1km) #r is -0.092
cor.test(test$Imperv500m, test$Water5km) #r is -0.056
cor.test(test$Imperv500m, test$Water30km) #r is -0.051
cor.test(test$Imperv500m, test$NSoilTypes) #r is -0.111
cor.test(test$Imperv500m, test$FPSiteIndex)  # r is 0.117
cor.test(test$Imperv500m, test$SiteIndexPrimaryS)  # r is 0.103
cor.test(test$Imperv500m, test$PISoils)  # r is -0.167
cor.test(test$Imperv500m, test$SISoils)  # r is  -0.198
cor.test(test$Imperv500m, test$HydricSoils)  # r is -0.098

#Imperv1km by each
#cor.test(test$Imperv1km, test$Treatment) #non-numeric
#cor.test(test$Imperv1km, test$Imperv1km)
cor.test(test$Imperv1km, test$Imperv5km) #r is 0.212
cor.test(test$Imperv1km, test$Imperv30km) #r is 0.125
cor.test(test$Imperv1km, test$Protected30km) #r is -0.188
cor.test(test$Imperv1km, test$HighDev500m) #r is -0.004
cor.test(test$Imperv1km, test$HighDev1km) #r is 0.992
cor.test(test$Imperv1km, test$HighDev5km) #r is 0.195
cor.test(test$Imperv1km, test$HighDev30km) #r is -0.283
cor.test(test$Imperv1km, test$LowDev500m) #r is 0.127
cor.test(test$Imperv1km, test$LowDev1km) #r is 0.234
cor.test(test$Imperv1km, test$LowDev5km) #r is 0.207
cor.test(test$Imperv1km, test$LowDev30km) #r is 0.140
cor.test(test$Imperv1km, test$OpenDev500m) #r is 0.242
cor.test(test$Imperv1km, test$OpenDev1km) #r is 0.309
cor.test(test$Imperv1km, test$OpenDev5km) #r is 0.217
cor.test(test$Imperv1km, test$OpenDev30km) #r is 0.225
cor.test(test$Imperv1km, test$Grass500m) #r is 0.133
cor.test(test$Imperv1km, test$Grass1km) #r is 0.067
cor.test(test$Imperv1km, test$Grass5km) #r is 0.234
cor.test(test$Imperv1km, test$Grass30km) #r is 0.202
cor.test(test$Imperv1km, test$Schrubs500m) #r is -0.004
cor.test(test$Imperv1km, test$Schrubs1km) #r is -0.095
cor.test(test$Imperv1km, test$Schrubs5km) #r is -0.231
cor.test(test$Imperv1km, test$Schrubs30km) #r is -0.280
cor.test(test$Imperv1km, test$Water500m) #r is -0.085
cor.test(test$Imperv1km, test$Water1km) #r is -0.097
cor.test(test$Imperv1km, test$Water5km) #r is -0.094
cor.test(test$Imperv1km, test$Water30km) #r is -0.149
cor.test(test$Imperv1km, test$NSoilTypes) #r is -0.136
cor.test(test$Imperv1km, test$FPSiteIndex)  # r is -0.113
cor.test(test$Imperv1km, test$SiteIndexPrimaryS)  # r is -0.111
cor.test(test$Imperv1km, test$PISoils)  # r is 0.110
cor.test(test$Imperv1km, test$SISoils)  # r is -0.042
cor.test(test$Imperv1km, test$HydricSoils)  # r is -0.045

#Imperv5km by each
#cor.test(test$Imperv5km, test$Treatment) #non-numeric
#cor.test(test$Imperv5km, test$Imperv5km)
cor.test(test$Imperv5km, test$Imperv30km) #r is 0.207
cor.test(test$Imperv5km, test$Protected30km) #r is -0.268
cor.test(test$Imperv5km, test$HighDev500m) #r is -0.077
cor.test(test$Imperv5km, test$HighDev1km) #r is 0.205
cor.test(test$Imperv5km, test$HighDev5km) #r is 0.999  # keep this in mind!!
cor.test(test$Imperv5km, test$HighDev30km) #r is -0.212
cor.test(test$Imperv5km, test$LowDev500m) #r is 0.059
cor.test(test$Imperv5km, test$LowDev1km) #r is 0.266
cor.test(test$Imperv5km, test$LowDev5km) #r is 0.927  #!!
cor.test(test$Imperv5km, test$LowDev30km) #r is 0.247
cor.test(test$Imperv5km, test$OpenDev500m) #r is 0.110
cor.test(test$Imperv5km, test$OpenDev1km) #r is 0.271
cor.test(test$Imperv5km, test$OpenDev5km) #r is 0.628
cor.test(test$Imperv5km, test$OpenDev30km) #r is 0.293
cor.test(test$Imperv5km, test$Grass500m) #r is 0.154
cor.test(test$Imperv5km, test$Grass1km) #r is 0.174
cor.test(test$Imperv5km, test$Grass5km) #r is 0.246
cor.test(test$Imperv5km, test$Grass30km) #r is 0.312
cor.test(test$Imperv5km, test$Schrubs500m) #r is 0.122
cor.test(test$Imperv5km, test$Schrubs1km) #r is 0.053
cor.test(test$Imperv5km, test$Schrubs5km) #r is -0.331
cor.test(test$Imperv5km, test$Schrubs30km) #r is -0.459
cor.test(test$Imperv5km, test$Water500m) #r is -0.122
cor.test(test$Imperv5km, test$Water1km) #r is 0.044
cor.test(test$Imperv5km, test$Water5km) #r is -0.057
cor.test(test$Imperv5km, test$Water30km) #r is -0.269
cor.test(test$Imperv5km, test$NSoilTypes) #r is -0.201
cor.test(test$Imperv5km, test$FPSiteIndex)  # r is 0.065
cor.test(test$Imperv5km, test$SiteIndexPrimaryS)  # r is 0.114
cor.test(test$Imperv5km, test$PISoils)  # r is -0.051
cor.test(test$Imperv5km, test$SISoils)  # r is 0.299
cor.test(test$Imperv5km, test$HydricSoils)  # r is -0.125

#Imperv30km by each
#cor.test(test$Imperv30km, test$Treatment) #non-numeric
#cor.test(test$Imperv30km, test$Imperv30km)
cor.test(test$Imperv30km, test$Protected30km) #r is -0.141
cor.test(test$Imperv30km, test$HighDev500m) #r is -0.008
cor.test(test$Imperv30km, test$HighDev1km) #r is 0.109
cor.test(test$Imperv30km, test$HighDev5km) #r is 0.193
cor.test(test$Imperv30km, test$HighDev30km) #r is -0.274
cor.test(test$Imperv30km, test$LowDev500m) #r is 0.008
cor.test(test$Imperv30km, test$LowDev1km) #r is 0.340
cor.test(test$Imperv30km, test$LowDev5km) #r is 0.257
cor.test(test$Imperv30km, test$LowDev30km) #r is 0.973   #!
cor.test(test$Imperv30km, test$OpenDev500m) #r is -0.056
cor.test(test$Imperv30km, test$OpenDev1km) #r is 0.117
cor.test(test$Imperv30km, test$OpenDev5km) #r is 0.235
cor.test(test$Imperv30km, test$OpenDev30km) #r is 0.730
cor.test(test$Imperv30km, test$Grass500m) #r is 0.238
cor.test(test$Imperv30km, test$Grass1km) #r is 0.382
cor.test(test$Imperv30km, test$Grass5km) #r is 0.117
cor.test(test$Imperv30km, test$Grass30km) #r is 0.122
cor.test(test$Imperv30km, test$Schrubs500m) #r is 0.144
cor.test(test$Imperv30km, test$Schrubs1km) #r is -0.032
cor.test(test$Imperv30km, test$Schrubs5km) #r is -0.392
cor.test(test$Imperv30km, test$Schrubs30km) #r is -0.411
cor.test(test$Imperv30km, test$Water500m) #r is 0.100
cor.test(test$Imperv30km, test$Water1km) #r is -0.035
cor.test(test$Imperv30km, test$Water5km) #r is 0.040
cor.test(test$Imperv30km, test$Water30km) #r is -0.021
cor.test(test$Imperv30km, test$NSoilTypes) #r is -0.082
cor.test(test$Imperv30km, test$FPSiteIndex)  # r is -0.052
cor.test(test$Imperv30km, test$SiteIndexPrimaryS)  # r is -0.017
cor.test(test$Imperv30km, test$PISoils)  # r is -0.062
cor.test(test$Imperv30km, test$SISoils)  # r is -0.039
cor.test(test$Imperv30km, test$HydricSoils)  # r is 0.003

#Protected30km by each
#cor.test(test$Protected30km, test$Treatment) #non-numeric
#cor.test(test$Protected30km, test$Protected30km)
cor.test(test$Protected30km, test$HighDev500m) #r is -0.118
cor.test(test$Protected30km, test$HighDev1km) #r is -0.178
cor.test(test$Protected30km, test$HighDev5km) #r is -0.256
cor.test(test$Protected30km, test$HighDev30km) #r is 0.568
cor.test(test$Protected30km, test$LowDev500m) #r is -0.083
cor.test(test$Protected30km, test$LowDev1km) #r is -0.178
cor.test(test$Protected30km, test$LowDev5km) #r is -0.396
cor.test(test$Protected30km, test$LowDev30km) #r is -0.126
cor.test(test$Protected30km, test$OpenDev500m) #r is 0.036
cor.test(test$Protected30km, test$OpenDev1km) #r is -0.172
cor.test(test$Protected30km, test$OpenDev5km) #r is -0.546
cor.test(test$Protected30km, test$OpenDev30km) #r is -0.495
cor.test(test$Protected30km, test$Grass500m) #r is -0.225
cor.test(test$Protected30km, test$Grass1km) #r is -0.195
cor.test(test$Protected30km, test$Grass5km) #r is -0.682
cor.test(test$Protected30km, test$Grass30km) #r is -0.573
cor.test(test$Protected30km, test$Schrubs500m) #r is -0.130
cor.test(test$Protected30km, test$Schrubs1km) #r is -0.134
cor.test(test$Protected30km, test$Schrubs5km) #r is 0.204
cor.test(test$Protected30km, test$Schrubs30km) #r is 0.298
cor.test(test$Protected30km, test$Water500m) #r is -0.109
cor.test(test$Protected30km, test$Water1km) #r is -0.216
cor.test(test$Protected30km, test$Water5km) #r is -0.219
cor.test(test$Protected30km, test$Water30km) #r is 0.757
cor.test(test$Protected30km, test$NSoilTypes) #r is 0.010
cor.test(test$Protected30km, test$FPSiteIndex)  # r is 0.327
cor.test(test$Protected30km, test$SiteIndexPrimaryS)  # r is 0.188
cor.test(test$Protected30km, test$PISoils)  # r is -0.136
cor.test(test$Protected30km, test$SISoils)  # r is 0.183
cor.test(test$Protected30km, test$HydricSoils)  # r is -0.204


#HighDev500m by each
#cor.test(test$HighDev500m, test$Treatment) #non-numeric
#cor.test(test$HighDev500m, test$HighDev500m)
cor.test(test$HighDev500m, test$HighDev1km) #r is -0.012
cor.test(test$HighDev500m, test$HighDev5km) #r is -0.076
cor.test(test$HighDev500m, test$HighDev30km) #r is -0.104
cor.test(test$HighDev500m, test$LowDev500m) #r is 0.143
cor.test(test$HighDev500m, test$LowDev1km) #r is -0.001
cor.test(test$HighDev500m, test$LowDev5km) #r is -0.085
cor.test(test$HighDev500m, test$LowDev30km) #r is 0.006
cor.test(test$HighDev500m, test$OpenDev500m) #r is -0.046
cor.test(test$HighDev500m, test$OpenDev1km) #r is 0.044
cor.test(test$HighDev500m, test$OpenDev5km) #r is -0.031
cor.test(test$HighDev500m, test$OpenDev30km) #r is 0.054
cor.test(test$HighDev500m, test$Grass500m) #r is -0.123
cor.test(test$HighDev500m, test$Grass1km) #r is 0.024
cor.test(test$HighDev500m, test$Grass5km) #r is -0.007
cor.test(test$HighDev500m, test$Grass30km) #r is 0.044
cor.test(test$HighDev500m, test$Schrubs500m) #r is -0.063
cor.test(test$HighDev500m, test$Schrubs1km) #r is 0.065
cor.test(test$HighDev500m, test$Schrubs5km) #r is -0.039
cor.test(test$HighDev500m, test$Schrubs30km) #r is -0.064
cor.test(test$HighDev500m, test$Water500m) #r is -0.060
cor.test(test$HighDev500m, test$Water1km) #r is -0.091
cor.test(test$HighDev500m, test$Water5km) #r is -0.024
cor.test(test$HighDev500m, test$Water30km) #r is -0.041
cor.test(test$HighDev500m, test$NSoilTypes) #r is -0.041
cor.test(test$HighDev500m, test$FPSiteIndex)  # r is 0.101
cor.test(test$HighDev500m, test$SiteIndexPrimaryS)  # r is 0.073
cor.test(test$HighDev500m, test$PISoils)  # r is -0.103
cor.test(test$HighDev500m, test$SISoils)  # r is -0.175
cor.test(test$HighDev500m, test$HydricSoils)  # r is 0.161

#HighDev1km by each
#cor.test(test$HighDev1km, test$Treatment) #non-numeric
#cor.test(test$HighDev1km, test$HighDev1km)
cor.test(test$HighDev1km, test$HighDev5km) #r is 0.189
cor.test(test$HighDev1km, test$HighDev30km) #r is -0.291
cor.test(test$HighDev1km, test$LowDev500m) #r is 0.043
cor.test(test$HighDev1km, test$LowDev1km) #r is 0.185
cor.test(test$HighDev1km, test$LowDev5km) #r is 0.193
cor.test(test$HighDev1km, test$LowDev30km) #r is 0.125
cor.test(test$HighDev1km, test$OpenDev500m) #r is 0.203
cor.test(test$HighDev1km, test$OpenDev1km) #r is 0.277
cor.test(test$HighDev1km, test$OpenDev5km) #r is 0.219
cor.test(test$HighDev1km, test$OpenDev30km) #r is 0.219
cor.test(test$HighDev1km, test$Grass500m) #r is 0.134
cor.test(test$HighDev1km, test$Grass1km) #r is 0.049
cor.test(test$HighDev1km, test$Grass5km) #r is 0.227
cor.test(test$HighDev1km, test$Grass30km) #r is 0.196
cor.test(test$HighDev1km, test$Schrubs500m) #r is -0.008
cor.test(test$HighDev1km, test$Schrubs1km) #r is -0.088
cor.test(test$HighDev1km, test$Schrubs5km) #r is -0.207
cor.test(test$HighDev1km, test$Schrubs30km) #r is -0.280
cor.test(test$HighDev1km, test$Water500m) #r is -0.087
cor.test(test$HighDev1km, test$Water1km) #r is -0.111
cor.test(test$HighDev1km, test$Water5km) #r is -0.087
cor.test(test$HighDev1km, test$Water30km) #r is -0.132
cor.test(test$HighDev1km, test$NSoilTypes) #r is -0.164
cor.test(test$HighDev1km, test$FPSiteIndex)  # r is -0.103
cor.test(test$HighDev1km, test$SiteIndexPrimaryS)  # r is -0.084
cor.test(test$HighDev1km, test$PISoils)  # r is 0.160
cor.test(test$HighDev1km, test$SISoils)  # r is -0.020
cor.test(test$HighDev1km, test$HydricSoils)  # r is -0.071

#HighDev5km by each
#cor.test(test$HighDev5km, test$Treatment) #non-numeric
#cor.test(test$HighDev5km, test$HighDev5km)
cor.test(test$HighDev5km, test$HighDev30km) #r is -0.194
cor.test(test$HighDev5km, test$LowDev500m) #r is 0.057
cor.test(test$HighDev5km, test$LowDev1km) #r is 0.255
cor.test(test$HighDev5km, test$LowDev5km) #r is 0.922  #too correlated
cor.test(test$HighDev5km, test$LowDev30km) #r is 0.235
cor.test(test$HighDev5km, test$OpenDev500m) #r is 0.107
cor.test(test$HighDev5km, test$OpenDev1km) #r is 0.261
cor.test(test$HighDev5km, test$OpenDev5km) #r is 0.614
cor.test(test$HighDev5km, test$OpenDev30km) #r is 0.271
cor.test(test$HighDev5km, test$Grass500m) #r is 0.142
cor.test(test$HighDev5km, test$Grass1km) #r is 0.165
cor.test(test$HighDev5km, test$Grass5km) #r is 0.236
cor.test(test$HighDev5km, test$Grass30km) #r is 0.305
cor.test(test$HighDev5km, test$Schrubs500m) #r is 0.117
cor.test(test$HighDev5km, test$Schrubs1km) #r is 0.054
cor.test(test$HighDev5km, test$Schrubs5km) #r is -0.324
cor.test(test$HighDev5km, test$Schrubs30km) #r is -0.444
cor.test(test$HighDev5km, test$Water500m) #r is -0.120
cor.test(test$HighDev5km, test$Water1km) #r is 0.047
cor.test(test$HighDev5km, test$Water5km) #r is -0.061
cor.test(test$HighDev5km, test$Water30km) #r is -0.261
cor.test(test$HighDev5km, test$NSoilTypes) #r is -0.202
cor.test(test$HighDev5km, test$FPSiteIndex)  # r is 0.080
cor.test(test$HighDev5km, test$SiteIndexPrimaryS)  # r is 0.127
cor.test(test$HighDev5km, test$PISoils)  # r is -0.055
cor.test(test$HighDev5km, test$SISoils)  # r is 0.307
cor.test(test$HighDev5km, test$HydricSoils)  # r is -0.126

#HighDev30km by each
#cor.test(test$HighDev30km, test$Treatment) #non-numeric
#cor.test(test$HighDev30km, test$HighDev30km)
cor.test(test$HighDev30km, test$LowDev500m) #r is -0.095
cor.test(test$HighDev30km, test$LowDev1km) #r is -0.287
cor.test(test$HighDev30km, test$LowDev5km) #r is -0.415
cor.test(test$HighDev30km, test$LowDev30km) #r is -0.260
cor.test(test$HighDev30km, test$OpenDev500m) #r is -0.287
cor.test(test$HighDev30km, test$OpenDev1km) #r is -0.411
cor.test(test$HighDev30km, test$OpenDev5km) #r is -0.593
cor.test(test$HighDev30km, test$OpenDev30km) #r is -0.782  #high
cor.test(test$HighDev30km, test$Grass500m) #r is -0.019
cor.test(test$HighDev30km, test$Grass1km) #r is -0.135
cor.test(test$HighDev30km, test$Grass5km) #r is -0.343
cor.test(test$HighDev30km, test$Grass30km) #r is -0.504
cor.test(test$HighDev30km, test$Schrubs500m) #r is -0.069
cor.test(test$HighDev30km, test$Schrubs1km) #r is 0.002
cor.test(test$HighDev30km, test$Schrubs5km) #r is 0.257
cor.test(test$HighDev30km, test$Schrubs30km) #r is 0.710 #
cor.test(test$HighDev30km, test$Water500m) #r is 0.323
cor.test(test$HighDev30km, test$Water1km) #r is 0.212
cor.test(test$HighDev30km, test$Water5km) #r is -0.148
cor.test(test$HighDev30km, test$Water30km) #r is 0.404
cor.test(test$HighDev30km, test$NSoilTypes) #r is -0.059
cor.test(test$HighDev30km, test$FPSiteIndex)  # r is 0.343
cor.test(test$HighDev30km, test$SiteIndexPrimaryS)  # r is 0.232
cor.test(test$HighDev30km, test$PISoils)  # r is -0.081
cor.test(test$HighDev30km, test$SISoils)  # r is 0.189
cor.test(test$HighDev30km, test$HydricSoils)  # r is -0.111

#LowDev500m by each
#cor.test(test$LowDev500m, test$Treatment) #non-numeric
#cor.test(test$LowDev500m, test$LowDev500m)
cor.test(test$LowDev500m, test$LowDev1km) #r is 0.734
cor.test(test$LowDev500m, test$LowDev5km) #r is 0.142
cor.test(test$LowDev500m, test$LowDev30km) #r is 0.066
cor.test(test$LowDev500m, test$OpenDev500m) #r is 0.533
cor.test(test$LowDev500m, test$OpenDev1km) #r is 0.466
cor.test(test$LowDev500m, test$OpenDev5km) #r is -0.034
cor.test(test$LowDev500m, test$OpenDev30km) #r is 0.083
cor.test(test$LowDev500m, test$Grass500m) #r is -0.141
cor.test(test$LowDev500m, test$Grass1km) #r is -0.042
cor.test(test$LowDev500m, test$Grass5km) #r is -0.071
cor.test(test$LowDev500m, test$Grass30km) #r is 0.050
cor.test(test$LowDev500m, test$Schrubs500m) #r is -0.142
cor.test(test$LowDev500m, test$Schrubs1km) #r is -0.121
cor.test(test$LowDev500m, test$Schrubs5km) #r is -0.236
cor.test(test$LowDev500m, test$Schrubs30km) #r is -0.226
cor.test(test$LowDev500m, test$Water500m) #r is -0.031
cor.test(test$LowDev500m, test$Water1km) #r is 0.156
cor.test(test$LowDev500m, test$Water5km) #r is -0.115
cor.test(test$LowDev500m, test$Water30km) #r is -0.132
cor.test(test$LowDev500m, test$NSoilTypes) #r is -0.090
cor.test(test$LowDev500m, test$FPSiteIndex)  # r is -0.113
cor.test(test$LowDev500m, test$SiteIndexPrimaryS)  # r is -0.122
cor.test(test$LowDev500m, test$PISoils)  # r is -0.162
cor.test(test$LowDev500m, test$SISoils)  # r is -0.177
cor.test(test$LowDev500m, test$HydricSoils)  # r is -0.079

#LowDev1km by each
#cor.test(test$LowDev1km, test$Treatment) #non-numeric
#cor.test(test$LowDev1km, test$LowDev1km)
cor.test(test$LowDev1km, test$LowDev5km) #r is 0.372
cor.test(test$LowDev1km, test$LowDev30km) #r is 0.358
cor.test(test$LowDev1km, test$OpenDev500m) #r is 0.525
cor.test(test$LowDev1km, test$OpenDev1km) #r is 0.633
cor.test(test$LowDev1km, test$OpenDev5km) #r is 0.243
cor.test(test$LowDev1km, test$OpenDev30km) #r is 0.455
cor.test(test$LowDev1km, test$Grass500m) #r is 0.016
cor.test(test$LowDev1km, test$Grass1km) #r is 0.026
cor.test(test$LowDev1km, test$Grass5km) #r is -0.055
cor.test(test$LowDev1km, test$Grass30km) #r is 0.114
cor.test(test$LowDev1km, test$Schrubs500m) #r is 0.043
cor.test(test$LowDev1km, test$Schrubs1km) #r is -0.049
cor.test(test$LowDev1km, test$Schrubs5km) #r is -0.355
cor.test(test$LowDev1km, test$Schrubs30km) #r is -0.472
cor.test(test$LowDev1km, test$Water500m) #r is -0.071
cor.test(test$LowDev1km, test$Water1km) #r is 0.093
cor.test(test$LowDev1km, test$Water5km) #r is 0.254
cor.test(test$LowDev1km, test$Water30km) #r is -0.173
cor.test(test$LowDev1km, test$NSoilTypes) #r is -0.276
cor.test(test$LowDev1km, test$FPSiteIndex)  # r is -0.255
cor.test(test$LowDev1km, test$SiteIndexPrimaryS)  # r is -0.236
cor.test(test$LowDev1km, test$PISoils)  # r is 0.051
cor.test(test$LowDev1km, test$SISoils)  # r is -0.247
cor.test(test$LowDev1km, test$HydricSoils)  # r is -0.151

#LowDev5km by each
#cor.test(test$LowDev5km, test$Treatment) #non-numeric
#cor.test(test$LowDev5km, test$LowDev5km)
cor.test(test$LowDev5km, test$LowDev30km) #r is 0.281
cor.test(test$LowDev5km, test$OpenDev500m) #r is 0.227
cor.test(test$LowDev5km, test$OpenDev1km) #r is 0.401
cor.test(test$LowDev5km, test$OpenDev5km) #r is 0.784
cor.test(test$LowDev5km, test$OpenDev30km) #r is 0.448
cor.test(test$LowDev5km, test$Grass500m) #r is 0.173
cor.test(test$LowDev5km, test$Grass1km) #r is 0.217
cor.test(test$LowDev5km, test$Grass5km) #r is 0.280
cor.test(test$LowDev5km, test$Grass30km) #r is 0.359
cor.test(test$LowDev5km, test$Schrubs500m) #r is 0.154
cor.test(test$LowDev5km, test$Schrubs1km) #r is 0.076
cor.test(test$LowDev5km, test$Schrubs5km) #r is -0.317
cor.test(test$LowDev5km, test$Schrubs30km) #r is -0.525
cor.test(test$LowDev5km, test$Water500m) #r is -0.150
cor.test(test$LowDev5km, test$Water1km) #r is 0.065
cor.test(test$LowDev5km, test$Water5km) #r is 0.092
cor.test(test$LowDev5km, test$Water30km) #r is -0.346
cor.test(test$LowDev5km, test$NSoilTypes) #r is -0.173
cor.test(test$LowDev5km, test$FPSiteIndex)  # r is -0.136
cor.test(test$LowDev5km, test$SiteIndexPrimaryS)  # r is -0.083
cor.test(test$LowDev5km, test$PISoils)  # r is -0.023
cor.test(test$LowDev5km, test$SISoils)  # r is 0.205
cor.test(test$LowDev5km, test$HydricSoils)  # r is -0.077

#LowDev30km by each
#cor.test(test$LowDev30km, test$Treatment) #non-numeric
#cor.test(test$LowDev30km, test$LowDev30km)
cor.test(test$LowDev30km, test$OpenDev500m) #r is -0.019
cor.test(test$LowDev30km, test$OpenDev1km) #r is 0.131
cor.test(test$LowDev30km, test$OpenDev5km) #r is 0.179
cor.test(test$LowDev30km, test$OpenDev30km) #r is 0.683
cor.test(test$LowDev30km, test$Grass500m) #r is 0.215
cor.test(test$LowDev30km, test$Grass1km) #r is 0.337
cor.test(test$LowDev30km, test$Grass5km) #r is 0.168
cor.test(test$LowDev30km, test$Grass30km) #r is 0.201
cor.test(test$LowDev30km, test$Schrubs500m) #r is 0.051
cor.test(test$LowDev30km, test$Schrubs1km) #r is -0.116
cor.test(test$LowDev30km, test$Schrubs5km) #r is -0.483
cor.test(test$LowDev30km, test$Schrubs30km) #r is -0.478
cor.test(test$LowDev30km, test$Water500m) #r is 0.096
cor.test(test$LowDev30km, test$Water1km) #r is -0.057
cor.test(test$LowDev30km, test$Water5km) #r is -0.067
cor.test(test$LowDev30km, test$Water30km) #r is -0.020
cor.test(test$LowDev30km, test$NSoilTypes) #r is -0.135
cor.test(test$LowDev30km, test$FPSiteIndex)  # r is -0.008
cor.test(test$LowDev30km, test$SiteIndexPrimaryS)  # r is 0.030
cor.test(test$LowDev30km, test$PISoils)  # r is -0.116
cor.test(test$LowDev30km, test$SISoils)  # r is -0.001
cor.test(test$LowDev30km, test$HydricSoils)  # r is -0.059

#OpenDev500m by each
#cor.test(test$OpenDev500m, test$Treatment) #non-numeric
#cor.test(test$OpenDev500m, test$OpenDev500m)
cor.test(test$OpenDev500m, test$OpenDev1km) #r is 0.853  #high
cor.test(test$OpenDev500m, test$OpenDev5km) #r is 0.197
cor.test(test$OpenDev500m, test$OpenDev30km) #r is 0.129
cor.test(test$OpenDev500m, test$Grass500m) #r is -0.116
cor.test(test$OpenDev500m, test$Grass1km) #r is -0.079
cor.test(test$OpenDev500m, test$Grass5km) #r is -0.116
cor.test(test$OpenDev500m, test$Grass30km) #r is 0.163
cor.test(test$OpenDev500m, test$Schrubs500m) #r is -0.170
cor.test(test$OpenDev500m, test$Schrubs1km) #r is -0.166
cor.test(test$OpenDev500m, test$Schrubs5km) #r is -0.243
cor.test(test$OpenDev500m, test$Schrubs30km) #r is -0.407
cor.test(test$OpenDev500m, test$Water500m) #r is -0.176
cor.test(test$OpenDev500m, test$Water1km) #r is -0.035
cor.test(test$OpenDev500m, test$Water5km) #r is 0.084
cor.test(test$OpenDev500m, test$Water30km) #r is -0.002
cor.test(test$OpenDev500m, test$NSoilTypes) #r is -0.156
cor.test(test$OpenDev500m, test$FPSiteIndex)  # r is -0.241
cor.test(test$OpenDev500m, test$SiteIndexPrimaryS)  # r is -0.209
cor.test(test$OpenDev500m, test$PISoils)  # r is -0.023
cor.test(test$OpenDev500m, test$SISoils)  # r is -0.045
cor.test(test$OpenDev500m, test$HydricSoils)  # r is -0.022

#OpenDev1km by each
#cor.test(test$OpenDev1km, test$Treatment) #non-numeric
#cor.test(test$OpenDev1km, test$OpenDev1km)
cor.test(test$OpenDev1km, test$OpenDev5km) #r is 0.498
cor.test(test$OpenDev1km, test$OpenDev30km) #r is 0.338
cor.test(test$OpenDev1km, test$Grass500m) #r is 0.095
cor.test(test$OpenDev1km, test$Grass1km) #r is 0.121
cor.test(test$OpenDev1km, test$Grass5km) #r is 0.032
cor.test(test$OpenDev1km, test$Grass30km) #r is 0.178
cor.test(test$OpenDev1km, test$Schrubs500m) #r is 0.015
cor.test(test$OpenDev1km, test$Schrubs1km) #r is 0.020
cor.test(test$OpenDev1km, test$Schrubs5km) #r is -0.190
cor.test(test$OpenDev1km, test$Schrubs30km) #r is -0.477
cor.test(test$OpenDev1km, test$Water500m) #r is -0.150
cor.test(test$OpenDev1km, test$Water1km) #r is 0.018
cor.test(test$OpenDev1km, test$Water5km) #r is 0.284
cor.test(test$OpenDev1km, test$Water30km) #r is -0.082
cor.test(test$OpenDev1km, test$NSoilTypes) #r is -0.223
cor.test(test$OpenDev1km, test$FPSiteIndex)  # r is -0.328
cor.test(test$OpenDev1km, test$SiteIndexPrimaryS)  # r is -0.288
cor.test(test$OpenDev1km, test$PISoils)  # r is 0.015
cor.test(test$OpenDev1km, test$SISoils)  # r is -0.134
cor.test(test$OpenDev1km, test$HydricSoils)  # r is 0.035

#OpenDev5km by each
#cor.test(test$OpenDev5km, test$Treatment) #non-numeric
#cor.test(test$OpenDev5km, test$OpenDev5km)
cor.test(test$OpenDev5km, test$OpenDev30km) #r is 0.584
cor.test(test$OpenDev5km, test$Grass500m) #r is 0.411
cor.test(test$OpenDev5km, test$Grass1km) #r is 0.382
cor.test(test$OpenDev5km, test$Grass5km) #r is 0.274
cor.test(test$OpenDev5km, test$Grass30km) #r is 0.215
cor.test(test$OpenDev5km, test$Schrubs500m) #r is 0.347
cor.test(test$OpenDev5km, test$Schrubs1km) #r is 0.322
cor.test(test$OpenDev5km, test$Schrubs5km) #r is 0.040
cor.test(test$OpenDev5km, test$Schrubs30km) #r is -0.458
cor.test(test$OpenDev5km, test$Water500m) #r is -0.163
cor.test(test$OpenDev5km, test$Water1km) #r is 0.104
cor.test(test$OpenDev5km, test$Water5km) #r is 0.531
cor.test(test$OpenDev5km, test$Water30km) #r is -0.348
cor.test(test$OpenDev5km, test$NSoilTypes) #r is -0.061
cor.test(test$OpenDev5km, test$FPSiteIndex)  # r is -0.450
cor.test(test$OpenDev5km, test$SiteIndexPrimaryS)  # r is -0.372
cor.test(test$OpenDev5km, test$PISoils)  # r is 0.106
cor.test(test$OpenDev5km, test$SISoils)  # r is -0.003
cor.test(test$OpenDev5km, test$HydricSoils)  # r is 0.132

#OpenDev30km by each
#cor.test(test$OpenDev30km, test$Treatment) #non-numeric
#cor.test(test$OpenDev30km, test$OpenDev30km)
cor.test(test$OpenDev30km, test$Grass500m) #r is 0.260
cor.test(test$OpenDev30km, test$Grass1km) #r is 0.384
cor.test(test$OpenDev30km, test$Grass5km) #r is 0.276
cor.test(test$OpenDev30km, test$Grass30km) #r is 0.326
cor.test(test$OpenDev30km, test$Schrubs500m) #r is 0.184
cor.test(test$OpenDev30km, test$Schrubs1km) #r is 0.020
cor.test(test$OpenDev30km, test$Schrubs5km) #r is -0.314
cor.test(test$OpenDev30km, test$Schrubs30km) #r is -0.663
cor.test(test$OpenDev30km, test$Water500m) #r is -0.151
cor.test(test$OpenDev30km, test$Water1km) #r is -0.138
cor.test(test$OpenDev30km, test$Water5km) #r is 0.188
cor.test(test$OpenDev30km, test$Water30km) #r is -0.279
cor.test(test$OpenDev30km, test$NSoilTypes) #r is 0.020
cor.test(test$OpenDev30km, test$FPSiteIndex)  # r is -0.286
cor.test(test$OpenDev30km, test$SiteIndexPrimaryS)  # r is -0.195
cor.test(test$OpenDev30km, test$PISoils)  # r is 0.134
cor.test(test$OpenDev30km, test$SISoils)  # r is -0.219
cor.test(test$OpenDev30km, test$HydricSoils)  # r is 0.108

#Grass500m by each
#cor.test(test$Grass500m, test$Treatment) #non-numeric
#cor.test(test$Grass500m, test$Grass500m)
cor.test(test$Grass500m, test$Grass1km) #r is 0.585
cor.test(test$Grass500m, test$Grass5km) #r is 0.132
cor.test(test$Grass500m, test$Grass30km) #r is -0.091
cor.test(test$Grass500m, test$Schrubs500m) #r is 0.154
cor.test(test$Grass500m, test$Schrubs1km) #r is 0.057
cor.test(test$Grass500m, test$Schrubs5km) #r is 0.232
cor.test(test$Grass500m, test$Schrubs30km) #r is -0.116
cor.test(test$Grass500m, test$Water500m) #r is 0.220
cor.test(test$Grass500m, test$Water1km) #r is 0.214
cor.test(test$Grass500m, test$Water5km) #r is 0.333
cor.test(test$Grass500m, test$Water30km) #r is -0.152
cor.test(test$Grass500m, test$NSoilTypes) #r is -0.121
cor.test(test$Grass500m, test$FPSiteIndex)  # r is -0.365
cor.test(test$Grass500m, test$SiteIndexPrimaryS)  # r is -0.283
cor.test(test$Grass500m, test$PISoils)  # r is 0.177
cor.test(test$Grass500m, test$SISoils)  # r is -0.081
cor.test(test$Grass500m, test$HydricSoils)  # r is 0.130

#Grass1km by each
#cor.test(test$Grass1km, test$Treatment) #non-numeric
#cor.test(test$Grass1km, test$Grass1km)
cor.test(test$Grass1km, test$Grass5km) #r is 0.275
cor.test(test$Grass1km, test$Grass30km) #r is -0.003
cor.test(test$Grass1km, test$Schrubs500m) #r is 0.140
cor.test(test$Grass1km, test$Schrubs1km) #r is 0.198
cor.test(test$Grass1km, test$Schrubs5km) #r is 0.204
cor.test(test$Grass1km, test$Schrubs30km) #r is -0.043
cor.test(test$Grass1km, test$Water500m) #r is 0.129
cor.test(test$Grass1km, test$Water1km) #r is 0.194
cor.test(test$Grass1km, test$Water5km) #r is 0.321
cor.test(test$Grass1km, test$Water30km) #r is -0.026
cor.test(test$Grass1km, test$NSoilTypes) #r is 0.056
cor.test(test$Grass1km, test$FPSiteIndex)  # r is -0.193
cor.test(test$Grass1km, test$SiteIndexPrimaryS)  # r is -0.182
cor.test(test$Grass1km, test$PISoils)  # r is -0.003
cor.test(test$Grass1km, test$SISoils)  # r is -0.128
cor.test(test$Grass1km, test$HydricSoils)  # r is 0.266

#Grass5km by each
#cor.test(test$Grass5km, test$Treatment) #non-numeric
#cor.test(test$Grass5km, test$Grass5km)
cor.test(test$Grass5km, test$Grass30km) #r is 0.740
cor.test(test$Grass5km, test$Schrubs500m) #r is 0.040
cor.test(test$Grass5km, test$Schrubs1km) #r is -0.021
cor.test(test$Grass5km, test$Schrubs5km) #r is -0.261
cor.test(test$Grass5km, test$Schrubs30km) #r is -0.120
cor.test(test$Grass5km, test$Water500m) #r is 0.107
cor.test(test$Grass5km, test$Water1km) #r is 0.127
cor.test(test$Grass5km, test$Water5km) #r is -0.007
cor.test(test$Grass5km, test$Water30km) #r is -0.422
cor.test(test$Grass5km, test$NSoilTypes) #r is 0.126
cor.test(test$Grass5km, test$FPSiteIndex)  # r is 0.087
cor.test(test$Grass5km, test$SiteIndexPrimaryS)  # r is 0.207
cor.test(test$Grass5km, test$PISoils)  # r is 0.012
cor.test(test$Grass5km, test$SISoils)  # r is -0.086
cor.test(test$Grass5km, test$HydricSoils)  # r is 0.223

#Grass30km by each
#cor.test(test$Grass30km, test$Treatment) #non-numeric
#cor.test(test$Grass30km, test$Grass30km)
cor.test(test$Grass30km, test$Schrubs500m) #r is -0.178
cor.test(test$Grass30km, test$Schrubs1km) #r is -0.237
cor.test(test$Grass30km, test$Schrubs5km) #r is -0.620
cor.test(test$Grass30km, test$Schrubs30km) #r is -0.450
cor.test(test$Grass30km, test$Water500m) #r is -0.045
cor.test(test$Grass30km, test$Water1km) #r is -0.023
cor.test(test$Grass30km, test$Water5km) #r is -0.062
cor.test(test$Grass30km, test$Water30km) #r is -0.563
cor.test(test$Grass30km, test$NSoilTypes) #r is -0.011
cor.test(test$Grass30km, test$FPSiteIndex)  # r is 0.089
cor.test(test$Grass30km, test$SiteIndexPrimaryS)  # r is 0.228
cor.test(test$Grass30km, test$PISoils)  # r is -0.237
cor.test(test$Grass30km, test$SISoils)  # r is 0.041
cor.test(test$Grass30km, test$HydricSoils)  # r is 0.185

#Schrubs500m by each
#cor.test(test$Schrubs500m, test$Treatment) #non-numeric
#cor.test(test$Schrubs500m, test$Schrubs500m)
cor.test(test$Schrubs500m, test$Schrubs1km) #r is 0.753
cor.test(test$Schrubs500m, test$Schrubs5km) #r is 0.344
cor.test(test$Schrubs500m, test$Schrubs30km) #r is 0.049
cor.test(test$Schrubs500m, test$Water500m) #r is -0.142
cor.test(test$Schrubs500m, test$Water1km) #r is 0.154
cor.test(test$Schrubs500m, test$Water5km) #r is 0.403
cor.test(test$Schrubs500m, test$Water30km) #r is 0.026
cor.test(test$Schrubs500m, test$NSoilTypes) #r is -0.042
cor.test(test$Schrubs500m, test$FPSiteIndex)  # r is -0.040
cor.test(test$Schrubs500m, test$SiteIndexPrimaryS)  # r is -0.085
cor.test(test$Schrubs500m, test$PISoils)  # r is 0.035
cor.test(test$Schrubs500m, test$SISoils)  # r is 0.063
cor.test(test$Schrubs500m, test$HydricSoils)  # r is 0.038

#Schrubs1km by each
#cor.test(test$Schrubs1km, test$Treatment) #non-numeric
#cor.test(test$Schrubs1km, test$Schrubs1km)
cor.test(test$Schrubs1km, test$Schrubs5km) #r is 0.550
cor.test(test$Schrubs1km, test$Schrubs30km) #r is 0.142
cor.test(test$Schrubs1km, test$Water500m) #r is -0.093
cor.test(test$Schrubs1km, test$Water1km) #r is 0.395
cor.test(test$Schrubs1km, test$Water5km) #r is 0.550
cor.test(test$Schrubs1km, test$Water30km) #r is 0.057
cor.test(test$Schrubs1km, test$NSoilTypes) #r is -0.067
cor.test(test$Schrubs1km, test$FPSiteIndex)  # r is -0.131
cor.test(test$Schrubs1km, test$SiteIndexPrimaryS)  # r is -0.175
cor.test(test$Schrubs1km, test$PISoils)  # r is -0.064
cor.test(test$Schrubs1km, test$SISoils)  # r is 0.181
cor.test(test$Schrubs1km, test$HydricSoils)  # r is 0.012

#Schrubs5km by each
#cor.test(test$Schrubs5km, test$Treatment) #non-numeric
#cor.test(test$Schrubs5km, test$Schrubs5km)
cor.test(test$Schrubs5km, test$Schrubs30km) #r is 0.545
cor.test(test$Schrubs5km, test$Water500m) #r is -0.013
cor.test(test$Schrubs5km, test$Water1km) #r is 0.168
cor.test(test$Schrubs5km, test$Water5km) #r is 0.491
cor.test(test$Schrubs5km, test$Water30km) #r is 0.395
cor.test(test$Schrubs5km, test$NSoilTypes) #r is 0.206
cor.test(test$Schrubs5km, test$FPSiteIndex)  # r is -0.234
cor.test(test$Schrubs5km, test$SiteIndexPrimaryS)  # r is -0.319
cor.test(test$Schrubs5km, test$PISoils)  # r is 0.237
cor.test(test$Schrubs5km, test$SISoils)  # r is -0.121
cor.test(test$Schrubs5km, test$HydricSoils)  # r is 0.193

#Schrubs30km by each
#cor.test(test$Schrubs30km, test$Treatment) #non-numeric
#cor.test(test$Schrubs30km, test$Schrubs30km)
cor.test(test$Schrubs30km, test$Water500m) #r is 0.271
cor.test(test$Schrubs30km, test$Water1km) #r is 0.254
cor.test(test$Schrubs30km, test$Water5km) #r is 0.126
cor.test(test$Schrubs30km, test$Water30km) #r is 0.399
cor.test(test$Schrubs30km, test$NSoilTypes) #r is 0.207
cor.test(test$Schrubs30km, test$FPSiteIndex)  # r is 0.230
cor.test(test$Schrubs30km, test$SiteIndexPrimaryS)  # r is 0.076
cor.test(test$Schrubs30km, test$PISoils)  # r is 0.059
cor.test(test$Schrubs30km, test$SISoils)  # r is -0.055
cor.test(test$Schrubs30km, test$HydricSoils)  # r is 0.145

#Water500m by each
#cor.test(test$Water500m, test$Treatment) #non-numeric
#cor.test(test$Water500m, test$Water500m) 
cor.test(test$Water500m, test$Water1km) #r is 0.537
cor.test(test$Water500m, test$Water5km) #r is -0.110
cor.test(test$Water500m, test$Water30km) #r is -0.115
cor.test(test$Water500m, test$NSoilTypes) #r is -0.122
cor.test(test$Water500m, test$FPSiteIndex)  # r is -0.020
cor.test(test$Water500m, test$SiteIndexPrimaryS)  # r is -0.049
cor.test(test$Water500m, test$PISoils)  # r is 0.096
cor.test(test$Water500m, test$SISoils)  # r is 0.083
cor.test(test$Water500m, test$HydricSoils)  # r is -0.047

#Water1km by each
#cor.test(test$Water1km, test$Treatment) #non-numeric
#cor.test(test$Water1km, test$Water1km)
cor.test(test$Water1km, test$Water5km) #r is 0.259
cor.test(test$Water1km, test$Water30km) #r is -0.150
cor.test(test$Water1km, test$NSoilTypes) #r is -0.333
cor.test(test$Water1km, test$FPSiteIndex)  # r is -0.186
cor.test(test$Water1km, test$SiteIndexPrimaryS)  # r is -0.174
cor.test(test$Water1km, test$PISoils)  # r is -0.074
cor.test(test$Water1km, test$SISoils)  # r is 0.106
cor.test(test$Water1km, test$HydricSoils)  # r is -0.196

#Water5km by each
#cor.test(test$Water5km, test$Treatment) #non-numeric
#cor.test(test$Water5km, test$Water5km)
cor.test(test$Water5km, test$Water30km) #r is 0.074
cor.test(test$Water5km, test$NSoilTypes) #r is 0.061
cor.test(test$Water5km, test$FPSiteIndex)  # r is -0.444
cor.test(test$Water5km, test$SiteIndexPrimaryS)  # r is -0.444
cor.test(test$Water5km, test$PISoils)  # r is -0.011
cor.test(test$Water5km, test$SISoils)  # r is -0.118
cor.test(test$Water5km, test$HydricSoils)  # r is 0.418

#Water30km by each
#cor.test(test$Water30km, test$Treatment) #non-numeric
#cor.test(test$Water30km, test$Water30km)
cor.test(test$Water30km, test$NSoilTypes) #r is 0.101
cor.test(test$Water30km, test$FPSiteIndex)  # r is 0.264
cor.test(test$Water30km, test$SiteIndexPrimaryS)  # r is 0.098
cor.test(test$Water30km, test$PISoils)  # r is 0.024
cor.test(test$Water30km, test$SISoils)  # r is 0.040
cor.test(test$Water30km, test$HydricSoils)  # r is -0.030

#soils with one another:
cor.test(test$NSoilTypes, test$FPSiteIndex)  # r is 0.045
cor.test(test$NSoilTypes, test$SiteIndexPrimaryS)  # r is -0.077
cor.test(test$NSoilTypes, test$PISoils)  # r is -0.077
cor.test(test$NSoilTypes, test$SISoils)  # r is -0.262
cor.test(test$NSoilTypes, test$HydricSoils)  # r is 0.533    #medium!

cor.test(test$FPSiteIndex, test$SiteIndexPrimaryS) #0.875   #HIGHLY, duh
cor.test(test$FPSiteIndex, test$PISoils) #-.130
cor.test(test$FPSiteIndex, test$SISoils) #0.181
cor.test(test$FPSiteIndex, test$HydricSoils) #-0.064

cor.test(test$SiteIndexPrimaryS, test$PISoils) #-0.172
cor.test(test$SiteIndexPrimaryS, test$SISoils) #0.308   #medium
cor.test(test$SiteIndexPrimaryS, test$HydricSoils)  #-0.147

cor.test(test$PISoils, test$SISoils) #-0.358    medium
cor.test(test$PISoils, test$HydricSoils) #0.043

cor.test(test$SISoils, test$HydricSoils) #-0.361   #medium
