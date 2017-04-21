####################################################################
# Author:
#  Nicolas Fortin St-Gelais, nicolas.fstgelais@gmail.com
# Description:
#  This script takes as an input temperature depth profiles and calculate
#   - thermocline depth
#   - metalimnion depth
#   - relative thermal resistance (RTR, water column stability) 
#   - average temperature  (Water column and epilimnion) 
#
####################################################################

rm(list=ls(all=TRUE))

library(rLakeAnalyzer)
library(RCurl)

# You need a file with the following colums: 
# SampleID = an ID for each sample 
# Zsample = the depth of each sample
# wtr = the water temperature at a given depth
# Import the profiles directly from Bitbucket using RCurl
profiles <-read.csv(text=getURL("https://bitbucket.org/GRIL_limno/gril_script/raw/780da6e62e80be82044ff2280cef1e0763cf462b/r/RLakeAnalyzer/profiles.csv"), header=T)

# Create a final matrix, rows=samples columns = measures
thermo=matrix(NA,length(unique(profiles$SampleID)),6,dimnames=list(unique(profiles$SampleID),c("thermoD","metaD1","metaD2","RTR","tempEpi","tempWc")))

# Function to calculate water density, will use later
po=function(wtr){
  p=(1-((wtr+288.9414)/(508929.2*(wtr+68.12963)))*(wtr-3.9863)^2)*1000
  return(p)}


# All the calculations are done within this loop.
# Thermocline calculations are done using the RLakeAnalyzer library
# If there is no thermocline the value will be set to NaN
# Water column (Wc) and epilimnetic (epi) temperature are calculated 
# using trapezoid integration. 

for(i in unique(profiles$SampleID)){
  wtr=profiles[profiles[,"SampleID"]%in%i,"wtr"]
  depths=profiles[profiles[,"SampleID"]%in%i,"Zsample"]
  thermo[i,"thermoD"]=round(thermo.depth(wtr, depths),2)
  thermo[i,"metaD1"]=round( meta.depths(wtr, depths),2)[1]
  thermo[i,"metaD2"]=round( meta.depths(wtr, depths),2)[2]
  thermo[i,"RTR"]=round(abs((po(wtr[1])-po(wtr[length(wtr)]))/(po(4)-po(5))))
  thermo[i,"tempEpi"]=round(trapz(depths[depths< thermo[i,"thermoD"]],wtr[depths< thermo[i,"thermoD"]])/(max(depths[depths< thermo[i,"thermoD"]])-min(depths)),2)
  thermo[i,"tempWc"]=round(trapz(depths,wtr)/(max(depths)-min(depths)),2)
  }

#output
head (thermo)
