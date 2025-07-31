## Extract CHL time series from ERDDAP

### Timeseries:
#SeaWiFS(ID = erdSWchlamday)
#MODIS(ID = erdMH1chlamday)
#VIIRS(ID = nesdisVHNSQchlaMonthly)
#OC-CCI(ID = pmlEsaCCI50OceanColorMonthly)


### package library
library(rerddap)
library(plotdap)
library(rerddapXtracto)
library(lubridate)
library(maps)
library(mapdata)
library(mapproj)
library(ncdf4)
library(reshape2)
library(colorRamps)
library(sp)
library(plyr)
library(ggplot2)
library(gridExtra)

### set area boundaries
xcoord<-c(-120, -115)
ycoord<-c(31,34)

ttext<-paste(paste(abs(xcoord), collapse="-"),"W, ", paste(ycoord, collapse="-"),"N")

### Get seawifs data
dataInfo <- rerddap::info('erdSWchlamday')
dataInfo

# Extract the parameter name from the metadata in dataInfo
parameter <- dataInfo$variable$variable_name

# Set the altitude coordinate to zero
zcoord <- 0.

# Extract the beginning and ending dates of the dataset from the metadata in dataInfo
global <- dataInfo$alldata$NC_GLOBAL
tt <- global[ global$attribute_name %in% c('time_coverage_end','time_coverage_start'), "value", ]

# Populate the time vector with the time_coverage_start from dataInfo
# Use the "last" option for the ending date
tcoord <- c(tt[2],"last")

# Extract the timeseries data using rxtracto_3D
chlSeaWiFS<-rerddapXtracto::rxtracto_3D(dataInfo,parameter=parameter,
                        tcoord=tcoord,
                        xcoord=xcoord,ycoord=ycoord,zcoord=zcoord)

# Remove extraneous zcoord dimension for chlorophyll 
chlSeaWiFS$chlorophyll <- drop(chlSeaWiFS$chlorophyll)

##########################

### Get MODIS data
dataInfo <- rerddap::info('erdMH1chlamday')
dataInfo

# Extract the parameter name from the metadata in dataInfo
parameter <- dataInfo$variable$variable_name

#Extract the start and end times of the dataset from the metadata in dataInfo
global <- dataInfo$alldata$NC_GLOBAL

# Populate the time vector with the time_coverage_start from dataInfo
# Use the "last" option for the ending date
tt <- global[ global$attribute_name %in% c('time_coverage_end','time_coverage_start'), "value", ]
tcoord <- c(tt[2],"last")

# Run rxtracto_3D
chlMODIS<-rxtracto_3D(dataInfo,parameter=parameter,
                      tcoord=tcoord,
                      xcoord=xcoord,ycoord=ycoord)

###############################

### Get VIIRS data (DOES NOT WORK)
dataInfo <- rerddap::info('erdVHNchlamday')
dataInfo

## This extracts the parameter name from the metadata in dataInfo
parameter <- dataInfo$variable$variable_name

#Extract the start and end times of the dataset from the metadata in dataInfo
global <- dataInfo$alldata$NC_GLOBAL

# Populate the time vector with the time_coverage_start from dataInfo
# Use the "last" option for the ending date
tt <- global[ global$attribute_name %in% c('time_coverage_end','time_coverage_start'), "value", ]
tcoord <- c(tt[1], tt[2])

# Run rxtracto_3D
chlVIIRS<-rxtracto_3D(dataInfo,parameter=parameter,
                      tcoord=tcoord,
                      xcoord=xcoord,ycoord=ycoord,zcoord=zcoord)

# Remove extraneous zcoord dimension for chlorophyll 
chlVIIRS$chla <- drop(chlVIIRS$chla)

####################

### Create Time Series of Mean Monthly Data

## Spatially average all the data within the box for each dataset.
## The c(3) indicates the dimension to keep - in this case time 
chlSeaWiFS$avg <- apply(chlSeaWiFS$chlorophyll, c(3),function(x) mean(x,na.rm=TRUE))
chlMODIS$avg <- apply(chlMODIS$chlorophyll, c(3),function(x) mean(x,na.rm=TRUE))
#chlVIIRS$avg <- apply(chlVIIRS$chla, c(3),function(x) mean(x,na.rm=TRUE))

## Temporally average all of the data into one map 
## The c(1,2) indicates the dimensions to keep - in this case latitude and longitude  
chlSeaWiFS$avgmap <- apply(chlSeaWiFS$chlorophyll,c(1,2),function(x) mean(x,na.rm=TRUE))
chlMODIS$avgmap <- apply(chlMODIS$chlorophyll,c(1,2),function(x) mean(x,na.rm=TRUE))
#chlVIIRS$avgmap <- apply(chlVIIRS$chla,c(1,2),function(x) mean(x,na.rm=TRUE))

### Plot time series

plot(as.Date(chlSeaWiFS$time), chlSeaWiFS$avg, 
     type='b', bg="blue", pch=21, xlab="", cex=.7,
     xlim=as.Date(c("1997-01-01","2019-01-01")),
     ylim=c(0,3),
     ylab="Chlorophyll", main=ttext)
#axis(2)

# Now add MODIS and VIIRS  data 
points(as.Date(chlMODIS$time), chlMODIS$avg, type='b', bg="red", pch=21,cex=.7)

text(as.Date("1997-03-01"),2.8, "SeaWiFS",col="blue", pos=4)
text(as.Date("1997-03-01"),2.5, "MODIS",col="red", pos=4)

#####################

### Add OCCCI data
dataInfo <- rerddap::info('pmlEsaCCI50OceanColorMonthly')

# This identifies the parameter to choose - there are > 60 in this dataset1 
parameter <- 'chlor_a'

global <- dataInfo$alldata$NC_GLOBAL
tt <- global[ global$attribute_name %in% c('time_coverage_end','time_coverage_start'), "value", ]
tcoord <- c(tt[2],"last")

chlOCCCI<-rxtracto_3D(dataInfo,parameter=parameter,
                      tcoord=tcoord,
                      xcoord=xcoord,ycoord=ycoord)

# Now spatially average the data into a timeseries
chlOCCCI$avg <- apply(chlOCCCI$chlor_a, c(3),function(x) mean(x,na.rm=TRUE))

# Now temporally average the data into one map 
chlOCCCI$avgmap <- apply(chlOCCCI$chlor_a,c(1,2),function(x) mean(x,na.rm=TRUE))
