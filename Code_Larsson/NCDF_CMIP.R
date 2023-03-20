### SETUP ------
#clear environment
rm(list=ls())

#libraries
library(ncdf4)
library(raster)
library(rgdal)
library(rgeos)

### GET COUNTY SHAPEFILE --------
county.loc <- "C:\\Users\\nalar\\Documents\\R\\PRISM\\cb_2016_us_county_5m\\Counties"
setwd(county.loc)
counties.shape<-readOGR(dsn=".",layer="cb_2016_us_county_5m") 

st.fps <- c(39, #ohio
            54, #wv
            51, #va
            47, #tn
            21, #ky
            37, #nc
            45) #sc 

app.counties <- subset(counties.shape, counties.shape$STATEFP == st.fps[1] |
                         counties.shape$STATEFP == st.fps[2] |
                         counties.shape$STATEFP == st.fps[3] |
                         counties.shape$STATEFP == st.fps[4] |
                         counties.shape$STATEFP == st.fps[5] |
                         counties.shape$STATEFP == st.fps[6] |
                         counties.shape$STATEFP == st.fps[7])

VA.counties <- subset(app.counties, app.counties$STATEFP == "51")
Montgomery.County <- subset(VA.counties, VA.counties$NAME == "Montgomery")
### ATTRIBUTES & SUMMARIES --------
#Confirm one from each types

#copied folder from Google Drive to local drive
CMIPfiles.loc <- "C:\\Users\\nalar\\Documents\\R\\NCDF\\UKESM1-0-LL_historical"
summary.loc <- "G:\\Shared drives\\LarssonWork\\NEXGDDP\\Processing"
images.loc <- "C:\\Users\\nalar\\Documents\\R\\NCDF\\Images"
setwd(CMIPfiles.loc)


#Attributes of files including: variables, extent, statistical summary
#convert to relevant units: temp = degrees C; precip = mm/day
getFileAttributes <- function(pat) {
  
  setwd(CMIPfiles.loc)
  nc.files <- list.files(pattern=pat)
  nc.filename <- nc.files[1]
  nc.filename

  ncin <- nc_open(nc.filename)
  print(ncin)
  nc_close(ncin)

  ncin <- nc_open(nc.filename)
  ncatt_get( ncin, varid=0)
  ncatt_get( ncin, varid="time")
  ncatt_get( ncin, varid="lat")
  ncatt_get( ncin, varid="lon")


  lon<-ncvar_get(ncin,"lon")
  lat<-ncvar_get(ncin,"lat")
  time<-ncvar_get(ncin,"time")

  nlon<-dim(lon)
  nlat<-dim(lat)
  ntime<-dim(time)
  print(c(nlon, nlat, ntime))
  
  setwd(summary.loc)
  att.data <- ncvar_get(ncin, varid=pat)
  
  if(pat=="pr"){
    att.data <- att.data*86400
  } else {
    att.data <- att.data-273.15
  }
  
  att.sum <- summary(att.data)
  write.csv(as.matrix(att.sum), file=paste0("summary_",pat,".csv"))
  #turn into csv
  return(att.data)
}

getFileAttributes("tasmin")
#getFileAttributes("tasmax")
getFileAttributes("pr")



### CHOOSING A LOCATION ----
#try to get precipitation data for blacksburg

#read in data
setwd(CMIPfiles.loc)
nc.files <- list.files(pattern="pr")
nc.filename <- nc.files[1]
nc.filename

ncin <- nc_open(nc.filename)
print(ncin)
nc_close(ncin)

ncin <- nc_open(nc.filename)
ncatt_get( ncin, varid=0)
ncatt_get( ncin, varid="time")
ncatt_get( ncin, varid="lat")
ncatt_get( ncin, varid="lon")

lon<-ncvar_get(ncin,"lon")
lat<-ncvar_get(ncin,"lat")
time<-ncvar_get(ncin,"time")

nlon<-dim(lon)
nlat<-dim(lat)
ntime<-dim(time)
print(c(nlon, nlat, ntime))


#target location -- Blacksburg, VA
bb.lon <- -80.413939 #degrees west
bb.lat <- 37.229573 #degrees north
#need degrees east. convert
bb.lon <- 360-abs(bb.lon)
bb.lon #check value

bb.xcoord <- max(which(lon < bb.lon))
bb.ycoord <- max(which(lat < bb.lat))

lon[bb.xcoord]
lat[bb.ycoord]

#once we get into the raster part of this, PLOT!!

### DEALING WITH TIME -----
setwd("C:\\Users\\nalar\\Documents\\R\\NCDF\\UKESM-0-LL_ssp585\\ssp585")

checkTimeDim <- function(iter.id) {
  nc.files <- list.files(pattern=as.character(
    paste0("ATL_tasmin_day_UKESM1-0-LL_ssp585_r1i1p1f2_gn_", iter.id, ".nc")))
  nc.filename <- nc.files[1]
  
  ncin <- nc_open(nc.filename)
  time <- ncvar_get(ncin, "time")
  ncatt_get(ncin,"time","units")
  print(dim(time)) 
  nc_close(ncin)
}

# #historical
# for(i in 1950:2014){
#   checkTimeDim(i)
# }

#future
for(i in 2015:2100){
  checkTimeDim(i)
}

# testlength <- seq(1950:2014)
# sapply(testlength, FUN = checkTimeDim)
#nc.files <- list.files(pattern="ATL_tasmin_day_UKESM1-0-LL_historical_r1i1p1f2_gn_2007.nc")


summary(time)
ncatt_get(ncin,"time","units")


#checking/extracting file metadata
#make into function??


### EXTRACTING AND PROCESSING DATA -----
setwd(CMIPfiles.loc)
nc.files.1981<-list.files(pattern="1981")

fill<-rep(NA,3)
file.summary<-data.frame(variable=fill, units=fill, fill.value=fill, min.value=fill, mean.value=fill, max.value=fill,
                         ndays=fill, nlon=fill, nlat=fill)
for (f in 1:length(nc.files.1981)){
  ncfname <- nc.files.1981[f]
  ncin <- nc_open(ncfname)

  file.summary$variable[f]<- names(ncin$var)
  file.summary$units[f]<-  ncatt_get(ncin,varid=names(ncin$var))[1]
  file.summary$fill.value[f]<- ncatt_get(ncin,varid=names(ncin$var))[2]
  file.summary$ndays[f]<- dim(ncvar_get(ncin,"time"))
  file.summary$nlon[f]<- dim(ncvar_get(ncin,"lon"))
  file.summary$nlat[f]<- dim(ncvar_get(ncin,"lat"))
  
  file.data <- ncvar_get(ncin,varid=names(ncin$var))
  file.summary$min.value[f]<- min(file.data)
  file.summary$mean.value[f]<- mean(file.data)
  file.summary$max.value[f]<- max(file.data)
  
  nc_close(ncin)  
  #rm(ncfname, ncin)
}
file.summary




#polygon operations
setwd(CMIPfiles.loc)
nc.filename <- nc.files.1981[3]
tmin.raster<-brick(nc.filename)
tmin.raster

#convert to C
tmin.raster <- tmin.raster-273.15

plot(tmin.raster[[1]])
plot(tmin.raster[[180]])

#make county layers match coordinate system of NetCDF files
app.reproj <- spTransform(app.counties,crs(tmin.raster))

VA.counties.reproj <- spTransform(VA.counties, crs(tmin.raster))
Montgomery.County.reproj <- spTransform(Montgomery.County, crs(tmin.raster))

#check extents
extent(app.reproj)
extent(tmin.raster)

#rotate to get lon values correct
tmin.raster <- rotate(tmin.raster)
extent(tmin.raster)
#plot to check spatial extent






#convert lon variable to degrees west
ncin <- nc_open(nc.filename)
lat<-ncvar_get(ncin,"lat")
lon<-ncvar_get(ncin,"lon")
nc_close(ncin)

lon <- (360-lon)*-1

#target location -- Blacksburg, VA -- coordinates from blacksbur nws
bb.lon <- -80.43 #degrees west
bb.lat <- 37.24 #degrees north
#need degrees east. convert
# bb.lon.east <- 360-abs(bb.lon)
# bb.lon.east #check value

bb.xindex <- max(which(lon < bb.lon))
bb.xindex
lon[bb.xindex]
lon[bb.xindex+1]
bb.yindex <- max(which(lat < bb.lat))
bb.yindex
lat[bb.yindex]
lat[bb.yindex+1]

# lon[bb.xcoord]-360
#i think the issue might be this rotation?? check w julie tomorrow

#Montgomery County raster is around cell 1600, 
#which is approximately row 24 & col 36
#current results yielding row 29, col 38
#cell (0,0) in upper left corner

#check previous blacksburg raster cell
# cell <- cellFromRowCol(tmin.raster, bb.yindex, bb.xindex)
# rc <- rowColFromCell(tmin.raster, cell)
#setwd(images.loc)
#png("CheckSpatialExtentError.png")
plot(tmin.raster[[1]]); lines(app.reproj)
plot(extent(tmin.raster[[1]], bb.yindex, bb.yindex, bb.xindex, bb.xindex), add=TRUE, col="red", lwd=3);
lines(Montgomery.County.reproj, col="blue", lwd=3)
#points(bb.xcoord, bb.ycoord , type="p", col="red", pch=19)
#dev.off()

# val <- extract(tmin.raster, data.frame(bb.lon, bb.lat))
# View(val)
# 
# val2 <- extract(tmin.raster, data.frame(bb.xcoord, bb.ycoord))
# View(val)
#check extent of rotated object
extent(tmin.raster)
extent(app.reproj)


#extract part of tutorial


