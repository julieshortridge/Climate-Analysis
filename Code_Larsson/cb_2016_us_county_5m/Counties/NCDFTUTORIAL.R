## SETUP
rm(list=ls())

library(ncdf4)
library(raster)
library(rgdal)
library(rgeos)

county.location <- "C:\\Users\\nalar\\Documents\\R\\PRISM\\cb_2016_us_county_5m\\Counties"
gddp.loc <- "G:\\Shared drives\\LarssonWork\\NEXGDDP\\Testfiles"
output.loc <- "G:\\My Drive\\R_Tutorials\\NCDF\\Output"

setwd(gddp.loc)
nc.files <- list.files()
nc.files <- nc.files[4:13]


first.raster <- brick(nc.files[1])
# nc.filename <- nc.files[2]
# 
# ncin <- nc_open(nc.filename)
# print(ncin)
# nc_close(ncin)
# 
# ncin <- nc_open(nc.filename)
# ncatt_get( ncin, varid=0)
setwd(county.location)
counties.shape <- readOGR(dsn=".", layer="cb_2016_us_county_5m")

counties.shape #view & check metadata

StateFP.codes <- data.frame(state=c("SC", "NC", "VA", "MD"), FP=c(45,37,51,24))
counties.shape <- counties.shape[counties.shape$STATEFP %in% StateFP.codes$FP, ]
plot(counties.shape)

#rotate and change spatial extent
counties.reproj <- spTransform(counties.shape, crs(first.raster))

setwd(gddp.loc)
for (i in 1:length(nc.files)) {
  nc.filename <- nc.files[i]
  pr.raster <- brick(nc.filename)
  pr.raster
  
  pr.raster <- pr.raster*86400
  pr.raster <- rotate(pr.raster)
  
  plot(pr.raster[[1]]); lines(counties.reproj)
  plot(pr.raster[[180]]); lines(counties.reproj)
  
  total.precip <- calc(pr.raster, fun=sum)
  
  pdf(paste0("plot_raster",i))
  plot(total.precip); lines(counties.reproj)
  dev.off()
}

bb.precip <- vector()
for(i in 1:length(nc.files)) {
  ncin <- nc_open(nc.files[i])
  pr.data <- ncvar_get(ncin,"pr")
  class(pr.data)
  dim(pr.data)
  pr.data<-pr.data*86400
  summary(pr.data)
  nc_close(ncin)
  
  pr.bburg<-pr.data[15,21,]
  #lon[15]; lat[21]
  plot(pr.bburg, type="l")
  bb.precip <- c(bb.precip, sum(pr.bburg))
} 

print(bb.precip)
