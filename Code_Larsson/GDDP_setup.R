# Download NEX-GDDP data: CMIP6 Version
# Extract attributes and clip to CONUS using NCfile_check_JR

# 1/17/2023 changes
# added lines 32 - 41
# added lines 261 - code end
#

# NOTES FOR 2022 VERSION -----

# Required files: gddp-cmip6-files.csv
# This .csv file contains the links for the .nc files on the aws
# 
# If running file on server, remove any setwd() and make 
# sure everything is running in the same directory
#
# Required changes are marked with "***"
#



### LIBRARIES -------
rm(list=ls())

library(ncdf4)
library(lubridate)
library(raster)
library(rgdal)
library(httr)
library(utils)
#library(googledrive)

### INFO THAT'S GOOD TO HAVE ----
modelnames <- c("UKESM1-0-LL","TaiESM1","NorESM2-MM",
            "NorESM2-LM","NESM3","MRI-ESM2-0","MPI-ESM1-2-LR",
            "MPI-ESM1-2-HR","MIROC6","MIROC-ES2L","KIOST-ESM",
            "KACE-1-0-G","IPSL-CM6A-LR","INM-CM5-0","INM-CM4-8",
            "IITM-ESM","HadGEM3-GC31-MM","HadGEM3-GC31-LL",
            "GISS-E2-1-G","GFDL-ESM4","GFDL-CM4_gr2","GFDL-CM4",            "FGOALS-g3",       #23
            "EC-Earth3-Veg-LR","EC-Earth3","CanESM5","CNRM-ESM2-1",            "CNRM-CM6-1",      #28
            "CMCC-ESM2","CMCC-CM2-SR5","CESM2-WACCM","CESM2",
            "BCC-CSM2-MR","ACCESS-ESM1-5","ACCESS-CM2")  


# *** change csv.loc to location of downloaded .csv file
#windows
#csv.loc <- "G:\\Shared drives\\LarssonWork\\NEXGDDP"
#filepathbase <- "G:\\Shared drives\\LarssonWork\\NEXGDDP\\tempFolder\\" 

# Change for linux
# *** if running on server, change filepathbase to something linux compatible
# *** change filepathbase to where files will save on local machine
csv.loc <- "/home/jshortr1/NEXGDDP"
filepathbase <- "/home/jshortr1/NEXGDDP/tempFolder/"


#read in .csv file, isolate links, fix formatting
setwd(csv.loc)
filecsv <- read.csv("gddp-cmip6-files.csv")
filelinksraw <- as.character(filecsv$fileURL)
filelinks <- gsub(" ","", filelinksraw)

#extract file names from file links
splitlinks <- strsplit(filelinks, split = "/")
filenames <- vector()
for (i in 1:length(splitlinks)){
  fn <- splitlinks[[i]][9]
  filenames <- c(filenames, fn)
}

#Climate and Forcast (CF) Parameters (all)
CFnames <- c("hurs", "huss", "pr", "rlds", "rsds", 
             "sfcWind", "tas", "tasmax", "tasmin") 

# *** select desired parameters from list above
#only need tasmax, tasmin, pr, sfcWind
CFdesired <- c("pr", "tasmax", "tasmin", "sfcWind")

#trimmed names and links to desired CF parameters
filenames <- grep(paste(CFdesired, collapse="|"), filenames, value=TRUE)
filelinks <- grep(paste(CFdesired, collapse="|"), filelinks, value=TRUE)

### FUNCTION TO CROP .nc FILE TO MID-ATLANTIC EXTENT & CRATE NEW FILE -----
NCfile_check_mAtl<-function(destname, path){
  ## OPEN .nc FILE, PULL ATTRIBUTE INFO
  try ({
    nc.loc=path
    setwd(nc.loc)
    nc.test=nc_open(destname)
  
    # Extract info about variable data
    varID <- attributes(nc.test$var)$names	# extract variable ID
    varUnits <- ncatt_get(nc.test,varID,"units")$value
    varLongName <- ncatt_get(nc.test,varID,"long_name")$value
  
    # Extract lat/lon values
    lon <- ncvar_get(nc.test,"lon")
    nlon <- dim(lon)	# x-dim is 1440, can also see this from print function
    lat <- ncvar_get(nc.test,"lat")
    nlat <- dim(lat)
  
    # Check time and reformat (this might be easier to do just by reading the year from the file name)
    time <- ncvar_get(nc.test,"time")
    tunits <- ncatt_get(nc.test,"time","units")
    ndays <- dim(time)	# make sure to check for leap years - 
    year <- floor((time[1]-0.5)/365)+2006 
    start.date<-paste(year,"-01-01",sep="")
    # create logical test to deal with leap years (pretty sure they aren't represented in NEX-GDDP)
    if (leap_year(year) == FALSE){
      date.seq<-as.Date(0:(ndays-1),origin=start.date)
    }else{
      date.seq<-as.Date(0:(ndays),origin="2020-01-01")
      date.seq<-date.seq[-60]	# remove Feb 29
    }
  
    # Get some global attributes from original nc file
    nc.title<-paste("CONUS Extent: ",ncatt_get(nc.test,0,"title")$value,sep="")
    nc.experiment<-ncatt_get(nc.test,0,"experiment")$value
    })
    try ({
    nc_close(nc.test)	# close connection with file so new one can be written
    })
  ## LOAD DATA AS RASTER BRICK, CROP, WRITE TO NEW FILE
    ncfname=destname
    tasmax.brick <- brick(ncfname,varname=varID)
    #tasmax.crop <- crop(tasmax.brick , extent(230, 295, 20, 50))  # CONUS
    atl=c(270,287,30,43)
    tasmax.crop <- crop(tasmax.brick , extent(atl))  # MD, VA, NC, SC

    tasmax.data <- as.array(tasmax.crop)
    # Need to permute dimensions and flip on y-axis so that array matches the way nc files are written
    tasmax.perm <- aperm(tasmax.data,c(2,1,3))
    tasmax.final <- tasmax.perm[,dim(tasmax.perm)[2]:1,]
  
    # Convert cropped extent to nc format
    ncpath <- nc.loc
    ncfname.ATL<-paste(ncpath,"ATL_",ncfname,sep="")
  
    lonATL<-lon[which(lon > atl[1] & lon < atl[2])]
    latATL<-lat[which(lat > atl[3] & lat < atl[4])]
    # length(latCONUS); length(lonCONUS)	# dimensions are 120 x 260 - same as tasmax.data
  
    # Define dimensions
    londim <- ncdim_def("lon","degrees_east",as.double(lonATL)) 
    latdim <- ncdim_def("lat","degrees_north",as.double(latATL)) 
    timedim <- ncdim_def("time",tunits$value,as.double(time))
  
    # Define variables
    fillvalue <- -1*10^12 #places very large number in for NA data
    var_def <- ncvar_def(name=varID,units=varUnits,dim=list(londim,latdim,timedim),missval=fillvalue,
                       longname=varLongName)	
  
    # Create file and add variables/attributes
    # If file already exists, nc_create will fail!!
    ncout <- nc_create(ncfname.ATL,vars=var_def,force_v4=TRUE)
    
    filenameATL <- ncfname.ATL
    filepathATL <- paste0(nc.loc,filenameATL)
  
    ncvar_put(ncout,var_def,tasmax.final)
    ncatt_put(ncout,"lon","axis","X")
    ncatt_put(ncout,"lat","axis","Y")
    ncatt_put(ncout,"time","axis","T")
  
    ncatt_put(ncout,0,"title",nc.title)
    ncatt_put(ncout,0,"experiment",nc.experiment)
  
    rm(tasmax.brick,tasmax.crop)
    closeAllConnections()
}

### GET FILES FUNCTION -----

get_files <- function(iter.id) {
  #error checks
    #changes test.ids to file.ids when running function
    cat(paste(iter.id), file="0.currentfile.txt")
    
  #create necessary variables
    filename <- filenames[iter.id]
    filelink <- filelinks[iter.id]
    filepath <- paste0(filepathbase,filename)
    #driveID <- driveIDs.vector[iter.id]
    wd <- getwd()
    NCfilepath <- paste(wd, "/", sep='')
    # Set dummy var to catch missing files
    dummy <- 0
    #retrieve data via link and save to disk as a temporary file
    try ({
      file <- httr::GET(filelink)
      sc <- status_code(file)
    
      if(sc==200){ #check for successful code
      #download file
        bin <- content(file, "raw") #get file content
        writeBin(bin, filename) #write file to computer
        #crop file using NCfile_check_mAtl function
        NCfile_check_mAtl(filename, NCfilepath)
        filenameATL <- paste0("ATL_", filename)
      #remove global file
        file.remove(filepath) #removes global raster file, keeps trimmed
        dummy <- 1
        }
    })
    if (dummy == 0){
      cat(paste("Error: ", filename), "\n", file=error.track, append=TRUE, sep='')
    }
}



