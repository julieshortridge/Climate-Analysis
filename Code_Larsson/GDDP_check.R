## CHECK TO MAKE SURE ALL FILES ARE PRESENT

modelnames <- c("UKESM1-0-LL","TaiESM1","NorESM2-MM",
                "NorESM2-LM","NESM3","MRI-ESM2-0","MPI-ESM1-2-LR",
                "MPI-ESM1-2-HR","MIROC6","MIROC-ES2L","KIOST-ESM",
                "KACE-1-0-G","IPSL-CM6A-LR","INM-CM5-0","INM-CM4-8",
                "IITM-ESM","HadGEM3-GC31-MM","HadGEM3-GC31-LL",
                "GISS-E2-1-G","GFDL-ESM4","GFDL-CM4_gr2","GFDL-CM4","FGOALS-g3",       #23
                "EC-Earth3-Veg-LR","EC-Earth3","CanESM5","CNRM-ESM2-1","CNRM-CM6-1",      #28
                "CMCC-ESM2","CMCC-CM2-SR5","CESM2-WACCM","CESM2",
                "BCC-CSM2-MR","ACCESS-ESM1-5","ACCESS-CM2")


#get file of links
csv.loc <- "~/R/Climate-Analysis/Code_Larsson"
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

#select downloaded parameters
CFdesired <- c("pr", "tasmax", "tasmin", "sfcWind")
#trimmed names and links to desired CF parameters
filenames <- grep(paste(CFdesired, collapse="|"), filenames, value=TRUE)
filelinks <- grep(paste(CFdesired, collapse="|"), filelinks, value=TRUE)


#establish data frame
GDDP.check <- data.frame(matrix(NA,
                                nrow=length(modelnames),
                                ncol=3))
colnames(GDDP.check) <- c("Model", "AvailableFiles", "DownloadedFiles")


modelpathbase <- "G:\\Shared drives\\LarssonWork\\NEXGDDP\\Downloads\\"
for(i in 1:25){    #length(modelnames)){
  
  #select model
  model <- modelnames[i]
  GDDP.check$Model[i] <- model
  
  model.quantity <- grep(model, filenames, value=TRUE)
  GDDP.check$AvailableFiles[i] <- length(model.quantity)

  model.loc <- paste0(modelpathbase,model)
  setwd(model.loc)
  model.downloads <- list.files()
  GDDP.check$DownloadedFiles[i] <- length(model.downloads)-2
  
}
