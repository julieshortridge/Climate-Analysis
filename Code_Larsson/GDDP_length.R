#check number of files downloaded in each data set
modelnames <- c("UKESM1-0-LL","TaiESM1","NorESM2-MM",
                "NorESM2-LM","NESM3","MRI-ESM2-0","MPI-ESM1-2-LR",
                "MPI-ESM1-2-HR","MIROC6","MIROC-ES2L","KIOST-ESM",
                "KACE-1-0-G","IPSL-CM6A-LR","INM-CM5-0","INM-CM4-8",
                "IITM-ESM","HadGEM3-GC31-MM","HadGEM3-GC31-LL",
                "GISS-E2-1-G","GFDL-ESM4","GFDL-CM4_gr2","GFDL-CM4","FGOALS-g3",       #23
                "EC-Earth3-Veg-LR","EC-Earth3","CanESM5","CNRM-ESM2-1","CNRM-CM6-1",      #28
                "CMCC-ESM2","CMCC-CM2-SR5","CESM2-WACCM","CESM2",
                "BCC-CSM2-MR","ACCESS-ESM1-5","ACCESS-CM2")

file.length.base <- ("G:\\Shared drives\\LarssonWork\\NEXGDDP\\Downloads\\")
  
lengthcheck.df <- data.frame(matrix(ncol=2, nrow=35))
colnames(lengthcheck.df) <- c("Model", "Number_of_files")

for(i in 1:length(modelnames)){
  #get path & set working directory
  length.path <- paste0(file.length.base,modelnames[i])
  setwd(length.path)
  
  #get number of files
  filenum <- length(list.files())
  
  #put in dataframe
  lengthcheck.df$Model[i] <- modelnames[i]
  lengthcheck.df$Number_of_files[i] <- filenum
}  

