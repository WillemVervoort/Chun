## MRT operations from R
# This file contains two different MRT functions 
# to resample/reproject and to mosaic
# Niranjan, Floris and Willem
# a specific example application is given

## --------------------------------------------------------------------
## reproject images if you want to do this
# custom function to write parameter file for MRT
# The easiest way to get the inputs right for MRT is to check the manual
# and run the GUI on a test example
MRTproj <- function(fname="tmp.prm",hdfName,outname,MRTpath,UL="",LR="",resample_type="NEAREST_NEIGHBOR",proj_type="UTM",
                    bands_subset="",proj_params="0 0 0 0 0 0 0 0 0 0 0 0",datum="WGS84",utm_zone=NA,pixel_size=1000){
  
  filename = file(fname, open="wt")
  write(paste("INPUT_FILENAME = ", getwd(), "/",hdfName, sep=""), filename) 
  # 	browser()
  if (bands_subset != "") {
    write(paste("SPECTRAL_SUBSET = ( ",bands_subset," )",sep=""),filename,append=TRUE)
  }
  if (UL[1] != "" & LR[1] != "") {
    write("SPATIAL_SUBSET_TYPE = INPUT_LAT_LONG", filename, append=TRUE)
    write(paste("SPATIAL_SUBSET_UL_CORNER = ( ", as.character(UL[1])," ",as.character(UL[2])," )",sep=""), filename, append=TRUE)
    write(paste("SPATIAL_SUBSET_LR_CORNER = ( ", as.character(LR[1])," ",as.character(LR[2])," )",sep=""), filename, append=TRUE)
  }
  write(paste("OUTPUT_FILENAME = ", outname, sep=""), filename, append=TRUE)
  write(paste("RESAMPLING_TYPE = ",resample_type,sep=""), filename, append=TRUE)
  write(paste("OUTPUT_PROJECTION_TYPE = ",proj_type,sep=""), filename, append=TRUE)
  write(paste("OUTPUT_PROJECTION_PARAMETERS = ( ",proj_params," )",sep=""), filename, append=TRUE, ncolumns=5)
  write(paste("DATUM = ",datum,sep=""), filename, append=TRUE)
  if (proj_type == "UTM") write(paste("UTM_ZONE = ",utm_zone,sep=""), filename, append=TRUE)
  write(paste("OUTPUT_PIXEL_SIZE = ",as.character(pixel_size),sep=""), filename, append=TRUE)
  close(filename)
  e <- system(paste(MRTpath, "/resample -p ",getwd(),"/",fname, sep=""))
  e
}

## MOSAIC images using MRT function
# this will write a tmpMosaic
MOSAIC_MRT <- function(mosaicname="TmpMosaic.prm",hdfName1,
                       hdfName2,MRTpath, WDIR_in=WDIR,
                       OUTmosaic='TmpMosaic.hdf') {
  # mosaicname is the name of the parameterfile for MRT
  # hdfName1 is the name of the first hdf file
  # hdfName2 is the name of the second hdf file
  # MRTpath is the path to the "bin" dir of the MRT program
  # WDIR is the directory where the hdf files live
  # OUTmosaic contains the mosaic (merged files)
  # you might not want to keep this, so here a temporary name is used as default
  
  # write the parameter file
  mosaicfile <- file(paste(MRTpath,"/",mosaicname, sep=""), open="wt")
  write(paste(WDIR,hdfName1, sep="/"), mosaicfile)
  write(paste(WDIR,hdfName2, sep="/"), mosaicfile, append=T)
  close(mosaicfile)
  # execute MRT
  shell(cmd=paste(MRTpath, '/mrtmosaic -i ', MRTpath,"/",mosaicname,
                  ' -s "1 0 0 0 0" -o ',OUTmosaic, sep=""))
}

# the working directory with the images
WDIR <- "C:\\Users\\rver4657\\owncloud\\Chun2\\Data\\Satellite\\MOD44"

# testing this with Chun's data
setwd(WDIR)
# read in the hdf file names
x <- dir(pattern="hdf")
# Identify the different states in the images
# NSW
NSW <- x[c(grep("h29v12",x),grep("h30v12",x))]
# Qld
Qld <- x[c(grep("h30v11",x),grep("h31v11",x))]

# this is needed so R can find the MRT data dir, check the data dir on your system
Sys.setenv(MRT_DATA_DIR="c:/MODIS/data")

# run through both states (extra loop, only because I have data for two site)
for (state in c("NSW","Qld")) {
 if(state =="NSW") WDIR2 <- NSW else WDIR2 <- Qld

  WOUT2 <- paste(WDIR,"B",sep="\\")
  # two images for each location
  n <- length(WDIR2)/2
  
  # run loop over files
  for(i in 1:n){
    #i=1
    # setting the right coordinates for two different sets
    # you might not need this
    if (grepl("v12",WDIR2[i])==T) {
      UL_in <- c(-35.075,147.075)
      LR_in <- c(-36.675,148.925)
    } else {
      UL_in <- c(-25.575,145.825)
      LR_in <- c(-28.075,148.625)
    }
    
    # run the mosaic function
    MOSAIC_MRT(mosaicname="TmpMosaic.prm",WDIR2[i],
               WDIR2[i+n],MRTpath="c:/MODIS/bin",
               WDIR_in=WDIR, OUTmosaic='TmpMosaic.hdf')
    # Now run the MRTproj function
    # specification of the path to MRT is important
    MRTproj(fname="tmp.prm",'TmpMosaic.hdf',
            paste(WOUT2,paste(substr(WDIR2[i],1,16), state,
                              substr(WDIR2[i],23,40), 
                              ".tif", sep = ""), sep = "\\"),
            "C:/MODIS/bin",
            UL=UL_in, LR=LR_in, resample_type="NEAREST_NEIGHBOR", 
            proj_type="GEO",
            #utm_zone = 55,
            # choose which bands to extract
            bands_subset="1 0 0 0 0 0 0",
            #, proj_params=, 
            datum="WGS84", pixel_size=0.0025) # approximately 250 m 
  }
}





