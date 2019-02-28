library(RCurl)
library(rgdal)
library(gdata)
library(raster)
library(maptools)
library(ncdf)
library(fields)
library(nnet)
library(RNetCDF)

#######################################################################
#                 read data
#######################################################################

MRT <- "C:\\MRT\\MRT\\bin\\"

### based on R:\\Data\\Satellite\\MOD44\\B, pixel size 250m
setwd("R:\\Data\\Other\\bushfires\\Burnt_Area")

mod <- dir("R:\\Data\\Other\\bushfires\\Burnt_Area") ## the last two are not .hdf file
datad <- "R:\\Data\\Other\\bushfires\\Burnt_Area\\"

mydirname <- unique(unlist(lapply(mod[grep(mod,pattern="MCD45A1.*.*.*.*.hdf")], function(x) unlist(strsplit(x, "\\."))[2])))

for (i in 1:length(mydirname))  {
  
    dirname_y <- mod[grep(mod,pattern=paste("MCD45A1.",mydirname[i],".*.*.*.hdf",sep=""))]
  
    # mosaic the blocks:
    mosaicname = file(paste(MRT, "TmpMosaic.prm", sep=""), open="wt")

    for (j in 1:length(dirname_y)) {
      if (j==1) write(paste(datad, dirname_y[1], sep=""), mosaicname) else
      { write(paste(datad, dirname_y[j], sep=""), mosaicname, append=T)}
    }
    
    close(mosaicname)


shell(cmd=paste(MRT, 'mrtmosaic -i ', MRT, 'TmpMosaic.prm -s "1 0 0 0 0 0 0 " -o ', datad, 'TmpMosaic.hdf', sep=""))

filename = file(paste(MRT, "mrt", mydirname[i], ".prm", sep=""), open="wt")
    write(paste('INPUT_FILENAME = ', datad, 'TmpMosaic.hdf', sep=""), filename) 
    # write(paste('INPUT_FILENAMES = ( ', workd, BLOCK1, ' ', workd, BLOCK2, ' ', workd, BLOCK3, ' ', workd, BLOCK4, ' ', workd, BLOCK5, ' ', workd, BLOCK6, ' ', workd, BLOCK7, ' ', workd, BLOCK8, ' ', workd, BLOCK9, ' )', sep=""), filename)  # unfortunatelly does not work via command line  :(
    write('  ', filename, append=TRUE) 
    # write('SPECTRAL_SUBSET = ( 0 1 0 0 0 0 0 0 0 0 0 )', filename, append=TRUE)
    write('SPECTRAL_SUBSET = ( 1 )', filename, append=TRUE)
    write('  ', filename, append=TRUE)
    write('SPATIAL_SUBSET_TYPE = OUTPUT_PROJ_COORDS', filename, append=TRUE)
    write('  ', filename, append=TRUE)
#     write('SPATIAL_SUBSET_UL_CORNER = ( -10.00 91.39 )', filename, append=TRUE)
#     write('SPATIAL_SUBSET_LR_CORNER = ( -50.00 179.90 )', filename, append=TRUE)
    write('  ', filename, append=TRUE)
    write(paste('OUTPUT_FILENAME = ', datad, 'tmp', mydirname[i], '.tif', sep=""), filename, append=TRUE)
    write('  ', filename, append=TRUE)
    write('RESAMPLING_TYPE = NEAREST_NEIGHBOR', filename, append=TRUE)
    write('  ', filename, append=TRUE)
    write('OUTPUT_PROJECTION_TYPE = longlat', filename, append=TRUE)
    write('  ', filename, append=TRUE)
    write('OUTPUT_PROJECTION_PARAMETERS = ( ', filename, append=TRUE)
    write(' 0.0 0.0 -18.0', filename, append=TRUE)
    write(' -36.0 132.0 -27.0', filename, append=TRUE)
    write(' 0.0 0.0 0.0', filename, append=TRUE)
    write(' 0.0 0.0 0.0', filename, append=TRUE)
    write(' 0.0 0.0 0.0 )', filename, append=TRUE)
    write('  ', filename, append=TRUE)
    write('DATUM = WGS84', filename, append=TRUE)
    write('  ', filename, append=TRUE)
    write('OUTPUT_PIXEL_SIZE = 0.25', filename, append=TRUE)
    write('  ', filename, append=TRUE)
    close(filename)

    # Mosaic the images to get the whole area:
    shell(cmd=paste(MRT, 'resample -p ', MRT, 'mrt', mydirname, '.prm', sep=""))
    
}
##convert to tiff using arcMap extract subdataset 

# Check the validity:
GDALinfo("tmpA2003001.burndate.tif")

fire_0301 <- raster("tmpA2003001.burndate.tif")

# trim off unneccessary part
aus_reg <- readShapePoly("R:\\Data\\Geo\\aust_state09\\coast_line_proj.shp",proj4string=CRS("+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=-27 +lon_0=132 +x_0=0 +y_0=0"))
fire_200301 <- crop(fire_0301,aus_reg,snap="in",filename="MQ_200301_0_proj.tif")