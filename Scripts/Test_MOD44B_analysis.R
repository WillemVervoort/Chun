# create raster brick for both NSW and Qld sites
require(raster)
#library(plotKML)
library(rgeos) #gSimplify
library(maptools) # readShapePoly
library(fields) # image.plot


setwd("C:/users/rver4657/owncloud/chun2/data/satellite/MOD44/B")

# read in the file list from the directory
x <- dir(pattern="Percent_Tree_Cover.tif")
# Identify the different states in the images
# NSW
NSW <- x[grep("NSW",x)]
# Qld
Qld <- x[grep("Qld",x)]

img <- list()
# raster and brick
for(i in 1:length(NSW)) {
  img[[i]] <- raster(NSW[i])
}

imgQ <- list()
# raster and brick
for(i in 1:length(Qld)) {
  imgQ[[i]] <- raster(Qld[i])
}

NSW_b <- do.call(brick,img)
Qld_b <- do.call(brick, imgQ)

tm <- seq(as.Date("2000-03-06"), as.Date("2016-03-06"), "year")
mod_tc_r22 <- setZ(NSW_b, tm, "year")

mod_tc_r11 <- setZ(Qld_b, tm, "year")

## apply t-test to tree cover data
fun <- function(x,m) {
  var_x <- sd(x,na.rm=T)
  mu_1 <- mean(x[1:3],na.rm=T)
  if (m==1) {y <- x[4:17]} else {y <- x[6:17]}   
  mu_2 <- mean(y,na.rm=T)
  z <- (mu_2-mu_1)/var_x
  if (!is.na(z) & abs(z)>1.645) {return(mu_2-mu_1)} else {return(NA)}
}

mod_tc_r1.z <- calc(mod_tc_r11,function(x) fun(x,1))
mod_tc_r2.z <- calc(mod_tc_r22,function(x) fun(x,2))

root_dir <- "c:/users/rver4657/owncloud/chun/chun/data"
aus_reg <- readShapePoly(paste(root_dir,
                               "Geo\\aus_state2011\\STE11aAust.shp",sep="/"),
                         proj4string=CRS("+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=-27 +lon_0=132 +x_0=0 +y_0=0"))
aus_reg2 <- gSimplify(aus_reg,0.01,topologyPreserve=T)

mdb_reg <- readShapePoly(paste(root_dir,
                               "Geo\\MDB\\mdb_boundary.shp", sep="/"),
                               proj4string=CRS("+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=-27 +lon_0=132 +x_0=0 +y_0=0"))
mdb_reg2 <- gSimplify(mdb_reg,0.01,topologyPreserve=TRUE)

remove(list=c("aus_reg","mdb_reg"))

e111 <- extent(c(145.825, 148.625, -28.075, -25.575)) 
e222 <- extent(c(147.075, 148.925, -36.675, -35.075)) 

require(RColorBrewer)
plotclr <- brewer.pal(8,"PiYG")

par(mar=c(3,3,1,7))
plot(e222)
image(mod_tc_r2.z,col=plotclr,breaks=c(-70,-20,-10,-5,0,5,10,20,70),legend=F,add=T)
#plot(e111,add=T)
plot(aus_reg2,border="grey",lwd=3,add=T)
image.plot(zlim=c(-4,4),breaks=c(-4,-3,-2,-1,-0.2,0.2,1,2,3,4),
           lab.breaks=c(-70,-20,-10,-5,"no tree","0 or",5,10,20,70),
           legend.only=T,horizontal=F,
           col=c("#C51B7D","#DE77AE","#F1B6DA","#FDE0EF","white",
                 "#E6F5D0","#B8E186","#7FBC41","#4D9221"),legend.shrink=0.7,
           legend.width=0.7,
           legend.args=list(text=expression(paste(Delta,"tree (% ground)",sep="")),
                            line=0.5,cex=0.9,font=2),
           axis.args=list(cex.axis=0.8),graphics.reset=F)

par(mar=c(3,3,1,7))
plot(e111)
image(mod_tc_r1.z,col=plotclr,breaks=c(-70,-20,-10,-5,0,5,10,20,70),legend=F,add=T)
#plot(e111,add=T)
plot(mdb_reg2,border="grey",lwd=3,add=T)
image.plot(zlim=c(-4,4),breaks=c(-4,-3,-2,-1,-0.2,0.2,1,2,3,4),
           lab.breaks=c(-70,-20,-10,-5,"no tree","0 or",5,10,20,70),
           legend.only=T,horizontal=F,
           col=c("#C51B7D","#DE77AE","#F1B6DA","#FDE0EF","white",
                 "#E6F5D0","#B8E186","#7FBC41","#4D9221"),legend.shrink=0.7,
           legend.width=0.7,
           legend.args=list(text=expression(paste(Delta,"tree (% ground)",sep="")),
                            line=0.5,cex=0.9,font=2),
           axis.args=list(cex.axis=0.8),graphics.reset=F)

# writeRaster