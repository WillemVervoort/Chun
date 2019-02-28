data souce: 

nino_3.4_wk.txt & nino_4_wk.txt

NINO SST index: 
http://www.bom.gov.au/climate/enso/indices.shtml

NOAA websit: 
http://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.html

useful guide: http://www.image.ucar.edu/GSP/Software/Netcdf/
note: resolution is 5x5 degree

read netCDF file in R: 
library(clim.pact)
retrieve.nc()

information:
[1] "ordinary"
[1] "Attribute time_origin not found"
[1] "The chronology is not straight forward: dt= 31 interval span= 56917 data points= 1871 for R:\\Data\\BoM\\SST\\sst.mon.anom.noaa.nc"
[1] "ncid$dim$'time$units'"
[1] "Time, units:  days since 1800-1-1 00:00:00"
[1] "torg= 1800-1-1 00:00:00"
[1] "Time origin: (year-month-day) 1800 - 1 - 1"
[1] "Time unit: days"
[1] "Latitudes:  -87.5 - 87.5 degrees_north"
[1] "Longitudes:  2.5 - 357.5 degrees_east"
[1] "Reading sst"
[1] "read the data from EASTERN hemisphere"
     start count varsize
[1,]     1    72      72
[2,]     1    36      36
[3,]     1  1871    1871
[1] "dim dat: nx.e= 72 nx.w= 0 ny= 36 nz= 1 nt= 1871 x.rng= 2.5 - 357.5"
[1] 1871   36   72
[1]   72   36 1871
[1] "Monthly data, but time unit set to 'hour'/'day'"
[1] "Set time unit to month"
[1] "Sort longs and lats"
[1] "First & last records: 1856 1 15 & 2011 11 15"
[1] "BEFORE scale adjustment & weeding"
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
     -4.6      -0.4       0.0       0.0       0.3       6.5 2591335.0 
[1] "2591335 of 4849632  are set to 'NA'; missing value= 32766"
[1] "AFTER scale adjustment & weeding"
[1] "Scaling: dat <- dat * 0.00100000004749745"
[1] "Offset: dat <- dat + 0"
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0       0       0       0       0       0 2591335 
[1] "dimensions 1871 36 72"

notification of NOAA: NOAA_OI_SST_V2 data provided by the NOAA/OAR/ESRL PSD, Boulder, Colorado, USA, from their Web site at http://www.esrl.noaa.gov/psd/



nina34.data.txt & nina3.data.txt

http://www.esrl.noaa.gov/psd/data/climateindices/list/


SST34_anomaly: 
http://iridl.ldeo.columbia.edu/SOURCES/.Indices/.nino/.EXTENDED/.NINO34

SST3_anomaly:
http://iridl.ldeo.columbia.edu/SOURCES/.Indices/.nino/.EXTENDED/.NINO3/

SST4_anomaly: 
http://iridl.ldeo.columbia.edu/SOURCES/.Indices/.nino/.EXTENDED/.NINO4