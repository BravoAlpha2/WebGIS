#Load Data
DataTable <- read.csv('Dados/TableData.csv')
Atlas <- read.csv('Dados/Data.csv')
AtlasEN <- read.csv('Dados/Data.csv')

# Load shapefiles
Montesinho <- shapefile('Dados/PNM-N_WGS84.shp')
Montesinho_EN <- shapefile('Dados/PNM-N_WGS84.shp')
Grid <- vect('Dados/Grid_1km.gpkg')
Grid_EN <- vect('Dados/Grid_1km.gpkg')

## Import remote sensing data (.tiff format) for the period 2001-2021 
LSTDay <- raster("Dados/LST-Day.tif")
LSTNight <- raster("Dados/LST-Night.tif")
SR_B1 <- raster("Dados/SR-Band1.tif")
EVI <- raster('Dados/EVI.tif')
TSF <- raster("Dados/TSF.tif")

# PT Version
modis.rasters <- stack(EVI, LSTDay, LSTNight, SR_B1, TSF)
crs(modis.rasters) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
names(modis.rasters) <- c("EVI","LSTDay","LSTNight","SR_B1","TSF")

# EN Version
modis.rasters_EN <- stack(EVI, LSTDay, LSTNight, SR_B1, TSF)
crs(modis.rasters_EN) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
names(modis.rasters_EN) <- c("EVI","LSTDay","LSTNight","SR_B1","TSF")
