#Load Data
DataTable <- utils::read.csv("Dados/DataTable.txt", header = TRUE, sep = "\t")
AtlasEN <- read.csv('Dados/Data_atlas.csv')

# Load shapefiles
Montesinho_EN <- shapefile('Dados/PNM-N_WGS84.shp')
Grid_EN <- vect('Dados/Grid_1km.gpkg')

## Import remote sensing data (.tiff format) for the period 2001-2021 
LSTDay <- raster("Dados/LST-Day.tif")
LSTNight <- raster("Dados/LST-Night.tif")
SR_B1 <- raster("Dados/SR-Band1.tif")
EVI <- raster('Dados/EVI.tif')
TSF <- raster("Dados/TSF.tif")

# EN Version
modis.rasters_EN <- stack(EVI, LSTDay, LSTNight, SR_B1, TSF)
names(modis.rasters_EN) <- c("EVI","LSTDay","LSTNight","SR_B1","TSF")
newproj <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=WGS84"
projectRaster(modis.rasters_EN, crs=newproj)
