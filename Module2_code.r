library(terra)
library(sf)
library(lpSolve)
library(DescTools)
library(irr)

Load the cropped sentinel data 
sentinel1 = rast("D:/GeoTraining Day 3/sentinel2_NDVI.tif")
sentinel2_nov= rast("D:/GeoTraining Day 3/sentinel2_20221127_ndvi.tif")
sentinel= c(sentinel1,sentinel2_nov)

#Load the polygons: can also be a .shp file
#Ask if layer over layer
polygons2= st_read("D:/GeoTraining Day 3/example2.gpkg")
sentinel
polygons2
