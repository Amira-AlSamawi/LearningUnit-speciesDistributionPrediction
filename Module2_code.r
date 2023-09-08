library(terra)
library(sf)
library(randomForest)
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

#Random forest algorithm
#y= classes and x= predictors from satellite 
rfmodel= randomForest(x= df[, c("B02_nov", "B03_nov","B04_nov", "B08_nov", "NDVI_nov", "B02", "B03","B04", "B08", "NDVI")],
                      y= as.factor(df[, c("class")]),ntree=100)

#Apply the model to the sentinel image
lcc= predict
lcc= predict(sentinel, rfmodel)
plot(lcc

plot(lcc, col= c("brown","green2", "black","blue","brown2", "white","green" ))

writeRaster(lcc, "D:/GeoTraining Day 3/LandCoverLayer.tif")

#Transform Geodetic CRS - Sentinel
polygons= st_transform(polygons, crs = st_crs(sentinel))
sentinel= c(sentinel1,sentinel2_nov)

#Extract
df= extract(sentinel, polygons)
View(df)
polygons$ID= seq(1, nrow(polygons))

df= merge(df, polygons)
View(df)

#Split all data into training and test
test_sample= sample(nrow(df), nrow(df)/4)
test_set= df[test_sample,]
training_set= df[test_sample,]
View(training_set)

#Random Forest Algorithm
#y= classes and x= predictors from satellite 
rfmodel= randomForest(x= training_set[, c("B02_nov", "B03_nov","B04_nov", "B08_nov", "NDVI_nov")],
                      y= as.factor(training_set[, c("class")]),ntree=100)

#Apply the model to the sentinel image
lcc= predict(sentinel, rfmodel)

#Predict the classes of the independent test set
test_prediction= predict(rfmodel, test_set)
print(test_prediction)

#CohenKappa
tabletest= table(test_set$class, test_prediction)
CohenKappa(tabletest)

#FleissKappa
kappam.fleiss(tabletest)

#Confusion matrix
cfm= table(test_set$class, test_prediction)
View(cfm)

#Accuracy: sum of diagonals divided by all
sum(diag(cfm))/sum(cfm)
