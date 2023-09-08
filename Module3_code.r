
library(lpSolve)
library(DescTools)
library(irr)


#Load csv
fogo_species = read.csv("D:/Geotraining Day 4/fogo_species.csv")
fogo_plots = read.csv("D:/Geotraining Day 4/fogo_plots.csv")

#Groups that need to deal with species richness data
#Amount of species for one plot
source_IS= fogo_species[fogo_species$SOURCE=="IS",]
source_NS= fogo_species[fogo_species$SOURCE=="NS",]

number_introduce = as.data.frame(table(source_NS$PLOT))
number_introduce2 = as.data.frame(table(source_IS$PLOT))

names(number_introduce)= c("PLOT", "SPECIES")
names(number_introduce2)= c("PLOT", "SPECIES")

#Combining the species data frame with the fogo plots data frame 
number_introduce = merge(number_introduce, fogo_plots)
number_introduce2 = merge(number_introduce2, fogo_plots)

#Selecting the first 4 in the above data frame
number_introduce = number_introduce[,1:4]
number_introduce2 = number_introduce2[,1:4]

#Project data
number_introduce = st_as_sf(number_introduce, coords = c("LONG", "LAT"), crs=4326)
plot(number_introduce[,"SPECIES"],
     main= c("Natural species"))
number_introduce2 = st_as_sf(number_introduce2, coords = c("LONG", "LAT"), crs=4326)
plot(number_introduce2[,"SPECIES"],
     main= ("Introduced species"))

Landcover = rast("D:/Geotraining Day 4/LandCover2.tif")
number_introduce = st_transform(number_introduce, st_crs(Landcover))
Landcover_2 = extract(Landcover, number_introduce)

number_introduce2 = st_transform(number_introduce2, st_crs(Landcover))
Landcover_3 = extract(Landcover, number_introduce2)

#Selecting the radius
number_buffer = st_buffer(number_introduce, dist = 100)
Landcover_buffer = extract(Landcover, number_buffer)

number_buffer_2 = st_buffer(number_introduce2, dist = 100)
Landcover_buffer_2 = extract(Landcover, number_buffer_2)

table_landcover = as.data.frame.array (table(Landcover_buffer)) 
table_landcover = cbind(table_landcover,number_introduce)

table_landcover_2 = as.data.frame.array (table(Landcover_buffer_2)) 
table_landcover_2 = cbind(table_landcover_2,number_introduce2)

#Boxplot
par(mfrow=c(1,2))
box_plot = boxplot(table_landcover$SPECIES,table_landcover$settlement,
                   main="Natural species and settlement",
                   names= c("Natural species", "Settlement"),
                   col=c("blue", "red"))
box_plot = boxplot(table_landcover_2$SPECIES,table_landcover_2$settlement,
                   main="Introduced species and settlement",
                   names= c("Introduced species", "Settlement"),
                   col=c("green","yellow"))

#Plotting the relationship between Species vs Settlement
par(mfrow=c(1,2))
    plot(table_landcover$SPECIES, table_landcover$settlement,col="blue",
         main="Natural species vs Settlement",
         sub="Distance 100",
         xlab="Natural species", ylab="Settlement")
        plot(table_landcover_2$SPECIES, table_landcover_2$settlement,col="red",
         main="Introduced species vs Settlement",
         sub="Distance 100",
         xlab="Introduced species", ylab="Settlement")
