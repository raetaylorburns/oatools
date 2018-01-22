##########################################################
#CREATE VARIABILITY LAYER#
##########################################################

install.packages("sdmpredictors")
# install package developed by Bio-ORACLE creators to manipulate layers

# Load package 
library(sdmpredictors)

######### Package and Layers exploration

#?sdmpredictors

# Explore datasets in the package
list_datasets() 

# Explore layers in a dataset 
list_layers()

# Explore names of layers in dataset
list_layers("Bio-ORACLE")

# BO_chlomean
# BO_dissox
# BO_sstmean
# BO_salinity
# BO2_chlomean_bdmin

######### Layer manipulation

### SEA SURFACE TEMPERATURE

SST <- load_layers("BO_sstmean", equalarea = TRUE)
# load mean SST layer w/o projection

crs(SST) <- CRS('+init=EPSG:6414') # assign coordinate reference system to CA Albers NAD 83 EPSG 6414

# Crop raster to fit the Eastern Pacific
NEpacific <- extent(-12500000, -11250000, 3500000, 6000000) # names the extent - this is just an estimate ***need to coordinate extent with Rae based on Gap analysis feedback!

SSTcrop <- crop(SST, NEpacific) # crops SST layer according to NEpacific extent

# Generate a nice color ramp and plot the map 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(SSTcrop,col=my.colors(1000),axes=FALSE, box=FALSE) 
title(cex.sub = 1.25, sub = "SST (ºC)")


### SST variability

sstvar=terrain(SSTcrop, opt='slope')
#get slope of SST

# Generate a nice color ramp and plot the map 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(sstvar,col=my.colors(1000),axes=FALSE, box=FALSE) 
title(cex.sub = 1.25, sub = "SST variability(ºC)")

################################################################

## DISSOLVED OXYGEN

DO <- load_layers("BO_dissox", equalarea = TRUE) 
# Load Dissolved Oxygen Data W/O projection

crs(DO) <- CRS('+init=EPSG:6414') # assign coordinate reference system to CA Albers NAD 83 EPSG 6414

# Crop raster to fit the Eastern Pacific
NEpacific <- extent(-12500000, -11250000, 3500000, 6000000) # names the extent - this is just an estimate ***need to coordinate extent with Rae based on Gap analysis feedback!

DOcrop <- crop(DO, NEpacific) # crops SST layer according to NEpacific extent

# Generate a nice color ramp and plot the map 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(DOcrop,col=my.colors(1000),axes=FALSE, box=FALSE) 
title(cex.sub = 1.25, sub = "DO (ºC)")

### DO variability

dovar=terrain(DOcrop, opt='slope')
#get slope of DO

# Generate a nice color ramp and plot the map 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(dovar,col=my.colors(1000),axes=FALSE, box=FALSE) 
title(cex.sub = 1.25, sub = "SST variability(ºC)")

###############################################################

#combine sst variability and do variability

donorm = dovar/maxValue(dovar)

sstnorm = sstvar/maxValue(sstvar)

variability = donorm+sstnorm

# Generate a nice color ramp and plot the map 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(variability,col=my.colors(1000),axes=FALSE, box=FALSE) 
title(cex.sub = 1.25, sub = "normalized variability")

###############################################################
#GAP ANALYSIS#
###############################################################

#import inventory

#isolate coordinate columns
coords<-cbind.data.frame(oahfocus$Longitude, oahfocus$Latitude)

#remove duplicate locations
deduped.coords<-unique(coords)

#voronoi polygons
install.packages("SDraw")
library(SDraw)

#create spatial points objects
voronoicoords<-SpatialPoints(deduped.coords, CRS('+init=EPSG:6414'))

#create voronoi polygons
voronoi <- voronoi.polygons(voronoicoords)

#attempt to crop voronoi polygons to the SST layer so that the polygons do not extend onto land. this did not work because result is empty... not sure why...
voronoicrop <- crop(voronoi, SSTcrop)

#plot voronoi shapes! success.
plot(voronoi) 

#attempt to assign values to voronoi polygons based on relative size. this did not work. 

install.packages("deldir")
library(deldir)

#get the relative sizes of each polygon.
polygons <- deldir(oahfocus$Longitude, oahfocus$Latitude)
size<-polygons$summary$dir.area

#attempt to plot voronoi polygons and color by size. this does not work... the colors do not correspond to the size of the polygons but seem to be randomly assigned...
vorfinal<- cbind(voronoi, size)
plot(vorfinal, col=size)

# i think the colors for the polygons here are randomly assigned...?
plot(voronoi, col=my.colors(1000),axes=FALSE, box=FALSE)





#PLEASE IGNORE THE FOLLOWING CODE.... LOTS OF FAILED ATTEMPTS AND APPROACHES. 
#############################################################################
#vor_pts<-SpatialPointsDataFrame(deduped.coords, deduped.coords, match.ID = TRUE)

#SPointsDF_to_voronoi_SPolysDF <-function(sp) {
#  vor_desc <- tile.list(deldir(sp@coords[,1],sp@coords[,2]))
#  lapply(1:(length(vor_desc)), function(i) {
#    tmp<-cbind(vor_desc[[i]]$x, vor_desc[[i]]$y)
#    tmp <-rbind(tmp, tmp[1,])
#    Polygons(list(Polygon(tmp)), ID=i)
#  })->vor_polygons
#  sp_dat <- sp@data
#  rownames(sp_dat) <-
#sapply(slot(SpatialPolygons(vor_polygons), 'polygons'), slot, 'ID')
  
#SpatialPolygonsDataFrame(SpatialPolygons(vor_polygons), data=sp_dat)
#}

#install.packages("maps")
#library(maps)

#vor<-SPointsDF_to_voronoi_SPolysDF(vor_pts)

#vor_df<-fortify(vor)

#states <-map_data("state")

#ggplot()+
#  geom_map(data=states, map=states, aes(x=long, y=lat, map_id=region))+
#  geom_map(data=vor_df, map=vor_df, aes(map_id=size))

#tiles<-tile.list(polygons)

#ggplot()+
#  geom_map(data=polygons, map=coords, aes(x=long, y=lat, map_id=size, fill = size))

#install.packages("raster")
#library(raster)

#install.packages("dismo")
#library(dismo)

#library(latticeExtra)

#library(ggplot2)

#extent <-as.matrix(SSTcrop)

#nepacificspatial<-SpatialPoints(NEpacific, CRS('+init=EPSG:6414'))

#average variability of each polygon

#relative size of each polygon

#combine size and variability
