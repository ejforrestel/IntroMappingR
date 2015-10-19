#Mapping Basics in R, EJ Forrestel
#Wolkovich Lab Meeting, 19 October 2015
___________________________________________________________________

#loading R packages
library(sp);library(spdep);library(maptools);library(maps);library(mapdata);library(mapproj);
library(raster);library(rgeos);library(rgdal);library(scales);library(plotrix); library(RColorBrewer);library(classInt);library(epitools);library(PBSmapping);library(RgoogleMaps)

setwd("~/Documents/IntroMappingR") #setting your working directory

rm(list = ls()) #purging memory

___________________________________________________________________
#1. Plotting map and spatial polygon data in R
___________________________________________________________________

#plotting a world map in R
	wrld <- map('world',col='gray90',fill=TRUE)
#looking at the structure of the data 
	str(wrld)
#let's plot the US alone, but looks strange!	 
	map('worldHires','usa',col='gray',border='black',fill=TRUE) 
#change extent of map
	map('worldHires','usa',col="gray90",xlim=c(-80,-70),ylim=c(40,50),fill=TRUE)
	
#plotting new map with state boundaries 		
	map('state',col="gray90",xlim=c(-80,-70),ylim=c(40,50),fill=TRUE)
#adding in border to map plot 
	box() 
#adding in a scale bar, note that this is an unprojected map! Therefore the position of the scalebar matters - the scale will change depending on where the scale bar is placed
	map.scale(x=-79.5,y=40.6,relwidth=.2,cex=.8,ratio=FALSE) 
#adding in Canada, but what if you want to add in provinces? Use the online GADM resource to get the administrative units of countries in R -> level 1 = provinces or states
	map('worldHires','canada',col='gray90',fill=TRUE, add=T) 
	canada <- getData("GADM",country="CAN",level=1)
	class(canada)
	plot(canada,col='gray80',fill=TRUE, add=TRUE)
	
___________________________________________________________________
#2. Overlaying points (GPS coordinates) onto your maps, keep map plotted from previous step
___________________________________________________________________

#reading in GPS coordinates from Wolkovich field sites, could also read in from a datafile
	allsites <- data.frame(lat=c(42.5, 44.0, 44.9, 45.9), long=c(-72.2, -71.4, -71.1, -74), sites=c("Harvard Forest", "White Mountains", "Second College Grant", "Saint Hippolyte")) 
	head(allsites)
#plotting the points onto your map of NE USA and Canada
	points(allsites$long,allsites$lat,add=TRUE,pch=19,col='red',cex=1)
#adding in site labels onto map, can relocate on map so they don't fall right on top of your GPS points
	text(allsites$long-1.8,allsites$lat+.4,labels=allsites$sites,add=TRUE,col='black',cex=1,font=2)

#generating new points of different sizes - perhaps relative to diversity in each area or another metric
	points(allsites$long,allsites$lat,add=TRUE,pch=19,col=c('red','orange','black','purple'),cex=c(3,2,1.5,1.2))

___________________________________________________________________
#3. Overlaying distributional data for species onto your maps
___________________________________________________________________

#Reading in polygons of distributional data for North American tree species and plotting
	ACERUB <- readShapePoly("acerrubr/acerrubr.shp") #Acer rubrum, red maple
	CASDEN <- readShapePoly("castdent/castdent.shp") #Castanea dentatum, American chesnut
#plotting new map with larger extent	
	map('world',xlim=c(-130,-60),ylim=c(25,60))
	box()
#plotting species distributions - for ShapePolygon files, use 'plot' function to map
	plot(ACERUB,fill=TRUE,col='red',add=TRUE)
	plot(CASDEN,fill=TRUE,col='cyan',,add=TRUE)
#remapping and using 'scales' library to make distribution maps transparent	so that state boundaries can be seen
	map('world',xlim=c(-130,-60),ylim=c(25,60))
	map('state',xlim=c(-130,-60),ylim=c(25,60),add=TRUE)				
	box()
#aplha is the function to generate tranparency, try changing values to see how it looks
	plot(ACERUB,fill=TRUE,col=alpha('red',.5),add=TRUE)
	plot(CASDEN,fill=TRUE,col=alpha('cyan',.5),,add=TRUE)
	
#Now work on writing code to add distributional data to your earlier plot!

___________________________________________________________________	
# 4. Transforming map into a polygon with a new projection and plotting using the rgdal library
___________________________________________________________________	

#Thus far we have only plotted maps without any projections, in many cases you would want to plot a projected map for more acccurate representation (i.e., equal-area map or WGS84)	
	no.proj <- map(database='world',fill=TRUE,plot=FALSE)
	class(no.proj)
#extracting IDs for each polygon region in new map	
	newmap.names <- no.proj$names
	newmap.IDs <- sapply(strsplit(newmap.names,":"),function(x) x[1])
#need to designate the 'IDs' for the polygons to convert to a shape file in R
	behr.proj <- map2SpatialPolygons(no.proj,ID=newmap.IDs,proj4string=CRS("+proj=cea +lat_ts=30"))
	class(behr.proj)
#compare no projection to an equal area projection
	layout(matrix(c(1,2),2,2,byrow=FALSE)) #plot layout for comparison
#note again that you use the map function for an object of 'map' class, versus plot for class 'SpatialPolygon'
	map(no.proj,xlim=c(-120,-70),ylim=c(20,50),col="grey80",fill=TRUE)
	box()
	plot(behr.proj,xlim=c(-120,-70),ylim=c(20,50),col="grey80",fill=TRUE)
	box()
#you can also designate the projection when reading in shape files
	ACERUB.behr <- readShapePoly("acerrubr/acerrubr.shp",proj4string=CRS("+proj=cea +lat_ts=30"))
	
___________________________________________________________________
# 5. Introduction to the raster library; mapping gridded cells with climate data as an example, extracting values from grid cells of raster layers
___________________________________________________________________

#reading in global mean annual temperature data as a raster layer
	mean.temp <- raster("bio1.bil")
#WorldClim temperature data is degrees celcius multiplied by ten
	mean.temp <- mean.temp/10
#mapping raster layer
	plot(mean.temp)
#extracting mean annual temperature from a raster layer for polygons, in this case the distribution of red maple
	ACERUB <- readShapePoly("acerrubr/acerrubr.shp")
	temp.acerub <- extract(mean.temp,ACERUB)
#generating a single vector of temperatures across Acer rubrum's distribution
	temp.acerub <- unlist(temp.acerub)
#plotting the distribtion of temperatures from grid cells that Acer rubrum's distribution falls into
	hist(temp.acerub,xlab="Degrees Celcius",main="Acer rubrum Mean Annual Temperature")
#extracting climate data for point data, in this case Wolkovich field sites
	site.temps <- extract(mean.temp,allsites[,c('long','lat')])

#Try to go back and plot points for each field site with the size of the dots scaled to temperature!
___________________________________________________________________
# 6. Generating a PolygonShapeDataFrame and adding your own data - plotting winegrape variety richness data in California
___________________________________________________________________

#reading in data on California wine regions
	CA.wine <- read.csv("CA_wine.csv")
	head(CA.wine)
	
#generating a color scheme for mapping varity richness data in CA wine regions
	my.int <-classIntervals(CA.wine$richness,n=15,style="equal") #binning data into groups, n = number of groups
	my.pal <-c("cyan1","green2","yellow","red") #choosing colors
	my.col<-findColours(my.int,my.pal) #ramping colors based on choosen palette
#reading in county level data for the US
	region <- getData("GADM", country='USA', level=2)
#converting polygon into a SpatialPolygonsDataFrame object with the WGS84 projection
	region <- spTransform(region,CRS("+proj=longlat +datum=WGS84"))
	head(region@data)
#subsetting country-level polygons to only those counties in the CA wine dataset
	sub.region <- region[which(region@data$NAME_2%in%CA.wine$level_2&region@data$NAME_1=="California"),]
#matching the color data to the county level data & attaching to dataframe of shape file
	sub.region$color <- my.col[match(CA.wine$level_2,sub.region@data$NAME_2)]
#can attach any data to the sub.region@data dataframe for plotting and generating color ramps!

#setting up plot for California which will be written to a pdf file saved to your current directory
	pdf('CA_rich.pdf')
	map(database='state',interior=TRUE,col='dark gray',xlim=c(-130,-112),ylim=c(30,45))
	map('worldHires','mexico',interior=TRUE,col='dark gray',xlim=c(-130,-112),ylim=c(30,45),add=TRUE)
	box(which="plot",lwd=.8)
	plot(sub.region,col=sub.region$color,border=sub.region$color,add=T)
	legend(-129,38,legend=names(attr(my.col,"table")),fill=attr(my.col,"palette"),cex=.8,bty="n",border='dark gray')
#turning off plotting device
	dev.off()		

___________________________________________________________________	
# 7. Adding a terrain map using RgoogleMaps
___________________________________________________________________

##define latitudinal and longitudinal range
	long <- c(-75,-71)
	lat <- c(42,46)
##defining center of plot
	center <- c(mean(lat),mean(long))
##generating points to plot (field sites in this case) in a designated format
	markers <- allsites[,c('lat','long')]
	names(markers) <- c('lat','lon')
	markers$size <- "small"
	markers$col <- "red"
#writing a plot to my current folder
	terrmap <- GetMap.bbox(lonR=range(long),latR=range(lat),center=center,zoom=5,markers=markers,maptype='terrain',dest='NE_terrain_map.png')
___________________________________________________________________	


