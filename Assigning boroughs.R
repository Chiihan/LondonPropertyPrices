mydf <- structure(list(longitude = c(128.6979, 153.0046, 104.3261, 124.9019, 
                                     126.7328, 153.2439, 142.8673, 152.689), latitude = c(-7.4197, 
                                                                                          -4.7089, -6.7541, 4.7817, 2.1643, -5.65, 23.3882, -5.571)), .Names = c("longitude", 
                                                                                                                                                                 "latitude"), class = "data.frame", row.names = c(NA, -8L))
library(sp)  # vector data
library(raster)  # raster data
library(rgdal)  # input/output, projections
library(rgeos)  # geometry ops
library(spdep) 
library(rgeos)
install.packages(spdep)


latlong = "+init=epsg:4326"
mastertableSpaital = mastertable[1:9800]
xy <- mastertableSpaital[,.(longitude,latitude)]
xysp = SpatialPoints(xy)
spdf <- SpatialPointsDataFrame(coords = xysp, data = mastertableSpaital
)
proj4string(spdf) = CRS(latlong)

SPtrans = spTransform(spdf, CRS(proj4string(lnd)))



mastertableSpaital[,Borough:='NA']

for (i in 1:length(BoroughList)) {
  l = lnd[lnd$name == BoroughList[i],]
  lo = gContains(l, SPtrans, byid = TRUE)
  mastertableSpaital[grep('TRUE',lo)]$Borough = BoroughList[i]
}


######
library(rgeos)
plot(lnd, col = "grey")

cent_lnd <- gCentroid(lnd[lnd$name == "City of London",]) 
points(cent_lnd, cex = 3)
# set 10 km buffer
lnd_buffer <- gBuffer(spgeom = cent_lnd, width = 10000) 


# method2 of subsetting selects only points within the buffer
lnd_cents <- SpatialPoints(coordinates(lnd),
                           proj4string = CRS(proj4string(lnd))) # create spatialpoints
sel <- lnd_cents[lnd_buffer,] # select points inside buffer
lnd_central <- lnd[sel,] # select zones intersecting w. sel

#turn all the boroughs into points then see if they are within circle
#text(coordinates(cent_lnd), "Central\nLondon")
mastertableSpaital[,central:=0]

for (i in 1:length(mastertableSpaital$latitude)) {
  if ((mastertableSpaital$Borough[i] %in% lnd_central@data$name) == TRUE) {
    mastertableSpaital$central[i] = 1
  }
}