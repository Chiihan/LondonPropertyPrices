library(httr)
library(XML)
library(htmltools)
library(rgdal)
firstpartAPI = 'api.zoopla.co.uk/api/v1/property_listings.xml?'
#postcode=London&output_type=county&area_type=towns
library(rvest)
pages = round(67559/100)-1 
#result pages for london will be 675.59
APIResults = list()
filename = 'APIpg1.xml'
  key = 'keyhere'
  
  for (i in 100:pages) {
    while (file.info(filename)$size > 1000) {
  pagenum = paste('page_number=', i, sep = '')
 x =  paste(firstpartAPI, 'county=London&page_size=100&summarised=yes&include_sold=1&listing_status=sale&order_by=age', pagenum, key, sep = "&")
 filename = paste('APIpg', i, '.xml', sep = '')
  y = content(GET(x))
  saveXML(y, file = filename)
  print(i)
  }}
  ChildrenSet = as.character('displayable_address', 'first_published_date', 
               'last_published_date', "latitude", 'longitude',
               'property_type', 'outcode', 'num_recepts', 
               'num_floors', 'num_bedrooms', 'num_bathrooms',
               'listing_id', 'listing_id', 'listing_status',
               'description')
  xmlInternalTreeParse(y)
  getNodeSet(y)
  get
  
  for (i in 1:100) {
    
 mastertable = data.table(address = 'na', first_published_date = 'na', 
                          last_published_date = 'na', latitude = 0, longitude = 0,
                          property_type = 'na', outcode = 'outcode', num_recepts = 0, 
                          num_floors = 0, num_bedrooms = 0, num_bathrooms = 0,
                          listing_id = 0, listing_status = 'na',
                          description = 'na', myRef = 1:65000, price = 0)
 options(digits = 10)    
 filename
filename = 'APIpg1.xml'
i = 30
 while (file.info(filename)$size > 1000) {
   
   filename = paste('APIpg', i, '.xml', sep = '')
  doc = xmlInternalTreeParse(filename)
  for (d in 1:100) {
    p = xmlChildren(getNodeSet(doc, path = '//listing')[[d]])  
tCount = (i*100) + d -100
    mastertable$address[tCount] = html_text(p$displayable_address)
    mastertable$first_published_date[tCount] = html_text(p$first_published_date)
    mastertable$last_published_date[tCount] = html_text(p$last_published_date)
    mastertable$outcode[tCount] = html_text(p$outcode)
    mastertable$latitude[tCount] = as.numeric(html_text(p$latitude))
    mastertable$longitude[tCount] = as.numeric(html_text(p$longitude))
    mastertable$lastPubDate[tCount] = html_text(p$last_published_date)
    mastertable$property_type[tCount] = html_text(p$property_type)
    mastertable$num_recepts[tCount] = as.integer(html_text(p$num_recepts))
    mastertable$num_floors[tCount] = as.integer(html_text(p$num_floors))
    mastertable$num_bedrooms[tCount] = as.integer(html_text(p$num_bedrooms)) 
    mastertable$listing_id[tCount] = as.integer(html_text(p$listing_id))
    mastertable$listing_status[tCount] = html_text(p$listing_status)
    mastertable$description[tCount] = html_text(p$description)
    mastertable$num_bathrooms[tCount] = as.integer(html_text(p$num_bathrooms))
    mastertable$price = as.integer(html_text(p$price)
    print(tCount)
  }
i = i + 1
print(i)
 }
options(digits = 10)
save.image(file = 'propertytable.RData')
length(unique(mastertable$listing_id))

  paste('html_text(xmlChildren(p[[', i, ']])$', d, ')')
  html_text(xmlChildren(p[[i]])$agent_address)
str(p[[1]][1:3])
xmlChildren(p[[1]])[5]

while (file.info(filename)$size > 1000) {
filename = paste('APIpg', i, '.xml', sep = '')
doc = xmlInternalTreeParse(filename)
for (d in 1:100) {
  p = xmlChildren(getNodeSet(doc, path = '//listing')[[d]])  
  tCount = (i*100) + d -100

  mastertable$latitude[tCount] = as.numeric(html_text(p$latitude))
  mastertable$longitude[tCount] = as.numeric(html_text(p$longitude))
  print(tCount)
}
i = i + 1
}
readOGR()
lnd <- readOGR(dsn = "data", layer = "london_sport")

for (i in 100:pages) {
  
    pagenum = paste('page_number=', i, sep = '')
    x =  paste(firstpartAPI, 'county=London&page_size=100&summarised=yes&include_sold=1&listing_status=sale&order_by=age', pagenum, key, sep = "&")
    filename = paste('APIpg', i, '.xml', sep = '')
    y = content(GET(x))
    saveXML(y, file = filename)
    print(i)
  }

stations <- readOGR(dsn = 'data', layer = 'lnd-stns')
stations227700 <- spTransform(stations,CRSobj = CRS(proj4string(lnd)))
plot(stations227700[lnd,])
length(stations227700[lnd,]@data$NAME)
length(stations227700@data$NAME)



mastertableSpaital[,Dist2Stn:=0]
mastertableSpaital[,ClostStn:=0]
lndstations = stations227700[lnd,]


# determins nearest station and then assigns distance to it as a variable
for (u in 1:length(SPtrans@data$address)) {
   tempPrpty= SPtrans[u,]
   tempDist = 200000
   ClosestDist = 200000
for (i in 1:length(lndstations@data$NAME)) {
   tempStn = lndstations[i,]
  tempDist = gDistance(tempPrpty,tempStn)
  if (tempDist < ClosestDist) {
    ClosestDist = tempDist 
    ClosestStn = i
  }
}
   mastertableSpaital$Dist2Stn[u] = ClosestDist
   mastertableSpaital$ClostStn[u] = ClosestStn  
   print(u)
}
