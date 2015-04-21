library(rleafmap) # devtools::install_github("fkeck/rleafmap")
library(rgdal)
library(maptools)
library(geojsonio)
library(ggmap)
library(leaflet)

latlong = "+init=epsg:4326"
google = "+init=epsg:3857"



matrix_cnr=read.csv("/home/alf/matrix_cnr_raw.csv")
names(matrix_cnr)

# [1] "id"                  "sigla"               "Dipartimento"        "nome"                "link_generale"       "link_direttore"      "link_descrizione"    "link_ricerca"       
# [9] "link_pubblicazioni"  "link_servizi"        "link_competenze"     "link_collaborazioni" "link_formazione"     "link_biblioteche"    "link_banchedati"     "link_focus"         
# [17] "link_focus_ITA"      "links_logo"          "nome_istituto"       "url"                 "links_webist"        "links_mailist"       "links_pres_ist"      "indirizzo"          
# [25] "web_site"            "direttore"           "mail"                "sigla.1"             "Dipartimento.1"      "nome.1"             

matrix_cnr=matrix_cnr[,1:27]

nrow(matrix_cnr)
#####################
# geolocation

res=lapply(as.list(paste0(matrix_cnr$indirizzo," ,Italy")),geocode)

geoCNR=readRDS("geoCNR.rds")




matrix_cnr$latitude=unlist(sapply(geoCNR,function(x) x$lat))
matrix_cnr$longitude=unlist(sapply(geoCNR,function(x) x$lon))

write.csv(matrix_cnr,"matrix_cnr_data.csv",row.names=F)

saveRDS(matrix_cnr,"matrix_cnr_data.rds")

############################################################################
# Create SP object

coordinates(matrix_cnr) = ~longitude + latitude
proj4string(matrix_cnr) = CRS(latlong)


matrix_cnr_f<-paste(getwd(), "/","geoCNR.geojson", sep="") 
writeOGR(matrix_cnr, matrix_cnr_f, layer="", driver="GeoJSON")
writeOGR(matrix_cnr, "matrix_cnr",matrix_cnr_f, driver='GeoJSON')

###############################################################################################################
# Create map

Stamen <- basemap("stamen.toner")
Mapquest <- basemap("mapquest.map")
Positron <- basemap("cartodb.positron")

gcol <- rev(rainbow(8))
region.col <- gcol[as.numeric(matrix_cnr$Dipartimento)]



Istitutes_CNR <- spLayer(matrix_cnr, fill.col =region.col, popup = matrix_cnr$sigla)

my.ui <- ui(attrib.text = "CNR Istitute Map: <a href='http://www.cnr.it/'>CNR</a>, <a href='https://www.expo.cnr.it/'>CNRxExPO</a>",layers = "topright")

writeMap(Positron,Stamen, Mapquest, Istitutes_CNR ,  interface = my.ui, setView = c(40, 12), setZoom = 12,directView = c( "browser"), leaflet.loc = "online")



#######################################################################################################
# Reference

# http://www.francoiskeck.fr/rleafmap/ 
# http://www.maths.lancs.ac.uk/~rowlings/Teaching/UseR2012/cheatsheet.html


