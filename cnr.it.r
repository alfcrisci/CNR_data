library(rvest)
library(stringr)
library(rleafmap)
library(rgdal)
library(maptools)

# html() is for download of content
# html_node() is for selecting node(s) from the downloaded content of a page
# html_text() is for extracting text from a previously selected node







cnr_rete <- html("http://data.cnr.it/data/cnr/individuo/retescientifica")
links <- cnr_rete %>% html() %>% html_nodes(".itemprop , a") %>% xml_attr("href") 
links_rete <- paste0("http://www.cnr.it/istituti/",unique(grep("DatiGenerali.html", links, fixed = TRUE, value=TRUE)))

cnr_site <- html("http://www.cnr.it/istituti/Istituti.html")
links <- cnr_site %>% html() %>% html_nodes(".itemprop , a") %>% xml_attr("href") 
links_ist <- paste0("http://www.cnr.it/istituti/",unique(grep("DatiGenerali.html", links, fixed = TRUE, value=TRUE)))






get_data_ist=function(url) {
  require(rvest)
  require(stringr)
  
  res=list()
  
  cds=strsplit(url, "=")[[1]][2]
  root_ist="http://www.cnr.it/istituti/"
  
  match_web="http://www."
  match_pres="istituti/?"
  match_mail="mailto:"
  
  res$link_direttore=paste0(root_ist,"DatiDirettore_eng.html?cds=",cds)                                                                                                                                                        
  res$link_descrizione=paste0(root_ist,"Descrizione_eng.html?cds=",cds)                                                                                                                                                                     
  res$link_ricerca=paste0(root_ist,"Ricerca_eng.html?cds=",cds)                                                                                                                                                                
  res$link_pubblicazioni=paste0("Pubblicazioni_eng.html?cds=",cds)                                                                                                                                                        
  res$link_servizi=paste0(root_ist,"Servizi_eng.html?cds=",cds) 
  res$link_competenze=paste0(root_ist,"Competenze_eng.html?cds=",cds)                                                                                                                                                           
  res$link_collaborazioni=paste0(root_ist,"Collaborazioni_eng.html?cds=",cds)                                                                                                                                                      
  res$link_formazione=paste0(root_ist,"Formazione_eng.html?cds=",cds)                                                                                                                                                            
  res$link_biblioteche=paste0(root_ist,"Biblioteche_eng.html?cds=",cds)                                                                                                                                                           
  res$link_banchedati=paste0(root_ist,"Banchedati_eng.html?cds=",cds)                                                                                                                                                           
  res$link_focus=paste0(root_ist,"Focus_eng.html?cds=",cds) 
  res$link_focus_ITA=paste0(root_ist,"Focus.html?cds=",cds) 
  
  url_content=html(url,encoding = "UTF-8") 
  
  ################################################################################################
  # Extract logo
  
  links_data <-url_content %>% html() %>% html_nodes(".itemprop , a") %>% xml_attr("href") 
  links_img <-links_img <-url_content %>% html() %>% html_nodes("img") %>% xml_attr("src")
  res$links_logo <- paste0("http://www.cnr.it/istituti/",unique(grep("/logo", links_img, fixed = TRUE, value=TRUE)))
  
  nome=url_content %>% html() %>% html_nodes(xpath='//td[contains(@class, "TitoloContenuto")]') 
  nometext=capture.output(print(nome[[1]]))
  res$nome_ist=str_match(nometext, "<!----> ([^/]*?) <!----> ")[,2]
  
  ################################################################################################
  # Extract links
  
  res$url=url
  res$links_webist <- paste0(unique(grep(match_web, links_data, fixed = TRUE, value=TRUE)))
  res$links_mailist <- paste0(unique(grep(match_mail, links_data, fixed = TRUE, value=TRUE)))
  res$links_pres_ist<- paste0("http://www.cnr.it/istituti",unique(grep(match_pres, links_data, fixed = TRUE, value=TRUE)))
 
  
  
  url_data=url_content %>% html() %>% html_table(fill=TRUE)
  data_ist=url_data[7][[1]]$X1[1:6]
  res$indirizzo=gsub("Indirizzo: ","",url_data[7][[1]]$X1[3])
  res$tel=gsub("Tel.: ","",url_data[7][[1]]$X1[4])
  res$fax=gsub("Fax.: ","",url_data[7][[1]]$X1[5])
  res$web_site=gsub("Sito web dell'Istituto: ","",url_data[7][[1]]$X1[1])
  res$direttore=gsub("Direttore: ","",url_data[7][[1]]$X1[2])
  res$direttore=gsub(" Direttore","",res$direttore)
  
  res$mail=gsub("E-Mail: ","",url_data[7][[1]]$X1[6])
  res$dipartimento=gsub("Dipartimento di prevista afferenza: ","",url_data[8][[1]]$X1[2])
  #res$geoloc=geo_nominatim(gsub("Indirizzo: ","",res$indirizzo)
  return(res)

}

all=lapply(as.list(links_ist),get_data_ist)

   
saveRDS(all,"CNR.rds")

novo_res=list()

for ( i in 1:length(all)) {

novo_res[[i]]=as.data.frame(all[[i]])
}


matrix=do.call(rbind,novo_res)





geo_nominatim <- function(location) {
  
  require(curl)
  require(stringr)
  require(jsonlite)
  
  # Forming url
  
  location <- str_replace_all(location, ' ', '+')
  location <- URLencode(location)
  url_string <- str_c('http://nominatim.openstreetmap.org/search?format=json&addressdetails=1&q=', location,"&limit=1")
  
  # Geocode

  gc <- fromJSON(url_string)
  
  Sys.sleep(2) 
  
   
  #format geocoded data
  
  
  return(as.list(gc))
  
}


