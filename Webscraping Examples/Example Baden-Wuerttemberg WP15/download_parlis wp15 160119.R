#------------------------------------
## Download der Dokumente auf PARLIS (Stand: 16.01.2019)
#------------------------------------

library(rvest)
library(RSelenium)
library(stringr)

## Die Seite ist leider arg verskriptet,
## wir müssen deshalb mit Selenium arbeiten

# In Terminal laufen lassen 
# (und natürlich vorher Docker installieren)
# docker run -d -p 4445:4444 selenium/standalone-chrome
# docker ps

## Define URL
url <- "https://parlis.landtag-bw.de/parlis/"

## Establish connection
driver <- remoteDriver(
	remoteServerAddr = "localhost",
	port = 4445L,
	browserName = "chrome"
)

driver$open(silent = T)


## Establish connection alternatively 
remDr<- 
  remoteDriver(remoteServerAddr = "192.168.99.100" 
               , port = 4445L 
               , browserName = "chrome"
  )

remDr$open()

## Navigate to page
remDr$navigate("https://www.google.com")
remDr$navigate(url)
Sys.sleep(1)

## Von Vorgängen auf Gesetze umschalten
button <- remDr$findElement(using = "id", value = "a-menu-suchegesetz")
button$clickElement()
Sys.sleep(1)

## Statt Wahlperiode 16 die Wahlperiode 15 auswählen
button1 <- remDr$findElement(using = "id", value = "wahlperiode-gesetz")
button1$clickElement()
Sys.sleep(1)

option <- remDr$findElement(using = 'xpath', "//select[@id='wahlperiode-gesetz']/option[@value='15']")
option$clickElement()
Sys.sleep(1)

## "Suche starten"
button2 <- remDr$findElement(using = "id", value = "SearchDisplayTriggerGesetz")
button2$clickElement()
Sys.sleep(1)

## Generate index
index <- 1

	while(T){

	## Details des Gesetzes öffnen
	buttons <- remDr$findElements(using = "class", value = "efxZoomTabVorgang")

	for(i in 1:length(buttons)){
	  remDr$mouseMoveToLocation(webElement = buttons[[i]])
	  remDr$click()
		Sys.sleep(1)
	
		cat("Gesetz", i, "ausgeklappt\n")
	}

	## Export source
	source <- remDr$getPageSource()
	write(source[[1]], file = str_c("C:/Users/lionb/Desktop/parlis_source/source_", index, ".html"))

	index <- index + 1

	## Nächste Seite 
	button <- remDr$findElement(using = "xpath", value = "//a/i[@class='fa fa-forward']")
	button$clickElement()
	Sys.sleep(7)

	cat("+++ Wir befinden uns bei Index", index, "+++\n")

}





rm(list = ls())

library(stringr)
library(rvest)

#------------------------
## Elemente extrahieren
#------------------------

## Elemente im Ordner
folder <- "C:/Users/lionb/Desktop/parlis_source/"
files_list <- list.files(folder)

## Liste für Output generieren
out_list <- list()

index <- 1

## Über Elemente im Ordner iterieren
for(i in 1:length(files_list)){

	## Read HTML in
	html <- read_html(str_c(folder, files_list[i]))

	## Zahl der Vorgänge im HTML
	N <- length(html_nodes(html, xpath = "//section[@class='vorgang']"))
	
	## Über Vorgänge im HTML iterieren und Infos extrahieren
	for(j in 1:N){
	
		## Datum der Datensammlung
		datum <- Sys.Date()

		## Titel
		titel <- html_nodes(
			html, 
			xpath = str_c(
				"(//div[@class='efxRecordRepeater well'])[",
				j,
				"]/div[@class='drucksache-liste-betreff']/a"
			)
		)
		titel <- html_text(titel)

		## Vorgangstyp
		vorgangstyp <- html_nodes(
			html, 
			xpath = str_c(
				"(//section[@class='vorgang'])[",
				j,
				"]//dt[contains(., 'Vorgangstyp')]/following-sibling::dd[1]"
			)
		)
		vorgangstyp <- html_text(vorgangstyp)
	
		## Initiative
		initiative <- html_nodes(
			html, 
			xpath = str_c(
				"(//section[@class='vorgang'])[",
				j,
				"]//dt[contains(., 'Initiative')]/following-sibling::dd[1]"
			)
		)
		initiative <- html_text(initiative)
		
		## Kurzreferat
		kurzreferat <- html_nodes(
			html, 
			xpath = str_c(
				"(//section[@class='vorgang'])[",
				j,
				"]//dt[contains(., 'Kurzreferat')]/following-sibling::dd[1]"
			)
		)
		kurzreferat <- html_text(kurzreferat)
		
		## Aktueller Stand
		aktueller_stand <- html_nodes(
			html, 
			xpath = str_c(
				"(//section[@class='vorgang'])[",
				j,
				"]//dt[contains(., 'Aktueller Stand')]/following-sibling::dd[1]"
			)
		)
		aktueller_stand <- html_text(aktueller_stand)
		
		## Sachgebiet
		sachgebiet <- html_nodes(
			html, 
			xpath = str_c(
				"(//section[@class='vorgang'])[",
				j,
				"]//dt[contains(., 'Sachgebiet')]/following-sibling::dd[1]"
			)
		)
		sachgebiet <- html_text(sachgebiet)
		
		## Deskriptoren
		deskriptoren <- html_nodes(
			html, 
			xpath = str_c(
				"(//section[@class='vorgang'])[",
				j,
				"]//dt[contains(., 'Deskriptoren')]/following-sibling::dd[1]"
			)
		)
		deskriptoren <- html_text(deskriptoren)
		
		## Fundstellen
		fundstellen <- html_nodes(
			html, 
			xpath = str_c(
				"(//section[@class='vorgang'])[",
				j,
				"]//dt[contains(., 'Fundstellen')]/following-sibling::dd//span/a"
			)
		)
		fundstellen_links <- html_attr(fundstellen, name = "href")
		fundstellen_name <- html_text(fundstellen)
		fundstellen_name <- str_replace_all(fundstellen_name, "[[:space:]]+", " ")
		
		## Protokoll
		protokoll <- html_nodes(
			html, 
			xpath = str_c(
				"(//section[@class='vorgang'])[",
				j,
				"]//dt[contains(., 'Redner')]/following-sibling::dd[contains(., 'Plenarprotokoll')]"
			)
		)
		protokoll <- str_replace(html_text(protokoll), ":", "")

		redner <- list()
		if(length(protokoll) > 0){
			for(k in 1:length(protokoll)){
				redner[[k]] <- html_nodes(
					html, 
					xpath = str_c(
						"(//section[@class='vorgang'])[",
						j,
						"]//dt[contains(., 'Redner')]/following-sibling::dd[",
						k * 2,
						"]//dd[@class='redner']"
					)
				)
				redner[[k]] <- html_text(redner[[k]])
			}
		}

		## Vorgangs-ID
		vorgangs_id <- html_nodes(
			html, 
			xpath = str_c(
				"(//section[@class='vorgang'])[",
				j,
				"]//dt[contains(., 'Vorgangs-ID')]/following-sibling::dd"
			)
		)
		vorgangs_id <- html_text(vorgangs_id)

		out_list[[index]] <- list(
			datum = datum,
			titel = titel,
			vorgangstyp = vorgangstyp,
			initiative = initiative,
			kurzreferat = kurzreferat,
			aktueller_stand = aktueller_stand,
			sachgebiet = sachgebiet,
			deskriptoren = deskriptoren,
			fundstellen_links = fundstellen_links,
			fundstellen_name = fundstellen_name,
			protokoll = protokoll,
			redner = redner,
			vorgangs_id = vorgangs_id
		)
		
		names(out_list)[index] <- vorgangs_id
		
		index <- index + 1
		
		rm(
			datum,
			titel,
			vorgangstyp,
			initiative,
			kurzreferat,
			aktueller_stand,
			sachgebiet,
			deskriptoren,
			fundstellen_links,
			fundstellen_name,
			protokoll,
			redner
		)
		
	}
}



vorgang <- "V-112087"

documents <- out_list[[vorgang]]$fundstellen_links

for(i in 1:length(documents)){
	if(!dir.exists(str_c("C:/Users/lionb/Desktop/parlis_source/out_dat/", vorgang))){
		dir.create(str_c("C:/Users/lionb/Desktop/parlis_source/out_dat/", vorgang))
	}
	download.file(
		documents[i], 
		destfile = str_c(
			"C:/Users/lionb/Desktop/parlis_source/out_dat/", 
			vorgang, 
			"/", 
			basename(documents[i])
		)
	)
}




documents

save(out_list, file = "~/Desktop/out_list.Rdata")


# Download alle Gesetzesentwürfe (nur erster Eintrag in fundstellen_links)
for (i in 1: length(out_list)){
  
  to_download <- str_replace_all(out_list[[i]]["fundstellen_links"][[1]][1], "%5F", "_")
  file_name <- basename(to_download)
  download.file(to_download, destfile = str_c(getwd(), "/", file_name), mode="wb")
  
}


# Download all amendment proposals

  # Iterate over all legislative projects

link_failures <- rep(NA, length(out_list))




  for (k in 1: length(out_list)) {
  
    tryCatch({
    
    # Detect which links contain amendment proposals
    amendment_elements <- rep(NA, length(out_list[[k]]["fundstellen_name"][[1]]))
    
    for (i in 1: length(out_list[[k]]["fundstellen_name"][[1]])) {
      amendment_elements[i] <- str_detect(word(out_list[[k]]["fundstellen_name"][[1]][i], 1), "Änderungsantrag")
    }
    
    # Download respective links
    for (i in 1: length(out_list[[k]]["fundstellen_name"][[1]])){
      if (amendment_elements[i] == TRUE){
        to_download <- str_replace_all(out_list[[k]]["fundstellen_links"][[1]][i], "%5F", "_")
        file_name <- basename(to_download)
        download.file(to_download, destfile = str_c(getwd(), "/", file_name), mode="wb")
      }
      
    }

  
    }, error=function(e){
      link_failures[k] <<- to_download})
  
  }


# Download all committee recommendations

  # Iterate over all legislative projects
  for (k in 1: length(out_list)) {
    
    # Detect which links contain amendment proposals
    amendment_elements <- rep(NA, length(out_list[[k]]["fundstellen_name"][[1]]))
    
    for (i in 1: length(out_list[[k]]["fundstellen_name"][[1]])) {
      amendment_elements[i] <- str_detect(word(out_list[[k]]["fundstellen_name"][[1]][i], 1), "Beschlussempfehlung")
    }
    
    # Download respective links
    for (i in 1: length(out_list[[k]]["fundstellen_name"][[1]])){
      if (amendment_elements[i] == TRUE){
        to_download <- str_replace_all(out_list[[k]]["fundstellen_links"][[1]][i], "%5F", "_")
        file_name <- basename(to_download)
        download.file(to_download, destfile = str_c(getwd(), "/", file_name), mode="wb")
      }
      
    }
    
  }



# Download all plenary protocols

# Iterate over all legislative projects

link_failures <- rep(NA, length(out_list))

for (k in 1: length(out_list)) {
  
  tryCatch({
    
    # Detect which links contain amendment proposals
    amendment_elements <- rep(NA, length(out_list[[k]]["fundstellen_name"][[1]]))
    
    for (i in 1: length(out_list[[k]]["fundstellen_name"][[1]])) {
      amendment_elements[i] <- str_detect(word(out_list[[k]]["fundstellen_name"][[1]][i], 1, 3), "Zweite Beratung Plenarprotokoll")
    }
    
    # Download respective links
    for (i in 1: length(out_list[[k]]["fundstellen_name"][[1]])){
      if (amendment_elements[i] == TRUE){
        to_download <- str_replace_all(out_list[[k]]["fundstellen_links"][[1]][i], "%5F", "_")
        to_download <- str_replace_all(out_list[[k]]["fundstellen_links"][[1]][i], "#page=[:digit:]+", "")
        file_name <- basename(to_download)
        download.file(to_download, destfile = str_c(getwd(), "/", file_name), mode="wb")
      }
      
    }
    
    
  }, error=function(e){
    link_failures[k] <<- to_download})
  
}



