








## POST-Befehl

library(httr)
library(rvest)
library(stringr)

url <- "http://dipbt.bundestag.de/dip21.web/"

baseline_html <- POST(
	str_c(url, "bt")
)

session_cookie <- cookies(baseline_html)[1, "value"]

results_html <- POST(
	str_c(
		url,
		"searchProcedures/simple_search.do"
	),
	config = list(
		set_cookies(
			.cookies = c(
				JSESSIONID = session_cookie,
				SESSIONID = session_cookie
			)
		)
	),
	body = list(
		wahlperiode = "9",
		nummer = "15/5658",
		method = "Suchen"
	),
	encode = "form"
)

write_html(content(results_html), file = "~/Desktop/file.html")





## Selenium

library(RSelenium)
library(stringr)

## Download and start Docker

## Run in Terminal

#docker run -d -p 4445:4444 selenium/standalone-chrome

## Set up connection to browser
driver <- remoteDriver(
	remoteServerAddr = "localhost",
	port = 4445L,
	browserName = "chrome"
)

driver$open(silent = T)

## Navigate to page
driver$navigate("https://dipbt.bundestag.de/dip21.web/searchDocuments/drs_search_text.do")

## Session-ID besorgen
button <- driver$findElement(using = "xpath", value = "//a[@class='linkIntern']")

button$clickElement()

## Beratungen auswählen
beratungen <- driver$findElement(
	using = "xpath",  
	value = "//div[@id='navigationMenu']/ul/li[4]/ul/li[1]//a"
)

beratungen$clickElement()

## Wahlperiode auswählen
wahlperiode <- driver$findElement(using = "xpath", value = "//select[@name='wahlperiode']//option[@value='9']")

wahlperiode$clickElement()

## Drucksache auswählen
drucksache <- driver$findElement(using = "id", value = "nummer")

drucksache$clickElement()

drucksache$sendKeysToActiveElement(list("15/5658"))

## Suche abschicken
suche <- driver$findElement(using = "id", value = "btnSuche")

suche$clickElement()

## Quellcode extrahieren
out <- driver$getPageSource()

html <- read_html(out[[1]])

## Kategorien und Werte des Gesetzes zusammenstellen
category <- html_nodes(html, xpath = "//fieldset[@class='field']//dt")
wert <- html_nodes(html, xpath = "//fieldset[@class='field']//dd")

category <- str_trim(html_text(category))
wert <- str_replace(str_trim(html_text(wert)), "[[:space:]]+", " ")







