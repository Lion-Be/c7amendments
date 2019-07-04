


















# scrape information from the simple search procedure from dip.de
library(httr)
library(rvest)
library(stringr)

url <- "https://dipbt.bundestag.de/dip21.web/"

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
	add_headers(Connection = "keep-alive"),
	body = list(
		wahlperiode = "5",
		nummer = "17/3958",
		vorgangstyp = "6",
		method = "Suchen"
	),
	encode = "form"
)


tmp_html <- GET(
	"https://dipbt.bundestag.de/dip21.web/searchProcedures/simple_search_detail_vp.do",
	config = list(
		set_cookies(
			.cookies = c(
				JSESSIONID = session_cookie,
				SESSIONID = session_cookie
			)
		)
	),
	add_headers(
		.headers = c(
			Referer = "http://dipbt.bundestag.de/dip21.web/searchProcedures/simple_search.do"
		)
	)
)

write_html(
	content(results_html),
	file = "~/Desktop/results.html"
)

write_html(
	content(tmp_html),
	file = "~/Desktop/tmp.html"
)

html <- read_html("~/Desktop/tmp.html")

nodes_title <- html_nodes(html, xpath = "//div[@id='inhaltsbereich']//fieldset//dl[@class='vorgangsablaufTitel']")
nodes_link <- html_nodes(html, xpath = "//*[@id='inhaltsbereich']//fieldset//dd/a")
					
title <- html_text(nodes_title)
link <- html_text(nodes_link)

link <- link[str_detect(link, "\\w")]

dat <- data.frame(
	title = title,
	link = link
)

dat$date <- str_extract(dat$link, "\\d{2}.\\d{2}.\\d{4}")

View(dat[str_detect(dat$title, "Gesetzentwurf"),])






