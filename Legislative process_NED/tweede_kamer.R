setwd("C:/Users/Modestas-PC/Desktop/Amendments_docs/test")

library(httr)
library(rvest)
library(stringr)
library(dplyr)
library(XML)


bill_downloader <- function(path, identifier) {
  main_html <-
    path %>% html_node("#document-list > div.documents") %>%  html_children()
  
  link <-
    main_html[grep(paste(identifier, collapse = "|"), main_html)] %>% html_node("a") %>% html_attr("href")
  
  download_link <-
    paste("https://www.tweedekamer.nl", link, sep = "")
  
  file_name <-
    main_html[grep(paste(identifier, collapse = "|"), main_html)] %>%   html_node("a") %>% html_text()
  
  file_name <- gsub(".*[(]|[)]|Download ", "", file_name)
  file_name <- gsub("-|\\s", "_", file_name)
  
  for (i in 1:length(download_link)) {
    if (grepl("([.])(pdf)", download_link[i]) == T) {
      file_name[i] <- paste(file_name[i], ".pdf", sep = "")
      
    }
    
    if (grepl("([.])(doc)", download_link[i]) == T) {
      file_name[i] <- paste(file_name[i], ".doc", sep = "")
      
    }
    
  }
  doc_table <- cbind(file_name, download_link)
  
  for (i in 1:nrow(doc_table)) {
    download.file(download_link[i],
                  file_name[i],
                  mode = "wb",
                  method = "wininet")
  }
}


url <- "https://www.tweedekamer.nl/kamerstukken/wetsvoorstellen/detail?cfg=wetsvoorsteldetails&qry=wetsvoorstel%3A35084"


baseline_html <- read_html(url)

doc_identifier <- c("voorstelvanwet","notavanwijziging", "amendement", "stemming" ,"stemmingen","stenogram")



bill_downloader(baseline_html, doc_identifier)


bill_name <- baseline_html %>% html_node("h1#main-title") %>% html_text()

bill_id <- baseline_html %>% html_node("#main > section.section.___white.___no-padding-top > div > div > div.col-md-3 > ul > li:nth-child(2) > div.link-list__text") %>% html_text()   


proposer_name <- baseline_html %>% html_node(
  "#main > section.section.___white.___no-padding-top > div > div > div.col-md-3 > ul > li:nth-child(4) > div.link-list__text > div:nth-child(2)"
) %>% html_text() 


