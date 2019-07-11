rm(list = objects())

setwd("C:/Users/Modestas-PC/Desktop/Amendments_docs/test")

library(httr)
library(rvest)
library(stringr)
library(dplyr)
library(XML)
library(RSelenium)




url <- "https://www.tweedekamer.nl/kamerstukken/wetsvoorstellen"

baseline_html <- read_html(url)

main_html <- baseline_html %>% html_node(".arrow-tabs__dropdown")  %>% html_children()

main_url<- paste0(url,main_html[grep("Afgedaan", main_html)] %>% html_attr("href"))


main_html <- read_html(main_url)


get_bill_links <- function(url) {
  page <- read_html(url)
  
  
  page_number <- page %>%
    html_node(".pagination") %>%
    html_node(".max-pages") %>%
    html_text()
  
  page_number <- gsub("[^0-9.-]", "", page_number) %>%
    as.integer()
  
  rel_links <- page %>%
    html_nodes(".col-md-9") %>%
    html_nodes("a") %>%
    html_attr("href")
  
  
  keep <- str_detect(rel_links, "/wetsvoorstellen/")
  rel_links <- rel_links[keep]
  abs_links <- paste0("https://www.tweedekamer.nl/", rel_links)
  
  next_page_url <- page %>% html_node(".next") %>% html_attr("href")
  next_page_url <-
    paste0("https://www.tweedekamer.nl/kamerstukken/wetsvoorstellen",
           next_page_url)
  
  for (i in 2:page_number) {
    page <- read_html(next_page_url)
    
    rel_links <- page %>%
      html_nodes(".col-md-9") %>%
      html_nodes("a") %>%
      html_attr("href")
    
    
    keep <- str_detect(rel_links, "/wetsvoorstellen/")
    rel_links <- rel_links[keep]
    page_links <- paste0("https://www.tweedekamer.nl/", rel_links)
    
    bill_counter <- length(abs_links) + length(page_links)
    
    abs_links[((bill_counter - length(page_links))+1):bill_counter] <- page_links
    
    next_page_url <-
      page %>% html_node(".next") %>% html_attr("href")
    next_page_url <-
      paste0("https://www.tweedekamer.nl/kamerstukken/wetsvoorstellen",
             next_page_url)
    
    
    
  }
  
  return(abs_links)
}


all_links <- get_bill_links(main_url)


bill_downloader <- function(page, identifier) {
  for (i in 1:length(page)) {
    main_html <- read_html(page[i])
    
    
    bill_id <- main_html %>%
      html_node(
        "#main > section.section.___white.___no-padding-top > div
        > div > div.col-md-3 > ul > li:nth-child(2) > div.link-list__text"
      ) %>%
      html_text()
    
    
    main_html <-
      main_html %>% html_node("#document-list > div.documents") %>%  html_children()
    
    link <-
      main_html[grep(paste(identifier, collapse = "|"), main_html)] %>% html_node("a") %>% html_attr("href")
    
    download_link <-
      paste("https://www.tweedekamer.nl", link, sep = "")
    
    file_name <-
      main_html[grep(paste(identifier, collapse = "|"), main_html)] %>%   html_node("a") %>% html_text()
    
    file_name <- trimws(file_name)
    file_name <- gsub(".*[(]|[)]|Download ", "", file_name)
    file_name <- gsub("-|\\s", "_", file_name)
    file_name <- gsub("[^a-zA-Z0-9_]|\\s", "", file_name)
    file_name <- gsub("U00..", "", file_name)

    
    for (i in 1:length(download_link)) {
      if (grepl("([.])(pdf)", download_link[i]) == T) {
        file_name[i] <- paste(file_name[i], ".pdf", sep = "")
        
      }
      
      if (grepl("([.])(doc)", download_link[i]) == T) {
        file_name[i] <- paste(file_name[i], ".doc", sep = "")
        
      }
      
    }
    doc_table <- cbind(file_name, download_link)
    
    
    if (dir.exists(paste0("./", bill_id)) == FALSE) {
      dir.create(paste0("./", bill_id))
      
      
      
    }
    
    oldw <- getOption("warn")
    options(warn = -1)
    
    for (i in 1:nrow(doc_table)) {
      
      if (file.exists(paste("./", bill_id, file_name[i], sep = "/")) == F){
        
      tryCatch(download.file(
            download_link[i],
            paste("./", bill_id, file_name[i], sep = "/"),
            mode = "wb",
            quiet = T
          ),
          error = function(e)
            print(paste(file_name[i], 'did not work out,', "bill id:", bill_id, "download link:", download_link[i]))
      ) 
        }
    }
    options(warn = oldw)
    
  }
}




doc_identifier <- c("voorstelvanwet","notavanwijziging", "amendement", "stemming" ,"stemmingen","stenogram","eindtekst","vanhetwetsvoorstel")


bill_downloader(all_links, doc_identifier)


