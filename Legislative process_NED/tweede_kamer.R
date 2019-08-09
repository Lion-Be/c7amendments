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

url <- main_url

get_bill_links <- function(url) {
  page <- read_html(url)
  
  
  page_number <- page %>%
    html_node(".pagination") %>%
    html_node(".max-pages") %>%
    html_text()
  
  page_number <- gsub("[^0-9.-]", "", page_number) %>%
    as.integer()
  
  bill_url <- vector("list", length = page_number)
  bill_id <- vector("list", length = page_number)
  bill_date <- vector("list", length = page_number)
  
  pb <- winProgressBar(
    title = "progress bar",
    min = 0,
    max = page_number,
    width = 300
  )
  
  for (i in seq_len(page_number)) {
    rel_links <- page %>%
      html_nodes(".col-md-9") %>%
      html_nodes("a") %>%
      html_attr("href")
    
    keep <- str_detect(rel_links, "/wetsvoorstellen/")
    rel_links <- rel_links[keep]
    bill_url[[i]] <-
      paste0("https://www.tweedekamer.nl/", rel_links)
    
    id <- page %>%
      html_nodes(".card__content") %>%
      html_nodes("span") %>%
      html_text %>% trimws()
    
    bill_id[[i]] <- id[grep("[0-9.-]", id)]
    
    bill_date[[i]] <- page %>%
      html_nodes(".card__pretitle") %>%
      html_text %>% trimws()
    
    next_page_url <-
      page %>% html_node(".next") %>% html_attr("href")
    next_page_url <-
      paste0("https://www.tweedekamer.nl/kamerstukken/wetsvoorstellen",
             next_page_url)
    
    setWinProgressBar(pb, i, title = paste(round(i / page_number * 100, 0),
                                           "% done"))
    
    page <- read_html(next_page_url)
    
  }
  
  out <- cbind(bill_id %>% unlist,
               bill_url %>% unlist %>% as.character,
               bill_date %>% unlist %>% as.character) %>% data.frame
  
  names(out) <- cbind("bill_id", "bill_url", "date")
  
  close(pb)
  
  return(out)
}


all_links <- get_bill_links(main_url)


save(all_links, file = "tweede_kamer.RData")

load("bill_id.RData")
load("tweede_kamer.RData")


openkamer_cover <- all_links[all_links$bill_id %in% data$bill_id,]

if (dir.exists("./open_kamer") == FALSE) {
  dir.create("./open_kamer") }

setwd("C:/Users/Modestas-PC/Desktop/Amendments_docs/test/open_kamer")


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



doc_identifier <- c("voorstelvanwet","notavanwijziging", "amendement")


bill_downloader(openkamer_cover$bill_url %>% as.character() , doc_identifier)

downloaded <- list.files()

undownloaded_bills <- openkamer_cover[!openkamer_cover$bill_url %in% downloaded,]

bill_downloader(undownloaded_bills$input_url, doc_identifier)
