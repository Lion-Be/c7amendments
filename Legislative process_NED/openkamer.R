rm(list = objects())

setwd("C:/Users/Modestas-PC/Desktop/Amendments_docs/test")

library(httr)
library(rvest)
library(stringr)
library(dplyr)
library(XML)

#### Reading URL ------

url <- "https://www.openkamer.org/wetsvoorstellen/?title=&dossier_id=&status=AAN&wetsvoorstel_submitter_party=&wetsvoorstel_submitter=&submitter="

baseline_html <- read_html(url)

#### Exctracting total number of page numbers -----

page_number <- baseline_html %>%
  html_node(".pagination") %>%
  html_node("li:nth-child(14) > a") %>%
  html_attr("href")

page_number <- gsub("[^0-9.-]", "", page_number) %>%
  as.integer()


#### Extracting all links to bills ----- 

get_bill_links <- function(url) {
  page <- read_html(url)
  
  rel_links <- page %>%
    html_node("body > div.container")  %>%
    html_children()
  
  rel_links <- rel_links %>%
    html_children() %>%
    html_node("a") %>%
    html_attr("href")
  
  keep <- str_detect(rel_links, "/dossier/tijdlijn")
  rel_links <- rel_links[keep]
  rel_links <- na.omit(rel_links)
  abs_links <- paste0("https://www.openkamer.org", rel_links)
    
  return(abs_links)
}

all_bill_urls <- lapply(seq_len(page_number), function(i) {
  url <- paste0("https://www.openkamer.org/wetsvoorstellen/?dossier_id=&status=AAN&submitter=&wetsvoorstel_submitter=&title=&wetsvoorstel_submitter_party=&page=", i)
  get_bill_links(url)
}) %>% unlist

all_bill_urls <- unique(all_bill_urls)



##### Exctracting all bill id---- 

get_bill_id <- function(url) {
  page <- read_html(url)
  
  bill_id <- page %>%
    html_node("body > div.container")  %>%
    html_children()
  
  bill_id <-  bill_id %>%
    html_children() %>%
    html_node(".col-xs-2") %>%
    html_text() %>%
    trimws() %>%
    na.omit()
  
  return(bill_id)
}


all_bill_id <- lapply(seq_len(page_number), function(i) {
  url <- paste0("https://www.openkamer.org/wetsvoorstellen/?dossier_id=&status=AAN&submitter=&wetsvoorstel_submitter=&title=&wetsvoorstel_submitter_party=&page=", i)
  get_bill_id(url)
}) %>% unlist

all_bill_id <- unique(all_bill_id)


adopted_bills <- cbind(all_bill_id,all_bill_urls)


###### Scraping info from bill page ----- 


bill_docs <- function(input) {
  id <- vector("list", length(input))
  bill_id <- vector("list", length(input))
  result <- vector("list", length(input))
  date <- vector("list", length(input))
  proposer_name <- vector("list", length(input))
  
  for (i in seq_along(input)) {
    tryCatch({
      url <- input[i]
      
      main_html <- read_html(url)
      
      changes <-    main_html %>%
        html_node("#cd-timeline") %>%
        html_children()
      
      
      doc_url <- paste0(
        "https://www.openkamer.org",
        changes[grep(".text-success|.text-danger", changes)] %>%
          html_node(".kamertuk-id-timeline-container") %>%
          html_node("a") %>%
          html_attr("href")
      )
      
      doc_id <- NA
      
      proposer <- NA
      
      time <- NA
      
      for (k in 1:length(doc_url)) {
        html <- read_html(doc_url[k])
        
        if (grepl("AMENDEMENT",
                  html %>% html_node(".stuktitel") %>% html_text) == T) {
          doc_id[k]  <-  gsub(
            "ID: ",
            "",
            html %>%
              html_node(
                "body > div.container > div:nth-child(3) > div > h5:nth-child(5)"
              ) %>%
              html_text
          )
          
          time[k] <- gsub(
            "Gepubliceerd: ",
            "",
            html %>%
              html_node(
                "body > div.container > div:nth-child(3) > div > h5:nth-child(1)"
              ) %>%
              html_text
          )
          
          proposer[k] <-  html %>%
            html_node(
              "body > div.container > div:nth-child(3) > div > h5:nth-child(2) > a:nth-child(1)"
            ) %>%
            html_text
        }
        
      }
      
      doc_id <- na.omit(doc_id)
      time <- na.omit(time)
      proposer <- na.omit(proposer)
      
      success <-
        changes[grep(".text-success", changes)] %>% html_node("a") %>% html_attr("id")
      
      adopted <- ifelse((doc_id %in% success == T), 1, 0)
      
      id[[i]] <- doc_id
      bill_id[[i]] <- gsub("-.*", "", doc_id)
      result[[i]] <- adopted
      proposer_name[[i]] <- proposer
      date[[i]] <- time
      
      
    },
    
    error = function(cond) {
      message(paste("URL does not seem to exist:", url))
      message("Here's the original error message:")
      message(cond)
    })
  }
  out <- cbind(
    bill_id %>% unlist,
    id %>% unlist,
    result %>% unlist,
    date %>% unlist,
    proposer_name %>% unlist
  ) %>% as.data.frame
  
  names(out) <-
    c("bill_id",
      "amendment_id",
      "adopted",
      "date",
      "proposer_name")
  
  return(out)
  
}


###### saving data from openkamer.org-----

data <- bill_docs(all_bill_urls)


uncovered_bills <- adopted_bills[!adopted_bills[,1] %in% data[,1],]

