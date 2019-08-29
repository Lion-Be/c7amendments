rm(list = objects())

setwd("C:/Users/Modestas-PC/Desktop/Amendments_docs/test")

library(httr)
library(rvest)
library(dplyr)
library(XML)
library(stringr)


#### Reading URL for finished and adopted bills ------

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

adopted_bill_urls <- lapply(seq_len(page_number), function(i) {
  url <- paste0("https://www.openkamer.org/wetsvoorstellen/?dossier_id=&status=AAN&submitter=&wetsvoorstel_submitter=&title=&wetsvoorstel_submitter_party=&page=", i)
  get_bill_links(url)
}) %>% unlist

adopted_bill_urls <- unique(adopted_bill_urls)


##### Exctracting bill id ---- 

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


adopted_bill_id <- lapply(seq_len(page_number), function(i) {
  url <- paste0("https://www.openkamer.org/wetsvoorstellen/?dossier_id=&status=AAN&submitter=&wetsvoorstel_submitter=&title=&wetsvoorstel_submitter_party=&page=", i)
  get_bill_id(url)
}) %>% unlist

adopted_bill_id <- unique(adopted_bill_id)


adopted_bills <- cbind(adopted_bill_id,adopted_bill_urls)  %>% as.data.frame

adopted_bills$adopted <- 1

names(adopted_bills)[1:2] <- c("bill_id","url")

#### rejected bills ------ 


url <- "https://www.openkamer.org/wetsvoorstellen/?title=&dossier_id=&status=VER&wetsvoorstel_submitter_party=&wetsvoorstel_submitter=&submitter="


baseline_html <- read_html(url)

rejected_bill_urls <- get_bill_links(url)

rejected_bill_id <- get_bill_id(url)

rejected_bills <- cbind(rejected_bill_id,rejected_bill_urls)  %>% as.data.frame()

rejected_bills$adopted <- 0

names(rejected_bills)[1:2] <- c("bill_id","url")

all_bills <- rbind(adopted_bills,rejected_bills)

save(all_bills, file = "bills_openkamer.RData")

###### Scraping info from bill page ----- 

bill_docs <- function(input) {
  id <- vector("list", length(input))
  bill_id <- vector("list", length(input))
  result <- vector("list", length(input))
  date <- vector("list", length(input))
  proposer_name <- vector("list", length(input))
  type <- vector("list", length(input))
  
  
  for (i in seq_along(input)) {
    tryCatch({
      url <- input[i] %>% as.character
      
      main_html <- read_html(url)
      
      changes <-    main_html %>%
        html_node("#cd-timeline") %>%
        html_children()
      
      
      doc_url <- paste0(
        "https://www.openkamer.org",
        changes[grep(".text-success|.text-danger|Motie|Amendement", changes)] %>%
          html_node(".kamertuk-id-timeline-container") %>%
          html_node("a") %>%
          html_attr("href")
      )
      
      doc_id <- rep(NA, length(doc_url))
      
      proposer <- rep(NA, length(doc_url))
      
      time <- rep(NA, length(doc_url))
      
      doc_type <- rep(NA, length(doc_url))
      
      for (k in 1:length(doc_url)) {
        html <- read_html(doc_url[k])
        
        if (grepl("AMENDEMENT|MOTIE",
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
          
          proposer[k] <-  gsub(
            "(?<=[\\s])\\s*|^\\s+|\\s+$|\n|.*:",
            "",
            html %>%
              html_node("body > div.container > div:nth-child(3) > div > h5:nth-child(2)") %>%
              html_text,
            perl = T
          )
          
         if(grepl("AMENDEMENT",
                html %>% html_node(".stuktitel") %>% html_text) == T){
           
           
           doc_type[k] <- "amendment"
           
         }
          
          if(grepl("AMENDEMENT",
                   html %>% html_node(".stuktitel") %>% html_text) == T){
            
            
            doc_type[k] <- "amendment"
            
          }
          
          if(grepl("MOTIE",
                   html %>% html_node(".stuktitel") %>% html_text) == T){
            
            
            doc_type[k] <- "motion"
            
          }
          
        }
        
      }
      

      success <-
        changes[grep(".text-success", changes)] %>% html_node("a") %>% html_attr("id")
      
      failure <-
        changes[grep(".text-danger", changes)] %>% html_node("a") %>% html_attr("id")
      
      adopted <- ifelse((doc_id %in% success == T), 1, 
                        ifelse((doc_id %in% failure == T), 0, "NO INFO"))
      
      id[[i]] <- gsub("-","_",doc_id)
      bill_id[[i]] <- gsub("-.*", "", doc_id)
      result[[i]] <- adopted
      proposer_name[[i]] <- proposer
      date[[i]] <- time
      type[[i]] <- doc_type
      
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
    proposer_name %>% unlist,
    type %>% unlist
  ) %>% as.data.frame %>% na.omit
  
  names(out) <-
    c("bill_id",
      "document_id",
      "document_adopted",
      "date",
      "proposer_name",
      "document_type")
  
  return(out)
  
}


###### saving data from openkamer.org-----

data <- bill_docs(all_bills[,2])

data$date  <- gsub("juni","06", data$date)
data$date  <- gsub("juli","07", data$date)
data$date  <- gsub("augustus","08", data$date)
data$date  <- gsub("september","09", data$date)
data$date  <- gsub("oktober","10", data$date)
data$date  <- gsub("november","11", data$date)
data$date  <- gsub("december","12", data$date)
data$date  <- gsub("januari","01", data$date)
data$date  <- gsub("februari","02", data$date)
data$date  <- gsub("maart","03", data$date)
data$date  <- gsub("april","04", data$date)
data$date  <- gsub("mei","05", data$date)

data$date <- as.Date(data$date, format='%d %m %Y')

save(data, file = "bill_data(openkamer).RData")

load("bill_data(openkamer).RData")
