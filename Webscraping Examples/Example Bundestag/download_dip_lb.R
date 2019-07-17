### Scraping all DIP-Drucksachen from specified legislative period
### Lion Behrens
library(stringr)

# Manually specify legislative period and latest Drucksache
period <- 15
latest_drucksache <- 6016

# Download all documents
for (drucksache in 1 : latest_drucksache) {
  
  tryCatch({
  
    # Add leading zeros 
    drucksache <- str_pad(drucksache, 5, pad = "0")
    
    # Construct download link 
    to_download <- str_c("http://dipbt.bundestag.de/dip21/btd/",
                         period, 
                         "/",
                         substr(drucksache, 1, 3), 
                         "/",
                         period,
                         drucksache,
                         ".pdf")
    
    # Download
    file_name <- basename(to_download)
    download.file(to_download, destfile = str_c(getwd(), "/", file_name), mode="wb")
  
  }, error=function(e){})
    
}
  