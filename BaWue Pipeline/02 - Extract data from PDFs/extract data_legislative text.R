# -------------------------------------------
# Automatically extract data from PDF files
# PDF Type: Legislative Text
# Lion Behrens
# -------------------------------------------
setwd("M:/Parlis complete corpus/corpus_complete/WP14/out_dat/Gesetzesentwuerfe")

library(tm)
library(stringr)

# --------------------
# 1. Import raw text
# --------------------

# Create an object containing all PDF file names
# The pattern argument stores all files ending with pdf
files <- list.files(pattern = "pdf$")

# Create function to read in PDF files
# -layout maintains original physical layout of text as best as possible
rpdf <- readPDF(control = list(text = "-layout"))

# Read in single document
# document <- Corpus(URISource("16_3638_D.pdf"), readerControl = list(reader = rpdf))

# Read in all pdf files stored in working directory
pdfs <- Corpus(URISource(files), 
               readerControl = list(reader = rpdf))

error <- rep(NA, length(pdfs))

# Inspect content of one pdf-file
for (z in 1: length(pdfs)){
  
  
  
  # Move on to next iteration if error
  tryCatch({
    
    
    
    content <- content(pdfs[[z]]) # content of first pdf, separated by page
    
    # Merge elements into joint string
    joint <- paste(content, collapse="")
    
    # Store text by row
    jointsplit <- strsplit(joint, split="\n")
    text <- unlist(jointsplit[1])
    
    
    
    # ----------------------
    # 2. Cleaning raw text
    # ----------------------
    
    # Remove metacharacters
    textnew <- gsub('[\r]', '', text)
    textnew <- gsub('[\a]', '', textnew)
    textnew <- gsub('[\t]', '', textnew)
    
    # Delete all headers except first page
    
    # Identify rows with headers and pagenumbers
    header_rows_prel <- grep("Landtag von Baden-Württemberg", textnew)  
    textnew[header_rows_prel] <- str_wrap(textnew[header_rows_prel], width=50)
    header_rows <- grep("Landtag von Baden-Württemberg Drucksache", textnew)  
    header_rows <- header_rows[-1] # first header row is not part of operation b/c we need this information
    
    pagenumber_rows <- rep(NA, length(header_rows))
    for (i in 1:length(header_rows)){
      pagenumber_rows[i] <- header_rows[i]-1                       
    } 
    pagenumber_rows <- c(pagenumber_rows, length(textnew)) # add last row manually because no header follows
    deleted_rows <- c(pagenumber_rows[1]-1, pagenumber_rows[1]-2, pagenumber_rows)  
    deleted_rows <- c(header_rows, deleted_rows)
    
    # Delete header row and row before
    textnew <- textnew[-deleted_rows]
    
    # Store text with and without whitespace
    textnew_white <- textnew
    textnew <- trimws(textnew, which = "left")
    
    
    
    # --------------------
    # 3. Extracting data
    # --------------------
    
    # -----------------------------------
    # Table: Legislative Text Documents
    # -----------------------------------
    
    # idDrucksache
    idDrucksache1 <- sub('.*Drucksache ', '', textnew[1]) # select all to the right of pattern
    
    # datestemp
    datestemp1 <- sub('.*Wahlperiode ', '', textnew[2]) # select all to the right of pattern
    datestemp1 <- trimws(datestemp1, which = "left")
    
    # period
    period1 <- sub('. Wahlperiode.*', '', textnew[2]) # select all to the left of pattern
    
    # Missing: pdfdocu, idVorgang
    
    
    # --------------------------
    # Table: Legislative Text
    # --------------------------
    
    # idDrucksache
    idDrucksache <- idDrucksache1
    
    # idVersion
    idVersion <- textnew[3]
    
    # final 
    if (idVersion == "Gesetzesbeschluss") {
      final <- 1 } else {
        final <- 0
      }
    
    # idFraction
    namerow_end <- (grep("A. Zielsetzung", textnew)[1]) - 1
    name_section <- textnew[seq(1,namerow_end, by=1)]
    
    namerow_start <- grep("\\bGesetz\\b", name_section)[1] 
    if (is.na(namerow_start) == TRUE){
      namerow_start <- grep("Änderung des Gesetzes", name_section)[1]
    }
    if (is.na(namerow_start) == TRUE){
      namerow_start <- grep("Ausführungsgesetz", name_section)[1]
    }
    if (is.na(namerow_start) == TRUE){
      namerow_start <- grep("Haushaltsbegleitgesetz", name_section)[1]
    }
    
    legisheader <- paste(textnew[4:namerow_start-1], collapse=" ")
    
    fractions <- c("Landesregierung", "SPD", "AfD", "CDU", "GRÜNE", "FDP/DVP")
    fractionresults <- matrix(NA, nrow=length(legisheader), ncol=length(fractions))
    colnames(fractionresults) <- fractions
    
    for (i in 1: length(fractions)){
      for (k in 1: length(legisheader)){
        fractionresults[k,i] <- str_detect(legisheader[k], fractions[i])
      }
    }
    
    for (i in 1: length(fractionresults)){
      
      if (fractionresults[i]==TRUE) {
        fractionresults[i] <- 1 } else if (fractionresults[i]==FALSE) {
          fractionresults[i] <- 0 }
    } 
    
    
    # withfraction
    
    # Identify date rows
    datepattern <- "[:digit:]{2}\\. [:digit:]{2}\\. [:digit:]{4}" # all spaces
    datepattern2 <- "[:digit:]{2}\\.[:digit:]{2}\\.[:digit:]{4}" # no spaces
    datepattern3 <- "[:digit:]{2}\\. [:digit:]{2}\\.[:digit:]{4}" # space after dd
    datepattern4 <- "[:digit:]{2}\\.[:digit:]{2}\\. [:digit:]{4}" # space after mm
    
    # nur behalten, wenn nur Datum in Zeile genannt ist, nichts anderes
    datestemp_rows <- (str_which(textnew[-2], datepattern) + 1)
    datestemp_rows2 <- (str_which(textnew[-2], datepattern2) + 1)
    datestemp_rows3 <- (str_which(textnew[-2], datepattern3) + 1)
    datestemp_rows4 <- (str_which(textnew[-2], datepattern4) + 1)
    datestemp_rows <- c(datestemp_rows, datestemp_rows2, datestemp_rows3, datestemp_rows4)
    
    if (length(datestemp_rows) > 0) {
      for (i in 1: length(datestemp_rows)){
        if (nchar(textnew[datestemp_rows[i]]) > 13) {
          datestemp_rows[i] <- NA
        }
      }
    }
    
    datestemp_row <- sort(na.omit(datestemp_rows))
    
    if (length(datestemp_row) > 0) {
      
      footer_start <- datestemp_row + 1
      footer_end <- grep("\\bBegründung\\b", textnew)
      condition <- textnew[footer_end] == "Begründung"
      footer_end <- (footer_end[condition != "FALSE"]) - 1
      
      legisfooter <- NA
      legisfooter <- paste(textnew[footer_start[i]:footer_end[i]], collapse=" ")
      
      withfraction <- str_detect(legisfooter, "und Fraktion")
      if (withfraction == TRUE) {
        withfraction <- 1
      } else {
        withfraction <- 0
      }
      
    } else {
      
      withfraction <- "NA"
      
    }
    
    # members2
    if (length(datestemp_row) > 0) {
      members2 <- legisfooter
    } else {
      members2 <- "none"
    }
    
    # members1
    if (str_detect(legisheader, "Fraktion") == FALSE & str_detect(legisheader, "Landesregierung") == FALSE){
      members1 <- legisheader
    } else {
      members1 <- "none"
    }
    
    
    # rawtext
    rawtext_begin <- grep("Der Landtag wolle beschließen", textnew) + 3
    
    if (length(datestemp_row) > 0) {
      rawtext_end <- datestemp_row - 1 } else {
        
        rawtext_end <- grep("\\bBegründung\\b", textnew)
        condition <- textnew[rawtext_end] == "Begründung"
        rawtext_end <- ((rawtext_end[condition != "FALSE"]) - 1)[1]
        
        rawtext_end_alt <- grep("^Anlage", textnew)
        if (length(rawtext_end_alt) > 0){
          
          for (i in 1: (length(rawtext_end_alt))) {
            if (nchar(textnew[rawtext_end_alt[i]]) > 10) 
              rawtext_end_alt[i] <- NA
          }
        }
        
        if (length(rawtext_end_alt) > 0)
          if (length(which(rawtext_end_alt != "NA") > 0)){
            rawtext_end_alt <- na.omit(rawtext_end_alt)  
            rawtext_end <- min(rawtext_end, rawtext_end_alt)-1
          }
        
      }
    
    rawtext_left <- list(textnew[seq(rawtext_begin,rawtext_end,1)])
    rawtext_white <- list(textnew_white[seq(rawtext_begin,rawtext_end,1)])
    
    
    # ------------------------
    # 4. Store data in table
    # ------------------------
    
    # ----------------------------
    # Table: Amendment Proposals
    # ----------------------------
    
    # what is with pdf-docu-table?
    
    if (z == 1){
      
      
      # --------------------------
      # Table: Legislative Text
      # --------------------------
      
      # Create empty matrix
      legistext <- as.data.frame(matrix(NA, ncol=28, nrow=1))
      colnames(legistext) <- c("idText", "idVorgang", "datestemp", "name", "idDrucksache", "version", "final", "status", "sachgebiet", "kurzreferat", "deskriptoren",
                               "rollcall", "rollcallbev", "idCommittee", "Landesregierung",          
                               "SPD", "AfD", "CDU", "GRUENE", "FDP/DVP", "members1", "members2", "withfraction", "links", "protokoll", 
                               "rawtext_left", "rawtext_white", "error")
      
      # Fill with information
      legistext$datestemp <- datestemp1
      legistext$idDrucksache <- idDrucksache   
      legistext$version <- idVersion
      legistext$final <- final  
      legistext$Landesregierung <- fractionresults[,"Landesregierung"]  
      legistext$rawtext_left <- rawtext_left
      legistext$rawtext_white <- rawtext_white
      legistext$SPD <- fractionresults[,"SPD"]  
      legistext$AfD <- fractionresults[,"AfD"]   
      legistext$CDU <- fractionresults[,"CDU"]  
      legistext$GRUENE <- fractionresults[,"GRÜNE"]
      legistext$"FDP/DVP" <- fractionresults[,"FDP/DVP"]
      legistext$members1 <- members1
      legistext$members2 <- members2
      legistext$withfraction <- withfraction
      legistext$error <- 0
      
    } else {
      
      
      # --------------------------
      # Table: Legislative Text
      # --------------------------  
      
      # Enlarge dataframe
      newdata <- as.data.frame(matrix(NA, ncol= ncol(legistext), nrow=1))
      colnames(newdata) <- colnames(legistext)
      
      newdata$datestemp <- datestemp1
      newdata$idDrucksache <- idDrucksache   
      newdata$version <- idVersion
      newdata$final <- final  
      newdata$Landesregierung <- fractionresults[,"Landesregierung"]  
      newdata$rawtext_left <- rawtext_left
      newdata$rawtext_white <- rawtext_white
      newdata$SPD <- fractionresults[,"SPD"]  
      newdata$AfD <- fractionresults[,"AfD"]   
      newdata$CDU <- fractionresults[,"CDU"]  
      newdata$GRUENE <- fractionresults[,"GRÜNE"]
      newdata$"FDP/DVP" <- fractionresults[,"FDP/DVP"]
      newdata$members1 <- members1
      newdata$members2 <- members2
      newdata$withfraction <- withfraction
      newdata$error <- 0
      
      legistext <- as.data.frame(rbind(legistext, newdata))
      
    }
    
    
  }, error = function(e){
    
    error[z] <<- sub('.*Drucksache ', '', textnew[1])
    
  }) # end tryCatch
  
  
} # end for loop over all legislative text documents  



# ---------------
# Error control
# ---------------

# Create error vector
error <- na.omit(error)

# Create error dataframes
legistext_err <- as.data.frame(matrix(NA, ncol=ncol(legistext), nrow=length(error)))
colnames(legistext_err) <- colnames(legistext)
legistext_err$idDrucksache <- error
legistext_err$error <- 1

# Merge dataframes
legistext <- as.data.frame(rbind(legistext, legistext_err))
rm(list=setdiff(ls(), "legistext"))



