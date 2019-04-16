# -------------------------------------------
# Automatically extract data from PDF files
# PDF Type: Amendment Proposals
# Lion Behrens
# -------------------------------------------

# -----------------------------------------------------------------------------------------------
# In diesem Skript wird der Erfolg von Änderungsanträgen ausgelesen
# Erst: Wenn Gesetz an sich abgelehnt wurde, Änderungsanträge direkt als unerfolgreich codieren
# Nur für den Rest der Gesetze auslesen
# -----------------------------------------------------------------------------------------------
abgelehnte_gesetze <- which(legistext$status=="Abgelehnt")
amendmentprop$success[which(is.element(amendmentprop$idDrucksacheLegis, legistext$idDrucksache[abgelehnte_gesetze]))] <- 0


setwd("M:/Parlis complete corpus/corpus_complete/PDF Plenarprotokolle")

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
  
  
  # loop over all protocols (PDFs)
  protocol_pdf_error <- rep(0, length(files))
  
  for (protocol_pdf in 1:length(files)) {
    
    tryCatch({
  
  
  content <- content(pdfs[[protocol_pdf]]) # content of first pdf, separated by page

  # Merge elements into joint string
  #joint <- paste(content, collapse="")
  
  # Store text by row
  text <- strsplit(content, split="\n")
  #text <- lapply(text, trimws, which="left")
  #text <- unlist(jointsplit[1])

  # Delete table of contents (needs automatization)
  iteration <- 1
  for (page in 1:length(text)) {
    if (length(grep("Protokoll\r", text[[page]])) == 0) {
      iteration <- iteration + 1
    } else { break }
  }
  
  if (iteration == 2) {
    text[[1]] <- NULL 
  }
  if (iteration == 3) {
    text[[1]] <- NULL
    text[[1]] <- NULL 
  }
  if (iteration == 4) { 
    text[[1]] <- NULL
    text[[1]] <- NULL
    text[[1]] <- NULL
  }
  if (iteration == 5) {
    text[[1]] <- NULL
    text[[1]] <- NULL
    text[[1]] <- NULL 
    text[[1]] <- NULL 
  }
    
  # ---------------
  # For page 1
  # ---------------
  
    # Separate two columns
    text_split <- str_split(text[[1]], "[:space:]{5,}")
    
    for (row in 1:length(text_split)){
      if ((length(text_split[[row]]) > 2) & length(which(nchar(text_split[[row]]) == 0)) > 0 )
        text_split[[row]] <- text_split[[row]][-which(nchar(text_split[[row]]) == 0)]
    }

    column_1 <- rep(NA, length(text_split))
    for (i in 1:length(text_split))
      column_1[i] <- text_split[[i]][1] 
    
    column_2 <- rep(NA, length(text_split))
    for (i in 1:length(text_split))
      column_2[i] <- text_split[[i]][2] 
    
    text_split <- c(column_1, column_2)
    text_split <- na.omit(text_split)
    
    # Header löschen
    text_split <- text_split[-grep("^Landtag von Baden-Württemberg...", text_split)]
    
    # Remove metacharacters
    text_split <- gsub('[\r]', '', text_split)
    
    # Store page object
    page_text <- text_split

    
  # ----------------------
  # For every other page
  # ----------------------
    
    for (page in 2:length(text)) {
    
      # Separate two columns
      text_split <- str_split(text[[page]], "[:space:]{5,}")
      
      for (row in 1:length(text_split)){
        if ((length(text_split[[row]]) > 2) & length(which(nchar(text_split[[row]]) == 0)) > 0 )
          text_split[[row]] <- text_split[[row]][-which(nchar(text_split[[row]]) == 0)]
      }
      
      column_1 <- rep(NA, length(text_split))
      for (i in 1:length(text_split))
        column_1[i] <- text_split[[i]][1] 
      
      column_2 <- rep(NA, length(text_split))
      for (i in 1:length(text_split))
        column_2[i] <- text_split[[i]][2] 
      
      text_split <- c(column_1, column_2)
      text_split <- na.omit(text_split)
     
      # Header löschen
      text_split <- text_split[-grep("^Landtag von Baden-Württemberg...", text_split)]
      
      # Remove metacharacters
      text_split <- gsub('[\r]', '', text_split)
      
      # Clip page to prior page(s)
      page_text <- c(page_text, text_split)
    }
    
    
    # -----------------------------
    # Delete hyphens in page_text
    # -----------------------------
    for (i in 1: length(page_text)) {
      
      # if there is text in cell
      if (nchar(page_text[i]) > 0) {
        
          # if row ends with "-"
          if((str_sub(page_text[i], -1, -1) == "-") == TRUE) {
            
            
            # if first character of next row is lower case
            is.lower <- "[a-z]"
            lowercase <- grepl(pattern = is.lower, x = str_sub(page_text[i+1], 1, 1))
            
            if (lowercase == TRUE) {
              
              # delete "-"  
              page_text[i] <- str_sub(page_text[i], 1, -2)
              
              # clip last word of row to next row
              page_text[i+1] <- paste(c(word(page_text[i], -1), page_text[i+1]), collapse="")
              
              # delete last word of row 
              
              # count number of words
              n.words <- sapply(strsplit(page_text[i], " "), length)
              
              # only keep first to second last word
              page_text[i] <- paste(word(page_text[i], 1:(n.words-1)), collapse=" ")
              
              
            } else { 
              
              
              # if first character of next row is upper case
              
              # clip "-" to next row  
              page_text[i+1] <- paste(c(str_sub(page_text[i], -1, -1), page_text[i+1]), collapse="")
              
              # delete "-" in row
              page_text[i] <- str_sub(page_text[i], 1, -2)
              
              # clip last word of row to next row
              page_text[i+1] <- paste(c(word(page_text[i], -1), page_text[i+1]), collapse="")
              
              # delete last word of row 
              
              # count number of words
              n.words <- sapply(strsplit(page_text[i], " "), length)
              
              # only keep first to second last word
              page_text[i] <- paste(word(page_text[i], 1:(n.words-1)), collapse=" ")
              
              
            } # end if (lowercase == TRUE)     
            
          } # end if((str_sub(legistext$rawtext_left[[k]][i], -1, -1) == "-") == TRUE)
          
      } # end if (nchar(page_text[k]) > 0) {
      
    } # end for i
    
    
    
    
    
    
# -----------------------------------------------------
# 2. Extract data (success of amendments)
# -----------------------------------------------------   
    
    # Which bills are discussed in this protocol?
    which_legis <- vector()
    iteration <- 1
    for (i in 1:nrow(legistext))
      if (length(which(str_detect(page_text, legistext$idDrucksache[i])==T))>0) {
        which_legis[iteration] <- legistext$idDrucksache[i]
        iteration <- iteration + 1
      }

    # Which bills were not rejected? (and thus have the potential for successfull amendments)
    which_legis_relevant <- which_legis[which(is.element(which_legis, legistext$idDrucksache[abgelehnte_gesetze]) == F)]
    
    if (length(which_legis_relevant)==0)
      next
    
    
    
    # -------------------------------
    # For each bill
    for (legis_relevant in 1:length(which_legis_relevant)) {
    # Extract relevant text
    # -------------------------------
    
    bill_mention <- which(str_detect(page_text, which_legis_relevant[legis_relevant])) # which bill?
    
    for (i in 1:length(bill_mention))
      if(length(which(str_detect(page_text[(bill_mention[i]-1) : (bill_mention[i]+1)], "(Abstimmung|A b s t i m m u n g)"))) == 0)
        bill_mention[i] <- NA    
    
    bill_mention <- na.omit(bill_mention)
    bill_mention <- bill_mention[1]
    
    # Wenn es zu diesem Gesetz keine Abstimmung gibt (Erste Beratung), dann überspringen
    if (is.na(bill_mention) == T)
      next
    
    sinnabschnitte <- which(str_detect(page_text, "^([:lower:][)][:space:]Aktuelle Debatte|Aktuelle Debatte)") |
          str_detect(page_text, "^([:lower:][)][:space:]Kleine Anfrage|Kleine Anfrage)") |  
          str_detect(page_text, "^([:lower:][)][:space:]Große Anfrage|Große Anfrage)") |
          str_detect(page_text, "^([:lower:][)][:space:]Zweite Beratung|Zweite Beratung)") |
          str_detect(page_text, "^([:lower:][)][:space:]Erste Beratung|Erste Beratung)") |
          str_detect(page_text, "^([:lower:][)][:space:]Dritte Beratung|Dritte Beratung)") |
          str_detect(page_text, "^([:lower:][)][:space:]Fortsetzung der Zweiten Beratung|Fortsetzung der Zweiten Beratung)") |
          str_detect(page_text, "^([:lower:][)][:space:]Fortsetzung der Ersten Beratung|Fortsetzung der Ersten Beratung)") |
          str_detect(page_text, "^([:lower:][)][:space:]Fortsetzung der Dritten Beratung|Fortsetzung der Dritten Beratung)") |
          str_detect(page_text, "^([:lower:][)][:space:]Antrag der|Antrag der)") |
          str_detect(page_text, "^([:lower:][)][:space:]Beschlussempfehlung und Bericht|Beschlussempfehlung und Bericht)") |
          str_detect(page_text, "^([:lower:][)][:space:]Einzelplan[:space:][:digit]+:|Einzelplan[:space:][:digit]+:)") |     
          str_detect(page_text, "^([:lower:][)][:space:]Herstellung des Einvernehmens|Herstellung des Einvernehmens)"))
    # fehlt: Beschlussempfehlung und Bericht
    
    
    end_bill_mention <- sinnabschnitte[which(sinnabschnitte > bill_mention)[1]]
    
    bill_mention_text <- page_text[bill_mention:end_bill_mention]
    
    
    # -------------------------------
    # For each bill
    # Create success matrix
    # -------------------------------
    
    # Drucksachen
    drucksachen_list <- str_extract(bill_mention_text, "([:digit:][:digit:][/][:digit:]+|Gesetz-|Gesetzesentwurf|Gesetzentwurf)")
    drucksachen_idx <- which(str_detect(bill_mention_text, "([:digit:][:digit:][/][:digit:]+|Gesetz-|Gesetzesentwurf|Gesetzentwurf)"))
    idx <- which(is.na(drucksachen_list)==F & is.element(drucksachen_list, amendmentprop$idDrucksache[amendmentprop$committee==0]))
    drucksachen <- drucksachen_list[idx]

    # Drucksachen-Nummer
    drucksachen_nummer_list <- str_extract(bill_mention_text, "([:digit:][:digit:][/][:digit:]+-[:digit:]+|[:digit:][:digit:][/][:digit:]+|Gesetz-|Gesetzesentwurf|Gesetzentwurf)")
    drucksachen_nummer_idx <- which(str_detect(bill_mention_text, "([:digit:][:digit:][/][:digit:]+-[:digit:]+|[:digit:][:digit:][/][:digit:]+|Gesetz-|Gesetzesentwurf|Gesetzentwurf)"))
    drucksachen_nummer <- drucksachen_nummer_list[idx]
    
    # Decisions
    decision_words <- c("angenommen", "zugestimmt", "beschlossen", "abgelehnt", "Ablehnung", "erledigt")
    decision_idx <- which(str_detect(bill_mention_text, decision_words[1]) |
                          str_detect(bill_mention_text, decision_words[2]) |
                          str_detect(bill_mention_text, decision_words[3]) |
                          str_detect(bill_mention_text, decision_words[4]) |
                          str_detect(bill_mention_text, decision_words[5]) |
                          str_detect(bill_mention_text, decision_words[6]) ) 
    decision_list <- list()
    for (i in 1:length(decision_words))
      decision_list[[i]] <- str_extract(bill_mention_text, decision_words[i])
    
    decision_list[[1]][is.na(decision_list[[2]])==F] <- decision_list[[2]][is.na(decision_list[[2]])==F]
    decision_list[[1]][is.na(decision_list[[3]])==F] <- decision_list[[3]][is.na(decision_list[[3]])==F]
    decision_list[[1]][is.na(decision_list[[4]])==F] <- decision_list[[4]][is.na(decision_list[[4]])==F]
    decision_list[[1]][is.na(decision_list[[5]])==F] <- decision_list[[5]][is.na(decision_list[[5]])==F]
    decision_list[[1]][is.na(decision_list[[6]])==F] <- decision_list[[6]][is.na(decision_list[[6]])==F]
    decision_list <- decision_list[[1]]
    
    decisions <- na.omit(decision_list)
    
    
    
    
    # ---------------------
    # ----------
    # - Wird in den entsprechenden Textstellen überhaupt über Änderungsanträge abgestimmt?
    # - Wenn nicht, überspringen
    # ----------
    # ---------------------
    if (length(which(is.element(drucksachen, amendmentprop$idDrucksache[amendmentprop$committee==0]))) == 0)
      next
    
    # Success
    #drucksachen_nummer_list[is.na(decision_list)==F] <- decision_list[is.na(decision_list)==F]
    #no_amendments <- which(is.element(drucksachen_list[is.na(decision_list)==T], amendmentprop$idDrucksache[amendmentprop$committee==0]) == F)
    #drucksachen_nummer_list[is.na(decision_list)==T][no_amendments] <- NA
    #drucksachen_nummer_list <- na.omit(drucksachen_nummer_list)
    
    # Success alt
    drucksachen_nummer_list <- c(rbind(drucksachen_nummer_list, decision_list))
    drucksachen_nummer_list <- na.omit(drucksachen_nummer_list)
    
    # Success marrix
    success_matrix <- matrix(NA, ncol=4, nrow=length(drucksachen_nummer))    
    colnames(success_matrix) <- c("idDrucksache", "number", "decision_words", "split")    
    success_matrix[,1] <- drucksachen_nummer    
    
    for (i in 1:nrow(success_matrix)) {
      if (is.element(tail(drucksachen_nummer_list[(which(str_detect(drucksachen_nummer_list, success_matrix[i,1]))+1)], n=1), decision_words)==T)
        success_matrix[i,3] <- tail(drucksachen_nummer_list[(which(str_detect(drucksachen_nummer_list, success_matrix[i,1]))+1)], n=1)
      
      if (is.element(tail(drucksachen_nummer_list[(which(str_detect(drucksachen_nummer_list, success_matrix[i,1]))+1)], n=1), decision_words)==F & is.element(tail(drucksachen_nummer_list[(which(str_detect(drucksachen_nummer_list, success_matrix[i,1]))+2)], n=1), decision_words)==T)
        success_matrix[i,3] <- tail(drucksachen_nummer_list[(which(str_detect(drucksachen_nummer_list, success_matrix[i,1]))+2)], n=1)
      
      
      # Wurden einzelne Vorschläge innerhalb Änderungsantrag getrennt abgestimmt?
      if (length(which(is.element(drucksachen_nummer_list[(which(str_detect(drucksachen_nummer_list, success_matrix[i,1]))+1)], decision_words))) > 1)
        success_matrix[i,4] <- 1 
      if (length(which(is.element(drucksachen_nummer_list[(which(str_detect(drucksachen_nummer_list, success_matrix[i,1]))+1)], decision_words))) == 1)
        success_matrix[i,4] <- 0
      
      
      
      if (str_detect(success_matrix[i,1], "-") == T) {
        success_matrix[i,2] <- sub('.*-', '', success_matrix[i,1])
        success_matrix[i,1] <- sub('-.*', '', success_matrix[i,1])
      } else {
        success_matrix[i,2] <- 1
      }
      
    }
      
    # In amendmentprop übertragen
    for (row in 1:nrow(success_matrix)){
    
      # in amendmentprop übertragen
      if (is.element(success_matrix[row,"idDrucksache"], amendmentprop$idDrucksache) == T) {
      
        amendmentprop_idx <- which(amendmentprop$idDrucksache==success_matrix[row,"idDrucksache"] & amendmentprop$number==success_matrix[row,"number"])
        
        if (length(amendmentprop_idx)==0)
          next
        
        # success = 1
        if(success_matrix[row,"decision_words"]=="zugestimmt" | success_matrix[row,"decision_words"]=="beschlossen" | success_matrix[row,"decision_words"]=="angenommen")
          amendmentprop$success[amendmentprop_idx] <- 1
        
        # success = 0
        if(success_matrix[row,"decision_words"]=="abgelehnt" | success_matrix[row,"decision_words"]=="Ablehnung" | success_matrix[row,"decision_words"]=="erledigt")
          amendmentprop$success[amendmentprop_idx] <- 0
        
        # split
        amendmentprop$split[amendmentprop_idx] <- success_matrix[row,4]
      }
    }
    
  }   
    

}, error = function(e){
  
  protocol_pdf_error[protocol_pdf] <- 1
  
}) # end tryCatch
        
    
} # end of loop over all protocols (PDFs)
    
    
    
    
# ---------------------------
# Manuelle Nachcodierung
# ---------------------------
amendmentprop$success[which(amendmentprop$idDrucksache=="14/1568" & (amendmentprop$number==1 | amendmentprop$number==2))] <- 0
amendmentprop$success[which(amendmentprop$idDrucksache=="14/4907" & (amendmentprop$number==1 | amendmentprop$number==2))] <- c(0,1)
amendmentprop$success[which(amendmentprop$idDrucksache=="14/5208" & (amendmentprop$number==1 | amendmentprop$number==2))] <- 0  
amendmentprop$success[which(amendmentprop$idDrucksache=="14/5364" & amendmentprop$number==9)] <- 0    
amendmentprop$success[which(amendmentprop$idDrucksache=="14/4205" & amendmentprop$number==2)] <- 0    
amendmentprop$success[which(amendmentprop$idDrucksache=="14/7100" & amendmentprop$number==1)] <- 0    
amendmentprop$success[which(amendmentprop$idDrucksache=="15/4978" & amendmentprop$number==15)] <- 0      
amendmentprop$success[which(amendmentprop$idDrucksache=="15/5218" & amendmentprop$number==1)] <- 0          
    


    
    
    
    
    
    
    
    
  