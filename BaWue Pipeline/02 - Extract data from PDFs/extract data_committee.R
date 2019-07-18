# -------------------------------------------
# Automatically extract data from PDF files
# PDF Type: Committee Recommendations
# Lion Behrens
# -------------------------------------------

setwd("M:/Parlis complete corpus/corpus_complete/out_dat/test")

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

# Inspect content of one pdf-file
# info <- content(pdfs[2]) # info about content of first pdf
error <- rep(NA, length(pdfs))

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
    textnew <- gsub('[\b]', '', textnew)
    
    # Delete all headers except first page
    
    # Just read out datestemp first b/c otherwise not possible
    datestemp1row <- grep("Ausgegeben:", textnew)[1]  
    datestemp1 <- sub('.*Ausgegeben: ', '', textnew[datestemp1row])
    datestemp1 <- substr(datestemp1, start = 1, stop = 12)
    
    # Identify rows with headers and pagenumbers
    header_rows_prel <- grep("Landtag von Baden-Württemberg", textnew)  
    textnew[header_rows_prel] <- str_wrap(textnew[header_rows_prel], width=50)
    header_rows <- grep("Landtag von Baden-Württemberg Drucksache", textnew)  
    header_rows <- header_rows[-1] # first header row is not part of operation b/c we need this information
    
    if (length(header_rows) > 0) { # only if there is more than one page
      pagenumber_rows <- rep(NA, length(header_rows))
      for (i in 1:length(header_rows)){
        pagenumber_rows[i] <- header_rows[i]-1                       
      } 
      pagenumber_rows <- c(pagenumber_rows, length(textnew)) # add last row manually because no header follows
      deleted_rows <- c(pagenumber_rows[1]-1, pagenumber_rows[1]-2, pagenumber_rows)  
      deleted_rows <- c(header_rows, deleted_rows)
      
      # Delete header row and row before
      textnew <- textnew[-deleted_rows]
    }
    
    # Store text with and without whitespace
    textnew_white <- textnew
    textnew <- trimws(textnew, which = "left")
    
    
    # Delete all text on Entschliessungsantraege
    
    # Identify all rows where any type of proposal starts
    amendmentprop_rows <- grep("\\bÄnderungsantrag\\b", textnew)
    condition <- textnew[amendmentprop_rows] == "Änderungsantrag"
    amendmentprop_rows <- amendmentprop_rows[condition != "FALSE"]
    
    entschliessungs_rows <- grep("Entschließungsantrag", textnew) 
    condition <- textnew[entschliessungs_rows] == "Entschließungsantrag"
    entschliessungs_rows <- entschliessungs_rows[condition != "FALSE"]
    
    entschliessungs_rows2 <- grep("Entschließung", textnew) 
    condition <- textnew[entschliessungs_rows2] == "Entschließung"
    entschliessungs_rows2 <- entschliessungs_rows2[condition != "FALSE"]
    
    entschliessungs_rows <- sort(c(entschliessungs_rows, entschliessungs_rows2))
    
    begin_rows <- sort(c(amendmentprop_rows,entschliessungs_rows))
    
    if (length(entschliessungs_rows) > 0){
      
      # Identify all rows where any type of proposal ends
      datepattern <- "[:digit:]{2}\\. [:digit:]{2}\\. [:digit:]{4}" # all spaces
      datepattern2 <- "[:digit:]{2}\\.[:digit:]{2}\\.[:digit:]{4}" # no spaces
      datepattern3 <- "[:digit:]{2}\\. [:digit:]{2}\\.[:digit:]{4}" # space after dd
      datepattern4 <- "[:digit:]{2}\\.[:digit:]{2}\\. [:digit:]{4}" # space after mm
      
      datestemp_rows <- (str_which(textnew, datepattern))
      datestemp_rows2 <- (str_which(textnew, datepattern2))
      datestemp_rows3 <- (str_which(textnew, datepattern3))
      datestemp_rows4 <- (str_which(textnew, datepattern4))
      datestemp_rows <- c(datestemp_rows, datestemp_rows2, datestemp_rows3, datestemp_rows4)
      datestemp_rows <- datestemp_rows[-c(1,2)]
      
      for (i in 1: length(datestemp_rows)){
        if (nchar(textnew[datestemp_rows[i]]) > 13){
          datestemp_rows[i] <- NA
        }
      }
      
      datestemp_rows <- na.omit(datestemp_rows)
      
      # Identify all raw text from any type of proposal
      all_rows <- sort(c(begin_rows,datestemp_rows))
      
      # Identify begin and end of Entschliessungsanträge
      entschliessung <- matrix(NA, ncol=2, nrow=length(all_rows))
      for (i in 1:length(all_rows)){
        if (is.element(all_rows[i], entschliessungs_rows)==TRUE){
          entschliessung[i,] <- c(all_rows[i], all_rows[i+1])
        } 
      }
      
      entschliessung <- na.omit(entschliessung)
      
      
      if (length(entschliessungs_rows) > 1){
        entschliessung <- apply(entschliessung,2,rev)
        
        # Delete text sections 
        for (i in 1:nrow(entschliessung)){
          textnew <- textnew[-seq(entschliessung[i,1],(entschliessung[i,2]+4),1)]
        }
        
      } else if (length(entschliessungs_rows) == 1){
        
        textnew <- textnew[-seq(entschliessung[1],(entschliessung[2]+4))]
        
      }
      
    } # end if (length(entschliessungs_rows) > 0)  
    
    
    
    
    # Extract sequences of amendment proposals 
    prop_rows <- grep("\\bÄnderungsantrag\\b", textnew)
    condition <- textnew[prop_rows] == "Änderungsantrag"
    prop_rows <- prop_rows[condition != "FALSE"]
    
    prop_end <- grep("\\bBegründung\\b", textnew)
    condition <- textnew[prop_end] == "Begründung" | textnew[prop_end] == "Begründung:"
    prop_end <- prop_end[condition != F]
    if (length(prop_end) == 0 & length(prop_rows) == 1)
      prop_end <- length(textnew)
    
    if (length(prop_rows) > 0) {
      
      all_amend_rows <- sort(c(prop_rows, prop_end))
      all_amend_rows <- matrix(all_amend_rows, ncol=length(prop_rows))
      
      proposaltext <- rep(NA, ncol(all_amend_rows))
      for (i in 1:ncol(all_amend_rows)) {
        proposaltext[i] <- list(textnew[seq(all_amend_rows[1,i],all_amend_rows[2,i])])
      }
      
      proposaltext_white <- rep(NA, ncol(all_amend_rows))
      for (i in 1:ncol(all_amend_rows)) {
        proposaltext_white[i] <- list(textnew_white[seq(all_amend_rows[1,i],all_amend_rows[2,i])])
      }
      
    }
    
    
    # --------------------
    # 3. Extracting data
    # --------------------
    
    # ----------------------------------
    # Table: Committee Recommendations
    # ----------------------------------
    
    # idDrucksache
    idDrucksache1 <- sub('.*Drucksache ', '', textnew[1]) # select all to the right of pattern
    
    # period
    period1 <- sub('. Wahlperiode.*', '', textnew[2]) # select all to the left of pattern
    
    # Missing: pdfdocu, idVorgang
    
    
    # ---------------------------------
    # Table: Committee Recommendation
    # ---------------------------------
    
    # idDrucksache
    idDrucksache <- sub('.*Drucksache ', '', textnew[1]) # select all to the right of pattern
    
    # idDrucksacheLegis - Logik: Ist immer in der Zeile, in der "Drucksache" das zweite Mal vorkommt
    drucksache_rows <- grep("Drucksache", textnew)
    #idDrucksacheLegis <- sub('.*Drucksache ', '', textnew[drucksache_rows[2]])
    idDrucksacheLegis <- str_extract(textnew[drucksache_rows[2]], "[:digit:][:digit:]/[:digit:]+")
    # suggestion
    suggsection_start <- (grep("Beschlussempfehlung", textnew)[2]) + 2
    suggsection_stop <- suggsection_start + 2
    suggsection <- paste(textnew[suggsection_start:suggsection_stop], collapse=" ")
    
    if (str_detect(suggsection, "zuzustimmen") == TRUE){
      suggestion <- 1
    } else 
      if (str_detect(suggsection, "zustimmen") == TRUE){
        suggestion <- 1
      } else 
        if (str_detect(suggsection, "stimmen") == TRUE){
          suggestion <- 1
        } else
          if (str_detect(suggsection, "zuzustimm") == TRUE){
            suggestion <- 1
          } else
            if (str_detect(suggsection, "abzulehnen") == TRUE){
              suggestion <- 0
            } else
              if (str_detect(suggsection, "zulehnen") == TRUE){
                suggestion <- 0
              } else
                if (str_detect(suggsection, "lehnen") == TRUE){
                  suggestion <- 0
                } else
                  if (str_detect(suggsection, "abzuleh") == TRUE){
                    suggestion <- 0
                  } 
    
    # rawtext
    rawtext_start <- (grep("Beschlussempfehlung", textnew)[2]) + 1
    
    rawtext_end1 <- ((grep("Der Berichterstatter", textnew)) - 2)
    rawtext_end2 <- ((grep("Die Berichterstatterin", textnew)) - 2)
    rawtext_end3 <- ((grep("Der Vorsitzende und Berichterstatter", textnew)) - 2)
    rawtext_end4 <- ((grep("Die Vorsitzende und Berichterstatterin", textnew)) - 2)
    rawtext_end <- sort(c(rawtext_end1, rawtext_end2, rawtext_end3,rawtext_end4))[1]
    
    rawtext_left <- list(textnew[seq(rawtext_start,rawtext_end,1)])
    rawtext_white <- list(textnew_white[seq(rawtext_start,rawtext_end,1)])
    
    
    # ---------------------------
    # Table: Amendment Proposal
    # ---------------------------
    amendmentprop_rows <- grep("\\bÄnderungsantrag\\b", textnew)
    condition <- textnew[amendmentprop_rows] == "Änderungsantrag"
    amendmentprop_rows <- amendmentprop_rows[condition != "FALSE"]
    
    
    if (length(amendmentprop_rows) > 0) {
      
      # number
      numbers <- rep(NA, length(begin_rows))
      for (i in 1: length(begin_rows)){
        numbers[i] <- is.element(begin_rows[i], amendmentprop_rows)
      }
      
      number <- which(numbers, TRUE)
      
      
      # committeeref
      committeeref <- 0
      
      
      # withfraction, members2, rawtext_left, rawtext_white, idFraction
      fractions <- c("SPD", "AfD", "CDU", "GRÜNE", "FDP/DVP")
      fractionresults <- matrix(NA, nrow=length(proposaltext), ncol=length(fractions))
      colnames(fractionresults) <- fractions
      
      withfraction <- rep(NA, length(proposaltext))
      members2 <- rep(NA, length(proposaltext))
      members1 <- rep(NA, length(proposaltext))
      
      rawtext_left2 <- as.data.frame(matrix(NA, ncol = 1, nrow = length(amendmentprop_rows)))
      colnames(rawtext_left2) <- "rawtext_left"
      rawtext_white2 <- as.data.frame(matrix(NA, ncol = 1, nrow = length(amendmentprop_rows)))
      colnames(rawtext_white2) <- "rawtext_white"
      
      explicitref <- rep(NA, length(proposaltext))
      
      datestemp <- rep(NA, length(proposaltext))
      
      
      for (k in 1: length(proposaltext)){
        
        # idFraction 
        amendmenthead_stop <- (grep("(zu dem Gesetzentwurf|zum Gesetzentwurf)", proposaltext[[k]]))
        
        #for (i in 1: length(amendmenthead_stop)){
        #  if (nchar(proposaltext[[k]][amendmenthead_stop[i]]) > 50) {
        #    amendmenthead_stop[i] <- NA
        #  }
        #}
        
        amendmenthead_stop <- na.omit(amendmenthead_stop)
        amendmenthead_stop <- amendmenthead_stop - 1
        
        proposalheader <- NA
        proposalheader <- paste(proposaltext[[k]][1:amendmenthead_stop], collapse=" ")
        
        for (i in 1: length(fractions)){
          fractionresults[k,i] <- str_detect(proposalheader, fractions[i])
        }
        
        for (i in 1: ncol(fractionresults)){
          
          if (fractionresults[k,i]==TRUE){
            fractionresults[k,i] <- 1} else if (fractionresults[k,i]==FALSE){
              fractionresults[k,i] <- 0}
        } 
        
        
        
        # datestemp
        datepattern1 <- "[:digit:]{2}\\. [:digit:]{2}\\. [:digit:]{4}" # all spaces
        datepattern2 <- "[:digit:]{2}\\.[:digit:]{2}\\.[:digit:]{4}" # no spaces
        datepattern3 <- "[:digit:]{2}\\. [:digit:]{2}\\.[:digit:]{4}" # space after dd
        datepattern4 <- "[:digit:]{2}\\.[:digit:]{2}\\. [:digit:]{4}" # space after mm
        
        # nur behalten, wenn nur Datum in Zeile genannt ist, nichts anderes
        datestemp1 <- str_subset(proposaltext[[k]], datepattern1)
        datestemp2 <- str_subset(proposaltext[[k]], datepattern2)
        datestemp3 <- str_subset(proposaltext[[k]], datepattern3)
        datestemp4 <- str_subset(proposaltext[[k]], datepattern4)
        
        datestemp_all <- c(datestemp1, datestemp2, datestemp3, datestemp4)
        #datestemp <- datestemp[-c(1,2)]
        
        for (i in 1: length(datestemp_all)){
          if (nchar(datestemp_all[i]) > 13) {
            datestemp_all[i] <- NA
          }
        }
        
        datestemp[k] <- na.omit(datestemp_all)
        
        
        # withfraction
        datestemp_rows <- (str_which(proposaltext[[k]], datepattern1))
        datestemp_rows2 <- (str_which(proposaltext[[k]], datepattern2))
        datestemp_rows3 <- (str_which(proposaltext[[k]], datepattern3))
        datestemp_rows4 <- (str_which(proposaltext[[k]], datepattern4))
        datestemp_rows <- c(datestemp_rows, datestemp_rows2, datestemp_rows3, datestemp_rows4)
        #datestemp_rows <- datestemp_rows[-c(1,2)]
        
        
        for (i in 1: length(datestemp_rows)){
          if (nchar(proposaltext[[k]][datestemp_rows[i]]) > 13){
            datestemp_rows[i] <- NA
          }
        }
        
        datestemp_rows <- sort(na.omit(datestemp_rows))
        
        footer_start <- datestemp_rows + 1
        footer_end <- (length(proposaltext[[k]])) - 1
        if(length(grep("\\Begründung\\b", textnew)) == 0 & length(prop_rows) == 1)
          footer_end <- (length(proposaltext[[k]])) 
        
        proposalfooter <- paste(proposaltext[[k]][footer_start:footer_end], collapse=" ")
        
        withfraction[k] <- str_detect(proposalfooter, "und Fraktion")
        if (withfraction[k]==TRUE) {
          withfraction[k] <- 1 } else if (withfraction[k]==FALSE) {
            withfraction[k] <- 0 }
        
        # members2
        members2[k] <- proposalfooter 
        
        # members1
        if (str_detect(proposalheader, "Fraktion") == FALSE){
          members1[k] <- proposalheader
        } else {
          members1[k] <- "none"
        }
        
        
        # rawtext2
        textbegin <- grep("Der Landtag wolle beschließen", proposaltext[[k]])
        if (length(textbegin) == 0)
          textbegin <- grep("Artikel", proposaltext[[k]])[1]
        textend <- datestemp_rows - 1
        
        rawtext_left2$rawtext_left[k] <- list(proposaltext[[k]][seq(textbegin+1,textend,1)])
        rawtext_white2$rawtext_white[k] <- list(proposaltext_white[[k]][seq(textbegin+1,textend,1)])
        
        # explicitref
        ref_section_stop <- amendmenthead_stop + 8
        ref_section <- NA
        
        ref_section <- paste(proposaltext[[k]][amendmenthead_stop:ref_section_stop], collapse=" ")
        
        
        drucksachepattern1 <- "[:digit:]{2}\\/[:digit:]{4}"
        drucksachepattern2 <- "[:digit:]{2}\\/[:digit:]{3}"
        drucksachepattern3 <- "[:digit:]{2}\\/[:digit:]{2}"
        
        #explicit <- rep(NA, length(amendmentprop_rows))
        #explicitref <- rep(NA, length(amendmentprop_rows))
        
        explicit1 <- str_detect(ref_section, drucksachepattern1)
        explicit2 <- str_detect(ref_section, drucksachepattern2)
        explicit3 <- str_detect(ref_section, drucksachepattern3)
        
        if (explicit1 == TRUE | explicit2 == TRUE | explicit3 == TRUE){
          explicit <- TRUE
        } else {
          explicit <- FALSE
        }
        
        #if (explicit == TRUE){
        # explicitref <- sub('.*sache ', '', ref_section)
        #explicitref[k] <- substr(explicitref, start = 1, stop = 7)
        #  } else if (explicit == FALSE){
        #   explicitref[k] <- "none"
        #}
        
        if (explicit1 == TRUE) {
          explicitref[k] <- str_extract(ref_section, drucksachepattern1)
        } else if (explicit2 == TRUE) {
          explicitref[k] <- str_extract(ref_section, drucksachepattern2)
        } else if (explicit3 == TRUE) {
          explicitref[k] <- str_extract(ref_section, drucksachepattern3)
        } else if (explicit == FALSE){
          explicitref[k] <- "none"
        }
        
        # if explicitref refers to own document, it refers to an earlier amendment proposal
        # in this case, just copy raw text of reference
        # Delete spaces in idDrucksache
        idDrucksache <- gsub(" ","",idDrucksache)
        if (explicitref[k] == idDrucksache){
          explicitref[k] <- paste(proposaltext[[k]][amendmenthead_stop+2:amendmenthead_stop+3], collapse=" ")
        } 
        
      }
      
      
      # members1
      #amendmenthead_stop <- (grep("zu dem Gesetzentwurf", textnew) - 1)
      #if (str_detect(wholetext, "Empfehlung und Bericht") == TRUE){
      #  amendmenthead_stop <- amendmenthead_stop[-c(1,2)] 
      #} else {
      #  amendmenthead_stop <- amendmenthead_stop[-1]
      #}
      
      #for (i in 1: length(amendmentprop_rows)){
      
      # proposalheader[i] <- paste(textnew[(amendmentprop_rows[i]+1):amendmenthead_stop[i]], collapse=" ")
      
      #} 
      
    } # end if length(amendmentprop_rows) > 0
    
    
    
    # ------------------------
    # 4. Store data in table
    # ------------------------
    
    # ----------------------------
    # Table: Amendment Proposals
    # ----------------------------
    
    if (length(datestemp1) == 0)
      datestemp1 <- datestemp
    
    if (z == 1) {
      
    
      # ---------------------------------
      # Table: Committee Recommendation
      # ---------------------------------
      
      # Create empty matrix
      committee <- as.data.frame(matrix(NA, ncol=9, nrow=1))
      colnames(committee) <- c("idRecomm", "idVorgang", "idDrucksache", "idDrucksacheLegis", "idCommittee",
                               "suggestion", "rawtext_left", "rawtext_white", "error")
      
      # Fill with information
      committee$idDrucksache <- idDrucksache  
      committee$idDrucksacheLegis <- idDrucksacheLegis
      #committee$idCommittee <- idCommittee
      committee$suggestion <- suggestion  
      committee$rawtext_left <- rawtext_left
      committee$rawtext_white <- rawtext_white
      committee$error <- 0
      
      # ----------------------------
      # Table: Amendment Proposal
      # ----------------------------
      
      # Create empty matrix
      amendmentpropc <- as.data.frame(matrix(NA, ncol=24, nrow=length(amendmentprop_rows)))
      colnames(amendmentpropc) <- c("idProposal", "idVorgang", "idDrucksache", "number", "idDrucksacheLegis",
                                    "idDrucksacheComm", "explicitref", "datestemp", "committee", "committeeref", "rollcall",
                                    "collcallbev", "SPD", "AfD", "CDU", "GRUENE", "FDP/DVP", "members1",
                                    "members2", "withfraction", "acceptedcommittee", "acceptedplenary", "splitplenary", "error")
      
      if (length(amendmentprop_rows) > 0) {
        # Fill with information
        amendmentpropc$idDrucksache <- idDrucksache   
        amendmentpropc$number <- number
        amendmentpropc$idDrucksacheLegis <- idDrucksacheLegis  
        amendmentpropc$idDrucksacheComm <- NA
        amendmentpropc$explicitref <- explicitref
        amendmentpropc$committee <- 1
        amendmentpropc$committeeref <- NA 
        amendmentpropc$datestemp <- datestemp
        amendmentpropc$members1 <- members1
        amendmentpropc$members2 <- members2
        amendmentpropc$withfraction <- withfraction
        amendmentpropc$SPD <- fractionresults[,"SPD"]  
        amendmentpropc$AfD <- fractionresults[,"AfD"]   
        amendmentpropc$CDU <- fractionresults[,"CDU"]  
        amendmentpropc$GRUENE <- fractionresults[,"GRÜNE"]
        amendmentpropc$"FDP/DVP" <- fractionresults[,"FDP/DVP"]
        amendmentpropc$error <- 0
        
        amendmentpropc <- as.data.frame(cbind(amendmentpropc, rawtext_left2, rawtext_white2))
        
        rm(list=setdiff(ls(), c("committeepdf", "committee", "amendmentpropc", "pdfs", "error")))
        
      } # end if (length(amendmentprop_rows) > 0)
      
    } else {
      
      
     
      # ---------------------------------
      # Table: Committee Recommendation
      # ---------------------------------
      
      # Create empty matrix
      newdata <- as.data.frame(matrix(NA, ncol=ncol(committee), nrow=1))
      colnames(newdata) <- colnames(committee)  
      
      # Fill with information
      newdata$idDrucksache <- idDrucksache  
      newdata$idDrucksacheLegis <- idDrucksacheLegis
      #newdata$idCommittee <- idCommittee
      newdata$suggestion <- suggestion  
      newdata$rawtext_left <- rawtext_left
      newdata$rawtext_white <- rawtext_white
      newdata$error <- 0
      
      committee <- as.data.frame(rbind(committee, newdata))
      
      if (length(amendmentprop_rows) > 0) {
        
        # ----------------------------
        # Table: Amendment Proposal
        # ----------------------------  
        
        # Enlarge dataframe
        newdata <- as.data.frame(matrix(NA, ncol= 23, nrow= length(amendmentprop_rows)))
        colnames(newdata) <- c("idProposal", "idVorgang", "idDrucksache", "number", "idDrucksacheLegis",
                               "idDrucksacheComm", "explicitref", "datestemp", "committee", "committeeref", "rollcall",
                               "collcallbev", "SPD", "AfD", "CDU", "GRUENE", "FDP/DVP", "members1",
                               "members2", "withfraction", "acceptedcommittee", "acceptedplenary", "splitplenary")
        
        newdata$idDrucksache <- idDrucksache
        newdata$number <- number
        newdata$idDrucksacheLegis <- idDrucksacheLegis
        newdata$idDrucksacheComm <- NA
        newdata$explicitref <- explicitref
        newdata$committee <- 1
        newdata$committeeref <- NA 
        newdata$datestemp <- datestemp
        newdata$members1 <- members1
        newdata$members2 <- members2
        newdata$withfraction <- withfraction
        newdata$SPD <- fractionresults[,"SPD"]  
        newdata$AfD <- fractionresults[,"AfD"] 
        newdata$CDU <- fractionresults[,"CDU"] 
        newdata$GRUENE <- fractionresults[,"GRÜNE"]
        newdata$"FDP/DVP" <- fractionresults[,"FDP/DVP"]
        newdata$error <- 0
        
        newdata <- as.data.frame(cbind(newdata, rawtext_left2, rawtext_white2))
        amendmentpropc <- as.data.frame(rbind(amendmentpropc, newdata))
        
        #rm(list=setdiff(ls(), c("committeepdf", "committee", "amendmentprop", "pdfs", "error")))
        
      } # end if (length(amendmentprop_rows) > 0)
    }
    
    
  }, error = function(e){
    
    error[z] <<- sub('.*Drucksache ', '', textnew[1])
    
  }) # end tryCatch
  
} # end for loop over all pdfs 



# ---------------
# Error control
# ---------------

# Create error vector
error <- na.omit(error)

committee_err <- as.data.frame(matrix(NA, ncol=ncol(committee), nrow=length(error)))
colnames(committee_err) <- colnames(committee)
committee_err$idDrucksache <- error
committee_err$error <- 1

amendment_err <- as.data.frame(matrix(NA, ncol=ncol(amendmentpropc), nrow=length(error)))
colnames(amendment_err) <- colnames(amendmentpropc)
amendment_err$idDrucksache <- error
amendment_err$error <- 1

# Merge dataframes
committee <- as.data.frame(rbind(committee, committee_err))
amendmentpropc <- as.data.frame(rbind(amendmentpropc, amendment_err))

rm(list=setdiff(ls(), c("committee", "amendmentpropc")))




