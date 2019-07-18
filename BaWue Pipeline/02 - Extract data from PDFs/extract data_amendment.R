# -------------------------------------------
# Automatically extract data from PDF files
# PDF Type: Amendment Proposals
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
    
    # Delete all text on Entschliessungsantraege
    
    # Identify all rows where any type of proposal starts
    if (textnew[3] == "(Änderungsantrag|Ä n d e r u n g s a n t r a g)") {
      proposal_rows <- grep("Der Landtag wolle beschließen", textnew)
    } else {
      proposal_rows <- grep("(\\b. Änderungsantrag\\b|\\b. Ä n d e r u n g s a n t r a g\\b)", textnew)
    }
    entschliessungs_rows <- grep("(Entschließungsantrag|E n t s c h l i e ß u n g s a n t r a g|A n t r a g)", textnew) 
    begin_rows <- sort(c(proposal_rows,entschliessungs_rows))
    
    if (length(entschliessungs_rows) > 0){
      
      # Identify all rows where any type of proposal ends
      proposalstop_rows <- grep("\\bBegründung\\b", textnew) 
      condition <- textnew[proposalstop_rows] == "Begründung"
      proposalstop_rows <- proposalstop_rows[condition != "FALSE"]
      
      # Identify all raw text from any type of proposal
      all_rows <- sort(c(begin_rows,proposalstop_rows))
      
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
          textnew <- textnew[-seq(entschliessung[i,1],entschliessung[i,2],1)]
        }
        
      } else if (length(entschliessungs_rows) == 1){
        
        textnew <- textnew[-seq(entschliessung[1],entschliessung[2])]
        
      }
      
    } # end if (length(entschliessungs_rows) > 0)
    
    
    
    
    # --------------------
    # 3. Extracting data
    # --------------------
    
    # ----------------------------
    # Table: Amendment Proposals
    # ----------------------------
    
    # idDrucksache
    idDrucksache1 <- sub('.*Drucksache ', '', textnew[1]) # select all to the right of pattern
    
    # datestemp
    datestemp1 <- sub('.*Wahlperiode ', '', textnew[2]) # select all to the right of pattern
    datestemp1 <- trimws(datestemp1, which = "left")
    
    # period
    period1 <- sub('. Wahlperiode.*', '', textnew[2]) # select all to the left of pattern
    
    # Missing: pdfdocu, idVorgang
    
    
    # ---------------------------
    # Table: Amendment Proposal
    # ---------------------------
    
    # idDrucksache
    idDrucksache <- sub('.*Drucksache ', '', textnew[1]) # select all to the right of pattern
    
    # number
    numbers <- rep(NA, length(begin_rows))
    
    for (i in 1: length(begin_rows)){
      numbers[i] <- is.element(begin_rows[i], proposal_rows)
    }
    
    number <- which(numbers, TRUE)
    
    if (textnew[3] == "(Änderungsantrag|Ä n d e r u n g s a n t r a g)") {
      amendmentprop_rows <- grep("Der Landtag wolle beschließen", textnew)
    } else {
      amendmentprop_rows <- grep("(\\b. Änderungsantrag\\b|\\b. Ä n d e r u n g s a n t r a g\\b)", textnew)
    }
    
    # Correct for case in which pdf only contains one amendment proposal without a number, so number and amendmentprop_rows is integer(0)
    if (length(number) == 0 & length(amendmentprop_rows) == 0) {
      number <- 1
      amendmentprop_rows <- grep("Der Landtag wolle beschließen", textnew)
    }
    
    
    # committeeref - Logik: 1 wenn es im Header 3x das Wort Drucksache gibt, 0 wenn es das Wort nur 2x gibt
    docu_header <- paste(textnew[1:amendmentprop_rows[1]], collapse=" ")
    
    if (str_count(docu_header, "\\bDrucksache\\b") == 2) {
      committeeref <- 0 } else if (str_count(docu_header, "\\bDrucksache\\b") == 3){
        committeeref <- 1 } else {
          committeeref <- NA }
    
    if (committeeref == 1){
      
      # idDrucksacheComm - Logik: Ist immer in der Zeile, in der "Drucksache" das zweite Mal vorkommt
      drucksache_rows <- grep("Drucksache", textnew)
      idDrucksacheComm <- sub('.*Drucksache ', '', textnew[drucksache_rows[2]])
      
      # idDrucksacheLegis - Logik: Ist immer in der Zeile, in der "Drucksache" das dritte Mal vorkommt
      idDrucksacheLegis <- sub('.*Drucksache ', '', textnew[drucksache_rows[3]])
      
    } else if (committeeref == 0) {
      
      # idDrucksacheLegis - Logik: Ist immer in der Zeile, in der "Drucksache" das zweite Mal vorkommt
      drucksache_rows <- grep("Drucksache", textnew)
      idDrucksacheLegis <- sub('.*Drucksache ', '', textnew[drucksache_rows[2]])
      
    }
    
    # datestemp 
    datepattern <- "[:digit:]{2}\\. [:digit:]{2}\\. [:digit:]{4}" # all spaces
    datepattern2 <- "[:digit:]{2}\\.[:digit:]{2}\\.[:digit:]{4}" # no spaces
    datepattern3 <- "[:digit:]{2}\\. [:digit:]{2}\\.[:digit:]{4}" # space after dd
    datepattern4 <- "[:digit:]{2}\\.[:digit:]{2}\\. [:digit:]{4}" # space after mm
    
    # nur behalten, wenn nur Datum in Zeile genannt ist, nichts anderes
    datestemp <- str_subset(textnew, datepattern)
    datestemp2 <- str_subset(textnew, datepattern2)
    datestemp3 <- str_subset(textnew, datepattern3)
    datestemp4 <- str_subset(textnew, datepattern4)
    
    datestemp <- c(datestemp, datestemp2, datestemp3, datestemp4)
    
    for (i in 1: length(datestemp)){
      if (nchar(datestemp[i]) > 13) {
        datestemp[i] <- NA
      }
    }
    
    datestemp <- na.omit(datestemp)
    
    
    # committee (direkt aus PDF-Typ impliziert)
    committee <- 0
    
    # rawtext
    textbegin <- grep("Der Landtag wolle beschließen", textnew)
    textbegin <- textbegin + 1
    
    datestemp_rows <- (str_which(textnew[-2], datepattern) + 1)
    datestemp_rows2 <- (str_which(textnew[-2], datepattern2) + 1)
    datestemp_rows3 <- (str_which(textnew[-2], datepattern3) + 1)
    datestemp_rows4 <- (str_which(textnew[-2], datepattern4) + 1)
    datestemp_rows <- c(datestemp_rows, datestemp_rows2, datestemp_rows3, datestemp_rows4)
    
    for (i in 1: length(datestemp_rows)){
      if (nchar(textnew[datestemp_rows[i]]) > 13){
        datestemp_rows[i] <- NA
      }
    }
    
    datestemp_rows <- sort(na.omit(datestemp_rows))
    
    textend <- datestemp_rows - 1
    
    rawtext_left <- as.data.frame(matrix(NA, ncol = 1, nrow = length(amendmentprop_rows)))
    colnames(rawtext_left) <- "rawtext_left"
    rawtext_white <- as.data.frame(matrix(NA, ncol = 1, nrow = length(amendmentprop_rows)))
    colnames(rawtext_white) <- "rawtext_white"
    
    for (i in 1: length(amendmentprop_rows)){
      
      rawtext_left$rawtext_left[i] <- list(textnew[seq(textbegin[i],textend[i],1)])
      rawtext_white$rawtext_white[i] <- list(textnew_white[seq(textbegin[i],textend[i],1)])
      
    }
    
    # idFraction - werden fünf Variablen, für jede Fraktion eine
    if (textnew[3] == "Änderungsantrag" | textnew[3] == "Ä n d e r u n g s a n t r a g"){
      
      proposalheader <- paste(textnew[4:6], collapse=" ")
      
    } else {
      
      amendmentprop_rows <- (grep("(\\b. Änderungsantrag\\b|\\b. Ä n d e r u n g s a n t r a g\\b)", textnew) + 1)
      amendmenthead_stop <- (grep("Der Landtag wolle beschließen", textnew) - 1)
      
      proposalheader <- rep(NA, length(amendmentprop_rows))
      for (i in 1: length(amendmentprop_rows)){
        
        proposalheader[i] <- paste(textnew[amendmentprop_rows[i]:amendmenthead_stop[i]], collapse=" ")
        
      } }
    
    fractions <- c("SPD", "AfD", "CDU", "GRÜNE", "FDP/DVP")
    
    fractionresults <- matrix(NA, nrow=length(proposalheader), ncol=length(fractions))
    colnames(fractionresults) <- fractions
    for (i in 1: length(fractions)){
      for (k in 1: length(proposalheader)){
        fractionresults[k,i] <- str_detect(proposalheader[k], fractions[i])
      }
    }
    
    for (i in 1: length(fractionresults)){
      
      if (fractionresults[i]==TRUE){
        fractionresults[i] <- 1} else if (fractionresults[i]==FALSE){
          fractionresults[i] <- 0}
    } 
    
    # explicitref
    if (textnew[3] == "Änderungsantrag" | textnew[3] == "Ä n d e r u n g s a n t r a g"){
      
      ref_section_start <- grep("Der Landtag wolle beschließen", textnew)  
      ref_section_stop <- ref_section_start + 3  
      ref_section <- paste(textnew[ref_section_start:ref_section_stop], collapse=" ")  
      
      
    } else {
      
      ref_section_stop <- amendmenthead_stop + 3
      ref_section <- rep(NA, length(amendmenthead_stop))
      
      for (i in 1: length(amendmentprop_rows)){
        ref_section[i] <- paste(textnew[amendmenthead_stop[i]:ref_section_stop[i]], collapse=" ")
      }
      
    }
    
    drucksachepattern <- "[:digit:]{2}\\/[:digit:]{4}"
    explicit <- rep(NA, length(amendmentprop_rows))
    explicitref <- rep(NA, length(amendmentprop_rows))
    
    for (i in 1: length(explicit)){
      explicit[i] <- str_detect(ref_section[i], drucksachepattern)
      
      if (explicit[i] == TRUE){
        explicitref[i] <- sub('.*sache ', '', ref_section[i])
        explicitref[i] <- substr(explicitref[i], start = 1, stop = 7)
      } else if (explicit[i] == FALSE){
        explicitref[i] <- "none"
      }
      
      
      # Delete spaces in idDrucksache
      idDrucksache <- gsub(" ","",idDrucksache)
      
      # Copy raw reference if it is referenced to proposal in same document
      if (explicitref[i] == idDrucksache){
        explicitref[i] <- paste(textnew[(amendmenthead_stop[i]+2):(amendmenthead_stop[i]+3)], collapse=" ")
      }
    }
    
    # withfraction
    #proposalstop_rows2 <- grep("\\bBegründung\\b", textnew) 
    #condition <- textnew[proposalstop_rows2] == "Begründung"
    #proposalstop_rows2 <- proposalstop_rows2[condition != "FALSE"]
    
    footer_start <- datestemp_rows + 1
    footer_end <- rep(NA, length(footer_start))
    
    for (i in 1: length(footer_end)){
      if (sum(fractionresults[i,]) == 1) {
        footer_end[i] <- footer_start[i] + 1 } 
      else if (sum(fractionresults[i,]) == 2) {
        footer_end[i] <- footer_start[i] + 3  
      } else if (sum(fractionresults[i,]) == 3) {
        footer_end[i] <- footer_start[i] + 5  
      } else if (sum(fractionresults[i,]) == 4) {
        footer_end[i] <- footer_start[i] + 7  
      } else if (sum(fractionresults[i,]) == 4) {
        footer_end[i] <- footer_start[i] + 9  
      }
    }
    
    proposalfooter <- rep(NA, length(amendmentprop_rows))
    for (i in 1: length(amendmentprop_rows)){
      
      proposalfooter[i] <- paste(textnew[footer_start[i]:footer_end[i]], collapse=" ")
      
    }
    
    withfraction <- str_detect(proposalfooter, "und Fraktion")
    for (i in 1: length(withfraction)){
      
      if (withfraction[i]==TRUE){
        withfraction[i] <- 1} else if (withfraction[i]==FALSE){
          withfraction[i] <- 0}
    }
    
    # members2
    members2 <- proposalfooter 
    
    # members1
    members1 <- rep(NA, length(proposalheader))
    for (i in 1: length(proposalheader)){
      if (str_detect(proposalheader[i], "Fraktion") == FALSE){
        members1[i] <- proposalheader[i]
      } else {
        members1[i] <- "none"
      }
    }
    
    
    
    # ------------------------
    # 4. Store data in table
    # ------------------------
    
    # ----------------------------
    # Table: Amendment Proposals
    # ----------------------------
    
    # what is with pdf-docu-table?
    
    if (z == 1){
      
     
      # ----------------------------
      # Table: Amendment Proposal
      # ----------------------------
      
      # Create empty matrix
      amendmentprop <- as.data.frame(matrix(NA, ncol=24, nrow=length(amendmentprop_rows)))
      colnames(amendmentprop) <- c("idProposal", "idVorgang", "idDrucksache", "number", "idDrucksacheLegis",
                                   "idDrucksacheComm", "explicitref", "datestemp", "committee", "committeeref", "rollcall",
                                   "collcallbev", "SPD", "AfD", "CDU", "GRUENE", "FDP/DVP", "members1",
                                   "members2", "withfraction", "acceptedcommittee", "acceptedplenary", "splitplenary", "error")
      
      if (exists("idDrucksacheComm") == F)
        idDrucksacheComm <- rep(NA, length(amendmentprop_rows))
      
      # Fill with information
      amendmentprop$idDrucksache <- idDrucksache   
      amendmentprop$number <- number
      amendmentprop$idDrucksacheLegis <- idDrucksacheLegis  
      amendmentprop$idDrucksacheComm <- idDrucksacheComm
      amendmentprop$explicitref <- explicitref
      amendmentprop$committee <- committee  
      amendmentprop$committeeref <- committeeref 
      amendmentprop$datestemp <- datestemp
      amendmentprop$members1 <- members1
      amendmentprop$members2 <- members2
      amendmentprop$withfraction <- withfraction
      amendmentprop$SPD <- fractionresults[,"SPD"]  
      amendmentprop$AfD <- fractionresults[,"AfD"]   
      amendmentprop$CDU <- fractionresults[,"CDU"]  
      amendmentprop$GRUENE <- fractionresults[,"GRÜNE"]
      amendmentprop$"FDP/DVP" <- fractionresults[,"FDP/DVP"]
      amendmentprop$error <- 0
      
      amendmentprop <- as.data.frame(cbind(amendmentprop, rawtext_left, rawtext_white))
      
      #rm(list=setdiff(ls(), c("amendmentprop", "amendmentpdf", "pdfs")))
      
    } else {
      
     
      # ----------------------------
      # Table: Amendment Proposal
      # ----------------------------  
      
      # Enlarge dataframe
      newdata <- as.data.frame(matrix(NA, ncol= ncol(amendmentprop) - 2, nrow= length(amendmentprop_rows)))
      colnames(newdata) <- c("idProposal", "idVorgang", "idDrucksache", "number", "idDrucksacheLegis",
                             "idDrucksacheComm", "explicitref", "datestemp", "committee", "committeeref", "rollcall",
                             "collcallbev", "SPD", "AfD", "CDU", "GRUENE", "FDP/DVP", "members1",
                             "members2", "withfraction", "acceptedcommittee", "acceptedplenary", "splitplenary", "error")
      
      newdata$idDrucksache <- idDrucksache
      newdata$number <- number
      newdata$idDrucksacheLegis <- idDrucksacheLegis
      newdata$idDrucksacheComm <- idDrucksacheComm
      newdata$explicitref <- explicitref
      newdata$committee <- committee
      newdata$committeeref <- committeeref 
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
      
      newdata <- as.data.frame(cbind(newdata, rawtext_left, rawtext_white))
      amendmentprop <- as.data.frame(rbind(amendmentprop, newdata))
      
      
      #rm(list=setdiff(ls(), c("amendmentprop", "amendmentpdf", "pdfs")))
      
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

amendment_err <- as.data.frame(matrix(NA, ncol=ncol(amendmentprop), nrow=length(error)))
colnames(amendment_err) <- colnames(amendmentprop)
amendment_err$idDrucksache <- error
if (length(error) > 0){
  amendment_err$error <- 1
}

# Merge dataframes
amendmentprop <- as.data.frame(rbind(amendmentprop, amendment_err))
rm(list=setdiff(ls(), "amendmentprop"))










