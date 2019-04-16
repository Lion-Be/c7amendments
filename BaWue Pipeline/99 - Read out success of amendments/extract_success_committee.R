# ----------------------------------------------------------
# -----------------------------------------------------
# Erfolg von Änderungsanträgen im Ausschuss auslesen
# -----------------------------------------------------
# ----------------------------------------------------------

setwd("M:/Parlis complete corpus/corpus_complete/PDF Beschlussempfehlungen")

library(tm)
library(stringr)

amendmentprop$success <- NA

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


committee_pdf_error <- rep(NA, length(files))
for (committee_pdf in 1:length(files)) {
  
  tryCatch({


# --------------------------------------
# Clean metacharacters and whitespace
# --------------------------------------
    
content <- content(pdfs[[committee_pdf]]) # content of first pdf, separated by page

# Merge elements into joint string
#joint <- paste(content, collapse="")

# Store text by row
text <- strsplit(content, split="\n")


# --------------
# For page 1
# --------------
text_clean <- text[[1]]
text_clean <- gsub('[\r]', '', text_clean)
text_clean <- trimws(text_clean, "left")
idDrucksache <- sub('.*Drucksache ', '', text_clean[1]) # to assign success to amendmentprop table
idDrucksache <- gsub(" ", "", idDrucksache)
text_clean <- text_clean[-grep("^Landtag von Baden-Württemberg...", text_clean)]
text_clean <- text_clean[-grep("^Ausgegeben[:punct:]", text_clean)]

page_text <- text_clean

# ----------------------
# For every other page
# ----------------------

for (page in 2:length(text)) {
  
  text_clean <- text[[page]]
  text_clean <- gsub('[\r]', '', text_clean)
  text_clean <- trimws(text_clean, "left")
  text_clean <- text_clean[-grep("^Landtag von Baden-Württemberg...", text_clean)]
  
  # Clip page to prior page(s)
  page_text <- c(page_text, text_clean)
  
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

bericht_beginn <- which(str_detect(page_text, "^Bericht$"))[1]

# datestemp
datepattern1 <- "[:digit:]{2}\\. [:digit:]{2}\\. [:digit:]{4}" # all spaces
datepattern2 <- "[:digit:]{2}\\.[:digit:]{2}\\.[:digit:]{4}" # no spaces
datepattern3 <- "[:digit:]{2}\\. [:digit:]{2}\\.[:digit:]{4}" # space after dd
datepattern4 <- "[:digit:]{2}\\.[:digit:]{2}\\. [:digit:]{4}" # space after mm

date_lines <- which(str_detect(page_text, datepattern1) |
                    str_detect(page_text, datepattern1) |
                    str_detect(page_text, datepattern1) |
                    str_detect(page_text, datepattern1))

bericht_ende <- date_lines[which((date_lines-bericht_beginn) == min((date_lines - bericht_beginn)[(date_lines - bericht_beginn)>0]))]

bericht_text <- page_text[bericht_beginn:bericht_ende]

if (length(which(str_detect(bericht_text, "Einzelabstimmung")) > 0))
    bericht_text <- bericht_text[which(str_detect(bericht_text, "Einzelabstimmung")):length(bericht_text)]

if (length(which(str_detect(bericht_text, "Einzelberatung")) > 0))
  bericht_text <- bericht_text[which(str_detect(bericht_text, "Einzelberatung")):length(bericht_text)]


# Änderungsanträge
aenderungsantr <- str_extract(bericht_text, "(Änderungsantrag Nr. [:digit:]+|Änderungsantrags Nr. [:digit:]+|Änderungsantrag|Änderungsantrags)")
aenderungsantr_idx <- which(str_detect(bericht_text, "(Änderungsantrag Nr. [:digit:]+|Änderungsantrags Nr. [:digit:]+|Änderungsantrag|Änderungsantrags)"))

# Decisions
decision_words <- c("angenommen", "zugestimmt", "beschlossen", "abgelehnt", "abzulehnen", "Ablehnung")
decision_idx <- which(str_detect(bericht_text, decision_words[1]) |
                        str_detect(bericht_text, decision_words[2]) |
                        str_detect(bericht_text, decision_words[3]) |
                        str_detect(bericht_text, decision_words[4]) |
                        str_detect(bericht_text, decision_words[5]) |
                        str_detect(bericht_text, decision_words[6])) 

decision_list <- list()
for (i in 1:length(decision_words))
  decision_list[[i]] <- str_extract(bericht_text, decision_words[i])

decision_list[[1]][is.na(decision_list[[2]])==F] <- decision_list[[2]][is.na(decision_list[[2]])==F]
decision_list[[1]][is.na(decision_list[[3]])==F] <- decision_list[[3]][is.na(decision_list[[3]])==F]
decision_list[[1]][is.na(decision_list[[4]])==F] <- decision_list[[4]][is.na(decision_list[[4]])==F]
decision_list[[1]][is.na(decision_list[[5]])==F] <- decision_list[[5]][is.na(decision_list[[5]])==F]
decision_list[[1]][is.na(decision_list[[6]])==F] <- decision_list[[6]][is.na(decision_list[[6]])==F]
decisions <- decision_list[[1]]

# Success
joint_success <- c(rbind(aenderungsantr, decisions))
joint_success <- na.omit(joint_success)

# Success matrix
success_matrix <- matrix(NA, ncol=3, nrow=length(unique(na.omit(aenderungsantr))))   
colnames(success_matrix) <- c("number", "decision_words", "split")    
success_matrix[,1] <- unique(na.omit(aenderungsantr))   
#success_matrix <- unique(success_matrix)

for (i in 1:nrow(success_matrix)) {
  
  if (length(which(str_detect(success_matrix[,"number"], "Nr")) > 0))
      if (success_matrix[i,"number"] == "Änderungsantrag") {
        success_matrix[i,"number"] <- NA
        next
      }
  # Ausgang der Abstimmung  
  if (is.element(tail(joint_success[(which(str_detect(joint_success, success_matrix[i,1]))+1)], n=1), decision_words)==T)
    success_matrix[i,2] <- tail(joint_success[(which(str_detect(joint_success, str_c(success_matrix[i,1], "\\b")))+1)], n=1)
  
  # Wurden einzelne Vorschläge innerhalb Änderungsantrag getrennt abgestimmt?
  if (length(which(is.element(joint_success[(which(str_detect(joint_success, success_matrix[i,1]))+1)], decision_words))) > 1)
    success_matrix[,3] <- 1 
  if (length(which(is.element(joint_success[(which(str_detect(joint_success, success_matrix[i,1]))+1)], decision_words))) == 1)
    success_matrix[,3] <- 0
  
}
success_matrix <- na.omit(success_matrix)


for (i in 1:nrow(success_matrix)) {
  if (str_detect(success_matrix[i,1], "[:digit:]") == T) {
    success_matrix[i,1] <- str_extract(success_matrix[i,1], "[:digit:]+")
  } else {
    success_matrix[i,1] <- 1
  }
}
success_matrix <- unique(success_matrix)


# In amendmentprop übertragen
for (row in 1:nrow(success_matrix)){
  
  # in amendmentprop übertragen
  if (is.element(idDrucksache, amendmentprop$idDrucksache) == T) {
    
    amendmentprop_idx <- which(amendmentprop$idDrucksache==idDrucksache & amendmentprop$number==success_matrix[row,"number"])
    
    if (length(amendmentprop_idx)==0)
      next
    
    # success = 1
    if(success_matrix[row,"decision_words"]=="zugestimmt" | success_matrix[row,"decision_words"]=="beschlossen" | success_matrix[row,"decision_words"]=="angenommen")
      amendmentprop$success[amendmentprop_idx] <- 1
    
    # success = 0
    if(success_matrix[row,"decision_words"]=="abgelehnt" | success_matrix[row,"decision_words"]=="abzulehnen" | success_matrix[row,"decision_words"]=="Ablehnung")
      amendmentprop$success[amendmentprop_idx] <- 0
    
    # split
    amendmentprop$split[amendmentprop_idx] <- success_matrix[row,3]
  }
}


  

}, error = function(e){
    
  committee_pdf_error[committee_pdf] <- 1
    
  }) # end tryCatch
  
  
} # end of loop over all protocols (PDFs)



# ---------------------------
# Manuelle Nachcodierung
# ---------------------------
amendmentprop$success[which(amendmentprop$idDrucksache=="14/2957" & amendmentprop$number==2)] <- 0
amendmentprop$success[which(amendmentprop$idDrucksache=="14/2957" & amendmentprop$number==5)] <- NA
amendmentprop$success[which(amendmentprop$idDrucksache=="14/3770" & amendmentprop$number==1)] <- 0
amendmentprop$success[which(amendmentprop$idDrucksache=="14/3770" & amendmentprop$number==2)] <- 0
amendmentprop$success[which(amendmentprop$idDrucksache=="15/4452" & amendmentprop$number==1)] <- 1
amendmentprop$success[which(amendmentprop$idDrucksache=="14/3284" & amendmentprop$number==1)] <- 0
amendmentprop$intra_coal[which(amendmentprop$idDrucksache=="14/2087" & amendmentprop$number==1)] <- NA  
amendmentprop$success[which(amendmentprop$idDrucksache=="16/1974" & amendmentprop$number==1)] <- 0  
amendmentprop$success[which(amendmentprop$idDrucksache=="16/2738" & amendmentprop$number==1)] <- 0  
amendmentprop$success[which(amendmentprop$idDrucksache=="16/5262" & amendmentprop$number==1)] <- 0  
amendmentprop$success[which(amendmentprop$idDrucksache=="16/5262" & amendmentprop$number==2)] <- 1 
amendmentprop$success[which(amendmentprop$idDrucksache=="15/3724" & amendmentprop$number==2)] <- 0
amendmentprop$split[which(amendmentprop$idDrucksache=="15/3724" & amendmentprop$number==2)] <- 1






