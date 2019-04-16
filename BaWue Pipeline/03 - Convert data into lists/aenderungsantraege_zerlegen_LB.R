# ---------------------------------------
# SFB 884, Projekt C7
# Aenderungsantraege zerlegen
# Lion Behrens
# ---------------------------------------
library(stringr)

amendmentprop$amendment_list <- NA
amendmentprop$error_list <- 0

for (amendmentprop_row in 1: nrow(amendmentprop)){
  
  roman_match <- NA
  nummer_match <- NA
  buchstaben_match <- NA
  
  tryCatch({
  
  amend_text <- amendmentprop$rawtext_left[amendmentprop_row]
  amend_text_white <- amendmentprop$rawtext_white[amendmentprop_row]
  
  amendment_list <- amend_text
  amendment_list_white <- amend_text_white
  
  
  #----------------------------------
  # Römische Ziffern (Ebene 1)
  #----------------------------------
  roman_match <- c(which(str_detect(amend_text[[1]], "^I[:punct:]"), T), 
                   which(str_detect(amend_text[[1]], "^II[:punct:]"), T), 
                   which(str_detect(amend_text[[1]], "^III[:punct:]"), T), 
                   which(str_detect(amend_text[[1]], "^IV[:punct:]"), T), 
                   which(str_detect(amend_text[[1]], "^V[:punct:]"), T),
                   which(str_detect(amend_text[[1]], "^VI[:punct:]"), T),
                   which(str_detect(amend_text[[1]], "^VII[:punct:]"), T),
                   which(str_detect(amend_text[[1]], "^VIII[:punct:]"), T),
                   which(str_detect(amend_text[[1]], "^IX[:punct:]"), T),
                   which(str_detect(amend_text[[1]], "^X[:punct:]"), T))
  
  # Wenn es keine römische Ziffern gibt, gesamter Aenderungsantrag wird als eine Ziffer wahrgenommen
  # Wenn es römische Ziffern gibt, in Ziffern aufsplitten
  if (length(roman_match) == 0 ) {
   # keine Ebene fuer roemische Ziffern anlegen
  } else {
    roman_sections <- c(roman_match, lengths(amend_text) +1)
    if (roman_sections[1]!=1) {
      roman_sections <- c(1, roman_sections)
      line_before_roman_I <- T
    } else { line_before_roman_I <- F }
  
  for(i in 1: (length(roman_sections) - 1)) {
    amendment_list[[i]] <- amend_text[[1]][roman_sections[i]:(roman_sections[i + 1] - 1)]
    amendment_list_white[[i]] <- amend_text_white[[1]][roman_sections[i]:(roman_sections[i + 1] - 1)]
  }
  
  if (line_before_roman_I == T){
    names(amendment_list) <- c("Nummer_fehlt", str_c("Roemisch_", 1:(length(amendment_list)-1)))
    names(amendment_list_white) <- c("Nummer_fehlt", str_c("Roemisch_", 1:(length(amendment_list_white)-1)))
  } else {
    names(amendment_list) <- str_c("Roemisch_", 1:length(amendment_list))
    names(amendment_list_white) <- str_c("Roemisch_", 1:length(amendment_list_white))
  }
  
  }
  
  #----------------------------------
  # Arabische Ziffern (Ebene 2)
  #----------------------------------
  
  ## Nummer-Zeilen (Ziffer(n) am Anfang der Zeile plus Punkt) 
  nummer_match <- lapply(amend_text, str_detect, pattern = "^\\d+?\\.") # LB: ohne Leerzeichen um robust bei Platzhalter-Fehlern zu sein 
  
  ## Zeilen mit einem Monat aussortieren
  monate <- c(
    "Januar",
    "Februar",
    "März",
    "April",
    "Mai",
    "Juni",
    "Juli",
    "August",
    "September",
    "Oktober",
    "November",
    "Dezember"
  )
  
  
  for(i in 1: lengths(nummer_match)) {
    if(str_detect(amend_text[[1]][i], str_c("^\\d{1,2}\\. ", monate, collapse = "|"))){
      nummer_match[[1]][i] <- F
    }
  }
  
  
  # Alles innerhalb von texteigenen Anfuehrungsstrichen aussortieren
  quote_start <- rep(NA, lengths(amend_text))
  quote_end <- rep(NA, lengths(amend_text))
  
  # Wo beginnen Zitationen?
  for(i in 1: length(quote_start)) {
    if(str_detect(amend_text[[1]][i], pattern="„") == T) 
      quote_start[i] <- T
  }
  
  # Wo enden Zitationen?
  for(i in 1: length(quote_end)) {
    if(str_detect(amend_text[[1]][i], pattern="“") == T) 
      quote_end[i] <- T
  }
  
  start_lines <- which(quote_start, T)
  end_lines <- which(quote_end, T)
  
  # Sonderfall beruecksichtigen: Innerhalb einer Zeile wird nur ein bestimmtes Wort zitiert, dann Zeile nicht aussortieren
  if (length(start_lines) > 0) {
  
  for (i in 1: length(start_lines)) {
    if (is.element(start_lines[i], end_lines) == T) {
      
      if (sum(str_count(amend_text[[1]][start_lines[i]], c("„", "“"))) %% 2 != 0)
        end_lines[(match(start_lines[i], end_lines))+1] <- NA
      
      end_lines[match(start_lines[i], end_lines)] <- NA
      start_lines[i] <- NA
    }
  }
  
  }
  quote_lines <- c(sort(c(start_lines, end_lines)), NA) # NA is needed for following operation
   
   
    # Sonderfall beruecksichtigen: Abgeordnete vergessen Anfuehrungszeichen zu schliessen
    
    # if the pattern in quote_lines is not: start, start, close, close, then just delete the second start
    # I don't have to correct for not opening quotations b/c that just never happens
    
    # can only be the case if quote_lines has uneven amount of elements without NA, so an even amount with NA
    if (length(quote_lines) %% 2 == 0 & length(quote_lines) > 2) {  
  
      for (i in 1: (length(quote_lines) - 3)) {
        if( (is.element(quote_lines[i], start_lines) & is.element(quote_lines[i+1], start_lines) == T ) &  
           (is.element(quote_lines[i+2], end_lines) & is.element(quote_lines[i+3], end_lines) == F ) ) {
          
          quote_lines[i+1] <- NA
        }
      }
    }
    
    quote_lines <- na.omit(quote_lines)
    quote_lines <- matrix(quote_lines, nrow = (length(quote_lines)/2), ncol= 2, byrow = T)
    
    if (length(quote_lines) > 0){
    
      for (i in 1: nrow(quote_lines)) {
        nummer_match[[1]][quote_lines[i,1]:quote_lines[i,2]] <- FALSE
      }
    }
  
    nummer_match_list <- amendment_list
    for (roemisch in 1: length(nummer_match_list)) {
      for (i in 1: length(nummer_match_list[[roemisch]])) {
        nummer_match_list[[roemisch]][i] <- NA
      }
    }
    
    
    
    for (roemisch in 1: length(nummer_match_list)) {
      nummer_match_list[[roemisch]] <- nummer_match[[1]][1:length(nummer_match_list[[roemisch]])]
      
      for (i in 1: length(nummer_match_list[[roemisch]])){
        nummer_match[[1]][i] <- NA
      }
      
      nummer_match[[1]] <- na.omit(nummer_match[[1]])
    }
    
    
    
  # Aenderungsvorschlaege innerhalb eines Antrags in Liste zerlegen
    
    ## Nummern in die Liste zerlegen
    for(roemisch in 1:length(amendment_list)){
      
      ## Gibt es überhaupt Nummer? 
      ## Wenn nicht, keine weitere Ebene anlegen
      if(sum(nummer_match_list[[roemisch]]) == 0){
        next
      }
      
      material <- amendment_list[[roemisch]]
      material_white <- amendment_list_white[[roemisch]]
      zeilen <- which(nummer_match_list[[roemisch]])
      
      ## Liste leeren im Eintrag leeren für die weitere Unterteilung
      amendment_list[[roemisch]] <- list()
      amendment_list_white[[roemisch]] <- list()
      
      ## Labels generieren
      nummer_label <- str_c("Nummer_", str_extract(material, "^\\d+")[zeilen])
      
      ## Erste und letzte Zeile hinzufügen, falls sie fehlen
      if(zeilen[1] != 1){
        zeilen <- c(1, zeilen)
        nummer_label <- c("Nummer_fehlt", nummer_label)
      }
      if(zeilen[length(zeilen)] != length(material)){
        zeilen <- c(zeilen, (length(material) + 1))
      }else{
        zeilen <- c(zeilen, zeilen[length(zeilen)])
        zeilen[length(zeilen)] <- zeilen[length(zeilen)] + 1
      }
      
      ## Listeneinträge auf Ebene 2 erstellen
      for(i in 1:(length(zeilen) - 1)){
        
        amendment_list[[roemisch]][[i]] <- material[zeilen[i]:(zeilen[i + 1] - 1)]
        amendment_list_white[[roemisch]][[i]] <- material_white[zeilen[i]:(zeilen[i + 1] - 1)]
        
      }
      
      ## Beschriften
      names(amendment_list[[roemisch]]) <- nummer_label
      names(amendment_list_white[[roemisch]]) <- nummer_label
      
    }
    
    
    
  #----------------------------------
  # Buchstaben (Ebene 3)
  #----------------------------------
  
  for(roemisch in 1:length(amendment_list)){
    
    buchstaben_match <- lapply(amendment_list[[roemisch]], str_detect, pattern = "(^|^...)[[:lower:]]\\)")
    
    # Wenn es keine Nummern aber Buchstaben gibt, Ebene Nummer_fehlt anlegen
    if (length(which(is.element(nummer_match_list, T), T)) == 0 & length(which(is.element(buchstaben_match, T), T)) > 0) {
      amendment_list <- list(amendment_list[[1]][1:(which(buchstaben_match==T)[1]-1)], amendment_list[[1]][which(buchstaben_match==T)[1]:length(amendment_list[[1]])])
      amendment_list_white <- list(amendment_list_white[[1]][1:(which(buchstaben_match==T)[1]-1)], amendment_list_white[[1]][which(buchstaben_match==T)[1]:length(amendment_list_white[[1]])])
      names(amendment_list) <- c("", "Nummer_fehlt")
      names(amendment_list_white) <- c("", "Nummer_fehlt")
      amendment_list <- list(amendment_list)
      amendment_list_white <- list(amendment_list_white)
      buchstaben <- T
    }
    
    buchstaben_match <- lapply(amendment_list[[roemisch]], str_detect, pattern = "(^|^...)[[:lower:]]\\)")
    
    
    ## Alles in texteigenen Anführungszeichen aussortieren
    if (length(quote_lines) > 0){
      
      for (i in 1: nrow(quote_lines)) {
        buchstaben_match[quote_lines[i,1]:quote_lines[i,2]] <- FALSE
      }
    }
    
    ## Buchstaben in die Liste zerlegen
    if (length(which(is.element(nummer_match_list, T), T)) == 0 & length(which(is.element(buchstaben_match, T), T)) > 0) {
      
      nummer <- 2
      
      if(sum(buchstaben_match[[nummer]]) > 0){
        
        material <- amendment_list[[roemisch]][[nummer]]
        material_white <- amendment_list_white[[roemisch]][[nummer]]
        zeilen <- which(buchstaben_match[[nummer]])
        
        ## Liste leeren im Eintrag leeren für die weitere Unterteilung
        amendment_list[[roemisch]][[nummer]] <- list()
        amendment_list_white[[roemisch]][[nummer]] <- list()
        
        ## Labels generieren
        buchstabe_label <- str_c("Buchstabe_", str_extract(material, "[[:lower:]]\\)")[zeilen])
        buchstabe_label <- substr(buchstabe_label, 1, nchar(buchstabe_label)-1)
        
        ## Erste und letzte Zeile hinzufügen, falls sie fehlen
        if(zeilen[1] != 1){
          zeilen <- c(1, zeilen)
          buchstabe_label <- c("Buchstabe_fehlt", buchstabe_label)
        }
        if(zeilen[length(zeilen)] != length(material)){
          zeilen <- c(zeilen, (length(material) + 1))
        }else{
          zeilen <- c(zeilen, zeilen[length(zeilen)])
          zeilen[length(zeilen)] <- zeilen[length(zeilen)] + 1
        }
        
        ## Listeneinträge auf Ebene 3 erstellen
        for(i in 1:(length(zeilen) - 1)){
          
          amendment_list[[roemisch]][[nummer]][[i]] <- material[zeilen[i]:(zeilen[i + 1] - 1)]
          amendment_list_white[[roemisch]][[nummer]][[i]] <- material_white[zeilen[i]:(zeilen[i + 1] - 1)]
          
        }
        
        ## Beschriften
        names(amendment_list[[roemisch]][[nummer]]) <- buchstabe_label
        names(amendment_list_white[[roemisch]][[nummer]]) <- buchstabe_label
        
      }
    
    } else {
    
    ## Buchstaben in die Liste zerlegen
    for(nummer in 1:length(buchstaben_match)){
   
      ## Gibt es überhaupt Buchstaben?
      ## Wenn nicht, keine weitere Ebene anlegen
      if(sum(buchstaben_match[[nummer]]) > 0){
        
        material <- amendment_list[[roemisch]][[nummer]]
        material_white <- amendment_list_white[[roemisch]][[nummer]]
        zeilen <- which(buchstaben_match[[nummer]])
        
        ## Liste leeren im Eintrag leeren für die weitere Unterteilung
        amendment_list[[roemisch]][[nummer]] <- list()
        amendment_list_white[[roemisch]][[nummer]] <- list()
        
        ## Labels generieren
        buchstabe_label <- str_c("Buchstabe_", str_extract(material, "[[:lower:]]\\)")[zeilen])
        buchstabe_label <- substr(buchstabe_label, 1, nchar(buchstabe_label)-1)
        
        ## Erste und letzte Zeile hinzufügen, falls sie fehlen
        if(zeilen[1] != 1){
          zeilen <- c(1, zeilen)
          buchstabe_label <- c("Buchstabe_fehlt", buchstabe_label)
        }
        if(zeilen[length(zeilen)] != length(material)){
          zeilen <- c(zeilen, (length(material) + 1))
        }else{
          zeilen <- c(zeilen, zeilen[length(zeilen)])
          zeilen[length(zeilen)] <- zeilen[length(zeilen)] + 1
        }
        
        ## Listeneinträge auf Ebene 3 erstellen
        for(i in 1:(length(zeilen) - 1)){
            
          amendment_list[[roemisch]][[nummer]][[i]] <- material[zeilen[i]:(zeilen[i + 1] - 1)]
          amendment_list_white[[roemisch]][[nummer]][[i]] <- material_white[zeilen[i]:(zeilen[i + 1] - 1)]
          
        }
          
        ## Beschriften
        names(amendment_list[[roemisch]][[nummer]]) <- buchstabe_label
        names(amendment_list_white[[roemisch]][[nummer]]) <- buchstabe_label
        
      }
     
    }
      
    }
  }
    
  
  
  #----------------------------------
  # Buchstabe/Buchstabe (Ebene 4)
  #----------------------------------
  
  ## Buchstaben/Buchstaben-Zeilen (Buchstabe/Buchstabe am Anfang der Zeile plus ")")
  for(roemisch in 1:length(amendment_list)){
    
    for(nummer in 1:length(amendment_list[[roemisch]])){
      
      ## Gibt es überhaupt Buchstaben?
      ## Wenn nicht, weiter
      
      if(class(amendment_list[[roemisch]][[nummer]]) == "character"){
        next
      }
      
      buchstaben_match <- lapply(amendment_list[[roemisch]][[nummer]], str_detect, pattern = "^[[:lower:]]{2}\\)")
      
      ## Alles in texteigenen Anführungszeichen aussortieren
      if (length(quote_lines) > 0){
        
        for (i in 1: nrow(quote_lines)) {
          buchstaben_match[quote_lines[i,1]:quote_lines[i,2]] <- FALSE
        }
      }
      
      ## Buchstaben in die Liste zerlegen
      for(buchstabe_l1 in 1:length(buchstaben_match)){
        
        ## Gibt es überhaupt Buchstaben?
        ## Wenn nicht, keine weitere Ebene anlegen
        if(sum(buchstaben_match[[buchstabe_l1]]) > 0){
          
          material <- amendment_list[[roemisch]][[nummer]][[buchstabe_l1]]
          material_white <- amendment_list_white[[roemisch]][[nummer]][[buchstabe_l1]]
          zeilen <- which(buchstaben_match[[buchstabe_l1]])
          
          ## Liste leeren im Eintrag leeren für die weitere Unterteilung
          amendment_list[[roemisch]][[nummer]][[buchstabe_l1]] <- list()
          amendment_list_white[[roemisch]][[nummer]][[buchstabe_l1]] <- list()
          
          ## Labels generieren
          buchstabe_label <- str_c("Buchstabe_", str_extract(material, "^\\w{2}")[zeilen])
          
          ## Erste und letzte Zeile hinzufügen, falls sie fehlen
          if(zeilen[1] != 1){
            zeilen <- c(1, zeilen)
            buchstabe_label <- c("Buchstabe_fehlt", buchstabe_label)
          }
          if(zeilen[length(zeilen)] != length(material)){
            zeilen <- c(zeilen, (length(material) + 1))
          }else{
            zeilen <- c(zeilen, zeilen[length(zeilen)])
            zeilen[length(zeilen)] <- zeilen[length(zeilen)] + 1
          }
          
          ## Listeneinträge auf Ebene 4 erstellen
          for(i in 1:(length(zeilen) - 1)){
            
            amendment_list[[roemisch]][[nummer]][[buchstabe_l1]][[i]] <- material[zeilen[i]:(zeilen[i + 1] - 1)]
            amendment_list_white[[roemisch]][[nummer]][[buchstabe_l1]][[i]] <- material_white[zeilen[i]:(zeilen[i + 1] - 1)]
            
          }
          
          ## Beschriften
          names(amendment_list[[roemisch]][[nummer]][[buchstabe_l1]]) <- buchstabe_label
          names(amendment_list_white[[roemisch]][[nummer]][[buchstabe_l1]]) <- buchstabe_label
          
        }
      }
    }	
  }
  
    
    
  #----------------------------------
  # Buchstabe/Buchstabe/Buchstabe (Ebene 5)
  #----------------------------------
  
  for(roemisch in 1:length(amendment_list)){
    
    ## Gibt es überhaupt Nummern?
    ## Wenn nicht, weiter
    if(class(amendment_list[[roemisch]]) == "character"){
      next
    }
    
    for(nummer in 1:length(amendment_list[[roemisch]])){
      
      ## Gibt es überhaupt Buchstaben?
      ## Wenn nicht, weiter
      if(class(amendment_list[[roemisch]][[nummer]]) == "character"){
        next
      }
      
      for(buchstabe_l1 in 1:length(amendment_list[[roemisch]][[nummer]])){
        
        if(class(amendment_list[[roemisch]][[nummer]][[buchstabe_l1]]) == "character"){
          next
        }
        
        buchstaben_match <- lapply(amendment_list[[roemisch]][[nummer]][[buchstabe_l1]], str_detect, pattern = "^[[:lower:]]{3}\\)")
        
        ## Alles in texteigenen Anführungszeichen aussortieren
        if (length(quote_lines) > 0){
          
          for (i in 1: nrow(quote_lines)) {
            buchstaben_match[quote_lines[i,1]:quote_lines[i,2]] <- FALSE
          }
        }
        
        ## Buchstaben in die Liste zerlegen
        for(buchstabe_l2 in 1:length(buchstaben_match)){
          
          ## Gibt es überhaupt Buchstaben?
          ## Wenn nicht, keine weitere Ebene anlegen
          if(sum(buchstaben_match[[buchstabe_l2]]) > 0){
            
            material <- amendment_list[[roemisch]][[nummer]][[buchstabe_l1]][[buchstabe_l2]]
            material_white <- amendment_list_white[[roemisch]][[nummer]][[buchstabe_l1]][[buchstabe_l2]]
            zeilen <- which(buchstaben_match[[buchstabe_l2]])
            
            ## Liste leeren im Eintrag leeren für die weitere Unterteilung
            amendment_list[[roemisch]][[nummer]][[buchstabe_l1]][[buchstabe_l2]] <- list()
            amendment_list_white[[roemisch]][[nummer]][[buchstabe_l1]][[buchstabe_l2]] <- list()
            
            ## Labels generieren
            buchstabe_label <- str_c("Buchstabe_", str_extract(material, "^\\w{3}")[zeilen])
            
            ## Erste und letzte Zeile hinzufügen, falls sie fehlen
            if(zeilen[1] != 1){
              zeilen <- c(1, zeilen)
              buchstabe_label <- c("Buchstabe_fehlt", buchstabe_label)
            }
            if(zeilen[length(zeilen)] != length(material)){
              zeilen <- c(zeilen, (length(material) + 1))
            }else{
              zeilen <- c(zeilen, zeilen[length(zeilen)])
              zeilen[length(zeilen)] <- zeilen[length(zeilen)] + 1
            }
            
            ## Listeneinträge auf Ebene 5 erstellen
            for(i in 1:(length(zeilen) - 1)){
              
              amendment_list[[roemisch]][[nummer]][[buchstabe_l1]][[buchstabe_l2]][[i]] <- material[zeilen[i]:(zeilen[i + 1] - 1)]
              amendment_list_white[[roemisch]][[nummer]][[buchstabe_l1]][[buchstabe_l2]][[i]] <- material_white[zeilen[i]:(zeilen[i + 1] - 1)]
              
            }
            
            ## Beschriften
            names(amendment_list[[roemisch]][[nummer]][[buchstabe_l1]][[buchstabe_l2]]) <- buchstabe_label
            names(amendment_list_white[[roemisch]][[nummer]][[buchstabe_l1]][[buchstabe_l2]]) <- buchstabe_label
            
          }
        }
      }
    }	
  }
 
    
    
  # --------------------------------------------------------------------------------------------------------
  # Ergänzung
  # Wenn keine Sinnabschnitte extrahiert wurden und Änderungsantrag als ein einziger Änderungsvorschlag 
  # wahrgenommen wird, checken, ob das der Fall ist
  # --------------------------------------------------------------------------------------------------------
  
  if (length(amendment_list)==1 & is.null(names(amendment_list[[1]])) == T) {
    
    string <- paste(amendment_list[[1]], collapse="\n ")
    string <- rm_between_multiple(string, '„', '“', extract=F, clean=F)
    string <- str_split(string, "\n")
    string[[1]] <- trimws(string[[1]], "left")
    #string <- amendment_list[[1]][which(str_detect(string, amendment_list[[1]]))]
    
    referenzen <- c("Artikel", "Art[:punct:]", "Nummer", "Num[:punct:]", "Ziffer", "Zif[:punct:]", 
                    "Buchstabe", "Buch[:punct:]", "Paragraph", "Par[:punct:]", "[§]+", "Absatz", "Abs[:punct:]",
                    "Satz\\b")
    
    referenz_zeilen <- which(str_detect(string[[1]], referenzen[1]) ==T |
                             str_detect(string[[1]], referenzen[2]) ==T |
                             str_detect(string[[1]], referenzen[3]) ==T |
                             str_detect(string[[1]], referenzen[4]) ==T |
                             str_detect(string[[1]], referenzen[5]) ==T |
                             str_detect(string[[1]], referenzen[6]) ==T |
                             str_detect(string[[1]], referenzen[7]) ==T |
                             str_detect(string[[1]], referenzen[8]) ==T |
                             str_detect(string[[1]], referenzen[9]) ==T |
                             str_detect(string[[1]], referenzen[10]) ==T |
                             str_detect(string[[1]], referenzen[11]) ==T |
                             str_detect(string[[1]], referenzen[12]) ==T |
                             str_detect(string[[1]], referenzen[13]) ==T |
                             str_detect(string[[1]], referenzen[14]) ==T )
    
    ref_zeilen <- rep(NA, length(referenz_zeilen))
    for (i in 1: length(referenz_zeilen))
      ref_zeilen[i] <- which(str_detect(amendment_list[[1]], substr(string[[1]][referenz_zeilen[i]], 1, 20)) == T)
    
    referenz_zeilen <- c(ref_zeilen, length(amendment_list[[1]])+1)
    
   
     if (length(referenz_zeilen) > 1) {
      amendment_list_new <- list()
      for (i in 1:(length(referenz_zeilen)-1))
          amendment_list_new[[i]] <- c(amendment_list[[1]][referenz_zeilen[i]:(referenz_zeilen[i+1]-1)])
      amendment_list <- amendment_list_new
     }
    
    
    # Listenelemente benennen
    names(amendment_list) <- str_c("Nummerierung_fehlt_", 1:length(amendment_list))
    
    
    
  }
    
   
  # ---------------------------------------------------
  # Store amendment_list in dataframe amendmentprop
  # ---------------------------------------------------
  
  amendmentprop$amendment_list[amendmentprop_row] <- list(amendment_list)
  # is stored without names. To get original named formad back: amendment_list <- amendmentprop$amendment_list[amendmentprop_row][[1]]
    
     
    
  }, error = function(e){
      
      amendmentprop$error_list[amendmentprop_row] <<- 1
      
  }) # end tryCatch

  
  
} # end loop over all amendmentprops
  



############################################################
# ---------------------------------------------------
# Manuelle Nachcodierung
# ---------------------------------------------------
############################################################

# amendmentprop$amendment_list[amendmentprop_row][[1]][[1]]$Nummer_4 <- c("4. In § 11 Abs. 2 Nr. 3 werden vor den Worten „Näheres regelt die",           
#                                                                         "Rechtsverordnung nach § 3 Abs. 2,“ folgende Worte eingefügt:",                
#                                                                         "„Abweichungen müssen veröffentlicht werden;“") 
# amendmentprop$amendment_list[amendmentprop_row][[1]][[1]]$Nummer_3 <- c("3. Die §§ 5 bis 9 werden gestrichen. Die bisherigen §§ 10 bis 28 werden §§ 5",
#                                                                         "bis 23.")       
## Muss noch sortiert werden









