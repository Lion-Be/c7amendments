#----------------------------------
#----------------------------------
## Gesetzesentwürfe in seine Partikel zerlegen
#----------------------------------
#----------------------------------
library(stringr)

aenderungs_gesetze <- unique(c(grep("Änderung", legistext$name), grep("wie folgt geändert", legistext$rawtext_left)))
neu_gesetze <- which(is.element(1:nrow(legistext), aenderungs_gesetze) == F)


# -----------------------------------------------------------
# Neugesetze (es wird nur die Ebene der Artikel angelegt)
# -----------------------------------------------------------
for (legistext_row in 1: nrow(legistext)){ # loop over all bills
  
  # Nicht für Änderungsgesetze
  if (is.element(legistext_row, aenderungs_gesetze))
    next
    
  tryCatch({
    
    bill <- legistext$rawtext_left[legistext_row][[1]]
    bill_white <- legistext$rawtext_white[legistext_row][[1]]
    
    
    #-------------------------------------
    ## Identifikation der Gesetzesstruktur -- in einer hierarchischen Liste speichern
    #-------------------------------------
   
    ## Liste des Gesetzes ohne Leerzeichen
    bill_list <- list()
    
    ## Liste des Gesetzes mit Leerzeichen
    bill_list_white <- list()
    
    #----------------------------------
    # Artikel (Ebene 1)
    #----------------------------------
    
    ## Artikel-Zeilen ("Artikel" am Anfang der Zeile)
    artikel_match <- c(which(str_detect(bill, "^Artikel")), (length(bill) + 1))
    
    if (length(artikel_match) > 1) {
      
      # Zeilen, die mit "Artikel" beginnen aber keine Struktureinheiten sind, aussortieren
      for (i in 1: (length(artikel_match) - 1)) {
        if (nchar(bill[artikel_match][i]) > 10) 
          artikel_match[i] <- NA
      }
      
      artikel_match <- na.omit(artikel_match)
    }
      
    # If statement wird erneut benötigt
    if (length(artikel_match) > 1) {
      ## Artikel in die Liste zerlegen
      for(i in 1:(length(artikel_match) - 1)){
        bill_list[[i]] <- bill[artikel_match[i]:(artikel_match[i + 1] - 1)]
        bill_list_white[[i]] <- bill_white[artikel_match[i]:(artikel_match[i + 1] - 1)]
      }
      
      ## Beschriften
      names(bill_list) <- str_c("Artikel_", 1:length(bill_list))
      
    } else {
      
      bill_list[[i]] <- bill
      bill_list_white[[i]] <- bill_white
      names(bill_list) <- "Artikel_fehlt"
      
    }
   
   
    
    #----------------------------------
    # Paragraph 
    #----------------------------------
    
    # Paragraph Zeilen
    #paragraph_match <- c(which(str_detect(bill, "^[§][:digit:]")), which(str_detect(bill, "^[§] [:digit:]+$")), (length(bill) + 1))
    
    
    
    
    # -----------------------------------------
    # Store bill_list in dataframe legistext
    # -----------------------------------------
    
    legistext$bill_list[legistext_row] <- list(bill_list)
    # is stored without names. To get original named formad back: bill_list <- legistext$bill_list[legistext_row][[1]]
    
    
  }, error = function(e){
    
    legistext$error_list[legistext_row] <<- 1
    
  }) # end tryCatch
  
} 



# ---------------------
# Änderungsgesetze
# ---------------------
for (legistext_row in 1: nrow(legistext)){ # loop over all bills
  
  # Nicht für Neugesetze
  if (is.element(legistext_row, neu_gesetze))
    next
  
  tryCatch({
  
  bill <- legistext$rawtext_left[legistext_row][[1]]
  bill_white <- legistext$rawtext_white[legistext_row][[1]]
  
  
  #-------------------------------------
  ## Identifikation der Gesetzesstruktur -- in einer hierarchischen Liste speichern
  #-------------------------------------
  
  ## Ordnung: Artikel, Nummer, Buchstabe, Buchstabe/Buchstabe, Buchstabe/Buchstabe/Buchstabe
  ## Vermutlich nur für Änderungsgesetze gültig
  ## Für ein neues Gesetz muss das Skript angepasst werden, 
  ## um auch Paragraphen und "(Nummer) fassen zu können"
  
  ## Liste des Gesetzes ohne Leerzeichen
  bill_list <- list()
  
  ## Liste des Gesetzes mit Leerzeichen
  bill_list_white <- list()
  
  #----------------------------------
  # Artikel (Ebene 1)
  #----------------------------------
  
  ## Artikel-Zeilen ("Artikel" am Anfang der Zeile)
  artikel_match <- c(which(str_detect(bill, "^Artikel")), (length(bill) + 1))
  
  # LB --------------------------------------------
  for (i in 1: (length(artikel_match) - 1)) {
    if (nchar(bill[artikel_match][i]) > 10) 
      artikel_match[i] <- NA
  }
  
  artikel_match <- na.omit(artikel_match)
  # End LB ----------------------------------------
  
  if (length(artikel_match) > 1) {
  
    ## Artikel in die Liste zerlegen
    for(i in 1:(length(artikel_match) - 1)){
      bill_list[[i]] <- bill[artikel_match[i]:(artikel_match[i + 1] - 1)]
      bill_list_white[[i]] <- bill_white[artikel_match[i]:(artikel_match[i + 1] - 1)]
    }
    
    ## Beschriften
    names(bill_list) <- str_c("Artikel_", 1:length(bill_list))
  
  } else {
    
    bill_list[[i]] <- bill
    bill_list_white[[i]] <- bill_white
    names(bill_list) <- "Artikel_fehlt"
    
  }
  
  
  
  #----------------------------------
  # Nummern (Ebene 2)
  #----------------------------------
  
  ## Nummer-Zeilen (Ziffer(n) am Anfang der Zeile plus Punkt) # LB: plus Leerzeichen um nicht bspw. 5.1 zu zählen
  nummer_match <- lapply(bill_list, str_detect, pattern = "^\\d+?\\. ")
  
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
  
  for(artikel in 1:length(bill_list)){
    for(hits in which(nummer_match[[artikel]])){
      if(str_detect(bill_list[[artikel]][hits], str_c("^\\d{1,2}\\. ", monate, collapse = "|"))){
        nummer_match[[artikel]][hits] <- F
      }
    }
  }
  
  
  ## LB: Zeilen aussortieren, in denen mehr whitespace als bei 1. vorhanden ist, die also eingerückte Aufzählungen sind

      # In welchem Artikel tritt der erste Aufzählungspunkt auf?
      vec <- rep(NA, length(bill_list))

      for (i in 1: length(nummer_match)) {
            vec[i] <- (min(which(nummer_match[[i]] == TRUE)))
          }

      first_art <- min(which(vec != Inf))

      # In welcher Zeile?
      first_line <- min(which(nummer_match[[first_art]] == TRUE))

      # Wie viel whitespace?
      base_white_amount <- nchar(bill_list_white[[first_art]][first_line]) - nchar(trimws(bill_list_white[[first_art]][first_line], which = "left"))

      # Wenn Nummerierung mehr whitespace aufweist als base_white_amount, nicht als Ebene 2 wahrnehmen
      for(artikel in 1:length(bill_list_white)){
        for(hits in which(nummer_match[[artikel]])){
          if(nchar(bill_list_white[[artikel]][hits]) - nchar(trimws(bill_list_white[[artikel]][hits], which = "left")) > base_white_amount){
            nummer_match[[artikel]][hits] <- F
          }
        }
      }
  # End LB
      
  
  ## Zeilen aussortieren, bei denen die Nummern Aufzählungen sind und mit kleinem Buchstaben weitergeschrieben wird
  for(artikel in 1:length(bill_list)){
    for(hits in which(nummer_match[[artikel]])){
      if(str_detect(substr(bill_list[[artikel]][hits], 1, 5), "^[:digit:]+[:punct:][:space:][:lower:]") == T){
        nummer_match[[artikel]][hits] <- F
      }
    }
  }
    
  # Wenn nur eine Nummer identifiziert wurde, kann es kein Sinnabschnitt sein
  for(artikel in 1:length(bill_list)){
    for(hits in which(nummer_match[[artikel]])){
      if(length(bill_list[[artikel]][which(nummer_match[[artikel]])]) == 1) {
        nummer_match[[artikel]][hits] <- F
      }
    }
  }
   
  # Nummer ist nur Teil von Sinnabschnitt-Nummerierung, wenn sich folgende Zeichen anschließen
  sinnabschnitt_formul <- c("[:digit:]+[:punct:][:space:]In\\b",
                            "[:digit:]+[:punct:][:space:]Die\\b",
                            "[:digit:]+[:punct:][:space:]Der\\b",
                            "[:digit:]+[:punct:][:space:]Nach\\b",
                            "[:digit:]+[:punct:][:space:]Bei\\b",
                            "[:digit:]+[:punct:][:space:].m\\b",
                            "[:digit:]+[:punct:][:space:]Anlage",
                            "[:digit:]+[:punct:][:space:]Artikel",
                            "[:digit:]+[:punct:][:space:]Nummer",
                            "[:digit:]+[:punct:][:space:]Buchstabe",
                            "[:digit:]+[:punct:][:space:][:lower:][)]",
                            "[:digit:]+[:punct:][:space:][§]",
                            "[:digit:]+[:punct:][:space:]Absatz",
                            "[:digit:]+[:punct:][:space:]Satz")    
  
  for(artikel in 1:length(bill_list)){
    for(hits in which(nummer_match[[artikel]])){
      if(str_detect(bill_list[[artikel]][hits], sinnabschnitt_formul[1]) == F &
         str_detect(bill_list[[artikel]][hits], sinnabschnitt_formul[2]) == F &
         str_detect(bill_list[[artikel]][hits], sinnabschnitt_formul[3]) == F &
         str_detect(bill_list[[artikel]][hits], sinnabschnitt_formul[4]) == F &
         str_detect(bill_list[[artikel]][hits], sinnabschnitt_formul[5]) == F &
         str_detect(bill_list[[artikel]][hits], sinnabschnitt_formul[6]) == F &
         str_detect(bill_list[[artikel]][hits], sinnabschnitt_formul[7]) == F &
         str_detect(bill_list[[artikel]][hits], sinnabschnitt_formul[8]) == F &
         str_detect(bill_list[[artikel]][hits], sinnabschnitt_formul[9]) == F &
         str_detect(bill_list[[artikel]][hits], sinnabschnitt_formul[10]) == F &
         str_detect(bill_list[[artikel]][hits], sinnabschnitt_formul[11]) == F &
         str_detect(bill_list[[artikel]][hits], sinnabschnitt_formul[12]) == F &
         str_detect(bill_list[[artikel]][hits], sinnabschnitt_formul[13]) == F &
         str_detect(bill_list[[artikel]][hits], sinnabschnitt_formul[14]) == F){
        nummer_match[[artikel]][hits] <- F
      }
    }
  }    
      
      
      
  ## Zeilen aussortieren, die nicht in die Zahlenfolge passen
  for(artikel in 1:length(bill_list)){
    
    ## Prüfen, ob überhaupt Einträge vorliegen
    if(sum(nummer_match[[artikel]]) == 0){
      next
    }
    
    zahlenfolge <- as.numeric(str_extract(bill_list[[artikel]], "^\\d+"))[which(nummer_match[[artikel]])]
    zeilen_index <- which(nummer_match[[artikel]])
    fehler_index <- numeric()
    
    lfd_nummer <- 1
    fehler_index <- 0
    
    for(i in 1:length(zahlenfolge)){
      
      if(i <= max(fehler_index)){
        next
      }
      
      ## Index der Fehler generieren 
      if(zahlenfolge[i] != lfd_nummer){
        
        ## Aktuelle Indentierung herauslesen
        indent <- nchar(str_extract(bill_list_white[[artikel]][zeilen_index[i]], "^\\s+|^\\d"))
        
        folge_fehler <- i
        
        ## So lange die Indentierung auf dieser Ebene bleibt Fehler notieren
        while(nchar(str_extract(bill_list_white[[artikel]][zeilen_index[folge_fehler]], "^\\s+|^\\d")) == indent){
          fehler_index <- c(fehler_index, folge_fehler)
          folge_fehler <- folge_fehler + 1
        }
        
      }else{
        
        lfd_nummer <- lfd_nummer + 1
        
      }
      
    }
    
    nummer_match[[artikel]][zeilen_index[fehler_index]] <- F
    
  }
  
  ## Nummern in die Liste zerlegen
  for(artikel in 1:length(bill_list)){
    
    ## Gibt es überhaupt Nummer? 
    ## Wenn nicht, keine weitere Ebene anlegen
    if(sum(nummer_match[[artikel]]) == 0){
      next
    }
    
    material <- bill_list[[artikel]]
    material_white <- bill_list_white[[artikel]]
    zeilen <- which(nummer_match[[artikel]])
    
    ## Liste leeren im Eintrag leeren für die weitere Unterteilung
    bill_list[[artikel]] <- list()
    bill_list_white[[artikel]] <- list()
    
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
      
      bill_list[[artikel]][[i]] <- material[zeilen[i]:(zeilen[i + 1] - 1)]
      bill_list_white[[artikel]][[i]] <- material_white[zeilen[i]:(zeilen[i + 1] - 1)]
      
    }
    
    ## Beschriften
    names(bill_list[[artikel]]) <- nummer_label
    names(bill_list_white[[artikel]]) <- nummer_label
    
  }
  
  #----------------------------------
  # Buchstabe (Ebene 3)
  #----------------------------------
  
  ## Buchstaben-Zeilen (Buchstabe am Anfang der Zeile plus ")")
  for(artikel in 1:length(bill_list)){
    
    ## Gibt es überhaupt Nummern?
    ## Wenn nicht, weiter
    
    if(class(bill_list[[artikel]]) == "character"){
      next
    }
    
    buchstaben_match <- lapply(bill_list[[artikel]], str_detect, pattern = "^[[:lower:]]\\)")
    
    ## Buchstaben in die Liste zerlegen
    for(nummer in 1:length(buchstaben_match)){
      
      ## Gibt es überhaupt Buchstaben?
      ## Wenn nicht, keine weitere Ebene anlegen
      if(sum(buchstaben_match[[nummer]]) > 0){
        
        material <- bill_list[[artikel]][[nummer]]
        material_white <- bill_list_white[[artikel]][[nummer]]
        zeilen <- which(buchstaben_match[[nummer]])
        
        ## Liste leeren im Eintrag leeren für die weitere Unterteilung
        bill_list[[artikel]][[nummer]] <- list()
        bill_list_white[[artikel]][[nummer]] <- list()
        
        ## Labels generieren
        buchstabe_label <- str_c("Buchstabe_", str_extract(material, "^\\w")[zeilen])
        
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
          
          bill_list[[artikel]][[nummer]][[i]] <- material[zeilen[i]:(zeilen[i + 1] - 1)]
          bill_list_white[[artikel]][[nummer]][[i]] <- material_white[zeilen[i]:(zeilen[i + 1] - 1)]
          
        }
        
        ## Beschriften
        names(bill_list[[artikel]][[nummer]]) <- buchstabe_label
        names(bill_list_white[[artikel]][[nummer]]) <- buchstabe_label
        
      }
    }	
  }
  
  #----------------------------------
  # Buchstabe/Buchstabe (Ebene 4)
  #----------------------------------
  
  ## Buchstaben/Buchstaben-Zeilen (Buchstabe/Buchstabe am Anfang der Zeile plus ")")
  for(artikel in 1:length(bill_list)){
    
    ## Gibt es überhaupt Nummern?
    ## Wenn nicht, weiter
    
    if(class(bill_list[[artikel]]) == "character"){
      next
    }
    
    for(nummer in 1:length(bill_list[[artikel]])){
      
      ## Gibt es überhaupt Buchstaben?
      ## Wenn nicht, weiter
      
      if(class(bill_list[[artikel]][[nummer]]) == "character"){
        next
      }
      
      buchstaben_match <- lapply(bill_list[[artikel]][[nummer]], str_detect, pattern = "^[[:lower:]]{2}\\)")
      
      ## Buchstaben in die Liste zerlegen
      for(buchstabe_l1 in 1:length(buchstaben_match)){
        
        ## Gibt es überhaupt Buchstaben?
        ## Wenn nicht, keine weitere Ebene anlegen
        if(sum(buchstaben_match[[buchstabe_l1]]) > 0){
          
          material <- bill_list[[artikel]][[nummer]][[buchstabe_l1]]
          material_white <- bill_list_white[[artikel]][[nummer]][[buchstabe_l1]]
          zeilen <- which(buchstaben_match[[buchstabe_l1]])
          
          ## Liste leeren im Eintrag leeren für die weitere Unterteilung
          bill_list[[artikel]][[nummer]][[buchstabe_l1]] <- list()
          bill_list_white[[artikel]][[nummer]][[buchstabe_l1]] <- list()
          
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
            
            bill_list[[artikel]][[nummer]][[buchstabe_l1]][[i]] <- material[zeilen[i]:(zeilen[i + 1] - 1)]
            bill_list_white[[artikel]][[nummer]][[buchstabe_l1]][[i]] <- material_white[zeilen[i]:(zeilen[i + 1] - 1)]
            
          }
          
          ## Beschriften
          names(bill_list[[artikel]][[nummer]][[buchstabe_l1]]) <- buchstabe_label
          names(bill_list_white[[artikel]][[nummer]][[buchstabe_l1]]) <- buchstabe_label
          
        }
      }
    }	
  }
  
  #----------------------------------
  # Buchstabe/Buchstabe/Buchstabe (Ebene 5)
  #----------------------------------
  
  ## Buchstaben/Buchstaben/Buchstaben-Zeilen (Buchstabe/Buchstabe/Buchstabe am Anfang der Zeile plus ")")
  for(artikel in 1:length(bill_list)){
    
    ## Gibt es überhaupt Nummern?
    ## Wenn nicht, weiter
    if(class(bill_list[[artikel]]) == "character"){
      next
    }
    
    for(nummer in 1:length(bill_list[[artikel]])){
      
      ## Gibt es überhaupt Buchstaben?
      ## Wenn nicht, weiter
      if(class(bill_list[[artikel]][[nummer]]) == "character"){
        next
      }
      
      for(buchstabe_l1 in 1:length(bill_list[[artikel]][[nummer]])){
        
        if(class(bill_list[[artikel]][[nummer]][[buchstabe_l1]]) == "character"){
          next
        }
        
        buchstaben_match <- lapply(bill_list[[artikel]][[nummer]][[buchstabe_l1]], str_detect, pattern = "^[[:lower:]]{3}\\)")
        
        ## Buchstaben in die Liste zerlegen
        for(buchstabe_l2 in 1:length(buchstaben_match)){
          
          ## Gibt es überhaupt Buchstaben?
          ## Wenn nicht, keine weitere Ebene anlegen
          if(sum(buchstaben_match[[buchstabe_l2]]) > 0){
            
            material <- bill_list[[artikel]][[nummer]][[buchstabe_l1]][[buchstabe_l2]]
            material_white <- bill_list_white[[artikel]][[nummer]][[buchstabe_l1]][[buchstabe_l2]]
            zeilen <- which(buchstaben_match[[buchstabe_l2]])
            
            ## Liste leeren im Eintrag leeren für die weitere Unterteilung
            bill_list[[artikel]][[nummer]][[buchstabe_l1]][[buchstabe_l2]] <- list()
            bill_list_white[[artikel]][[nummer]][[buchstabe_l1]][[buchstabe_l2]] <- list()
            
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
              
              bill_list[[artikel]][[nummer]][[buchstabe_l1]][[buchstabe_l2]][[i]] <- material[zeilen[i]:(zeilen[i + 1] - 1)]
              bill_list_white[[artikel]][[nummer]][[buchstabe_l1]][[buchstabe_l2]][[i]] <- material_white[zeilen[i]:(zeilen[i + 1] - 1)]
              
            }
            
            ## Beschriften
            names(bill_list[[artikel]][[nummer]][[buchstabe_l1]][[buchstabe_l2]]) <- buchstabe_label
            names(bill_list_white[[artikel]][[nummer]][[buchstabe_l1]][[buchstabe_l2]]) <- buchstabe_label
            
          }
        }
      }
    }	
  }
      
  
      
      
  # -----------------------------------------
  # Store bill_list in dataframe legistext
  # -----------------------------------------
  
  legistext$bill_list[legistext_row] <- list(bill_list)
  # is stored without names. To get original named formad back: bill_list <- legistext$bill_list[legistext_row][[1]]
  
      
  }, error = function(e){
    
    legistext$error_list[legistext_row] <<- 1
    
  }) # end tryCatch
    
} # end loop over all bills



# Einzelne manuelle Anpassungen
legistext$bill_list[94][[1]] <- list(legistext$bill_list[94][[1]][[13]])
legistext$bill_list[95][[1]] <- list(legistext$bill_list[95][[1]][13])
legistext$bill_list[98][[1]] <- list(legistext$bill_list[98][[1]][[13]])
legistext$bill_list[106][[1]] <- list(legistext$bill_list[106][[1]][[2]]) 
legistext$bill_list[127][[1]] <- list(legistext$bill_list[127][[1]][[6]])
legistext$bill_list[135][[1]] <- list(legistext$bill_list[135][[1]][[6]])
legistext$bill_list[138][[1]] <- list(legistext$bill_list[138][[1]][[6]])
legistext$bill_list[145][[1]] <- list(legistext$bill_list[145][[1]][[6]]) 
legistext$bill_list[178][[1]] <- list(legistext$bill_list[178][[1]][[2]]) 
legistext$bill_list[191][[1]] <- list(legistext$bill_list[191][[1]][[3]]) 
legistext$bill_list[237][[1]] <- list(legistext$bill_list[237][[1]][[3]]) 
legistext$bill_list[240][[1]] <- list(legistext$bill_list[240][[1]][[3]]) 
legistext$bill_list[244][[1]] <- list(legistext$bill_list[244][[1]][[3]]) 
legistext$bill_list[245][[1]] <- list(legistext$bill_list[245][[1]][[3]]) 
legistext$bill_list[247][[1]] <- list(legistext$bill_list[247][[1]][[2]])
legistext$bill_list[249][[1]] <- list(legistext$bill_list[249][[1]][[2]])
legistext$bill_list[251][[1]] <- list(legistext$bill_list[251][[1]][[2]])
legistext$bill_list[255][[1]] <- list(legistext$bill_list[255][[1]][[2]])
legistext$bill_list[307][[1]] <- list(legistext$bill_list[307][[1]][[2]])
legistext$bill_list[309][[1]] <- list(legistext$bill_list[309][[1]][[2]])
legistext$bill_list[317][[1]] <- list(legistext$bill_list[317][[1]][[2]])
legistext$bill_list[321][[1]] <- list(legistext$bill_list[321][[1]][[2]])
legistext$bill_list[324][[1]] <- list(legistext$bill_list[324][[1]][[2]])
legistext$bill_list[328][[1]] <- list(legistext$bill_list[328][[1]][[2]])
legistext$bill_list[349][[1]] <- list(legistext$bill_list[349][[1]][[2]])
# Für alle diese Zeilen: Namen müssen sein "Artikel_fehlt"

