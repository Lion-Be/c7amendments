# ---------------------------------------
# SFB 884, Projekt C7
# Erstellung konkurrierender Entwuerfe
# Lion Behrens
# ---------------------------------------

library(stringr)

# Originalversion sichern, bevor Amendments eingearbeitet werden
bill_list.original <- bill_list

# -----------------------------------
# -----------------------------------
# --- Text loeschen
# -----------------------------------
# -----------------------------------

loeschen <- function (ref.art, ref.num=NA, ref.buch=NA, ref.buchbuch=NA, ref.para=NA, ref.absatz=NA, ref.satz=NA, spec.satz=NA, spec.wort=NA, e.list) {

  # Neues Objekt erstellen
  bill_list.new <<- bill_list
  
  # ----------------------------------------------------------------
  # -----------------------------------------------------------
  # Spezifischen Sinnabschnitt von Änderungsgesetzen löschen
  # -----------------------------------------------------------
  # ----------------------------------------------------------------
  if (e.list == T) {
  
    if (is.na(ref.num) == T)
      bill_list.new[[ref.art]] <<- NULL
    else if (is.na(ref.buch) == T)
      bill_list.new[[ref.art]][ref.num] <<- NULL
    else if (is.na(ref.buchbuch) == T)
      bill_list.new[[ref.art]][[ref.num]][ref.buch] <<- NULL
    else if (is.na(ref.buchbuch) == F)
      bill_list.new[[ref.art]][[ref.num]][[ref.buch]][ref.buchbuch] <<- NULL
    
  
  # ----------------------------------------------------------------
  # -----------------------------------------------------------
  # Text innerhalb eines Sinnabschnitts löschen
  # -----------------------------------------------------------
  # ---------------------------------------------------------------- 
  } else if (e.list == F) {
  
    
    if (is.na(ref.buch) == T) {
      
      text.ref <- bill_list.new[[ref.art]][ref.num]
  
      
      # ---------------------------------------------------------
      # Absatz innerhalb eines Paragraphen soll geloescht werden
      if (is.na(ref.para) == F & is.na(ref.absatz) == F) {
      # ---------------------------------------------------------
        
        # Text des Paragraphen, innerhalb dessen Text gelöscht werden soll, identifizieren
        ifelse(ref.para < 10, 
               paragraph <- str_c("[§]", ref.para),
               paragraph <- str_c("[§] ", ref.para))
        ref.start <- grep(str_c("\\b", paragraph, "\\b"), text.ref[[1]]) 
        
        ifelse(ref.para < 9, 
               ref.end <- str_c("[§]", ref.para+1), 
               ref.end <- str_c("[§] ", ref.para+1))
        ref.end <- grep(str_c("\\b", ref.end, "\\b"), text.ref[[1]]) 
        if (length(ref.end) == 0){ref.end <- length(text.ref[[1]])}
        
        
        "
        Old version  
        paragraph <- str_c('§', ref.para)
        ref.start <- grep(paragraph, text.ref[[1]]) 
        
        ref.end <- str_c('§', ref.para+1) # geht nicht wenn der letze Paragraph gelöscht werden soll, dann einfach bis Ende
        ref.end <- grep(ref.end, text.ref[[1]]) 
        "
        
        
        text.ref <- text.ref[[1]][ref.start:(ref.end-1)]
      
        # Absatz innerhalb des Paragraphen identifizieren
        absatz <- str_c("(", ref.absatz, ")")
        del.start <- grep(str_c("\\((", ref.absatz, "\\))"), text.ref) -1  # Hier war vorher noch -1. Klappt weiterhin alles, wo ein Absatz gelöscht werden soll?
        
        if (length( grep(str_c("\\((", ref.absatz + 1, "\\))"), text.ref) ) > 0) {
          del.end <- grep(str_c("\\((", ref.absatz + 1, "\\))"), text.ref) -2} else {
          del.end <- length(text.ref) - 1
        }
        
        # Absatz loeschen
        bill_list.new[[ref.art]][ref.num][[1]] <<- bill_list.new[[ref.art]][ref.num][[1]][-seq((ref.start+del.start), (ref.start+(del.end)), 1)]
        
      } 
      
      
      # ---------------------------------------------------------
      # Paragraph soll geloescht werden
      if (is.na(ref.para) == F & is.na(ref.absatz) == T) {
      # --------------------------------------------------------- 
       
      # Text des Paragraphen, der geloescht werden soll, identifizieren
      ifelse(ref.para < 10, 
             paragraph <- str_c("[§]", ref.para),
             paragraph <- str_c("[§] ", ref.para))
      ref.start <- grep(str_c("\\b", paragraph, "\\b"), text.ref[[1]]) 
      
      ifelse(ref.para < 9, 
             ref.end <- str_c("[§]", ref.para+1), 
             ref.end <- str_c("[§] ", ref.para+1))
      ref.end <- grep(str_c("\\b", ref.end, "\\b"), text.ref[[1]]) 
      if (length(ref.end) == 0){ref.end <- length(text.ref[[1]])}  
      "  
      Old version
      paragraph <- str_c('§', ref.para)
      ref.start <- grep(paragraph, text.ref[[1]]) 
      
      ref.end <- str_c('§', ref.para+1) # geht nicht wenn der letze Paragraph gelöscht werden soll, dann einfach bis Ende
      ref.end <- grep(ref.end, text.ref[[1]]) 
      "
        
      bill_list.new[[ref.art]][ref.num][[1]] <<- bill_list.new[[ref.art]][ref.num][[1]][-seq(ref.start, (ref.end-1), 1)]  
      
      }
    }
    
  
  if (is.na(ref.buch) == F) {
    
      # --------------------------------------------------------
      # Nummerierter Satz soll in Absatz geloescht werden
      if (is.na(ref.absatz) == F & is.na(ref.satz) == F) {
      # --------------------------------------------------------
      
      text.ref <- bill_list[[ref.art]][[ref.num]][ref.buch][[1]]
        
      # Absatz identifizieren
      absatz.start <- grep(str_c("\\((", ref.absatz, "\\))"), text.ref) 
      
      # Zu loeschenden Text identifizieren
      satz.grenzen <- grep("\\.", text.ref) 
      
      # Datumsangaben aussortieren
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
      
      monat_zeilen <- str_detect(text.ref[satz.grenzen], str_c("\\d{1,2}\\. ", monate, collapse = "|"))
      satz.grenzen <- satz.grenzen[monat_zeilen==FALSE]
      
      split1 <- str_split(text.ref[satz.grenzen[ref.satz-1]], "")
      split2 <- str_split(text.ref[satz.grenzen[ref.satz]], "")
      
      del.start <- which(str_detect(split1[[1]], "\\."), TRUE) + 2
      del.stop <- which(str_detect(split2[[1]], "\\."), TRUE)
      
      # Delete start of sentence
      split1 <- split1[[1]][-((del.start-1):lengths(split1))]
      split1 <- paste(split1, collapse = '')
      text.ref[satz.grenzen[ref.satz-1]] <- split1
      
      # Delete end of sentence
      split2 <- split2[[1]][-(1:del.stop)]
      split2 <- paste(split2, collapse = '')
      text.ref[satz.grenzen[ref.satz]] <- split2
      
      # Delete all in the middle if sentence spanned several lines
      if ((satz.grenzen[ref.satz] - satz.grenzen[ref.satz-1]) > 1)  {
        del.lines <- seq((satz.grenzen[ref.satz-1]+1),(satz.grenzen[ref.satz]-1), 1)
        for (i in 1: length(del.lines)){
          text.ref <- text.ref[-del.lines[i]]
          }
        }
      
      
      bill_list.new[[ref.art]][[ref.num]][ref.buch][[1]] <<- text.ref
      
      }
  }
    
    # --------------------------------------------------------
    # Spezifischer Satz soll in Artikel geloescht werden
    if (is.na(spec.satz) == F) {  
    # --------------------------------------------------------
    
    text.ref <- bill_list[[ref.art]]
    
    # Satzgrenzen identifizieren
      
      # Wo sind Punkte
      satz.grenzen <- grep("\\.", text.ref)
    
      # Datumsangaben aussortieren
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
      
      monat_zeilen <- str_detect(text.ref[satz.grenzen], str_c("\\d{1,2}\\. ", monate, collapse = "|"))
      satz.grenzen <- satz.grenzen[monat_zeilen==FALSE]
      
    # Konkreten Satzbeginn identifizieren
    satzbeginn <- grep(str_c("\\. ", substr(spec.satz, 0, 5)), text.ref)
    beginn <- satz.grenzen==satzbeginn
    beginn_index <- which(beginn, TRUE)
    satzende <- satz.grenzen[beginn_index+1]
    
    split1 <- str_split(text.ref[satzbeginn], "")  
    split2 <- str_split(text.ref[satzende], "")  
    
    del.start <- which(str_detect(split1[[1]], "\\."), TRUE) + 2 
    del.stop <- which(str_detect(split2[[1]], "\\."), TRUE)  
      
    # Delete start of sentence
    split1 <- split1[[1]][-((del.start-1):lengths(split1))]
    split1 <- paste(split1, collapse = '')
    text.ref[satzbeginn] <- split1
    
    # Delete end of sentence
    split2 <- split2[[1]][-(1:del.stop)]
    split2 <- paste(split2, collapse = '')
    text.ref[satzende] <- split2
    
    # Delete all in the middle if sentence spanned several lines
    if ((satzende - satzbeginn) > 1)  {
      del.lines <- seq(from=satzbeginn+1, to=satzende-1, by=1)
      for (i in 1: length(del.lines)){
        text.ref <- text.ref[-del.lines[i]]
      }
    }   
      
    bill_list.new[[ref.art]] <<- text.ref 
      
    }
    
    # --------------------------------------------------------
    # Spezifische Angabe soll in Nummer geloescht werden
    if (is.na(spec.wort) == F) {  
    # --------------------------------------------------------
    
    text.ref <- bill_list[[ref.art]][ref.num]
    
    # Zeile in der spezifische Angabe liegt identifizieren
    zeile <- grep(str_c("\\b", spec.wort,"\\b"), text.ref[[1]])  
    
    # Spezifische Angabe loeschen
    text.ref[[1]][zeile] <- str_remove(text.ref[[1]][zeile], str_c("\\b", spec.wort,"\\b"))
    
    # Extra Leerzeichen löschen
    text.ref[[1]][zeile] <- str_replace(text.ref[[1]][zeile], "  ", " ")
    
    bill_list.new[[ref.art]][ref.num] <<- text.ref 
    
    }
  }
  
  
  # Letzter Schritt: abgeänderten Text wieder als bill_list abspeichern
  # Damit kann die Funktion iterativ angewandt werden und mehrere Veränderungen können hintereinander eingebaut werden
  # Einfach vorher ein Objekt names bill_list.original abspeichern, in dem die uspruengliche Version behalten wird
  bill_list <<- bill_list.new
  
}


# ---------------------
# Funktion testen
# ---------------------

  ###########
  # 16/2009 #
  ###########

  # # 16/2009, I. 1.
  # loeschen(ref.art="Artikel_1", ref.num="Nummer_2", ref.buch="Buchstabe_b", e.list=T)
  # 
  # # 16/2009, I. 2. b)
  # loeschen(ref.art="Artikel_1", ref.num="Nummer_3", ref.para=4, ref.absatz=3, e.list=F)
  # 
  # # 16/2009, I. 2. c)
  # loeschen(ref.art="Artikel_1", ref.num="Nummer_3", ref.para=5, e.list=F)
  # 
  # # 16/2009, I. 2. e) aa)
  # loeschen(ref.art="Artikel_1", ref.num="Nummer_3", ref.para=6, ref.absatz=1, e.list=F) 
  # loeschen(ref.art="Artikel_1", ref.num="Nummer_3", ref.para=6, ref.absatz=3, e.list=F)
  # 
  # # 16/2009, I. 2. e) cc)
  # loeschen(ref.art="Artikel_1", ref.num="Nummer_3", ref.para=6, ref.absatz=6, e.list=F)
  # loeschen(ref.art="Artikel_1", ref.num="Nummer_3", ref.para=6, ref.absatz=7, e.list=F)
  # 
  # # 16/2009, I. 3. 
  # loeschen(ref.art="Artikel_1", ref.num="Nummer_5", e.list=T)
  # 
  # 
  # 
  # ###########
  # # 16/2039 #
  # ###########
  # 
  # # 16/2039 I. 1. 
  # loeschen(ref.art="Artikel_1", ref.num="Nummer_2", ref.buch="Buchstabe_b", ref.absatz=3, ref.satz=2, e.list=F)
  # 
  # # 16/2039 II.
  # loeschen(ref.art="Artikel_2", spec.satz="Das besondere Verbot nach Satz 1 gilt nicht für ehrenamtliche Richter.", e.list=F)
  # 
  # # 16/2039 III.
  # loeschen(ref.art="Artikel_6", ref.num="Nummer_1", spec.wort="Satz 1", e.list=F)
  # 
  # 
  # 
  # ###########
  # # 16/2029 #
  # ###########
  # 
  # # 16/2029 1.
  # loeschen(ref.art="Artikel_1", e.list=T)
  # loeschen(ref.art="Artikel_2", e.list=T)
  # 
  # 
  # 
  # ###########
  # # 16/2957 #
  # ###########
  # 
  # # 16/2957-2 
  # loeschen(ref.art="Artikel_1", ref.num=15, e.list=T)
  
  
  
  
# -----------------------------------
# -----------------------------------
# --- Text neu fassen 
# -----------------------------------
# -----------------------------------

neufassen <- function(ref.art, ref.num=NA, ref.buch=NA, ref.buchbuch=NA, ref.para=NA, ref.absatz=NA, ref.satz=NA, spec.satz=NA, spec.wort=NA, text.neu, e.list){
  
  # Urspruengliche Sektion loeschen
  bill_list.new <<- loeschen(ref.art=ref.art, ref.num=ref.num, ref.buch=ref.buch, ref.buchbuch=ref.buchbuch, ref.para=ref.para, ref.absatz=ref.absatz, ref.satz=ref.satz, spec.satz=spec.satz, spec.wort=spec.wort, e.list)
  
  # Neues Objekt erstellen
  bill_list.new <<- bill_list
  
  
  # ----------------------------------------------------------------
  # -----------------------------------------------------------
  # Spezifischen Sinnabschnitt von Änderungsgesetzen neufassen
  # -----------------------------------------------------------
  # ----------------------------------------------------------------
  
  
  
  
  
  if (e.list == T) {
    
    if (is.na(ref.num) == T)
      bill_list.new[[ref.art]] <<- text.neu
    else if (is.na(ref.buch) == T)
      bill_list.new[[ref.art]][ref.num] <<- text.neu
    else if (is.na(ref.buchbuch) == T)
      bill_list.new[[ref.art]][[ref.num]][ref.buch] <<- text.neu
    else if (is.na(ref.buchbuch) == F)
      bill_list.new[[ref.art]][[ref.num]][[ref.buch]][ref.buchbuch] <<- text.neu
  } 
  
  
  
  
  # ----------------------------------------------------------------
  # -----------------------------------------------------------
  # Text innerhalb eines Sinnabschnitts neufassen
  # -----------------------------------------------------------
  # ---------------------------------------------------------------- 
        
  if (e.list == F) {
    
    # Arbeits-string text.ref.orig definieren
    if (is.na(ref.num) == T)
      text.ref.orig <- bill_list.new[ref.art]
    else if (is.na(ref.buch) == T)
      text.ref.orig <- bill_list.new[[ref.art]][ref.num]
    else if (is.na(ref.buchbuch) == T)
      text.ref.orig <- bill_list.new[[ref.art]][[ref.num]][ref.buch]
    else if (is.na(ref.buchbuch) == F)
      text.ref.orig <- bill_list.new[[ref.art]][[ref.num]][[ref.buch]][ref.buchbuch]
    
  
    
    # ------------------------------------------------
    # Absatz innerhalb eines Paragraphen neufassen
    if ((is.na(ref.para) == F) & (is.na(ref.absatz) == F)) {
    # ------------------------------------------------
    
      # Text des Paragraphen, innerhalb dessen Text gelöscht werden soll, identifizieren
      ifelse(ref.para < 10, 
             paragraph <- str_c("[§]", ref.para),
             paragraph <- str_c("[§] ", ref.para))
      ref.start <- grep(str_c("\\b", paragraph, "\\b"), text.ref.orig[[1]]) 
      
      ifelse(ref.para < 9, 
             ref.end <- str_c("[§]", ref.para+1), 
             ref.end <- str_c("[§] ", ref.para+1))
      ref.end <- grep(str_c("\\b", ref.end, "\\b"), text.ref.orig[[1]]) 
      if (length(ref.end) == 0){ref.end <- length(text.ref.orig[[1]])}
      "
      Old version      
      paragraph <- str_c('§', ref.para)
      ref.start <- grep(paragraph, text.ref.orig[[1]]) 
      
      ref.end <- str_c('§', ref.para+1) # geht nicht wenn der letze Paragraph gelöscht werden soll, dann einfach bis Ende
      ref.end <- grep(ref.end, text.ref.orig[[1]]) 
      "
      
      text.ref <- text.ref.orig[[1]][ref.start:(ref.end-1)]
      
      
      # Neu gefassten Absatz anklippen
      if (ref.absatz == 1) {
        text.ref <- c(text.ref[1:2], text.neu, text.ref[3:length(text.ref)])
        text.ref.orig <- text.ref.orig[[1]][-(ref.start:(ref.end-1))]
        text.ref.orig <- c(text.ref.orig[1:(ref.start-1)], text.ref, text.ref.orig[ref.start:length(text.ref.orig)])
      }
      
      
      # Zum Originalobjekt hinzufügen
      if (is.na(ref.num) == T)
        bill_list.new[ref.art][[1]] <- text.ref.orig
      else if (is.na(ref.buch) == T)
        bill_list.new[[ref.art]][ref.num][[1]] <- text.ref.orig
      else if (is.na(ref.buchbuch) == T)
        bill_list.new[[ref.art]][[ref.num]][ref.buch][[1]] <- text.ref.orig
      else if (is.na(ref.buchbuch) == F)
        bill_list.new[[ref.art]][[ref.num]][[ref.buch]][ref.buchbuch][[1]] <- text.ref.orig
      
      # Letzter Schritt: abgeänderten Text wieder als bill_list abspeichern
      # Damit kann die Funktion iterativ angewandt werden und mehrere Veränderungen können hintereinander eingebaut werden
      # Einfach vorher ein Objekt names bill_list.original abspeichern, in dem die uspruengliche Version behalten wird
      bill_list <<- bill_list.new
   
    }
    
  
  }
}


  
# ---------------------
# Funktion testen
# ---------------------

  ###########
  # 16/2009 #
  ###########
  
  # 16/2009 I. 2. a)
  # text.neu <- amendmentprop$rawtext_left[[10]][5:13]
  # text.neu[1] <- substring(text.neu[1], 2, nchar(text.neu[1]))
  # text.neu[length(text.neu)] <- substring(text.neu[length(text.neu)], 1, nchar(text.neu[length(text.neu)])-1)
  # 
  # neufassen(ref.art="Artikel_1", ref.num="Nummer_3", ref.para=4, ref.absatz=1, text.neu=text.neu, e.list=F)
 
  
  
  
  
  
  
  
  
# ------------------------------------
# ------------------------------------
# --- Sinnabschnitte neu nummerieren
# ------------------------------------
# ------------------------------------
  
neunummerieren <- function(ref.art, ref.num=NA, ref.buch=NA, ref.buchbuch=NA, ref.para=NA, ref.absatz=NA, ref.art.neu=NA, ref.num.neu=NA, ref.buch.neu=NA, ref.buchbuch.neu=NA, ref.para.neu=NA, ref.absatz.neu=NA){
  
  # -------------------------------------------------------------------------------------
  # Schritt 1: Neues Element als Kopie eines alten Elements definieren
  # Schritt 2: Altes Element loeschen 
  # -------------------------------------------------------------------------------------
  
  bill_list.new <- bill_list
  
  # ----------------
  # Ebene: Artikel
  if (is.na(ref.art) == F & is.na(ref.num) == T & is.na(ref.buch) == T & is.na(ref.buchbuch) == T) {
  # ----------------
  
    text.ref.orig <- bill_list.new[ref.art]
    
    # --------------------------------------------------
    # Direkt Artikel neu nummerieren
    if (is.na(ref.para) == T & is.na(ref.absatz) == T) {
    # --------------------------------------------------  
      
      # Artikel shiften und altes Objekt loeschen
      bill_list[[ref.art.neu]] <<- bill_list[[ref.art]]
      loeschen(ref.art=ref.art, e.list=T) 
      
      # Artikel-Referenz im raw text ändern
      bill_list[[ref.art.neu]][[1]][1] <<- str_c("Artikel ", str_sub(ref.art.neu,-1,-1))
      
    } else {
  
    # --------------------------------------------------
    # Paragraph innerhalb Artikel neu nummerieren
    if (is.na(ref.para) == F & is.na(ref.absatz) == T) {
    # --------------------------------------------------
      ifelse(ref.para < 10, 
             paragraph <- str_c("[§]", ref.para),
             paragraph <- str_c("[§] ", ref.para))
      paragraph.line <- grep(str_c("\\b", paragraph, "\\b"), text.ref.orig) 
      
      "
      Old version
      paragraph <- str_c('§', ref.para)
      paragraph.line <- grep(paragraph, text.ref.orig) 
      "      
      text.ref.orig[paragraph.line] <- str_c("§", ref.para.neu)
    }
    
    # --------------------------------------------------
    # Absatz innerhalb Artikel neu nummerieren
    if (is.na(ref.para) == T & is.na(ref.absatz) == F) {
    # --------------------------------------------------
      absatz <- str_c("(", ref.absatz, ")")
      absatz.line <- grep(absatz, text.ref.orig) 
      text.ref.orig[absatz.line] <- str_c("(", ref.absatz.neu, ")")
    }
    
    # --------------------------------------------------------------
    # Absatz innerhalb Paragraph innerhalb Artikel neu nummerieren
    if (is.na(ref.para) == F & is.na(ref.absatz) == F) {
    # ------------------------------------------------------------
      
      # Text des Paragraphen, innerhalb dessen Text gelöscht werden soll, identifizieren
      ifelse(ref.para < 10, 
             paragraph <- str_c("[§]", ref.para),
             paragraph <- str_c("[§] ", ref.para))
      ref.start <- grep(str_c("\\b", paragraph, "\\b"), text.ref.orig[[1]]) 
      
      ifelse(ref.para < 8, 
             ref.end <- str_c("[§]", ref.para+1), 
             ref.end <- str_c("[§] ", ref.para+1))
      ref.end <- grep(str_c("\\b", ref.end, "\\b"), text.ref.orig[[1]]) 
      if (length(ref.end) == 0){ref.end <- length(text.ref.orig[[1]])}
      
      text.ref <- text.ref.orig[[1]][ref.start:(ref.end-1)]
      
      # Absatz neu nummerieren
      absatz <- str_c("[(]", ref.absatz, "[)]")
      absatz.line <- grep(absatz, text.ref) 
      text.ref[absatz.line] <- str_c("(", ref.absatz.neu, ")")
      
      # An ursprüngliches Textobjekt anklippen
      text.ref.orig <- c(text.ref.orig[[1]][1:ref.start-1], text.ref, text.ref.orig[[1]][ref.end:lengths(text.ref.orig)])
    }
    
    # An ursprüngliches Textobjekt anklippen
    bill_list.new[ref.art][[1]] <- text.ref.orig
    bill_list <<- bill_list.new
    }
    
  }
  
  
  
  
  # ---------------------------------------------------------------------------------------------------
  # Ebene: Nummer
  if (is.na(ref.art) == F & is.na(ref.num) == F & is.na(ref.buch) == T & is.na(ref.buchbuch) == T) {
  # ---------------------------------------------------------------------------------------------------
    
    text.ref.orig <- bill_list.new[[ref.art]][ref.num][[1]]
    
    # --------------------------------------------------
    # Direkt Nummer neu nummerieren
    if (is.na(ref.para) == T & is.na(ref.absatz) == T) {
    # --------------------------------------------------  
      
      # Nummer shiften und altes Objekt loeschen
      bill_list[[ref.art]][ref.num.neu] <<- bill_list[[ref.art]][ref.num]
      loeschen(ref.art=ref.art, ref.num=ref.num, e.list=T) 
      
      # Nummer-Referenz im raw text ändern
      bill_list[[ref.art]][ref.num.neu][[1]][1] <<- str_c(str_sub(ref.num.neu,-1,-1), str_sub(bill_list[[ref.art]][ref.num.neu][[1]][1],2,-1))
      
    } else {
    
    # --------------------------------------------------
    # Paragraph innerhalb Nummer neu nummerieren
    if (is.na(ref.para) == F & is.na(ref.absatz) == T) {
    # --------------------------------------------------
      ifelse(ref.para < 10, 
             paragraph <- str_c("[§]", ref.para),
             paragraph <- str_c("[§] ", ref.para))
      paragraph.line <- grep(str_c("\\b", paragraph, "\\b"), text.ref.orig)   
      
      "
      Old version
      paragraph <- str_c('§', ref.para)
      paragraph.line <- grep(paragraph, text.ref.orig) 
      "
      ifelse(ref.para.neu < 10, 
             text.ref.orig[paragraph.line] <- str_c("§", ref.para.neu),
             text.ref.orig[paragraph.line] <- str_c("§ ", ref.para.neu))
      
    }
    
    # --------------------------------------------------
    # Absatz innerhalb Nummer neu nummerieren
    if (is.na(ref.para) == T & is.na(ref.absatz) == F) {
      # --------------------------------------------------
      absatz <- str_c("(", ref.absatz, ")")
      absatz.line <- grep(absatz, text.ref.orig) 
      text.ref.orig[absatz.line] <- str_c("(", ref.absatz.neu, ")")
    }
    
    # --------------------------------------------------------------
    # Absatz innerhalb Paragraph innerhalb Nummer neu nummerieren
    if (is.na(ref.para) == F & is.na(ref.absatz) == F) {
      # ------------------------------------------------------------
      
      # Text des Paragraphen, innerhalb dessen Text gelöscht werden soll, identifizieren
      ifelse(ref.para < 10, 
             paragraph <- str_c("[§]", ref.para),
             paragraph <- str_c("[§] ", ref.para))
      ref.start <- grep(str_c("\\b", paragraph, "\\b"), text.ref.orig[[1]]) 
      
      ifelse(ref.para < 8, 
             ref.end <- str_c("[§]", ref.para+1), 
             ref.end <- str_c("[§] ", ref.para+1))
      ref.end <- grep(str_c("\\b", ref.end, "\\b"), text.ref.orig[[1]]) 
      if (length(ref.end) == 0){ref.end <- length(text.ref.orig[[1]])}
      
      text.ref <- text.ref.orig[[1]][ref.start:(ref.end-1)]
      
      # Absatz neu nummerieren
      absatz <- str_c("[(]", ref.absatz, "[)]")
      absatz.line <- grep(absatz, text.ref) 
      text.ref[absatz.line] <- str_c("(", ref.absatz.neu, ")")
      
      # An ursprüngliches Textobjekt anklippen
      text.ref.orig <- c(text.ref.orig[[1]][1:ref.start-1], text.ref, text.ref.orig[[1]][ref.end:lengths(text.ref.orig)])
      
    }
    
    # An ursprüngliches Textobjekt anklippen
    bill_list.new[[ref.art]][ref.num][[1]] <- text.ref.orig
    bill_list <<- bill_list.new
    }
    
  }  
    
  
  
  
    
  # ---------------------------------------------------------------------------------------------------
  # Ebene: Buchstabe
  if (is.na(ref.art) == F & is.na(ref.num) == F & is.na(ref.buch) == F & is.na(ref.buchbuch) == T) {
  # ---------------------------------------------------------------------------------------------------
  
    text.ref.orig <- bill_list.new[[ref.art]][[ref.num]][ref.buch]
    
    # --------------------------------------------------
    # Direkt Buchstabe neu nummerieren
    if (is.na(ref.para) == T & is.na(ref.absatz) == T) {
    # --------------------------------------------------  
      
      # Buchstabe shiften und altes Objekt loeschen
      bill_list[[ref.art]][[ref.num]][ref.buch.neu] <<- bill_list[[ref.art]][[ref.num]][ref.buch]
      loeschen(ref.art=ref.art, ref.num=ref.num, ref.buch=ref.buch, e.list=T) 
      
      # Buchstaben-Refferenz im raw text ändern
      str_sub(bill_list[[ref.art]][[ref.num]][ref.buch.neu][[1]][1],1,1) <<- str_sub(ref.buch.neu,-1,-1)
  } else {
    
    # --------------------------------------------------
    # Paragraph innerhalb Buchstabe neu nummerieren
    if (is.na(ref.para) == F & is.na(ref.absatz) == T) {
    # --------------------------------------------------
      ifelse(ref.para < 10, 
             paragraph <- str_c("[§]", ref.para),
             paragraph <- str_c("[§] ", ref.para))
      paragraph.line <- grep(str_c("\\b", paragraph, "\\b"), text.ref.orig)  
      
      "
      Old version
      paragraph <- str_c('§', ref.para)
      paragraph.line <- grep(paragraph, text.ref.orig) 
      "
      
      text.ref.orig[paragraph.line] <- str_c("§", ref.para.neu)
    }
    
    # --------------------------------------------------
    # Absatz innerhalb Buchstabe neu nummerieren
    if (is.na(ref.para) == T & is.na(ref.absatz) == F) {
      # --------------------------------------------------
      absatz <- str_c("(", ref.absatz, ")")
      absatz.line <- grep(absatz, text.ref.orig) 
      text.ref.orig[absatz.line] <- str_c("(", ref.absatz.neu, ")")
    }
    
    # ---------------------------------------------------------------
    # Absatz innerhalb Paragraph innerhalb Buchstabe neu nummerieren
    if (is.na(ref.para) == F & is.na(ref.absatz) == F) {
      # -------------------------------------------------------------
      
      # Text des Paragraphen, innerhalb dessen Text gelöscht werden soll, identifizieren
      ifelse(ref.para < 10, 
             paragraph <- str_c("[§]", ref.para),
             paragraph <- str_c("[§] ", ref.para))
      ref.start <- grep(str_c("\\b", paragraph, "\\b"), text.ref.orig[[1]]) 
      
      ifelse(ref.para < 9, 
             ref.end <- str_c("[§]", ref.para+1), 
             ref.end <- str_c("[§] ", ref.para+1))
      ref.end <- grep(str_c("\\b", ref.end, "\\b"), text.ref.orig[[1]]) 
      if (length(ref.end) == 0){ref.end <- length(text.ref.orig[[1]])}
      
      text.ref <- text.ref.orig[[1]][ref.start:(ref.end-1)]
      
      # Absatz neu nummerieren
      absatz <- str_c("[(]", ref.absatz, "[)]")
      absatz.line <- grep(absatz, text.ref) 
      text.ref[absatz.line] <- str_c("(", ref.absatz.neu, ")")
      
      # An ursprüngliches Textobjekt anklippen
      text.ref.orig <- c(text.ref.orig[[1]][1:ref.start-1], text.ref, text.ref.orig[[1]][ref.end:lengths(text.ref.orig)])
      
    }
    
    # An ursprüngliches Textobjekt anklippen
    bill_list.new[[ref.art]][[ref.num]][ref.buch][[1]] <- text.ref.orig
    bill_list <<- bill_list.new
    }
    
  }
  
  
  
  
  # ---------------------------------------------------------------------------------------------------
  # Ebene: Buchstabe/Buchstabe
  if (is.na(ref.art) == F & is.na(ref.num) == F & is.na(ref.buch) == F & is.na(ref.buchbuch) == F) {
  # ---------------------------------------------------------------------------------------------------
  
    text.ref.orig <- bill_list.new[[ref.art]][[ref.num]][[ref.buch]][ref.buchbuch]
    
    # --------------------------------------------------
    # Direkt Buchstabe/Buchstabe neu nummerieren
    if (is.na(ref.para) == T & is.na(ref.absatz) == T) {
    # --------------------------------------------------  
      
      # Buchstabe/Buchstabe shiften und altes Objekt loeschen
      bill_list[[ref.art]][[ref.num]][[ref.buch]][ref.buchbuch.neu] <<- bill_list[[ref.art]][[ref.num]][[ref.buch]][ref.buchbuch]
      loeschen(ref.art=ref.art, ref.num=ref.num, ref.buch=ref.buch, ref.buchbuch=ref.buchbuch, e.list=T) 
      
      # Buchstabe/Buchstabe-Refferenz im raw text ändern
      str_sub(bill_list[[ref.art]][[ref.num]][[ref.buch]][ref.buchbuch.neu][[1]][1],1,2) <<- str_c(str_sub(ref.buchbuch.neu,-1,-1), str_sub(ref.buchbuch.neu,-1,-1)) 
      
    } else {
      
      # ----------------------------------------------------------
      # Paragraph innerhalb Buchstabe/Buchstabe neu nummerieren
      if (is.na(ref.para) == F & is.na(ref.absatz) == T) {
      # --------------------------------------------------------
        ifelse(ref.para < 10, 
               paragraph <- str_c("[§]", ref.para),
               paragraph <- str_c("[§] ", ref.para))
        paragraph.line <- grep(str_c("\\b", paragraph, "\\b"), text.ref.orig)  
        
        "
        Old version
        paragraph <- str_c('§', ref.para)
        paragraph.line <- grep(paragraph, text.ref.orig) 
        "
        text.ref.orig[paragraph.line] <- str_c("§", ref.para.neu)
      }
      
      # -------------------------------------------------------
      # Absatz innerhalb Buchstabe/Buchstabe neu nummerieren
      if (is.na(ref.para) == T & is.na(ref.absatz) == F) {
        # -----------------------------------------------------
        absatz <- str_c("(", ref.absatz, ")")
        absatz.line <- grep(absatz, text.ref.orig) 
        text.ref.orig[absatz.line] <- str_c("(", ref.absatz.neu, ")")
      }
      
      # --------------------------------------------------------------------------
      # Absatz innerhalb Paragraph innerhalb Buchstabe/Buchstabe neu nummerieren
      if (is.na(ref.para) == F & is.na(ref.absatz) == F) {
      # ------------------------------------------------------------------------
        
        # Text des Paragraphen, innerhalb dessen Text gelöscht werden soll, identifizieren
        ifelse(ref.para < 10, 
               paragraph <- str_c("[§]", ref.para),
               paragraph <- str_c("[§] ", ref.para))
        ref.start <- grep(str_c("\\b", paragraph, "\\b"), text.ref.orig[[1]]) 
        
        ifelse(ref.para < 8, 
               ref.end <- str_c("[§]", ref.para+1), 
               ref.end <- str_c("[§] ", ref.para+1))
        ref.end <- grep(str_c("\\b", ref.end, "\\b"), text.ref.orig[[1]]) 
        if (length(ref.end) == 0){ref.end <- length(text.ref.orig[[1]])}
        
        text.ref <- text.ref.orig[[1]][ref.start:(ref.end-1)]
        
        # Absatz neu nummerieren
        absatz <- str_c("[(]", ref.absatz, "[)]")
        absatz.line <- grep(absatz, text.ref) 
        text.ref[absatz.line] <- str_c("(", ref.absatz.neu, ")")
        
        # An ursprüngliches Textobjekt anklippen
        text.ref.orig <- c(text.ref.orig[[1]][1:ref.start-1], text.ref, text.ref.orig[[1]][ref.end:lengths(text.ref.orig)])
        
      }
      
      # An ursprüngliches Textobjekt anklippen
      bill_list.new[[ref.art]][[ref.num]][[ref.buch]][ref.buchbuch][[1]] <- text.ref.orig
      bill_list <<- bill_list.new
    }
 
  }
  
}
  
# Funktion testen
# neunummerieren(ref.art="Artikel_2", ref.art.neu="Artikel_1")  
# neunummerieren(ref.art="Artikel_1", ref.num="Nummer_7", ref.buch="Buchstabe_a", ref.buchbuch="Buchstabe_bb", ref.buchbuch.neu="Buchstabe_aa") 
# 
# neunummerieren(ref.art="Artikel_1", ref.num="Nummer_3", ref.para=10, ref.absatz=6, ref.absatz.neu=5)    
# 
# neunummerieren(ref.art="Artikel_1", ref.num="Nummer_2", ref.buch="Buchstabe_b", ref.buch.neu ="Buchstabe_a")  
# neunummerieren(ref.art="Artikel_1", ref.num="Nummer_18", ref.buch="Buchstabe_b", ref.buchbuch="Buchstabe_bb", ref.buchbuch.neu="Buchstabe_aa") 
 
  
  
# -------------------------------------------------------------
# -------------------------------------------------------------
# --- Text hinzufügen (nachfolgende Ordnung bleibt erhalten)
# -------------------------------------------------------------
# -------------------------------------------------------------

hinzufuegen <- function(ref.art=NA, ref.num=NA, ref.buch=NA, ref.buchbuch=NA, ref.para=NA, ref.absatz=NA, ref.art.neu=NA, ref.num.neu=NA, ref.buch.neu=NA, ref.buchbuch.neu=NA, ref.para.neu=NA, ref.absatz.neu=NA, text.neu=NA, e.list){
  
  # Neues Objekt erstellen
  bill_list.new <<- bill_list  
  
  # ----------------------------------------------------------------
  # -----------------------------------------------------------
  # Spezifischen Sinnabschnitt neu hinzufügen
  # -----------------------------------------------------------
  # ----------------------------------------------------------------
  
  if (e.list == T) {
    
    # -----------------------------
    # Ebene: Artikel hinzufügen
    if (is.na(ref.art.neu) == F){
    # -----------------------------
      bill_list.new[ref.art.neu] <- text.neu
    }
    
    # ---------------------------------------------------
    # Ebene: Nummer hinzufügen
    if (is.na(ref.art) == F & is.na(ref.num.neu) == F){
    # ---------------------------------------------------
      bill_list.new[[ref.art]][ref.num.neu] <- text.neu
    }
    
    # ---------------------------------------------------------------------------
    # Ebene: Buchstabe hinzufügen
    if (is.na(ref.art) == F & is.na(ref.num) == F & is.na(ref.buch.neu) == F){
    # ---------------------------------------------------------------------------
      bill_list.new[[ref.art]][[ref.num]][ref.buch.neu] <- text.neu
    }
    
    # -----------------------------------------------------------------------------------------------------
    # Ebene: Buchstabe/Buchstabe hinzufügen
    if (is.na(ref.art) == F & is.na(ref.num) == F & is.na(ref.buch) == F & is.na(ref.buchbuch.neu) == F){
    # -----------------------------------------------------------------------------------------------------
      bill_list.new[[ref.art]][[ref.num]][[ref.buch]][ref.buchbuch.neu] <- text.neu
    }
  
    
  } else if (e.list == F) {
  
  
    
  # --------------------------------------------------------------------------------------------
  # --------------------------------------------------------------------------------------------
  # Text (Paragraphen, Absatz, Sätze) an das Ende eines Sinnabschnitts hinzufügen
  # --------------------------------------------------------------------------------------------
  # --------------------------------------------------------------------------------------------
  
    if (is.na(ref.para) == T & is.na(ref.absatz) == T) {
    
      # ---------------------------------------------------------------------------------------------------
      # Ebene: Artikel
      if (is.na(ref.art) == F & is.na(ref.num) == T & is.na(ref.buch) == T & is.na(ref.buchbuch) == T ) {
      # ---------------------------------------------------------------------------------------------------
        bill_list.new[[ref.art]] <- c(bill_list.new[ref.art][[1]], text.neu[[1]])
      }
      
      # ---------------------------------------------------------------------------------------------------
      # Ebene: Nummer
      if (is.na(ref.art) == F & is.na(ref.num) == F & is.na(ref.buch) == T & is.na(ref.buchbuch) == T ) {
      # ---------------------------------------------------------------------------------------------------
        bill_list.new[[ref.art]][[ref.num]] <- c(bill_list.new[[ref.art]][ref.num][[1]], text.neu[[1]])
      }
      
      # ---------------------------------------------------------------------------------------------------
      # Ebene: Buchstabe
      if (is.na(ref.art) == F & is.na(ref.num) == F & is.na(ref.buch) == F & is.na(ref.buchbuch) == T ) {
      # ---------------------------------------------------------------------------------------------------
        bill_list.new[[ref.art]][[ref.num]][[ref.buch]] <- c(bill_list.new[[ref.art]][[ref.num]][ref.buch][[1]], text.neu[[1]])
      }
      
      # ---------------------------------------------------------------------------------------------------
      # Ebene: Buchstabe/Buchstabe
      if (is.na(ref.art) == F & is.na(ref.num) == F & is.na(ref.buch) == F & is.na(ref.buchbuch) == F ) {
      # ---------------------------------------------------------------------------------------------------
        bill_list.new[[ref.art]][[ref.num]][[ref.buch]][[ref.buchbuch]] <- c(bill_list.new[[ref.art]][[ref.num]][[ref.buch]][ref.buchbuch][[1]], text.neu[[1]])
      }
  
    }
  
    
    
  # -------------------------------------------------------------------------------------------------------------
  # -------------------------------------------------------------------------------------------------------------
  # Text (Absatz/Sätze) innerhalb eines Sinnabschnitts an Ende eines bestimmten Paragpahen/Absatzes hinzufügen
  # -------------------------------------------------------------------------------------------------------------
  # -------------------------------------------------------------------------------------------------------------
    
    # Zu bearbeitendes Textobjekt festlegen
    if (is.na(ref.art) == F & is.na(ref.num) == T & is.na(ref.buch) == T & is.na(ref.buchbuch) == T ) {
      text.ref.orig <- bill_list.new[ref.art]
    } else if (is.na(ref.art) == F & is.na(ref.num) == F & is.na(ref.buch) == T & is.na(ref.buchbuch) == T ) { 
      text.ref.orig <- bill_list.new[[ref.art]][ref.num]
    } else if (is.na(ref.art) == F & is.na(ref.num) == F & is.na(ref.buch) == F & is.na(ref.buchbuch) == T ) { 
      text.ref.orig <- bill_list.new[[ref.art]][[ref.num]][ref.buch]
    } else if (is.na(ref.art) == F & is.na(ref.num) == F & is.na(ref.buch) == F & is.na(ref.buchbuch) == F ) { 
      text.ref.orig <- bill_list.new[[ref.art]][[ref.num]][[ref.buch]][ref.buchbuch]
    }
    
    # ------------------------------------------------------------
    # Text (Absatz/Sätze) am Ende eines Paragraphen hinzufügen
    if (is.na(ref.para) == F & is.na(ref.absatz) == T) {
    # ------------------------------------------------------------
  
      # Zeile des Hinzufügens lokalisieren
      ifelse(ref.para < 9, 
             paragraph <- str_c("[§]", ref.para+1),
             paragraph <- str_c("[§] ", ref.para+1))
      paragraph.line <- grep(str_c("\\b", paragraph, "\\b"), text.ref.orig[[1]])  
      if (length(paragraph.line) == 0){paragraph.line <- length(text.ref.orig[[1]])}
      
      # Text anklippen
      text.ref.orig <- c(text.ref.orig[[1]][1:(paragraph.line-1)], text.neu[[1]], text.ref.orig[[1]][paragraph.line:lengths(text.ref.orig)])
      
  }
    
    
  # ----------------------------------------------------------------------------------
  # Text (Satz/Sätze) innerhalb eines Paragraphen am Ende eines Absatzes hinzufügen
  if (is.na(ref.para) == F & is.na(ref.absatz) == F) {
  # ----------------------------------------------------------------------------------
    
    # Paragraph in dem wir arbeiten lokalisieren
    ifelse(ref.para < 10, 
           paragraph <- str_c("[§]", ref.para),
           paragraph <- str_c("[§] ", ref.para))
    paragraph.line <- grep(str_c("\\b", paragraph, "\\b"), text.ref.orig[[1]])  
    
    ifelse(ref.para < 9, 
           paragraph <- str_c("[§]", ref.para+1),
           paragraph <- str_c("[§] ", ref.para+1))
    next.paragraph.line <- grep(str_c("\\b", paragraph, "\\b"), text.ref.orig[[1]]) 
    if (length(next.paragraph.line) == 0){next.paragraph.line <- length(text.ref.orig[[1]])}
    
    paragraph.text <- text.ref.orig[[1]][paragraph.line:(next.paragraph.line-1)]
    
    # Absatz lokalisieren
    next.absatz <- str_c("[(]", ref.absatz+1, "[)]") 
    next.absatz.line <- grep(next.absatz, paragraph.text) 
    if (length(next.absatz.line) == 0){next.absatz.line <- length(paragraph.text)}
    
    # Text anklippen
    paragraph.text.new <- c(paragraph.text[1:(next.absatz.line-1)], text.neu[[1]], paragraph.text[next.absatz.line:length(paragraph.text)])
    text.ref.orig <- c(text.ref.orig[[1]][1:(paragraph.line-1)], paragraph.text.new, text.ref.orig[[1]][next.paragraph.line:lengths(text.ref.orig)])
    
    
    
  }
    
 
    # Textobjekt in Ursprungsobjekt einfügen
    if (is.na(ref.art) == F & is.na(ref.num) == T & is.na(ref.buch) == T & is.na(ref.buchbuch) == T ) {
      bill_list.new[[ref.art]] <- text.ref.orig 
    } else if (is.na(ref.art) == F & is.na(ref.num) == F & is.na(ref.buch) == T & is.na(ref.buchbuch) == T ) { 
      bill_list.new[[ref.art]][[ref.num]] <- text.ref.orig 
    } else if (is.na(ref.art) == F & is.na(ref.num) == F & is.na(ref.buch) == F & is.na(ref.buchbuch) == T ) { 
      bill_list.new[[ref.art]][[ref.num]][[ref.buch]] <- text.ref.orig
    } else if (is.na(ref.art) == F & is.na(ref.num) == F & is.na(ref.buch) == F & is.na(ref.buchbuch) == F ) { 
      bill_list.new[[ref.art]][[ref.num]][[ref.buch]][[ref.buchbuch]] <- text.ref.orig
    } 
    
    
  }
  
  
  
  
  bill_list <<- bill_list.new
  
}  

# text.neu <- list(c("Artikel 4", "Das ist der angefügte Artikel 4."))
# text.neu <- list(c("§ 19", "Das ist der angefügte Paragraph"))
# text.neu <- list(c("(4) Neuer Absatz", "Das ist der angefügte Absatz"))
# 
# hinzufuegen(ref.art="Artikel_1", ref.num="Nummer_4", ref.para=17, ref.absatz=2, text.neu=text.neu, e.list=F)  
  
  


# ---------------------------------------------------------
# ---------------------------------------------------------
# --- Text einfügen (nachfolgende Ordnung wird geändert)
# ---------------------------------------------------------
# ---------------------------------------------------------

einfuegen <- function(ref.art=NA, ref.num=NA, ref.buch=NA, ref.buchbuch=NA, ref.para=NA, ref.absatz=NA, ref.satz=NA, spec.satz=NA, spec.wort=NA, ref.art.neu=NA, ref.num.neu=NA, ref.buch.neu=NA, ref.buchbuch.neu=NA, ref.para.neu=NA, ref.absatz.neu=NA, ref.satz.neu=NA, text.neu=NA, e.list){
  
  # ref.x-Paramater: Stelle *nach* der etwas eingefügt werden soll
  
  # Neues Objekt erstellen
  bill_list.new <<- bill_list  
  
  # ----------------------------------------------------------------
  # -----------------------------------------------------------
  # Spezifischen Sinnabschnitt einfügen
  # -----------------------------------------------------------
  # ----------------------------------------------------------------
  
  if (e.list == T) {
    
    # -----------------------------
    # Ebene: Artikel einfügen
    if (is.na(ref.art.neu) == F){
    # -----------------------------
      
      # Artikel einfügen und Sinnabschnitte neunummerieren
      bill_list.new <- c(bill_list.new[1:as.numeric(str_sub(ref.art,-1,-1))], text.neu, bill_list.new[(as.numeric(str_sub(ref.art.neu,-1,-1))):length(bill_list.new)])
      names(bill_list.new) <- str_c("Artikel_", 1:length(bill_list.new))
      
      # Artikel innerhalb des Sinnabschnitts neunummerieren
      for (idx in 1:(length(bill_list.new)-as.numeric(str_sub(ref.art.neu,-1,-1))))
        bill_list.new[[str_c("Artikel_", as.numeric(str_sub(ref.art.neu,-1,-1))+idx)]][[1]][1] <- str_c("Artikel ", as.numeric(str_sub(ref.art.neu,-1,-1))+idx)
      
      }
    
    # ---------------------------------------------------
    # Ebene: Nummer einfügen
    if (is.na(ref.art) == F & is.na(ref.num.neu) == F){
    # ---------------------------------------------------
      
      # Nummer einfügen und Sinnabschnitte neunummerieren
      bill_list.new[[ref.art]] <- c(bill_list.new[[ref.art]][1:as.numeric(str_sub(ref.num.neu,-1,-1))], 
                                    text.neu, 
                                    bill_list.new[[ref.art]][(as.numeric(str_sub(ref.num.neu,-1,-1))+1):lengths(bill_list.new[ref.art])])
      names(bill_list.new[[ref.art]]) <- c("Nummer_fehlt", str_c("Nummer_", 1:(lengths(bill_list.new[ref.art])-1)))
     
      # Nummer innerhalb des Sinnabschnitts neunummerieren
      for (idx in 1:(length(bill_list.new[[ref.art]])-(as.numeric(str_sub(ref.num.neu,-1,-1))+1)))
        bill_list.new[[ref.art]][[str_c("Nummer_", as.numeric(str_sub(ref.num.neu,-1,-1))+idx)]][[1]][1] <- str_c(as.numeric(str_sub(ref.num.neu,-1,-1))+idx, str_sub(bill_list.new[[ref.art]][[str_c("Nummer_", as.numeric(str_sub(ref.num.neu,-1,-1))+idx)]][[1]][1],2,-1))
      
      
    }
    
    # ---------------------------------------------------------------------------
    # Ebene: Buchstabe einfügen
    if (is.na(ref.art) == F & is.na(ref.num) == F & is.na(ref.buch.neu) == F){
    # ---------------------------------------------------------------------------
      # Buchstabe einfügen und Sinnabschnitte neunummerieren
      bill_list.new[[ref.art]][[ref.num]] <- c(bill_list.new[[ref.art]][[ref.num]][1:(which(str_sub(ref.buch,-1,-1)==letters)+1)], 
                                               text.neu, 
                                               bill_list.new[[ref.art]][[ref.num]][(which(str_sub(ref.buch,-1,-1)==letters)+2):length(bill_list.new[[ref.art]][[ref.num]])])
      names(bill_list.new[[ref.art]][[ref.num]]) <- c("Buchstabe_fehlt", str_c("Buchstabe_", letters[1:lengths(bill_list.new[[ref.art]][ref.num])-1]))
      
      # Buchstabe innerhalb des Sinnabschnitts neunummerieren
      namen <- names(bill_list.new[[ref.art]][[ref.num]])
      for (i in 2: length(namen)) {
        bill_list.new[[ref.art]][[ref.num]][[namen[i]]][1] <- str_c(str_sub(namen[i],-1,-1), str_sub(bill_list.new[[ref.art]][[ref.num]][[namen[i]]][1],2,-1))
      }
      
      
    }
    
    # -----------------------------------------------------------------------------------------------------
    # Ebene: Buchstabe/Buchstabe einfügen
    if (is.na(ref.art) == F & is.na(ref.num) == F & is.na(ref.buch) == F & is.na(ref.buchbuch.neu) == F){
    # -----------------------------------------------------------------------------------------------------
      
      # Buchstabe/Buchstabe einfügen und Sinnabschnitte neunummerieren
      bill_list.new[[ref.art]][[ref.num]][[ref.buch]] <- c(bill_list.new[[ref.art]][[ref.num]][[ref.buch]][1:(which(str_sub(ref.buchbuch,-1,-1)==letters)+1)], 
                                               text.neu, 
                                               bill_list.new[[ref.art]][[ref.num]][[ref.buch]][(which(str_sub(ref.buchbuch,-1,-1)==letters)+2):length(bill_list.new[[ref.art]][[ref.num]][[ref.buch]])])
      names(bill_list.new[[ref.art]][[ref.num]][[ref.buch]]) <- c("Buchstabe_fehlt", str_c("Buchstabe_", letters[1:lengths(bill_list.new[[ref.art]][ref.num])-1], letters[1:lengths(bill_list.new[[ref.art]][ref.num])-1]))
      
      # Buchstabe/Buchstabe innerhalb des Sinnabschnitts neunummerieren
      namen <- names(bill_list.new[[ref.art]][[ref.num]][[ref.buch]])
      for (i in 2: length(namen)) {
        bill_list.new[[ref.art]][[ref.num]][[ref.buch]][[namen[i]]][1] <- str_c(str_sub(namen[i],-1,-1), str_sub(namen[i],-1,-1), str_sub(bill_list.new[[ref.art]][[ref.num]][[ref.buch]][[namen[i]]][1],3,-1))
      }
      
      
    }
    
    
    
  # ----------------------------------------------------------------
  # -----------------------------------------------------------
  # Text innerhalb von Sinnabschnitten einfügen
  # -----------------------------------------------------------
  # ----------------------------------------------------------------
    
  } else if (e.list == F) {
    
    # Zu bearbeitendes Textobjekt festlegen
    if (is.na(ref.art) == F & is.na(ref.num) == T & is.na(ref.buch) == T & is.na(ref.buchbuch) == T ) {
      text.ref.orig <- bill_list.new[ref.art]
    } else if (is.na(ref.art) == F & is.na(ref.num) == F & is.na(ref.buch) == T & is.na(ref.buchbuch) == T ) { 
      text.ref.orig <- bill_list.new[[ref.art]][ref.num]
    } else if (is.na(ref.art) == F & is.na(ref.num) == F & is.na(ref.buch) == F & is.na(ref.buchbuch) == T ) { 
      text.ref.orig <- bill_list.new[[ref.art]][[ref.num]][ref.buch]
    } else if (is.na(ref.art) == F & is.na(ref.num) == F & is.na(ref.buch) == F & is.na(ref.buchbuch) == F ) { 
      text.ref.orig <- bill_list.new[[ref.art]][[ref.num]][[ref.buch]][ref.buchbuch]
    }
  
    # --------------------------------------------------------------------------------------------------
    # Ebene: Artikel
    #if (is.na(ref.art) == F & is.na(ref.num) == T & is.na(ref.buch) == T & is.na(ref.buchbuch) == T) {
    # --------------------------------------------------------------------------------------------------
    
      # ------------------------------
      # Paragraphen einfügen
      if (is.na(ref.para.neu)==F) {
      # ------------------------------
      
        # Zeile des Einfügens lokalisieren
        ifelse(ref.para < 9, 
               paragraph <- str_c("[§]", ref.para+1),
               paragraph <- str_c("[§] ", ref.para+1))
        paragraph.line <- grep(str_c("\\b", paragraph, "\\b"), text.ref.orig[[1]])  
        
        # Text anklippen
        text.ref.orig <- c(text.ref.orig[[1]][1:(paragraph.line-1)], text.neu[[1]], text.ref.orig[[1]][paragraph.line:lengths(text.ref.orig)])
        
        # Folgende Paragraphen neunummerieren
        
          # Identifizieren
          folgende.paras <- grep("[§]", text.ref.orig)
          folgende.paras <- folgende.paras[folgende.paras>paragraph.line]
          
            
          for (i in 1:length(folgende.paras)){
            if (nchar(text.ref.orig[folgende.paras[i]]) > 4) {
              folgende.paras[i] <- NA
            }
          }
          folgende.paras <- na.omit(folgende.paras)
        
          # Neunummerieren
          for (i in 1:length(folgende.paras)){
            ifelse(ref.para.neu+i <10,
                   text.ref.orig[folgende.paras[i]] <- str_c("§", ref.para.neu+i),
                   text.ref.orig[folgende.paras[i]] <- str_c("§ ", ref.para.neu+i))
          }
      }
      
      
      
      # ------------------------------
      # Absatz einfügen
      if (is.na(ref.absatz.neu)==F) {
      # ------------------------------
      
        # Paragraph in dem wir arbeiten lokalisieren
        ifelse(ref.para < 10, 
               paragraph <- str_c("[§]", ref.para),
               paragraph <- str_c("[§] ", ref.para))
        paragraph.line <- grep(str_c("\\b", paragraph, "\\b"), text.ref.orig[[1]])  
        
        ifelse(ref.para < 9, 
               paragraph <- str_c("[§]", ref.para+1),
               paragraph <- str_c("[§] ", ref.para+1))
        next.paragraph.line <- grep(str_c("\\b", paragraph, "\\b"), text.ref.orig[[1]]) 
        if (length(next.paragraph.line) == 0){next.paragraph.line <- length(text.ref.orig[[1]])}
        
        paragraph.text <- text.ref.orig[[1]][paragraph.line:(next.paragraph.line-1)]
        
        # Absatz lokalisieren
        next.absatz <- str_c("[(]", ref.absatz.neu, "[)]") 
        next.absatz.line <- grep(next.absatz, paragraph.text) 
        
        # Text anklippen
        paragraph.text.new <- c(paragraph.text[1:(next.absatz.line-1)], text.neu[[1]], paragraph.text[next.absatz.line:length(paragraph.text)])
        
        # Folgende Absätze neunummerieren
          
          # Identifizieren
          folgende.absaetze <- grep(str_c("[(]", "\\d", "[)]") , paragraph.text.new)
          folgende.absaetze <- folgende.absaetze[folgende.absaetze>next.absatz.line]
          
          # Neunummerieren
          for (i in 1:length(folgende.absaetze)){
            ifelse(ref.absatz.neu+i <10,
                   str_sub(paragraph.text.new[folgende.absaetze[i]], 2, 2) <- ref.absatz.neu+i,
                   str_sub(paragraph.text.new[folgende.absaetze[i]], 2, 3) <- ref.absatz.neu+i)
          }
        
        # Text anklippen
        text.ref.orig <- c(text.ref.orig[[1]][1:(paragraph.line-1)], paragraph.text.new, text.ref.orig[[1]][next.paragraph.line:lengths(text.ref.orig)])
      
      }  
      
      
      # ------------------------------------------------------------
      # Satz einfügen (nach nummeriertem oder spezifischen Satz)
      if (is.na(ref.satz.neu)==F | is.na(spec.satz) == F) {
      # ------------------------------------------------------------
      
          # ----------------------------------------------------
          # Satz im Fließtext des Artikels einfügen
          if (is.na(ref.para) == T & is.na(ref.absatz) == T) {
          # ----------------------------------------------------
        
            # Satzgrenzen identifizieren
            satz.grenzen <- grep("\\.", text.ref.orig[[1]]) 
            
            # Datumsangaben aussortieren
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
            
            monat_zeilen <- str_detect(text.ref.orig[[1]][satz.grenzen], str_c("\\d{1,2}\\. ", monate, collapse = "|"))
            satz.grenzen <- satz.grenzen[monat_zeilen==FALSE]
            
            # Restsatz in nächste Zeile schiften 
            if (is.na(ref.satz.neu)==F){
              zeilen.id <- satz.grenzen[ref.satz]
            } else if (is.na(spec.satz) == F) {
              zeilen.id <- grep(paste(word(spec.satz,c(-2,-1)), collapse = " "), text.ref.orig)
            }
            
            splitted <- str_split(text.ref.orig[[1]][zeilen.id], pattern="\\. ")
            if (lengths(splitted) == 2){
              text.ref.orig[[1]][zeilen.id] <- str_c(sub('\\..*', '', text.ref.orig[[1]][zeilen.id]), ".")
              text.ref.orig[[1]][zeilen.id+1] <- str_c(splitted[[1]][2], " ",  text.ref.orig[[1]][zeilen.id+1])
            }
            # Neuen Satz einfügen
            text.ref.orig[[1]] <- c(text.ref.orig[[1]][1:zeilen.id], text.neu[[1]], text.ref.orig[[1]][(zeilen.id+1):length(text.ref.orig[[1]])])
            
          }
            
        
      
        # -------------------------------------------------------------------------------------
        # Satz innerhalb von Paragraphen einfügen (nach nummeriertem oder spezifischem Satz)
        if (is.na(ref.para) == F & is.na(ref.absatz) == T) {
        # -------------------------------------------------------------------------------------
        
          # Paragraph in dem wir arbeiten lokalisieren
          ifelse(ref.para < 10, 
                 paragraph <- str_c("[§]", ref.para),
                 paragraph <- str_c("[§] ", ref.para))
          paragraph.line <- grep(str_c("\\b", paragraph, "\\b"), text.ref.orig[[1]])  
          
          ifelse(ref.para < 9, 
                 paragraph <- str_c("[§]", ref.para+1),
                 paragraph <- str_c("[§] ", ref.para+1))
          next.paragraph.line <- grep(str_c("\\b", paragraph, "\\b"), text.ref.orig[[1]]) 
          if (length(next.paragraph.line) == 0){
            last.paragraph <- T
            next.paragraph.line <- length(text.ref.orig[[1]])
            paragraph.text <- text.ref.orig[[1]][paragraph.line:(next.paragraph.line)]
          } else {
            last.paragraph <- F
            paragraph.text <- text.ref.orig[[1]][paragraph.line:(next.paragraph.line-1)]
          }
          
          # Satzgrenzen identifizieren
          satz.grenzen <- grep("\\.", paragraph.text) 
          
          # Datumsangaben aussortieren
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
          
          monat_zeilen <- str_detect(paragraph.text[satz.grenzen], str_c("\\d{1,2}\\. ", monate, collapse = "|"))
          satz.grenzen <- satz.grenzen[monat_zeilen==FALSE]
          
          # Restsatz in nächste Zeile schiften 
          if (is.na(ref.satz.neu)==F){
            zeilen.id <- satz.grenzen[ref.satz]
          } else if (is.na(spec.satz) == F) {
            zeilen.id <- grep(paste(word(spec.satz,c(-2,-1)), collapse = " "), paragraph.text)
          }
          
          splitted <- str_split(paragraph.text[zeilen.id], pattern="\\. ")
          if (lengths(splitted) == 2){
            paragraph.text[zeilen.id] <- str_c(sub('\\..*', '', paragraph.text[zeilen.id]), ".")
            paragraph.text[zeilen.id+1] <- str_c(splitted[[1]][2], " ",  paragraph.text[zeilen.id+1])
          }
          # Neuen Satz einfügen
          paragraph.text <- c(paragraph.text[1:zeilen.id], text.neu[[1]], paragraph.text[(zeilen.id+1):length(paragraph.text)])
          
          # An text.ref.orig anklippen
          ifelse(last.paragraph==T, 
                 text.ref.orig[[1]] <- c(text.ref.orig[[1]][1:paragraph.line-1], paragraph.text),
                 text.ref.orig[[1]] <- c(text.ref.orig[[1]][1:paragraph.line-1], paragraph.text, text.ref.orig[[1]][next.paragraph.line:length(text.ref.orig[[1]])]))
      }
        
        
        # ----------------------------------------------------
        # Satz innerhalb von Absatz einfügen
        if (is.na(ref.para) == F & is.na(ref.absatz) == F) {
        # ----------------------------------------------------
          
          # Paragraph in dem wir arbeiten lokalisieren
          ifelse(ref.para < 10, 
                 paragraph <- str_c("[§]", ref.para),
                 paragraph <- str_c("[§] ", ref.para))
          paragraph.line <- grep(str_c("\\b", paragraph, "\\b"), text.ref.orig[[1]])  
          
          ifelse(ref.para < 9, 
                 paragraph <- str_c("[§]", ref.para+1),
                 paragraph <- str_c("[§] ", ref.para+1))
          next.paragraph.line <- grep(str_c("\\b", paragraph, "\\b"), text.ref.orig[[1]]) 
          if (length(next.paragraph.line) == 0){
            last.paragraph <- T
            next.paragraph.line <- length(text.ref.orig[[1]])
            paragraph.text <- text.ref.orig[[1]][paragraph.line:(next.paragraph.line)]
          } else {
            last.paragraph <- F
            paragraph.text <- text.ref.orig[[1]][paragraph.line:(next.paragraph.line-1)]
          }
          
          # Absatz lokalisieren
          absatz.line <- grep(str_c("[(]", ref.absatz, "[)]") , paragraph.text)
          next.absatz.line <- grep(str_c("[(]", ref.absatz+1, "[)]") , paragraph.text) 
          if (length(next.absatz.line) == 0){
            last.absatz <- T
            next.absatz.line <- length(paragraph.text)
            absatz.text <- paragraph.text[absatz.line:next.absatz.line]
          } else {
            last.absatz <- F
            absatz.text <- paragraph.text[absatz.line:(next.absatz.line-1)]
          }
          
          # Satzgrenzen identifizieren
          satz.grenzen <- grep("\\.", absatz.text) 
          
          # Datumsangaben aussortieren
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
          
          monat_zeilen <- str_detect(absatz.text[satz.grenzen], str_c("\\d{1,2}\\. ", monate, collapse = "|"))
          satz.grenzen <- satz.grenzen[monat_zeilen==FALSE]
          
          # Restsatz in nächste Zeile schiften 
          if (is.na(ref.satz.neu)==F){
            zeilen.id <- satz.grenzen[ref.satz]
          } else if (is.na(spec.satz) == F) {
            zeilen.id <- grep(paste(word(spec.satz,c(-2,-1)), collapse = " "), absatz.text)
          }
          
          splitted <- str_split(absatz.text[zeilen.id], pattern="\\. ")
          if (lengths(splitted) == 2){
            absatz.text[zeilen.id] <- str_c(sub('\\..*', '', absatz.text[zeilen.id]), ".")
            absatz.text[zeilen.id+1] <- str_c(splitted[[1]][2], " ",  absatz.text[zeilen.id+1])
          }
          # Neuen Satz einfügen
          absatz.text <- c(absatz.text[1:zeilen.id], text.neu[[1]], absatz.text[(zeilen.id+1):length(absatz.text)])
          
          # An text.ref.orig anklippen
          ifelse(last.absatz==T, 
                 paragraph.text <- c(paragraph.text[1:absatz.line-1], absatz.text),
                 paragraph.text <- c(paragraph.text[1:absatz.line-1], absatz.text, paragraph.text[next.absatz.line:length(paragraph.text)]))
          
          ifelse(last.paragraph==T, 
                 text.ref.orig[[1]] <- c(text.ref.orig[[1]][1:paragraph.line-1], paragraph.text),
                 text.ref.orig[[1]] <- c(text.ref.orig[[1]][1:paragraph.line-1], paragraph.text, text.ref.orig[[1]][next.paragraph.line:length(text.ref.orig[[1]])]))
        }
      
      }
      
      # ------------------------------------------------------------
      # Wort(e) einfügen (nach spezifischem Wort)
      if (is.na(spec.wort) == F) {
      # ------------------------------------------------------------
        
        # ----------------------------------------------------
        # Wort(e) im Fließtext des Artikels einfügen
        if (is.na(ref.para) == T & is.na(ref.absatz) == T) {
        # ----------------------------------------------------
      
        wort.zeile <- grep(spec.wort, text.ref.orig[[1]])
        text.ref.orig[[1]][wort.zeile] <- str_c(sub(str_c(spec.wort, ".*"), '', text.ref.orig[[1]][wort.zeile]), spec.wort, " ", text.neu, sub(str_c(".*",spec.wort), '', text.ref.orig[[1]][wort.zeile]))
 
        }
      
        # ------------------------------------------------------
        # Wort(e) innerhalb von Paragraphen einfügen
        if (is.na(ref.para) == F & is.na(ref.absatz) == T) {
        # ------------------------------------------------------
 
        # Paragraph in dem wir arbeiten lokalisieren
        ifelse(ref.para < 10, 
               paragraph <- str_c("[§]", ref.para),
               paragraph <- str_c("[§] ", ref.para))
        paragraph.line <- grep(str_c("\\b", paragraph, "\\b"), text.ref.orig[[1]])  
        
        ifelse(ref.para < 9, 
               paragraph <- str_c("[§]", ref.para+1),
               paragraph <- str_c("[§] ", ref.para+1))
        next.paragraph.line <- grep(str_c("\\b", paragraph, "\\b"), text.ref.orig[[1]]) 
        if (length(next.paragraph.line) == 0){
          last.paragraph <- T
          next.paragraph.line <- length(text.ref.orig[[1]])
          paragraph.text <- text.ref.orig[[1]][paragraph.line:(next.paragraph.line)]
        } else {
          last.paragraph <- F
          paragraph.text <- text.ref.orig[[1]][paragraph.line:(next.paragraph.line-1)]
        }
          
        # Wort hinzufügen
        wort.zeile <- grep(spec.wort, paragraph.text)
        paragraph.text[wort.zeile] <- str_c(sub(str_c(spec.wort, ".*"), '', paragraph.text[wort.zeile]), spec.wort, " ", text.neu, sub(str_c(".*",spec.wort), '', paragraph.text[wort.zeile]))
       
        # Paragraph anklippen 
        ifelse(last.paragraph==T, 
               text.ref.orig[[1]] <- c(text.ref.orig[[1]][1:paragraph.line-1], paragraph.text),
               text.ref.orig[[1]] <- c(text.ref.orig[[1]][1:paragraph.line-1], paragraph.text, text.ref.orig[[1]][next.paragraph.line:length(text.ref.orig[[1]])]))
        
        }
        
        
        # -----------------------------------------------------
        # Wort(e) innerhalb von Absatz einfügen
        if (is.na(ref.para) == F & is.na(ref.absatz) == F) {
        # -----------------------------------------------------
      
        # Paragraph in dem wir arbeiten lokalisieren
        ifelse(ref.para < 10, 
               paragraph <- str_c("[§]", ref.para),
               paragraph <- str_c("[§] ", ref.para))
        paragraph.line <- grep(str_c("\\b", paragraph, "\\b"), text.ref.orig[[1]])  
        
        ifelse(ref.para < 9, 
               paragraph <- str_c("[§]", ref.para+1),
               paragraph <- str_c("[§] ", ref.para+1))
        next.paragraph.line <- grep(str_c("\\b", paragraph, "\\b"), text.ref.orig[[1]]) 
        if (length(next.paragraph.line) == 0){
          last.paragraph <- T
          next.paragraph.line <- length(text.ref.orig[[1]])
          paragraph.text <- text.ref.orig[[1]][paragraph.line:(next.paragraph.line)]
        } else {
          last.paragraph <- F
          paragraph.text <- text.ref.orig[[1]][paragraph.line:(next.paragraph.line-1)]
        }
        
        # Absatz lokalisieren
        absatz.line <- grep(str_c("[(]", ref.absatz, "[)]") , paragraph.text)
        next.absatz.line <- grep(str_c("[(]", ref.absatz+1, "[)]") , paragraph.text) 
        if (length(next.absatz.line) == 0){
          last.absatz <- T
          next.absatz.line <- length(paragraph.text)
          absatz.text <- paragraph.text[absatz.line:next.absatz.line]
        } else {
          last.absatz <- F
          absatz.text <- paragraph.text[absatz.line:(next.absatz.line-1)]
        }  
          
        # Wort hinzufügen
        wort.zeile <- grep(spec.wort, absatz.text)
        absatz.text[wort.zeile] <- str_c(sub(str_c(spec.wort, ".*"), '', absatz.text[wort.zeile]), spec.wort, " ", text.neu, sub(str_c(".*",spec.wort), '', absatz.text[wort.zeile]))
        
        # An text.ref.orig anklippen
        ifelse(last.absatz==T, 
               paragraph.text <- c(paragraph.text[1:absatz.line-1], absatz.text),
               paragraph.text <- c(paragraph.text[1:absatz.line-1], absatz.text, paragraph.text[next.absatz.line:length(paragraph.text)]))
        
        ifelse(last.paragraph==T, 
               text.ref.orig[[1]] <- c(text.ref.orig[[1]][1:paragraph.line-1], paragraph.text),
               text.ref.orig[[1]] <- c(text.ref.orig[[1]][1:paragraph.line-1], paragraph.text, text.ref.orig[[1]][next.paragraph.line:length(text.ref.orig[[1]])]))
        
        }
        
        
      }
      
      
    
    #}
    
    
    # Textobjekt in Ursprungsobjekt einfügen
    if (is.na(ref.art) == F & is.na(ref.num) == T & is.na(ref.buch) == T & is.na(ref.buchbuch) == T ) {
      bill_list.new[[ref.art]] <- text.ref.orig 
    } else if (is.na(ref.art) == F & is.na(ref.num) == F & is.na(ref.buch) == T & is.na(ref.buchbuch) == T ) { 
      bill_list.new[[ref.art]][[ref.num]] <- text.ref.orig 
    } else if (is.na(ref.art) == F & is.na(ref.num) == F & is.na(ref.buch) == F & is.na(ref.buchbuch) == T ) { 
      bill_list.new[[ref.art]][[ref.num]][[ref.buch]] <- text.ref.orig
    } else if (is.na(ref.art) == F & is.na(ref.num) == F & is.na(ref.buch) == F & is.na(ref.buchbuch) == F ) { 
      bill_list.new[[ref.art]][[ref.num]][[ref.buch]][[ref.buchbuch]] <- text.ref.orig
    } 
    
    
  }
  
  bill_list <<- bill_list.new   
  
}


# # Artikel einfügen
# text.neu <- list(c("Artikel 4", "Das ist der angefügte Artikel 4."))
# einfuegen(ref.art="Artikel_3", ref.art.neu="Artikel_4", text.neu=text.neu, e.list=T)  
# 
# # Nummer einfügen
# text.neu <- list(c("2. Das ist die neue Nummer zwei.", "Hier kommt mehr Text."))
# einfuegen(ref.art="Artikel_1", ref.num="Nummer_1", ref.num.neu="Nummer_2", text.neu=text.neu, e.list=T)  
# 
# # Buchstabe einfügen
# text.neu <- list(c("b) Das ist der neue Buchstabe b.", "Hier kommt mehr Text."))
# einfuegen(ref.art="Artikel_1", ref.num="Nummer_2", ref.buch="Buchstabe_a", ref.buch.neu="Buchstabe_b", text.neu=text.neu, e.list=T)  
# 
# # Buchstabe/Buchstabe einfügen
# text.neu <- list(c("bb) Das ist der neue Buchstabe bb.", "Hier kommt mehr Text."))
# einfuegen(ref.art="Artikel_1", ref.num="Nummer_19", ref.buch="Buchstabe_a", ref.buchbuch="Buchstabe_aa", ref.buchbuch.neu="Buchstabe_bb", text.neu=text.neu, e.list=T)  
# 
# # Paragraphen einfügen (16_2231)
# bill_list$Artikel_1 <- bill_list$Artikel_1$Nummer_4
# text.neu <- list(c("§ 17", "Das ist der neue Paragraph 17.", "Hier kommt mehr Text."))
# einfuegen(ref.art="Artikel_1", ref.num="Nummer_4", ref.para=16, ref.para.neu=17, text.neu=text.neu, e.list=F)
# 
# # Absatz einfügen (16_2231)
# text.neu <- list(c("(3) Das ist der neue Absatz 3.", "Hier kommt mehr Text."))
# einfuegen(ref.art="Artikel_1", ref.para=15, ref.absatz=2, ref.absatz.neu=3, text.neu=text.neu, e.list=F)
# 
# # Satz einfügen im Fließtext (16_2331)
# bill_list$Artikel_1 <- bill_list.original$Artikel_1$Nummer_4[75:105]
# text.neu <- list(c("Das hier ist der neue Satz,", "der über zwei Zeilen verläuft."))
# 
#   # Nummeriert
#   einfuegen(ref.art="Artikel_1", ref.satz=2, ref.satz.neu=3, text.neu=text.neu, e.list=F)
# 
#   # Nach spezifischem Satz
#   spec.satz <- "Dieser berücksichtigt raumstrukturelle, auf den öffentlichen Personennahverkehr bezogene und leistungsbezogene Parameter."
#   einfuegen(ref.art="Artikel_1", spec.satz=spec.satz, text.neu=text.neu, e.list=F)
#   
# # Satz einfügen in Paragraph (16_2331)  
# text.neu <- list(c("Das hier ist der neue Satz,", "der über zwei Zeilen verläuft."))  
# 
#   # Nummeriert
#   einfuegen(ref.art="Artikel_1", ref.para=18, ref.satz=2, ref.satz.neu=3, text.neu=text.neu, e.list=F)
#   
# # Satz einfügen in Absatz (16_2331)
#   
#   # Nummeriert
#   einfuegen(ref.art="Artikel_1", ref.para=17, ref.absatz=1, ref.satz=1, ref.satz.neu=2, text.neu=text.neu, e.list=F)
#   
#   # Nach spezifischem Satz
#   spec.satz <- "eine einheitliche Rabattierung nach § 16 sicher"
#   einfuegen(ref.art="Artikel_1", ref.para=17, ref.absatz=1, spec.satz=spec.satz, text.neu=text.neu, e.list=F)
#   
# # Wort(e) einfügen nach spezifischem Wort
# 
#   # Ebene Artikel
#   spec.wort <- "Personenbeförderungsgesetzes"
#   text.neu <- "NEUE WORTE"
#   einfuegen(ref.art="Artikel_2", spec.wort=spec.wort, text.neu=text.neu, e.list=F)
#   
#   # Paragraph innerhalb Artikel
#   spec.wort <- "Verkehrskooperation"
#   text.neu <- "NEUE WORTE"
#   einfuegen(ref.art="Artikel_1", ref.para=17, spec.wort=spec.wort, text.neu=text.neu, e.list=F)
#   
#   # Absatz innerhalb Paragraph/Artikel
#   spec.wort <- "Verkehrsministerium"
#   text.neu <- "NEUE WORTE"
#   einfuegen(ref.art="Artikel_1", ref.para=18, ref.absatz=3, spec.wort=spec.wort, text.neu=text.neu, e.list=F)
#   
  
# ---------------------------------------------------------------------------------------------------------------------------------
# Zur Neunummerierung: Die einfuegen-Funktion nummeriert automatisch neu. Die neunummerierungs-Funktion also im Information retrieval
# nur aktivieren, wenn nicht von "bisherigen" Artikeln/Nummern/Paragraphen gesprochen wird. 
#
# Generell wichtig 
# Welcher Ausgangstext soll amendet werden? (explicitref? Bereits eingearbeitete Änderungen der Beschlussempfehlung)
# ---------------------------------------------------------------------------------------------------------------------------------




























