# -------------------------------------------------------------------------
# Erstellung des Gesetzesentwurfs nach parteispezifischer Präferenz
# -------------------------------------------------------------------------
library(stringr)

amendmentprop$action_matrix <- NA 
amendmentprop$manual <- 0 

for (amendmentprop_row in 1: nrow(amendmentprop)){

tryCatch({
  
# --------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------
# ----------------------------------
# ----- TEIL I: Geplante Aktionen aus einzelnen Änderungsvorschlägen in action_matrix auslesen
# ----------------------------------
# --------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------

# Loop über alle entsprechenden amendment_lists, die im Ausschuss verändert werden
if (is.null(names(amendmentprop$amendment_list[amendmentprop_row][[1]][[1]][[1]][[1]])) == F)
  amendment_list <- amendmentprop$amendment_list[amendmentprop_row][[1]][[1]][[1]][[1]]
if (is.null(names(amendmentprop$amendment_list[amendmentprop_row][[1]][[1]][[1]])) == F)
  amendment_list <- amendmentprop$amendment_list[amendmentprop_row][[1]][[1]][[1]]
if (is.null(names(amendmentprop$amendment_list[amendmentprop_row][[1]][[1]])) == F)
  amendment_list <- amendmentprop$amendment_list[amendmentprop_row][[1]][[1]]
if (is.null(names(amendmentprop$amendment_list[amendmentprop_row][[1]])) == F)
  amendment_list <- amendmentprop$amendment_list[amendmentprop_row][[1]]

if (is.null(names(amendmentprop$amendment_list[amendmentprop_row][[1]][[1]][[1]][[1]])) == T &
    is.null(names(amendmentprop$amendment_list[amendmentprop_row][[1]][[1]][[1]])) == T &
    is.null(names(amendmentprop$amendment_list[amendmentprop_row][[1]][[1]])) == T &
    is.null(names(amendmentprop$amendment_list[amendmentprop_row][[1]])) == T) 
  amendment_list <- amendmentprop$amendment_list[amendmentprop_row][[1]]


if (is.list(amendment_list) == F){ amendment_list <- list(amendment_list)}


# ---------------------------------------------------------------------------------------
# Szenario: Beschlussempfehlung beschließt Änderungen am ursprünglichen Gesetzesentwurf
# ---------------------------------------------------------------------------------------

# ------------------------------------------------------
# Leere Matrix für gesamten Änderungsantrag erstellen
# ------------------------------------------------------
list.dims <- names(unlist(rapply(amendment_list, length, how="list")))
list.dims <- gsub("\\.", "$",list.dims)
list.dims <- str_c("$", list.dims)

action_matrix <- matrix(NA, nrow=length(list.dims), ncol=1+18+5+1)
colnames(action_matrix) <- c("vorschlag.idx", "ref.art", "ref.num", "ref.buch", "ref.buchbuch", "ref.para", "ref.absatz", "ref.satz", "spec.satz", "spec.wort", "ref.art.neu", "ref.num.neu", "ref.buch.neu", "ref.buchbuch.neu", "ref.para.neu", "ref.absatz.neu", "ref.satz.neu", "text.neu", "e.list", "loeschen", "neunummerieren", "neufassen", "hinzufuegen", "einfuegen", "ref.fkt")


# ------------------------------------------------------------------
# Matrix erweitern falls mehrere Funktionen aktiviert werden
# ------------------------------------------------------------------

action_matrix[,"vorschlag.idx"] <- 1:nrow(action_matrix)

# Zuallererst: schauen ob Vorschläge mehrere Funktionen aktivieren und zuerst Matrix erweitern
# Also: Wenn innerhalb eines Vorschlags mehrere Keywords vorkommen ("gestrichen"/"aufgehoben" und(!) "angefügt")
# Dazu einfach mal alle Änderungsanträge durchgehen und alle Keywords aufschreiben
action_words <- c("geändert",
                  "gestrichen", "aufgehoben", "gelöscht",
                  "angefügt", "hinzugefügt",
                  "eingefügt", "eingeschoben",
                  "gefasst", "neugefasst", "ersetzt", "Fassung")

loeschen_words <- c("gestrichen", "aufgehoben", "gelöscht")
hinzufuegen_words <- c("angefügt", "hinzugefügt")
einfuegen_words <- c("eingefügt", "eingeschoben")
neufassen_words <- c("gefasst", "neugefasst", "ersetzt", "Fassung")
neunummerieren_words <- c("werden", "wird")



library(qdapRegex) # for rm_between

activations <- matrix(NA, nrow=length(list.dims), ncol=length(action_words[-1]))

for (i in 1: length(list.dims)) {
  
  if (list.dims=="$"){
    action_text <- amendment_list
    activations[i,] <- str_count(action_text, action_words[-1])
    
  } else {
    
    action_text <- paste(eval(parse(text=str_c("amendment_list", list.dims[i]))), collapse = " ")
    
    if (str_detect(action_text, "“") == T)
      action_text <- rm_between(action_text, '„', '“', extract=F)
    
    if (str_detect(action_text, "‘‘") == T)
      action_text <- rm_between(action_text, '„', '‘‘', extract=F)
    
    activations[i,] <- str_count(action_text, action_words[-1])
    
  }
}

colnames(activations) <- action_words[-1]

sums <- rowSums(activations)

if (length(which(sums>1)) > 0 & (activations[i, "ersetzt"] + activations[i, "Fassung"]) < 2) 
  amendmentprop$manual[amendmentprop_row] <- 1


for (i in 1: length(sums)) {
  
  if (sums[i] == 2 & (activations[i, "ersetzt"] + activations[i, "Fassung"]) < 2) {
    
    if (i<length(sums))
      action_matrix <- rbind(action_matrix[1:(i+nrow(action_matrix)-length(sums)),], action_matrix[(i+nrow(action_matrix)-length(sums)),], action_matrix[((i+nrow(action_matrix)-length(sums))+1):nrow(action_matrix),])
    if (i==length(sums))
      action_matrix <- rbind(action_matrix[1:(i+nrow(action_matrix)-length(sums)),], action_matrix[(i+nrow(action_matrix)-length(sums)),])
    
  } else if (sums[i] == 3) {
    
    if (i<length(sums))
      action_matrix <- rbind(action_matrix[1:(i+nrow(action_matrix)-length(sums)),], action_matrix[(i+nrow(action_matrix)-length(sums)),], action_matrix[(i+nrow(action_matrix)-length(sums)),], action_matrix[((i+nrow(action_matrix)-length(sums))+1):nrow(action_matrix),])
    if (i==length(sums))
      action_matrix <- rbind(action_matrix[1:(i+nrow(action_matrix)-length(sums)),], action_matrix[(i+nrow(action_matrix)-length(sums)),], action_matrix[(i+nrow(action_matrix)-length(sums)),])
    
  } else if (sums[i] == 4) {
    
    if (i<length(sums))
      action_matrix <- rbind(action_matrix[1:(i+nrow(action_matrix)-length(sums)),], action_matrix[(i+nrow(action_matrix)-length(sums)),], action_matrix[(i+nrow(action_matrix)-length(sums)),], action_matrix[(i+nrow(action_matrix)-length(sums)),], action_matrix[((i+nrow(action_matrix)-length(sums))+1):nrow(action_matrix),])
    if (i==length(sums))
      action_matrix <- rbind(action_matrix[1:(i+nrow(action_matrix)-length(sums)),], action_matrix[(i+nrow(action_matrix)-length(sums)),], action_matrix[(i+nrow(action_matrix)-length(sums)),], action_matrix[(i+nrow(action_matrix)-length(sums)),])
  }
}

if (list.dims == "$")
  list.dims <- "[[1]]"

# --------------------------------------------------------------------------------------------------------------
# Matrix auffüllen (wenn zwei verschiedene Funktionen in einem Vorschlag aufgerufen werden: Manuell kodieren)
# --------------------------------------------------------------------------------------------------------------

# Relevanten Text identifizieren
action_text <- rep(NA, length(list.dims))

for (i in 1: length(list.dims)){
  
  action_text[i] <- paste(eval(parse(text=str_c("amendment_list", list.dims[i]))), collapse = " ")
  if (str_detect(action_text[i], "“") == T)
    action_text[i] <- rm_between(action_text[i], '„', '“', extract=F)
  
  if (str_detect(action_text[i], "‘‘") == T)
    action_text[i] <- rm_between(action_text[i], '„', '‘‘', extract=F)
}


# ---------
# ref.fkt
# ---------

for (i in 1: length(list.dims)){
  
  # header amendment[[1]] ist speziell
  if (i==1)
    if (length(na.omit(str_extract(action_text[i], action_words))) == 2)
      if (length(na.omit(str_extract(action_text[i], action_words[-1]))) == 1)
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.fkt"] <- na.omit(str_extract(action_text[i], action_words[-1]))
      
      if (length(na.omit(str_extract(action_text[i], action_words))) > 0)
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.fkt"] <- na.omit(str_extract(action_text[i], action_words))[1]
      
      if (length(na.omit(str_extract(action_text[i], action_words[-1]))) > 0)
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.fkt"] <- na.omit(str_extract(action_text[i], action_words[-1]))[1]
      
      if (length(na.omit(str_extract(action_text[i], action_words))) == 0 & (length(grep("\\bwerden\\b", action_text[i])) > 0))
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.fkt"] <- "werden"
      
      if (length(na.omit(str_extract(action_text[i], action_words))) == 0 & (length(grep("\\bwird\\b", action_text[i])) > 0))
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.fkt"] <- "wird"
      
}

# ------------
# funktionen
# -----------
for (i in 1: nrow(action_matrix)){
  
  # loeschen
  if (is.element(action_matrix[i,"ref.fkt"], loeschen_words) == T) 
    action_matrix[i,"loeschen"] <- 1
  
  # neunummerieren
  if (is.element(action_matrix[i,"ref.fkt"], neunummerieren_words) == T) 
    action_matrix[i,"neunummerieren"] <- 1
  
  # neufassen
  if (is.element(action_matrix[i,"ref.fkt"], neufassen_words) == T) 
    action_matrix[i,"neufassen"] <- 1
  
  # hinzufuegen
  if (is.element(action_matrix[i,"ref.fkt"], hinzufuegen_words) == T) 
    action_matrix[i,"hinzufuegen"] <- 1
  
  # einfuegen
  if (is.element(action_matrix[i,"ref.fkt"], einfuegen_words) == T) 
    action_matrix[i,"einfuegen"] <- 1
  
}


# ---------
# ref.art
# ---------
for (i in 1:length(list.dims)){
  
  # Vorschläge die mehrere Funktionen ausführen überspringen (manuell kodieren)
  if (length(action_matrix[action_matrix[,"vorschlag.idx"]==i,"vorschlag.idx"]) == 1) {
    
    # ---------------------------
    # Einfache Referenzen
    if (is.na(action_matrix[action_matrix[,"vorschlag.idx"]==i,"neunummerieren"]) == T & is.na(action_matrix[action_matrix[,"vorschlag.idx"]==i,"einfuegen"]) == T) {
      # ---------------------------
      
      # Referenz: "Artikel 1"
      if (str_count(action_text[i], "\\bArtikel\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.art"] <- str_extract(action_text[i], "\\bArtikel\\s[:digit:]+\\b")
      
      
      # Referenz: "Artikel 1 und Artikel 2"
      if (str_count(action_text[i], "\\b(Artikel|Artikeln)\\s[:digit:]\\sund\\sArtikel\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.art"] <- str_extract(action_text[i], "\\bArtikel\\s[:digit:]+\\sund\\sArtikel\\s[:digit:]+\\b")
      
      # Referenz: "Artikel 1 und 2"
      if (str_count(action_text[i], "\\b(Artikel|Artikeln)\\s[:digit:]\\sund\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.art"] <- str_extract(action_text[i], "\\bArtikel\\s[:digit:]+\\sund\\s[:digit:]+\\b")
      
      # Referenz: "Artikel 1, 2 und 3"
      if (str_count(action_text[i], "\\b(Artikel|Artikeln)\\s[:digit:],\\s[:digit:]\\sund\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.art"] <- str_extract(action_text[i], "\\bArtikel\\s[:digit:]+,\\s[:digit:]\\sund\\s[:digit:]+\\b")
      
      
      # Referenz: "Artikel 1 bis Artikel 3"
      if (str_count(action_text[i], "\\b(Artikel|Artikeln)\\s[:digit:]\\sbis\\sArtikel\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.art"] <- str_extract(action_text[i], "\\bArtikel\\s[:digit:]+\\sbis\\sArtikel\\s[:digit:]+\\b")
      
      # Referenz: "Artikel 1 bis 3"
      if (str_count(action_text[i], "\\b(Artikel|Artikeln)\\s[:digit:]\\sbis\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.art"] <- str_extract(action_text[i], "\\bArtikel\\s[:digit:]+\\sbis\\s[:digit:]+\\b")
      
      
      
    } else {
      
      # ------------------------------------------
      # Doppelte Referenzen bei Neunummerierung
      # ------------------------------------------
      
      neunum_text <- action_text[i]
      neunum_text <- str_split_fixed(neunum_text, "(wird|werden)", n=2)
      
      # ref.art
      
      # Referenz: "Artikel 1"
      if (str_count(neunum_text[1], "\\bArtikel\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.art"] <- str_extract(neunum_text[1], "\\bArtikel\\s[:digit:]+\\b")
      
      
      # Referenz: "Artikel 1 und Artikel 2"
      if (str_count(neunum_text[1], "\\bArtikel\\s[:digit:]+\\sund\\sArtikel\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.art"] <- str_extract(neunum_text[1], "\\bArtikel\\s[:digit:]+\\sund\\sArtikel\\s[:digit:]+\\b")
      
      # Referenz: "Artikel 1 und 2"
      if (str_count(neunum_text[1], "\\b(Artikel|Artikeln)\\s[:digit:]+\\sund\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.art"] <- str_extract(neunum_text[1], "\\bArtikel\\s[:digit:]+\\sund\\s[:digit:]+\\b")
      
      # Referenz: "Artikel 1, 2 und 3"
      if (str_count(neunum_text[1], "\\b(Artikel|Artikeln)\\s[:digit:]+,\\s[:digit:]+\\sund\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.art"] <- str_extract(neunum_text[1], "\\bArtikel\\s[:digit:]+,\\s[:digit:]+\\sund\\s[:digit:]+\\b")
      
      
      # Referenz: "Artikel 1 bis Artikel 3"
      if (str_count(neunum_text[1], "\\b(Artikel|Artikeln)\\s[:digit:]+\\sbis\\sArtikel\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.art"] <- str_extract(neunum_text[1], "\\bArtikel\\s[:digit:]+\\sbis\\sArtikel\\s[:digit:]+\\b")
      
      # Referenz: "Artikel 1 bis 3"
      if (str_count(neunum_text[1], "\\b(Artikel|Artikeln)\\s[:digit:]\\sbis\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.art"] <- str_extract(neunum_text[1], "\\bArtikel\\s[:digit:]+\\sbis\\s[:digit:]+\\b")
      
      
      
      # ref.art.neu
      
      # Referenz: "Artikel 1"
      if (str_count(neunum_text[2], "\\bArtikel\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.art.neu"] <- str_extract(neunum_text[2], "\\bArtikel\\s[:digit:]+\\b")
      
      
      # Referenz: "Artikel 1 und Artikel 2"
      if (str_count(neunum_text[2], "\\bArtikel\\s[:digit:]\\sund\\sArtikel\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.art.neu"] <- str_extract(neunum_text[2], "\\bArtikel\\s[:digit:]+\\sund\\sArtikel\\s[:digit:]+\\b")
      
      # Referenz: "Artikel 1 und 2"
      if (str_count(neunum_text[2], "\\b(Artikel|Artikeln)\\s[:digit:]\\sund\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.art.neu"] <- str_extract(neunum_text[2], "\\bArtikel\\s[:digit:]+\\sund\\s[:digit:]+\\b")
      
      # Referenz: "Artikel 1, 2 und 3"
      if (str_count(neunum_text[2], "\\b(Artikel|Artikeln)\\s[:digit:],\\s[:digit:]\\sund\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.art.neu"] <- str_extract(neunum_text[2], "\\bArtikel\\s[:digit:],\\s[:digit:]+\\sund\\s[:digit:]+\\b")
      
      
      # Referenz: "Artikel 1 bis Artikel 3"
      if (str_count(neunum_text[2], "\\bArtikel\\s[:digit:]\\sbis\\sArtikel\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.art.neu"] <- str_extract(neunum_text[2], "\\bArtikel\\s[:digit:]+\\sbis\\sArtikel\\s[:digit:]+\\b")
      
      # Referenz: "Artikel 1 bis 3"
      if (str_count(neunum_text[2], "\\b(Artikel|Artikeln)\\s[:digit:]\\sbis\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.art.neu"] <- str_extract(neunum_text[2], "\\bArtikel\\s[:digit:]+\\sbis\\s[:digit:]+\\b")
      
    }
    
  }    
  
}


# Wenn bei keinem Änderungsvorschlag ein Artikel ausgelesen wurde aber Artikel übergeordent bei Nummer_fehlt erwähnt wird, diesen 
# Artikel auf alle Änderungsvorschläge anwenden
if (length(which(is.na(action_matrix[,"ref.art"]) == F)) == 0)
  if (length(na.omit(str_extract(amendment_list[[1]], "Artikel[:space:][:digit:]+")))>0)
    action_matrix[,"ref.art"] <- na.omit(str_extract(amendment_list[[1]], "Artikel[:space:][:digit:]+"))


# ------------
# ref.num
# ------------

for (i in 1:length(list.dims)){
  
  # Vorschläge die mehrere Funktionen ausführen überspringen (manuell kodieren)
  if (length(action_matrix[action_matrix[,"vorschlag.idx"]==i,"vorschlag.idx"]) == 1) {
    
    # ---------------------------
    # Einfache Referenzen
    if (is.na(action_matrix[action_matrix[,"vorschlag.idx"]==i,"neunummerieren"]) == T & is.na(action_matrix[action_matrix[,"vorschlag.idx"]==i,"einfuegen"]) == T) {
      # ---------------------------
      
      # Referenz: "Nummer 1"
      if (str_count(action_text[i], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.num"] <- str_extract(action_text[i], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+")
      
      
      # Referenz: "Nummer 1 und Nummer 2"
      if (str_count(action_text[i], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]\\sund\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.num"] <- str_extract(action_text[i], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+\\sund\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+")
      
      # Referenz: "Nummer 1 und 2"
      if (str_count(action_text[i], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]\\sund\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.num"] <- str_extract(action_text[i], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+\\sund\\s[:digit:]+")
      
      # Referenz: "Nummer 1, 2 und 3"
      if (str_count(action_text[i], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:],\\s[:digit:]\\sund\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.num"] <- str_extract(action_text[i], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+,\\s[:digit:]\\sund\\s[:digit:]+")
      
      
      # Referenz: "Nummer 1 bis Nummer 3"
      if (str_count(action_text[i], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]\\sbis\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.num"] <- str_extract(action_text[i], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+\\sbis\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+")
      
      # Referenz: "Nummer 1 bis 3"
      if (str_count(action_text[i], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]\\sbis\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.num"] <- str_extract(action_text[i], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+\\sbis\\s[:digit:]+")
      
      
      
    } else {
      
      # ------------------------------------------
      # Doppelte Referenzen bei Neunummerierung
      # ------------------------------------------
      
      neunum_text <- action_text[i]
      neunum_text <- str_split_fixed(neunum_text, "(wird|werden)", n=2)
      
      # ref.art
      
      # Referenz: "Nummer 1"
      if (str_count(neunum_text[1], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.num"] <- str_extract(neunum_text[1], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+")
      
      
      # Referenz: "Nummer 1 und Nummer 2"
      if (str_count(neunum_text[1], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+\\sund\\s(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.num"] <- str_extract(neunum_text[1], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+\\sund\\s(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+")
      
      # Referenz: "Nummer 1 und 2"
      if (str_count(neunum_text[1], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+\\sund\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.num"] <- str_extract(neunum_text[1], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+\\sund\\s[:digit:]+")
      
      # Referenz: "Nummer 1, 2 und 3"
      if (str_count(neunum_text[1], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+,\\s[:digit:]+\\sund\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.num"] <- str_extract(neunum_text[1], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+,\\s[:digit:]+\\sund\\s[:digit:]+")
      
      
      # Referenz: "Nummer 1 bis Nummer 3"
      if (str_count(neunum_text[1], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+\\sbis\\s(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.num"] <- str_extract(neunum_text[1], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+\\sbis\\s(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+")
      
      # Referenz: "Nummer 1 bis 3"
      if (str_count(neunum_text[1], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]\\sbis\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.num"] <- str_extract(neunum_text[1], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+\\sbis\\s[:digit:]+")
      
      
      
      # ref.art.neu
      
      # Referenz: "Artikel 1"
      if (str_count(neunum_text[2], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.num.neu"] <- str_extract(neunum_text[2], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+")
      
      
      # Referenz: "Nummer 1 und Nummer 2"
      if (str_count(neunum_text[2], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]\\sund\\s(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.num.neu"] <- str_extract(neunum_text[2], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+\\sund\\s(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+")
      
      # Referenz: "Nummer 1 und 2"
      if (str_count(neunum_text[2], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]\\sund\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.num.neu"] <- str_extract(neunum_text[2], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+\\sund\\s[:digit:]+")
      
      # Referenz: "Nummer 1, 2 und 3"
      if (str_count(neunum_text[2], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:],\\s[:digit:]\\sund\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.num.neu"] <- str_extract(neunum_text[2], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:],\\s[:digit:]+\\sund\\s[:digit:]+")
      
      
      # Referenz: "Nummer 1 bis Nummer 3"
      if (str_count(neunum_text[2], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]\\sbis\\s(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.num.neu"] <- str_extract(neunum_text[2], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+\\sbis\\s(Nummer|Nummern)\\s[:digit:]+")
      
      # Referenz: "Nummer 1 bis 3"
      if (str_count(neunum_text[2], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]\\sbis\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.num.neu"] <- str_extract(neunum_text[2], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+\\sbis\\s[:digit:]+")
      
    }
    
  }    
  
}




# ---------------
# ref.buch
# ---------------

for (i in 1:length(list.dims)){
  
  # Vorschläge die mehrere Funktionen ausführen überspringen (manuell kodieren)
  if (length(action_matrix[action_matrix[,"vorschlag.idx"]==i,"vorschlag.idx"]) == 1) {
    
    # ---------------------------
    # Einfache Referenzen
    if (is.na(action_matrix[action_matrix[,"vorschlag.idx"]==i,"neunummerieren"]) == T & is.na(action_matrix[action_matrix[,"vorschlag.idx"]==i,"einfuegen"]) == T) {
      # ---------------------------
      
      # Referenz: "Nummer 1"
      if (str_count(action_text[i], "\\b(Buchstabe|Buchstaben|Buchst.)") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buch"] <- str_extract(action_text[i], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]")
      
      
      # Referenz: "Nummer 1 und Nummer 2"
      if (str_count(action_text[i], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sund\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buch"] <- str_extract(action_text[i], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sund\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]")
      
      # Referenz: "Nummer 1 und 2"
      if (str_count(action_text[i], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sund\\s[:lower:]") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buch"] <- str_extract(action_text[i], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sund\\s[:lower:]")
      
      # Referenz: "Nummer 1, 2 und 3"
      if (str_count(action_text[i], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:],\\s[:lower:]\\sund\\s[:lower:]") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buch"] <- str_extract(action_text[i], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:],\\s[:lower:]\\sund\\s[:lower:]")
      
      
      # Referenz: "Nummer 1 bis Nummer 3"
      if (str_count(action_text[i], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sbis\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buch"] <- str_extract(action_text[i], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sbis\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]")
      
      # Referenz: "Nummer 1 bis 3"
      if (str_count(action_text[i], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sbis\\s[:lower:]") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buch"] <- str_extract(action_text[i], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sbis\\s[:lower:]")
      
      
      
    } else {
      
      # ------------------------------------------
      # Doppelte Referenzen bei Neunummerierung
      # ------------------------------------------
      
      neunum_text <- action_text[i]
      neunum_text <- str_split_fixed(neunum_text, "(wird|werden)", n=2)
      
      # ref.art
      
      # Referenz: "Nummer 1"
      if (str_count(neunum_text[1], "\\b(Buchstabe|Buchstaben|Buchst.)") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buch"] <- str_extract(neunum_text[1], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]")
      
      
      # Referenz: "Nummer 1 und Nummer 2"
      if (str_count(neunum_text[1], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sund\\s(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buch"] <- str_extract(neunum_text[1], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sund\\s(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]")
      
      # Referenz: "Nummer 1 und 2"
      if (str_count(neunum_text[1], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sund\\s[:lower:]") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buch"] <- str_extract(neunum_text[1], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sund\\s[:lower:]")
      
      # Referenz: "Nummer 1, 2 und 3"
      if (str_count(neunum_text[1], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:],\\s[:digit:]\\sund\\s[:lower:]") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buch"] <- str_extract(neunum_text[1], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:],\\s[:lower:]\\sund\\s[:lower:]")
      
      
      # Referenz: "Nummer 1 bis Nummer 3"
      if (str_count(neunum_text[1], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sbis\\s(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buch"] <- str_extract(neunum_text[1], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sbis\\s(Buchstabe|Buchstaben)\\s[:lower:]")
      
      # Referenz: "Nummer 1 bis 3"
      if (str_count(neunum_text[1], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sbis\\s[:lower:]") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buch"] <- str_extract(neunum_text[1], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sbis\\s[:lower:]")
      
      
      
      # ref.art.neu
      
      # Referenz: "Artikel 1"
      if (str_count(neunum_text[2], "\\b(Buchstabe|Buchstaben|Buchst.)") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buch.neu"] <- str_extract(neunum_text[2], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]")
      
      
      # Referenz: "Nummer 1 und Nummer 2"
      if (str_count(neunum_text[2], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sund\\s(Buchstabe|Buchstaben)\\s[:lower:]") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buch.neu"] <- str_extract(neunum_text[2], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sund\\s(Buchstabe|Buchstaben|Buchst.)\\s[:lower:")
      
      # Referenz: "Nummer 1 und 2"
      if (str_count(neunum_text[2], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sund\\s[:lower:]\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buch.neu"] <- str_extract(neunum_text[2], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sund\\s[:lower:]")
      
      # Referenz: "Nummer 1, 2 und 3"
      if (str_count(neunum_text[2], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:],\\s[:lower:]\\sund\\s[:lower:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buch.neu"] <- str_extract(neunum_text[2], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:],\\s[:lower:]+\\sund\\s[:lower:]")
      
      
      # Referenz: "Nummer 1 bis Nummer 3"
      if (str_count(neunum_text[2], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sbis\\s(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buch.neu"] <- str_extract(neunum_text[2], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sbis\\s(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]")
      
      # Referenz: "Nummer 1 bis 3"
      if (str_count(neunum_text[2], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sbis\\s[:lower:]") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buch.neu"] <- str_extract(neunum_text[2], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sbis\\s[:lower:]")
      
    }
    
  }    
  
}



# ---------------
# ref.buchbuch
# ---------------

for (i in 1:length(list.dims)){
  
  # Vorschläge die mehrere Funktionen ausführen überspringen (manuell kodieren)
  if (length(action_matrix[action_matrix[,"vorschlag.idx"]==i,"vorschlag.idx"]) == 1){
    
    # ---------------------------
    # Einfache Referenzen
    if (is.na(action_matrix[action_matrix[,"vorschlag.idx"]==i,"neunummerieren"]) == T & is.na(action_matrix[action_matrix[,"vorschlag.idx"]==i,"einfuegen"]) == T){
      # ---------------------------
      
      # Referenz: "Nummer 1"
      if (str_count(action_text[i], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buchbuch"] <- str_extract(action_text[i], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]")
      
      
      # Referenz: "Nummer 1 und Nummer 2"
      if (str_count(action_text[i], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sund\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buchbuch"] <- str_extract(action_text[i], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sund\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]")
      
      # Referenz: "Nummer 1 und 2"
      if (str_count(action_text[i], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sund\\s[:lower:][:lower:]") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buchbuch"] <- str_extract(action_text[i], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sund\\s[:lower:][:lower:]")
      
      # Referenz: "Nummer 1, 2 und 3"
      if (str_count(action_text[i], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:],\\s[:lower:][:lower:]\\sund\\s[:lower:][:lower:]") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buchbuch"] <- str_extract(action_text[i], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:],\\s[:lower:][:lower:]\\sund\\s[:lower:][:lower:]")
      
      
      # Referenz: "Nummer 1 bis Nummer 3"
      if (str_count(action_text[i], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sbis\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buchbuch"] <- str_extract(action_text[i], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sbis\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]")
      
      # Referenz: "Nummer 1 bis 3"
      if (str_count(action_text[i], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sbis\\s[:lower:][:lower:]") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buchbuch"] <- str_extract(action_text[i], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sbis\\s[:lower:][:lower:]")
      
      
      
    } else {
      
      # ------------------------------------------
      # Doppelte Referenzen bei Neunummerierung
      # ------------------------------------------
      
      neunum_text <- action_text[i]
      neunum_text <- str_split_fixed(neunum_text, "(wird|werden)", n=2)
      
      # ref.art
      
      # Referenz: "Nummer 1"
      if (str_count(neunum_text[1], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buchbuch"] <- str_extract(neunum_text[1], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]")
      
      
      # Referenz: "Nummer 1 und Nummer 2"
      if (str_count(neunum_text[1], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sund\\s(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buchbuch"] <- str_extract(neunum_text[1], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sund\\s(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]")
      
      # Referenz: "Nummer 1 und 2"
      if (str_count(neunum_text[1], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sund\\s[:lower:][:lower:]") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buchbuch"] <- str_extract(neunum_text[1], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sund\\s[:lower:][:lower:]")
      
      # Referenz: "Nummer 1, 2 und 3"
      if (str_count(neunum_text[1], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:],\\s[:digit:]\\sund\\s[:lower:][:lower:]") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buchbuch"] <- str_extract(neunum_text[1], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:],\\s[:lower:][:lower:]\\sund\\s[:lower:][:lower:]")
      
      
      # Referenz: "Nummer 1 bis Nummer 3"
      if (str_count(neunum_text[1], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sbis\\s(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buchbuch"] <- str_extract(neunum_text[1], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sbis\\s(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]")
      
      # Referenz: "Nummer 1 bis 3"
      if (str_count(neunum_text[1], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sbis\\s[:lower:][:lower:]") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buchbuch"] <- str_extract(neunum_text[1], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sbis\\s[:lower:][:lower:]")
      
      
      
      # ref.art.neu
      
      # Referenz: "Artikel 1"
      if (str_count(neunum_text[2], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buchbuch.neu"] <- str_extract(neunum_text[2], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]")
      
      
      # Referenz: "Nummer 1 und Nummer 2"
      if (str_count(neunum_text[2], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sund\\s(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buchbuch.neu"] <- str_extract(neunum_text[2], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sund\\s(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]")
      
      # Referenz: "Nummer 1 und 2"
      if (str_count(neunum_text[2], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sund\\s[:lower:][:lower:]") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buchbuch.neu"] <- str_extract(neunum_text[2], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sund\\s[:lower:][:lower:]")
      
      # Referenz: "Nummer 1, 2 und 3"
      if (str_count(neunum_text[2], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:],\\s[:lower:][:lower:]\\sund\\s[:lower:][:lower:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buchbuch.neu"] <- str_extract(neunum_text[2], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:],\\s[:lower:][:lower:]+\\sund\\s[:lower:][:lower:]")
      
      
      # Referenz: "Nummer 1 bis Nummer 3"
      if (str_count(neunum_text[2], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sbis\\s(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buchbuch.neu"] <- str_extract(neunum_text[2], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sbis\\s(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]")
      
      # Referenz: "Nummer 1 bis 3"
      if (str_count(neunum_text[2], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sbis\\s[:lower:][:lower:]") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.buchbuch.neu"] <- str_extract(neunum_text[2], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sbis\\s[:lower:][:lower:]")
      
    }
    
  }    
  
}




# ------------
# ref.para
# ------------

for (i in 1:length(list.dims)){
  
  # Vorschläge die mehrere Funktionen ausführen überspringen (manuell kodieren)
  if (length(action_matrix[action_matrix[,"vorschlag.idx"]==i,"vorschlag.idx"]) == 1) {
    
    # ---------------------------
    # Einfache Referenzen
    if (is.na(action_matrix[action_matrix[,"vorschlag.idx"]==i,"neunummerieren"]) == T & is.na(action_matrix[action_matrix[,"vorschlag.idx"]==i,"einfuegen"]) == T) {
      # ---------------------------
      
      # Referenz: "§ 1"
      if (str_count(action_text[i], "(^|\\s)[§]\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.para"] <- str_extract(action_text[i], "(^|\\s)[§]\\s[:digit:]+\\b")
      
      # Referenz: "§ 1 und § 2"
      if (str_count(action_text[i], "(^|\\s)[§]\\s[:digit:]+\\sund\\s[§]\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.para"] <- str_extract(action_text[i], "(^|\\s)[§]\\s[:digit:]+\\sund\\s[§]\\s[:digit:]+\\b")
      
      # Referenz: "§§ 1 und 2"
      if (str_count(action_text[i], "(^|\\s)[§][§]\\s[:digit:]+\\sund\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.para"] <- str_extract(action_text[i], "(^|\\s)[§][§]\\s[:digit:]+\\sund\\s[:digit:]+\\b")
      
      # Referenz: "§§ 1, 2 und 3"
      if (str_count(action_text[i], "(^|\\s)[§][§]\\s[:digit:]+,\\s[:digit:]+\\sund\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.para"] <- str_extract(action_text[i], "(^|\\s)[§][§]\\s[:digit:]+,\\s[:digit:]+\\sund\\s[:digit:]+\\b")
      
      
      # Referenz: "§ 1 bis § 3"
      if (str_count(action_text[i], "(^|\\s)[§]\\s[:digit:]+\\sbis\\s[§]\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.para"] <- str_extract(action_text[i], "(^|\\s)[§]\\s[:digit:]+\\sbis\\s[§]\\s[:digit:]+\\b")
      
      # Referenz: "§§ 1 bis 3"
      if (str_count(action_text[i], "(^|\\s)[§][§]\\s[:digit:]+\\sbis\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.para"] <- str_extract(action_text[i], "(^|\\s)[§][§]\\s[:digit:]+\\sbis\\s[:digit:]+\\b")
      
      
    } else {
      
      # ------------------------------------------
      # Doppelte Referenzen bei Neunummerierung
      # ------------------------------------------
      
      neunum_text <- action_text[i]
      neunum_text <- str_split_fixed(neunum_text, "(wird|werden)", n=2)
      
      # ref.para
      
      # Referenz: "§ 1"
      if (str_count(neunum_text[1], "(^|\\s)[§]\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.para"] <- str_extract(neunum_text[1], "(^|\\s)[§]\\s[:digit:]+\\b")
      
      
      # Referenz: "§ 1 und § 2"
      if (str_count(neunum_text[1], "(^|\\s)[§]\\s[:digit:]+\\sund\\s[§]\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.para"] <- str_extract(neunum_text[1], "(^|\\s)[§]\\s[:digit:]+\\sund\\s[§]\\s[:digit:]+\\b")
      
      # Referenz: "§§ 1 und 2"
      if (str_count(neunum_text[1], "(^|\\s)[§][§]\\s[:digit:]+\\sund\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.para"] <- str_extract(neunum_text[1], "(^|\\s)[§][§]\\s[:digit:]+\\sund\\s[:digit:]+\\b")
      
      # Referenz: "§§ 1, 2 und 3"
      if (str_count(neunum_text[1], "(^|\\s)[§][§]\\s[:digit:]+,\\s[:digit:]+\\sund\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.para"] <- str_extract(neunum_text[1], "(^|\\s)[§][§]\\s[:digit:]+,\\s[:digit:]+\\sund\\s[:digit:]+\\b")
      
      
      # Referenz: "§ 1 bis § 3
      if (str_count(neunum_text[1], "(^|\\s)[§]\\s[:digit:]+\\sbis\\s[§]\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.para"] <- str_extract(neunum_text[1], "(^|\\s)[§]\\s[:digit:]+\\sbis\\s[§]\\s[:digit:]+\\b")
      
      # Referenz: "§§ 1 bis 3"
      if (str_count(neunum_text[1], "(^|\\s)[§][§]\\s[:digit:]+\\sbis\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.para"] <- str_extract(neunum_text[1], "(^|\\s)[§][§]\\s[:digit:]+\\sbis\\s[:digit:]+\\b")
      
      
      
      # ref.para.neu
      
      # Referenz: "§ 1"
      if (str_count(neunum_text[2], "(^|\\s)[§]\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.para.neu"] <- str_extract(neunum_text[2], "(^|\\s)[§]\\s[:digit:]+\\b")
      
      
      # Referenz: "§ 1 und § 2"
      if (str_count(neunum_text[2], "(^|\\s)[§]\\s[:digit:]+\\sund\\s[§]\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.para.neu"] <- str_extract(neunum_text[2], "(^|\\s)[§]\\s[:digit:]+\\sund\\s[§]\\s[:digit:]+\\b")
      
      # Referenz: "§§ 1 und 2"
      if (str_count(neunum_text[2], "(^|\\s)[§][§]\\s[:digit:]+\\sund\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.para.neu"] <- str_extract(neunum_text[2], "(^|\\s)[§][§]\\s[:digit:]+\\sund\\s[:digit:]+\\b")
      
      # Referenz: "§§ 1, 2 und 3"
      if (str_count(neunum_text[2], "(^|\\s)[§][§]\\s[:digit:]+,\\s[:digit:]+\\sund\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.para.neu"] <- str_extract(neunum_text[2], "(^|\\s)[§][§]\\s[:digit:]+,\\s[:digit:]+\\sund\\s[:digit:]+\\b")
      
      
      # Referenz: "§ 1 bis § 3
      if (str_count(neunum_text[2], "(^|\\s)[§]\\s[:digit:]+\\sbis\\s[§]\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.para.neu"] <- str_extract(neunum_text[2], "(^|\\s)[§]\\s[:digit:]+\\sbis\\s[§]\\s[:digit:]+\\b")
      
      # Referenz: "§§ 1 bis 3"
      if (str_count(neunum_text[2], "(^|\\s)[§][§]\\s[:digit:]+\\sbis\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.para.neu"] <- str_extract(neunum_text[2], "(^|\\s)[§][§]\\s[:digit:]+\\sbis\\s[:digit:]+\\b")
      
    }
    
  }    
  
}





# ------------
# ref.absatz
# ------------

for (i in 1:length(list.dims)){
  
  # Vorschläge die mehrere Funktionen ausführen überspringen (manuell kodieren)
  if (length(action_matrix[action_matrix[,"vorschlag.idx"]==i,"vorschlag.idx"]) == 1) {
    
    # ---------------------------
    # Einfache Referenzen
    if (is.na(action_matrix[action_matrix[,"vorschlag.idx"]==i,"neunummerieren"]) == T & is.na(action_matrix[action_matrix[,"vorschlag.idx"]==i,"einfuegen"]) == T) {
      # ---------------------------
      
      # Referenz: "Absatz 1"
      if (str_count(action_text[i], "\\b(Absatz|Abs[:punct:])") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.absatz"] <- str_extract(action_text[i], "\\b(Absatz|Abs[:punct:])\\s[:digit:]+")
      
      # Referenz: "Absatz 1 und Absatz 2"
      if (str_count(action_text[i], "\\b(Absatz|Abs.)\\s[:digit:]\\sund\\b(Absatz|Abs.)\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.absatz"] <- str_extract(action_text[i], "\\b(Absatz|Abs.)\\s[:digit:]+\\sund\\b(Absatz|Abs.)\\s[:digit:]+")
      
      
      # Referenz: "Absatz 1 und 2"
      if (str_count(action_text[i], "\\b(Absatz|Abs.)\\s[:digit:]\\sund\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.absatz"] <- str_extract(action_text[i], "\\b(Absatz|Abs.)\\s[:digit:]+\\sund\\s[:digit:]+")
      
      # Referenz: "Absätze 1 und 2"
      if (str_count(action_text[i], "\\b(Absätze|Abs.)\\s[:digit:]\\sund\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.absatz"] <- str_extract(action_text[i], "\\b(Absätze|Abs.)\\s[:digit:]+\\sund\\s[:digit:]+")
      
      # Referenz: "Absatz 1, 2 und 3"
      if (str_count(action_text[i], "\\b(Absatz|Abs.)\\s[:digit:],\\s[:digit:]\\sund\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.absatz"] <- str_extract(action_text[i], "\\b(Absatz|Abs.)\\s[:digit:]+,\\s[:digit:]\\sund\\s[:digit:]+")
      
      # Referenz: "Absätze 1, 2 und 3"
      if (str_count(action_text[i], "\\b(Absätze|Abs.)\\s[:digit:],\\s[:digit:]\\sund\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.absatz"] <- str_extract(action_text[i], "\\b(Absätze|Abs.)\\s[:digit:]+,\\s[:digit:]\\sund\\s[:digit:]+")
      
      
      # Referenz: "Absatz 1 bis Absatz 3"
      if (str_count(action_text[i], "\\b(Absatz|Abs.)\\s[:digit:]\\sbis\\b(Absatz|Abs.)\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.absatz"] <- str_extract(action_text[i], "\\b(Absatz|Abs.)\\s[:digit:]+\\sbis\\b(Absatz|Abs.)\\s[:digit:]+")
      
      # Referenz: "Absatz 1 bis 3"
      if (str_count(action_text[i], "\\b(Absatz|Abs.)\\s[:digit:]\\sbis\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.absatz"] <- str_extract(action_text[i], "\\b(Absatz|Abs.)\\s[:digit:]+\\sbis\\s[:digit:]+")
      
      # Referenz: "Absätze 1 bis 3"
      if (str_count(action_text[i], "\\b(Absätze|Abs.)\\s[:digit:]\\sbis\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.absatz"] <- str_extract(action_text[i], "\\b(Absätze|Abs.)\\s[:digit:]+\\sbis\\s[:digit:]+")
      
      
      
    } else {
      
      # ------------------------------------------
      # Doppelte Referenzen bei Neunummerierung
      # ------------------------------------------
      
      neunum_text <- action_text[i]
      neunum_text <- str_split_fixed(neunum_text, "(wird|werden)", n=2)
      
      # ref.absatz
      
      # Referenz: "Absatz 1"
      if (str_count(neunum_text[1], "\\b(Absatz|Abs[:punct:])") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.absatz"] <- str_extract(neunum_text[1], "\\b(Absatz|Abs[:punct:])\\s[:digit:]+")
      
      # Referenz: "Absatz 1 und Absatz 2"
      if (str_count(neunum_text[1], "\\b(Absatz|Abs.)\\s[:digit:]\\sund\\b(Absatz|Abs.)\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.absatz"] <- str_extract(neunum_text[1], "\\b(Absatz|Abs.)\\s[:digit:]+\\sund\\b(Absatz|Abs.)\\s[:digit:]+")
      
      
      # Referenz: "Absatz 1 und 2"
      if (str_count(neunum_text[1], "\\b(Absatz|Abs.)\\s[:digit:]\\sund\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.absatz"] <- str_extract(neunum_text[1], "\\b(Absatz|Abs.)\\s[:digit:]+\\sund\\s[:digit:]+")
      
      # Referenz: "Absätze 1 und 2"
      if (str_count(neunum_text[1], "\\b(Absätze|Abs.)\\s[:digit:]\\sund\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.absatz"] <- str_extract(neunum_text[1], "\\b(Absätze|Abs.)\\s[:digit:]+\\sund\\s[:digit:]+")
      
      # Referenz: "Absatz 1, 2 und 3"
      if (str_count(neunum_text[1], "\\b(Absatz|Abs.)\\s[:digit:],\\s[:digit:]\\sund\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.absatz"] <- str_extract(neunum_text[1], "\\b(Absatz|Abs.)\\s[:digit:]+,\\s[:digit:]\\sund\\s[:digit:]+")
      
      # Referenz: "Absätze 1, 2 und 3"
      if (str_count(neunum_text[1], "\\b(Absätze|Abs.)\\s[:digit:],\\s[:digit:]\\sund\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.absatz"] <- str_extract(neunum_text[1], "\\b(Absätze|Abs.)\\s[:digit:]+,\\s[:digit:]\\sund\\s[:digit:]+")
      
      
      # Referenz: "Absatz 1 bis Absatz 3"
      if (str_count(neunum_text[1], "\\b(Absatz|Abs.)\\s[:digit:]\\sbis\\b(Absatz|Abs.)\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.absatz"] <- str_extract(neunum_text[1], "\\b(Absatz|Abs.)\\s[:digit:]+\\sbis\\b(Absatz|Abs.)\\s[:digit:]+")
      
      # Referenz: "Absatz 1 bis 3"
      if (str_count(neunum_text[1], "\\b(Absatz|Abs.)\\s[:digit:]\\sbis\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.absatz"] <- str_extract(neunum_text[1], "\\b(Absatz|Abs.)\\s[:digit:]+\\sbis\\s[:digit:]+")
      
      # Referenz: "Absätze 1 bis 3"
      if (str_count(neunum_text[1], "\\b(Absätze|Abs.)\\s[:digit:]\\sbis\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.absatz"] <- str_extract(neunum_text[1], "\\b(Absätze|Abs.)\\s[:digit:]+\\sbis\\s[:digit:]+")
      
      
      
      # ref.absatz.neu
      
      # Referenz: "Absatz 1"
      if (str_count(neunum_text[2], "\\b(Absatz|Abs[:punct:])") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.absatz.neu"] <- str_extract(neunum_text[2], "\\b(Absatz|Abs[:punct:])\\s[:digit:]+")
      
      # Referenz: "Absatz 1 und Absatz 2"
      if (str_count(neunum_text[2], "\\b(Absatz|Abs.)\\s[:digit:]\\sund\\b(Absatz|Abs.)\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.absatz.neu"] <- str_extract(neunum_text[2], "\\b(Absatz|Abs.)\\s[:digit:]+\\sund\\b(Absatz|Abs.)\\s[:digit:]+")
      
      
      # Referenz: "Absatz 1 und 2"
      if (str_count(neunum_text[2], "\\b(Absatz|Abs.)\\s[:digit:]\\sund\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.absatz.neu"] <- str_extract(neunum_text[2], "\\b(Absatz|Abs.)\\s[:digit:]+\\sund\\s[:digit:]+")
      
      # Referenz: "Absätze 1 und 2"
      if (str_count(neunum_text[2], "\\b(Absätze|Abs.)\\s[:digit:]\\sund\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.absatz.neu"] <- str_extract(neunum_text[2], "\\b(Absätze|Abs.)\\s[:digit:]+\\sund\\s[:digit:]+")
      
      # Referenz: "Absatz 1, 2 und 3"
      if (str_count(neunum_text[2], "\\b(Absatz|Abs.)\\s[:digit:],\\s[:digit:]\\sund\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.absatz.neu"] <- str_extract(neunum_text[2], "\\b(Absatz|Abs.)\\s[:digit:]+,\\s[:digit:]\\sund\\s[:digit:]+")
      
      # Referenz: "Absätze 1, 2 und 3"
      if (str_count(neunum_text[2], "\\b(Absätze|Abs.)\\s[:digit:],\\s[:digit:]\\sund\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.absatz.neu"] <- str_extract(neunum_text[2], "\\b(Absätze|Abs.)\\s[:digit:]+,\\s[:digit:]\\sund\\s[:digit:]+")
      
      
      # Referenz: "Absatz 1 bis Absatz 3"
      if (str_count(neunum_text[2], "\\b(Absatz|Abs.)\\s[:digit:]\\sbis\\b(Absatz|Abs.)\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.absatz.neu"] <- str_extract(neunum_text[2], "\\b(Absatz|Abs.)\\s[:digit:]+\\sbis\\b(Absatz|Abs.)\\s[:digit:]+")
      
      # Referenz: "Absatz 1 bis 3"
      if (str_count(neunum_text[2], "\\b(Absatz|Abs.)\\s[:digit:]\\sbis\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.absatz.neu"] <- str_extract(neunum_text[2], "\\b(Absatz|Abs.)\\s[:digit:]+\\sbis\\s[:digit:]+")
      
      # Referenz: "Absätze 1 bis 3"
      if (str_count(neunum_text[2], "\\b(Absätze|Abs.)\\s[:digit:]\\sbis\\s[:digit:]+") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.absatz.neu"] <- str_extract(neunum_text[2], "\\b(Absätze|Abs.)\\s[:digit:]+\\sbis\\s[:digit:]+")
      
      
    }
    
  }    
  
}






# ------------
# ref.satz
# ------------

for (i in 1:length(list.dims)){
  
  # Vorschläge die mehrere Funktionen ausführen überspringen (manuell kodieren)
  if (length(action_matrix[action_matrix[,"vorschlag.idx"]==i,"vorschlag.idx"]) == 1) {
    
    # ---------------------------
    # Einfache Referenzen
    if (is.na(action_matrix[action_matrix[,"vorschlag.idx"]==i,"neunummerieren"]) == T & is.na(action_matrix[action_matrix[,"vorschlag.idx"]==i,"einfuegen"]) == T) {
      # ---------------------------
      
      # Referenz: "Satz 1"
      if (str_count(action_text[i], "\\b(Satz|Halbsatz)\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.satz"] <- str_extract(action_text[i], "\\b(Satz|Halbsatz)\\s[:digit:]+\\b")
      
      # Referenz: "Satz 1 und Satz 2"
      if (str_count(action_text[i], "\\b(Satz|Halbsatz)\\s[:digit:]\\sund\\b(Satz|Halbsatz)\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.satz"] <- str_extract(action_text[i], "\\b(Satz|Halbsatz)\\s[:digit:]+\\sund\\b(Satz|Halbsatz)\\s[:digit:]+\\b")
      
      
      # Referenz: "Satz 1 und 2"
      if (str_count(action_text[i], "\\b(Satz|Halbsatz)\\s[:digit:]\\sund\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.satz"] <- str_extract(action_text[i], "\\b(Satz|Halbsatz)\\s[:digit:]+\\sund\\s[:digit:]+\\b")
      
      # Referenz: "Sätze(n) 1 und 2"
      if (str_count(action_text[i], "\\b(Sätze|Sätzen)\\s[:digit:]\\sund\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.satz"] <- str_extract(action_text[i], "\\bSätze\\s[:digit:]+\\sund\\s[:digit:]+\\b")
      
      # Referenz: "Satz 1, 2 und 3"
      if (str_count(action_text[i], "\\b(Satz|Halbsatz)\\s[:digit:],\\s[:digit:]\\sund\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.satz"] <- str_extract(action_text[i], "\\b(Satz|Halbsatz)\\s[:digit:]+,\\s[:digit:]\\sund\\s[:digit:]+\\b")
      
      # Referenz: "Sätze(n) 1, 2 und 3"
      if (str_count(action_text[i], "\\b(Sätze|Sätzen)\\s[:digit:],\\s[:digit:]\\sund\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.satz"] <- str_extract(action_text[i], "\\bSätze\\s[:digit:]+,\\s[:digit:]\\sund\\s[:digit:]+\\b")
      
      
      # Referenz: "Satz 1 bis Satz 3"
      if (str_count(action_text[i], "\\b(Satz|Halbsatz)\\s[:digit:]\\sbis\\b(Satz|Halbsatz)\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.satz"] <- str_extract(action_text[i], "\\b(Satz|Halbsatz)\\s[:digit:]+\\sbis\\b(Satz|Halbsatz)\\s[:digit:]+\\b")
      
      # Referenz: "Satz 1 bis 3"
      if (str_count(action_text[i], "\\b(Satz|Halbsatz)\\s[:digit:]\\sbis\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.satz"] <- str_extract(action_text[i], "\\b(Satz|Halbsatz)\\s[:digit:]+\\sbis\\s[:digit:]+\\b")
      
      # Referenz: "Sätze(n) 1 bis 3"
      if (str_count(action_text[i], "\\b(Sätze|Sätzen)\\s[:digit:]\\sbis\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.satz"] <- str_extract(action_text[i], "\\bSätze\\s[:digit:]+\\sbis\\s[:digit:]+\\b")
      
      
      
    } else {
      
      # ------------------------------------------
      # Doppelte Referenzen bei Neunummerierung
      # ------------------------------------------
      
      neunum_text <- action_text[i]
      neunum_text <- str_split_fixed(neunum_text, "(wird|werden)", n=2) 
      
      
      # ref.satz
      
      # Referenz: "Satz 1"
      if (str_count(neunum_text[1], "\\b(Satz|Halbsatz)\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.satz"] <- str_extract(neunum_text[1], "\\b(Satz|Halbsatz)\\s[:digit:]+\\b")
      
      # Referenz: "Satz 1 und Satz 2"
      if (str_count(neunum_text[1], "\\b(Satz|Halbsatz)\\s[:digit:]\\sund\\b(Satz|Halbsatz)\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.satz"] <- str_extract(neunum_text[1], "\\b(Satz|Halbsatz)\\s[:digit:]+\\sund\\b(Satz|Halbsatz)\\s[:digit:]+\\b")
      
      
      # Referenz: "Satz 1 und 2"
      if (str_count(neunum_text[1], "\\b(Satz|Halbsatz)\\s[:digit:]\\sund\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.satz"] <- str_extract(neunum_text[1], "\\b(Satz|Halbsatz)\\s[:digit:]+\\sund\\s[:digit:]+\\b")
      
      # Referenz: "Sätze 1 und 2"
      if (str_count(neunum_text[1], "\\b(Sätze|Sätzen)\\s[:digit:]\\sund\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.satz"] <- str_extract(neunum_text[1], "\\bSätze\\s[:digit:]+\\sund\\s[:digit:]+\\b")
      
      # Referenz: "Satz 1, 2 und 3"
      if (str_count(neunum_text[1], "\\b(Satz|Halbsatz)\\s[:digit:],\\s[:digit:]\\sund\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.satz"] <- str_extract(neunum_text[1], "\\b(Satz|Halbsatz)\\s[:digit:]+,\\s[:digit:]\\sund\\s[:digit:]+\\b")
      
      # Referenz: "Sätze 1, 2 und 3"
      if (str_count(neunum_text[1], "\\b(Sätze|Sätzen)\\s[:digit:],\\s[:digit:]\\sund\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.satz"] <- str_extract(neunum_text[1], "\\bSätze\\s[:digit:]+,\\s[:digit:]\\sund\\s[:digit:]+\\b")
      
      
      # Referenz: "Satz 1 bis Satz 3"
      if (str_count(neunum_text[1], "\\b(Satz|Halbsatz)\\s[:digit:]\\sbis\\b(Satz|Halbsatz)\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.satz"] <- str_extract(neunum_text[1], "\\b(Satz|Halbsatz)\\s[:digit:]+\\sbis\\b(Satz|Halbsatz)\\s[:digit:]+\\b")
      
      # Referenz: "Satz 1 bis 3"
      if (str_count(neunum_text[1], "\\b(Satz|Halbsatz)\\s[:digit:]\\sbis\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.satz"] <- str_extract(neunum_text[1], "\\b(Satz|Halbsatz)\\s[:digit:]+\\sbis\\s[:digit:]+\\b")
      
      # Referenz: "Sätze 1 bis 3"
      if (str_count(neunum_text[1], "\\b(Sätze|Sätzen)\\s[:digit:]\\sbis\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.satz"] <- str_extract(neunum_text[1], "\\bSätze\\s[:digit:]+\\sbis\\s[:digit:]+\\b")
      
      
      # ref.satz.neu
      
      # Referenz: "Satz 1"
      if (str_count(neunum_text[2], "\\b(Satz|Halbsatz)\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.satz.neu"] <- str_extract(neunum_text[2], "\\b(Satz|Halbsatz)\\s[:digit:]+\\b")
      
      # Referenz: "Satz 1 und Satz 2"
      if (str_count(neunum_text[2], "\\b(Satz|Halbsatz)\\s[:digit:]\\sund\\b(Satz|Halbsatz)\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.satz.neu"] <- str_extract(neunum_text[2], "\\b(Satz|Halbsatz)\\s[:digit:]+\\sund\\b(Satz|Halbsatz)\\s[:digit:]+\\b")
      
      
      # Referenz: "Satz 1 und 2"
      if (str_count(neunum_text[2], "\\b(Satz|Halbsatz)\\s[:digit:]\\sund\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.satz.neu"] <- str_extract(neunum_text[2], "\\b(Satz|Halbsatz)\\s[:digit:]+\\sund\\s[:digit:]+\\b")
      
      # Referenz: "Sätze 1 und 2"
      if (str_count(neunum_text[2], "\\b(Sätze|Sätzen)\\s[:digit:]\\sund\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.satz.neu"] <- str_extract(neunum_text[2], "\\bSätze\\s[:digit:]+\\sund\\s[:digit:]+\\b")
      
      # Referenz: "Satz 1, 2 und 3"
      if (str_count(neunum_text[2], "\\b(Satz|Halbsatz)\\s[:digit:],\\s[:digit:]\\sund\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.satz.neu"] <- str_extract(neunum_text[2], "\\b(Satz|Halbsatz)\\s[:digit:]+,\\s[:digit:]\\sund\\s[:digit:]+\\b")
      
      # Referenz: "Sätze 1, 2 und 3"
      if (str_count(neunum_text[2], "\\b(Sätze|Sätzen)\\s[:digit:],\\s[:digit:]\\sund\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.satz.neu"] <- str_extract(neunum_text[2], "\\bSätze\\s[:digit:]+,\\s[:digit:]\\sund\\s[:digit:]+\\b")
      
      
      # Referenz: "Satz 1 bis Satz 3"
      if (str_count(neunum_text[2], "\\b(Satz|Halbsatz)\\s[:digit:]\\sbis\\b(Satz|Halbsatz)\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.satz.neu"] <- str_extract(neunum_text[2], "\\b(Satz|Halbsatz)\\s[:digit:]+\\sbis\\b(Satz|Halbsatz)\\s[:digit:]+\\b")
      
      # Referenz: "Satz 1 bis 3"
      if (str_count(neunum_text[2], "\\b(Satz|Halbsatz)\\s[:digit:]\\sbis\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.satz.neu"] <- str_extract(neunum_text[2], "\\b(Satz|Halbsatz)\\s[:digit:]+\\sbis\\s[:digit:]+\\b")
      
      # Referenz: "Sätze 1 bis 3"
      if (str_count(neunum_text[2], "\\b(Sätze|Sätzen)\\s[:digit:]\\sbis\\s[:digit:]+\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.satz.neu"] <- str_extract(neunum_text[2], "\\bSätze\\s[:digit:]+\\sbis\\s[:digit:]+\\b")
      
    }
  } 
}



# Bei Neugesetzen und Änderungsgesetzen funktioniert es anders
# Bei Neugesetzen: Nur innerhalb der gefundenen Hierarchiestufe auffüllen


aenderungs_gesetze <- unique(c(grep("Änderung", legistext$name), grep("wie folgt geändert", legistext$rawtext_left)))
neu_gesetze <- which(is.element(1:nrow(legistext), aenderungs_gesetze) == F)

aenderungs_gesetze <- legistext$idDrucksache[aenderungs_gesetze]
neu_gesetze <- legistext$idDrucksache[neu_gesetze]

if (is.element(amendmentprop$idDrucksacheLegis[amendmentprop_row], neu_gesetze) == T) {
  
  if (nrow(action_matrix)>1 & length(list.dims)>1){
    
    # Für Neugesetze
    for (i in 2:nrow(action_matrix)){
      
      if (str_detect(list.dims[as.numeric(action_matrix[i,"vorschlag.idx"])], "fehlt") == F) {
        if (str_detect(list.dims[as.numeric(action_matrix[i,"vorschlag.idx"])-1], "fehlt") == T)
          abschnitt_uebertrag <- names(which(is.na(action_matrix[as.numeric(action_matrix[i,"vorschlag.idx"])-1,c("ref.art", "ref.num", "ref.buch", "ref.buchbuch", "ref.para", "ref.absatz", "ref.satz")])==F))
        
        uebertrag <- names(which(is.na(action_matrix[as.numeric(action_matrix[i,"vorschlag.idx"]), abschnitt_uebertrag])))
        action_matrix[as.numeric(action_matrix[i,"vorschlag.idx"]),uebertrag] <- action_matrix[as.numeric(action_matrix[i,"vorschlag.idx"])-1,uebertrag]
      }
    }  
    
  }
}


if (is.element(amendmentprop$idDrucksacheLegis[amendmentprop_row], aenderungs_gesetze) == T) {
  
  # Für Änderungsgesetze
  
  if (nrow(action_matrix)>1){  
    
    # ref.art
    for (i in 2:nrow(action_matrix)){
      if(is.na(action_matrix[,"ref.art"])[i] == T)
        action_matrix[i,"ref.art"] <- action_matrix[i-1,"ref.art"]
    }  
    
    # ref.num
    for (i in 2:nrow(action_matrix)){
      if(is.na(action_matrix[,"ref.num"])[i] == T & identical(action_matrix[i,"ref.art"], action_matrix[i-1,"ref.art"]) & identical(action_matrix[i,"ref.art.neu"], action_matrix[i-1,"ref.art.neu"]))
        action_matrix[i,"ref.num"] <- action_matrix[i-1,"ref.num"]
    }
    
    # ref.buch
    for (i in 2:nrow(action_matrix)){
      if(is.na(action_matrix[,"ref.buch"])[i] == T & identical(action_matrix[i,"ref.art"], action_matrix[i-1,"ref.art"]) & identical(action_matrix[i,"ref.num"], action_matrix[i-1,"ref.num"]) & identical(action_matrix[i,"ref.num.neu"], action_matrix[i-1,"ref.num.neu"]))
        action_matrix[i,"ref.buch"] <- action_matrix[i-1,"ref.buch"]
    }  
    
    # ref.buchbuch 
    for (i in 2:nrow(action_matrix)){
      if(is.na(action_matrix[,"ref.buchbuch"])[i] == T & identical(action_matrix[i,"ref.art"], action_matrix[i-1,"ref.art"]) & identical(action_matrix[i,"ref.num"], action_matrix[i-1,"ref.num"]) & identical(action_matrix[i,"ref.buch"], action_matrix[i-1,"ref.buch"]) & identical(action_matrix[i,"ref.buch.neu"], action_matrix[i-1,"ref.buch.neu"]))
        action_matrix[i,"ref.buchbuch"] <- action_matrix[i-1,"ref.buchbuch"]
    } 
    
    # ref.para
    for (i in 2:nrow(action_matrix)){
      if(is.na(action_matrix[,"ref.para"])[i] == T & identical(action_matrix[i,"ref.art"], action_matrix[i-1,"ref.art"]) & identical(action_matrix[i,"ref.num"], action_matrix[i-1,"ref.num"]) & identical(action_matrix[i,"ref.buch"], action_matrix[i-1,"ref.buch"]) & identical(action_matrix[i,"ref.buchbuch"], action_matrix[i-1,"ref.buchbuch"]) & identical(action_matrix[i,"ref.buchbuch.neu"], action_matrix[i-1,"ref.buchbuch.neu"]))
        action_matrix[i,"ref.para"] <- action_matrix[i-1,"ref.para"]
    } 
    
    # ref.absatz
    for (i in 2:nrow(action_matrix)){
      if(is.na(action_matrix[,"ref.absatz"])[i] == T & identical(action_matrix[i,"ref.art"], action_matrix[i-1,"ref.art"]) & identical(action_matrix[i,"ref.num"], action_matrix[i-1,"ref.num"]) & identical(action_matrix[i,"ref.buch"], action_matrix[i-1,"ref.buch"]) & identical(action_matrix[i,"ref.buchbuch"], action_matrix[i-1,"ref.buchbuch"]) & identical(action_matrix[i,"ref.para"], action_matrix[i-1,"ref.para"]) & identical(action_matrix[i,"ref.para.neu"], action_matrix[i-1,"ref.para.neu"]))
        action_matrix[i,"ref.absatz"] <- action_matrix[i-1,"ref.absatz"]
    } 
    
    # ref.satz
    for (i in 2:nrow(action_matrix)){
      if(is.na(action_matrix[,"ref.satz"])[i] == T & identical(action_matrix[i,"ref.art"], action_matrix[i-1,"ref.art"]) & identical(action_matrix[i,"ref.num"], action_matrix[i-1,"ref.num"]) & identical(action_matrix[i,"ref.buch"], action_matrix[i-1,"ref.buch"]) & identical(action_matrix[i,"ref.buchbuch"], action_matrix[i-1,"ref.buchbuch"]) & identical(action_matrix[i,"ref.para"], action_matrix[i-1,"ref.para"]) & identical(action_matrix[i,"ref.absatz"], action_matrix[i-1,"ref.absatz"]) & identical(action_matrix[i,"ref.absatz.neu"], action_matrix[i-1,"ref.absatz.neu"]))
        action_matrix[i,"ref.satz"] <- action_matrix[i-1,"ref.satz"]
    } 
    
  }
}




# ----------------------
# Referenzen auffüllen
# ----------------------
for (i in 1: nrow(action_matrix)) {
  
  if (is.na(action_matrix[i,"einfuegen"])==F){
    
    if (is.na(action_matrix[i,"ref.art.neu"]) == F &  is.na(action_matrix[i,"ref.art"] == T))
      action_matrix[i,"ref.art"] <- str_c(str_split(action_matrix[i,"ref.art.neu"], "[:digit:]")[[1]][1], str_extract(action_matrix[i,"ref.art.neu"], "[:digit:]+"))
    
    if (is.na(action_matrix[i,"ref.num.neu"]) == F &  is.na(action_matrix[i,"ref.num"] == T))
      action_matrix[i,"ref.num"] <- str_c(str_split(action_matrix[i,"ref.num.neu"], "[:digit:]")[[1]][1], str_extract(action_matrix[i,"ref.num.neu"], "[:digit:]+"))
    
    if (is.na(action_matrix[i,"ref.buch.neu"]) == F &  is.na(action_matrix[i,"ref.buch"] == T))
      action_matrix[i,"ref.buch"] <- str_c(str_split(action_matrix[i,"ref.buch.neu"], "\\s[:lower:]")[[1]][1], " ", letters[which(letters==str_split(action_matrix[i,"ref.buch.neu"], "\\s")[[1]][[2]])-1])
    
    if (is.na(action_matrix[i,"ref.buchbuch.neu"]) == F &  is.na(action_matrix[i,"ref.buchbuch"] == T))
      action_matrix[i,"ref.buchbuch"] <- str_c(str_split(action_matrix[i,"ref.buchbuch.neu"], "\\s[:lower:]")[[1]][1], " ", letters[which(letters==substr(str_split(action_matrix[i,"ref.buchbuch.neu"], "\\s")[[1]][[2]],1,1))-1], letters[which(letters==substr(str_split(action_matrix[i,"ref.buchbuch.neu"], "\\s")[[1]][[2]],1,1))-1])
    
    if (is.na(action_matrix[i,"ref.para.neu"]) == F &  is.na(action_matrix[i,"ref.para"] == T))
      action_matrix[i,"ref.para"] <- str_c(str_split(action_matrix[i,"ref.para.neu"], "[:digit:]")[[1]][1], as.numeric(str_extract(action_matrix[i,"ref.para.neu"], "[:digit:]+"))-1)
    
    if (is.na(action_matrix[i,"ref.absatz.neu"]) == F &  is.na(action_matrix[i,"ref.absatz"] == T))
      action_matrix[i,"ref.absatz"] <- str_c(str_split(action_matrix[i,"ref.absatz.neu"], "[:digit:]")[[1]][1], as.numeric(str_extract(action_matrix[i,"ref.absatz.neu"], "[:digit:]+"))-1)
    
    if (is.na(action_matrix[i,"ref.satz.neu"]) == F &  is.na(action_matrix[i,"ref.satz"] == T))
      action_matrix[i,"ref.satz"] <- str_c(str_split(action_matrix[i,"ref.satz.neu"], "[:digit:]")[[1]][1], as.numeric(str_extract(action_matrix[i,"ref.satz.neu"], "[:digit:]+"))-1)
    
    
  }
  
}



# ------------
# spec.satz
# ------------

for (i in 1:length(list.dims)){
  
  # Vorschläge die mehrere Funktionen ausführen überspringen (manuell kodieren)
  if (length(action_matrix[action_matrix[,"vorschlag.idx"]==i,"vorschlag.idx"]) == 1) {
    
    
    # gegeben, dass loeschen-Funktion aktiviert wird
    if (is.na(action_matrix[action_matrix[,"vorschlag.idx"]==i,"loeschen"]) == F) {
      
      # Referenz: "Satz"
      if (str_count(eval(parse(text=str_c("amendment_list", list.dims[i]))), "\\bSatz\\s([:lower:]|„)\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"spec.satz"] <- rm_between(paste(eval(parse(text=str_c("amendment_list", list.dims[i]))), collapse=" "), '„', '“', extract=T)[[1]]
      
      # Referenz: "Sätze"
      if (str_count(eval(parse(text=str_c("amendment_list", list.dims[i]))), "\\b(Sätze|Sätzen)\\s([:lower:]|„)\\b") == 1) 
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"spec.satz"] <- rm_between(paste(eval(parse(text=str_c("amendment_list", list.dims[i]))), collapse=" "), '„', '“', extract=T)[[1]]
      
    }
    
    # gegeben, dass einfuegen-Funktion aktiviert wird (und nach spec.satz text.neu eingefügt werden soll)
    if (is.na(action_matrix[action_matrix[,"vorschlag.idx"]==i,"einfuegen"]) == F) {
      
      # Referenz: "Satz"
      if (str_count(eval(parse(text=str_c("amendment_list", list.dims[i]))), "\\bSatz\\s„\\b") > 0){
        if (lengths(rm_between(paste(eval(parse(text=str_c("amendment_list", list.dims[i]))), collapse=" "), '„', '“', extract=T)) == 2)
          action_matrix[action_matrix[,"vorschlag.idx"]==i,"spec.satz"] <- rm_between(paste(eval(parse(text=str_c("amendment_list", list.dims[i]))), collapse=" "), '„', '“', extract=T)[[1]][1]
      }
      
      # Referenz: "Sätze"
      if (str_count(eval(parse(text=str_c("amendment_list", list.dims[i]))), "\\b(Sätze|Sätzen)\\s„\\b") > 0){
        if (lengths(rm_between(paste(eval(parse(text=str_c("amendment_list", list.dims[i]))), collapse=" "), '„', '“', extract=T)) == 2)
          action_matrix[action_matrix[,"vorschlag.idx"]==i,"spec.satz"] <- rm_between(paste(eval(parse(text=str_c("amendment_list", list.dims[i]))), collapse=" "), '„', '“', extract=T)[[1]][1]
      }
      
      
      
    }  
    
  }    
  
}


# -----------
# spec.wort 
# -----------

for (i in 1:length(list.dims)){
  
  # Vorschläge die mehrere Funktionen ausführen überspringen (manuell kodieren)
  if (length(action_matrix[action_matrix[,"vorschlag.idx"]==i,"vorschlag.idx"]) == 1) {
    
    # Referenz: "Wort"
    if (sum(str_count(eval(parse(text=str_c("amendment_list", list.dims[i]))), "(\\bWort\\s([:lower:]|„)|\\b(Wörter|Wörtern)\\s([:lower:]|„)|\\b(Worte|Worten)\\s([:lower:]|„)|\\b(Angabe|Angaben)\\s([:lower:]|„))")) > 0) 
      action_matrix[action_matrix[,"vorschlag.idx"]==i,"spec.wort"] <- rm_between(paste(eval(parse(text=str_c("amendment_list", list.dims[i]))), collapse=" "), '„', '“', extract=T)[[1]][1]
    
    # # Referenz: "Wörter"
    # if (str_count(eval(parse(text=str_c("amendment_list", list.dims[i]))), "\\b(Wörter|Wörtern)\\s([:lower:]|„)\\b") > 0) 
    #   action_matrix[action_matrix[,"vorschlag.idx"]==i,"spec.wort"] <- rm_between(paste(eval(parse(text=str_c("amendment_list", list.dims[i]))), collapse=" "), '„', '“', extract=T)[[1]][1]
    # 
    # # Referenz: "Worte"
    # if (str_count(eval(parse(text=str_c("amendment_list", list.dims[i]))), "\\b(Worte|Worten)\\s([:lower:]|„)\\b") > 0) 
    #   action_matrix[action_matrix[,"vorschlag.idx"]==i,"spec.wort"] <- rm_between(paste(eval(parse(text=str_c("amendment_list", list.dims[i]))), collapse=" "), '„', '“', extract=T)[[1]][1]
    # 
    # # Referenz: "Angabe"
    # if (str_count(eval(parse(text=str_c("amendment_list", list.dims[i]))), "\\b(Angabe|Angaben)\\s([:lower:]|„)\\b")> 0) 
    #   action_matrix[action_matrix[,"vorschlag.idx"]==i,"spec.wort"] <- rm_between(paste(eval(parse(text=str_c("amendment_list", list.dims[i]))), collapse=" "), '„', '“', extract=T)[[1]][1]
    
  }    
}


# -----------
# text.neu
# -----------

anfuehrungs_zeichen <- c("„", "“", "‘‘")

for (i in 1:length(list.dims)){
  
  # Vorschläge die mehrere Funktionen ausführen überspringen (manuell kodieren)
  if (length(action_matrix[action_matrix[,"vorschlag.idx"]==i,"vorschlag.idx"]) == 1) {
    
    citations <- c(str_detect(paste(eval(parse(text=str_c("amendment_list", list.dims[i]))), collapse=" "), "„"),
                   str_detect(paste(eval(parse(text=str_c("amendment_list", list.dims[i]))), collapse=" "), "“"),
                   str_detect(paste(eval(parse(text=str_c("amendment_list", list.dims[i]))), collapse=" "), "‘‘"))
    
    # Sofern es Anführungsstriche gibt
    if (length(which(citations)) > 0) {
      
      # if spec.satz & spec.wort == NA
      if (is.na(action_matrix[action_matrix[,"vorschlag.idx"]==i,"spec.satz"]) == T & is.na(action_matrix[action_matrix[,"vorschlag.idx"]==i,"spec.wort"]) == T) {
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"text.neu"] <- rm_between(paste(eval(parse(text=str_c("amendment_list", list.dims[i]))), collapse=" "), anfuehrungs_zeichen[which(citations)[1]], anfuehrungs_zeichen[which(citations)[2]], extract=T)[[1]][1] 
        
      } else {
        action_matrix[action_matrix[,"vorschlag.idx"]==i,"text.neu"] <- rm_between(paste(eval(parse(text=str_c("amendment_list", list.dims[i]))), collapse=" "), anfuehrungs_zeichen[which(citations)[1]], anfuehrungs_zeichen[which(citations)[2]], extract=T)[[1]][2] 
        
      }  
      
    }
    
  }    
  
}



# --------
# e.list
# --------

for (i in 1:length(list.dims)) {
  
  # Vorschläge die mehrere Funktionen ausführen überspringen (manuell kodieren)
  if (length(action_matrix[action_matrix[,"vorschlag.idx"]==i,"vorschlag.idx"]) == 1) {
    
    if (is.na(action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.para"]) == T & is.na(action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.absatz"]) == T & is.na(action_matrix[action_matrix[,"vorschlag.idx"]==i,"ref.satz"]) == T & is.na(action_matrix[action_matrix[,"vorschlag.idx"]==i,"spec.satz"]) == T & is.na(action_matrix[action_matrix[,"vorschlag.idx"]==i,"spec.wort"]) == T) {
      
      action_matrix[action_matrix[,"vorschlag.idx"]==i,"e.list"] <- T 
      
    } else {
      
      action_matrix[action_matrix[,"vorschlag.idx"]==i,"e.list"] <- F }
    
  }
}



#############################################################################################################################################

# ---------------------------------------------------------------
# Einträge in action_matrix kompatibel zu Funktionsinput machen 
# ---------------------------------------------------------------

# Alle Änderungsvorschläge löschen, die multiple Funktionen aktivieren (manuell kodieren)
for (i in 1:length(list.dims)) {
  
  if (length(action_matrix[action_matrix[,"vorschlag.idx"]==i,"vorschlag.idx"]) > 1) 
    action_matrix <- action_matrix[!(action_matrix[,"vorschlag.idx"]==i),]
}


# ref.art & ref.art.neu
action_matrix[,"ref.art"] <- str_replace_all(action_matrix[,"ref.art"], "Artikel", "")
action_matrix[,"ref.art"] <- str_replace_all(action_matrix[,"ref.art"], ",", "")

action_matrix[,"ref.art.neu"] <- str_replace_all(action_matrix[,"ref.art.neu"], "Artikel", "")
action_matrix[,"ref.art.neu"] <- str_replace_all(action_matrix[,"ref.art.neu"], ",", "")

for (i in 1: nrow(action_matrix)) {
  
  if (length(grep("bis", action_matrix[i,"ref.art"])) == 1) {
    # Matrix erweitern
    n.new.rows <- as.numeric(strsplit(action_matrix[i,"ref.art"], "bis")[[1]][2]) - as.numeric(strsplit(action_matrix[i,"ref.art"], "bis")[[1]][1])
    if (i < nrow(action_matrix))
      action_matrix <- rbind(action_matrix[1:i,], matrix(rep(action_matrix[i,], n.new.rows), nrow=n.new.rows, ncol=ncol(action_matrix), byrow=T), action_matrix[(i+1):nrow(action_matrix),])
    if (i == nrow(action_matrix))
      action_matrix <- rbind(action_matrix[1:i,], matrix(rep(action_matrix[i,], n.new.rows), nrow=n.new.rows, ncol=ncol(action_matrix), byrow=T))
    
    # Matrix auffüllen
    action_matrix[i:(i+n.new.rows), "ref.art"] <- seq(as.numeric(str_extract_all(action_matrix[i,"ref.art"], "[:digit:]+")[[1]][1]), as.numeric(str_extract_all(action_matrix[i,"ref.art"], "[:digit:]+")[[1]][2]))
    
    if (length(grep("bis", action_matrix[i,"ref.art.neu"])) == 1)
      action_matrix[i:(i+n.new.rows), "ref.art.neu"] <- seq(as.numeric(str_extract_all(action_matrix[i,"ref.art.neu"], "[:digit:]+")[[1]][1]), as.numeric(str_extract_all(action_matrix[i,"ref.art.neu"], "[:digit:]+")[[1]][2]))
    if (length(grep("und", action_matrix[i,"ref.art.neu"])) == 1)
      action_matrix[i:(i+n.new.rows), "ref.art.neu"] <- str_extract_all(action_matrix[i,"ref.art.neu"], "[:digit:]+")[[1]]
  }
  
  if (length(grep("und", action_matrix[i,"ref.art"])) == 1) {
    # Matrix erweitern
    n.new.rows <- lengths(str_extract_all(action_matrix[i,"ref.art"], "[:digit:]+"))
    
    if (i < nrow(action_matrix))
      action_matrix <- rbind(action_matrix[1:i-1,], matrix(rep(action_matrix[i,], n.new.rows), nrow=n.new.rows, ncol=ncol(action_matrix), byrow=T), action_matrix[(i+1):nrow(action_matrix),])
    
    if (i == nrow(action_matrix))
      action_matrix <- rbind(action_matrix[1:i-1,], matrix(rep(action_matrix[i,], n.new.rows), nrow=n.new.rows, ncol=ncol(action_matrix), byrow=T))
    
    # Matrix auffüllen
    action_matrix[i:(i+n.new.rows-1), "ref.art"] <- str_extract_all(action_matrix[i,"ref.art"], "[:digit:]+")[[1]]
    
    if (length(grep("und", action_matrix[i,"ref.art.neu"])) == 1)
      action_matrix[i:(i+n.new.rows-1), "ref.art.neu"] <- str_extract_all(action_matrix[i,"ref.art.neu"], "[:digit:]+")[[1]]
    if (length(grep("bis", action_matrix[i,"ref.art.neu"])) == 1)
      action_matrix[i:(i+n.new.rows-1), "ref.art.neu"] <- seq(as.numeric(str_extract_all(action_matrix[i,"ref.art.neu"], "[:digit:]+")[[1]][1]), as.numeric(str_extract_all(action_matrix[i,"ref.art.neu"], "[:digit:]+")[[1]][2]))
    
  }
  
}

action_matrix[,"ref.art"] <- str_replace_all(action_matrix[,"ref.art"], " ", "")  
action_matrix[,"ref.art"] <- str_c("Artikel_", action_matrix[,"ref.art"])

action_matrix[,"ref.art.neu"] <- str_replace_all(action_matrix[,"ref.art.neu"], " ", "")  
action_matrix[,"ref.art.neu"] <- str_c("Artikel_", action_matrix[,"ref.art.neu"])



# ref.num & ref.num.neu
action_matrix[,"ref.num"] <- str_replace_all(action_matrix[,"ref.num"], "(Nummern|Nummer|Nr|Ziffern|Ziffer)", "")
action_matrix[,"ref.num"] <- str_replace_all(action_matrix[,"ref.num"], "(,|[:punct:])", "")

action_matrix[,"ref.num.neu"] <- str_replace_all(action_matrix[,"ref.num.neu"], "(Nummern|Nummer|Nr|Ziffern|Ziffer)", "")
action_matrix[,"ref.num.neu"] <- str_replace_all(action_matrix[,"ref.num.neu"], "(,|[:punct:])", "")

for (i in 1: nrow(action_matrix)) {
  
  if (length(grep("bis", action_matrix[i,"ref.num"])) == 1) {
    # Matrix erweitern
    n.new.rows <- as.numeric(strsplit(action_matrix[i,"ref.num"], "bis")[[1]][2]) - as.numeric(strsplit(action_matrix[i,"ref.num"], "bis")[[1]][1])
    if (i < nrow(action_matrix))
      action_matrix <- rbind(action_matrix[1:i,], matrix(rep(action_matrix[i,], n.new.rows), nrow=n.new.rows, ncol=ncol(action_matrix), byrow=T), action_matrix[(i+1):nrow(action_matrix),])
    if (i == nrow(action_matrix))
      action_matrix <- rbind(action_matrix[1:i,], matrix(rep(action_matrix[i,], n.new.rows), nrow=n.new.rows, ncol=ncol(action_matrix), byrow=T))
    
    # Matrix auffüllen
    action_matrix[i:(i+n.new.rows), "ref.num"] <- seq(as.numeric(str_extract_all(action_matrix[i,"ref.num"], "[:digit:]+")[[1]][1]), as.numeric(str_extract_all(action_matrix[i,"ref.num"], "[:digit:]+")[[1]][2]))
    
    if (length(grep("bis", action_matrix[i,"ref.num.neu"])) == 1)
      action_matrix[i:(i+n.new.rows), "ref.num.neu"] <- seq(as.numeric(str_extract_all(action_matrix[i,"ref.num.neu"], "[:digit:]+")[[1]][1]), as.numeric(str_extract_all(action_matrix[i,"ref.num.neu"], "[:digit:]+")[[1]][2]))
    if (length(grep("und", action_matrix[i,"ref.num.neu"])) == 1)
      action_matrix[i:(i+n.new.rows), "ref.num.neu"] <- str_extract_all(action_matrix[i,"ref.num.neu"], "[:digit:]+")[[1]]
  }
  
  if (length(grep("und", action_matrix[i,"ref.num"])) == 1) {
    # Matrix erweitern
    n.new.rows <- lengths(str_extract_all(action_matrix[i,"ref.num"], "[:digit:]+"))
    if (i < nrow(action_matrix))
      action_matrix <- rbind(action_matrix[1:i-1,], matrix(rep(action_matrix[i,], n.new.rows), nrow=n.new.rows, ncol=ncol(action_matrix), byrow=T), action_matrix[(i+1):nrow(action_matrix),])
    if (i == nrow(action_matrix))
      action_matrix <- rbind(action_matrix[1:i-1,], matrix(rep(action_matrix[i,], n.new.rows), nrow=n.new.rows, ncol=ncol(action_matrix), byrow=T))
    
    # Matrix auffüllen
    action_matrix[i:(i+n.new.rows-1), "ref.num"] <- str_extract_all(action_matrix[i,"ref.num"], "[:digit:]+")[[1]]
    
    if (length(grep("und", action_matrix[i,"ref.num.neu"])) == 1)
      action_matrix[i:(i+n.new.rows-1), "ref.num.neu"] <- str_extract_all(action_matrix[i,"ref.num.neu"], "[:digit:]+")[[1]]
    if (length(grep("bis", action_matrix[i,"ref.num.neu"])) == 1)
      action_matrix[i:(i+n.new.rows-1), "ref.num.neu"] <- seq(as.numeric(str_extract_all(action_matrix[i,"ref.num.neu"], "[:digit:]+")[[1]][1]), as.numeric(str_extract_all(action_matrix[i,"ref.num.neu"], "[:digit:]+")[[1]][2]))
    
  }
  
}

action_matrix[,"ref.num"] <- str_replace_all(action_matrix[,"ref.num"], " ", "")  
action_matrix[,"ref.num"] <- str_c("Nummer_", action_matrix[,"ref.num"])

action_matrix[,"ref.num.neu"] <- str_replace_all(action_matrix[,"ref.num.neu"], " ", "")  
action_matrix[,"ref.num.neu"] <- str_c("Nummer_", action_matrix[,"ref.num.neu"])



# ref.buch & ref.buch.neu
action_matrix[,"ref.buch"] <- str_replace_all(action_matrix[,"ref.buch"], "\\b(Buchstabe|Buchstaben|Buchst)\\b", "")
action_matrix[,"ref.buch"] <- str_replace_all(action_matrix[,"ref.buch"], "(,|[:punct:])", "")

action_matrix[,"ref.buch.neu"] <- str_replace_all(action_matrix[,"ref.buch.neu"], "\\b(Buchstabe|Buchstaben|Buchst)\\b", "")
action_matrix[,"ref.buch.neu"] <- str_replace_all(action_matrix[,"ref.buch.neu"], "(,|[:punct:])", "")

for (i in 1: nrow(action_matrix)) {
  
  if (length(grep("bis", action_matrix[i,"ref.buch"])) == 1) {
    # Matrix erweitern
    n.new.rows <- which(letters == str_extract_all(action_matrix[i,"ref.buch"], "[:lower:]+")[[1]][3]) - which(letters == str_extract_all(action_matrix[i,"ref.buch"], "[:lower:]+")[[1]][1])
    if (i < nrow(action_matrix))
      action_matrix <- rbind(action_matrix[1:i,], matrix(rep(action_matrix[i,], n.new.rows), nrow=n.new.rows, ncol=ncol(action_matrix), byrow=T), action_matrix[(i+1):nrow(action_matrix),])
    if (i == nrow(action_matrix))
      action_matrix <- rbind(action_matrix[1:i,], matrix(rep(action_matrix[i,], n.new.rows), nrow=n.new.rows, ncol=ncol(action_matrix), byrow=T))
    
    # Matrix auffüllen
    action_matrix[i:(i+n.new.rows), "ref.buch"] <- letters[which(letters == str_extract_all(action_matrix[i,"ref.buch"], "[:lower:]+")[[1]][1]):which(letters == str_extract_all(action_matrix[i,"ref.buch"], "[:lower:]+")[[1]][3])]
    
    if (length(grep("bis", action_matrix[i,"ref.buch.neu"])) == 1)
      action_matrix[i:(i+n.new.rows), "ref.buch.neu"] <- letters[which(letters == str_extract_all(action_matrix[i,"ref.buch.neu"], "[:lower:]+")[[1]][1]):which(letters == str_extract_all(action_matrix[i,"ref.buch.neu"], "[:lower:]+")[[1]][3])]
    if (length(grep("und", action_matrix[i,"ref.buch.neu"])) == 1) {
      action_matrix[i,"ref.buch.neu"] <- str_replace_all(action_matrix[i,"ref.buch.neu"], "\\bund\\b", "")
      action_matrix[i:(i+n.new.rows), "ref.buch.neu"] <- str_extract_all(action_matrix[i,"ref.buch.neu"], "[:lower:]+")[[1]]
    }
  }
  
  if (length(grep("und", action_matrix[i,"ref.buch"])) == 1) {
    # Matrix erweitern
    action_matrix[i,"ref.buch"] <- str_replace_all(action_matrix[i,"ref.buch"], "\\bund\\b", "")
    
    n.new.rows <- lengths(str_extract_all(action_matrix[i,"ref.buch"], "[:lower:]+"))
    if (i < nrow(action_matrix))
      action_matrix <- rbind(action_matrix[1:i-1,], matrix(rep(action_matrix[i,], n.new.rows), nrow=n.new.rows, ncol=ncol(action_matrix), byrow=T), action_matrix[(i+1):nrow(action_matrix),])
    if (i == nrow(action_matrix))
      action_matrix <- rbind(action_matrix[1:i-1,], matrix(rep(action_matrix[i,], n.new.rows), nrow=n.new.rows, ncol=ncol(action_matrix), byrow=T))
    
    # Matrix auffüllen
    action_matrix[i:(i+n.new.rows-1), "ref.buch"] <- str_extract_all(action_matrix[i,"ref.buch"], "[:lower:]+")[[1]]
    
    if (length(grep("und", action_matrix[i,"ref.buch.neu"])) == 1){
      action_matrix[i,"ref.buch.neu"] <- str_replace_all(action_matrix[i,"ref.buch.neu"], "\\bund\\b", "")
      action_matrix[i:(i+n.new.rows-1), "ref.buch.neu"] <- str_extract_all(action_matrix[i,"ref.buch.neu"], "[:lower:]+")[[1]]
    }
    if (length(grep("bis", action_matrix[i,"ref.buch.neu"])) == 1){
      action_matrix[i,"ref.buch.neu"] <- str_replace_all(action_matrix[i,"ref.buch.neu"], "\\bbis\\b", "")
      action_matrix[i:(i+n.new.rows-1), "ref.buch.neu"] <- letters[which(letters == str_extract_all(action_matrix[i,"ref.buch.neu"], "[:lower:]+")[[1]][1]):which(letters == str_extract_all(action_matrix[i,"ref.buch.neu"], "[:lower:]+")[[1]][2])]
    }
  }
  
}

action_matrix[,"ref.buch"] <- str_replace_all(action_matrix[,"ref.buch"], " ", "")  
action_matrix[,"ref.buch"] <- str_c("Buchstabe_", action_matrix[,"ref.buch"])

action_matrix[,"ref.buch.neu"] <- str_replace_all(action_matrix[,"ref.buch.neu"], " ", "")  
action_matrix[,"ref.buch.neu"] <- str_c("Buchstabe_", action_matrix[,"ref.buch.neu"])

for (i in 1: nrow(action_matrix)){
  if (is.na(action_matrix[i,"ref.buch.neu"]) == F & action_matrix[i,"ref.buch.neu"] == "Buchstabe_a") {
    levels(action_matrix[,"ref.buch"]) <- c(levels(action_matrix[,"ref.buch"]), "Buchstabe_fehlt")
    action_matrix[i,"ref.buch"] <- "Buchstabe_fehlt"
  }
}


# ref.buchbuch & ref.buchbuch.neu
action_matrix[,"ref.buchbuch"] <- str_replace_all(action_matrix[,"ref.buchbuch"], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\b", "")
action_matrix[,"ref.buchbuch"] <- str_replace_all(action_matrix[,"ref.buchbuch"], "(,|[:punct:])", "")

action_matrix[,"ref.buchbuch.neu"] <- str_replace_all(action_matrix[,"ref.buchbuch.neu"], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\b", "")
action_matrix[,"ref.buchbuch.neu"] <- str_replace_all(action_matrix[,"ref.buchbuch.neu"], "(,|[:punct:])", "")

for (i in 1: nrow(action_matrix)) {
  
  if (length(grep("bis", action_matrix[i,"ref.buchbuch"])) == 1) {
    # Matrix erweitern
    n.new.rows <- which(letters == substr(str_extract_all(action_matrix[i,"ref.buchbuch"], "[:lower:]+")[[1]][3], 1, 1)) - which(letters == substr(str_extract_all(action_matrix[i,"ref.buchbuch"], "[:lower:]+")[[1]][1], 1, 1))
    if (i < nrow(action_matrix))
      action_matrix <- rbind(action_matrix[1:i,], matrix(rep(action_matrix[i,], n.new.rows), nrow=n.new.rows, ncol=ncol(action_matrix), byrow=T), action_matrix[(i+1):nrow(action_matrix),])
    if (i == nrow(action_matrix))
      action_matrix <- rbind(action_matrix[1:i,], matrix(rep(action_matrix[i,], n.new.rows), nrow=n.new.rows, ncol=ncol(action_matrix), byrow=T))
    
    # Matrix auffüllen
    action_matrix[i:(i+n.new.rows), "ref.buchbuch"] <- letters[which(letters == substr(str_extract_all(action_matrix[i,"ref.buchbuch"], "[:lower:]+")[[1]][1], 1, 1)):which(letters == substr(str_extract_all(action_matrix[i,"ref.buchbuch"], "[:lower:]+")[[1]][3], 1, 1))]
    action_matrix[i:(i+n.new.rows), "ref.buchbuch"] <- str_c(action_matrix[i:(i+n.new.rows), "ref.buchbuch"], action_matrix[i:(i+n.new.rows), "ref.buchbuch"])
    
    if (length(grep("bis", action_matrix[i,"ref.buchbuch.neu"])) == 1){
      action_matrix[i:(i+n.new.rows), "ref.buchbuch.neu"] <- letters[which(letters == substr(str_extract_all(action_matrix[i,"ref.buchbuch.neu"], "[:lower:]+")[[1]][1], 1, 1)):which(letters == substr(str_extract_all(action_matrix[i,"ref.buchbuch.neu"], "[:lower:]+")[[1]][3], 1, 1))]
      action_matrix[i:(i+n.new.rows), "ref.buchbuch.neu"] <- str_c(action_matrix[i:(i+n.new.rows), "ref.buchbuch.neu"], action_matrix[i:(i+n.new.rows), "ref.buchbuch.neu"])
    }
    if (length(grep("und", action_matrix[i,"ref.buchbuch.neu"])) == 1) {
      action_matrix[i,"ref.buchbuch.neu"] <- str_replace_all(action_matrix[i,"ref.buchbuch.neu"], "\\bund\\b", "")
      action_matrix[i:(i+n.new.rows), "ref.buchbuch.neu"] <- str_extract_all(action_matrix[i,"ref.buchbuch.neu"], "[:lower:]+")[[1]]
    }
  }
  
  if (length(grep("und", action_matrix[i,"ref.buchbuch"])) == 1) {
    # Matrix erweitern
    action_matrix[i,"ref.buchbuch"] <- str_replace_all(action_matrix[i,"ref.buchbuch"], "\\bund\\b", "")
    
    n.new.rows <- lengths(str_extract_all(action_matrix[i,"ref.buchbuch"], "[:lower:]+"))
    if (i < nrow(action_matrix))
      action_matrix <- rbind(action_matrix[1:i-1,], matrix(rep(action_matrix[i,], n.new.rows), nrow=n.new.rows, ncol=ncol(action_matrix), byrow=T), action_matrix[(i+1):nrow(action_matrix),])
    if (i == nrow(action_matrix))
      action_matrix <- rbind(action_matrix[1:i-1,], matrix(rep(action_matrix[i,], n.new.rows), nrow=n.new.rows, ncol=ncol(action_matrix), byrow=T))
    
    # Matrix auffüllen
    action_matrix[i:(i+n.new.rows-1), "ref.buchbuch"] <- str_extract_all(action_matrix[i,"ref.buchbuch"], "[:lower:]+")[[1]]
    
    if (length(grep("und", action_matrix[i,"ref.buchbuch.neu"])) == 1){
      action_matrix[i,"ref.buchbuch.neu"] <- str_replace_all(action_matrix[i,"ref.buchbuch.neu"], "\\bund\\b", "")
      action_matrix[i:(i+n.new.rows-1), "ref.buchbuch.neu"] <- str_extract_all(action_matrix[i,"ref.buchbuch.neu"], "[:lower:]+")[[1]]
    }
    if (length(grep("bis", action_matrix[i,"ref.buchbuch.neu"])) == 1){
      action_matrix[i,"ref.buchbuch.neu"] <- str_replace_all(action_matrix[i,"ref.buchbuch.neu"], "\\bbis\\b", "")
      action_matrix[i:(i+n.new.rows-1), "ref.buchbuch.neu"] <- letters[which(letters == substr(str_extract_all(action_matrix[i,"ref.buchbuch.neu"], "[:lower:]+")[[1]][1], 1, 1)): which(letters == substr(str_extract_all(action_matrix[i,"ref.buchbuch.neu"], "[:lower:]+")[[1]][2], 1, 1))]
      action_matrix[i:(i+n.new.rows-1), "ref.buchbuch.neu"] <- str_c(action_matrix[i:(i+n.new.rows-1), "ref.buchbuch.neu"], action_matrix[i:(i+n.new.rows-1), "ref.buchbuch.neu"])
    }
  }
  
}

action_matrix[,"ref.buchbuch"] <- str_replace_all(action_matrix[,"ref.buchbuch"], " ", "")  
action_matrix[,"ref.buchbuch"] <- str_c("Buchstabe_", action_matrix[,"ref.buchbuch"])

action_matrix[,"ref.buchbuch.neu"] <- str_replace_all(action_matrix[,"ref.buchbuch.neu"], " ", "")  
action_matrix[,"ref.buchbuch.neu"] <- str_c("Buchstabe_", action_matrix[,"ref.buchbuch.neu"])

for (i in 1: nrow(action_matrix)){
  if (is.na(action_matrix[i,"ref.buchbuch.neu"]) == F & action_matrix[i,"ref.buchbuch.neu"] == "Buchstabe_aa") {
    levels(action_matrix[,"ref.buchbuch"]) <- c(levels(action_matrix[,"ref.buchbuch"]), "Buchstabe_fehlt")
    action_matrix[i,"ref.buchbuch"] <- "Buchstabe_fehlt"
  }
}


# ref.para & ref.para.neu
action_matrix[,"ref.para"] <- str_replace_all(action_matrix[,"ref.para"], "§", "")
action_matrix[,"ref.para"] <- str_replace_all(action_matrix[,"ref.para"], ",", "")

action_matrix[,"ref.para.neu"] <- str_replace_all(action_matrix[,"ref.para.neu"], "§", "")
action_matrix[,"ref.para.neu"] <- str_replace_all(action_matrix[,"ref.para.neu"], ",", "")

for (i in 1: nrow(action_matrix)) {
  
  if (length(grep("bis", action_matrix[i,"ref.para"])) == 1) {
    
    # Matrix erweitern
    n.new.rows <- as.numeric(strsplit(action_matrix[i,"ref.para"], "bis")[[1]][2]) - as.numeric(strsplit(action_matrix[i,"ref.para"], "bis")[[1]][1])
    if (i < nrow(action_matrix))
      action_matrix <- rbind(action_matrix[1:i,], matrix(rep(action_matrix[i,], n.new.rows), nrow=n.new.rows, ncol=ncol(action_matrix), byrow=T), action_matrix[(i+1):nrow(action_matrix),])
    if (i == nrow(action_matrix))
      action_matrix <- rbind(action_matrix[1:i,], matrix(rep(action_matrix[i,], n.new.rows), nrow=n.new.rows, ncol=ncol(action_matrix), byrow=T))
    
    # Matrix auffüllen
    action_matrix[i:(i+n.new.rows), "ref.para"] <- seq(as.numeric(str_extract_all(action_matrix[i,"ref.para"], "[:digit:]+")[[1]][1]), as.numeric(str_extract_all(action_matrix[i,"ref.para"], "[:digit:]+")[[1]][2]))
    
    if (length(grep("bis", action_matrix[i,"ref.para.neu"])) == 1) 
      action_matrix[i:(i+n.new.rows), "ref.para.neu"] <- seq(as.numeric(str_extract_all(action_matrix[i,"ref.para.neu"], "[:digit:]+")[[1]][1]), as.numeric(str_extract_all(action_matrix[i,"ref.para.neu"], "[:digit:]+")[[1]][2]))
    if (length(grep("und", action_matrix[i,"ref.para.neu"])) == 1)
      action_matrix[i:(i+n.new.rows), "ref.para.neu"] <- str_extract_all(action_matrix[i,"ref.para.neu"], "[:digit:]+")[[1]]
  } 
  
  
  
  if (length(grep("und", action_matrix[i,"ref.para"])) == 1) {
    
    # Matrix erweitern
    n.new.rows <- lengths(str_extract_all(action_matrix[i,"ref.para"], "[:digit:]+"))
    if (i < nrow(action_matrix))
      action_matrix <- rbind(action_matrix[1:i-1,], matrix(rep(action_matrix[i,], n.new.rows), nrow=n.new.rows, ncol=ncol(action_matrix), byrow=T), action_matrix[(i+1):nrow(action_matrix),])
    if (i == nrow(action_matrix))
      action_matrix <- rbind(action_matrix[1:i-1,], matrix(rep(action_matrix[i,], n.new.rows), nrow=n.new.rows, ncol=ncol(action_matrix), byrow=T))
    
    # Matrix auffüllen
    action_matrix[i:(i+n.new.rows-1), "ref.para"] <- str_extract_all(action_matrix[i,"ref.para"], "[:digit:]+")[[1]]
    
    if (length(grep("und", action_matrix[i,"ref.para.neu"])) == 1)
      action_matrix[i:(i+n.new.rows-1), "ref.para.neu"] <- str_extract_all(action_matrix[i,"ref.para.neu"], "[:digit:]+")[[1]]
    if (length(grep("bis", action_matrix[i,"ref.para.neu"])) == 1) 
      action_matrix[i:(i+n.new.rows-1), "ref.para.neu"] <- seq(as.numeric(str_extract_all(action_matrix[i,"ref.para.neu"], "[:digit:]+")[[1]][1]), as.numeric(str_extract_all(action_matrix[i,"ref.para.neu"], "[:digit:]+")[[1]][2]))
  }
  
}

action_matrix[,"ref.para"] <- str_replace_all(action_matrix[,"ref.para"], " ", "")
action_matrix[,"ref.para.neu"] <- str_replace_all(action_matrix[,"ref.para.neu"], " ", "")




# ref.absatz & ref.absatz.neu
action_matrix[,"ref.absatz"] <- str_replace_all(action_matrix[,"ref.absatz"], "Abs[:punct:]", "")
action_matrix[,"ref.absatz"] <- str_replace_all(action_matrix[,"ref.absatz"], "Absatz", "")
action_matrix[,"ref.absatz"] <- str_replace_all(action_matrix[,"ref.absatz"], "Absätze", "")
action_matrix[,"ref.absatz"] <- str_replace_all(action_matrix[,"ref.absatz"], ",", "")

action_matrix[,"ref.absatz.neu"] <- str_replace_all(action_matrix[,"ref.absatz.neu"], "Abs[:punct:]", "")
action_matrix[,"ref.absatz.neu"] <- str_replace_all(action_matrix[,"ref.absatz.neu"], "Absatz", "")
action_matrix[,"ref.absatz.neu"] <- str_replace_all(action_matrix[,"ref.absatz.neu"], "Absätze", "")
action_matrix[,"ref.absatz.neu"] <- str_replace_all(action_matrix[,"ref.absatz.neu"], ",", "")

for (i in 1: nrow(action_matrix)) {
  
  if (length(grep("bis", action_matrix[i,"ref.absatz"])) == 1) {
    # Matrix erweitern
    n.new.rows <- as.numeric(strsplit(action_matrix[i,"ref.absatz"], "bis")[[1]][2]) - as.numeric(strsplit(action_matrix[i,"ref.absatz"], "bis")[[1]][1])
    if (i < nrow(action_matrix))
      action_matrix <- rbind(action_matrix[1:i,], matrix(rep(action_matrix[i,], n.new.rows), nrow=n.new.rows, ncol=ncol(action_matrix), byrow=T), action_matrix[(i+1):nrow(action_matrix),])
    if (i == nrow(action_matrix))
      action_matrix <- rbind(action_matrix[1:i,], matrix(rep(action_matrix[i,], n.new.rows), nrow=n.new.rows, ncol=ncol(action_matrix), byrow=T))
    
    # Matrix auffüllen
    action_matrix[i:(i+n.new.rows), "ref.absatz"] <- seq(as.numeric(str_extract_all(action_matrix[i,"ref.absatz"], "[:digit:]+")[[1]][1]), as.numeric(str_extract_all(action_matrix[i,"ref.absatz"], "[:digit:]+")[[1]][2]))
    
    if (length(grep("bis", action_matrix[i,"ref.absatz.neu"])) == 1)
      action_matrix[i:(i+n.new.rows), "ref.absatz.neu"] <- seq(as.numeric(str_extract_all(action_matrix[i,"ref.absatz.neu"], "[:digit:]+")[[1]][1]), as.numeric(str_extract_all(action_matrix[i,"ref.absatz.neu"], "[:digit:]+")[[1]][2]))
    if (length(grep("und", action_matrix[i,"ref.absatz.neu"])) == 1)
      action_matrix[i:(i+n.new.rows), "ref.absatz.neu"] <- str_extract_all(action_matrix[i,"ref.absatz.neu"], "[:digit:]+")[[1]]
  }
  
  if (length(grep("und", action_matrix[i,"ref.absatz"])) == 1) {
    # Matrix erweitern
    n.new.rows <- lengths(str_extract_all(action_matrix[i,"ref.absatz"], "[:digit:]+"))
    if (i < nrow(action_matrix))
      action_matrix <- rbind(action_matrix[1:i-1,], matrix(rep(action_matrix[i,], n.new.rows), nrow=n.new.rows, ncol=ncol(action_matrix), byrow=T), action_matrix[(i+1):nrow(action_matrix),])
    if (i == nrow(action_matrix))
      action_matrix <- rbind(action_matrix[1:i-1,], matrix(rep(action_matrix[i,], n.new.rows), nrow=n.new.rows, ncol=ncol(action_matrix), byrow=T))
    
    # Matrix auffüllen
    action_matrix[i:(i+n.new.rows-1), "ref.absatz"] <- str_extract_all(action_matrix[i,"ref.absatz"], "[:digit:]+")[[1]]
    
    if (length(grep("und", action_matrix[i,"ref.absatz.neu"])) == 1)
      action_matrix[i:(i+n.new.rows-1), "ref.absatz.neu"] <- str_extract_all(action_matrix[i,"ref.absatz.neu"], "[:digit:]+")[[1]]
    if (length(grep("bis", action_matrix[i,"ref.absatz.neu"])) == 1)
      action_matrix[i:(i+n.new.rows-1), "ref.absatz.neu"] <- seq(as.numeric(str_extract_all(action_matrix[i,"ref.absatz.neu"], "[:digit:]+")[[1]][1]), as.numeric(str_extract_all(action_matrix[i,"ref.absatz.neu"], "[:digit:]+")[[1]][2]))
    
  }
  
}

action_matrix[,"ref.absatz"] <- str_replace_all(action_matrix[,"ref.absatz"], " ", "")  
action_matrix[,"ref.absatz.neu"] <- str_replace_all(action_matrix[,"ref.absatz.neu"], " ", "")  




# ref.satz & ref.satz.neu
action_matrix[,"ref.satz"] <- str_replace_all(action_matrix[,"ref.satz"], "Satz", "")
action_matrix[,"ref.satz"] <- str_replace_all(action_matrix[,"ref.satz"], "Sätze", "")
action_matrix[,"ref.satz"] <- str_replace_all(action_matrix[,"ref.satz"], ",", "")

action_matrix[,"ref.satz.neu"] <- str_replace_all(action_matrix[,"ref.satz.neu"], "Satz", "")
action_matrix[,"ref.satz.neu"] <- str_replace_all(action_matrix[,"ref.satz.neu"], "Sätze", "")
action_matrix[,"ref.satz.neu"] <- str_replace_all(action_matrix[,"ref.satz.neu"], ",", "")

for (i in 1: nrow(action_matrix)) {
  
  if (length(grep("bis", action_matrix[i,"ref.satz"])) == 1) {
    # Matrix erweitern
    n.new.rows <- as.numeric(strsplit(action_matrix[i,"ref.satz"], "bis")[[1]][2]) - as.numeric(strsplit(action_matrix[i,"ref.satz"], "bis")[[1]][1])
    if (i < nrow(action_matrix))
      action_matrix <- rbind(action_matrix[1:i,], matrix(rep(action_matrix[i,], n.new.rows), nrow=n.new.rows, ncol=ncol(action_matrix), byrow=T), action_matrix[(i+1):nrow(action_matrix),])
    if (i == nrow(action_matrix))
      action_matrix <- rbind(action_matrix[1:i,], matrix(rep(action_matrix[i,], n.new.rows), nrow=n.new.rows, ncol=ncol(action_matrix), byrow=T))
    
    # Matrix auffüllen
    action_matrix[i:(i+n.new.rows), "ref.satz"] <- seq(as.numeric(str_extract_all(action_matrix[i,"ref.satz"], "[:digit:]+")[[1]][1]), as.numeric(str_extract_all(action_matrix[i,"ref.satz"], "[:digit:]+")[[1]][2]))
    
    if (length(grep("bis", action_matrix[i,"ref.satz.neu"])) == 1)
      action_matrix[i:(i+n.new.rows), "ref.satz.neu"] <- seq(as.numeric(str_extract_all(action_matrix[i,"ref.satz.neu"], "[:digit:]+")[[1]][1]), as.numeric(str_extract_all(action_matrix[i,"ref.satz.neu"], "[:digit:]+")[[1]][2]))
    if (length(grep("und", action_matrix[i,"ref.satz.neu"])) == 1)
      action_matrix[i:(i+n.new.rows), "ref.satz.neu"] <- str_extract_all(action_matrix[i,"ref.satz.neu"], "[:digit:]+")[[1]]
  }
  
  if (length(grep("und", action_matrix[i,"ref.satz"])) == 1) {
    # Matrix erweitern
    n.new.rows <- lengths(str_extract_all(action_matrix[i,"ref.satz"], "[:digit:]+"))
    if (i < nrow(action_matrix))
      action_matrix <- rbind(action_matrix[1:i-1,], matrix(rep(action_matrix[i,], n.new.rows), nrow=n.new.rows, ncol=ncol(action_matrix), byrow=T), action_matrix[(i+1):nrow(action_matrix),])
    if (i == nrow(action_matrix))
      action_matrix <- rbind(action_matrix[1:i-1,], matrix(rep(action_matrix[i,], n.new.rows), nrow=n.new.rows, ncol=ncol(action_matrix), byrow=T))
    
    # Matrix auffüllen
    action_matrix[i:(i+n.new.rows-1), "ref.satz"] <- str_extract_all(action_matrix[i,"ref.satz"], "[:digit:]+")[[1]]
    
    if (length(grep("und", action_matrix[i,"ref.satz.neu"])) == 1)
      action_matrix[i:(i+n.new.rows-1), "ref.satz.neu"] <- str_extract_all(action_matrix[i,"ref.satz.neu"], "[:digit:]+")[[1]]
    if (length(grep("bis", action_matrix[i,"ref.satz.neu"])) == 1)
      action_matrix[i:(i+n.new.rows-1), "ref.satz.neu"] <- seq(as.numeric(str_extract_all(action_matrix[i,"ref.satz.neu"], "[:digit:]+")[[1]][1]), as.numeric(str_extract_all(action_matrix[i,"ref.satz.neu"], "[:digit:]+")[[1]][2]))
    
  }
  
}

action_matrix[,"ref.satz"] <- str_replace_all(action_matrix[,"ref.satz"], " ", "")  
action_matrix[,"ref.satz.neu"] <- str_replace_all(action_matrix[,"ref.satz.neu"], " ", "")  




# --------------------------------------------------------------------
# Korrektur/Besonderheit
# Was wenn einmalig zitierter Text über mehrere Sinnabschnitte geht 
# Z.B. (Artikel 1-3 werden wie folgt neugefasst:"...")  
# --------------------------------------------------------------------

dupl <- which(duplicated(action_matrix[,"text.neu"]))
dupl <- c(dupl[which(is.element(dupl, which(is.na(action_matrix[,"text.neu"])))==F)][1]-1, dupl[which(is.element(dupl, which(is.na(action_matrix[,"text.neu"])))==F)])
idx <- 0

for (i in dupl){
  
  idx <- idx + 1
  
  sinnab <- max(which(c(is.na(action_matrix[i,"ref.art"]), is.na(action_matrix[i,"ref.num"]), is.na(action_matrix[i,"ref.buch"]), 
                        is.na(action_matrix[i,"ref.buchbuch"]), is.na(action_matrix[i,"ref.para"]), is.na(action_matrix[i,"ref.absatz"]), is.na(action_matrix[i,"ref.satz"])) == F))
  
  # Für Artikel
  if (sinnab == 1) {
    
    if (str_detect(action_matrix[i, "text.neu"], "^Artikel") == T) {
      
      if (idx < length(dupl)){
        new_text <- as.character(str_c(str_c("Artikel ", str_extract(action_matrix[i, "ref.art"], "[:digit:]+"), " "),
                                       rm_between(action_matrix[i, "text.neu"], str_c("Artikel ", str_extract(action_matrix[i, "ref.art"], "[:digit:]+")), 
                                                  str_c("Artikel ", as.numeric(str_extract(action_matrix[i, "ref.art"], "[:digit:]+"))+1), extract=T)))
        levels(action_matrix[,"text.neu"]) <- c(levels(action_matrix[i,"text.neu"]), new_text)
        action_matrix[i,"text.neu"] <- new_text
      }
      
      if (idx == length(dupl)){
        new_text <- as.character(str_c(str_c("Artikel ", str_extract(action_matrix[i, "ref.art"], "[:digit:]+")), " ", 
                                       rm_between(action_matrix[i, "text.neu"], str_c("Artikel ", str_extract(action_matrix[i, "ref.art"], "[:digit:]+")), word(action_matrix[i, "text.neu"],-1), extract=T),
                                       " ", word(action_matrix[i, "text.neu"],-1)))
        levels(action_matrix[,"text.neu"]) <- c(levels(action_matrix[i,"text.neu"]), new_text)
        action_matrix[i,"text.neu"] <- new_text
      } 
    }
  }
  
  # Für Nummer
  if (sinnab == 2) {
    
    if (str_detect(action_matrix[i, "text.neu"], "^[:digit:]+") == T) {
      
      if (idx < length(dupl)){
        new_text <- as.character(str_c(str_extract(action_matrix[i, "ref.num"], "[:digit:]+"), ". ",
                                       rm_between(action_matrix[i, "text.neu"], str_c(str_extract(action_matrix[i, "ref.num"], "[:digit:]+"), ". "), 
                                                  str_c(as.numeric(str_extract(action_matrix[i, "ref.num"], "[:digit:]+"))+1, ". "), extract=T)))
        levels(action_matrix[,"text.neu"]) <- c(levels(action_matrix[i,"text.neu"]), new_text)
        action_matrix[i,"text.neu"] <- new_text
      }
      
      if (idx == length(dupl)){
        new_text <- as.character(str_c(str_extract(action_matrix[i, "ref.num"], "[:digit:]+"), ". ",
                                       rm_between(action_matrix[i, "text.neu"], str_c(str_extract(action_matrix[i, "ref.num"], "[:digit:]+"), ". "), 
                                                  word(action_matrix[i, "text.neu"],-1), extract=T), " ", word(action_matrix[i, "text.neu"],-1)))
        levels(action_matrix[,"text.neu"]) <- c(levels(action_matrix[i,"text.neu"]), new_text)
        action_matrix[i,"text.neu"] <- new_text
      } 
    }
  }
  
  # Für Buchstabe
  if (sinnab == 3) {
    
    if (str_detect(action_matrix[i, "text.neu"], "^[:lower:][)]") == T) {
      
      if (idx < length(dupl)){
        new_text <- as.character(str_c(str_extract(action_matrix[i, "ref.buch"], str_sub(action_matrix[i, "ref.buch"], -1, -1)), ") ",
                                       rm_between(action_matrix[i, "text.neu"], str_c(str_extract(action_matrix[i, "ref.buch"], str_sub(action_matrix[i, "ref.buch"], -1, -1)), ") "), 
                                                  str_c(as.character(str_extract(action_matrix[i+1, "ref.buch"], str_sub(action_matrix[i+1, "ref.buch"], -1, -1))), ") "), extract=T)))
        levels(action_matrix[,"text.neu"]) <- c(levels(action_matrix[i,"text.neu"]), new_text)
        action_matrix[i,"text.neu"] <- new_text
      }
      
      if (idx == length(dupl)){
        new_text <- as.character(str_c(str_extract(action_matrix[i, "ref.buch"], str_sub(action_matrix[i, "ref.buch"], -1, -1)), ") ",
                                       rm_between(action_matrix[i, "text.neu"], str_c(str_extract(action_matrix[i, "ref.buch"], str_sub(action_matrix[i, "ref.buch"], -1, -1)), ") "), 
                                                  word(action_matrix[i, "text.neu"],-1), extract=T), " ", word(action_matrix[i, "text.neu"],-1)))
        levels(action_matrix[,"text.neu"]) <- c(levels(action_matrix[i,"text.neu"]), new_text)
        action_matrix[i,"text.neu"] <- new_text
      } 
    }
  }
  
  # Für Buchstabe/Buchstabe
  if (sinnab == 4) {
    
    if (str_detect(action_matrix[i, "text.neu"], "^[:lower:][:lower:][)]") == T) {
      
      if (idx < length(dupl)){
        new_text <- as.character(str_c(str_extract(action_matrix[i, "ref.buchbuch"], str_sub(action_matrix[i, "ref.buchbuch"], -1, -1)), str_extract(action_matrix[i, "ref.buchbuch"], str_sub(action_matrix[i, "ref.buchbuch"], -1, -1)), ") ",
                                       rm_between(action_matrix[i, "text.neu"], str_c(str_extract(action_matrix[i, "ref.buchbuch"], str_sub(action_matrix[i, "ref.buchbuch"], -1, -1)), str_extract(action_matrix[i, "ref.buchbuch"], str_sub(action_matrix[i, "ref.buchbuch"], -1, -1)), ") "), 
                                                  str_c(as.character(str_extract(action_matrix[i+1, "ref.buchbuch"], str_sub(action_matrix[i+1, "ref.buchbuch"], -1, -1))), str_extract(action_matrix[i+1, "ref.buchbuch"], str_sub(action_matrix[i+1, "ref.buchbuch"], -1, -1)),  ") "), extract=T)))
        levels(action_matrix[,"text.neu"]) <- c(levels(action_matrix[i,"text.neu"]), new_text)
        action_matrix[i,"text.neu"] <- new_text
      }
      
      if (idx == length(dupl)){
        new_text <- as.character(str_c(str_extract(action_matrix[i, "ref.buchbuch"], str_sub(action_matrix[i, "ref.buchbuch"], -1, -1)), str_extract(action_matrix[i, "ref.buchbuch"], str_sub(action_matrix[i, "ref.buchbuch"], -1, -1)), ") ",
                                       rm_between(action_matrix[i, "text.neu"], str_c(str_extract(action_matrix[i, "ref.buchbuch"], str_sub(action_matrix[i, "ref.buchbuch"], -1, -1)), str_extract(action_matrix[i, "ref.buchbuch"], str_sub(action_matrix[i, "ref.buchbuch"], -1, -1)),  ") "), 
                                                  word(action_matrix[i, "text.neu"],-1), extract=T), " ", word(action_matrix[i, "text.neu"],-1)))
        levels(action_matrix[,"text.neu"]) <- c(levels(action_matrix[i,"text.neu"]), new_text)
        action_matrix[i,"text.neu"] <- new_text
      } 
    }
  }
  
  
  # Für Paragraph
  if (sinnab == 5) {
    
    if (str_detect(action_matrix[i, "text.neu"], "^§") == T) {
      
      if (idx < length(dupl)) {
        first_par <- str_extract(action_matrix[i, "text.neu"], str_c("([§] |[§])", str_extract(action_matrix[i, "ref.para"], "[:digit:]+")))
        second_par <- str_extract(action_matrix[i, "text.neu"], str_c("([§] |[§])", str_extract(action_matrix[i+1, "ref.para"], "[:digit:]+")))
        
        new_text <- as.character(str_c(str_c("§ ", str_extract(action_matrix[i, "ref.para"], "[:digit:]+"), " "),
                                       rm_between(action_matrix[i, "text.neu"], first_par, second_par, extract=T, fixed=F)))
        levels(action_matrix[,"text.neu"]) <- c(levels(action_matrix[i,"text.neu"]), new_text)
        action_matrix[i,"text.neu"] <- new_text
      }
      
      if (idx == length(dupl)) {
        first_par <- str_extract(action_matrix[i, "text.neu"], str_c("([§] |[§])", str_extract(action_matrix[i, "ref.para"], "[:digit:]+")))
        
        new_text <- as.character(str_c(str_c("§", str_extract(action_matrix[i, "ref.para"], "[:digit:]+"), " "),
                                       rm_between(action_matrix[i, "text.neu"], first_par, 
                                                  word(action_matrix[i, "text.neu"],-1), extract=T, fixed=F), " ", word(action_matrix[i, "text.neu"],-1)))
        levels(action_matrix[,"text.neu"]) <- c(levels(action_matrix[i,"text.neu"]), new_text)
        action_matrix[i,"text.neu"] <- new_text
      }
    }
  }
  
  
  
  # Für Absatz
  if (sinnab == 6) {
    
    if (str_detect(action_matrix[i, "text.neu"], "^[(][:digit:]+[)]") == T) {
      
      if (idx < length(dupl)) {
        new_text <- as.character(str_c(str_c("(", str_extract(action_matrix[i, "ref.absatz"], "[:digit:]+"), ")", " "),
                                       rm_between(action_matrix[i, "text.neu"], str_c("(", str_extract(action_matrix[i, "ref.absatz"], "[:digit:]+"), ")"), 
                                                  str_c("(", as.numeric(str_extract(action_matrix[i, "ref.absatz"], "[:digit:]+"))+1, ")"), extract=T)))
        levels(action_matrix[,"text.neu"]) <- c(levels(action_matrix[i,"text.neu"]), new_text)
        action_matrix[i,"text.neu"] <- new_text
      }
      
      if (idx == length(dupl)) {
        new_text <- as.character(str_c(str_c("(", str_extract(action_matrix[i, "ref.absatz"], "[:digit:]+"), ")", " "),
                                       rm_between(action_matrix[i, "text.neu"], str_c("(", str_extract(action_matrix[i, "ref.absatz"], "[:digit:]+"), ")"), 
                                                  word(action_matrix[i, "text.neu"],-1), extract=T), " ", word(action_matrix[i, "text.neu"],-1)))
        levels(action_matrix[,"text.neu"]) <- c(levels(action_matrix[i,"text.neu"]), new_text)
        action_matrix[i,"text.neu"] <- new_text
      }
    }
  }
}


# --------------------------------------------------------------------------
# action_matrix auf die Zeilen reduzieren, die Funktionen auslösen sollen
# --------------------------------------------------------------------------
for (i in 1:nrow(action_matrix)){
  
  if (length(which(is.na(action_matrix[i,2:6]) == F)) == 0)
    action_matrix[i,] <- NA
  
  if (is.na(action_matrix[i, "ref.fkt"]) == T | action_matrix[i, "ref.fkt"] == "geändert")
    action_matrix[i,] <- NA
}


if (is.null(ncol(action_matrix)) == F) {
  
  if (nrow(action_matrix) > 1) {
    for (i in 2:nrow(action_matrix)){
      if (is.na(action_matrix[i, "neunummerieren"]) == F & is.na(action_matrix[i-1, "einfuegen"]) == F)
        action_matrix[i,] <- NA
    }
  }
  
  if (length(which(rowSums(is.na(action_matrix)) == ncol(action_matrix)) > 0))
    action_matrix <- action_matrix[-which(rowSums(is.na(action_matrix)) == ncol(action_matrix)), ]
}

if (is.null(ncol(action_matrix)) == F) {
  # repeat 3 times
  if (nrow(action_matrix) > 1) {
    for (i in 2:nrow(action_matrix)){
      if (is.na(action_matrix[i, "neunummerieren"]) == F & is.na(action_matrix[i-1, "einfuegen"]) == F)
        action_matrix[i,] <- NA
    }
  }
  
  if (length(which(rowSums(is.na(action_matrix)) == ncol(action_matrix)) > 0))
    action_matrix <- action_matrix[-which(rowSums(is.na(action_matrix)) == ncol(action_matrix)), ]
}

if (is.null(ncol(action_matrix)) == F) {
  
  if (nrow(action_matrix) > 1) {
    for (i in 2:nrow(action_matrix)){
      if (is.na(action_matrix[i, "neunummerieren"]) == F & is.na(action_matrix[i-1, "einfuegen"]) == F)
        action_matrix[i,] <- NA
    }
  }
  
  if (length(which(rowSums(is.na(action_matrix)) == ncol(action_matrix)) > 0))
    action_matrix <- action_matrix[-which(rowSums(is.na(action_matrix)) == ncol(action_matrix)), ]
}

if (is.null(ncol(action_matrix)) == F) {
  if (nrow(action_matrix) > 1) {
    for (i in 2:nrow(action_matrix)){
      if (is.na(action_matrix[i, "neunummerieren"]) == F & is.na(action_matrix[i-1, "einfuegen"]) == F)
        action_matrix[i,] <- NA
    }
  }
  
  if (length(which(rowSums(is.na(action_matrix)) == ncol(action_matrix)) > 0))
    action_matrix <- action_matrix[-which(rowSums(is.na(action_matrix)) == ncol(action_matrix)), ]
}

if (is.null(ncol(action_matrix))) {
  action_matrixa <- matrix(NA, nrow=1, ncol=1+18+5+1)
  colnames(action_matrixa) <- c("vorschlag.idx", "ref.art", "ref.num", "ref.buch", "ref.buchbuch", "ref.para", "ref.absatz", "ref.satz", "spec.satz", "spec.wort", "ref.art.neu", "ref.num.neu", "ref.buch.neu", "ref.buchbuch.neu", "ref.para.neu", "ref.absatz.neu", "ref.satz.neu", "text.neu", "e.list", "loeschen", "neunummerieren", "neufassen", "hinzufuegen", "einfuegen", "ref.fkt")
  action_matrixa[1,] <- action_matrix
  action_matrix <- action_matrixa
}


# ------------------------------------------------------------------
# In dataframe umwandeln um verschiedene variable types zu fassen  
# ------------------------------------------------------------------
action_matrix <- as.data.frame(action_matrix)
action_matrix[,"vorschlag.idx"] <- as.numeric(as.character(action_matrix[,"vorschlag.idx"]))  
action_matrix[,"ref.para"] <- as.numeric(as.character(action_matrix[,"ref.para"]))  
action_matrix[,"ref.absatz"] <- as.numeric(as.character(action_matrix[,"ref.absatz"]))  
action_matrix[,"ref.satz"] <- as.numeric(as.character(action_matrix[,"ref.satz"]))
action_matrix[,"ref.para.neu"] <- as.numeric(as.character(action_matrix[,"ref.para.neu"]))
action_matrix[,"ref.absatz.neu"] <- as.numeric(as.character(action_matrix[,"ref.absatz.neu"]))
action_matrix[,"ref.satz.neu"] <- as.numeric(as.character(action_matrix[,"ref.satz.neu"]))


amendmentprop$action_matrix[amendmentprop_row] <- list(action_matrix)

if (nrow(action_matrix) == 0)
  amendmentprop$action_matrix[amendmentprop_row] <- "none"

print(amendmentprop_row)

}, error = function(e){
  
  amendmentprop$manual[amendmentprop_row] <<- 1
  
}) # end tryCatch

}



