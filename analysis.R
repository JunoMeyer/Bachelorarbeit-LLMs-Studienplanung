############################################################
# Bachelorarbeit – Analyse der LLM-Outputs
# Autorin: Juno Meyer
# Datum: 10.08.2026
#
# Zweck des Skripts:
  # Dieses Skript berechnet deskriptive Statistiken sowie absolute
  # und relative Häufigkeiten für die in der Bachelorarbeit berichteten
  # Tabellen. Analysiert werden Ratings zu drei Studienplanungsaspekten:
  #   1. Studiendesign
  #   2. Messinstrumente
  #   3. Kontrollvariablen
  #
  # Pro Aspekt liegen zwei Bewertungsdimensionen vor:
  #   - Angemessenheit
  #   - Entscheidungshilfe
  #
  # Zusätzlich werden Auswertungen getrennt nach Promptbedingungen
  # durchgeführt:
  #   - Reflexionsanweisung: yes / no
  #   - Hypothesenbedingung: yes / no
#
# Hinweise zur Reproduzierbarkeit:
# - Das Skript verwendet relative Pfade.
# - Es sollten keine personenbezogenen lokalen Pfade verwendet werden.
# - Paketversionen werden am Ende über sessionInfo() dokumentiert.
# - Rohdaten sollten im Ordner "data/" liegen.
# - Ergebnisse werden im Ordner "results/" gespeichert.
############################################################

############################################################
# 0. Pakete laden
############################################################

# Pakete sollten vorab installiert sein.
# install.packages(c("readxl", "dplyr", "tidyr", "tibble"))

library(readxl)
library(dplyr)
library(tidyr)
library(tibble)

# Das Paket psych wurde im ursprünglichen Skript geladen,
# wird in den folgenden Analysen aber nicht benötigt.
# Falls später psych-Funktionen ergänzt werden, kann es wieder aktiviert werden.
# library(psych)


############################################################
# 1. Daten einlesen
############################################################

# Dateiname der Excel-Datei
data_file <- "raw_llm_outputs_and_ratings.xlsx"

# Bevorzugter relativer Pfad für Open-Science-Reproduzierbarkeit
data_path <- file.path("data", data_file)

# Falls die Datei nicht im Unterordner "data" liegt,
# wird alternativ im aktuellen Arbeitsverzeichnis gesucht.
if (!file.exists(data_path)) {
  data_path <- data_file
}

# Falls die Datei an keinem der beiden Orte gefunden wird,
# wird eine verständliche Fehlermeldung ausgegeben.
if (!file.exists(data_path)) {
  stop(
    "Die Datendatei wurde nicht gefunden. Bitte die Excel-Datei entweder ",
    "im Unterordner 'data/' oder im aktuellen Arbeitsverzeichnis ablegen."
  )
}

# Daten einlesen
d <- read_excel(data_path)

# Datenstruktur prüfen
str(d)


############################################################
# 2. Zentrale Variablendefinitionen
############################################################

# Rating-Variablen:
# Diese sechs Spalten enthalten die Kodierungen der LLM-Outputs.
rating_spalten <- c(
  "Krit_Studiendesign_rating",
  "Studiendesign_rating",
  "Krit_Messinstr_rating",
  "Messinstrumente_rating",
  "Krit_Kontrollvar_rating",
  "Kontrollvariablen_rating"
)

# Prüfung, ob alle benötigten Spalten im Datensatz vorhanden sind.
benoetigte_spalten <- c("type", "hypo", rating_spalten)

fehlende_spalten <- setdiff(benoetigte_spalten, names(d))

if (length(fehlende_spalten) > 0) {
  stop(
    "Folgende benötigte Spalten fehlen im Datensatz: ",
    paste(fehlende_spalten, collapse = ", ")
  )
}

# Zuordnung der Rating-Variablen zu Studienplanungsaspekt
# und Bewertungsdimension.
#
# Diese Tabelle wird mehrfach verwendet, damit die Zuordnung an
# einer zentralen Stelle dokumentiert ist.
rating_info <- tribble(
  ~rating_variable,               ~Aspekt,              ~Bewertungsdimension,
  "Studiendesign_rating",         "Studiendesign",      "Angemessenheit",
  "Krit_Studiendesign_rating",    "Studiendesign",      "Entscheidungshilfe",
  "Messinstrumente_rating",       "Messinstrumente",    "Angemessenheit",
  "Krit_Messinstr_rating",        "Messinstrumente",    "Entscheidungshilfe",
  "Kontrollvariablen_rating",     "Kontrollvariablen",  "Angemessenheit",
  "Krit_Kontrollvar_rating",      "Kontrollvariablen",  "Entscheidungshilfe"
)

# Dieselbe Information mit dem ursprünglichen Spaltennamen "Variable".
# Diese Variante wird für die erste Häufigkeitstabelle genutzt.
var_info <- rating_info %>%
  rename(
    Variable = rating_variable,
    Dimension = Bewertungsdimension
  )

# Zuordnung der Variablen zu den drei Studienplanungsaspekten.
# Pro Aspekt werden jeweils zwei Bewertungsdimensionen zusammengefasst.
aspekte <- list(
  Studiendesign = c(
    "Krit_Studiendesign_rating",
    "Studiendesign_rating"
  ),
  Messinstrumente = c(
    "Krit_Messinstr_rating",
    "Messinstrumente_rating"
  ),
  Kontrollvariablen = c(
    "Krit_Kontrollvar_rating",
    "Kontrollvariablen_rating"
  )
)

# Kodierungen im Datensatz.
# Erwartet werden inhaltlich die Werte 1, 2 und 3.
kodierungen <- c(1, 2, 3)

# Bezeichnungen der Promptbedingungen.
ohne_refl <- "no"
mit_refl  <- "yes"

ohne_hypo <- "no"
mit_hypo  <- "yes"


############################################################
# 3. Hilfsfunktionen
############################################################

# Funktion zur Berechnung zentraler deskriptiver Kennwerte
# für einen numerischen Vektor.
deskriptiv <- function(x) {
  c(
    Mittelwert = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Standardabweichung = sd(x, na.rm = TRUE),
    Minimum = min(x, na.rm = TRUE),
    Maximum = max(x, na.rm = TRUE),
    N = sum(!is.na(x))
  )
}

# Funktion zur Berechnung eines Mittelwertscores über mehrere Rating-Spalten. # GGF. Prüfen
#
# Falls eine Zeile ausschließlich fehlende Werte enthält, erzeugt rowMeans()
# den Wert NaN. Dieser wird anschließend in NA umgewandelt.
berechne_aspekt_score <- function(data, spalten) {
  score <- rowMeans(data[, spalten], na.rm = TRUE)
  score[is.nan(score)] <- NA
  return(score)
}

# Funktion zur Berechnung gruppierter deskriptiver Statistiken.
#
# Sie entspricht inhaltlich den wiederholten aggregate()-Blöcken
# des ursprünglichen Skripts.
deskriptiv_nach_gruppe <- function(data, gruppenvariable, rating_vector, gruppenname) {
  
  tmp <- data.frame(
    gruppe = data[[gruppenvariable]],
    rating = rating_vector
  )
  
  out <- aggregate(
    rating ~ gruppe,
    data = tmp,
    FUN = function(x) c(
      mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE),
      N = sum(!is.na(x))
    )
  )
  
  out <- do.call(data.frame, out)
  
  names(out) <- c(
    gruppenname,
    "mean",
    "sd",
    "median",
    "min",
    "max",
    "N"
  )
  
  return(out)
}

# Funktion zum Ergänzen der Mittelwertdifferenz:
# Differenz = Mittelwert der Bedingung "yes" minus Mittelwert der Bedingung "no".
ergaenze_mw_diff <- function(tab, gruppenvariable, mit, ohne) {
  
  mw_diff <- tab$mean[tab[[gruppenvariable]] == mit] -
    tab$mean[tab[[gruppenvariable]] == ohne]
  
  tab$MW_Diff_mit_minus_ohne <- mw_diff
  
  return(tab)
}

# Funktion zur Erstellung einer Häufigkeitstabelle im Wide-Format.
#
# Für jede Gruppe werden die Kodierungen über mehrere Rating-Spalten hinweg
# zusammengeführt und anschließend absolute sowie relative Häufigkeiten
# berechnet.
haeufigkeiten_wide <- function(data, gruppenvariable, rating_spalten, werte = c(1, 2, 3)) {
  
  gruppen <- unique(data[[gruppenvariable]][!is.na(data[[gruppenvariable]])])
  
  tab <- do.call(rbind, lapply(gruppen, function(gruppe) {
    
    # Zeilen der jeweiligen Gruppe auswählen
    zeilen <- !is.na(data[[gruppenvariable]]) &
      data[[gruppenvariable]] == gruppe
    
    # Alle Bewertungsdimensionen der Gruppe zusammenführen
    x <- unlist(data[zeilen, rating_spalten], use.names = FALSE)
    
    # Fehlende Werte entfernen
    x <- x[!is.na(x)]
    
    # Absolute Häufigkeiten für die definierten Kodierungen
    abs_h <- table(factor(x, levels = werte))
    
    # Prozentwerte innerhalb der Gruppe
    proz <- prop.table(abs_h) * 100
    
    # Darstellung: absolute Häufigkeit und Prozentwert
    werte_formatiert <- sprintf(
      "%d (%.1f%%)",
      as.numeric(abs_h),
      as.numeric(proz)
    )
    
    names(werte_formatiert) <- paste0("Kodierung_", werte)
    
    data.frame(
      Gruppe = gruppe,
      N_Einzelbewertungen = length(x),
      t(werte_formatiert),
      check.names = FALSE
    )
  }))
  
  rownames(tab) <- NULL
  
  return(tab)
}


############################################################
# 4. Häufigkeiten der Kodierungen nach Aspekt und Dimension
############################################################
# Übersicht der Verteilung der Kodierungen pro Bewertungsdimension.
############################################################

# Alle im Datensatz vorkommenden Rating-Ausprägungen bestimmen.
rating_levels <- d %>%
  select(all_of(var_info$Variable)) %>%
  pivot_longer(everything(), values_to = "Rating") %>%
  filter(!is.na(Rating)) %>%
  mutate(Rating = as.character(Rating)) %>%
  distinct(Rating) %>%
  arrange(Rating) %>%
  pull(Rating)

# Häufigkeiten im Wide-Format berechnen.
tab_wide <- d %>%
  select(all_of(var_info$Variable)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Rating"
  ) %>%
  left_join(var_info, by = "Variable") %>%
  filter(!is.na(Rating)) %>%
  mutate(
    Rating = as.character(Rating),
    Aspekt = factor(
      Aspekt,
      levels = c("Studiendesign", "Messinstrumente", "Kontrollvariablen")
    ),
    Dimension = factor(
      Dimension,
      levels = c("Angemessenheit", "Entscheidungshilfe")
    )
  ) %>%
  count(Aspekt, Dimension, Rating, name = "n") %>%
  group_by(Aspekt, Dimension) %>%
  complete(Rating = rating_levels, fill = list(n = 0)) %>%
  mutate(
    N = sum(n),
    rel = n / N,
    Wert = sprintf("%d (%.1f%%)", n, rel * 100)
  ) %>%
  ungroup() %>%
  select(Aspekt, Dimension, N, Rating, Wert) %>%
  pivot_wider(
    names_from = Rating,
    values_from = Wert,
    names_prefix = "Rating_"
  ) %>%
  arrange(Aspekt, Dimension)

tab_wide # Vergleich Tabelle 1 im Bericht


############################################################
# 5. Frage 1:
# Deskriptive Kennwerte der drei Studienplanungsaspekte
############################################################

# Für jeden Studienplanungsaspekt wird ein Gesamtscore berechnet.
# Dieser ist der Mittelwert der beiden zugehörigen Bewertungsdimensionen.

Studiendesign_Gesamt <- berechne_aspekt_score(
  data = d,
  spalten = aspekte$Studiendesign
)

Messinstrumente_Gesamt <- berechne_aspekt_score(
  data = d,
  spalten = aspekte$Messinstrumente
)

Kontrollvariablen_Gesamt <- berechne_aspekt_score(
  data = d,
  spalten = aspekte$Kontrollvariablen
)

# Deskriptive Kennwerte der drei Aspekt-Gesamtscores berechnen.
Deskriptiv_Gesamt <- rbind(
  Studiendesign = deskriptiv(Studiendesign_Gesamt),
  Messinstrumente = deskriptiv(Messinstrumente_Gesamt),
  Kontrollvariablen = deskriptiv(Kontrollvariablen_Gesamt)
)

Deskriptiv_Gesamt <- as.data.frame(Deskriptiv_Gesamt)

Deskriptiv_Gesamt # entspricht Tabelle 2 im Bericht


############################################################
# 6. Frage 1:
# Absolute und relative Häufigkeiten nach Aspekt
############################################################
# Hier werden die beiden Bewertungsdimensionen pro Aspekt zusammengeführt.
############################################################

# Alle vorkommenden Rating-Werte bestimmen.
alle_werte <- sort(unique(na.omit(unlist(d[unlist(aspekte)], use.names = FALSE))))

# Wide-Tabelle erstellen.
Haeufigkeiten_Aspekte_Wide <- do.call(rbind, lapply(names(aspekte), function(aspekt) {
  
  # Alle Ratings des jeweiligen Aspekts zusammenführen.
  x <- unlist(d[aspekte[[aspekt]]], use.names = FALSE)
  x <- x[!is.na(x)]
  
  # Absolute Häufigkeiten
  abs_h <- table(factor(x, levels = alle_werte))
  
  # Relative Häufigkeiten in Prozent
  proz <- prop.table(abs_h) * 100
  
  # Formatierung: "n (Prozent%)"
  werte_formatiert <- sprintf(
    "%d (%.1f%%)",
    as.numeric(abs_h),
    as.numeric(proz)
  )
  
  names(werte_formatiert) <- paste0("Wert_", alle_werte)
  
  data.frame(
    Aspekt = aspekt,
    t(werte_formatiert),
    check.names = FALSE
  )
}))

rownames(Haeufigkeiten_Aspekte_Wide) <- NULL


Haeufigkeiten_Aspekte_Wide # entspricht den Gesamtwerten pro Aspekt in Tabelle 1 


############################################################
# 7. Frage 2a:
# Gesamtqualität abhängig von Promptbedingungen
############################################################
# Gesamtqualität = Mittelwert über alle sechs Rating-Spalten.
############################################################

Gesamtqualitaet <- rowMeans(
  d[, rating_spalten],
  na.rm = FALSE
)

############################################################
# 7.1 Gesamtqualität nach Reflexionsanweisung
############################################################

Refl_Gesamt <- data.frame(
  type = d$type,
  rating = Gesamtqualitaet
)

Deskriptiv_Refl_Gesamt <- deskriptiv_nach_gruppe(
  data = d,
  gruppenvariable = "type",
  rating_vector = Gesamtqualitaet,
  gruppenname = "type"
)

Deskriptiv_Refl_Gesamt # entspricht Tabelle 3 im Bericht, oberer Teil


############################################################
# 7.2 Gesamtqualität nach Hypothesenbedingung
############################################################

Hypo_Gesamt <- data.frame(
  type = d$hypo,
  rating = Gesamtqualitaet
)

Deskriptiv_Hypo_Gesamt <- deskriptiv_nach_gruppe(
  data = d,
  gruppenvariable = "hypo",
  rating_vector = Gesamtqualitaet,
  gruppenname = "type"
)

# Abgleich mit Tabelle 1, unterer Teil
Deskriptiv_Hypo_Gesamt


############################################################
# 8. Frage 2a:
# Häufigkeiten der Einzelbewertungen nach Promptbedingungen
############################################################
# Die sechs Bewertungsdimensionen werden zusammengeführt.
############################################################

# Häufigkeiten nach Reflexionsanweisung
Haeufigkeiten_Refl_Wide <- haeufigkeiten_wide(
  data = d,
  gruppenvariable = "type",
  rating_spalten = rating_spalten
)

Haeufigkeiten_Refl_Wide # entspricht Tabelle 4 im Bericht, oberer Teil

# Häufigkeiten nach Hypothesenbedingung
Haeufigkeiten_Hypo_Wide <- haeufigkeiten_wide(
  data = d,
  gruppenvariable = "hypo",
  rating_spalten = rating_spalten
)

Haeufigkeiten_Hypo_Wide # entspricht Tabelle 4 im Bericht, unterer Teil


############################################################
# 9. Frage 2b:
# Deskriptive Kennwerte nach Aspekt und Promptbedingung
############################################################
# Für jeden Studienplanungsaspekt werden separat Kennwerte nach
# Reflexionsanweisung und Hypothesenbedingung berechnet.
############################################################

# Ausprägungen der Promptbedingungen prüfen
unique(d$type)
unique(d$hypo)


############################################################
# 9.1 Studiendesign
############################################################

# Nach Reflexionsanweisung
Refl_Studiendesign <- data.frame(
  type = d$type,
  rating = Studiendesign_Gesamt
)

Deskriptiv_Refl_Studiendesign <- deskriptiv_nach_gruppe(
  data = d,
  gruppenvariable = "type",
  rating_vector = Studiendesign_Gesamt,
  gruppenname = "type"
)

Deskriptiv_Refl_Studiendesign <- ergaenze_mw_diff(
  tab = Deskriptiv_Refl_Studiendesign,
  gruppenvariable = "type",
  mit = mit_refl,
  ohne = ohne_refl
)

Deskriptiv_Refl_Studiendesign # ensptricht einem Teil der Tabelle 5 im Bericht

# Nach Hypothesenbedingung
Hypo_Studiendesign <- data.frame(
  hypo = d$hypo,
  rating = Studiendesign_Gesamt
)

Deskriptiv_Hypo_Studiendesign <- deskriptiv_nach_gruppe(
  data = d,
  gruppenvariable = "hypo",
  rating_vector = Studiendesign_Gesamt,
  gruppenname = "hypo"
)

Deskriptiv_Hypo_Studiendesign <- ergaenze_mw_diff(
  tab = Deskriptiv_Hypo_Studiendesign,
  gruppenvariable = "hypo",
  mit = mit_hypo,
  ohne = ohne_hypo
)

Deskriptiv_Hypo_Studiendesign # ensptricht einem Teil der Tabelle 5 im Bericht


############################################################
# 9.2 Messinstrumente
############################################################

# Nach Reflexionsanweisung
Refl_Messinstrumente <- data.frame(
  type = d$type,
  rating = Messinstrumente_Gesamt
)

Deskriptiv_Refl_Messinstrumente <- deskriptiv_nach_gruppe(
  data = d,
  gruppenvariable = "type",
  rating_vector = Messinstrumente_Gesamt,
  gruppenname = "type"
)

Deskriptiv_Refl_Messinstrumente <- ergaenze_mw_diff(
  tab = Deskriptiv_Refl_Messinstrumente,
  gruppenvariable = "type",
  mit = mit_refl,
  ohne = ohne_refl
)

Deskriptiv_Refl_Messinstrumente # ensptricht einem Teil der Tabelle 5 im Bericht

# Nach Hypothesenbedingung
Hypo_Messinstrumente <- data.frame(
  hypo = d$hypo,
  rating = Messinstrumente_Gesamt
)

Deskriptiv_Hypo_Messinstrumente <- deskriptiv_nach_gruppe(
  data = d,
  gruppenvariable = "hypo",
  rating_vector = Messinstrumente_Gesamt,
  gruppenname = "hypo"
)

Deskriptiv_Hypo_Messinstrumente <- ergaenze_mw_diff(
  tab = Deskriptiv_Hypo_Messinstrumente,
  gruppenvariable = "hypo",
  mit = mit_hypo,
  ohne = ohne_hypo
)

Deskriptiv_Hypo_Messinstrumente # ensptricht einem Teil der Tabelle 5 im Bericht


############################################################
# 9.3 Kontrollvariablen
############################################################

# Nach Reflexionsanweisung
Refl_Kontrollvariablen <- data.frame(
  type = d$type,
  rating = Kontrollvariablen_Gesamt
)

Deskriptiv_Refl_Kontrollvar <- deskriptiv_nach_gruppe(
  data = d,
  gruppenvariable = "type",
  rating_vector = Kontrollvariablen_Gesamt,
  gruppenname = "type"
)

Deskriptiv_Refl_Kontrollvar <- ergaenze_mw_diff(
  tab = Deskriptiv_Refl_Kontrollvar,
  gruppenvariable = "type",
  mit = mit_refl,
  ohne = ohne_refl
)

Deskriptiv_Refl_Kontrollvar # ensptricht einem Teil der Tabelle 5 im Bericht

# Nach Hypothesenbedingung
Hypo_Kontrollvariablen <- data.frame(
  hypo = d$hypo,
  rating = Kontrollvariablen_Gesamt
)

Deskriptiv_Hypo_Kontrollvar <- deskriptiv_nach_gruppe(
  data = d,
  gruppenvariable = "hypo",
  rating_vector = Kontrollvariablen_Gesamt,
  gruppenname = "hypo"
)

Deskriptiv_Hypo_Kontrollvar <- ergaenze_mw_diff(
  tab = Deskriptiv_Hypo_Kontrollvar,
  gruppenvariable = "hypo",
  mit = mit_hypo,
  ohne = ohne_hypo
)

Deskriptiv_Hypo_Kontrollvar # ensptricht einem Teil der Tabelle 5 im Bericht


############################################################
# 10. Frage 2b:
# Absolute und relative Häufigkeiten der Einzelbewertungen
# getrennt nach Aspekt und Promptbedingung
############################################################
# Abgleich mit Tabelle 6.
############################################################

############################################################
# 10.1 Daten ins Long-Format bringen
############################################################

d_long <- d %>%
  pivot_longer(
    cols = all_of(rating_spalten),
    names_to = "rating_variable",
    values_to = "rating"
  ) %>%
  mutate(
    Aspekt = case_when(
      rating_variable %in% c(
        "Krit_Studiendesign_rating",
        "Studiendesign_rating"
      ) ~ "Studiendesign",
      
      rating_variable %in% c(
        "Krit_Messinstr_rating",
        "Messinstrumente_rating"
      ) ~ "Messinstrumente",
      
      rating_variable %in% c(
        "Krit_Kontrollvar_rating",
        "Kontrollvariablen_rating"
      ) ~ "Kontrollvariablen"
    ),
    rating = as.numeric(rating)
  ) %>%
  filter(!is.na(rating))


############################################################
# 10.2 Promptbedingungen ins Long-Format bringen
############################################################

d_prompt_long <- d_long %>%
  pivot_longer(
    cols = c(type, hypo),
    names_to = "Promptvariable",
    values_to = "Bedingung"
  ) %>%
  mutate(
    Promptbedingung = case_when(
      Promptvariable == "type" ~ "Reflexionsanweisung",
      Promptvariable == "hypo" ~ "Hypothesen"
    ),
    Bedingung = as.character(Bedingung)
  ) %>%
  filter(!is.na(Bedingung))


############################################################
# 10.3 Häufigkeiten berechnen
############################################################

Haeufigkeiten_Aspekt_Prompt_Long <- d_prompt_long %>%
  count(
    Aspekt,
    Promptbedingung,
    Bedingung,
    rating,
    name = "n"
  ) %>%
  group_by(
    Aspekt,
    Promptbedingung,
    Bedingung
  ) %>%
  complete(
    rating = kodierungen,
    fill = list(n = 0)
  ) %>%
  ungroup() %>%
  group_by(
    Aspekt,
    Promptbedingung,
    Bedingung,
    rating
  ) %>%
  summarise(
    n = sum(n),
    .groups = "drop"
  ) %>%
  group_by(
    Aspekt,
    Promptbedingung,
    Bedingung
  ) %>%
  mutate(
    N_Einzelbewertungen = sum(n),
    Prozent = n / N_Einzelbewertungen * 100,
    Häufigkeit = sprintf("%d (%.1f%%)", n, Prozent)
  ) %>%
  ungroup()


############################################################
# 10.4 Wide-Tabelle erstellen
############################################################

Haeufigkeiten_Aspekt_Prompt_Wide <- Haeufigkeiten_Aspekt_Prompt_Long %>%
  mutate(
    Aspekt = factor(
      Aspekt,
      levels = c(
        "Studiendesign",
        "Messinstrumente",
        "Kontrollvariablen"
      )
    ),
    Promptbedingung = factor(
      Promptbedingung,
      levels = c(
        "Reflexionsanweisung",
        "Hypothesen"
      )
    ),
    Bedingung = factor(
      Bedingung,
      levels = c("yes", "no")
    ),
    rating = paste0("Kodierung_", rating)
  ) %>%
  select(
    Aspekt,
    Promptbedingung,
    Bedingung,
    N_Einzelbewertungen,
    rating,
    Häufigkeit
  ) %>%
  pivot_wider(
    names_from = rating,
    values_from = Häufigkeit
  ) %>%
  arrange(
    Aspekt,
    Promptbedingung,
    Bedingung
  )

# Abgleich mit Tabelle 6
Haeufigkeiten_Aspekt_Prompt_Wide


############################################################
# 11. Explorative Analyse:
# Deskriptive Kennwerte nach Promptbedingung,
# Studienplanungsaspekt und Bewertungsdimension
############################################################
# Abgleich mit Tabelle 8.
############################################################

############################################################
# 11.1 Daten ins Long-Format bringen
############################################################

d_long_vergleich <- d %>%
  mutate(Output_ID = row_number()) %>%
  pivot_longer(
    cols = all_of(rating_info$rating_variable),
    names_to = "rating_variable",
    values_to = "Rating"
  ) %>%
  left_join(rating_info, by = "rating_variable") %>%
  pivot_longer(
    cols = c(type, hypo),
    names_to = "Promptvariable",
    values_to = "Bedingung"
  ) %>%
  mutate(
    Rating = as.numeric(Rating),
    
    Promptbedingung = case_when(
      Promptvariable == "type" ~ "Reflexionsanweisung",
      Promptvariable == "hypo" ~ "Hypothesen"
    ),
    
    # Vereinheitlichung der Bedingungsbezeichnungen:
    # yes/no werden hier für die tabellarische Darstellung in ja/nein
    # umkodiert.
    Bedingung = case_when(
      tolower(as.character(Bedingung)) %in% c("yes", "ja", "1", "true") ~ "ja",
      tolower(as.character(Bedingung)) %in% c("no", "nein", "0", "false") ~ "nein",
      TRUE ~ as.character(Bedingung)
    ),
    
    # Festlegen der Reihenfolge für spätere Tabellen.
    Aspekt = factor(
      Aspekt,
      levels = c(
        "Studiendesign",
        "Messinstrumente",
        "Kontrollvariablen"
      )
    ),
    
    Bewertungsdimension = factor(
      Bewertungsdimension,
      levels = c(
        "Angemessenheit",
        "Entscheidungshilfe"
      )
    ),
    
    Promptbedingung = factor(
      Promptbedingung,
      levels = c(
        "Reflexionsanweisung",
        "Hypothesen"
      )
    ),
    
    Bedingung = factor(
      Bedingung,
      levels = c("ja", "nein")
    )
  ) %>%
  filter(!is.na(Rating), !is.na(Bedingung))


############################################################
# 11.2 Deskriptive Kennwerte berechnen
############################################################

tabelle_vergleich <- d_long_vergleich %>%
  group_by(
    Aspekt,
    Bewertungsdimension,
    Promptbedingung,
    Bedingung
  ) %>%
  summarise(
    N = n(),
    M = mean(Rating, na.rm = TRUE),
    Md = median(Rating, na.rm = TRUE),
    SD = sd(Rating, na.rm = TRUE),
    Min = min(Rating, na.rm = TRUE),
    Max = max(Rating, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(
    Aspekt,
    Bewertungsdimension,
    Promptbedingung
  ) %>%
  mutate(
    MW_Diff_ja_minus_nein =
      M[match("ja", Bedingung)] -
      M[match("nein", Bedingung)]
  ) %>%
  ungroup() %>%
  arrange(
    Aspekt,
    Bewertungsdimension,
    Promptbedingung,
    Bedingung
  )

tabelle_vergleich # entspricht Tabelle 6

############################################################
# 12. Explorative Analyse:
# Absolute und relative Häufigkeiten der Einzelbewertungen
# nach Promptbedingung, Studienplanungsaspekt und
# Bewertungsdimension
############################################################
# Diese Tabelle ergänzt tabelle_vergleich um Häufigkeiten.
#
# Berechnet werden absolute und relative Häufigkeiten der Kodierungen
# 1, 2 und 3 auf Basis der Einzelbewertungen der LLM-Outputs.
#
# Die Auswertung erfolgt getrennt nach:
#   - Studienplanungsaspekt
#   - Bewertungsdimension: Angemessenheit / Entscheidungshilfe
#   - Promptbedingung: Reflexionsanweisung / Hypothesen
#   - Bedingung: ja / nein
#
# Voraussetzung:
# Das Objekt d_long_vergleich wurde in Abschnitt 11.1 erzeugt.
############################################################


############################################################
# 12.1 Häufigkeiten im Long-Format berechnen
############################################################

Haeufigkeiten_Dimension_Prompt_Long <- d_long_vergleich %>%
  count(
    Aspekt,
    Bewertungsdimension,
    Promptbedingung,
    Bedingung,
    Rating,
    name = "n"
  ) %>%
  group_by(
    Aspekt,
    Bewertungsdimension,
    Promptbedingung,
    Bedingung
  ) %>%
  complete(
    Rating = kodierungen,
    fill = list(n = 0)
  ) %>%
  mutate(
    N_Einzelbewertungen = sum(n),
    Prozent = n / N_Einzelbewertungen * 100,
    Haeufigkeit = sprintf("%d (%.1f%%)", n, Prozent)
  ) %>%
  ungroup()


############################################################
# 12.2 Häufigkeiten im Wide-Format darstellen
############################################################

Haeufigkeiten_Dimension_Prompt_Wide <- Haeufigkeiten_Dimension_Prompt_Long %>%
  mutate(
    Aspekt = factor(
      Aspekt,
      levels = c(
        "Studiendesign",
        "Messinstrumente",
        "Kontrollvariablen"
      )
    ),
    Bewertungsdimension = factor(
      Bewertungsdimension,
      levels = c(
        "Angemessenheit",
        "Entscheidungshilfe"
      )
    ),
    Promptbedingung = factor(
      Promptbedingung,
      levels = c(
        "Reflexionsanweisung",
        "Hypothesen"
      )
    ),
    Bedingung = factor(
      Bedingung,
      levels = c("ja", "nein")
    ),
    Rating = paste0("Kodierung_", Rating)
  ) %>%
  select(
    Aspekt,
    Bewertungsdimension,
    Promptbedingung,
    Bedingung,
    N_Einzelbewertungen,
    Rating,
    Haeufigkeit
  ) %>%
  pivot_wider(
    names_from = Rating,
    values_from = Haeufigkeit
  ) %>%
  arrange(
    Aspekt,
    Bewertungsdimension,
    Promptbedingung,
    Bedingung
  )

# Tabelle anzeigen
Haeufigkeiten_Dimension_Prompt_Wide

############################################################
# 15. Ergebnisse speichern
############################################################

# Tabellen werden als CSV-Dateien gespeichert.
# Dadurch können sie unabhängig von R geöffnet und geprüft werden.

write.csv(
  tabelle_vergleich,
  file = file.path("results", "tabelle_vergleich.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  Haeufigkeiten_Aspekt_Prompt_Wide,
  file = file.path("results", "Haeufigkeiten_Aspekt_Prompt_Wide.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  Deskriptiv_Hypo_Kontrollvar,
  file = file.path("results", "Deskriptiv_Hypo_Kontrollvar.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  Deskriptiv_Prompt_Gesamt,
  file = file.path("results", "deskriptiv_prompt_gesamt.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  Haeufigkeiten_Prompt_Gesamt,
  file = file.path("results", "haeufigkeiten_prompt_gesamt.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  Deskriptiv_Aspekt_Prompt,
  file = file.path("results", "deskriptiv_aspekt_prompt.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  Deskriptiv_Refl_Kontrollvar,
  file = file.path("results", "Deskriptiv_Refl_Kontrollvar.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

## ab hier weiter ergänzen mit den Tabellen aus diesem Skript

write.csv(
  tab_wide,
  file = file.path("results", "tab_wide.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  Deskriptiv_Gesamt,
  file = file.path("results", "Deskriptiv_Gesamt.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


write.csv(
  Haeufigkeiten_Aspekte_Wide,
  file = file.path("results", "Haeufigkeiten_Aspekte_Wide.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# Zusätzlich alle zentralen Tabellen als RDS-Datei speichern.
# Das erhält Objektstrukturen und Faktoren besser als CSV.
saveRDS(
  list(
    tab_kodierungen_dimension = tab_kodierungen_dimension,
    Deskriptiv_Gesamt = Deskriptiv_Gesamt,
    Haeufigkeiten_Aspekte_Wide = Haeufigkeiten_Aspekte_Wide,
    Deskriptiv_Prompt_Gesamt = Deskriptiv_Prompt_Gesamt,
    Haeufigkeiten_Prompt_Gesamt = Haeufigkeiten_Prompt_Gesamt,
    Deskriptiv_Aspekt_Prompt = Deskriptiv_Aspekt_Prompt,
    Haeufigkeiten_Aspekt_Prompt_Wide = Haeufigkeiten_Aspekt_Prompt_Wide,
    Tabelle_Explorativ_Dimension = Tabelle_Explorativ_Dimension
  ),
  file = file.path("results", "alle_ergebnistabellen.rds")
)


############################################################
# 13. Reproduzierbarkeitsinformationen 
############################################################

sessionInfo()

############################################################
# Bachelorarbeit – Analyse der LLM-Outputs
# Autorin: Juno Meyer
# Datum: 10.08.2026
#
# Zweck des Skripts:
# Dieses Skript berechnet deskriptive Statistiken sowie absolute
# und relative Häufigkeiten für die in der Bachelorarbeit berichteten
# Tabellen. Analysiert werden Ratings zu drei Studienplanungsaspekten:
#   1. Studiendesign
#   2. Messinstrumente
#   3. Kontrollvariablen
#
# Pro Aspekt liegen zwei Bewertungsdimensionen vor:
#   - Angemessenheit
#   - Entscheidungshilfe
#
# Zusätzlich werden Auswertungen getrennt nach Promptbedingungen
# durchgeführt:
#   - Reflexionsanweisung: yes / no
#   - Hypothesenbedingung: yes / no
#
# Hinweise zur Reproduzierbarkeit:
# - Das Skript verwendet relative Pfade.
# - Es sollten keine personenbezogenen lokalen Pfade verwendet werden.
# - Paketversionen werden am Ende über sessionInfo() dokumentiert.
# - Rohdaten sollten im Ordner "data/" liegen.
# - Ergebnisse werden im Ordner "results/" gespeichert.
############################################################


############################################################
# 0. Vorbereitung
############################################################

# Ergebnisordner erstellen, falls er noch nicht existiert
if (!dir.exists("results")) {
  dir.create("results")
}


############################################################
# 1. Pakete laden
############################################################

# Pakete sollten vorab installiert sein.
# Für vollständige Reproduzierbarkeit kann zusätzlich renv genutzt werden.
# renv sollte einmalig im Projekt initialisiert werden:
# install.packages("renv")
# renv::init()
#
# Nach erfolgreicher Ausführung des Skripts kann der Paketstand mit
# folgendem Befehl dokumentiert werden:
# renv::snapshot()
#
# Andere Personen können die Paketumgebung später wiederherstellen mit:
# renv::restore()


library(readxl)
library(dplyr)
library(tidyr)
library(tibble)


############################################################
# 2. Daten einlesen
############################################################

# Relativer Pfad zur Datendatei.
# Bitte sicherstellen, dass die Datei im Ordner "data/" liegt.
data_path <- file.path(
  "data",
  "raw_llm_outputs_and_ratings.xlsx"
)

# Daten einlesen
d_raw <- read_excel(data_path)

# Erste Prüfung der Datenstruktur
str(d_raw)


############################################################
# 3. Definitionen und Metadaten
############################################################

# In diesem Abschnitt werden zentrale Informationen einmalig definiert.
# Dadurch wird vermieden, dass Variablennamen im Skript mehrfach
# manuell wiederholt werden müssen.

# Zuordnung der Rating-Variablen zu:
# - Studienplanungsaspekt
# - Bewertungsdimension
rating_info <- tribble(
  ~rating_variable,              ~Aspekt,              ~Bewertungsdimension,
  "Studiendesign_rating",        "Studiendesign",      "Angemessenheit",
  "Krit_Studiendesign_rating",   "Studiendesign",      "Entscheidungshilfe",
  "Messinstrumente_rating",      "Messinstrumente",    "Angemessenheit",
  "Krit_Messinstr_rating",       "Messinstrumente",    "Entscheidungshilfe",
  "Kontrollvariablen_rating",    "Kontrollvariablen",  "Angemessenheit",
  "Krit_Kontrollvar_rating",     "Kontrollvariablen",  "Entscheidungshilfe"
)

# Rating-Spalten aus rating_info ableiten
rating_cols <- rating_info$rating_variable

# Erwartete Kodierungswerte
# Annahme: 1 = niedrigste Ausprägung, 3 = höchste Ausprägung
rating_levels <- c(1, 2, 3)

# Reihenfolge der Studienplanungsaspekte
aspekt_levels <- c(
  "Studiendesign",
  "Messinstrumente",
  "Kontrollvariablen"
)

# Reihenfolge der Bewertungsdimensionen
dimension_levels <- c(
  "Angemessenheit",
  "Entscheidungshilfe"
)

# Reihenfolge der Promptbedingungen
prompt_levels <- c(
  "Reflexionsanweisung",
  "Hypothesen"
)

# Reihenfolge der Bedingungsausprägungen
# yes = Bedingung vorhanden
# no  = Bedingung nicht vorhanden
condition_levels <- c("yes", "no")


############################################################
# 4. Hilfsfunktionen
############################################################

# Funktion zur Vereinheitlichung von yes/no-Kodierungen.
# Dadurch wäre das Skript auch robust, falls Werte als "ja", "nein",
# "1", "0", "true" oder "false" vorliegen.
normalise_condition <- function(x) {
  x <- tolower(trimws(as.character(x)))
  
  case_when(
    x %in% c("yes", "ja", "1", "true") ~ "yes",
    x %in% c("no", "nein", "0", "false") ~ "no",
    TRUE ~ x
  )
}

# Funktion zur Berechnung von Zeilenmittelwerten.
# Falls eine Zeile ausschließlich fehlende Werte enthält, wird NA gesetzt.
safe_row_mean <- function(data, cols) {
  x <- rowMeans(data[, cols, drop = FALSE], na.rm = TRUE)
  x[is.nan(x)] <- NA_real_
  x
}

# Funktion zur Formatierung von Häufigkeiten:
# Beispiel: 12 (60.0%)
format_n_percent <- function(n, total) {
  percent <- ifelse(total > 0, n / total * 100, NA_real_)
  sprintf("%d (%.1f%%)", n, percent)
}


############################################################
# 5. Daten vorbereiten und prüfen
############################################################

# Prüfen, ob alle benötigten Spalten vorhanden sind
required_cols <- c("type", "hypo", rating_cols)

missing_cols <- setdiff(required_cols, names(d_raw))

if (length(missing_cols) > 0) {
  stop(
    "Folgende benötigte Spalten fehlen im Datensatz: ",
    paste(missing_cols, collapse = ", ")
  )
}

# Arbeitsdatensatz erstellen:
# - Promptbedingungen vereinheitlichen
# - Rating-Spalten numerisch kodieren
d <- d_raw %>%
  mutate(
    type = normalise_condition(type),
    hypo = normalise_condition(hypo),
    across(all_of(rating_cols), ~ suppressWarnings(as.numeric(.x)))
  )

# Prüfen, ob unerwartete Rating-Werte vorhanden sind
observed_ratings <- sort(unique(na.omit(unlist(d[rating_cols]))))
unexpected_ratings <- setdiff(observed_ratings, rating_levels)

if (length(unexpected_ratings) > 0) {
  warning(
    "Es wurden unerwartete Rating-Werte gefunden: ",
    paste(unexpected_ratings, collapse = ", ")
  )
}


############################################################
# 6. Long-Format der Einzelbewertungen erstellen
############################################################

# Für viele Auswertungen ist das Long-Format sinnvoller:
# Jede Zeile entspricht dann einer einzelnen Bewertung eines Outputs.
d_ratings_long <- d %>%
  mutate(Output_ID = row_number()) %>%
  select(Output_ID, type, hypo, all_of(rating_cols)) %>%
  pivot_longer(
    cols = all_of(rating_cols),
    names_to = "rating_variable",
    values_to = "Rating"
  ) %>%
  left_join(rating_info, by = "rating_variable") %>%
  mutate(
    Aspekt = factor(Aspekt, levels = aspekt_levels),
    Bewertungsdimension = factor(
      Bewertungsdimension,
      levels = dimension_levels
    )
  ) %>%
  filter(!is.na(Rating))


############################################################
# 7. Tabelle: Häufigkeiten der Kodierungen nach
# Studienplanungsaspekt und Bewertungsdimension
############################################################

# Diese Tabelle zeigt, wie oft die Kodierungen 1, 2 und 3 je
# Studienplanungsaspekt und Bewertungsdimension vergeben wurden.

tab_kodierungen_dimension <- d_ratings_long %>%
  count(
    Aspekt,
    Bewertungsdimension,
    Rating,
    name = "n"
  ) %>%
  group_by(Aspekt, Bewertungsdimension) %>%
  complete(
    Rating = rating_levels,
    fill = list(n = 0)
  ) %>%
  mutate(
    N = sum(n),
    Wert = format_n_percent(n, N)
  ) %>%
  ungroup() %>%
  mutate(
    Rating = paste0("Rating_", Rating)
  ) %>%
  select(
    Aspekt,
    Bewertungsdimension,
    N,
    Rating,
    Wert
  ) %>%
  pivot_wider(
    names_from = Rating,
    values_from = Wert
  ) %>%
  arrange(
    Aspekt,
    Bewertungsdimension
  )

tab_kodierungen_dimension


############################################################
# 8. Forschungsfrage 1:
# Deskriptive Kennwerte der Studienplanungsaspekte
############################################################

# Für jeden Output wird pro Studienplanungsaspekt ein Mittelwert aus den
# beiden zugehörigen Bewertungsdimensionen gebildet:
#
# Studiendesign:
#   - Studiendesign_rating
#   - Krit_Studiendesign_rating
#
# Messinstrumente:
#   - Messinstrumente_rating
#   - Krit_Messinstr_rating
#
# Kontrollvariablen:
#   - Kontrollvariablen_rating
#   - Krit_Kontrollvar_rating

aspect_cols <- list(
  Studiendesign = c(
    "Krit_Studiendesign_rating",
    "Studiendesign_rating"
  ),
  Messinstrumente = c(
    "Krit_Messinstr_rating",
    "Messinstrumente_rating"
  ),
  Kontrollvariablen = c(
    "Krit_Kontrollvar_rating",
    "Kontrollvariablen_rating"
  )
)

# Datensatz mit aggregierten Aspekt-Scores erstellen
d_scores <- d %>%
  mutate(Output_ID = row_number())

d_scores$Studiendesign <- safe_row_mean(
  d_scores,
  aspect_cols$Studiendesign
)

d_scores$Messinstrumente <- safe_row_mean(
  d_scores,
  aspect_cols$Messinstrumente
)

d_scores$Kontrollvariablen <- safe_row_mean(
  d_scores,
  aspect_cols$Kontrollvariablen
)

# Gesamtqualität über alle sechs Rating-Variablen
d_scores$Gesamtqualitaet <- safe_row_mean(
  d_scores,
  rating_cols
)

# Deskriptive Kennwerte pro Studienplanungsaspekt
Deskriptiv_Gesamt <- d_scores %>%
  select(
    Output_ID,
    all_of(names(aspect_cols))
  ) %>%
  pivot_longer(
    cols = all_of(names(aspect_cols)),
    names_to = "Aspekt",
    values_to = "Score"
  ) %>%
  mutate(
    Aspekt = factor(Aspekt, levels = aspekt_levels)
  ) %>%
  group_by(Aspekt) %>%
  summarise(
    N = sum(!is.na(Score)),
    Mittelwert = mean(Score, na.rm = TRUE),
    Median = median(Score, na.rm = TRUE),
    Standardabweichung = sd(Score, na.rm = TRUE),
    Minimum = min(Score, na.rm = TRUE),
    Maximum = max(Score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Aspekt)

Deskriptiv_Gesamt


############################################################
# 9. Forschungsfrage 1:
# Absolute und relative Häufigkeiten je Studienplanungsaspekt
############################################################

# Hier werden die Einzelbewertungen innerhalb eines Aspekts zusammengefasst.
# Pro Aspekt liegen pro Output zwei Einzelbewertungen vor.

Haeufigkeiten_Aspekte_Wide <- d_ratings_long %>%
  count(
    Aspekt,
    Rating,
    name = "n"
  ) %>%
  group_by(Aspekt) %>%
  complete(
    Rating = rating_levels,
    fill = list(n = 0)
  ) %>%
  mutate(
    N_Einzelbewertungen = sum(n),
    Haeufigkeit = format_n_percent(n, N_Einzelbewertungen),
    Rating = paste0("Kodierung_", Rating)
  ) %>%
  ungroup() %>%
  select(
    Aspekt,
    N_Einzelbewertungen,
    Rating,
    Haeufigkeit
  ) %>%
  pivot_wider(
    names_from = Rating,
    values_from = Haeufigkeit
  ) %>%
  arrange(Aspekt)

Haeufigkeiten_Aspekte_Wide


############################################################
# 10. Forschungsfrage 2a:
# Gesamtqualität nach Promptbedingung
############################################################

# Fragestellung:
# Unterscheidet sich die aggregierte Gesamtqualität je nachdem,
# ob eine Reflexionsanweisung bzw. Hypothesenbedingung vorlag?

# Long-Format für Promptbedingungen erstellen:
# Pro Output entstehen zwei Zeilen:
# - eine für type bzw. Reflexionsanweisung
# - eine für hypo bzw. Hypothesen
d_prompt_scores_total <- d_scores %>%
  select(
    Output_ID,
    type,
    hypo,
    Gesamtqualitaet
  ) %>%
  pivot_longer(
    cols = c(type, hypo),
    names_to = "Promptvariable",
    values_to = "Bedingung"
  ) %>%
  mutate(
    Promptbedingung = case_when(
      Promptvariable == "type" ~ "Reflexionsanweisung",
      Promptvariable == "hypo" ~ "Hypothesen"
    ),
    Promptbedingung = factor(
      Promptbedingung,
      levels = prompt_levels
    ),
    Bedingung = factor(
      Bedingung,
      levels = condition_levels
    )
  ) %>%
  filter(!is.na(Bedingung))

# Deskriptive Kennwerte der Gesamtqualität nach Promptbedingung
Deskriptiv_Prompt_Gesamt <- d_prompt_scores_total %>%
  group_by(
    Promptbedingung,
    Bedingung
  ) %>%
  summarise(
    N = sum(!is.na(Gesamtqualitaet)),
    Mittelwert = mean(Gesamtqualitaet, na.rm = TRUE),
    Standardabweichung = sd(Gesamtqualitaet, na.rm = TRUE),
    Median = median(Gesamtqualitaet, na.rm = TRUE),
    Minimum = min(Gesamtqualitaet, na.rm = TRUE),
    Maximum = max(Gesamtqualitaet, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Promptbedingung) %>%
  mutate(
    MW_Diff_yes_minus_no =
      Mittelwert[Bedingung == "yes"][1] -
      Mittelwert[Bedingung == "no"][1]
  ) %>%
  ungroup() %>%
  arrange(
    Promptbedingung,
    Bedingung
  )

Deskriptiv_Prompt_Gesamt


############################################################
# 11. Forschungsfrage 2a:
# Häufigkeiten der Einzelbewertungen nach Promptbedingung
############################################################

# Hier werden alle sechs Einzelbewertungen gemeinsam betrachtet.
# Die Tabelle zeigt, wie häufig die Kodierungen 1, 2 und 3 je
# Promptbedingung vergeben wurden.

d_prompt_ratings_long <- d_ratings_long %>%
  pivot_longer(
    cols = c(type, hypo),
    names_to = "Promptvariable",
    values_to = "Bedingung"
  ) %>%
  mutate(
    Promptbedingung = case_when(
      Promptvariable == "type" ~ "Reflexionsanweisung",
      Promptvariable == "hypo" ~ "Hypothesen"
    ),
    Promptbedingung = factor(
      Promptbedingung,
      levels = prompt_levels
    ),
    Bedingung = factor(
      Bedingung,
      levels = condition_levels
    )
  ) %>%
  filter(!is.na(Bedingung))

Haeufigkeiten_Prompt_Gesamt <- d_prompt_ratings_long %>%
  count(
    Promptbedingung,
    Bedingung,
    Rating,
    name = "n"
  ) %>%
  group_by(
    Promptbedingung,
    Bedingung
  ) %>%
  complete(
    Rating = rating_levels,
    fill = list(n = 0)
  ) %>%
  mutate(
    N_Einzelbewertungen = sum(n),
    Haeufigkeit = format_n_percent(n, N_Einzelbewertungen),
    Rating = paste0("Kodierung_", Rating)
  ) %>%
  ungroup() %>%
  select(
    Promptbedingung,
    Bedingung,
    N_Einzelbewertungen,
    Rating,
    Haeufigkeit
  ) %>%
  pivot_wider(
    names_from = Rating,
    values_from = Haeufigkeit
  ) %>%
  arrange(
    Promptbedingung,
    Bedingung
  )

Haeufigkeiten_Prompt_Gesamt


############################################################
# 12. Forschungsfrage 2b:
# Deskriptive Kennwerte je Studienplanungsaspekt und Promptbedingung
############################################################

# Hier wird untersucht, ob sich die Qualität innerhalb einzelner
# Studienplanungsaspekte zwischen den Promptbedingungen unterscheidet.

d_aspect_scores_long <- d_scores %>%
  select(
    Output_ID,
    type,
    hypo,
    all_of(names(aspect_cols))
  ) %>%
  pivot_longer(
    cols = all_of(names(aspect_cols)),
    names_to = "Aspekt",
    values_to = "Score"
  ) %>%
  pivot_longer(
    cols = c(type, hypo),
    names_to = "Promptvariable",
    values_to = "Bedingung"
  ) %>%
  mutate(
    Aspekt = factor(
      Aspekt,
      levels = aspekt_levels
    ),
    Promptbedingung = case_when(
      Promptvariable == "type" ~ "Reflexionsanweisung",
      Promptvariable == "hypo" ~ "Hypothesen"
    ),
    Promptbedingung = factor(
      Promptbedingung,
      levels = prompt_levels
    ),
    Bedingung = factor(
      Bedingung,
      levels = condition_levels
    )
  ) %>%
  filter(!is.na(Bedingung))

Deskriptiv_Aspekt_Prompt <- d_aspect_scores_long %>%
  group_by(
    Aspekt,
    Promptbedingung,
    Bedingung
  ) %>%
  summarise(
    N = sum(!is.na(Score)),
    Mittelwert = mean(Score, na.rm = TRUE),
    Standardabweichung = sd(Score, na.rm = TRUE),
    Median = median(Score, na.rm = TRUE),
    Minimum = min(Score, na.rm = TRUE),
    Maximum = max(Score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(
    Aspekt,
    Promptbedingung
  ) %>%
  mutate(
    MW_Diff_yes_minus_no =
      Mittelwert[Bedingung == "yes"][1] -
      Mittelwert[Bedingung == "no"][1]
  ) %>%
  ungroup() %>%
  arrange(
    Aspekt,
    Promptbedingung,
    Bedingung
  )

Deskriptiv_Aspekt_Prompt


############################################################
# 13. Forschungsfrage 2b:
# Häufigkeiten je Studienplanungsaspekt und Promptbedingung
############################################################

# Diese Tabelle zeigt die Verteilung der Einzelbewertungen getrennt nach:
# - Studienplanungsaspekt
# - Promptbedingung
# - yes/no-Bedingung

Haeufigkeiten_Aspekt_Prompt_Wide <- d_prompt_ratings_long %>%
  count(
    Aspekt,
    Promptbedingung,
    Bedingung,
    Rating,
    name = "n"
  ) %>%
  group_by(
    Aspekt,
    Promptbedingung,
    Bedingung
  ) %>%
  complete(
    Rating = rating_levels,
    fill = list(n = 0)
  ) %>%
  mutate(
    N_Einzelbewertungen = sum(n),
    Haeufigkeit = format_n_percent(n, N_Einzelbewertungen),
    Rating = paste0("Kodierung_", Rating)
  ) %>%
  ungroup() %>%
  select(
    Aspekt,
    Promptbedingung,
    Bedingung,
    N_Einzelbewertungen,
    Rating,
    Haeufigkeit
  ) %>%
  pivot_wider(
    names_from = Rating,
    values_from = Haeufigkeit
  ) %>%
  arrange(
    Aspekt,
    Promptbedingung,
    Bedingung
  )

Haeufigkeiten_Aspekt_Prompt_Wide


############################################################
# 14. Explorative Analyse:
# Deskriptive Kennwerte nach Promptbedingung,
# Studienplanungsaspekt und Bewertungsdimension
############################################################

# Diese Tabelle betrachtet die Einzelbewertungen noch feiner:
# getrennt nach
# - Studienplanungsaspekt
# - Bewertungsdimension
# - Promptbedingung
# - yes/no-Bedingung

Tabelle_Explorativ_Dimension <- d_prompt_ratings_long %>%
  group_by(
    Aspekt,
    Bewertungsdimension,
    Promptbedingung,
    Bedingung
  ) %>%
  summarise(
    N = n(),
    Mittelwert = mean(Rating, na.rm = TRUE),
    Median = median(Rating, na.rm = TRUE),
    Standardabweichung = sd(Rating, na.rm = TRUE),
    Minimum = min(Rating, na.rm = TRUE),
    Maximum = max(Rating, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(
    Aspekt,
    Bewertungsdimension,
    Promptbedingung
  ) %>%
  mutate(
    MW_Diff_yes_minus_no =
      Mittelwert[Bedingung == "yes"][1] -
      Mittelwert[Bedingung == "no"][1]
  ) %>%
  ungroup() %>%
  arrange(
    Aspekt,
    Bewertungsdimension,
    Promptbedingung,
    Bedingung
  )

Tabelle_Explorativ_Dimension


############################################################
# 15. Ergebnisse speichern
############################################################

# Tabellen werden als CSV-Dateien gespeichert.
# Dadurch können sie unabhängig von R geöffnet und geprüft werden.

write.csv(
  tab_kodierungen_dimension,
  file = file.path("results", "tab_kodierungen_dimension.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  Deskriptiv_Gesamt,
  file = file.path("results", "deskriptiv_gesamt.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  Haeufigkeiten_Aspekte_Wide,
  file = file.path("results", "haeufigkeiten_aspekte_wide.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  Deskriptiv_Prompt_Gesamt,
  file = file.path("results", "deskriptiv_prompt_gesamt.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  Haeufigkeiten_Prompt_Gesamt,
  file = file.path("results", "haeufigkeiten_prompt_gesamt.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  Deskriptiv_Aspekt_Prompt,
  file = file.path("results", "deskriptiv_aspekt_prompt.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  Haeufigkeiten_Aspekt_Prompt_Wide,
  file = file.path("results", "haeufigkeiten_aspekt_prompt_wide.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  Tabelle_Explorativ_Dimension,
  file = file.path("results", "tabelle_explorativ_dimension.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# Zusätzlich alle zentralen Tabellen als RDS-Datei speichern.
# Das erhält Objektstrukturen und Faktoren besser als CSV.
saveRDS(
  list(
    tab_kodierungen_dimension = tab_kodierungen_dimension,
    Deskriptiv_Gesamt = Deskriptiv_Gesamt,
    Haeufigkeiten_Aspekte_Wide = Haeufigkeiten_Aspekte_Wide,
    Deskriptiv_Prompt_Gesamt = Deskriptiv_Prompt_Gesamt,
    Haeufigkeiten_Prompt_Gesamt = Haeufigkeiten_Prompt_Gesamt,
    Deskriptiv_Aspekt_Prompt = Deskriptiv_Aspekt_Prompt,
    Haeufigkeiten_Aspekt_Prompt_Wide = Haeufigkeiten_Aspekt_Prompt_Wide,
    Tabelle_Explorativ_Dimension = Tabelle_Explorativ_Dimension
  ),
  file = file.path("results", "alle_ergebnistabellen.rds")
)

############################################################
# 16. Ergänzende Tabelle:
# Häufigkeiten der Einzelbewertungen nach Studienplanungsaspekt,
# Bewertungsdimension und Promptbedingung
############################################################

# Diese Tabelle ergänzt die explorative Analyse aus Abschnitt 14.
# Sie zeigt, wie häufig die Kodierungen 1, 2 und 3 vergeben wurden,
# getrennt nach:
# - Studienplanungsaspekt
# - Bewertungsdimension
# - Promptbedingung
# - yes/no-Bedingung
#
# Grundlage sind die Einzelbewertungen der LLM-Outputs.

Haeufigkeiten_Dimension_Prompt_Wide <- d_prompt_ratings_long %>%
  count(
    Aspekt,
    Bewertungsdimension,
    Promptbedingung,
    Bedingung,
    Rating,
    name = "n"
  ) %>%
  group_by(
    Aspekt,
    Bewertungsdimension,
    Promptbedingung,
    Bedingung
  ) %>%
  complete(
    Rating = rating_levels,
    fill = list(n = 0)
  ) %>%
  mutate(
    N_Einzelbewertungen = sum(n),
    Haeufigkeit = format_n_percent(n, N_Einzelbewertungen),
    Rating = paste0("Kodierung_", Rating)
  ) %>%
  ungroup() %>%
  select(
    Aspekt,
    Bewertungsdimension,
    Promptbedingung,
    Bedingung,
    N_Einzelbewertungen,
    Rating,
    Haeufigkeit
  ) %>%
  pivot_wider(
    names_from = Rating,
    values_from = Haeufigkeit
  ) %>%
  arrange(
    Aspekt,
    Bewertungsdimension,
    Promptbedingung,
    Bedingung
  )

Haeufigkeiten_Dimension_Prompt_Wide


############################################################
# 17. Ergänzende Tabelle speichern
############################################################

# Die ergänzende Häufigkeitstabelle wird als CSV-Datei gespeichert.

write.csv(
  Haeufigkeiten_Dimension_Prompt_Wide,
  file = file.path(
    "results",
    "haeufigkeiten_dimension_prompt_wide.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# Zusätzlich wird die Tabelle als einzelne RDS-Datei gespeichert.
# Dadurch bleiben Objektstruktur und Faktorinformationen erhalten.

saveRDS(
  Haeufigkeiten_Dimension_Prompt_Wide,
  file = file.path(
    "results",
    "haeufigkeiten_dimension_prompt_wide.rds"
  )
)

# Falls bereits eine zentrale RDS-Datei mit allen Ergebnistabellen
# existiert, wird diese um die neue Tabelle ergänzt.
# Dadurch bleibt die neue Tabelle auch in der zentralen Ergebnisdatei
# dokumentiert.

ergebnistabellen_path <- file.path(
  "results",
  "alle_ergebnistabellen.rds"
)

if (file.exists(ergebnistabellen_path)) {
  
  alle_ergebnistabellen <- readRDS(ergebnistabellen_path)
  
  alle_ergebnistabellen$Haeufigkeiten_Dimension_Prompt_Wide <-
    Haeufigkeiten_Dimension_Prompt_Wide
  
  saveRDS(
    alle_ergebnistabellen,
    file = ergebnistabellen_path
  )
  
}
############################################################
# 18. Reproduzierbarkeit dokumentieren
############################################################

# sessionInfo() dokumentiert:
# - R-Version
# - Betriebssystem
# - geladene Pakete
# - Paketversionen
#
# Diese Information ist wichtig, damit andere Personen die Analyse
# später möglichst exakt reproduzieren können.

sink(file.path("results", "sessionInfo.txt"))
print(sessionInfo())
sink()


############################################################
# Ende des Skripts
############################################################