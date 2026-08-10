
############################################################
# 0. Arbeitsverzeichnis setzen
############################################################

setwd("C:/Users/User/OneDrive - Universität Münster/Uni/Bachelorarbeit/Code")

############################################################
# 1. Pakete installieren und laden
############################################################

# install.packages("readxl", type = "binary")
library(readxl)
# install.packages("dplyr")
library(dplyr)
# install.packages("tidyr")
library(tidyr)
# install.packages("tibble")
library(tibble)
# install.packages("psych")
library(psych)


############################################################
# 2. Daten einlesen
############################################################

d <- read_excel(
  "C:/Users/User/OneDrive - Universität Münster/Uni/Bachelorarbeit/Code/Condition_Matrix_with_LLM_Output_31 - Kopie.xlsx"
)

# Datenstruktur prüfen
str(d)



############################################################
# Tabelle: Häufigkeiten der Kodierungen nach
# Studienplanungsaspekt und Bewertungsdimension
############################################################

# Zuordnung der Variablen zu Aspekt und Bewertungsdimension
var_info <- tribble(
  ~Variable,                     ~Aspekt,              ~Dimension,
  "Studiendesign_rating",         "Studiendesign",      "Angemessenheit",
  "Krit_Studiendesign_rating",    "Studiendesign",      "Entscheidungshilfe",
  "Messinstrumente_rating",       "Messinstrumente",    "Angemessenheit",
  "Krit_Messinstr_rating",        "Messinstrumente",    "Entscheidungshilfe",
  "Kontrollvariablen_rating",     "Kontrollvariablen",  "Angemessenheit",
  "Krit_Kontrollvar_rating",      "Kontrollvariablen",  "Entscheidungshilfe"
)

# alle vorhandenen Rating-Ausprägungen bestimmen
rating_levels <- d %>%
  select(all_of(var_info$Variable)) %>%
  pivot_longer(everything(), values_to = "Rating") %>%
  filter(!is.na(Rating)) %>%
  mutate(Rating = as.character(Rating)) %>%
  distinct(Rating) %>%
  arrange(Rating) %>%
  pull(Rating)

# Tabelle im Wide-Format
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

tab_wide # Abgleich mit Tabelle 7 (Übersicht der Verteilung der Kodierungen pro Bewertungsdimensionen)


############################################################
# Daten aggregieren Frage 1
############################################################

# Teildatensatz erstellen 

Studiendesign_Gesamt <- rowMeans(
  d[, c("Krit_Studiendesign_rating", "Studiendesign_rating")],
  na.rm = TRUE
)

Studiendesign_Gesamt


Messinstrumente_Gesamt <- rowMeans(
  d[, c("Krit_Messinstr_rating", "Messinstrumente_rating")],
  na.rm = TRUE
)

Messinstrumente_Gesamt

Kontrollvariablen_Gesamt <- rowMeans(
  d[, c("Krit_Kontrollvar_rating", "Kontrollvariablen_rating")],
  na.rm = TRUE
)

Kontrollvariablen_Gesamt


# Deskriptive Statistiken berechnen 
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

Deskriptiv_Gesamt <- rbind(
  Studiendesign = deskriptiv(Studiendesign_Gesamt),
  Messinstrumente = deskriptiv(Messinstrumente_Gesamt),
  Kontrollvariablen = deskriptiv(Kontrollvariablen_Gesamt)
)

Deskriptiv_Gesamt <- as.data.frame(Deskriptiv_Gesamt)

Deskriptiv_Gesamt # Abgleich mit Tabelle 4


#########################################################################
# Absolute und relative Häufigkeiten Frage 1: Darstellung in Wide-Tabelle
#########################################################################

# eine Wide-Tabelle erstellen zum Vergleich der Häufigkeiten zwischen den Aspekten 

# Aspekte und zugehörige Variablen definieren
aspekte <- list(
  Studiendesign = c("Krit_Studiendesign_rating", "Studiendesign_rating"),
  Messinstrumente = c("Krit_Messinstr_rating", "Messinstrumente_rating"),
  Kontrollvariablen = c("Krit_Kontrollvar_rating", "Kontrollvariablen_rating")
)

# Alle vorkommenden Rating-Werte bestimmen
alle_werte <- sort(unique(na.omit(unlist(d[unlist(aspekte)], use.names = FALSE))))

# Wide-Tabelle erstellen
Haeufigkeiten_Aspekte_Wide <- do.call(rbind, lapply(names(aspekte), function(aspekt) {
  
  x <- unlist(d[aspekte[[aspekt]]], use.names = FALSE)
  x <- x[!is.na(x)]
  
  abs_h <- table(factor(x, levels = alle_werte))
  proz <- prop.table(abs_h) * 100
  
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

Haeufigkeiten_Aspekte_Wide # Abgleich Tabelle 7 


############################################################
# Daten aggregieren Frage 2a
############################################################

# Gesamtqualität abhängig von Reflexionsanweisung

# Teildatensatz im Longformat erstellen 
# ausprobieren nur n = 20 pro Gruppe 
Refl_Gesamt <- data.frame(
  type = d$type,
  rating = rowMeans(d[ , c("Krit_Studiendesign_rating", "Studiendesign_rating", "Krit_Messinstr_rating", "Messinstrumente_rating", "Krit_Kontrollvar_rating", "Kontrollvariablen_rating")])
)


# alle deskriptiven Kennwerte gleichzeitig berechnen
Deskriptiv_Refl_Gesamt <- aggregate(
  rating ~ type,
  data = Refl_Gesamt,
  FUN = function(x) c(
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE), 
    N = sum(!is.na(x))
  )
)

Deskriptiv_Refl_Gesamt <- do.call(data.frame, Deskriptiv_Refl_Gesamt)

names(Deskriptiv_Refl_Gesamt) <- c(
  "type", "mean", "sd", "median", "min", "max"
)

Deskriptiv_Refl_Gesamt # Abgleich Tabelle 1 (oberer Teil)

# Gesamtqualität abhängig von Hypothesen

# Teildatensatz im Longformat erstellen
# für n = 20 pro Gruppe
Hypo_Gesamt <- data.frame(
  type = d$hypo,
  rating = rowMeans(d[ , c("Krit_Studiendesign_rating", "Studiendesign_rating", "Krit_Messinstr_rating", "Messinstrumente_rating", "Krit_Kontrollvar_rating", "Kontrollvariablen_rating")])
)


# alle deskriptiven Kennwerte gleichzeitig berechnen
Deskriptiv_Hypo_Gesamt <- aggregate(
  rating ~ type,
  data = Hypo_Gesamt,
  FUN = function(x) c(
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    N = sum(!is.na(x))
  )
)

Deskriptiv_Hypo_Gesamt <- do.call(data.frame, Deskriptiv_Hypo_Gesamt)

names(Deskriptiv_Hypo_Gesamt) <- c(
  "type", "mean", "sd", "median", "min", "max"
)

Deskriptiv_Hypo_Gesamt  # Abgleich Tabelle 1 (unterer Teil)

# Häufigkeiten der Einzelbewertungen berechnen zur Beantwortung von Frage 2a: summiert über alle Bewertungsdimensionen 

# Sechs Bewertungsdimensionen definieren
rating_spalten <- c(
  "Krit_Studiendesign_rating",
  "Studiendesign_rating",
  "Krit_Messinstr_rating",
  "Messinstrumente_rating",
  "Krit_Kontrollvar_rating",
  "Kontrollvariablen_rating"
)

# Funktion für Häufigkeitstabelle im Wide-Format
haeufigkeiten_wide <- function(data, gruppenvariable, rating_spalten, werte = c(1, 2, 3)) {
  
  gruppen <- unique(data[[gruppenvariable]][!is.na(data[[gruppenvariable]])])
  
  tab <- do.call(rbind, lapply(gruppen, function(gruppe) {
    
    # Zeilen der jeweiligen Gruppe auswählen
    zeilen <- !is.na(data[[gruppenvariable]]) & data[[gruppenvariable]] == gruppe
    
    # Alle sechs Bewertungsdimensionen der Gruppe zusammenführen
    x <- unlist(data[zeilen, rating_spalten], use.names = FALSE)
    
    # Fehlende Werte entfernen
    x <- x[!is.na(x)]
    
    # Absolute Häufigkeiten für Kodierungen 1, 2, 3
    abs_h <- table(factor(x, levels = werte))
    
    # Prozentwerte innerhalb der Gruppe
    proz <- prop.table(abs_h) * 100
    
    # Darstellung: absolute Häufigkeit und Prozent in Klammern
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

# Wide-Tabelle Häufigkeiten je nach Gruppe Reflexionsanweisung
Haeufigkeiten_Refl_Wide <- haeufigkeiten_wide(
  data = d,
  gruppenvariable = "type",
  rating_spalten = rating_spalten
)

Haeufigkeiten_Refl_Wide # Abgleich Tabelle 2 (oberer Teil)

# Wide-Tabelle Häufigkeiten je nach Gruppe Hypothesen
Haeufigkeiten_Hypo_Wide <- haeufigkeiten_wide(
  data = d,
  gruppenvariable = "hypo",
  rating_spalten = rating_spalten
)

Haeufigkeiten_Hypo_Wide # Abgleich Tabelle 2 (unterer Teil)


#################################################################
# Daten aggregieren für deskriptive Kennwerte (Frage 2b)  #######
#################################################################
unique(d$type)
unique(d$hypo)

ohne_refl <- "no"
mit_refl  <- "yes"

ohne_hypo <- "no"
mit_hypo  <- "yes"

# Aspekt Studiendesign abhängig von Reflexionsanweisung

Refl_Studiendesign <- data.frame(
  type = d$type,
  rating = rowMeans(
    d[, c("Krit_Studiendesign_rating", "Studiendesign_rating")],
    na.rm = TRUE
  )
)

Refl_Studiendesign$rating[is.nan(Refl_Studiendesign$rating)] <- NA

Deskriptiv_Refl_Studiendesign <- aggregate(
  rating ~ type,
  data = Refl_Studiendesign,
  FUN = function(x) c(
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    N = sum(!is.na(x))
  )
)

Deskriptiv_Refl_Studiendesign <- do.call(data.frame, Deskriptiv_Refl_Studiendesign)

names(Deskriptiv_Refl_Studiendesign) <- c(
  "type", "mean", "sd", "median", "min", "max", "N"
)

MW_Diff_Refl_Studiendesign <- 
  Deskriptiv_Refl_Studiendesign$mean[Deskriptiv_Refl_Studiendesign$type == mit_refl] -
  Deskriptiv_Refl_Studiendesign$mean[Deskriptiv_Refl_Studiendesign$type == ohne_refl]

Deskriptiv_Refl_Studiendesign$MW_Diff_mit_minus_ohne <- MW_Diff_Refl_Studiendesign

Deskriptiv_Refl_Studiendesign

# Aspekt Studiendesign abhängig von Hypothesenbedingung

Hypo_Studiendesign <- data.frame(
  hypo = d$hypo,
  rating = rowMeans(
    d[, c("Krit_Studiendesign_rating", "Studiendesign_rating")],
    na.rm = TRUE
  )
)

Hypo_Studiendesign$rating[is.nan(Hypo_Studiendesign$rating)] <- NA

Deskriptiv_Hypo_Studiendesign <- aggregate(
  rating ~ hypo,
  data = Hypo_Studiendesign,
  FUN = function(x) c(
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    N = sum(!is.na(x))
  )
)

Deskriptiv_Hypo_Studiendesign <- do.call(data.frame, Deskriptiv_Hypo_Studiendesign)

names(Deskriptiv_Hypo_Studiendesign) <- c(
  "hypo", "mean", "sd", "median", "min", "max", "N"
)

MW_Diff_Hypo_Studiendesign <- 
  Deskriptiv_Hypo_Studiendesign$mean[Deskriptiv_Hypo_Studiendesign$hypo == mit_hypo] -
  Deskriptiv_Hypo_Studiendesign$mean[Deskriptiv_Hypo_Studiendesign$hypo == ohne_hypo]

Deskriptiv_Hypo_Studiendesign$MW_Diff_mit_minus_ohne <- MW_Diff_Hypo_Studiendesign

Deskriptiv_Hypo_Studiendesign

# Aspekt Messinstrumente abhängig von Reflexionsanweisung

Refl_Messinstrumente <- data.frame(
  type = d$type,
  rating = rowMeans(
    d[, c("Krit_Messinstr_rating", "Messinstrumente_rating")],
    na.rm = TRUE
  )
)

Refl_Messinstrumente$rating[is.nan(Refl_Messinstrumente$rating)] <- NA

Deskriptiv_Refl_Messinstrumente <- aggregate(
  rating ~ type,
  data = Refl_Messinstrumente,
  FUN = function(x) c(
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    N = sum(!is.na(x))
  )
)

Deskriptiv_Refl_Messinstrumente <- do.call(data.frame, Deskriptiv_Refl_Messinstrumente)

names(Deskriptiv_Refl_Messinstrumente) <- c(
  "type", "mean", "sd", "median", "min", "max", "N"
)

MW_Diff_Refl_Messinstrumente <- 
  Deskriptiv_Refl_Messinstrumente$mean[Deskriptiv_Refl_Messinstrumente$type == mit_refl] -
  Deskriptiv_Refl_Messinstrumente$mean[Deskriptiv_Refl_Messinstrumente$type == ohne_refl]

Deskriptiv_Refl_Messinstrumente$MW_Diff_mit_minus_ohne <- MW_Diff_Refl_Messinstrumente

Deskriptiv_Refl_Messinstrumente

# Aspekt Messinstrumente abhängig von Hypothesenbedingung

Hypo_Messinstrumente <- data.frame(
  hypo = d$hypo,
  rating = rowMeans(
    d[, c("Krit_Messinstr_rating", "Messinstrumente_rating")],
    na.rm = TRUE
  )
)

Hypo_Messinstrumente$rating[is.nan(Hypo_Messinstrumente$rating)] <- NA

Deskriptiv_Hypo_Messinstrumente <- aggregate(
  rating ~ hypo,
  data = Hypo_Messinstrumente,
  FUN = function(x) c(
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    N = sum(!is.na(x))
  )
)

Deskriptiv_Hypo_Messinstrumente <- do.call(data.frame, Deskriptiv_Hypo_Messinstrumente)

names(Deskriptiv_Hypo_Messinstrumente) <- c(
  "hypo", "mean", "sd", "median", "min", "max", "N"
)

MW_Diff_Hypo_Messinstrumente <- 
  Deskriptiv_Hypo_Messinstrumente$mean[Deskriptiv_Hypo_Messinstrumente$hypo == mit_hypo] -
  Deskriptiv_Hypo_Messinstrumente$mean[Deskriptiv_Hypo_Messinstrumente$hypo == ohne_hypo]

Deskriptiv_Hypo_Messinstrumente$MW_Diff_mit_minus_ohne <- MW_Diff_Hypo_Messinstrumente

Deskriptiv_Hypo_Messinstrumente

# Aspekt Kontrollvariablen abhängig von Reflexionsanweisung

Refl_Kontrollvariablen <- data.frame(
  type = d$type,
  rating = rowMeans(
    d[, c("Krit_Kontrollvar_rating", "Kontrollvariablen_rating")],
    na.rm = TRUE
  )
)

Refl_Kontrollvariablen$rating[is.nan(Refl_Kontrollvariablen$rating)] <- NA

Deskriptiv_Refl_Kontrollvar <- aggregate(
  rating ~ type,
  data = Refl_Kontrollvariablen,
  FUN = function(x) c(
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    N = sum(!is.na(x))
  )
)

Deskriptiv_Refl_Kontrollvar <- do.call(data.frame, Deskriptiv_Refl_Kontrollvar)

names(Deskriptiv_Refl_Kontrollvar) <- c(
  "type", "mean", "sd", "median", "min", "max", "N"
)

MW_Diff_Refl_Kontrollvar <- 
  Deskriptiv_Refl_Kontrollvar$mean[Deskriptiv_Refl_Kontrollvar$type == mit_refl] -
  Deskriptiv_Refl_Kontrollvar$mean[Deskriptiv_Refl_Kontrollvar$type == ohne_refl]

Deskriptiv_Refl_Kontrollvar$MW_Diff_mit_minus_ohne <- MW_Diff_Refl_Kontrollvar

Deskriptiv_Refl_Kontrollvar

# Aspekt Kontrollvariablen abhängig von Hypothesenbedingung

Hypo_Kontrollvariablen <- data.frame(
  hypo = d$hypo,
  rating = rowMeans(
    d[, c("Krit_Kontrollvar_rating", "Kontrollvariablen_rating")],
    na.rm = TRUE
  )
)

Hypo_Kontrollvariablen$rating[is.nan(Hypo_Kontrollvariablen$rating)] <- NA

Deskriptiv_Hypo_Kontrollvar <- aggregate(
  rating ~ hypo,
  data = Hypo_Kontrollvariablen,
  FUN = function(x) c(
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    N = sum(!is.na(x))
  )
)

Deskriptiv_Hypo_Kontrollvar <- do.call(data.frame, Deskriptiv_Hypo_Kontrollvar)

names(Deskriptiv_Hypo_Kontrollvar) <- c(
  "hypo", "mean", "sd", "median", "min", "max", "N"
)

MW_Diff_Hypo_Kontrollvar <- 
  Deskriptiv_Hypo_Kontrollvar$mean[Deskriptiv_Hypo_Kontrollvar$hypo == mit_hypo] -
  Deskriptiv_Hypo_Kontrollvar$mean[Deskriptiv_Hypo_Kontrollvar$hypo == ohne_hypo]

Deskriptiv_Hypo_Kontrollvar$MW_Diff_mit_minus_ohne <- MW_Diff_Hypo_Kontrollvar

Deskriptiv_Hypo_Kontrollvar



############################################################
# Absolute und relative Häufigkeiten der Einzelbewertungen
# getrennt nach Aspekt und Promptbedingung (Frage 2b)
############################################################

############################################################
# 1. Rating-Spalten definieren
############################################################

rating_spalten <- c(
  "Krit_Studiendesign_rating",
  "Studiendesign_rating",
  "Krit_Messinstr_rating",
  "Messinstrumente_rating",
  "Krit_Kontrollvar_rating",
  "Kontrollvariablen_rating"
)

############################################################
# 2. Daten ins Long-Format bringen
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
# 3. Promptbedingungen ebenfalls ins Long-Format bringen
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
# 4. Häufigkeiten berechnen
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
    rating = c(1, 2, 3),
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
# 5. Wide-Tabelle erstellen
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

Haeufigkeiten_Aspekt_Prompt_Wide # Abgleich mit Tabelle 6



###############################################################################################################################
# Tabelle: Deskriptive Kennwerte nach Promptbedingung, Studienplanungsaspekt und Bewertungsdimension (explorative Analyse)#####
###############################################################################################################################

############################################################
# Zuordnung der Rating-Variablen zu Aspekt und Dimension
############################################################

rating_info <- tribble(
  ~rating_variable,               ~Aspekt,              ~Bewertungsdimension,
  "Studiendesign_rating",         "Studiendesign",      "Angemessenheit",
  "Krit_Studiendesign_rating",    "Studiendesign",      "Entscheidungshilfe",
  "Messinstrumente_rating",       "Messinstrumente",    "Angemessenheit",
  "Krit_Messinstr_rating",        "Messinstrumente",    "Entscheidungshilfe",
  "Kontrollvariablen_rating",     "Kontrollvariablen",  "Angemessenheit",
  "Krit_Kontrollvar_rating",      "Kontrollvariablen",  "Entscheidungshilfe"
)

############################################################
# Daten ins Long-Format bringen
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
    
    Bedingung = case_when(
      tolower(as.character(Bedingung)) %in% c("yes", "ja", "1", "true") ~ "ja",
      tolower(as.character(Bedingung)) %in% c("no", "nein", "0", "false") ~ "nein",
      TRUE ~ as.character(Bedingung)
    ),
    
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
# Deskriptive Kennwerte berechnen
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

tabelle_vergleich # Abgleich mit Tabelle 8
View(tabelle_vergleich)
