############################################################
# Bachelorarbeit – Analyse der LLM-Outputs
# Autorin: Juno Meyer
# Datum: 10.08.2026
# Hinweis: dieses Skript wurde mit GPT-5.5 erstellt und durch die Autorin geprüft
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

tab_kodierungen_dimension # Vergleich Tabelle 1 im Bericht


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
    .groups = "drop"
  ) %>%
  arrange(Aspekt)

Deskriptiv_Gesamt # Vergleich Tabelle 2 im Bericht


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

Haeufigkeiten_Aspekte_Wide # Vergleich Tabelle 1 im Bericht (Gesamt pro Aspekt)


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

Deskriptiv_Prompt_Gesamt # Vergleich Tabelle 3 im Bericht


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

Haeufigkeiten_Prompt_Gesamt # Vergleich Tabelle 4 im Bericht


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

Deskriptiv_Aspekt_Prompt # Vergleich Tabelle 5 im Bericht


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

Haeufigkeiten_Aspekt_Prompt_Wide # Vergleich Tabelle 6 im Bericht


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

Tabelle_Explorativ_Dimension # vergleich Tabelle 7 im Bericht 


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

Haeufigkeiten_Dimension_Prompt_Wide # noch keine vergleichbare Tabelle im Bericht 


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


sink(file.path("results", "sessionInfo.txt"))
print(sessionInfo())
sink()


############################################################
# Ende des Skripts
############################################################