# Install necessary packages (if not already installed)
if (!require("haven")) install.packages("haven")
if (!require("dplyr")) install.packages("dplyr")
if (!require("labelled")) install.packages("labelled")

# Load the necessary libraries
library(haven)
library(dplyr)
library(labelled)

# Load data
data <- read.spss("/Users/anilcaneldiven/Downloads/dji_suf_personen.sav", to.data.frame = TRUE)

# Add value labels (numlabel in Stata is for numeric labeling, labelled package helps in R)
# Not directly applicable in R, assume values are already labelled
data1 <- data %>%
  mutate(XALTER = as.numeric(as.character(XALTER)))

# Filter the data
data2 <- data1 %>%
  filter(IZP == "trifft zu" & XALTER < 12)

#h71310_mu, h71310_va,h11021_mu, h11021_va
# Select relevant variables
data3 <- data2 %>%
  select(HHLFD, INTNR, GESCHLECHT, XALTER, h11008, h11008_mu, h11008_va, h11008_omamu, h11008_opamu, h11008_omava, h11008_opava,
         h91102, h91107, h91109, h91110, h91061, h91113, h91114, h71338, h71332, h71350, h51117, h35854, h35855, h35856, h35857,
         h35017_1, h35017_2, h35017_3, h35017_4, h35017_5, h35017_6, h35017_7, h33106_1, h33106_2, h33106_3, h33106_4, h33106_5,
         h33106_6, h33117_1, h33117_2, h33117_3, h33117_4, h33117_5, h33117_6, h33109_1, h33109_2, h33109_3, h33109_4, h33120, 
         h33121, h33212_1, h33212_2, h33212_3, h33212_4, h33212_5, h33212_6, h33212_7, h33213, h22803_1, h22803_2, h22803_3, 
         h22803_4, h22803_5, h22803_6, h22803_7, h22803_8, h22803_9, h22803_10, h22803_11, h22803_12, h22803_13, h22803_14,
         v62022_1, v62023_1, v62022_2, v62023_2, h79859_1, h79859_2, h79859_3, h79859_4, k_sorgerecht_1, k_sorgerecht_2, k_natio,
         k_NEPSmig, k_hkland, k_AnzETHH, m_Anzahl_Erz_Kind, k_aktiv, k_muttersprache1, k_muttersprache2, k_othersprache1, k_othersprache2,
         k_othersprache3, k_othersprache4, k_othersprache5, k_othersprache6, k_migra_sprache, hh11001, hh11066, hh11067, hh11070_1, 
         hh11070_2, hh11070_3, hh11073, hh11075, hh11078, hh11079, k_haushaltstyp_hh, k_familientyp_hh, k_aequi_min_hh, k_aequi_max_hh,
         k_aequi_mean_hh, k_arm60p_hh, k_OSSsum_hh, k_OSScat_hh, k_deprivationsindex_hh, gkbik10, polgk, bland, casa_typ, casa_hh,
         casa_dist_opnv, h11014_mu, h11014_va, h11015_mu, h11015_va, h11018_mu, h11018_va, h11019_mu, h11019_va, h11021_mu, h11021_va,
         h11022_mu, h11022_va, h11024_mu, h11024_va, h11025_mu, h11025_va, h11026_mu, h11026_va, h71020_mu, h71020_va, h71305_mu,
         h71305_va, h71307_mu, h71308_mu, h71308_va, h71310_mu, h71310_va, h71312_mu, h71312_va, h71314_mu, h71317_mu, h71317_va,
         h71318_mu, h71318_va, h71319_mu, h71319_va, h71321_mu, h71321_va, h71323_mu, h71323_va, h71327_mu, h71327_va, h71328_mu,
         h71328_va, h71332_mu, h71332_va, h71338_mu, h71338_va, h71401_mu, h71401_va, h71404_mu, h71404_va, h91061_mu, h91061_va,
         h91102_mu, h91102_va, h91114_mu, h91114_va, h91115_1_mu, h91115_1_va, h91115_2_mu, h91115_2_va, h91115_3_mu, h91115_3_va,
         h91115_4_mu, h91115_4_va, h91115_5_mu, h91115_5_va, h91115_6_mu, h91115_6_va, h91115_7_mu, h91115_7_va, h91115_8_mu, 
         h91115_8_va, h91115_9_mu, h91115_9_va, h91115_10_mu, h91115_10_va, h91115_11_mu, h91115_11_va, k_bija_mu, k_bija_va, 
         k_HatPartner_mu, k_HatPartner_va, k_isced2011_mu, k_isced2011_va, k_natio_mu, k_natio_va, k_muttersprache1_mu, 
         k_muttersprache1_va, k_muttersprache2_mu, k_muttersprache2_va, XALTER_mu, XALTER_va, k_extern_mu, k_extern_va, 
         k_ZP_mit_Eltern, k_isco08_mu3, k_isco08_va3, k_eseg_2_mu, k_eseg_2_va)

# Child deprivation
# Drop previous deprivation variables if they exist
data4 <- data3 %>% select(-starts_with("dep_c_"))

# Korrekte Rekodierung der Deprivationsvariablen von Text in numerische Werte
data5 <- data4 %>%
  mutate(
    dep_c_three_meals = if_else(h22803_1 == "Ja" | h22803_1 == "Nein aus anderen Gründen", 0, if_else(h22803_1 == "Nein aus finanziellen Gründen", 1, NA_real_)),
    dep_c_quality_meal = if_else(h22803_2 == "Ja" | h22803_2 == "Nein aus anderen Gründen", 0, if_else(h22803_2 == "Nein aus finanziellen Gründen", 1, NA_real_)),
    dep_c_fruits = if_else(h22803_3 == "Ja" | h22803_3 == "Nein aus anderen Gründen", 0, if_else(h22803_3 == "Nein aus finanziellen Gründen", 1, NA_real_)),
    dep_c_books = if_else(h22803_4 == "Ja" | h22803_4 == "Nein aus anderen Gründen", 0, if_else(h22803_4 == "Nein aus finanziellen Gründen", 1, NA_real_)),
    dep_c_outdoor = if_else(h22803_5 == "Ja" | h22803_5 == "Nein aus anderen Gründen", 0, if_else(h22803_5 == "Nein aus finanziellen Gründen", 1, NA_real_)),
    dep_c_leisure = if_else(h22803_6 == "Ja" | h22803_6 == "Nein aus anderen Gründen", 0, if_else(h22803_6 == "Nein aus finanziellen Gründen", 1, NA_real_)),
    dep_c_indoor = if_else(h22803_7 == "Ja" | h22803_7 == "Nein aus anderen Gründen", 0, if_else(h22803_7 == "Nein aus finanziellen Gründen", 1, NA_real_)),
    dep_c_clothes = if_else(h22803_10 == "Ja" | h22803_10 == "Nein aus anderen Gründen", 0, if_else(h22803_10 == "Nein aus finanziellen Gründen", 1, NA_real_)),
    dep_c_shoes = if_else(h22803_11 == "Ja" | h22803_11 == "Nein aus anderen Gründen", 0, if_else(h22803_11 == "Nein aus finanziellen Gründen", 1, NA_real_)),
    dep_c_meet = if_else(h22803_12 == "Ja" | h22803_12 == "Nein aus anderen Gründen", 0, if_else(h22803_12 == "Nein aus finanziellen Gründen", 1, NA_real_)),
    dep_c_celeb = if_else(h22803_13 == "Ja" | h22803_13 == "Nein aus anderen Gründen", 0, if_else(h22803_13 == "Nein aus finanziellen Gründen", 1, NA_real_)),
    dep_c_holiday = if_else(h22803_14 == "Ja" | h22803_14 == "Nein aus anderen Gründen", 0, if_else(h22803_14 == "Nein aus finanziellen Gründen", 1, NA_real_))
  )



# Erstellen der child specific material deprivation Variablen
data6 <- data5 %>%
  rowwise() %>%
  mutate(
    help = sum(c_across(starts_with("dep_c_")), na.rm = TRUE),
    help2 = sum(is.na(c_across(starts_with("dep_c_")))),
    dep_child = if_else(help > 0, 1, 0),
    dep_child2 = if_else(help > 1, 1, 0)
  ) %>%
  ungroup()

# Nur Beobachtungen ohne fehlende Werte in den Deprivationsvariablen behalten
data7 <- data6 %>%
  filter(help2 == 0)

# Label the new variables
data8 <- data7 %>%
  mutate(
    dep_child = factor(dep_child, levels = c(0, 1), labels = c("No", "Yes")),
    dep_child2 = factor(dep_child2, levels = c(0, 1), labels = c("No", "Yes"))
  )

nzv <- nearZeroVar(data8, saveMetrics = TRUE)
data9 <- data8[, !nzv$nzv]

length(colnames(data9))
length(colnames(data8))

##########################

# Lade die Excel-Liste
var_list <- read_excel("/Users/anilcaneldiven/Desktop/consulting/VarListePersonen.xlsx")
colnames(var_list)
# Erstelle ein Wörterbuch (Named Vector) für die Spaltennamen-Umbenennung
colnames_mapping <- setNames(var_list$Label, var_list$`colnames(daten_personen)`)


# Ändere die Spaltennamen in data9 nur, wenn sie im Wörterbuch vorhanden sind
new_colnames <- colnames(data9)
for (i in seq_along(new_colnames)) {
  if (new_colnames[i] %in% names(colnames_mapping)) {
    new_colnames[i] <- colnames_mapping[new_colnames[i]]
  }
}

# Setze die neuen Spaltennamen in data9
colnames(data9) <- new_colnames
length(colnames(data10))

################

# Funktion zur Überprüfung und Bereinigung von doppelten Spaltennamen
clean_column_names <- function(df) {
  # Identifizieren von doppelten Spaltennamen
  duplicated_names <- names(df)[duplicated(names(df))]
  
  if(length(duplicated_names) > 0) {
    # Erstellen eindeutiger Namen für doppelte Spalten
    names(df) <- make.unique(names(df))
    message("Doppelte Spaltennamen wurden bereinigt.")
  } else {
    message("Keine doppelten Spaltennamen gefunden.")
  }
  
  return(df)
}

# Bereinigen von doppelten Spaltennamen in data9
data10 <- clean_column_names(data9)

# Funktion zur spezifischen Behandlung und Aggregierung jeder Variable
aggregate_specific <- function(df, household_id_col) {
  df %>%
    group_by(across(all_of(household_id_col))) %>%
    summarize(
      # Numerische Variablen: Mittelwert
      across(where(is.numeric), ~mean(.x, na.rm = TRUE), .names = "mean_{.col}"),
      
      # Kategoriale Variablen: Häufigster Wert (Modus)
      across(where(is.factor), ~names(which.max(table(.x))), .names = "mode_{.col}"),
      
      .groups = 'drop'
    )
}

# Setzen der Haushalts-ID Spalte
household_id_col <- "HHLFD_"

# Anwenden der Funktion auf den DataFrame data9
data_aggregated <- aggregate_specific(data10, household_id_col)

data_aggregated$mean_Eindeutige_Personennummer_ <- NULL
