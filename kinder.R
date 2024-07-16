# Install necessary packages (if not already installed)
if (!require("haven")) install.packages("haven")
if (!require("dplyr")) install.packages("dplyr")
if (!require("labelled")) install.packages("labelled")
if (!require("randomForest")) install.packages("randomForest")
if (!require("caret")) install.packages("caret")

# Load the necessary libraries
library(haven)
library(dplyr)
library(labelled)
library(randomForest)
library(caret)
library(foreign)
library(readxl)
library(Hmisc)
library(data.table)

# Load data
data <- read.spss("/Users/anilcaneldiven/Downloads/dji_suf_personen.sav", to.data.frame = TRUE)


# Add value labels (numlabel in Stata is for numeric labeling, labelled package helps in R)
# Not directly applicable in R, assume values are already labelled
data1 <- data %>%
  mutate(XALTER = as.numeric(as.character(XALTER)))

# Filter the data
data2 <- data1 %>%
  filter(IZP == "trifft zu" & XALTER < 12)

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


# Korrekte Rekodierung der Deprivationsvariablen von Text in numerische Werte
data5 <- data3 %>%
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
    dep_child = case_when(
      help == 0 ~ "Keine Deprivation",
      help >= 1 ~ "Deprivation"
    )
  ) %>%
  ungroup()


# Liste der zu entfernenden Variablen, die mit "dep_c" beginnen, aber nicht "dep_child"
remove_dep_c_vars <- names(data6)[grepl("^dep_c($|[^hild])", names(data6))]

# Gesamtliste der zu entfernenden Variablen erstellen
remove_vars <- unique(c(remove_dep_c_vars))

# Entfernen der hoch korrelierten Variablen und der "dep_c"-Variablen aus dem Datensatz
data6 <- data6 %>% select(-all_of(remove_vars))




# Nur Beobachtungen ohne fehlende Werte in den Deprivationsvariablen behalten
data7 <- data6 %>%
  filter(help2 == 0)

# Label the new variables

data7$dep_child <- factor(data7$dep_child, levels = c("Keine Deprivation", "Deprivation"))


# Lade die Excel-Liste
var_list <- read_excel("/Users/anilcaneldiven/Desktop/consulting/VarListePersonen.xlsx")

# Erstelle ein Wörterbuch (Named Vector) für die Spaltennamen-Umbenennung
colnames_mapping <- setNames(var_list$Label, var_list$`colnames(daten_personen)`)


# Ändere die Spaltennamen in data9 nur, wenn sie im Wörterbuch vorhanden sind
new_colnames <- colnames(data7)
for (i in seq_along(new_colnames)) {
  if (new_colnames[i] %in% names(colnames_mapping)) {
    new_colnames[i] <- colnames_mapping[new_colnames[i]]
  }
}

# Setze die neuen Spaltennamen in data9
colnames(data7) <- new_colnames


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
data9 <- clean_column_names(data7)

# Define the columns to be converted to numeric
columns_to_convert <- c("Alter_der_Person_in_Jahren_Mutter_",                     
                        "Alter_der_Person_in_Jahren_Vater_" ,
                        "vereinbarte_Arbeitszeit_Mutter_" ,                     
                        "vereinbarte_Arbeitszeit_Vater_" ,
                        "Trennungsjahr_der_Eltern_Mutter_",                        
                        "Trennungsjahr_der_Eltern_Vater_" ,                       
                        "Wohnentfernung_Elternteil_Mutter_" ,                      
                        "Wohnentfernung_Elternteil_Vater_" ,
                        "Anzahl_Beschäftigungsverhältnisse_Mutter_",               
                        "Anzahl_Beschäftigungsverhältnisse_Vater_",
                        "Zuwanderungsjahr_",
                        "tatsächliche_Arbeitszeit_Mutter_",
                        "tatsächliche_Arbeitszeit_Vater_")

# Convert the specified columns to numeric
data10 <- data9 %>%
  mutate(across(all_of(columns_to_convert), ~ as.numeric(as.character(.)))) %>%
  mutate(across(all_of(columns_to_convert), ~ ifelse(is.na(.), 0, .)))



# Assume data10 is your dataset filtered for children under 12

# Calculate the number of children per household
data12 <- data10 %>%
  group_by(HHLFD_) %>%
  mutate(
    Anzahl_Kinder = n()  # Number of children per household
  ) %>%
  ungroup()

# SIeht nicht notwendig aus Angst vor collider
# Create new variables relevant to children under 12
# data13 <- data12 %>%
#   mutate(
#     Durchschnittliches_Einkommen = rowMeans(select(., starts_with("Äquivalenzeinkommen_")), na.rm = TRUE),
#     Alter_Kategorie = case_when(
#       Alter_der_Person_in_Jahren_ < 6 ~ "Kleinkind",
#       Alter_der_Person_in_Jahren_ >= 6 & Alter_der_Person_in_Jahren_ < 12 ~ "Kind"
#     ),
#     Relative_Entfernung_ÖPNV = ifelse(`Entfernung_zur_nächsten_ÖPNV-Haltest._(in_Metern)_` < 500, "Nah", "Weit"),
#     Kinder_pro_Zimmer = Anzahl_Kinder / Anzahl_Kinderzimmer_,
#     Soziale_Unterstützung_Kategorie = case_when(
#       `OSLO-3_Skala_Soziale_Unterstützung_` >= 12 ~ "Hoch",
#       `OSLO-3_Skala_Soziale_Unterstützung_` >= 9 & `OSLO-3_Skala_Soziale_Unterstützung_` < 12 ~ "Mittel",
#       TRUE ~ "Niedrig"
#     )
#   )

# # Filter the data
# data13 <- data13 %>%
#   filter(Alter_Kategorie == "Kind")


###################

# lasse alle irrelevanten variablen raus
nzv <- nearZeroVar(data12, saveMetrics = TRUE)
data14 <- data12[, !nzv$nzv]


# Top-N Kategorien identifizieren
top_n <- 10
top_berufe_counts <- sort(table(trimws(data14$`Aktueller_oder_letzter_Beruf_Mutter_(ISCO-08)_`)), decreasing = TRUE)
top_berufe <- names(top_berufe_counts[1:top_n])

# Neue Spalte hinzufügen und Kategorien zuweisen
data14$`Aktueller_oder_letzter_Beruf_Mutter_(ISCO-08)_` <- trimws(data14$`Aktueller_oder_letzter_Beruf_Mutter_(ISCO-08)_`)
data14$TopKategorie <- ifelse(data14$`Aktueller_oder_letzter_Beruf_Mutter_(ISCO-08)_` %in% top_berufe, 
                              data14$`Aktueller_oder_letzter_Beruf_Mutter_(ISCO-08)_`, "Sonstige")


# Faktorspalte erstellen mit den dynamisch erstellten Labels
data14$TopKategorie_Mutter <- factor(data14$TopKategorie, levels = c(top_berufe, "Sonstige"))


## vater
# Top-N Kategorien identifizieren
top_berufe_counts <- sort(table(trimws(data14$`Aktueller_oder_letzter_Beruf_Vater_(ISCO-08)_`)), decreasing = TRUE)
top_berufe <- names(top_berufe_counts[1:top_n])

# Neue Spalte hinzufügen und Kategorien zuweisen
data14$`Aktueller_oder_letzter_Beruf_Vater_(ISCO-08)_` <- trimws(data14$`Aktueller_oder_letzter_Beruf_Vater_(ISCO-08)_`)
data14$TopKategorie <- ifelse(data14$`Aktueller_oder_letzter_Beruf_Vater_(ISCO-08)_` %in% top_berufe, 
                              data14$`Aktueller_oder_letzter_Beruf_Vater_(ISCO-08)_`, "Sonstige")

# Faktorspalte erstellen mit den dynamisch erstellten Labels
data14$TopKategorie_Vater <- factor(data14$TopKategorie, levels = c(top_berufe, "Sonstige"))

# raus mit den alten variablen
data_final <- data14 %>%
  select(!c(`Aktueller_oder_letzter_Beruf_Vater_(ISCO-08)_`,
            `Aktueller_oder_letzter_Beruf_Mutter_(ISCO-08)_`,
            TopKategorie,
            help))

# verwndle character zu factoren um
data_final_1 <- data_final %>%
  mutate(across(where(is.character), as.factor))

# muss zu faktoren werden
data_final_1$`Person_lebt_nicht_im_Haushalt.1`<- as.factor(data_final_1$`Person_lebt_nicht_im_Haushalt.1`)

# imputation function
replace_na_values <- function(df) {
  df %>%
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
    mutate(across(where(is.factor), ~ ifelse(is.na(.), "unbekannt", .))) %>%
    mutate(across(where(is.character), ~ ifelse(is.na(.), "unbekannt", .)))
}

data_final_2 <- replace_na_values(data_final_1)

# Function to clean column names to remove special characters
clean_names <- function(df) {
  colnames(df) <- make.names(colnames(df), unique = TRUE)
  return(df)
}

# Apply the function to clean column names
data_final_clean <- clean_names(data_final_2)

# verwndle character zu factoren um
data_final_clean_2 <- data_final_clean %>%
  mutate(across(where(is.character), as.factor))

# Ensure the response variable is a factor for classification
data_final_clean_2$dep_child <- as.factor(data_final_clean_2$dep_child)


### collider

# Function to calculate Cramer's V for categorical variables
cramersV <- function(x, y) {
  tbl <- table(x, y)
  chi2 <- chisq.test(tbl)$statistic
  n <- sum(tbl)
  phi2 <- chi2 / n
  k <- min(dim(tbl)) - 1
  return(sqrt(phi2 / k))
}

# Function to calculate correlations between numeric variables and dep_child
num_correlations <- function(data, dep_var) {
  cor_results <- data.frame(Variable = character(), Correlation = numeric(), stringsAsFactors = FALSE)
  numeric_vars <- data %>% select(where(is.numeric))
  
  for (var in colnames(numeric_vars)) {
    cor_value <- cor(numeric_vars[[var]], as.numeric(as.factor(data[[dep_var]])), use = "complete.obs")
    cor_results <- rbind(cor_results, data.frame(Variable = var, Correlation = cor_value, stringsAsFactors = FALSE))
  }
  
  return(cor_results)
}

# Function to calculate Cramer's V for categorical variables and dep_child
cat_correlations <- function(data, dep_var) {
  cat_results <- data.frame(Variable = character(), CramersV = numeric(), stringsAsFactors = FALSE)
  cat_vars <- data %>% select(where(is.factor))
  
  for (var in colnames(cat_vars)) {
    cramer_value <- cramersV(cat_vars[[var]], data[[dep_var]])
    cat_results <- rbind(cat_results, data.frame(Variable = var, CramersV = cramer_value, stringsAsFactors = FALSE))
  }
  
  return(cat_results)
}

# Calculate correlations
numeric_corrs <- num_correlations(data_final_clean_2, "dep_child")
categorical_corrs <- cat_correlations(data_final_clean_2, "dep_child")

# Combine and filter results to identify potential colliders
threshold <- 0.3

# Ensure both data frames have the same column names
numeric_corrs <- numeric_corrs %>% rename(Score = Correlation)
categorical_corrs <- categorical_corrs %>% rename(Score = CramersV)

potential_colliders <- rbind(
  numeric_corrs %>% filter(abs(Score) > threshold),
  categorical_corrs %>% filter(Score > threshold)
)


# Select the variables that are potential colliders
collider_vars <- potential_colliders$Variable
collider_vars <- setdiff(collider_vars, "dep_child")

# Remove collider variables from the dataset
data_final_clean_2_1 <- data_final_clean_2 %>% select(-all_of(collider_vars))


###

# Entfernen oder Ersetzen von NaN-Werten
data_final_clean_3 <- data_final_clean_2_1 %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), median(., na.rm = TRUE), .)))

# Entfernen oder Ersetzen von Inf-Werten
data_final_clean_4 <- data_final_clean_3 %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), median(., na.rm = TRUE), .)))
###########

remove_highly_correlated_categorical <- function(data, threshold = 0.05) {
  # Alle Spaltennamen des Datensatzes
  cols <- colnames(data)
  
  # Liste, um zu speichernde Spalten zu speichern
  to_keep <- cols
  
  # Doppelte Schleife, um Chi-Quadrat-Tests für alle Spaltenpaare durchzuführen
  for (i in 1:(length(cols) - 1)) {
    for (j in (i + 1):length(cols)) {
      var1 <- cols[i]
      var2 <- cols[j]
      
      # Kontingenztabelle erstellen
      contingency_table <- table(data[[var1]], data[[var2]])
      
      # Prüfen, ob die Kontingenztabelle gültig ist
      if (all(contingency_table > 0)) {
        # Chi-Quadrat-Test durchführen
        chi_test <- chisq.test(contingency_table)
        
        # P-Wert prüfen
        if (!is.na(chi_test$p.value) && chi_test$p.value < threshold) {
          # Hoch korrelierte Variable entfernen (zweite Variable wird entfernt)
          if (var2 %in% to_keep && var2 != "dep_child") {
            to_keep <- setdiff(to_keep, var2)
          }
        }
      }
    }
  }
  
  # Datensatz mit den verbleibenden Spalten zurückgeben
  return(data[to_keep])
}

# Beispielnutzung der Funktion
result <- remove_highly_correlated_categorical(data_final_clean_4)


#############################

remove_highly_correlated_numerical <- function(data, threshold = 0.9) {
  # Nur numerische Spalten auswählen
  num_data <- data[sapply(data, is.numeric)]
  
  # Korrelationsmatrix berechnen
  cor_matrix <- cor(num_data, use = "pairwise.complete.obs")
  
  # Eine Liste der zu entfernenden Spalten initialisieren
  to_remove <- c()
  
  # Doppelte Schleife, um die Korrelationsmatrix zu durchsuchen
  for (i in 1:(ncol(cor_matrix) - 1)) {
    for (j in (i + 1):ncol(cor_matrix)) {
      if (abs(cor_matrix[i, j]) > threshold) {
        var1 <- colnames(cor_matrix)[i]
        var2 <- colnames(cor_matrix)[j]
        
        # Eine der hoch korrelierten Variablen zur Entfernung markieren (z.B. die zweite Variable)
        if (!(var2 %in% to_remove) && var2 != "dep_child") {
          to_remove <- c(to_remove, var2)
        }
      }
    }
  }
  
  # Die verbleibenden Spalten zurückgeben
  return(data[, !(colnames(data) %in% to_remove)])
}

# Beispielnutzung der Funktion
result2 <- remove_highly_correlated_numerical(result)

########check for multikollinearity

##################

# Umwandlung der Faktoren in numerische Werte und Ersetzen von NAs
data <- replace_na_values(result2)
# Entfernen oder Ersetzen von NaN-Werten
data<- data%>% 
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), median(., na.rm = TRUE), .)))

# Entfernen oder Ersetzen von Inf-Werten
data <- data %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), median(., na.rm = TRUE), .)))

# Durchschnittliche Beziehungsqualität
data <- data %>%
  rowwise() %>%
  mutate(Durchschnitt_Beziehungsqualitaet = mean(as.numeric(c_across(starts_with("Qualität_Beziehung_"))), na.rm = TRUE)) %>%
  ungroup()

# Migrationshintergrund - hier fassen wir Geburtsland-Variablen zusammen
data <- data %>%
  mutate(Migrationshintergrund = ifelse(Geburtsland_Mutter_ == Geburtsland_Vater_, as.character(Geburtsland_Mutter_), "gemischt"))

# Familienklima-Index
data <- data %>%
  rowwise() %>%
  mutate(Familienklima_Index = mean(as.numeric(c_across(starts_with("Familienklima."))), na.rm = TRUE)) %>%
  ungroup()

# Entfernen der ursprünglichen Einzelvariablen, die zusammengefasst wurden
data <- data %>%
  select(-starts_with("Qualität_Beziehung_"), 
         -starts_with("Geburtsland_"),
         -starts_with("Familienklima."))


# Überprüfen auf Multikolinearität
vif_values <- vif(lm(as.numeric(dep_child) ~ ., data = data)) # 'dep_child' als abhängige Variable

table <- data.frame(vif_values)


# Filtern der Zeilen mit GVIF^(1/(2*Df)) > 2
filtered_table <- table %>% filter(`GVIF..1..2.Df..` > 5)

# Hinzufügen der Rownames als eine eigene Spalte
filtered_table  <- filtered_table  %>%
  tibble::rownames_to_column(var = "Variable")


data <- data %>% select(-all_of(filtered_table$Variable))

### random forest
# Set seed for reproducibility
set.seed(123)

data$HHLFD_ <- NULL
data$dep_child <- as.factor(data$dep_child)

# data <- data %>%
#   filter(Alter_der_Person_in_Jahren_ > 6 & Alter_der_Person_in_Jahren_ < 12)

# Split the data into training and testing sets
trainIndex <- createDataPartition(data$dep_child, p = .8,
                                  list = FALSE,
                                  times = 1)

data_train <- data[trainIndex, ]
data_test <- data[-trainIndex, ]

# Train the Random Forest model
#getuned HPO
rf_model <- randomForest(dep_child ~ ., data = data_train,
                         importance = TRUE,
                         ntree = 100,
                         mtry = 20,
                         nodesize = 1)

# Vorhersagen und Bewertung
predicted_classes <- predict(rf_model, data_test)
conf_matrix <- confusionMatrix(predicted_classes, data_test$dep_child)
print(conf_matrix)

# Plot der Variable Importance
varImpPlot(rf_model)


