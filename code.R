library(foreign)
library(readr)
library(dplyr)
library(randomForest)
library(forcats)
library(corrplot)
library(vcd)
library(ggplot2)
library(reshape2)
library(lubridate)
library(tidyverse)

######################
# Datenverarbeitung #
####################

########### 1 DatenBeziehungen

# Lese den Datensatz ein
daten_beziehungen <- read.spss("/Users/anilcaneldiven/Downloads/dji_suf_beziehungen.sav", to.data.frame = TRUE)
View(daten_beziehungen)
nrow(daten_beziehungen)

# Umbenennen der Variablen
daten_beziehungen <- daten_beziehungen %>%
  rename(
    Matchvariable_HHLFD = HHLFD,
    Lfd1 = INTNR1,
    Lfd2 = INTNR2,
    Relationship_Type = RELATION,
    Original_Pair_Relationship = RELATION_ori,
    Pair_Relationship = PAIRRELATION,
    Pair_Relationship_ori = PAIRRELATION_ori,
    m_hhmitzp = m_hhmitzp,
    Grandparent_Type = GrosselternArt,
    Target_Person1 = IZP1,
    Gender1 = GESCHLECHT1,
    Target_Person2 = IZP2,
    Gender2 = GESCHLECHT2,
    Non_Household_Person1 = extern1,
    Non_Household_Person2 = extern2
  )


lookup <- c("Pair_Relationship","Pair_Relationship_ori")

# Annahme, dass die Daten geladen sind und 'daten_beziehungen' zur Verfügung steht
daten_beziehungen <- daten_beziehungen%>%
  select(-all_of(lookup))%>%
  mutate(
    # Konvertiere Faktoren zu numerischen Werten, vorausgesetzt die Werte sind gültige Zahlen
    Non_Household_Person1 = as.numeric(as.character(Non_Household_Person1)),
    Non_Household_Person2 = as.numeric(as.character(Non_Household_Person2))
  ) %>%
  mutate(
    Same_Household = ifelse(Non_Household_Person1 == 0 & Non_Household_Person2 == 0, "Yes", "No"),
    #External_Relationship_Interaction = interaction(Non_Household_Person1, Relationship_Type),
    Is_Parent_Child = ifelse(
      Relationship_Type %in% c(
        "Leibliche Mutter/leiblicher Vater",
        "Stiefmutter/Stiefvater",
        "Adoptivmutter/Adoptivvater",
        "Pflegemutter/Pflegevater",
        "leibliche Tochter/leiblicher Sohn",
        "Stieftochter/Stiefsohn",
        "Adoptivtochter/Adoptivsohn",
        "Pflegetochter/Pflegesohn"
      ), 1, 0
    ),
    Grandparent_Present = ifelse(Grandparent_Type %in% c("väterlicherseits", "mütterlicherseits"), 1, 0)
  )

# Aggregation mit korrekter numerischer Verarbeitung
aggregated_relationship_data <- daten_beziehungen %>%
  group_by(Matchvariable_HHLFD) %>%
  summarise(
    Total_Relationships = n(),
    Num_Same_Household = sum(Same_Household == "Yes", na.rm = TRUE),
    Num_Different_Household = sum(Same_Household == "No", na.rm = TRUE),
    Num_Parent_Child_Relationships = sum(Is_Parent_Child, na.rm = TRUE),
    Num_Grandparent_Present = max(Grandparent_Present, na.rm = TRUE),
    #External_Relationships = list(unique(External_Relationship_Interaction)),
    Num_External_Relationships = sum(Non_Household_Person1 > 0 | Non_Household_Person2 > 0, na.rm = TRUE)
  )

########### 2 DatenHaushalt
# Lese den Datensatz ein
daten_haushalt <- read.spss("/Users/anilcaneldiven/Downloads/dji_suf_haushalt.sav", to.data.frame = TRUE)

relevant_variables_HH <- c("hh11900",
                          "hh11901",
                           "hh11902",
                           "point",
                           "telja",
                           "oa_result",
                           "nationn",
                           "INTNR_hh",
                           "ANZPHH_hh",
                           "LMETH_hh",
                           "CATI_SW_hh",
                           "TRANCHE_hh",
                           "vollstaendig_hh",
                           "TRACK_hh",
                           "t_forc_hh",
                           "ch_tele_hh",
                           "ch_email_hh",
                           "ch_post_hh",
                           "telja_hh",
                           "FO_Dat_hh",
                           "FORC_hh",
                           "INTERNR_hh",
                           "foreal_hh",
                           "verinn1_hh",
                           "verinn2_hh",
                          "vkonv_hh",
                           "cati_vk_hh",
                           "panelbr_hh",
                           "GewHH",
                           "calHH",
                           "GemID",
                           "m_intyear_hh",
                           "m_intmonth_hh")

# VARIABELN rausselektieren aus HH
daten_haushalt <- daten_haushalt %>%
  select(-all_of(relevant_variables_HH))

# Umbenennen der Variablen
daten_haushalt <- daten_haushalt %>%
  rename(
    Matchvariable_HHLFD = HHLFD,
    Reverse_Count_Target_Persons = z_gew,
    Scaled_Reverse_Count_Target_Persons = z_gew2,
    Number_of_People_in_Household = hh11001,
    All_Persons_at_Primary_Residence = hh11076,
    Support_for_Personal_Problems = hh11058,
    Care_and_Interest = hh11059,
    Practical_Help = hh11060,
    Number_of_Children_Rooms = hh11066,
    Care_Dependent_Persons_in_Household = hh11067,
    Monthly_Saving = hh11070_1,
    Replace_Furniture = hh11070_2,
    Pay_for_Unexpected_Expenses = hh11070_3,
    Receipt_of_ALG_II_or_Hartz_IV = hh11073,
    Household_Net_Income = hh11075,
    External_Children = hh11078,
    Number_of_External_Children = hh11079,
    m_hhmitzp = m_hhmitzp,
    Household_Type = k_haushaltstyp_hh,
    Family_Type = k_familientyp_hh,
    Lower_Limit_Equivalent_Income = k_aequi_min_hh,
    Upper_Limit_Equivalent_Income = k_aequi_max_hh,
    Midpoint_Equivalent_Income = k_aequi_mean_hh,
    Below_60_Percent_Median_Income = k_arm60p_hh,
    OSLO_3_Social_Support_Scale = k_OSSsum_hh,
    Categorical_OSLO_3_Support = k_OSScat_hh,
    Financial_Deprivation_Index = k_deprivationsindex_hh,
    BIK_GK_10 = gkbik10,
    Political_Community_Size_Class = polgk,
    BLand = bland,
    Birth_Year = gebjahr_brutto,
    Gender_Recorded = sex_brutto,
    Tranche = TRANCHE,
    Building_Type = casa_typ,
    Number_of_Private_Households = casa_hh,
    Distance_to_Nearest_Public_Transport = casa_dist_opnv,
    Total_Contact_Count_Household_Level = anzkonHH2_hh,
    All_Contacts_in_Household_Incl_Person_Contact = anzkonHH3_hh
  )


# Variablen, die entfernt werden sollen
unwanted_vars2 <- c("Interviewer_Filled_Silently",
                    "Save_Address",
                    "Address_Is_Correct",
                    "m_hhmitzp",
                    "BLand",
                    "Nationality_Recoded",
                    "Interview_Number",
                    "CATI_Switch",
                    "TRANCHE_hh",
                    "Household_Fully_Interviewed",
                    "Household_Tracking_Level",
                    "Total_Tracking_Final_Outcome",
                    "telja_hh",
                    "Final_Outcome_Date",
                    "Final_Outcome_Households",
                    "Household_Ready_for_Panel",
                    "Contact_Count_Household_Level",
                    "Total_Contact_Count_Household_Level",
                    "All_Contacts_in_Household_Incl_Person_Contact",
                    "Household_Weight_Without_Calibration",
                    "Calibrated_Household_Weight",
                    "Community_ID",  "Tranche", 
                    "Point_Number_Drawn", 
                    "Interviewer_Number_Final_Outcome", 
                    "Phone_Number_Found", 
                    "New_Phone_Number", 
                    "New_Email_Address", 
                    "New_Address", 
                    "Valid_Cases_Realized", 
                    "First_Reminder_Sent", 
                    "Second_Reminder_Sent", 
                    "Conversion_Letter_Sent", 
                    "Pre_Contact_CATI")


# Entfernen der weniger relevanten Variablen
daten_haushalt <- daten_haushalt %>%
  select(-all_of(unwanted_vars2))

daten_haushalt <- daten_haushalt %>%
  mutate(Birth_Year = as.character(Birth_Year),  # Ensure Birth_Year is character for replacement
         Birth_Year = ifelse(Birth_Year == "keine Angabe", NA, Birth_Year),
         Birth_Year = as.numeric(Birth_Year))  # Convert to numeric safely

daten_haushalt <- daten_haushalt %>%
  mutate(Age = ifelse(!is.na(Birth_Year),
                      as.numeric(format(Sys.Date(), "%Y")) - Birth_Year,
                      NA_real_))  # Handle NA to prevent NAs from coercion

# Definiere eine Funktion zur Imputation der numerischen Variablen
impute_numeric <- function(x) {
  ifelse(is.na(x), median(x, na.rm = TRUE), x)
}

# Funktion zur Bestimmung des Modus
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Definieren Sie eine Funktion zur Imputation der kategorialen Variablen
impute_categorical <- function(x) {
  ifelse(is.na(x), as.character(getmode(x)), as.character(x))
}


daten_haushalt <- daten_haushalt %>%
  mutate(across(where(is.factor), ~ fct_explicit_na(.x, na_level = "Unbekannt"))) %>%
  mutate(
    Monthly_Saving = ifelse(is.na(Monthly_Saving), median(as.numeric(as.character(Monthly_Saving)), na.rm = TRUE), Monthly_Saving),
    Number_of_Children_Rooms = ifelse(is.na(Number_of_Children_Rooms), 0, Number_of_Children_Rooms),
    Household_Net_Income = ifelse(is.na(Household_Net_Income), mean(as.numeric(as.character(Household_Net_Income)), na.rm = TRUE), Household_Net_Income),
    Number_of_People_in_Household = ifelse(is.na(Number_of_People_in_Household), median(Number_of_People_in_Household, na.rm = TRUE), Number_of_People_in_Household),
    External_Children = replace_na(External_Children, "nein")
  ) %>%
  filter(as.numeric(as.character(Birth_Year)) >= 1900 & as.numeric(as.character(Birth_Year)) <= as.numeric(format(Sys.Date(), "%Y"))) %>%
  mutate(Age = as.numeric(format(Sys.Date(), "%Y")) - as.numeric(as.character(Birth_Year))) %>%
  filter(Number_of_People_in_Household > 0) %>%
  mutate(across(where(is.numeric), ~ impute_numeric(.x))) %>%
  mutate(across(where(is.factor), ~ impute_categorical(.x)))

# # Überprüfen, ob alle NAs beseitigt wurden
# sapply(daten_haushalt, function(x) sum(is.na(x)))


#NEUE variablenkreation
daten_haushalt <- daten_haushalt %>%
mutate(
# Finanzstabilität und Ressourcen
Savings_Income_Ratio = Monthly_Saving / Household_Net_Income,
Unexpected_Expense_Coverage = ifelse(Pay_for_Unexpected_Expenses == "ja", 1, 0),

# Soziale Unterstützung und Umfeld
Normalized_Social_Support = OSLO_3_Social_Support_Scale / Number_of_People_in_Household,
Good_Access_Public_Transport = ifelse(Distance_to_Nearest_Public_Transport <= 500, 1, 0),

# Haushaltsstruktur und Lebensbedingungen
Child_Room_Ratio = Number_of_Children_Rooms / Number_of_People_in_Household,
Dependent_Care_Ratio = ifelse(Care_Dependent_Persons_in_Household == "ja", 1, 0) / Number_of_People_in_Household,

#Deprivation Risk Index
Deprivation_Risk_Index = (1 - Savings_Income_Ratio) + (1 - Unexpected_Expense_Coverage) + (1 - Normalized_Social_Support),
)


#####################################
# Datensätze zusammenführen
daten <- merge(aggregated_relationship_data, daten_haushalt, by = "Matchvariable_HHLFD", all = TRUE)
#sapply(daten, function(x) sum(is.na(x)))

daten <- daten %>%
  mutate(
    # Erstellen von Indikatoren für jede Deprivationsart
    sparen_depriviert = ifelse(Monthly_Saving == "nein, aus finanziellen Gründen nicht", 1, 0),
    moebel_depriviert = ifelse(Replace_Furniture == "nein, aus finanziellen Gründen nicht", 1, 0),
    ausgaben_depriviert = ifelse(Pay_for_Unexpected_Expenses == "nein, aus finanziellen Gründen nicht", 1, 0),
    alg2_depriviert = ifelse(Receipt_of_ALG_II_or_Hartz_IV == "ja", 1, 0),
    einkommen_depriviert = ifelse(Below_60_Percent_Median_Income == "ja", 1, 0),
    
    # Berechnen eines Gesamtdeprivationsindex
    gesamt_depriviert = sparen_depriviert + moebel_depriviert + ausgaben_depriviert + alg2_depriviert + einkommen_depriviert
     )

# Variablen, die entfernt werden sollen
unwanted_vars3 <- c("sparen_depriviert",
                    "moebel_depriviert",
                    "ausgaben_depriviert",
                    "alg2_depriviert",
                    "einkommen_depriviert")


# Entfernen der weniger relevanten Variablen
daten <- daten %>%
  select(-all_of(unwanted_vars3))

# Sicherstellen, dass alle kategorialen Variablen als Faktoren behandelt werden
daten <- daten %>%
  mutate_if(is.character, as.factor)

daten <- daten %>%
  mutate(
    # Sozial- und Familienstruktur
    Social_Structure_Index = Num_Parent_Child_Relationships + Num_Grandparent_Present,
    Total_Dependents = Num_Parent_Child_Relationships + Num_Grandparent_Present,  # Addieren Sie weitere abhängige Personen, falls vorhanden

    # Wirtschaftlicher Druck
    Economic_Strain_Index = as.integer(Monthly_Saving == "nein, aus finanziellen Gründen nicht") +
      as.integer(Replace_Furniture == "nein, aus finanziellen Gründen nicht") +
      as.integer(Pay_for_Unexpected_Expenses == "nein, aus finanziellen Gründen nicht"),

    Dependency_Ratio = (Num_Parent_Child_Relationships + Num_Grandparent_Present) / Number_of_People_in_Household,
    Income_to_needs_ratio = Household_Net_Income / (Number_of_People_in_Household * Lower_Limit_Equivalent_Income),
    # Ökonomische Indikatoren
    Is_Receiving_ALG_II = as.integer(Receipt_of_ALG_II_or_Hartz_IV == "ja"),
    Is_Below_Poverty_Line = as.integer(Below_60_Percent_Median_Income == "ja"),

    # Komplexerer Index könnte weitere Variablen umfassen
    Financial_Security_Index = Economic_Strain_Index - Is_Receiving_ALG_II - Is_Below_Poverty_Line,
    # Erstellen der Deprivation Outcome Variable
    deprivation_outcome = ifelse(gesamt_depriviert >= 3, 1, 0) # Als depriviert gilt, wer bei 3 oder mehr Kriterien betroffen ist 
    
    )


# Überprüfen, ob 'deprivation_outcome' als Faktor behandelt wird

daten <- daten %>%
  drop_na()

# Entfernen oder Ersetzen von NaN-Werten
daten <- daten %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), median(., na.rm = TRUE), .)))

# Entfernen oder Ersetzen von Inf-Werten
daten <- daten %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), median(., na.rm = TRUE), .)))

if (!is.factor(daten$deprivation_outcome)) {
  daten$deprivation_outcome <- factor(daten$deprivation_outcome)
}
############# Modell

levels(daten$deprivation_outcome)
levels(daten$deprivation_outcome) <- make.names(levels(daten$deprivation_outcome))

# Auswahl der Features für das Random Forest Modell
features <- c("Num_Same_Household", "Num_Different_Household", "Num_Parent_Child_Relationships",
              "Num_Grandparent_Present", "Num_External_Relationships", "Number_of_People_in_Household",
              "Support_for_Personal_Problems", "Care_and_Interest", "Practical_Help",
              "Number_of_Children_Rooms", "Care_Dependent_Persons_in_Household", "Monthly_Saving",
              "Replace_Furniture", "Pay_for_Unexpected_Expenses", "Household_Net_Income",
              "OSLO_3_Social_Support_Scale", "Distance_to_Nearest_Public_Transport",
              "Household_Level_Person_Count", "Age", "Savings_Income_Ratio", "Unexpected_Expense_Coverage",
              "Normalized_Social_Support", "Good_Access_Public_Transport", "Child_Room_Ratio",
              "Dependent_Care_Ratio", "Deprivation_Risk_Index")


# Zielvariable
target <- "deprivation_outcome"

# Daten vorbereiten
daten_model <- daten[, c(features, target)]

# Entfernen von fehlenden Werten
daten_model <- na.omit(daten_model)
# Aktualisieren Sie die Trainingskontrolle, um Vorhersagen und Resampling-Ergebnisse zu speichern
fitControl <- trainControl(
  method = "cv",  # Kreuzvalidierung
  number = 10,    # Anzahl der Kreuzvalidierungsfalten
  savePredictions = "final",  # Speichern der Vorhersagen für jeden Fold
  returnResamp = "all"  # Speichert Resampling-Statistiken für jeden Fold
)

# Annahme daten_model ist bereits definiert und enthält die notwendigen Daten
trainIndex <- createDataPartition(daten_model$deprivation_outcome, p = .7, 
                                  list = FALSE, 
                                  times = 1)
trainData <- daten_model[trainIndex, ]
testData <- daten_model[-trainIndex, ]

# Training des Modells
trainModel <- train(deprivation_outcome ~ ., data = trainData,
                    method = "rf",
                    trControl = fitControl,
                    ntree = 100)

# Plot der Modell-Performance
results <- trainModel$results


# Erstellen eines Plots für die Genauigkeit jeder Kreuzvalidierungsfalte
ggplot(trainModel$results, aes(x = mtry, y = Accuracy)) + 
  geom_point() +  # Punkte für die tatsächlichen Datenpunkte jeder Faltung
  geom_line() +  # Linie zur Verbindung der Punkte
  geom_errorbar(aes(ymin = Accuracy - AccuracySD, ymax = Accuracy + AccuracySD), width = 0.2) + # Fehlerbalken hinzufügen
  ggtitle("Genauigkeit über die verschiedenen mtry Werte") +
  xlab("Anzahl der Variablen pro Split (mtry)") +
  ylab("Genauigkeit mit Standardabweichung")


# Vorhersagen auf den Testdaten
predictions <- predict(trainModel, testData)

# Bewertung der Vorhersagen
confusionMatrix <- confusionMatrix(predictions, testData$deprivation_outcome)
print(confusionMatrix)

# Berechnung der Genauigkeit aus der Konfusionsmatrix
accuracy <- sum(diag(confusionMatrix$table)) / sum(confusionMatrix$table)
print(paste("Accuracy:", accuracy))


# Berechnung weiterer Bewertungsmaße
print(confusionMatrix$overall)  # Gibt eine Liste von Bewertungsmaßen aus, einschließlich Genauigkeit, Kappa usw.

# Wichtigkeit der Variablen aus dem Random Forest-Modell
importance <- varImp(trainModel, scale = TRUE)
print(importance)
plot(importance, main = "Variable Importance")

#####################reduce model again

# Erstellen eines neuen Datensatzes ohne die ausgewählten Variablen aus korrelationsanalyse
reduced_data <- trainData[, !names(trainData) %in% c("Number_of_People_in_Household", "Child_Room_Ratio", "Deprivation_Risk_Index")]

# Modell neu trainieren
reduced_model <- train(deprivation_outcome ~ ., data = reduced_data, method = "rf", trControl = fitControl)

# Leistung des neuen Modells vergleichen
print(reduced_model$results)
colnames(reduced_data)
# Wichtigkeit der Variablen aus dem Random Forest-Modell
importance <- varImp(reduced_model, scale = TRUE)
print(importance)





