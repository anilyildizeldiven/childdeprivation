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

# Annahme, dass die Daten geladen sind und 'daten_beziehungen' zur Verfügung steht
daten_beziehungen <- daten_beziehungen %>%
  mutate(
    # Konvertiere Faktoren zu numerischen Werten, vorausgesetzt die Werte sind gültige Zahlen
    Non_Household_Person1 = as.numeric(as.character(Non_Household_Person1)),
    Non_Household_Person2 = as.numeric(as.character(Non_Household_Person2))
  ) %>%
  mutate(
    Same_Household = ifelse(Non_Household_Person1 == 0 & Non_Household_Person2 == 0, "Yes", "No"),
    External_Relationship_Interaction = interaction(Non_Household_Person1, Relationship_Type),
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
    External_Relationships = list(unique(External_Relationship_Interaction)),
    Num_External_Relationships = sum(Non_Household_Person1 > 0 | Non_Household_Person2 > 0, na.rm = TRUE)
  )


View(aggregated_relationship_data)


########### 2 DatenHaushalt
# Lese den Datensatz ein
daten_haushalt <- read.spss("/Users/anilcaneldiven/Downloads/dji_suf_haushalt.sav", to.data.frame = TRUE)

str(daten_haushalt)

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
    Interviewer_Filled_Silently = hh11900,
    Save_Address = hh11901,
    Address_Is_Correct = hh11902,
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
    Point_Number_Drawn = point,
    Birth_Year = gebjahr_brutto,
    Gender_Recorded = sex_brutto,
    Tranche = TRANCHE,
    Phone_Number_Found = telja,
    Building_Type = casa_typ,
    Number_of_Private_Households = casa_hh,
    Distance_to_Nearest_Public_Transport = casa_dist_opnv,
    Group = oa_result,
    Nationality_Recoded = nationn,
    Interview_Number = INTNR_hh,
    Household_Level_Person_Count = ANZPHH_hh,
    Last_Deployment_Method = LMETH_hh,
    CATI_Switch = CATI_SW_hh,
    Household_Fully_Interviewed = vollstaendig_hh,
    Household_Tracking_Level = TRACK_hh,
    Total_Tracking_Final_Outcome = t_forc_hh,
    New_Phone_Number = ch_tele_hh,
    New_Email_Address = ch_email_hh,
    New_Address = ch_post_hh,
    Final_Outcome_Date = FO_Dat_hh,
    Final_Outcome_Households = FORC_hh,
    Interviewer_Number_Final_Outcome = INTERNR_hh,
    Valid_Cases_Realized = foreal_hh,
    First_Reminder_Sent = verinn1_hh,
    Second_Reminder_Sent = verinn2_hh,
    Conversion_Letter_Sent = vkonv_hh,
    Pre_Contact_CATI = cati_vk_hh,
    Household_Ready_for_Panel = panelbr_hh,
    Contact_Count_Household_Level = anzkonHH1_hh,
    Total_Contact_Count_Household_Level = anzkonHH2_hh,
    All_Contacts_in_Household_Incl_Person_Contact = anzkonHH3_hh,
    Household_Weight_Without_Calibration = GewHH,
    Calibrated_Household_Weight = calHH,
    Community_ID = GemID,
    Household_Interview_Year = m_intyear_hh,
    Household_Interview_Month = m_intmonth_hh
  )


# Variablen, die entfernt werden sollen
unwanted_vars <- c(
  "Tranche", 
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
  "Pre_Contact_CATI"
)

# Entfernen der weniger relevanten Variablen
daten_haushalt <- daten_haushalt %>%
  select(-all_of(unwanted_vars))

daten_haushalt <- daten_haushalt %>%
  mutate(Birth_Year = as.character(Birth_Year),  # Ensure Birth_Year is character for replacement
         Birth_Year = ifelse(Birth_Year == "keine Angabe", NA, Birth_Year),
         Birth_Year = as.numeric(Birth_Year))  # Convert to numeric safely

daten_haushalt <- daten_haushalt %>%
  mutate(Age = ifelse(!is.na(Birth_Year),
                      as.numeric(format(Sys.Date(), "%Y")) - Birth_Year,
                      NA_real_))  # Handle NA to prevent NAs from coercion




# data transformation
daten_haushalt <- daten_haushalt %>%
  # Umwandlung aller kategorialen Variablen in Faktoren mit einer spezifischen NA-Behandlung
  mutate(across(where(is.factor), ~ fct_explicit_na(.x, na_level = "Unbekannt"))) %>%
  # Spezifische NA-Behandlungen für numerische Variablen
  mutate(
    Monthly_Saving = ifelse(is.na(Monthly_Saving), median(as.numeric(as.character(Monthly_Saving)), na.rm = TRUE), Monthly_Saving),
    Number_of_Children_Rooms = ifelse(is.na(Number_of_Children_Rooms), 0, Number_of_Children_Rooms),
    Household_Net_Income = ifelse(is.na(Household_Net_Income), mean(as.numeric(as.character(Household_Net_Income)), na.rm = TRUE), Household_Net_Income),
    Number_of_People_in_Household = ifelse(is.na(Number_of_People_in_Household), median(Number_of_People_in_Household, na.rm = TRUE), Number_of_People_in_Household),
    External_Children = replace_na(External_Children, "nein")
  ) %>%
  # Filtern unplausibler Werte (Beispiel: Alter über 120 Jahre)
  filter(as.numeric(as.character(Birth_Year)) >= 1900 & as.numeric(as.character(Birth_Year)) <= as.numeric(format(Sys.Date(), "%Y"))) %>%
  # Transformation von Geburtsjahr in Alter
  mutate(Age = as.numeric(format(Sys.Date(), "%Y")) - as.numeric(as.character(Birth_Year))) %>%
  # Skalierung von numerischen Daten
  mutate(across(where(is.numeric), ~ scale(.x))) %>%
  # Überprüfen, dass die Anzahl der Personen im Haushalt nicht null ist
  filter(Number_of_People_in_Household > 0) %>%
  # Entfernen aller Zeilen, die noch NA enthalten
  drop_na()


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
    
    # Deprivation Risk Index
    Deprivation_Risk_Index = (1 - Savings_Income_Ratio) + (1 - Unexpected_Expense_Coverage) + (1 - Normalized_Social_Support),
    
  #   # Zusätzliche metrische Indikatoren
  #   Employment_Rate = ...,  # Beschäftigungsrate der Erwachsenen im Haushalt
  #   Nutritional_Security = ...,  # Ernährungssicherheit basierend auf Zugang zu ausgewogener Ernährung
  #   Housing_Security = ...,  # Sicherheitsbewertung der Wohnsituation
  #   Household_Stress_Index = ...,  # Stresslevel im Haushalt basierend auf Umfrageantworten
  #   Social_Integration_Score = ...  # Teilnahme an gesellschaftlichen Aktivitäten
  # 
  )






#####################################
# Datensätze zusammenführen
daten <- merge(aggregated_relationship_data, daten_haushalt, by = "Matchvariable_HHLFD", all = TRUE)

# Fehlende Werte für numerische und faktorbezogene Variablen imputieren
daten <- daten %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  mutate(across(where(is.factor), fct_explicit_na, na_level = "NA")) %>%
  mutate(across(where(is.factor), ~fct_explicit_na(., na_level = get_mode(.))))

# Deprivationsindikatoren definieren und Gesamtindex berechnen
daten <- daten %>%
  mutate(
    sparen_depriviert = ifelse(Monthly_Saving == "nein, aus finanziellen Gründen nicht", 1, 0),
    moebel_depriviert = ifelse(Replace_Furniture == "nein, aus finanziellen Gründen nicht", 1, 0),
    ausgaben_depriviert = ifelse( Pay_for_Unexpected_Expenses == "nein, aus finanziellen Gründen nicht", 1, 0),
    alg2_depriviert = ifelse(Receipt_of_ALG_II_or_Hartz_IV == "ja", 1, 0),
    gesamt_depriviert = sparen_depriviert + moebel_depriviert + ausgaben_depriviert + alg2_depriviert,
    deprivation_outcome = ifelse(gesamt_depriviert >= 1, 1, 0)
  )

# Daten für das Random-Forest-Modell vorbereiten
predictors <- daten %>% select(-deprivation_outcome)
outcome <- daten$deprivation_outcome

# Überprüfen, ob jetzt zwei Klassen vorhanden sind
if (length(unique(outcome)) < 2) {
  print("Not enough classes for classification. Adjust the conditions.")
} else {
  rf_model <- randomForest(as.factor(outcome) ~ ., data = daten, ntree = 500, importance = TRUE)
  print(rf_model)
}

# Anzahl der Fälle in jeder Klasse ausgeben
print(table(daten$deprivation_outcome))



library(caret)
set.seed(123)  # Für reproduzierbare Ergebnisse
# Aufteilung der Daten in 70% Training und 30% Test
train_indices <- createDataPartition(daten$deprivation_outcome, p = 0.7, list = FALSE)
train_data <- daten[train_indices, ]
test_data <- daten[-train_indices, ]


rf_model_train <- randomForest(as.factor(deprivation_outcome) ~ ., data = train_data, ntree = 500)
print(rf_model_train)

test_predictions <- predict(rf_model_train, test_data)
confusion_matrix <- table(test_data$deprivation_outcome, test_predictions)
print(confusion_matrix)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy: ", accuracy))
importance <- importance(rf_model_train)
varImpPlot(rf_model_train)
library(caret)
control <- trainControl(method = "cv", number = 10)  # 10-fache Kreuzvalidierung
cv_model <- train(as.factor(deprivation_outcome) ~ ., data = train_data, method = "rf", 
                  trControl = control, ntree = 500)
print(cv_model)



library(caret)
set.seed(123)
fitControl <- trainControl(
  method = "cv",      # Kreuzvalidierung
  number = 10         # Anzahl der Folds
)

rf_model_cv <- train(
  as.factor(deprivation_outcome) ~ ., 
  data = daten, 
  method = "rf",
  trControl = fitControl,
  ntree = 500
)

# Ergebnisse der Kreuzvalidierung ausgeben
print(rf_model_cv)

