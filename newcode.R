############## LIBRARIES AND PACKAGES ##############

# Install necessary packages (if not already installed)
if (!require("haven")) install.packages("haven")
if (!require("dplyr")) install.packages("dplyr")
if (!require("labelled")) install.packages("labelled")
if (!require("randomForest")) install.packages("randomForest")
if (!require("caret")) install.packages("caret")
if (!require("mice")) install.packages("mice")

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
library(car)
library(mice)
library(VIM)

############### DATA CLEANING ##############

# Function to replace NA values using KNN imputation
replace_na_values <- function(df, k = 5) {
  # Define the codes for missing values
  missing_codes <- c(-1, -2, -3, -4, -5, -6, -7, -8, -9, -10, -11)
  
  # Replace missing codes with NA
  df <- df %>%
    mutate(across(where(is.numeric), ~ ifelse(. %in% missing_codes, NA, .))) %>%
    mutate(across(where(is.character), ~ ifelse(. %in% as.character(missing_codes), NA, .)))
  
  # Perform KNN imputation
  df_imputed <- kNN(df, k = k, imp_var = FALSE)  # Set imp_var = FALSE to avoid creating extra columns for indicators
  
  return(df_imputed)
}

# Load data from SPSS file
data <- read.spss("dji_suf_personen.sav", to.data.frame = TRUE)

# Convert specific variables to numeric for processing
data1 <- data %>%
  mutate(XALTER = as.numeric(as.character(XALTER)))

# Filter the data based on specific conditions
data2 <- data1 %>%
  filter(IZP == "trifft zu" & XALTER < 12)

# Convert and recode variables for region (east vs. west Germany)
data2 <- data2 %>%
  mutate(bland_numeric = as.numeric(bland)) %>%
  mutate(east = case_when(
    bland_numeric <= 10 ~ 0,
    bland_numeric > 10 & bland_numeric <= 16 ~ 1,
    TRUE ~ NA_real_
  )) %>%
  mutate(east = factor(east, levels = c(0, 1), labels = c("[0] West Germany", "[1] East Germany (incl. Berlin)"))) %>%
  select(-bland_numeric) 

attr(data2$east, "label") <- "Region"

# Recode deprivation variables
data5 <- data2 %>%
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

# Create child-specific material deprivation variable
data6 <- data5 %>%
  rowwise() %>%
  mutate(
    help = sum(c_across(starts_with("dep_c_")), na.rm = TRUE),
    help2 = sum(is.na(c_across(starts_with("dep_c_")))),
    dep_child = case_when(
      help == 0 ~ "No Deprivation",
      help >= 1 ~ "Deprivation"
    )
  ) %>%
  ungroup()

# Remove unnecessary variables related to deprivation but not needed for analysis
remove_dep_c_vars <- names(data6)[grepl("^dep_c($|[^hild])", names(data6))]
remove_vars <- unique(c(remove_dep_c_vars))
data6 <- data6 %>% select(-all_of(remove_vars))

# Keep only observations without missing values in deprivation variables
data7 <- data6 %>%
  filter(help2 == 0)

# Recode and clean other variables as needed
data7$dep_child <- factor(data7$dep_child, levels = c("No Deprivation", "Deprivation"))

# Load and apply a list of variables to exclude based on external file
file_path <- "Varlist20240722.xlsx"
varlist <- readxl::read_excel(file_path)
included_columns <- varlist %>%
  filter(`Exclude in next step` == "a") %>%
  pull(`Varname`)
data_reduced <- data7 %>% select(all_of(included_columns))

# Load Excel file with column names and labels, and rename columns accordingly
var_list <- read_excel("VarListePersonen.xlsx")
colnames_mapping <- setNames(var_list$Label, var_list$`colnames(daten_personen)`)
new_colnames <- colnames(data_reduced)
for (i in seq_along(new_colnames)) {
  if (new_colnames[i] %in% names(colnames_mapping)) {
    new_colnames[i] <- colnames_mapping[new_colnames[i]]
  }
}
colnames(data_reduced) <- new_colnames

# Clean duplicated column names
clean_column_names <- function(df) {
  duplicated_names <- names(df)[duplicated(names(df))]
  if(length(duplicated_names) > 0) {
    names(df) <- make.unique(names(df))
    message("Duplicated column names have been cleaned.")
  } else {
    message("No duplicated column names found.")
  }
  return(df)
}
data9 <- clean_column_names(data_reduced)

# Convert relevant variables to numeric and clean data
data10 <- data9
data10$Alter_der_Person_in_Jahren_ <- as.numeric(as.character(data10$Alter_der_Person_in_Jahren_))
data10$Alter_der_Person_in_Jahren_Mutter_ <- as.numeric(as.character(data10$Alter_der_Person_in_Jahren_Mutter_))
data10$Alter_der_Person_in_Jahren_Vater_ <- as.numeric(as.character(data10$Alter_der_Person_in_Jahren_Vater_))
anomalien <- data10[data10$Alter_der_Person_in_Jahren_Mutter_ == 0, ]
data10_clean <- data10[
  data10$Alter_der_Person_in_Jahren_Mutter_ != 0 & 
    data10$Alter_der_Person_in_Jahren_Vater_ != 0, 
]

# Recode migration background variable
data10_clean$Migrationshintergrund_analog_zum_NEPS_ <- as.numeric(factor(ifelse(data10_clean$Migrationshintergrund_analog_zum_NEPS_ %in% c("1,0 Generation", "1,5 Generation"), 
                                                                                "1. Generation",
                                                                                ifelse(data10_clean$Migrationshintergrund_analog_zum_NEPS_ %in% c("2,0 Generation", "2,25 Generation", "2,5 Generation", "2,75 Generation"),
                                                                                       "2. Generation",
                                                                                       ifelse(data10_clean$Migrationshintergrund_analog_zum_NEPS_ %in% c("3,0 Generation", "3,25 Generation", "3,5 Generation", "3,75 Generation"),
                                                                                              "3. Generation", 
                                                                                              "No Migration Background")))))

data10_clean$Migrationshintergrund_analog_zum_NEPS_ <- as.numeric(factor(data10_clean$Migrationshintergrund_analog_zum_NEPS_))

# Recode and clean health status and income variables
data10_clean$Gesundheitszustand_ <- factor(ifelse(data10_clean$Gesundheitszustand_ %in% c("4", "5", "sehr schlecht"), 
                                                  "sehr schlecht", 
                                                  as.character(data10_clean$Gesundheitszustand_)))
data10_clean$Gesundheitszustand_ <- as.numeric(factor(data10_clean$Gesundheitszustand_, 
                                                      levels = c("sehr gut", "2", "3", "sehr schlecht")))

data10_clean$persönliches_Nettoeinkommen_Mutter_<- as.numeric(as.character(factor(data10_clean$persönliches_Nettoeinkommen_Mutter_, 
                                                                                  levels = c("kein persönliches Einkommen", 
                                                                                             "Unter 150 Euro", 
                                                                                             "150 bis unter 300 Euro",
                                                                                             "300 bis unter 500 Euro", 
                                                                                             "500 bis unter 700 Euro",
                                                                                             "700 bis unter 900 Euro",
                                                                                             "900 bis unter 1 100 Euro",
                                                                                             "1 100 bis unter 1 300 Euro",
                                                                                             "1 300 bis unter 1 500 Euro",
                                                                                             "1 500 bis unter 1 700 Euro",
                                                                                             "1 700 bis unter 2 000 Euro",
                                                                                             "2 000 bis unter 2 300 Euro",
                                                                                             "2 300 bis unter 2 600 Euro",
                                                                                             "2 600 bis unter 2 900 Euro",
                                                                                             "2 900 bis unter 3 200 Euro",
                                                                                             "3 200 bis unter 3 600 Euro",
                                                                                             "3 600 bis unter 4 000 Euro",
                                                                                             "4 000 bis unter 4 500 Euro",
                                                                                             "4 500 bis unter 5 000 Euro",
                                                                                             "5 000 bis unter 5 500 Euro",
                                                                                             "5 500 bis unter 6 000 Euro",
                                                                                             "6 000 bis unter 7 500 Euro",
                                                                                             "7 500 bis unter 10 000 Euro",
                                                                                             "10 000 bis unter 18 000 Euro",
                                                                                             "18 000 und mehr Euro",
                                                                                             "möchte ich nicht beantworten"),
                                                                                  labels = c(0,                     
                                                                                             75,                    
                                                                                             225,                   
                                                                                             400,                    
                                                                                             600,                    
                                                                                             800,                   
                                                                                             1000,                   
                                                                                             1200,                   
                                                                                             1400,                  
                                                                                             1600,                  
                                                                                             1850,                  
                                                                                             2150,                  
                                                                                             2450,                  
                                                                                             2750,                  
                                                                                             3050,                   
                                                                                             3400,                   
                                                                                             3800,                   
                                                                                             4250,                   
                                                                                             4750,                   
                                                                                             5250,                   
                                                                                             5750,                  
                                                                                             6750,                   
                                                                                             8750,                   
                                                                                             14000,                  
                                                                                             18000,                  
                                                                                             NA))))

data10_clean$persönliches_Nettoeinkommen_Vater_ <- as.numeric(as.character(factor(data10_clean$persönliches_Nettoeinkommen_Vater_, 
                                                                                  levels = c("kein persönliches Einkommen", 
                                                                                             "Unter 150 Euro", 
                                                                                             "150 bis unter 300 Euro",
                                                                                             "300 bis unter 500 Euro", 
                                                                                             "500 bis unter 700 Euro",
                                                                                             "700 bis unter 900 Euro",
                                                                                             "900 bis unter 1 100 Euro",
                                                                                             "1 100 bis unter 1 300 Euro",
                                                                                             "1 300 bis unter 1 500 Euro",
                                                                                             "1 500 bis unter 1 700 Euro",
                                                                                             "1 700 bis unter 2 000 Euro",
                                                                                             "2 000 bis unter 2 300 Euro",
                                                                                             "2 300 bis unter 2 600 Euro",
                                                                                             "2 600 bis unter 2 900 Euro",
                                                                                             "2 900 bis unter 3 200 Euro",
                                                                                             "3 200 bis unter 3 600 Euro",
                                                                                             "3 600 bis unter 4 000 Euro",
                                                                                             "4 000 bis unter 4 500 Euro",
                                                                                             "4 500 bis unter 5 000 Euro",
                                                                                             "5 000 bis unter 5 500 Euro",
                                                                                             "5 500 bis unter 6 000 Euro",
                                                                                             "6 000 bis unter 7 500 Euro",
                                                                                             "7 500 bis unter 10 000 Euro",
                                                                                             "10 000 bis unter 18 000 Euro",
                                                                                             "18 000 und mehr Euro",
                                                                                             "möchte ich nicht beantworten"),
                                                                                  labels = c(0,                     
                                                                                             75,                    
                                                                                             225,                   
                                                                                             400,                    
                                                                                             600,                    
                                                                                             800,                   
                                                                                             1000,                   
                                                                                             1200,                   
                                                                                             1400,                  
                                                                                             1600,                  
                                                                                             1850,                  
                                                                                             2150,                  
                                                                                             2450,                  
                                                                                             2750,                  
                                                                                             3050,                   
                                                                                             3400,                   
                                                                                             3800,                   
                                                                                             4250,                   
                                                                                             4750,                   
                                                                                             5250,                   
                                                                                             5750,                  
                                                                                             6750,                   
                                                                                             8750,                   
                                                                                             14000,                  
                                                                                             18000,                  
                                                                                             NA))))

# Log transformation for equivalence income
data10_clean$log_Äquivalenzeinkommen <- log(data10_clean$Äquivalenzeinkommen_Intervallmitte_ + 1)
data10_clean$Äquivalenzeinkommen_Intervallmitte_ <- NULL

# Clean and recode other relevant variables
data10_clean <- data10_clean[data10_clean$Geschlecht_ != "keines der beiden oben genannten", ]
data10_clean$Geschlecht_ <- droplevels(data10_clean$Geschlecht_)
data10_clean$Familientyp_aggregiert <- factor(ifelse(data10_clean$Familientyp_ %in% c("komplexe Stiefväterfamilie", 
                                                                                      "komplexe Stiefmütterfamilie", 
                                                                                      "komplexe Stieffamilie", 
                                                                                      "Stieffamilie ohne explizite Stiefeltern"), 
                                                     "komplexe Stieffamilien", 
                                                     as.character(data10_clean$Familientyp_)))
data10_clean$Familientyp_aggregiert <- as.numeric(factor(data10_clean$Familientyp_aggregiert))
data10_clean$Familientyp_ <- NULL
data10_clean$`Berufsausbildung/Studium_Mutter_` <- as.numeric(factor(data10_clean$`Berufsausbildung/Studium_Mutter_`))
data10_clean$ÖPNV_Distance_Binned <- cut(data10_clean$`Entfernung_zur_nächsten_ÖPNV-Haltest._(in_Metern)_`,
                                         breaks = c(0, 100, 500, 1000, 2000, Inf),
                                         labels = c("Very Close", "Close", "Medium", "Far", "Very Far"))
data10_clean$`Entfernung_zur_nächsten_ÖPNV-Haltest._(in_Metern)_` <- NULL
data10_clean$Socioeconomic_Group_Aggregated_Mutter <- factor(
  ifelse(data10_clean$`European_Socioeconomic_Groups_2-Steller_Mutter_` %in% c(
    "11. Higher managerial self-employed", "12. Lower managerial self-employed", 
    "13. Higher managerial employees", "14. Lower managerial employees"),
    "Managerial Occupations",
    ifelse(data10_clean$`European_Socioeconomic_Groups_2-Steller_Mutter_` %in% c(
      "21. Science, engineering and information and communications technology (ICT) professionals", 
      "22. Health professionals", "23. Business and administration professionals", 
      "24. Legal, social and cultural professionals", "25. Teaching professionals"),
      "Professional Occupations",
      ifelse(data10_clean$`European_Socioeconomic_Groups_2-Steller_Mutter_` %in% c(
        "31. Science, engineering and ICT technicians and associated professionals", 
        "32. Health associate professionals", "33. Business and administration associate professionals", 
        "34. Legal, social and cultural associate professionals"),
        "Technicians and Associate Professionals",
        ifelse(data10_clean$`European_Socioeconomic_Groups_2-Steller_Mutter_` %in% c(
          "51. General and numerical clerks and other clerical support employees", "52. Customer services clerks"),
          "Clerical Occupations",
          ifelse(data10_clean$`European_Socioeconomic_Groups_2-Steller_Mutter_` %in% c(
            "71. Personal services and sales employees", "72. Blue collar employees and food preparation assistants in elementary occupations",
            "73. Cleaners and helpers and services employees in elementary occupations"),
            "Service and Sales Occupations",
            ifelse(data10_clean$`European_Socioeconomic_Groups_2-Steller_Mutter_` %in% c(
              "61. Building and related trade employees", "62. Food processing, wood working, garment employees", 
              "63. Metal, machinery, handicraft, printing, electrical and electronic trades employees", 
              "64. Stationary plant and machine operators and assemblers", "65. Drivers"),
              "Skilled Trades Occupations",
              ifelse(data10_clean$`European_Socioeconomic_Groups_2-Steller_Mutter_` %in% c(
                "41. Skilled agricultural self-employed workers", "42. Technicians, clerical support, services and sales self-employed workers", 
                "43. Craft and related trades self-employed workers"),
                "Self-employed Workers",
                ifelse(data10_clean$`European_Socioeconomic_Groups_2-Steller_Mutter_` %in% c(
                  "35. Non-commissioned armed forces officers", "54. Armed forced occupations and protective service employees"),
                  "Armed Forces and Protective Services",
                  ifelse(data10_clean$`European_Socioeconomic_Groups_2-Steller_Mutter_` %in% c(
                    "81. Retired Managers", "82. Retired professionals", "83. Retired technicians and associate professionals", 
                    "84. Retired small entrepreneurs", "85. Retired skilled white collars", "86. Retired skilled blue-collars", 
                    "87. Retired less skilled workers", "88. Other inactive aged 65 or more"),
                    "Retired and Inactive",
                    ifelse(data10_clean$`European_Socioeconomic_Groups_2-Steller_Mutter_` %in% c(
                      "92. Permanently disabled", "93. Unemployed not elsewhere classified", "94. Other inactive aged less than 65 years", "91. Students"),
                      "Other Inactive Groups",
                      as.character(data10_clean$`European_Socioeconomic_Groups_2-Steller_Mutter_`)
                    )))))))))))

data10_clean$`European_Socioeconomic_Groups_2-Steller_Mutter_` <- NULL
data10_clean$Socioeconomic_Group_Aggregated_Mutter<-as.numeric(factor(data10_clean$Socioeconomic_Group_Aggregated_Mutter))
data10_clean$Socioeconomic_Group_Aggregated_Vater <- factor(
  ifelse(data10_clean$`European_Socioeconomic_Groups_2-Steller_Vater_` %in% c(
    "11. Higher managerial self-employed", "12. Lower managerial self-employed", 
    "13. Higher managerial employees", "14. Lower managerial employees"),
    "Managerial Occupations",
    ifelse(data10_clean$`European_Socioeconomic_Groups_2-Steller_Vater_` %in% c(
      "21. Science, engineering and information and communications technology (ICT) professionals", 
      "22. Health professionals", "23. Business and administration professionals", 
      "24. Legal, social and cultural professionals", "25. Teaching professionals"),
      "Professional Occupations",
      ifelse(data10_clean$`European_Socioeconomic_Groups_2-Steller_Vater_` %in% c(
        "31. Science, engineering and ICT technicians and associated professionals", 
        "32. Health associate professionals", "33. Business and administration associate professionals", 
        "34. Legal, social and cultural associate professionals"),
        "Technicians and Associate Professionals",
        ifelse(data10_clean$`European_Socioeconomic_Groups_2-Steller_Vater_` %in% c(
          "51. General and numerical clerks and other clerical support employees", "52. Customer services clerks"),
          "Clerical Occupations",
          ifelse(data10_clean$`European_Socioeconomic_Groups_2-Steller_Vater_` %in% c(
            "71. Personal services and sales employees", "72. Blue collar employees and food preparation assistants in elementary occupations",
            "73. Cleaners and helpers and services employees in elementary occupations"),
            "Service and Sales Occupations",
            ifelse(data10_clean$`European_Socioeconomic_Groups_2-Steller_Vater_` %in% c(
              "61. Building and related trade employees", "62. Food processing, wood working, garment employees", 
              "63. Metal, machinery, handicraft, printing, electrical and electronic trades employees", 
              "64. Stationary plant and machine operators and assemblers", "65. Drivers"),
              "Skilled Trades Occupations",
              ifelse(data10_clean$`European_Socioeconomic_Groups_2-Steller_Vater_` %in% c(
                "41. Skilled agricultural self-employed workers", "42. Technicians, clerical support, services and sales self-employed workers", 
                "43. Craft and related trades self-employed workers"),
                "Self-employed Workers",
                ifelse(data10_clean$`European_Socioeconomic_Groups_2-Steller_Vater_` %in% c(
                  "54. Armed forced occupations and protective service employees"),
                  "Armed Forces and Protective Services",
                  ifelse(data10_clean$`European_Socioeconomic_Groups_2-Steller_Vater_` %in% c(
                    "81. Retired Managers", "82. Retired professionals", "83. Retired technicians and associate professionals", 
                    "84. Retired small entrepreneurs", "85. Retired skilled white collars", "86. Retired skilled blue-collars", 
                    "87. Retired less skilled workers", "88. Other inactive aged 65 or more"),
                    "Retired and Inactive",
                    ifelse(data10_clean$`European_Socioeconomic_Groups_2-Steller_Vater_` %in% c(
                      "92. Permanently disabled", "93. Unemployed not elsewhere classified", "94. Other inactive aged less than 65 years", "91. Students"),
                      "Other Inactive Groups",
                      as.character(data10_clean$`European_Socioeconomic_Groups_2-Steller_Vater_`)
                    )))))))))))

data10_clean$Socioeconomic_Group_Aggregated_Vater<-as.numeric(factor(data10_clean$Socioeconomic_Group_Aggregated_Vater))
data10_clean$`European_Socioeconomic_Groups_2-Steller_Vater_` <- NULL
data10_clean$vereinbarte_Arbeitszeit_Mutter_ <- as.numeric(factor(data10_clean$vereinbarte_Arbeitszeit_Mutter_))
data10_clean$vereinbarte_Arbeitszeit_Vater_ <- as.numeric(factor(data10_clean$vereinbarte_Arbeitszeit_Vater_))
data10_clean$Gebäudetypologie_aggregiert <- factor(ifelse(data10_clean$Gebäudetypologie_in_Klassen_ %in% c("1a1 freistehendes Ein- bis Zweiparteienhaus, klein", "1b1 freistehendes Ein- bis Zweiparteienhaus, mittel", "1c1 freistehendes Ein- bis Zweiparteienhaus, groß"),
                                                          "Detached Houses",
                                                          ifelse(data10_clean$Gebäudetypologie_in_Klassen_ %in% c("1f1 klassische Doppelhaushälfte, klein", "1g1 klassische Doppelhaushälfte, mittel", "1h1 klassische Doppelhaushälfte, groß",
                                                                                                                  "1j1 Reihenhaus, klein", "1k1 Reihenhaus, mittel", "1l1 Reihenhaus, groß"),
                                                                 "Semi-Detached & Row Houses",
                                                                 ifelse(data10_clean$Gebäudetypologie_in_Klassen_ %in% c("2a1 freistehendes Mehrparteienhaus, klein", "2b1 freistehendes Mehrparteienhaus, mittel", "2c1 freistehendes Mehrparteienhaus, groß",
                                                                                                                         "3b Mehrfamilienkomplex", "3c Hochhaus"),
                                                                        "Apartment Buildings",
                                                                        "Special & Non-Residential"))))
data10_clean$Gebäudetypologie_in_Klassen_ <- NULL
data10_clean$Gebäudetypologie_aggregiert <- as.numeric(factor(data10_clean$Gebäudetypologie_aggregiert))

# Recode and clean education variables for parents
data10_clean$ISCED_2011_Mutter_ordinal <- factor(data10_clean$ISCED_2011_Mutter_,
                                                 levels = c("Primarbereich, allgemeinbildend", 
                                                            "Sekundarbereich I, allgemeinbildend", 
                                                            "Sekundarbereich II allgemeinbildend", 
                                                            "Sekundarbereich II berufsbildend", 
                                                            "Postsekundar nichttertiär, berufsbildend", 
                                                            "Bachelor oder gleichw. akademisch", 
                                                            "Bachelor oder gleichw. berufsorientiert", 
                                                            "Master oder gleichwertig", 
                                                            "Promotion"),
                                                 ordered = TRUE)
data10_clean$ISCED_2011_Mutter_ <- NULL
data10_clean$ISCED_2011_Mutter_ordinal <- as.numeric(factor(data10_clean$ISCED_2011_Mutter_ordinal))

data10_clean$ISCED_2011_Vater_ordinal <- factor(data10_clean$ISCED_2011_Vater_,
                                                levels = c("Primarbereich, allgemeinbildend", 
                                                           "Sekundarbereich I, allgemeinbildend", 
                                                           "Sekundarbereich II allgemeinbildend", 
                                                           "Sekundarbereich II berufsbildend", 
                                                           "Postsekundar nichttertiär, berufsbildend", 
                                                           "Bachelor oder gleichw. akademisch", 
                                                           "Bachelor oder gleichw. berufsorientiert", 
                                                           "Master oder gleichwertig", 
                                                           "Promotion"),
                                                ordered = TRUE)
data10_clean$ISCED_2011_Vater_ordinal <- as.numeric(factor(data10_clean$ISCED_2011_Vater_ordinal))
data10_clean$ISCED_2011_Vater_ <- NULL

# Recode and clean variables related to parents' activity status
data10_clean$Aktivitätsstatus_Mutter_Aggregated <- factor(
  data10_clean$Aktivitätsstatus_Mutter_,
  levels = c(
    "erwerbstätig     Job, selbständig, Praktikum,", 
    "Besuch einer Schule, um einen allgemeinbildenden", 
    "in beruflicher Ausbildung, Studium, Umschulung oder", 
    "arbeitslos gemeldet     d.h. mit oder ohne finanzielle", 
    "etwas anderes    wie z.B. Kindererziehung, Elternzeit ohne"
  ),
  labels = c(
    "Employed", 
    "In Education", 
    "In Education", 
    "Unemployed or Other", 
    "Unemployed or Other"
  )
)

data10_clean$Aktivitätsstatus_Vater_Aggregated <- factor(
  data10_clean$Aktivitätsstatus_Vater_,
  levels = c(
    "erwerbstätig     Job, selbständig, Praktikum,", 
    "Besuch einer Schule, um einen allgemeinbildenden", 
    "in beruflicher Ausbildung, Studium, Umschulung oder", 
    "arbeitslos gemeldet     d.h. mit oder ohne finanzielle", 
    "etwas anderes    wie z.B. Kindererziehung, Elternzeit ohne"
  ),
  labels = c(
    "Employed", 
    "In Education", 
    "In Education", 
    "Unemployed or Other", 
    "Unemployed or Other"
  )
)

data10_clean$Aktivitätsstatus_Vater_ <- NULL
data10_clean$Aktivitätsstatus_Muter_ <- NULL

# Filter out near-zero variance features
nzv <- nearZeroVar(data10_clean, saveMetrics = TRUE)
data14 <- data10_clean[, !nzv$nzv]

# Replace missing values using KNN imputation
data_final_2 <- replace_na_values(data14, k=5)

# Function to clean column names to remove special characters
clean_names <- function(df) {
  colnames(df) <- make.names(colnames(df), unique = TRUE)
  return(df)
}

# Apply the function to clean column names
data_final_clean <- clean_names(data_final_2)

# Function to calculate Cramer's V for categorical variables
cramersV <- function(x, y, use_fisher = FALSE) {
  tbl <- table(x, y)
  
  # If all counts are > 5, use Chi-squared test
  if (!use_fisher && all(tbl > 5)) {
    chi2 <- chisq.test(tbl, simulate.p.value = TRUE)$statistic
    n <- sum(tbl)
    phi2 <- chi2 / n
    k <- min(dim(tbl)) - 1
    return(sqrt(phi2 / k))
  } else {
    # Fall back to Fisher's test with simulated p-values if necessary
    if (min(dim(tbl)) == 2) {
      p_val <- fisher.test(tbl, simulate.p.value = TRUE)$p.value
      if (p_val < 0.05) {
        return(1)  # Strong association
      } else {
        return(0)  # Weak or no association
      }
    } else {
      return(NA)  # Skip Fisher's test on larger tables
    }
  }
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
cat_correlations <- function(data, dep_var, use_fisher = FALSE) {
  cat_results <- data.frame(Variable = character(), CramersV = numeric(), stringsAsFactors = FALSE)
  cat_vars <- data %>% select(where(is.factor))
  
  for (var in colnames(cat_vars)) {
    cramer_value <- cramersV(cat_vars[[var]], data[[dep_var]], use_fisher)
    if (!is.na(cramer_value)) {
      cat_results <- rbind(cat_results, data.frame(Variable = var, CramersV = cramer_value, stringsAsFactors = FALSE))
    }
  }
  
  return(cat_results)
}

# Calculate correlations
numeric_corrs <- num_correlations(data_final_clean, "dep_child")
categorical_corrs <- cat_correlations(data_final_clean, "dep_child")

# Combine and filter results to identify potential colliders
threshold <- 0.4
numeric_corrs <- numeric_corrs %>% rename(Score = Correlation)
categorical_corrs <- categorical_corrs %>% rename(Score = CramersV)

potential_colliders <- rbind(
  numeric_corrs %>% filter(abs(Score) > threshold),
  categorical_corrs %>% filter(Score > threshold)
)

collider_vars <- potential_colliders$Variable
collider_vars <- setdiff(collider_vars, "dep_child")

# Remove collider variables from the dataset
data_final_clean <- data_final_clean %>% select(-all_of(collider_vars))

# Remove or replace NaN values
data_final_clean_3 <- data_final_clean %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), median(., na.rm = TRUE), .)))

# Remove or replace Inf values
data_final_clean_4 <- data_final_clean_3 %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), median(., na.rm = TRUE), .)))

# Remove highly correlated categorical variables
remove_highly_correlated_categorical <- function(data, threshold = 0.05) {
  categorical_cols <- names(data)[sapply(data, is.factor)]
  to_keep <- categorical_cols
  
  for (i in 1:(length(categorical_cols) - 1)) {
    for (j in (i + 1):length(categorical_cols)) {
      var1 <- categorical_cols[i]
      var2 <- categorical_cols[j]
      
      contingency_table <- table(data[[var1]], data[[var2]])
      
      if (all(contingency_table > 0)) {
        test <- tryCatch({
          if (any(contingency_table < 5)) {
            fisher.test(contingency_table)
          } else {
            chisq.test(contingency_table)
          }
        }, error = function(e) {
          list(p.value = NA)
        })
        
        if (!is.na(test$p.value) && test$p.value < threshold) {
          if (var2 %in% to_keep && var2 != "dep_child") {
            to_keep <- setdiff(to_keep, var2)
          }
        }
      }
    }
  }
  
  remaining_data <- data[c(to_keep, setdiff(names(data), categorical_cols))]
  return(remaining_data)
}

result <- remove_highly_correlated_categorical(data_final_clean_4)

# Remove highly correlated numerical variables
remove_highly_correlated_numerical <- function(data, threshold = 0.9) {
  num_data <- data[sapply(data, is.numeric)]
  cor_matrix <- cor(num_data, use = "pairwise.complete.obs")
  to_remove <- c()
  
  for (i in 1:(ncol(cor_matrix) - 1)) {
    for (j in (i + 1):ncol(cor_matrix)) {
      if (abs(cor_matrix[i, j]) > threshold) {
        var1 <- colnames(cor_matrix)[i]
        var2 <- colnames(cor_matrix)[j]
        
        if (!(var2 %in% to_remove) && var2 != "dep_child") {
          to_remove <- c(to_remove, var2)
        }
      }
    }
  }
  
  return(data[, !(colnames(data) %in% to_remove)])
}

result2 <- remove_highly_correlated_numerical(result)

# Check for multicollinearity and remove variables with high VIF
data <- result2 %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), median(., na.rm = TRUE), .)))

data <- data %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), median(., na.rm = TRUE), .)))

vif_values <- vif(lm(as.numeric(dep_child) ~ ., data = data))

if (is.matrix(vif_values)) {
  table <- data.frame(Variable = rownames(vif_values), 
                      GVIF = vif_values[, "GVIF"], 
                      Df = vif_values[, "Df"], 
                      GVIF_norm = vif_values[, "GVIF"]^(1/(2*vif_values[, "Df"])))
  
  filtered_table <- table %>% filter(GVIF_norm > 5)
  data <- data %>% select(-all_of(filtered_table$Variable))
  
} else {
  table <- data.frame(Variable = names(vif_values), VIF = vif_values)
  filtered_table <- table %>% filter(VIF > 5)
  data <- data %>% select(-all_of(filtered_table$Variable))
}

#################### RANDOM FOREST #################

# Remove unnecessary variable
data$HHLFD_ <- NULL
data$dep_child <- factor(data$dep_child, levels = c("No Deprivation", "Deprivation"), labels = c("0", "1"))

# Split the data into training and testing sets
trainIndex <- createDataPartition(data$dep_child, p = .8, list = FALSE, times = 1)
data_train <- data[trainIndex, ]
data_test <- data[-trainIndex, ]

# Train the Random Forest model with adjusted sampling
set.seed(123)
rf_model <- randomForest(
  dep_child ~ .,
  data = data_train,
  importance = TRUE,
  ntree = 200,
  nodesize = 1,
  mtry = 2,
  sampsize = c(900, min(table(data_train$dep_child)))  # Equal sampling from both classes
)

# Prediction and evaluation on the test set
predicted_classes <- predict(rf_model, data_test)
conf_matrix <- confusionMatrix(predicted_classes, data_test$dep_child, positive = "1")
print(conf_matrix)

# Plot variable importance
varImpPlot(rf_model)

################## LOGISTIC MODEL #################

# Train a logistic regression model
glm_model <- glm(
  dep_child ~ ., 
  data = data,  
  family = binomial(link = "logit")  # Logistic regression 
)
summary(glm_model)

# Display coefficients of the model
coefficients <- summary(glm_model)$coefficients
print(coefficients)

# Sort coefficients by absolute value
coefficients_sorted <- coefficients[order(abs(coefficients[, "Estimate"]), decreasing = TRUE), ]

# Display the top 10 most important variables by influence
print("Top 10 Most Important Variables by Influence (Absolute Value of Coefficients):")
print(coefficients_sorted[1:10, ])

# Predict on the test set
predicted_probs <- predict(glm_model, newdata = data_test, type = "response")

# Calculate and plot ROC curve and AUC
library(pROC)
roc_curve <- roc(data_test$dep_child, predicted_probs)
plot(roc_curve)
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))

# Display distribution of the dependent variable
table(data$dep_child)

