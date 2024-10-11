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
library(ranger)

############### DATA CLEANING ##############

# Function to replace NA values using KNN imputation

replace_na_values <- function(df) {

  # Define the codes for missing values
  missing_codes <- c(-1, -2, -3, -4, -5, -6, -7, -8, -9, -10, -11, -99, -999)

  # Replace missing codes with NA
  df <- df %>%
    mutate(across(where(is.numeric), ~ ifelse(. %in% missing_codes, NA, .))) %>%
    mutate(across(where(is.character), ~ ifelse(. %in% as.character(missing_codes), NA, .)))

  # Store original types
  original_types <- sapply(df, class)

  # Defensive Imputation
  # For numeric variables, replace NAs with the median to minimize impact
  df <- df %>%
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

  # For categorical variables, replace NAs with the mode, or a placeholder if all NAs
  df <- df %>%
    mutate(across(where(is.factor), ~ ifelse(is.na(.), get_mode_or_placeholder(.), .)))

  # Restore original types for categorical variables
  df <- df %>%
    mutate(across(names(df)[original_types == "factor"], as.factor))

  return(df)
}





# Helper function to calculate the mode, or return "Unbekannt" if all values are NA
get_mode_or_placeholder <- function(v) {
  if (all(is.na(v))) {
    return("Unbekannt")
  } else {
    uniqv <- unique(v)
    return(uniqv[which.max(tabulate(match(v, uniqv)))])
  }
}



# Load data from SPSS file

data <- read.spss("dji_suf_personen.sav", to.data.frame = TRUE)

# Convert specific variables to numeric for processing

data2 <- data %>%
  
  mutate(XALTER = as.numeric(as.character(XALTER)))

# Filter the data based on specific conditions

data3 <- data2 %>%
  
  filter(IZP == "trifft zu" & XALTER < 12)


# Convert and recode variables for region (east vs. west Germany)

data4 <- data3 %>%
  
  mutate(bland_numeric = as.numeric(bland)) %>%
  
  mutate(east = case_when(
    
    bland_numeric <= 10 ~ 0,
    
    bland_numeric > 10 & bland_numeric <= 16 ~ 1,
    
    TRUE ~ NA_real_
    
  )) %>%
  
  mutate(east = factor(east, levels = c(0, 1), labels = c("West Germany", "East Germany (incl. Berlin)"))) %>%
  
  select(-bland_numeric) 

attr(data4$east, "label") <- "Region"


# Calculate deprivation variable

data5 <- data4 %>%
  
  mutate(
    
    dep_c_three_meals = if_else(h22803_1 == "Nein aus finanziellen Gründen", 1, 0),
    
    dep_c_quality_meal = if_else(h22803_2 == "Nein aus finanziellen Gründen", 1, 0),
    
    dep_c_fruits = if_else(h22803_3 == "Nein aus finanziellen Gründen", 1, 0),
    
    dep_c_books = if_else(h22803_4 == "Nein aus finanziellen Gründen", 1, 0),
    
    dep_c_outdoor = if_else(h22803_5 == "Nein aus finanziellen Gründen", 1, 0),
    
    dep_c_leisure = if_else(h22803_6 == "Nein aus finanziellen Gründen", 1, 0),
    
    dep_c_indoor = if_else(h22803_7 == "Nein aus finanziellen Gründen", 1, 0),
    
    dep_c_clothes = if_else(h22803_10 == "Nein aus finanziellen Gründen", 1, 0),
    
    dep_c_shoes = if_else(h22803_11 == "Nein aus finanziellen Gründen", 1, 0),
    
    dep_c_meet = if_else(h22803_12 == "Nein aus finanziellen Gründen", 1, 0),
    
    dep_c_celeb = if_else(h22803_13 == "Nein aus finanziellen Gründen", 1, 0),
    
    dep_c_holiday = if_else(h22803_14 == "Nein aus finanziellen Gründen", 1, 0)
    
  )


# weights for deprivation variable

weights <- c(

  dep_c_three_meals = 1,

  dep_c_quality_meal = 1,

  dep_c_fruits = 1,

  dep_c_books = 1,

  dep_c_outdoor = 1,

  dep_c_leisure = 1,

  dep_c_indoor = 1,

  dep_c_clothes = 1,

  dep_c_shoes = 1,

  dep_c_meet = 1,

  dep_c_celeb = 1,

  dep_c_holiday = 1

)



# use weights on dimensions and create final deprivation variable based on dimension

# threshold is adapted

data6 <- data5 %>%
  
  rowwise() %>%
  
  mutate(
    
    help = sum(
      
      dep_c_three_meals * weights["dep_c_three_meals"],
      
      dep_c_quality_meal * weights["dep_c_quality_meal"],
      
      dep_c_fruits * weights["dep_c_fruits"],
      
      dep_c_books * weights["dep_c_books"],
      
      dep_c_outdoor * weights["dep_c_outdoor"],
      
      dep_c_leisure * weights["dep_c_leisure"],
      
      dep_c_indoor * weights["dep_c_indoor"],
      
      dep_c_clothes * weights["dep_c_clothes"],
      
      dep_c_shoes * weights["dep_c_shoes"],
      
      dep_c_meet * weights["dep_c_meet"],
      
      dep_c_celeb * weights["dep_c_celeb"],
      
      dep_c_holiday * weights["dep_c_holiday"],
      
      na.rm = TRUE
      
    ),
    
    help2 = sum(is.na(c_across(starts_with("dep_c_"))))
    
  )  %>%
  
  ungroup()


# fitler cases with na in dep_c_ variables

data7 <- data6 %>%
  
  filter(help2 != 12)



# setting threshold for deprivation

data8 <- data7 %>%
  
  rowwise() %>%
  
  mutate(
    
    dep_child = case_when(
      
      help <= 0 ~ "No Deprivation",
      
      help > 0 ~ "Deprivation"
      
    )
  )  %>%
  
  ungroup()

#   0    1 
# 3182 2314 

# Load necessary libraries
library(dplyr)


# List of relevant variables to keep
relevant_vars <- c(
  "HHLFD", "INTNR", "GESCHLECHT", "XALTER", 
  "h11008", "h11008_mu", "h11008_va", "h11008_omamu", "h11008_opamu", "h11008_omava", "h11008_opava", 
  "h91102", "h91107", "h91109", "h91110", "h91061", "h91113", "h91114", 
  "h71338", "h71332", "h71350", "h51117", 
  "h35854", "h35855", "h35856", "h35857", 
  "h35017_1", "h35017_2", "h35017_3", "h35017_4", "h35017_5", "h35017_6", "h35017_7", 
  "h33106_1", "h33106_2", "h33106_3", "h33106_4", "h33106_5", "h33106_6", 
  "h33117_1", "h33117_2", "h33117_3", "h33117_4", "h33117_5", "h33117_6", 
  "h33109_1", "h33109_2", "h33109_3", "h33109_4", 
  "h33120", "h33121", "h33212_1", "h33212_2", "h33212_3", "h33212_4", "h33212_5", "h33212_6", "h33212_7", 
  "h33213", 
  "v62022_1", "v62023_1", "v62022_2", "v62023_2", 
  "h79859_1", "h79859_2", "h79859_3", "h79859_4", 
  "k_sorgerecht_1", "k_sorgerecht_2", "k_natio", "k_NEPSmig", "k_hkland", "k_AnzETHH", 
  "m_Anzahl_Erz_Kind", "k_aktiv", "k_muttersprache1", "k_muttersprache2", 
  "k_othersprache1", "k_othersprache2", "k_othersprache3", "k_othersprache4", 
  "k_othersprache5", "k_othersprache6", 
  "k_migra_sprache", 
  "hh11001", "hh11066", "hh11067", "hh11070_1", "hh11070_2", "hh11070_3", 
  "hh11073", "hh11075", "hh11078", "hh11079", 
  "k_haushaltstyp_hh", "k_familientyp_hh", "k_aequi_min_hh", "k_aequi_max_hh", "k_aequi_mean_hh", 
  "k_arm60p_hh", "k_OSSsum_hh", "k_OSScat_hh", "k_deprivationsindex_hh", 
  "gkbik10", "polgk", "bland", "casa_typ", "casa_hh", "casa_dist_opnv", 
  "h11014_mu", "h11014_va", "h11015_mu", "h11015_va", "h11018_mu", "h11018_va", "h11019_mu", "h11019_va", 
  "h11021_mu", "h11021_va", "h11022_mu", "h11022_va", "h11024_mu", "h11024_va", 
  "h11025_mu", "h11025_va", "h11026_mu", "h11026_va", 
  "h71020_mu", "h71020_va", "h71305_mu", "h71305_va", "h71307_mu", "h71308_mu", "h71308_va", 
  "h71310_mu", "h71310_va", "h71312_mu", "h71312_va", "h71314_mu", "h71317_mu", "h71317_va", 
  "h71318_mu", "h71318_va", "h71319_mu", "h71319_va", "h71321_mu", "h71321_va", 
  "h71323_mu", "h71323_va", "h71327_mu", "h71327_va", "h71328_mu", "h71328_va", 
  "h71332_mu", "h71332_va", "h71338_mu", "h71338_va", "h71401_mu", "h71401_va", 
  "h71404_mu", "h71404_va", "h91061_mu", "h91061_va", "h91102_mu", "h91102_va", 
  "h91114_mu", "h91114_va", 
  "h91115_1_mu", "h91115_1_va", "h91115_2_mu", "h91115_2_va", "h91115_3_mu", "h91115_3_va", 
  "h91115_4_mu", "h91115_4_va", "h91115_5_mu", "h91115_5_va", "h91115_6_mu", "h91115_6_va", 
  "h91115_7_mu", "h91115_7_va", "h91115_8_mu", "h91115_8_va", "h91115_9_mu", "h91115_9_va", 
  "h91115_10_mu", "h91115_10_va", "h91115_11_mu", "h91115_11_va", 
  "k_bija_mu", "k_bija_va", "k_HatPartner_mu", "k_HatPartner_va", 
  "k_isced2011_mu", "k_isced2011_va", "k_natio_mu", "k_natio_va", 
  "k_muttersprache1_mu", "k_muttersprache1_va", "XALTER_mu", "XALTER_va", 
  "k_extern_mu", "k_extern_va", "k_ZP_mit_Eltern", 
  "k_isco08_mu3", "k_isco08_va3", "k_eseg_2_mu", "k_eseg_2_va","dep_child", "east"

)

# Select only the relevant variables
data9 <- data8 %>% select(all_of(relevant_vars))

# Convert to factor with simplified classes
# 
# data9$dep_child <- factor(data9$dep_child, levels = c("No Deprivation", "Deprivation"), labels = c("0","1"))



# Load Excel file with column names and labels, and rename columns accordingly

var_list <- read_excel("VarListePersonen.xlsx")

colnames_mapping <- setNames(var_list$Label, var_list$`colnames(daten_personen)`)

new_colnames <- colnames(data9)

for (i in seq_along(new_colnames)) {
  
  if (new_colnames[i] %in% names(colnames_mapping)) {
    
    new_colnames[i] <- colnames_mapping[new_colnames[i]]
    
  }
  
}

colnames(data9) <- new_colnames



# Delete variables which are not useful

data9$`Deprivationsindex_(Subskala_finanzielle_Deprivatiton)_` <- NULL

data9$Äquivalenzeinkommen_Untergrenze_ <- NULL

data9$Äquivalenzeinkommen_Untergrenze_ <- NULL

data9$Äquivalenzeinkommen_Obergrenze_ <- NULL

data9$`Aktueller_oder_letzter_Beruf_Mutter_(ISCO-08)_` <- NULL

data9$`Aktueller_oder_letzter_Beruf_Vater_(ISCO-08)_` <- NULL

data9$`Möbel_ersetzen_[Deprivationsindex_Finanzen]_` <- NULL

data9$`Unerwartete_Ausgaben_bezahlen_[Deprivationsindex…_` <- NULL

data9$Eindeutige_Personennummer_ <- NULL

data9$HHLFD_ <- NULL

data9$`Monatlich_sparen_[Deprivationsindex_Finanzen]_` <- NULL



# Prepare features for the data imputation

# Change some datatypes to prevent NAs, especially on these ones, recode income

# AAges

data9$Alter_der_Person_in_Jahren_Vater_ <- as.numeric(data9$Alter_der_Person_in_Jahren_Vater_)

data9$Alter_der_Person_in_Jahren_Mutter_ <- as.numeric(data9$Alter_der_Person_in_Jahren_Vater_)


#income

data9$persönliches_Nettoeinkommen_Mutter_<- as.numeric(as.character(factor(data9$persönliches_Nettoeinkommen_Mutter_,
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


data9$persönliches_Nettoeinkommen_Vater_ <- as.numeric(as.character(factor(data9$persönliches_Nettoeinkommen_Vater_,
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


# Function to remove columns with more than a specified number of NAs
remove_columns_with_nas <- function(df, na_threshold = 2000) {
  # Calculate the number of NAs in each column
  na_counts <- sapply(df, function(x) sum(is.na(x)))
  
  # Identify columns with NA count greater than the threshold
  columns_to_remove <- names(na_counts[na_counts > na_threshold])
  
  # Remove these columns from the dataframe
  df_cleaned <- df[, !names(df) %in% columns_to_remove]
  
  return(df_cleaned)
}


# remove all features which have got a lot of NAs

data10 <- remove_columns_with_nas(data9, na_threshold = 1000)

# Replace NAs

data11 <- replace_na_values(data10)

# Check

names(data11)


# Identify near-zero variance variables, excluding dep_child
nzv <- nearZeroVar(data11 %>% select(-dep_child), saveMetrics = TRUE)

# List of columns to keep (excluding NZV variables and keeping dep_child)
columns_to_keep <- names(data11)[!nzv$nzv]

# Check if dep_child is already in the list and add it if it's not
if (!"dep_child" %in% columns_to_keep) {
  
  columns_to_keep <- c(columns_to_keep, "dep_child")
}

# Subset data to keep only the desired columns

data12 <- data11[, columns_to_keep]

#Check

names(data12)
# Delete IDs to prevent overfitting


data12$HHLFD_ <- NULL
data12$Eindeutige_Personennummer_ <- NULL


####### treating colder + high correlation

# function to calculate correlation between categorical variables

library(infotheo)

mutualInformation <- function(x, y) {
  
  mi <- mutinformation(discretize(x), discretize(y))
  
  return(mi)
}

# Function to calculate correlations between categorical variables and dep_child

cat_correlations <- function(data, dep_var) {
  
  cat_results <- data.frame(Variable = character(), MutualInfo = numeric(), stringsAsFactors = FALSE)
  
  cat_vars <- data %>% select(where(is.factor))
  
  for (var in colnames(cat_vars)) {
    
    mi_value <- mutualInformation(cat_vars[[var]], data[[dep_var]])
    
    
    cat_results <- rbind(cat_results, data.frame(Variable = var, MutualInfo = mi_value, stringsAsFactors = TRUE))
    
  }
  
  return(cat_results)
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


# Calculate correlations numeric

numeric_corrs <- num_correlations(data12 , "dep_child")

numeric_corrs <- numeric_corrs %>% rename(Score = Correlation)


# Calculate correlations categorical

categorical_corrs <- cat_correlations(data12 , "dep_child")

categorical_corrs <- categorical_corrs %>% rename(Score = MutualInfo)

# identify colliders

potential_colliders <- rbind(
  
  numeric_corrs %>% filter(abs(Score) > 0.5),
  
  categorical_corrs %>% filter(Score > 0.1)
  
)


collider_vars <- potential_colliders$Variable

collider_vars <- setdiff(collider_vars, "dep_child")

# Remove collider variables from the dataset

data13 <- data12 %>% select(-all_of(collider_vars))

# check removed columns

names(data13)

## Check NaNs and Inf

# Remove or replace NaN values

data14 <- data13 %>% 
  
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), median(., na.rm = TRUE), .)))

# Remove or replace Inf values

data15 <- data14 %>% 
  
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), median(., na.rm = TRUE), .)))

# Multicollineatiry

# Delete highly correlated categorical variables

remove_highly_correlated_categorical <- function(data, threshold = 0.1) {
  
  categorical_cols <- names(data)[sapply(data, is.factor)]
  
  to_keep <- categorical_cols
  
  for (i in 1:(length(categorical_cols) - 1)) {
    
    for (j in (i + 1):length(categorical_cols)) {
      
      var1 <- categorical_cols[i]
      
      var2 <- categorical_cols[j]
      
      mi_value <- mutualInformation(data[[var1]], data[[var2]])
      
      if (!is.na(mi_value) && mi_value > threshold) {
        
        if (var2 %in% to_keep && var2 != "dep_child") {
          
          to_keep <- setdiff(to_keep, var2)
          
        }
        
      }
      
    }
    
  }
  
  remaining_data <- data[c(to_keep, setdiff(names(data), categorical_cols))]
  
  return(remaining_data)
  
}

# remove highly correlated categorical variables

data16 <- remove_highly_correlated_categorical(data15)


#check

names(data16)


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

# Conduct Removing of highly correlated numerical variables

data17<- remove_highly_correlated_numerical(data16)

# check deleted columns

names(data17)

# Check for multicollinearity and remove variables with high VIF

# Delete NaN and Inf again

data18 <- data17 %>% 
  
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), median(., na.rm = TRUE), .)))

data19 <- data18 %>% 
  
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), median(., na.rm = TRUE), .)))


# Recode deprivation variable

data19$dep_child <- factor(data19$dep_child, levels = c("No Deprivation", "Deprivation" ), labels = c("0", "1"))

# VIF Calculation
colnames( data19) <- make.names(colnames( data19))

vif_values <- vif(lm(as.numeric(dep_child) ~ ., data = data19))

if (is.matrix(vif_values)) {
  
  table <- data.frame(Variable = rownames(vif_values), 
                      
                      GVIF = vif_values[, "GVIF"], 
                      
                      Df = vif_values[, "Df"], 
                      
                      GVIF_norm = vif_values[, "GVIF"]^(1/(2*vif_values[, "Df"])))
  
  filtered_table <- table %>% filter(GVIF_norm > 5)
  
  data20 <- data19 %>% select(-all_of(filtered_table$Variable))
  
} else {
  
  table <- data.frame(Variable = names(vif_values), VIF = vif_values)
  
  filtered_table <- table %>% filter(VIF > 5)
  
  data20 <- data19 %>% select(-all_of(filtered_table$Variable))
  
}

# check names

datanames <- as.data.frame(names(data20))
df <- write.csv(datanames, "/Users/anilcaneldiven/Desktop/data_names.csv")
#################### RANDOM FOREST #################### 


# colnames makes problems with rf implementation --> change columnnames

colnames(data20) <- make.names(colnames(data20))

# name vector to rename columns
colnames_mapping <- c(
  "Geschlecht_" = "Gender",
  "Geburtsland_Mutter_" = "Mother_Birth_Country",
  "Gesundheitszustand_" = "Health_Status",
  "Behinderung_" = "Disability",
  "Aktivitätsstatus_differenziert_" = "Activity_Status_Detailed",
  "andere_Sprache_1_" = "Other_Language_1",
  "Pflegebedürftige_Personen_im_Haushalt_" = "People_Need_Care_in_Household",
  "Bezug_ALG_II_bzw._Hartz_IV_" = "Receiving_ALG_II_or_Hartz_IV",
  "Externe_Kinder_" = "External_Children",
  "Haushaltstyp_" = "Household_Type",
  "OSLO.3_Skala_Soziale_Unterstützung_kategorial_" = "OSLO_3_Scale_Social_Support_Categorical",
  "BIK.GK_10_" = "BIK_GK_10",
  "Gebäudetypologie_in_Klassen_" = "Building_Typology_Classes",
  "Aktuelle_Erwerbstätigkeit_Mutter_" = "Current_Employment_Mother",
  "Aktuelle_Erwerbstätigkeit_Vater_" = "Current_Employment_Father",
  "Nebenerwerbstätigkeit_Mutter_" = "Secondary_Employment_Mother",
  "Behinderung_Mutter_" = "Disability_Mother",
  "Alter_der_Person_in_Jahren_" = "Age_in_Years",
  "Anzahl_Elternteile_i.w.S._einer_ZP_im_Haushalt_" = "Number_of_Parents_in_Household",
  "Anzahl_realisierter_Erziehungsmodule_pro_Kind_" = "Number_of_Parenting_Modules_Per_Child",
  "Anzahl_Personen_im_Haushalt_" = "Number_of_People_in_Household",
  "Anzahl_Kinderzimmer_" = "Number_of_Children_Rooms",
  "Äquivalenzeinkommen_Intervallmitte_" = "Equivalized_Income_Midpoint",
  "OSLO.3_Skala_Soziale_Unterstützung_" = "OSLO_3_Scale_Social_Support",
  "Anzahl_Privathaushalte_" = "Number_of_Private_Households",
  "Entfernung_zur_nächsten_ÖPNV.Haltest._.in_Metern._" = "Distance_to_Nearest_Public_Transport_in_Meters",
  "persönliches_Nettoeinkommen_Mutter_" = "Personal_Net_Income_Mother",
  "Bildungsjahre_nach_Ostermeier.Blossfeld_1998_Mutter_" = "Years_of_Education_Mother",
  "Bildungsjahre_nach_Ostermeier.Blossfeld_1998_Vater_" = "Years_of_Education_Father",
  "Alter_der_Person_in_Jahren_Mutter_" = "Age_of_Mother_in_Years",
  "Person_lebt_nicht_im_Haushalt_Vater" = "Father_Not_Living_in_Household"
)

# Rename the columns using the mapping, keeping 'dep_child' unchanged
colnames(data20) <- ifelse(
  colnames(data20) %in% names(colnames_mapping), 
  colnames_mapping[colnames(data20)], 
  colnames(data20)
)



data21 <- data20



data21$Father_Not_Living_in_Household <- factor(data21$Father_Not_Living_in_Household)


data21$Equivalized_Income_Midpoint <- NULL
data21$Gender <- NULL
data21$Disability <- NULL
data21$Health_Status <- NULL
data21$Disability_Mother <-NULL




# separate data set
# remove outliers
remove_outliers_zscore <- function(df, threshold = 4) {
  # z-score to numeirc
  df_numeric <- df %>% select(where(is.numeric))
  z_scores <- as.data.frame(scale(df_numeric))  
  df_no_outliers <- df[apply(z_scores, 1, function(row) all(abs(row) <= threshold)), ]
  
  return(df_no_outliers)
}

data_cleaned <- remove_outliers_zscore(data21)  # Use Z-score method to remove outliers
data_cleaned$Personal_Net_Income_Mother <- NULL
# data_cleaned$Years_of_Education_Father <- NULL
# data_cleaned$Number_of_Parents_in_Household <- NULL
# str(data_cleaned)
# Exportiere die bereinigten Daten als CSV-Datei
write.csv(data_cleaned, "/Users/anilcaneldiven/Desktop/data_cleaned.csv", row.names = FALSE)


trainIndex <- createDataPartition(data_cleaned$dep_child, p = 0.9, list = FALSE, times = 1)

data_train <- data_cleaned[trainIndex, ]

data_test <- data_cleaned[-trainIndex, ]


# Calculate class weights

class_weights <- table(data_train$dep_child)

class_weights <- max(class_weights) / class_weights



# Train random forest 

#set.seed(123)
str(data_train)

rf_model <- ranger( dep_child ~ .,
                    
                    data =data_train,
                    
                    importance = 'permutation',
                    
                    num.trees =300,
                    mtry = 6,
                    
                    case.weights = class_weights[as.character(data_train$dep_child)]
)







# predictions on test data

predicted_classes <- predict(rf_model, data_test)$predictions

# Calculate confusion matrix

conf_matrix <- confusionMatrix(predicted_classes, data_test$dep_child, positive = "1")

print(conf_matrix)




# Calculate Precision, Recall and F1-Score 

precision <- conf_matrix$byClass['Pos Pred Value']

recall <- conf_matrix$byClass['Sensitivity']

f1_score <- 2 * ((precision * recall) / (precision + recall))

# print results

cat("Precision: ", precision, "\n")

cat("Recall: ", recall, "\n")

cat("F1-Score: ", f1_score, "\n")


######### Graphics #############

# Plot variable importance

var_importance <- rf_model$variable.importance

#print(var_importance)

varImpPlot <- as.data.frame(var_importance)

varImpPlot$Variable <- rownames(varImpPlot)

varImpPlot <- varImpPlot[order(-varImpPlot$var_importance), ]  # Sortieren nach Wichtigkeit

ggplot(varImpPlot, aes(x = reorder(Variable, var_importance), y = var_importance)) +
  
  geom_bar(stat = "identity") +
  
  coord_flip() +
  
  xlab("Variable") +
  
  ylab("Importance") +
  
  ggtitle("Variable Importance Plot")


############################

