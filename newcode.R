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
    
    dep_c_three_meals = if_else(h22803_1 == "Ja", 0, if_else(h22803_1 %in% c("Nein aus finanziellen Gründen", "Nein aus anderen Gründen"), 1, NA_real_)),
    
    dep_c_quality_meal = if_else(h22803_2 == "Ja", 0, if_else(h22803_2 %in% c("Nein aus finanziellen Gründen", "Nein aus anderen Gründen"), 1, NA_real_)),
    
    dep_c_fruits = if_else(h22803_3 == "Ja", 0, if_else(h22803_3 %in% c("Nein aus finanziellen Gründen", "Nein aus anderen Gründen"), 1, NA_real_)),
    
    dep_c_books = if_else(h22803_4 == "Ja", 0, if_else(h22803_4 %in% c("Nein aus finanziellen Gründen", "Nein aus anderen Gründen"), 1, NA_real_)),
    
    dep_c_outdoor = if_else(h22803_5 == "Ja", 0, if_else(h22803_5 %in% c("Nein aus finanziellen Gründen", "Nein aus anderen Gründen"), 1, NA_real_)),
    
    dep_c_leisure = if_else(h22803_6 == "Ja", 0, if_else(h22803_6 %in% c("Nein aus finanziellen Gründen", "Nein aus anderen Gründen"), 1, NA_real_)),
    
    dep_c_indoor = if_else(h22803_7 == "Ja", 0, if_else(h22803_7 %in% c("Nein aus finanziellen Gründen", "Nein aus anderen Gründen"), 1, NA_real_)),
    
    dep_c_clothes = if_else(h22803_10 == "Ja", 0, if_else(h22803_10 %in% c("Nein aus finanziellen Gründen", "Nein aus anderen Gründen"), 1, NA_real_)),
    
    dep_c_shoes = if_else(h22803_11 == "Ja", 0, if_else(h22803_11 %in% c("Nein aus finanziellen Gründen", "Nein aus anderen Gründen"), 1, NA_real_)),
    
    dep_c_meet = if_else(h22803_12 == "Ja", 0, if_else(h22803_12 %in% c("Nein aus finanziellen Gründen", "Nein aus anderen Gründen"), 1, NA_real_)),
    
    dep_c_celeb = if_else(h22803_13 == "Ja", 0, if_else(h22803_13 %in% c("Nein aus finanziellen Gründen", "Nein aus anderen Gründen"), 1, NA_real_)),
    
    dep_c_holiday = if_else(h22803_14 == "Ja", 0, if_else(h22803_14 %in% c("Nein aus finanziellen Gründen", "Nein aus anderen Gründen"), 1, NA_real_))
    
  )

# weights for deprivation variable

weights <- c(
  
  dep_c_three_meals = 1.5,
  
  dep_c_quality_meal = 1.2,
  
  dep_c_fruits = 1.0,
  
  dep_c_books = 1.3,
  
  dep_c_outdoor = 1.1,
  
  dep_c_leisure = 1.4,
  
  dep_c_indoor = 1.0,
  
  dep_c_clothes = 1.3,
  
  dep_c_shoes = 1.1,
  
  dep_c_meet = 1.5,
  
  dep_c_celeb = 1.0,
  
  dep_c_holiday = 1.2
  
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
      
      help <= 1.5 ~ "No Deprivation",
      
      help > 1.5 ~ "Deprivation"
      
    )
  )  %>%
  
  ungroup()


# Convert to factor with simplified classes

data8$dep_child <- factor(data8$dep_child, levels = c("No Deprivation", "Deprivation"), labels = c("0","1"))



# Load and apply a list of variables to exclude based on external file

file_path <- "Varlist20240722.xlsx"

varlist <- readxl::read_excel(file_path)

included_columns <- varlist %>%
  
  filter(`Exclude in next step` == "a") %>%
  
  pull(`Varname`)

data9 <- data8 %>% select(all_of(included_columns))


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


# Change some datatypes to prevent NAs, especially on these ones, recode income

data9$Alter_der_Person_in_Jahren_Vater_ <- as.numeric(data9$Alter_der_Person_in_Jahren_Vater_)

data9$Alter_der_Person_in_Jahren_Mutter_ <- as.numeric(data9$Alter_der_Person_in_Jahren_Vater_)



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

# Conduct imputation

data10 <- replace_na_values(data9)

# Delete all deprivation dimensions to prevent overfitting

data11 <- data10 %>%
  
  select(-starts_with("Deprivations"))


# Filter out near-zero variance features

nzv <- nearZeroVar(data11, saveMetrics = TRUE)

data12 <- data11[, !nzv$nzv]

# Check deleted columns
names(data11)


# Delete IDs to prevent overfitting

data12$Eindeutige_Personennummer_ <- NULL

data12$HHLFD_ <- NULL



####### treating colder + high correlation

# function to calculate correlation between categorical variables

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

remove_highly_correlated_categorical <- function(data, threshold = 0.2) {
  
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

remove_highly_correlated_numerical <- function(data, threshold = 0.8) {
  
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

# data18$dep_child <- factor(data18$dep_child, levels = c("No Deprivation", "Deprivation" ), labels = c("0", "1"))

# VIF Calculation

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

names(data20)

#################### RANDOM FOREST #################### 



# Separate into trainings and test data

# colnames makes problems with rf implementation --> change columnnames

colnames(data20) <- make.names(colnames(data20))

# recode deprivation variable

data20$dep_child <- factor(data20$dep_child , levels = c("1", "2"), labels = c("0","1"))

# separate data set

trainIndex <- createDataPartition(data20$dep_child, p = 0.8, list = FALSE, times = 1)

data_train <- data20[trainIndex, ]

data_test <- data20[-trainIndex, ]



# Calculate class weights

class_weights <- table(data_train$dep_child)

class_weights <- max(class_weights) / class_weights


# Train random forest 

set.seed(123)


rf_model <- ranger( dep_child ~ .,
                    
                    data = data_train,
                    
                    importance = 'impurity',
                    
                    num.trees =300,
                    
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

print(var_importance)

varImpPlot <- as.data.frame(var_importance)

varImpPlot$Variable <- rownames(varImpPlot)

varImpPlot <- varImpPlot[order(-varImpPlot$var_importance), ]  # Sortieren nach Wichtigkeit

ggplot(varImpPlot, aes(x = reorder(Variable, var_importance), y = var_importance)) +
  
  geom_bar(stat = "identity") +
  
  coord_flip() +
  
  xlab("Variable") +
  
  ylab("Importance") +
  
  ggtitle("Variable Importance Plot")


# Plot partial dependence plots

# Random Forest Modell trainieren

set.seed(123)

rf_model <- ranger(dep_child ~ ., 
                   data = data_train, 
                   importance = 'impurity', 
                   num.trees = 500, 
                   case.weights = class_weights[as.character(data_train$dep_child)],
                   probability = TRUE)

# exctract variable importance

var_importance <- rf_model$variable.importance

varImpPlot <- as.data.frame(var_importance)

varImpPlot$Variable <- rownames(varImpPlot)

varImpPlot <- varImpPlot[order(-varImpPlot$var_importance), ]

ggplot(varImpPlot, aes(x = reorder(Variable, var_importance), y = var_importance)) +
  
  geom_bar(stat = "identity") +
  
  coord_flip() +
  
  xlab("Variable") +
  
  ylab("Importance") +
  
  ggtitle("Variable Importance Plot")

# generate Partial Dependence Plots

top_vars <- head(varImpPlot$Variable, 5)

# loop to plot
for (var in top_vars) {
  pd <- partial(rf_model, pred.var = var, train = data_train)
  print(plotPartial(pd, main = paste("Partial Dependence Plot for", var)))
}

# Partial Dependence Plot visualisieren

plot(pd$persönliches_Nettoeinkommen_Mutter_, pd$yhat, type = "l",
     
     xlab = "Persönliches Nettoeinkommen der Mutter",
     
     ylab = "Vorhergesagte Wahrscheinlichkeit für Deprivation",
     
     main = "Partial Dependence Plot")



# Plot confusion matrix

# extract confusionsmatrix

conf_matrix_table <- as.table(conf_matrix$table)

# convert into df

conf_matrix_df <- as.data.frame(conf_matrix_table)

# plot as heatmap

ggplot(data = conf_matrix_df, aes(x = Prediction, y = Reference, fill = Freq)) +
  
  geom_tile(color = "white") +
  
  geom_text(aes(label = Freq), vjust = 1) +
  
  scale_fill_gradient(low = "white", high = "#006600") +
  
  theme_minimal() +
  
  labs(title = "Confusion Matrix", x = "Predicted", y = "Actual")




