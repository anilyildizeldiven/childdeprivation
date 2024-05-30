# Installiere und lade die notwendigen Pakete
library(readxl)
library(dplyr)
library(foreign)
library(caret)
library(glmnet)
library(car)
library(carData)
library(FSelectorRcpp)
library(infotheo)

relevante_variablen2 <- c(
  "INTNR",
  "IZP",
  "k_extern",
  "m_Zu_Befragen_HH",
  "m_Befragt_in",
  "k_IstETvonZP",
  "k_IstETvonZP17",
  "PID",
  "BEFRPERS",
  "GEBMON",
  "GEBJAHR",
  "ERSTWOHNSITZ",
  "h11008_omamu",
  "h11008_opamu",
  "h11008_omava",
  "h11008_opava",
  "h11021",
  "h11022",
  "h11025",
  "Trenner",
  "h91202",
  "h91102",
  "h91107",
  "h91109",
  "h91112_1",
  "h91112_2",
  "h91112_3",
  "h91112_4",
  "h91112_5",
  "h91112_6",
  "h91112_7",
  "h91112_8",
  "h91112_9",
  "h91112_10",
  "h91112_11",
  "h91112_12",
  "h91112_13",
  "h91112_14",
  "h91112_15",
  "h91112_16",
  "h91112_17",
  "h91112_18",
  "h91112_19",
  "h91112_20",
  "h91112_21",
  "h91112_22",
  "h91112_23",
  "h91112_24",
  "h91112_25",
  "h91112_26",
  "h91112_27",
  "h91112_28",
  "h91112_29",
  "h91112_30",
  "h91112_31",
  "h91112_32",
  "h91112_33",
  "h91112_34",
  "h91112_35",
  "h91112_36",
  "h91112_37",
  "h91112_38",
  "h91112_39",
  "h91112_40",
  "h91112_41",
  "h91112_42",
  "h91112_43",
  "h91112_44",
  "h91115_1",
  "h91115_2",
  "h91115_3",
  "h91115_4",
  "h91115_5",
  "h91115_6",
  "h91115_7",
  "h91115_8",
  "h91115_9",
  "h91115_10",
  "h91115_11",
  "Trenner2",
  "h71310",
  "h71312",
  "h71314",
  "h71317",
  "h71352_1",
  "h71352_2",
  "h71352_3",
  "h71352_4",
  "h71352_5",
  "h71352_6",
  "h71352_7",
  "h71352_8",
  "h71352_9",
  "h71355_1",
  "h71355_2",
  "h71355_3",
  "h71355_4",
  "h71355_5",
  "h71356_1",
  "h71356_2",
  "h71356_3",
  "h71356_4",
  "h71356_5",
  "h71356_6",
  "h71356_7",
  "h71357",
  "h77100",
  "h74403",
  "h74404",
  "h74405_1",
  "h74405_2",
  "h74406",
  "h55867_1",
  "h55867_2",
  "h35858_1",
  "h35858_2",
  "h35858_3",
  "h35858_4",
  "h35017_1",
  "h35017_2",
  "h35017_3",
  "h35017_4",
  "h35017_5",
  "h35017_6",
  "h35017_7",
  "h35017_8",
  "h35017_9",
  "h35017_10",
  "h35017_11",
  "h35017_12",
  "h35017_13",
  "h35017_14",
  "h35017_15",
  "h35017_16",
  "h35017_17",
  "h35017_18",
  "h35017_19",
  "h35017_20",
  "h33501",
  "h33500",
  "h33510_1",
  "h33510_2",
  "h33510_3",
  "h33510_4",
  "h33510_5",
  "h33510_6",
  "h33115_1",
  "h33115_2",
  "h33115_3",
  "h33115_4",
  "h33115_5",
  "h33115_6",
  "h33115_7",
  "h33115_8",
  "h33115_9",
  "h33115_10",
  "h33115_11",
  "h33306",
  "h33850_8",
  "h33850_9",
  "h33850_1",
  "h33850_2",
  "h33850_3",
  "h33850_10",
  "h33850_11",
  "h33850_4",
  "h33101_1",
  "h33101_2",
  "h33101_3",
  "h33101_4",
  "h33101_5",
  "h33101_6",
  "h33101_7",
  "h22336_2",
  "h22336_3",
  "h22336_4",
  "h22336_5",
  "h22336_6",
  "h22336_7",
  "h22336_8",
  "h22336_9",
  "h22336_10",
  "Trenner5",
  "v62014_1",
  "v62014_2",
  "v62014_3",
  "v62014_4",
  "v62014_5",
  "v62014_6",
  "v62014_7",
  "v62014_8",
  "v62014_9",
  "v62014_10",
  "v62014_11",
  "v62014_12",
  "v62014_13",
  "v62014_14",
  "v62014_15",
  "v62014_16",
  "v62014_17",
  "v62014_18",
  "v62014_19",
  "v62014_20",
  "v62014_21",
  "v62014_22",
  "v62014_23",
  "v62014_24",
  "v62014_25",
  "m_hhmitzp",
  "PANEL",
  "h99901AP",
  "h99902AP",
  "h99903AP",
  "h99904AP",
  "h99901ZP",
  "h99902ZP",
  "h99903ZP",
  "h99904ZP",
  "h99901ELT",
  "h99902ELT",
  "h99903ELT",
  "h99904ELT",
  "Trenner0S",
  "TrennerS8",
  "Trenner911",
  "h91115_1_mu",
  "h91115_1_va",
  "h91115_2_mu",
  "h91115_2_va",
  "h91115_3_mu",
  "h91115_3_va",
  "h91115_4_mu",
  "h91115_4_va",
  "h91115_5_mu",
  "h91115_5_va",
  "h91115_6_mu",
  "h91115_6_va",
  "h91115_7_mu",
  "h91115_7_va",
  "h91115_8_mu",
  "h91115_8_va",
  "h91115_9_mu",
  "h91115_9_va",
  "h91115_10_mu",
  "h91115_10_va",
  "h91115_11_mu",
  "h91115_11_va",
  "k_extern_mu",
  "k_extern_va",
  "k_ZP_mit_Eltern",
  "APINT",
  "ZPINT",
  "ELTINT",
  "RCL1",
  "RCL2",
  "anzkon1",
  "anzkon2",
  "LMETH1",
  "LMETH2",
  "CATI_SW1",
  "CATI_SW2",
  "INTERNR1",
  "INTERNR2",
  "SMETH1",
  "SMETH2",
  "vollstaendig",
  "RC_KDAT1",
  "RC_KDAT2",
  "foreal_p1",
  "foreal_p2",
  "ZPSTIPRO",
  "panelbr",
  "h11022_g3",
  "h11022_g14",
  "h11022_g16",
  "APINTNR",
  "Trenner911ZP",
  "Trenner1217",
  "v33900",
  "v33901",
  "Trenner1832",
  "v33910",
  "v33911",
  "TrennerELT",
  "v44901",
  "k_othersprache1",
  "k_othersprache2",
  "k_othersprache3",
  "k_othersprache4",
  "k_othersprache5",
  "k_othersprache6",
  "hh11900",
  "hh11901",
  "hh11902",
  "hh11903",
  "hh11904",
  "gkbik10",
  "polgk",
  "point",
  "gebjahr_brutto",
  "sex_brutto",
  "TRANCHE",
  "telja",
  "casa_typ",
  "casa_hh",
  "casa_dist_opnv",
  "oa_result",
  "nationn",
  "INTNR_hh",
  "ANZPHH_hh",
  "RCL_hh",
  "LMETH_hh",
  "CATI_SW_hh",
  "SMETH_hh",
  "TRANCHE_hh",
  "vollstaendig_hh",
  "TRACK_hh",
  "t_forc_hh",
  "ch_tele_hh",
  "ch_email_hh",
  "ch_post_hh",
  "telja_hh",
  "FORC_DIFF_hh",
  "FO_Dat_hh",
  "FORC_hh",
  "INTERNR_hh",
  "foreal_hh",
  "vansch_hh",
  "verinn1_hh",
  "verinn2_hh",
  "vkonv_hh",
  "cati_vk_hh",
  "panelbr_hh",
  "anzkonHH1_hh",
  "anzkonHH2_hh",
  "anzkonHH3_hh",
  "GewHH",
  "calHH",
  "GemID",
  "z_gew",
  "z_gew2",
  "m_intyear_ap",
  "m_intmonth_ap",
  "m_intyear_hh",
  "m_intmonth_hh",
  "m_intyear_zp",
  "m_intmonth_zp",
  "m_intyear_eltern",
  "m_intmonth_eltern",
  "m_edited_infas",
  "minkid",
  "h71310_mu",
  "h71310_va",
  "h71312_mu",
  "h71312_va",
  "h71314_mu",
  "h71314_va",
  "h71317_mu",
  "h71317_va"
)

# lade personen datensatz hoch
daten_personen <- read.spss("/Users/anilcaneldiven/Downloads/dji_suf_personen.sav", to.data.frame = TRUE)

# Funktion zum Ersetzen von NA-Werten
replace_na_values <- function(df) {
  df %>%
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
    mutate(across(where(is.factor), ~ ifelse(is.na(.), "unbekannt", .))) %>%
    mutate(across(where(is.character), ~ ifelse(is.na(.), "unbekannt", .)))
}


###### bilde scores für deprivation

# Auswählen der relevanten Variablen
relevante_variablen <- c(
  "hh11073", "hh11075", "hh11070_1", "hh11070_2", "hh11070_3",  # Finanzielle Deprivation
  "h11014", "h11014_mu", "h11014_va", "h11015", "h11015_mu", "h11015_va",  # Bildungsstatus
  "h11018", "h11018_mu", "h11018_va", "h11021_mu", "h11021_va",  # Erwerbstätigkeit
  "hh11001", "hh11066", "hh11079",  # Wohnsituation
  "hh11058", "hh11059", "hh11060",  # Soziale Unterstützung
  "h22803_10", "h22803_11", "h22803_12", "h22803_14", "h22803_3", "h22803_6"  # Deprivationsindikatoren
)

# Extraktion der relevanten Variablen aus dem Datensatz
daten_personen_relevant <- daten_personen[, relevante_variablen]

# Funktion zur Umwandlung von Faktoren in numerische Werte
convert_factor_to_numeric <- function(factor_column) {
  suppressWarnings(as.numeric(as.character(factor_column)))
}

# Überprüfen und Umwandeln von Faktoren in numerische Werte
daten_personen_relevant <- daten_personen_relevant %>%
  mutate(across(where(is.factor), ~ convert_factor_to_numeric(.)))

# Manuelle Umkodierung spezifischer Variablen
daten_personen_relevant <- daten_personen_relevant %>%
  mutate(
    hh11073 = as.numeric(hh11073 == "ja"),
    hh11075 = case_when(
      hh11075 == "0" ~ 0,
      hh11075 == "unter 150 Euro" ~ 1,
      hh11075 == "150 bis unter 300 Euro" ~ 2,
      hh11075 == "300 bis unter 450 Euro" ~ 3,
      hh11075 == "450 bis unter 600 Euro" ~ 4,
      hh11075 == "600 bis unter 900 Euro" ~ 5,
      hh11075 == "900 bis unter 1200 Euro" ~ 6,
      hh11075 == "1200 bis unter 1500 Euro" ~ 7,
      hh11075 == "1500 bis unter 2000 Euro" ~ 8,
      hh11075 == "2000 bis unter 2600 Euro" ~ 9,
      hh11075 == "2600 bis unter 3200 Euro" ~ 10,
      hh11075 == "3200 bis unter 4000 Euro" ~ 11,
      hh11075 == "4000 bis unter 5000 Euro" ~ 12,
      hh11075 == "5000 bis unter 6000 Euro" ~ 13,
      hh11075 == "6000 bis unter 7500 Euro" ~ 14,
      hh11075 == "7500 bis unter 9000 Euro" ~ 15,
      hh11075 == "9000 bis unter 12000 Euro" ~ 16,
      hh11075 == "12000 bis unter 15000 Euro" ~ 17,
      hh11075 == "15000 Euro und mehr" ~ 18,
      TRUE ~ NA_real_
    ),
    hh11070_1 = as.numeric(hh11070_1 == "nein, aus finanziellen Gründen nicht"),
    hh11070_2 = as.numeric(hh11070_2 == "nein, aus finanziellen Gründen nicht"),
    hh11070_3 = as.numeric(hh11070_3 == "nein, aus finanziellen Gründen nicht"),
    h11014 = convert_factor_to_numeric(h11014),
    h11014_mu = convert_factor_to_numeric(h11014_mu),
    h11014_va = convert_factor_to_numeric(h11014_va),
    h11015 = convert_factor_to_numeric(h11015),
    h11015_mu = convert_factor_to_numeric(h11015_mu),
    h11015_va = convert_factor_to_numeric(h11015_va),
    h11018 = as.numeric(h11018 == "nein"),
    h11018_mu = as.numeric(h11018_mu == "nein"),
    h11018_va = as.numeric(h11018_va == "nein"),
    hh11001 = as.numeric(hh11001),
    hh11066 = as.numeric(hh11066),
    hh11079 = as.numeric(hh11079),
    hh11058 = convert_factor_to_numeric(hh11058),
    hh11059 = convert_factor_to_numeric(hh11059),
    hh11060 = convert_factor_to_numeric(hh11060),
    h22803_10 = as.numeric(h22803_10 == "Nein aus finanziellen Gründen"),
    h22803_11 = as.numeric(h22803_11 == "Nein aus finanziellen Gründen"),
    h22803_12 = as.numeric(h22803_12 == "Nein aus finanziellen Gründen"),
    h22803_14 = as.numeric(h22803_14 == "Nein aus finanziellen Gründen"),
    h22803_3 = as.numeric(h22803_3 == "Nein aus finanziellen Gründen"),
    h22803_6 = as.numeric(h22803_6 == "Nein aus finanziellen Gründen")
  )

# Berechnung des Deprivationsindex
daten_personen_relevant <- daten_personen_relevant %>%
  rowwise() %>%
  mutate(y = sum(c_across(everything()), na.rm = TRUE))

# Hinzufügen des Deprivationsindex zum Originaldatensatz
daten_personen$y <- daten_personen_relevant$y

# Extraktion der relevanten Variablen aus dem Datensatz
daten_personen <- daten_personen[, !colnames(daten_personen) %in% relevante_variablen2]
daten_personen1 <- replace_na_values(daten_personen)

# lade lookup tabele für colnames hoch
variablen <- read_xlsx("/Users/anilcaneldiven/Desktop/variablennamenpersonen.xlsx")

# daten1 <- daten_personen %>%
#   select(-all_of(relevante_variablen2))
daten1 <- daten_personen1
nzv <- nearZeroVar(daten1, saveMetrics = TRUE)
daten2 <- daten1[, !nzv$nzv]


### einteilung nuemric und categorical

# Umwandlung der Charakter-Spalten in Faktoren
daten2[] <- lapply(daten2, function(x) if(is.character(x)) as.factor(x) else x)

# Einteilung in numerische und kategoriale Variablen
numeric_vars <- sapply(daten2, is.numeric)
categorical_vars <- sapply(daten2, is.factor)

data_numeric <- daten2[, numeric_vars]
data_categorical <- daten2[, categorical_vars]
data_categorical$y <- daten_personen_relevant$y

length(colnames(data_categorical))
length(colnames(data_numeric))

data_numeric <- replace_na_values(data_numeric)
data_categorical <- replace_na_values(data_categorical)

## categorial

## Informationsgehalt
# Anzahl der Bins (du kannst die Anzahl anpassen)
num_bins <- 5

# Diskretisiere die Zielvariable 'y' basierend auf der Verteilung
quantiles <- quantile(data_categorical$y, probs = seq(0, 1, length.out = num_bins + 1), na.rm = TRUE)
data_categorical$y <- cut(data_categorical$y, breaks = quantiles, include.lowest = TRUE, labels = FALSE)


# Berechne den Informationsgehalt für jede Variable
info_gain <- information_gain(y ~ ., data_categorical)

#Berechne den Durchschnitt des Informationsgehalts
average_info_gain <- mean(info_gain$importance)

# Identifiziere die Variablen, deren Informationsgehalt unter dem Durchschnitt liegt
below_average_vars <- info_gain$attributes[info_gain$importance < average_info_gain]

# Entferne die Variablen mit Informationsgehalt unter dem Durchschnitt
data_categorical_reduced <- data_categorical[, !names(data_categorical) %in% below_average_vars]
length(colnames(data_categorical_reduced))


## nuemrisch
#### correlation numeric

data_numeric <- data_numeric %>% 
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

cor_matrix <- cor(data_numeric, use = "complete.obs")

high_cor <- findCorrelation(cor_matrix, cutoff = 0.9)

data_numeric <- data_numeric[, -high_cor]
data_numeric <- data_numeric[, !colnames(data_numeric) %in% "y"]

length(colnames(data_numeric))
### lasso
# Matrix der Prädiktoren und Zielvariable

x <- as.matrix(data_numeric)
y <- daten_personen_relevant$y

# Lasso Regression
lasso_model <- cv.glmnet(x, y, alpha = 1)
selected_vars <- coef(lasso_model, s = "lambda.min")
selected_vars <- row.names(selected_vars)[selected_vars[, 1] != 0]
selected_vars <- selected_vars[-1]
data_reduced_lasso <- data_numeric[, selected_vars]

length(colnames(data_reduced_lasso))

### VIF

# Berechnung von VIF
vif_values <- vif(lm(data_reduced_lasso))

# Schwellenwert für VIF (z.B. 5)
high_vif <- names(vif_values[vif_values > 5])

# Entfernen der Variablen mit hohem VIF
data_numeric_vif_reduced <- data_reduced_lasso[, !colnames(data_reduced_lasso) %in% high_vif]


length(colnames(data_numeric_vif_reduced))
length(colnames(data_categorical_reduced))





