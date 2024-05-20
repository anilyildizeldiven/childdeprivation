# Installiere und lade die notwendigen Pakete
library(readxl)
library(dplyr)
library(foreign)
library(caret)

daten_personen <- read.spss("/Users/anilcaneldiven/Downloads/dji_suf_personen.sav", to.data.frame = TRUE)


# Entferne Zeilen mit fehlenden Werten
# Funktion zum Ersetzen von NA-Werten
replace_na_values <- function(df) {
  df %>%
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
    mutate(across(where(is.factor), ~ ifelse(is.na(.), "unbekannt", .))) %>%
    mutate(across(where(is.character), ~ ifelse(is.na(.), "unbekannt", .)))
}

# NA-Werte in data_reduced ersetzen
data_reduced <- replace_na_values(data_reduced)



daten_personen$y <- rowSums(daten_personen == "Nein aus finanziellen Gründen", na.rm = TRUE)
scores <- rowSums(daten_personen == "Nein aus finanziellen Gründen", na.rm = TRUE)

variablen <- read_xlsx("/Users/anilcaneldiven/Desktop/variablennamenpersonen.xlsx")


relevante_variablen <- c(
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


daten1 <- daten_personen %>%
  select(-all_of(relevante_variablen))
length(names(daten1))

nzv <- nearZeroVar(daten1, saveMetrics = TRUE)
daten2 <- daten1[, !nzv$nzv]

length(names(daten2))
# 450 schon mal weg

numeric_vars <- sapply(daten2, is.numeric)
categorical_vars <- sapply(daten2, is.factor)

data_numeric <- daten2[, numeric_vars]
data_categorical <- daten2[, categorical_vars]

length(colnames(data_categorical))

####

data_numeric <- data_numeric %>% 
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

cor_matrix <- cor(data_numeric, use = "complete.obs")

high_cor <- findCorrelation(cor_matrix, cutoff = 0.9)

data_numeric <- data_numeric[, -high_cor]


# Laden des Pakets
library(caret)
data_categorical$y <- scores


# Dummy-Codierung der kategorialen Variablen
dummy_model <- dummyVars(y ~ ., data = data_categorical)
data_categorical_transformed <- predict(dummy_model, newdata = data_categorical)

# Chi-Quadrat-Test auf die Dummy-codierten Variablen anwenden
chi2_results <- nearZeroVar(data_categorical_transformed, saveMetrics = TRUE)
important_dummy_vars <- rownames(chi2_results[chi2_results$nzv == FALSE, ])

# Wichtige Dummy-Variablen identifizieren und die ursprünglichen Variablennamen extrahieren
# Hier nehmen wir an, dass der ursprüngliche Variablenname durch einen Punkt getrennt ist
important_categorical_vars <- unique(sub("\\..*", "", important_dummy_vars))

# Reduzierung der ursprünglichen Daten auf die wichtigen Variablen
data_categorical_reduced <- data_categorical[, c(important_categorical_vars, "y")]

data_new <- cbind(data_categorical_reduced,data_numeric)

# Anzahl der wichtigen Variablen
print(length(colnames(data_new)))





