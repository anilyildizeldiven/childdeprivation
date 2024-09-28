library(pdp)
library(ggplot2)


# write specific features to analyze
top_numeric_features <- c("Age_in_Years", "Personal_Net_Income_Mother", 
                          "Number_of_People_in_Household", "Years_of_Education_Father", 
                          "Years_of_Education_Mother")

# just to be sure that there are numeric
data_train <- data_train %>%
  mutate(across(all_of(top_numeric_features), as.numeric))

# plot theses pdps for nuemricss
for (feature in top_numeric_features) {
  
  # print the namess
  print(paste("Erstelle PDP für Feature:", feature))
  print(paste("Datentyp:", class(data_train[[feature]])))
  
  # calculate date WITH PROB = TRUE
  pd <- partial(rf_model, pred.var = feature, train = data_train, prob = TRUE)
  
  # PLOT
  pd_plot <- autoplot(pd) +
    ggtitle(paste("Partial Dependence Plot for:", feature)) +
    xlab(feature) +
    ylab("Predicted Probability of Deprivation") +
    theme_minimal()
  
  # show the plots
  print(pd_plot)
}



# PART 2 now categorical ones
top_categorical_features <- c("Gender", "Household_Type", "BIK_GK_10", "Current_Employment_Mother")

# compare numerics
for (feature in top_categorical_features) {
  
  # just to be sure
  data_train[[feature]] <- as.factor(data_train[[feature]])
  
  # debug printing
  print(paste("Erstelle PDP für kategoriales Feature:", feature))
  print(paste("Datentyp:", class(data_train[[feature]])))
  
  # calculate data
  pd <- partial(rf_model, pred.var = feature, train = data_train, prob = TRUE)
  
  # plot
  pd_plot <- autoplot(pd) +
    ggtitle(paste("Partial Dependence Plot for:", feature)) +
    xlab(feature) +
    ylab("Predicted Probability of Deprivation") +
    theme_minimal()
  
  # show plot
  print(pd_plot)
}



##### altern. ICE PLOTS

# ICE-Plot for Years_of_Education_Mother
ice_plot <- partial(rf_model, pred.var = "Years_of_Education_Mother", 
                    train = data_train, ice = TRUE)

# Plot 
autoplot(ice_plot, center = TRUE) +
  ggtitle("ICE Plot for Years of Education (Mother)") +
  xlab("Years of Education (Mother)") +
  ylab("Predicted Probability of Deprivation") +
  theme_minimal()
