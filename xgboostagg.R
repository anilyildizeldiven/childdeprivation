library(caret)
library(xgboost)

# tuned on best params
# $objective
# [1] "binary:logistic"
# 
# $eval_metric
# [1] "logloss"
# 
# $eta
# [1] 0.1
# 
# $max_depth
# [1] 12

best_params

trainIndex <- createDataPartition(data20$dep_child, p = 0.8, list = FALSE, times = 1)
data_train <- data20[trainIndex, ]
data_test <- data20[-trainIndex, ]

names(data_train) <- make.names(names(data_train))
names(data_test) <- make.names(names(data_test))
# Konvertiere die abhängige Variable in numerische Werte
data_train$dep_child <- as.numeric(as.character(data_train$dep_child))
data_test$dep_child <- as.numeric(as.character(data_test$dep_child))

# One-Hot-Encoding für die Prädiktorvariablen durchführen
data_train_matrix <- model.matrix(dep_child ~ . - 1, data = data_train)
data_test_matrix <- model.matrix(dep_child ~ . - 1, data = data_test)

# Die Zielvariable extrahieren
train_label <- data_train$dep_child
test_label <- data_test$dep_child


# Vorbereitung der DMatrix-Objekte für XGBoost
dtrain <- xgb.DMatrix(data = data_train_matrix, label = train_label)
dtest <- xgb.DMatrix(data = data_test_matrix, label = test_label)


# Setze die Parameter für das XGBoost-Modell
params <- list(
  objective = "binary:logistic",  # für binäre Klassifikation
  max_depth = 15,
  eta = 0.1,
  min_child_weight = 15,
  eval_metric = "logloss"
)


# Trainiere das XGBoost-Modell
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 500,
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 30,
  maximize = FALSE
)




# Vorhersagen auf dem Testdatensatz machen
pred_probs <- predict(xgb_model, newdata = dtest)

# Konvertiere die Wahrscheinlichkeiten in Klassen (0 oder 1)
pred_classes <- ifelse(pred_probs > 0.3, 1, 0)

# Konvertiere die Vorhersagen und die echten Werte in Faktoren
pred_classes <- factor(pred_classes, levels = c(0, 1))
test_label_factor <- factor(test_label, levels = c(0, 1))

# Berechne die Konfusionsmatrix
conf_matrix <- confusionMatrix(pred_classes, test_label_factor, positive = "1")

# Zeige die Konfusionsmatrix und die zugehörigen Metriken an
print(conf_matrix)

# Berechne die Variablenwichtigkeit
importance_matrix <- xgb.importance(feature_names = colnames(data_train_matrix), model = xgb_model)

# Zeige die wichtigsten Variablen an
print(importance_matrix)

# Visualisiere die Variablenwichtigkeit mit xgb.plot.importance
xgb.plot.importance(importance_matrix, top_n = 10)  # Zeigt die Top 10 wichtigsten Features

# Get feature importance data frame
importance_matrix <- xgb.importance(model = xgb_model)

# Aggregate importance for categorical variables by prefix matching (dummy variables)
importance_aggregated <- importance_matrix %>%
  mutate(Feature = sub("_[^_]+$", "", Feature)) %>%  # Remove suffix after the underscore
  group_by(Feature) %>%
  summarise(Gain = sum(Gain)) %>%  # Sum the importance gains for each variable
  arrange(desc(Gain))

# Print aggregated feature importance
print(importance_aggregated)

# Plot aggregated importance
ggplot(importance_aggregated, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Variable") +
  ylab("Importance (Gain)") +
  ggtitle("Aggregated Feature Importance")


