# Load necessary packages
if (!require("glmnet")) install.packages("glmnet")
library(glmnet)

# Convert factor variables to dummy variables
data_final_dummy <- model.matrix(dep_child ~ ., data = data_final_clean_4)[, -1]
data_final_dummy <- as.data.frame(data_final_dummy)
data_final_dummy$dep_child <- data_final_clean_4$dep_child

# Split the data into training and testing sets
trainIndex <- createDataPartition(data_final_dummy$dep_child, p = .8, list = FALSE, times = 1)
data_train_dummy <- data_final_dummy[trainIndex, ]
data_test_dummy <- data_final_dummy[-trainIndex, ]


# Train the LASSO model
x_train <- as.matrix(data_train_dummy[, -ncol(data_train_dummy)])
y_train <- as.factor(data_train_dummy$dep_child)
lasso_model <- cv.glmnet(x_train, y_train, family = "multinomial", type.multinomial = "grouped", alpha = 1)

# Make predictions on the test set
x_test <- as.matrix(data_test_dummy[, -ncol(data_test_dummy)])
y_test <- as.factor(data_test_dummy$dep_child)


# Make predictions on the test set
predicted_classes_lasso <- predict(lasso_model, s = "lambda.min", newx = x_test, type = "class")
predicted_classes_lasso <- factor(predicted_classes_lasso, levels = levels(y_test)) # Ensure levels match

# Evaluate the LASSO model
conf_matrix_lasso <- confusionMatrix(predicted_classes_lasso, y_test)
print(conf_matrix_lasso)

# Evaluate the LASSO model
conf_matrix_lasso <- confusionMatrix(predicted_classes_lasso, y_test)
print(conf_matrix_lasso)


# Plot Variable Importance for Random Forest
varImpPlot(rf_model)



# Variable Importance for Random Forest Model
varImp <- importance(rf_model)
varImp_df <- data.frame(Variable = rownames(varImp), Importance = varImp[, 1])
varImp_df <- varImp_df[order(varImp_df$Importance, decreasing = TRUE), ]
print(head(varImp_df, 10)) # Top 10 important variables
varImpPlot(rf_model)

# Variable Importance for LASSO Model
lasso_coef <- coef(lasso_model, s = "lambda.min")
lasso_coef_df <- do.call(rbind, lapply(lasso_coef, function(x) {
  data.frame(Variable = rownames(x), Coefficient = as.numeric(x))
}))
lasso_coef_df <- lasso_coef_df[order(abs(lasso_coef_df$Coefficient), decreasing = TRUE), ]
lasso_coef_df <- lasso_coef_df[lasso_coef_df$Coefficient != 0, ]
print(head(lasso_coef_df, 10)) # Top 10 important variables

# Compare top variables
top_rf_vars <- head(varImp_df$Variable, 10)
top_lasso_vars <- head(lasso_coef_df$Variable, 20)
common_vars <- intersect(top_rf_vars, top_lasso_vars)
unique_rf_vars <- setdiff(top_rf_vars, top_lasso_vars)
unique_lasso_vars <- setdiff(top_lasso_vars, top_rf_vars)

list(
  common_vars = common_vars,
  unique_rf_vars = unique_rf_vars,
  unique_lasso_vars = unique_lasso_vars
)
