# Define a custom prediction function with exactly two arguments
custom_pred_simple <- function(rf_model, newdata) {
  # Get class predictions from the model using `predict` method
  preds <- predict(rf_model, data = newdata)$predictions
  
  # Return the proportion of predictions for the positive class (e.g., class "1")
  return(rowMeans(preds == "1"))  # Use "1" as a string to match factor levels
}

# Generate the partial dependence plot with custom prediction function
pdp_plot_simple <- partial(
  object = rf_model, 
  pred.var = "aquivalenzeinkommen_intervallmitte", 
  train = data_train, 
  pred.fun = custom_pred_simple,  # Use the custom prediction function
  grid.resolution = 10  # Use lower resolution to test faster
)

# Plot the result
plot(pdp_plot_simple, main = "Partial Dependence Plot - Simple Check")

#######################

# pdf to a variable
pdp_plot <- partial(rf_model, 
                    pred.var = "Years_of_Education_Mother", 
                    train = data_train)

# plotting
autoplot(pdp_plot) +
  ggtitle("Partial Dependence Plot for Years_of_Education_Mother") +
  xlab("Years_of_Education_Mother") +
  ylab("Partial Dependence of dep_child")
