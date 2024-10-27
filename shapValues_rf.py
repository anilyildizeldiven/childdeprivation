lmu_green = "#006233"
import shap
import matplotlib.pyplot as plt

# Annahme: 'best_rf_model' ist das trainierte Modell und 'X_train' ist das Trainingsset mit numerischen und kategorialen Features

# Step 1: SHAP Explainer erstellen (TreeExplainer für RandomForest)
explainer = shap.TreeExplainer(best_rf_model)

# Step 2: SHAP-Werte für den Test-Datensatz berechnen
shap_values = explainer.shap_values(X_test)

# SHAP summary bar plot for class 1 (positive class)
shap.summary_plot(shap_values[:, :, 1], X_test, plot_type="bar", color=lmu_green, show=False)


# X-Achsen-Beschriftung ändern
plt.xlabel("mean(|SHAP value|)")

# Speichern und anzeigen des Barplots
plt.savefig('shap_summary_bar_class1_xgb.png')
plt.show()

# Optional: SHAP Violin plot for class 1 (positive class)
shap.summary_plot(shap_values[:, :, 1], X_test, show=False)
plt.savefig('shap_summary_violin_class1_xgb.png')
plt.show()
