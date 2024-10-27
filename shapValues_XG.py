import shap
import numpy as np
import matplotlib.pyplot as plt


# Step 1: SHAP Explainer für das XGBoost-Modell erstellen (TreeExplainer)
explainer = shap.TreeExplainer(best_xgb_model)

# Step 2: SHAP-Werte für den Test-Datensatz berechnen (Logits)
shap_values = explainer.shap_values(X_test)

# Wir nehmen die SHAP-Werte für Klasse 1 (positive class, falls binäre Klassifikation)
if isinstance(shap_values, list) and len(shap_values) == 2:
    shap_values = shap_values[1]  # Nehme die SHAP-Werte für Klasse 1 (positive class)

# Berechne den Basis-Logit (erwarteter Wert, auch "expected value" genannt)
base_logit = explainer.expected_value[1] if isinstance(explainer.expected_value, list) else explainer.expected_value

# Step 3: Umwandlung der Logits in Wahrscheinlichkeiten (mit der Sigmoid-Funktion)
def sigmoid(logit):
    return 1 / (1 + np.exp(-logit))

# Für jede Instanz und jedes Feature: Berechne die SHAP-Werte in Wahrscheinlichkeiten
shap_values_proba = np.zeros_like(shap_values)  # Erstelle eine leere Matrix für die Wahrscheinlichkeits-SHAP-Werte

for i in range(X_test.shape[0]):
    # Summe der SHAP-Werte für die Instanz ohne spezifische Features (nur der Basiswert)
    base_proba = sigmoid(base_logit)

    # Berechne den neuen Logit-Wert für jedes Feature und wandle es in Wahrscheinlichkeiten um
    for j in range(X_test.shape[1]):  # Für jedes Feature
        logit_with_shap = base_logit + shap_values[i, j]  # SHAP-Wert für dieses Feature addieren
        proba_with_shap = sigmoid(logit_with_shap)

        # Die Änderung der Wahrscheinlichkeit durch dieses Feature speichern
        shap_values_proba[i, j] = proba_with_shap - base_proba

# Step 4: SHAP-Summary-Plot für Wahrscheinlichkeiten (Violin-Plot-Stil)
shap.summary_plot(shap_values_proba, X_test, plot_type="violin", show=False)

# Optional: Speichere den Violin-Plot
plt.savefig('shap_summary_violin_class1_xgb_proba.png')
plt.show()

# Optional: Erstelle auch einen Barplot (SHAP-Summary-Plot) für die Feature-Wichtigkeit
shap.summary_plot(shap_values_proba, X_test, plot_type="bar",color=lmu_green, show=False)
# X-Achsen-Beschriftung ändern
plt.xlabel("mean(|SHAP value|)")
plt.savefig('shap_summary_bar_class1_xgb_proba.png')
plt.show()
