from xgboost import XGBClassifier
from sklearn.model_selection import RandomizedSearchCV, StratifiedKFold, train_test_split
from sklearn.metrics import f1_score, confusion_matrix, ConfusionMatrixDisplay, classification_report, cohen_kappa_score
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

# Annahme: X enthält numerische und kategoriale Features, y ist der Zielwert

# Step 1: Split data into train (90%) and test sets (10%)
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.1, random_state=123, stratify=y)

# Step 2: Define XGBoost hyperparameters for RandomizedSearchCV
param_dist = {
    'n_estimators': [100, 200, 300, 400, 500, 600, 700],  # Anzahl der Bäume
    'max_depth': [3, 5, 10, 20, 30],                      # Maximale Tiefe der Bäume
    'learning_rate': [0.01, 0.05, 0.1, 0.2],              # Lernrate
    'subsample': [0.6, 0.8, 1.0],                         # Anteil der Daten für jeden Baum
    'colsample_bytree': [0.6, 0.8, 1.0],                  # Anteil der Features pro Baum
    'gamma': [0, 0.1, 0.2, 0.3],                          # Mindestverlustreduktion
    'reg_alpha': [0, 0.01, 0.1, 1],                       # L1-Regularisierung
    'reg_lambda': [1, 1.5, 2]                             # L2-Regularisierung
}

# Step 3: Initialize XGBClassifier with scale_pos_weight to handle imbalanced data
xgb_model = XGBClassifier(random_state=123, scale_pos_weight=1, use_label_encoder=False, eval_metric='logloss')

# Step 4: RandomizedSearchCV for Hyperparameter Tuning
random_search = RandomizedSearchCV(
    estimator=xgb_model,
    param_distributions=param_dist,
    n_iter=50,  # Number of random configurations
    cv=StratifiedKFold(n_splits=5),  # 5-fold cross-validation
    scoring='f1',  # Optimize for F1 score
    verbose=2,
    random_state=123,
    n_jobs=-1
)

# Step 5: Fit the RandomizedSearchCV
random_search.fit(X_train, y_train)

# Step 6: Get the best model and evaluate performance on the test set
best_xgb_model = random_search.best_estimator_
y_test_pred = best_xgb_model.predict(X_test)

# Step 7: Calculate F1-score on the test set
test_f1 = f1_score(y_test, y_test_pred, pos_label=1)

# Step 8: Confusion Matrix (using green color map)
conf_matrix = confusion_matrix(y_test, y_test_pred)
disp = ConfusionMatrixDisplay(confusion_matrix=conf_matrix, display_labels=best_xgb_model.classes_)
disp.plot(cmap=plt.cm.Greens)  # Use green colormap for LMU representation
plt.title(f"Confusion Matrix - Best XGBoost Model (F1-Score: {test_f1:.4f})")
plt.show()

# Step 9: Other Metrics
classification_metrics = classification_report(y_test, y_test_pred)
kappa = cohen_kappa_score(y_test, y_test_pred)

print("Beste Hyperparameter: ", random_search.best_params_)
print("\nKlassifikationsbericht:\n", classification_metrics)
print(f"\nKappa-Wert: {kappa:.4f}")
