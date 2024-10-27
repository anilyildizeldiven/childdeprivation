import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split, cross_val_score
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import f1_score, make_scorer
from sklearn.model_selection import StratifiedKFold
import pandas as pd

# CSV-Datei in Python importieren
data_cleaned = pd.read_csv('data_cleaned.csv')

X = data_cleaned.drop('dep_child', axis=1)
y = data_cleaned['dep_child']



from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import RandomizedSearchCV, StratifiedKFold, train_test_split, learning_curve
from sklearn.metrics import f1_score, confusion_matrix, ConfusionMatrixDisplay, classification_report, cohen_kappa_score
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd


# Step 1: Split data into train (90%) and test sets (10%)
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.1, random_state=123, stratify=y)

# Step 2: Define Random Forest hyperparameters for RandomizedSearchCV
param_dist = {
    'n_estimators': [100, 200, 300, 400, 500, 600, 700],  # Breiterer Range für n_estimators
    'max_depth': [None, 10, 20, 30, 40, 50],              # Höherer Max-Depth Bereich
    'min_samples_split': [2, 5, 10, 15],                  # Mehr Optionen für min_samples_split
    'min_samples_leaf': [1, 2, 4, 6],                     # Mehr Optionen für min_samples_leaf
    'max_features': ['sqrt', 'log2'],                     # Optionen für max_features
    'bootstrap': [True, False]                            # Bootstrap und Non-Bootstrap Optionen
}

# Step 3: Initialize RandomForestClassifier with class_weight='balanced'
rf_model = RandomForestClassifier(random_state=123, class_weight='balanced')

# Step 4: RandomizedSearchCV for Hyperparameter Tuning
random_search = RandomizedSearchCV(
    estimator=rf_model,
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
best_rf_model = random_search.best_estimator_
y_test_pred = best_rf_model.predict(X_test)

# Step 7: Calculate F1-score on the test set
test_f1 = f1_score(y_test, y_test_pred, pos_label=1)

# Step 8: Confusion Matrix 
conf_matrix = confusion_matrix(y_test, y_test_pred)
disp = ConfusionMatrixDisplay(confusion_matrix=conf_matrix, display_labels=best_rf_model.classes_)
disp.plot(cmap=plt.cm.Greens)
plt.title(f"Confusion Matrix - Best Model (F1-Score: {test_f1:.4f})")
plt.show()

# Step 9: Other Metrics
classification_metrics = classification_report(y_test, y_test_pred)
kappa = cohen_kappa_score(y_test, y_test_pred)

print("Beste Hyperparameter: ", random_search.best_params_)
print("\nKlassifikationsbericht:\n", classification_metrics)
print(f"\nKappa-Wert: {kappa:.4f}")

