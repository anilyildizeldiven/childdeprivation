from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import RandomizedSearchCV, StratifiedKFold, train_test_split
from sklearn.metrics import f1_score, confusion_matrix, ConfusionMatrixDisplay, classification_report, cohen_kappa_score
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

# Step 1: Split data into train (90%) and test sets (10%)
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.1, random_state=123, stratify=y)

# Step 2: Define Logistic Regression hyperparameters for RandomizedSearchCV with valid combinations
param_dist = [
    {'penalty': ['l2'], 'C': np.logspace(-4, 4, 20), 'solver': ['lbfgs'], 'class_weight': ['balanced', None], 'max_iter': [100, 200, 300]},
    {'penalty': ['l1'], 'C': np.logspace(-4, 4, 20), 'solver': ['liblinear'], 'class_weight': ['balanced', None], 'max_iter': [100, 200, 300]},
    {'penalty': ['elasticnet'], 'C': np.logspace(-4, 4, 20), 'solver': ['saga'], 'l1_ratio': [0.5, 0.7, 0.9], 'class_weight': ['balanced', None], 'max_iter': [100, 200, 300]},
]

# Step 3: Initialize LogisticRegression
log_reg_model = LogisticRegression(random_state=123)

# Step 4: RandomizedSearchCV for Hyperparameter Tuning
random_search = RandomizedSearchCV(
    estimator=log_reg_model,
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
best_log_reg_model = random_search.best_estimator_
y_test_pred = best_log_reg_model.predict(X_test)

# Step 7: Calculate F1-score on the test set
test_f1 = f1_score(y_test, y_test_pred, pos_label=1)

# Step 8: Confusion Matrix
conf_matrix = confusion_matrix(y_test, y_test_pred)
disp = ConfusionMatrixDisplay(confusion_matrix=conf_matrix, display_labels=best_log_reg_model.classes_)
disp.plot(cmap=plt.cm.Greens)
plt.title(f"Confusion Matrix - Best Model (F1-Score: {test_f1:.4f})")
plt.show()

# Step 9: Other Metrics
classification_metrics = classification_report(y_test, y_test_pred)
kappa = cohen_kappa_score(y_test, y_test_pred)

print("Beste Hyperparameter: ", random_search.best_params_)
print("\nKlassifikationsbericht:\n", classification_metrics)
print(f"\nKappa-Wert: {kappa:.4f}")
