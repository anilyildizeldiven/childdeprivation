from sklearn.ensemble import RandomForestClassifier, GradientBoostingClassifier
from sklearn.svm import SVC
from sklearn.neighbors import KNeighborsClassifier
from sklearn.naive_bayes import GaussianNB
from xgboost import XGBClassifier
from sklearn.metrics import roc_curve, auc, precision_recall_curve, average_precision_score
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt

# Step 1: Split data into train (90%) and test sets (10%)
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.1, random_state=123, stratify=y)

# Step 2: Define Models for comparison
models = {
    'Random Forest': RandomForestClassifier(random_state=123, class_weight='balanced'),
    'SVM': SVC(probability=True, random_state=123),  # For ROC and PR curves, we need probability estimates
    'KNN': KNeighborsClassifier(),
    'Naive Bayes': GaussianNB(),
    'Gradient Boosting': GradientBoostingClassifier(random_state=123),
    'XGBoost': XGBClassifier(random_state=123, use_label_encoder=False, eval_metric='logloss')
}

# Initialize lists to store ROC curve and AUC values
roc_auc_values = {}
precision_recall_values = {}

# Step 3: Train each model and evaluate performance on the test set
for model_name, model in models.items():
    # Train the model
    model.fit(X_train, y_train)

    # Predict probabilities for ROC and Precision-Recall curves
    if hasattr(model, "predict_proba"):
        y_score = model.predict_proba(X_test)[:, 1]  # For binary classification, we need the probability of the positive class
    else:
        y_score = model.decision_function(X_test)  # For models like SVM without predict_proba

    # ROC Curve and AUC
    fpr, tpr, _ = roc_curve(y_test, y_score)
    roc_auc = auc(fpr, tpr)
    roc_auc_values[model_name] = roc_auc


    # Precision-Recall Curve
    precision, recall, _ = precision_recall_curve(y_test, y_score)
    avg_precision = average_precision_score(y_test, y_score)
    precision_recall_values[model_name] = avg_precision

    # Plot Precision-Recall Curve
    plt.figure(2)
    plt.plot(recall, precision, label=f'{model_name} (AP = {avg_precision:.2f})')



# Plot Precision-Recall Curve with all models
plt.figure(2)
plt.xlabel('Recall')
plt.ylabel('Precision')
plt.title('Precision-Recall Curve Comparison Between Models')
plt.legend(loc="lower left")
plt.show()


