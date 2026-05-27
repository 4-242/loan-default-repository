# loan-default-repository
# Bank Loan Default Prediction
**Predicting loan defaulters using classification algorithms with age-segmented feature analysis**

---

## Problem Statement
Loan defaults cost financial institutions billions annually. This project builds 
and compares classification models on a large-scale dataset of  over 150,000 
loan applicants. Further, the project analyses how default risk drivers 
shift across different age groups.

---

## Dataset
- **Source:** Kaggle — Loan Default Dataset
- **Size:** ~150,000 observations
- **Target Variable:** Loan Status (Defaulter / Non-Defaulter)
- **Features:** Loan amount, rate of interest, interest rate spread, 
  upfront charges, property value, income, credit score, age, 
  debt-to-income ratio, credit type, gender, region

---

## Methodology

### 1. Data Cleaning & Imputation
- Selected 14 meaningful features, discarding low-variance nuisance variables
- Handled missing values using median/mode imputation (imputeMissings package)
- Visualised missing data patterns using gg_miss_upset()

### 2. Exploratory Data Analysis
- Proportion of defaulters by age group, region, and gender
- Income distribution by default status
- Correlation heatmap of numeric variables
- Finding: dataset is imbalanced with a high proportion of non-defaulters

### 3. Classification Models
Both models trained with 5-fold cross-validation using the caret package:
- **Logistic Regression** (glm, ROC metric)
- **Decision Tree** (rpart)

Train/test split: 70/30 stratified by target variable

### 4. Age-Segmented Feature Importance
Trained separate decision tree models for each age group:
<25, 25-34, 35-44, 45-54, 55-64, 65-74, 75+

Used the vip package to identify the most influential predictors 
per age segment.

---

## Results

| Model | Accuracy | Kappa |
|---|---|---|
| Logistic Regression | ~87% | 0.55 |
| Decision Tree | **99.9%** | **0.9997** |

- Decision Tree significantly outperforms Logistic Regression 
  on both accuracy and Kappa statistic
- ROC curve confirms Decision Tree achieves greater AUC
- Note: High Decision Tree accuracy partly reflects class imbalance 
  in the dataset

### Key Finding — Feature Importance Across Age Groups
Across all age groups, the top default predictors are consistent:
1. **Upfront Charges** — most influential across nearly all segments
2. **Interest Rate Spread**
3. **Rate of Interest**
4. **Credit Type (EQUI)**

Property value and debt-to-income ratio (dtir1) become more relevant 
for middle-aged borrowers (35–54).

---

## Tools & Packages
- **Language:** R
- **Packages:** dplyr, ggplot2, caret, rsample, rpart, rpart.plot, 
  pROC, vip, imputeMissings, VIM, naniar

---

## Files
- `loan_default_prediction.R` — Full analysis script
- `README.md` — Project documentation

---

## Author
Ayomide Abodunrin
