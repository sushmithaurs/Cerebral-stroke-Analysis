# 📊 Stroke Risk Analysis – Probability & Statistics Project

## 📌 Project Overview
This project analyzes stroke risk factors using a real-world healthcare dataset containing 43,400 observations and 12 variables. The goal is to identify statistically significant predictors of stroke through exploratory data analysis, hypothesis testing, and regression modeling.

The analysis integrates demographic, lifestyle, and medical variables to understand the multifactorial nature of stroke risk.

---

## 📂 Dataset Information
- Source: Kaggle Stroke Prediction Dataset
- Observations: 43,400
- Variables: 12
- Target Variable: `stroke` (Yes/No)

### Key Features:
- Age
- BMI
- Average Glucose Level
- Hypertension
- Heart Disease
- Smoking Status
- Gender
- Marital Status
- Residence Type
- Work Type

---

## 🛠 Data Preprocessing
- Median imputation for numerical missing values
- Mode imputation for categorical missing values
- Winsorization at 1st and 99th percentiles to reduce outlier impact
- Conversion of binary variables to categorical factors
- Removal of duplicate records

---

## 📊 Exploratory Data Analysis (EDA)
- Summary statistics
- Histograms and boxplots for numerical variables
- Bar charts for categorical variables
- Correlation matrix
- Scatterplots for relationship analysis

---

## 🧪 Hypothesis Testing
1. **One-Sample t-test**  
   - BMI vs benchmark mean (26)

2. **Two-Sample t-test**  
   - Glucose levels by hypertension status

3. **Two-Sample Proportion Test**  
   - Stroke occurrence by smoking status

Results showed statistically significant differences across tested groups.

---

## 📈 Regression Analysis
- Linear Regression: BMI ~ Age
- Linear Regression: Glucose ~ Age
- Linear Regression: Glucose ~ BMI

Model outputs include coefficients, p-values, and R² values for interpretation.

---

## 🔧 Tools & Technologies
- R Programming
- Statistical Inference
- Data Visualization
- Regression Modeling

---

## 🎯 Key Insights
- Average BMI is significantly higher than the benchmark value.
- Hypertensive individuals show significantly higher glucose levels.
- Stroke occurrence is significantly higher among smokers.
- Age and BMI are significant predictors of glucose levels.

---

## 📚 References
- Kaggle Stroke Dataset  
- Field, A. (2018). *Discovering Statistics Using R*  
- World Health Organization (2023) – Stroke Fact Sheet

---

## 🚀 Future Improvements
- Logistic regression modeling for stroke prediction
- Multivariable regression analysis
- Feature importance analysis
- Predictive modeling with machine learning algorithms

---

### 👩‍💻 Author
**Sushmitha Urs**  
Probability & Statistics Final Project

