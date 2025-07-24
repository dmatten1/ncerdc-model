# 📊 North Carolina Education Data Analysis

This repository contains code, analyses, and models related to our research using North Carolina public school student data, provided by the **North Carolina Education Research Data Center (NCERDC)**.

## 📁 Dataset

Our project is built upon a longitudinal dataset sourced from the [NCERDC](https://childandfamilypolicy.duke.edu/north-carolina-education-research-data/), which maintains a secure archive of student-level data from the North Carolina Department of Public Instruction. The dataset includes detailed student demographics, course enrollments, test scores, school and district identifiers, and program participation flags (e.g., gifted, special education, free/reduced lunch).

> **Note**: Access to the NCERDC dataset requires a data use agreement. This repository does **not** contain any raw or identifiable student data.

## 🧠 Project Overview

Our analysis explores how various educational experiences and labels influence student outcomes over time, particularly in light of significant systemic disruptions like the COVID-19 pandemic. We focused on three key areas:

### 1. Impact of COVID-19 on Academic Outcomes

We analyzed how student achievement metrics (e.g., standardized test scores, GPA) changed before, during, and after the COVID-19 pandemic. Our methods included:

- Cohort comparisons and trend visualization  
- Difference-in-differences models  
- Subgroup analyses by race/ethnicity, economic status
- SAT participation  

### 2. Gifted Label Effects

To estimate the causal impact of being labeled as “gifted” in North Carolina public schools, we simulated a **randomized controlled trial** using observational data. We:

- Matched gifted and non-gifted students on observable characteristics using propensity scores and nearest-neighbor techniques  
- Created statistically similar pairs to control for confounding factors  
- Conducted a causal inference analysis to quantify the effect of the gifted label on future academic outcomes such as test scores, advanced course enrollment, and peer composition  

### 3. Early Dropout Prediction Using Machine Learning

We developed a machine learning pipeline to predict **high school dropout risk** as early as middle school. Key components include:

- Feature engineering from longitudinal academic and demographic data  
- Training models including XGBoost and logistic regression  
- Evaluation using ROC-AUC, precision-recall curves, and calibration  
- SHAP-based interpretability to identify key risk factors  

## 📌 Poster Presentation

A summary of our findings and methods is available in our project poster: **[View Poster](https://1drv.ms/p/c/d10f3c827d439696/EWxcg2O__UxEiw0DIHZastsBt0qQ388dUe4_wrIR3m4KNA?e=UnZO6G)**

## 📄 License & Acknowledgments

This repository is for **academic and non-commercial** use only. It adheres to all NCERDC data use agreements and contains no raw or identifiable student data.

Special thanks to the North Carolina Education Research Data Center for making this research possible.
