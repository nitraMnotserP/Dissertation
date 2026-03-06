## Dissertation Analysis Repository

This repository contains code for two sets of **logistic regression models** and **quantile regression analyses** used in my dissertation research.

### Logistic Regression Analyses

Two sets of logistic regression models are included:

- **College Enrollment** (binary outcome)
- **STEM Major Selection** (binary outcome)

Each outcome includes:

- A **data cleaning and wrangling script**
- A **multiple imputation script**
- The **analysis scripts for the final models**

All analyses account for:

- **Complex survey sampling procedures**
- **Multiple imputation (m = 30)**
- Calculation of **average marginal effects** to facilitate interpretation and comparability across logit models

### Quantile Regression Analyses

Quantile regression models estimate the **effect of socioeconomic status (SES) on mathematics achievement** at each decile of the achievement distribution. This allows the relationship between SES and achievement to vary across different points of the ability distribution.

### Future Additions

Structural equation models used in the dissertation will be added to this repository as they are developed.