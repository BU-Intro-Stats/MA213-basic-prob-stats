# Deliverable 2: Project 2 Results

*Complete this document as a group in Google Docs. Submit one document per group, along with your R code and data file. Due Thursday, Nov 12 at 9:00 PM.*

**How to use this template:** The sections match your Deliverable 1 outline. For each section, copy in your Deliverable 1 answers and revise and update them as needed. Then fill in the new **Results** prompts. Paste tables and figures directly into this document; every figure needs a title and labeled axes.

---

## Title and Group Information

**Project title:**



**Group members (name, BU ID):**

1.
2.
3.
4.

**TF/LA mentor (name, email address):**



---

## 1. Introduction

### 1a. Background and Motivation

**Topic or area of interest:**



**Why is this topic interesting or worth studying?**



**Main objective of the project:**



### 1b. Data Source and Description

**Data source (link/citation):**

**What does one row represent?**

**Number of rows used in your analysis (after cleaning):**

### 1c. Variables of Interest

| Variable name | Numerical or categorical? | Role in project | Notes |
|---|---|---|---|
|  |  |  |  |
|  |  |  |  |
|  |  |  |  |
|  |  |  |  |

### 1d. Data Cleaning

Report what you actually found and what you did about it.

---

## 2. Categorical Data Analysis

### 2a. Research Question and Variables

**Categorical research question:**



**Which variable(s) did you use?**



### 2b. Exploratory Data Analysis

**Summary table** (frequency table or contingency table):



**Figure** (e.g., bar plot or pie chart):



**What do the table and figure show?** (1–3 sentences)



### 2c. Hypothesis Test

**Test used:** (single proportion / difference of two proportions / chi-square goodness-of-fit / chi-square test of independence)



**Null and alternative hypotheses** (in words and in symbols):

- H₀:
- Hₐ:

**Conditions for inference** — list each condition, and show how you checked it (e.g., the number of successes and failures, or the smallest expected count):


**Test statistic** (and degrees of freedom, if applicable):

**p-value:**

**Significance level:**

### 2d. Results in Context

**Do you reject the null hypothesis? What does this mean for your research question?** (2–3 sentences, in the context of your data)



---

## 3. Numerical Data Analysis

### 3a. Research Question and Variables

**Numerical research question:**



**Which variable(s) did you use?**



### 3b. Exploratory Data Analysis

**Summary statistics table** (e.g., n, mean, median, sd, IQR, by group if relevant):



**Figure** (e.g., histogram, boxplot, bar plot, or scatterplot):



**What do the table and figure show?** (1–3 sentences: shape, center, spread, unusual values)



### 3c. Hypothesis Test

**Test used:** (one-sample mean / paired means / difference of two means / ANOVA)



**Null and alternative hypotheses** (in words and in symbols):

- H₀:
- Hₐ:

**Conditions for inference** — list each condition, and show how you checked it (e.g., sample size, a histogram or boxplot for normality, similar spreads across groups):



**Test statistic** (and degrees of freedom):

**p-value:**

**Significance level:**

### 3d. Results in Context

**Do you reject the null hypothesis? What does this mean for your research question?** (2–3 sentences, in the context of your data)



---

## 4. Conclusion (draft)

**Key findings** (one or two bullet points per analysis):

-
-

**Limitations** (e.g., how the data were collected, conditions that were borderline, why the results are not causal):

-

**Questions for future work:**

-

---

## 5. References

1.

---

## Roles and Timeline

**Roles by deliverable** — update with what each member actually did for Deliverable 2, and plan Deliverable 3.

| Group member | Deliverable 1 (Outline) | Deliverable 2 (Results + R code + data) | Deliverable 3 (Written report + R code + data) |
|---|---|---|---|
|  |  |  |  |
|  |  |  |  |
|  |  |  |  |
|  |  |  |  |

**Next group meeting or check-in time:**



---

## R Code and Data Submission

Submit these alongside this document:

- Your **R code** as a `.R` or `.Rmd` file. It should be well-commented, run from start to finish without errors, and produce every table, figure, test statistic, and p-value in this document.
- Your **data file**, if it is needed to run your code. The code should run when the data file is in the same folder.
- Only use functions and packages covered in class, tutorials, or labs, or that are part of the standard R distribution.
- Do not use functions that directly perform hypothesis tests (e.g., `t.test()`, `chisq.test()`, `prop.test()`). Compute test statistics and p-values step by step. If you use ANOVA, you may use `aov()`, `anova()`, or `lm()`.

---

## Submission Checklist

Before submitting, make sure your group has:

- [ ] Revised all Deliverable 1 sections based on feedback
- [ ] Reported data cleaning steps (1d)
- [ ] Categorical: summary table, labeled figure, and description (2b)
- [ ] Categorical: hypotheses, conditions checked, test statistic, p-value (2c), and results in context (2d)
- [ ] Numerical: summary statistics table, labeled figure, and description (3b)
- [ ] Numerical: hypotheses, conditions checked, test statistic, p-value (3c), and results in context (3d)
- [ ] Draft conclusion (4) and references (5)
- [ ] Roles updated
- [ ] R code (and data file, if needed) submitted, and the code runs from start to finish

**Optional: One question your group still has for the instructor:**


