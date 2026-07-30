# MA 213 Project 2 Week 2 Activity

## Project 2 Workday: Inference Results, Slides, and Code

**Purpose:** Use today to turn your Project 2 outline into evidence. Your group should leave with cleaned data, checked inference conditions, at least one draft categorical result, at least one draft numerical result, a results-slide plan, and assigned next tasks.

**Deliverable this activity supports:** Project 2 Deliverable 2: results slides, R code, and data file.

**Before submitting Deliverable 2:** Make sure your slides, code, and data all answer the research questions from your Project 2 outline. Your conclusions should be written in context, not only as p-values.

---

## 1. Group Check-in

**Group members:**

| Name | Completed since Week 1 | Question or blocker |
|---|---|---|
|  |  |  |
|  |  |  |
|  |  |  |
|  |  |  |

**Current project title:**

> 

**What changed since your Week 1 outline?**

> 

---

## 2. Research Question and Method Check

State the current version of each research question. Revise any question that is too broad, causal, or not answerable with your data.

**Categorical research question:**

> 

**Numerical research question:**

> 

| Analysis | Planned method | Variables used | Why this method matches the question |
|---|---|---|---|
| Categorical inference |  |  |  |
| Numerical inference |  |  |  |

**Method reminder:** Do not use `t.test()`, `chisq.test()`, or `prop.test()` for the main computations. Compute the test statistic manually. For ANOVA, `aov()`, `anova()`, and `lm()` are allowed.

---

## 3. Data and Cleaning Check

Confirm that the data file is ready to use in R.

| Check | Group notes |
|---|---|
| Data file name |  |
| Data source and citation |  |
| Observational unit |  |
| Number of rows and variables |  |
| Missing values in key variables |  |
| Variables recoded or renamed |  |
| Outliers or unusual values |  |
| Data file ready to submit |  |

**One cleaning decision your group made today:**

> 

**One data issue your group still needs to solve:**

> 

---

## 4. Categorical Inference Draft

Choose the categorical method that matches your question.

**Categorical method:**

- [ ] One proportion
- [ ] Difference of two proportions
- [ ] Chi-square goodness-of-fit
- [ ] Chi-square test of independence

**Variables used:**

> 

**Exploratory table or plot:**

| Evidence | What it shows |
|---|---|
| Frequency or contingency table |  |
| Bar plot or stacked bar plot |  |

**Hypotheses in context:**

| Hypothesis | Statement |
|---|---|
| Null hypothesis |  |
| Alternative hypothesis |  |

**Condition checks:**

| Condition | Met? | Evidence or explanation |
|---|---|---|
| Observations are independent |  |  |
| Data source is appropriate for the question |  |  |
| Counts or expected counts are large enough |  |  |

**Draft result:**

| Quantity | Value |
|---|---|
| Test statistic |  |
| Degrees of freedom, if needed |  |
| p-value |  |

**Conclusion in context:**

> 

---

## 5. Numerical Inference Draft

Choose the numerical method that matches your question.

**Numerical method:**

- [ ] One-sample mean
- [ ] Paired means
- [ ] Difference of two means
- [ ] Comparing many means with ANOVA
- [ ] Linear regression, if appropriate

**Variables used:**

> 

**Exploratory table or plot:**

| Evidence | What it shows |
|---|---|
| Summary statistics table |  |
| Histogram, boxplot, scatter plot, or QQ plot |  |

**Hypotheses in context:**

| Hypothesis | Statement |
|---|---|
| Null hypothesis |  |
| Alternative hypothesis |  |

**Condition checks:**

| Condition | Met? | Evidence or explanation |
|---|---|---|
| Observations are independent |  |  |
| Data source is appropriate for the question |  |  |
| Distribution is approximately normal or sample size is large enough |  |  |
| Equal variance is reasonable, if comparing groups |  |  |

**Draft result:**

| Quantity | Value |
|---|---|
| Test statistic |  |
| Degrees of freedom, if needed |  |
| p-value |  |
| Confidence interval, if included |  |

**Conclusion in context:**

> 

---

## 6. R Code Check

Your R code should be reproducible and readable. Someone else should be able to run it from top to bottom.

**Code checklist:**

- [ ] Loads required packages
- [ ] Imports the submitted data file
- [ ] Cleans or recodes variables clearly
- [ ] Creates required EDA tables and plots
- [ ] Checks inference conditions
- [ ] Computes categorical test statistic manually
- [ ] Computes numerical test statistic manually, unless using allowed ANOVA functions
- [ ] Uses clear object names
- [ ] Runs from top to bottom without errors

**One code section your group improved today:**

> 

**One code issue your group still needs to fix:**

> 

---

## 7. Results Slide Plan

Draft the structure for Deliverable 2. Keep the story focused: question, data, method, evidence, conclusion.

| Slide | Purpose | Speaker |
|---|---|---|
| Title | Project title, group members, and topic |  |
| Research questions | Categorical and numerical questions |  |
| Data | Source, observational unit, key variables, and cleaning decisions |  |
| Categorical inference | Method, key table or plot, test statistic, p-value, conclusion |  |
| Numerical inference | Method, key table or plot, test statistic, p-value, conclusion |  |
| Interpretation | What the results mean in context and one limitation |  |
| References and files | Citation, data file, and R code status |  |

**Main takeaway your audience should remember:**

> 

---

## 8. Feedback and Revision

Exchange with another group or meet with the instructor. Record feedback that you can actually use.

| Feedback type | Notes |
|---|---|
| Strongest part of the analysis |  |
| Unclear method or conclusion |  |
| Suggested revision |  |

**The feedback we will use first is:**

> 

**Why this feedback matters:**

> 

---

## 9. Roles and Next Steps

Assign specific tasks before leaving class. Each task should have one owner and one deadline.

| Group member | Next task before Deliverable 2 | Due date |
|---|---|---|
|  |  |  |
|  |  |  |
|  |  |  |
|  |  |  |

**Next group meeting or check-in time:**

> 

---

## Exit Ticket

Before leaving class, make sure your group has:

- [ ] Cleaned data loaded in R
- [ ] Categorical method selected
- [ ] Numerical method selected
- [ ] Hypotheses written in context
- [ ] Conditions checked for both analyses
- [ ] At least one draft test statistic or clear plan to compute it
- [ ] Results slide plan
- [ ] R code issue list
- [ ] Next task for each group member

**One question your group still has for the instructor:**

> 
