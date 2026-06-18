# MA 213 Lab Summary

This version preserves the original course sequence so that the lab flow stays familiar to students while still being aligned with the lecture schedule and learning objectives.

## Sequence that follows the original course order

- Each week includes 3 lectures and 1 lab.
- The lab/project flow below follows the original sequence: Lab 1, Lab 2, Lab 3, P1-1, Lab 4, Lab 5, P1-2, P2-1, Lab 6, P2-2/P2-3, Lab 7.
- The lecture anchors are used to show where each item best fits in the course timeline.

| Sequence item | Lecture anchor | Main purpose | Main objective focus |
| --- | --- | --- | --- |
| Lab 1 | Lecture 1 | R workflow and first data exploration | Module 1 and Module 6 |
| Lab 2 | Lecture 3 | Data transformation and visualization | Module 1 |
| Lab 3 | Lecture 5 | Probability foundations and simulation basics | Module 2 |
| P1-1 | Lecture 7 or soon after | Project 1 launch and planning | Modules 1 and 6 |
| Lab 4 | Lecture 9 or 10 | Random variables, expectation, variance, and normality | Module 2 |
| Lab 5 | Lecture 15 or 16 | Sampling variability and CLT | Module 3 |
| P1-2 | After Lab 5 | Project 1 workday and feedback | Modules 1 and 6 |
| P2-1 | Before Lab 6 | Project 2 launch and planning | Modules 3–6 |
| Lab 6 | Lecture 18 or 20 | Confidence intervals and hypothesis tests | Modules 3 and 4 |
| P2-2/P2-3 | Around Lab 6 and after | Project 2 workdays and report revisions | Modules 5 and 6 |
| Lab 7 | Lecture 26 or later | Regression or Bayesian extension | Module 4 and optional Module 5 |

## How this sequence connects to lectures and objectives

- The original order is preserved so students see the planned progression clearly.
- Labs 1–3 build foundational R and data/probability skills before Project 1 begins.
- Lab 4 and Lab 5 connect probability and sampling ideas to the later inference units.
- Project workdays are placed exactly where they support the course flow rather than replacing the lab sequence.

## Skills labs and projects

### Skills lab
- Labs 1–7 are designed to build the computational and statistical workflow used throughout the course.
- Every lab should include a short pre-lab preparation, a guided in-lab activity, and a post-lab submission.

### Projects
- Project 1: presentation-based exploratory analysis
- Project 2: written report using inference and modeling tools

## Tutorial plan

Tutorials should be used as preparation for the corresponding lab so students arrive ready to use the tools.

| Tutorial | Suggested purpose |
| --- | --- |
| Tutorial 1 | R basics, loading data, R Markdown/Quarto |
| Tutorial 2 | Data wrangling, variable types, summaries |
| Tutorial 3 | Plotting and interpretation |
| Tutorial 4 | Probability rules, simulation, and random variables |
| Tutorial 5 | Sampling distributions and inference in R |
| Tutorial 6 | Regression or Bayesian modeling extension |

## Lab details

### Lab 1: R Intro and first data exploration

**Lecture anchor:** Lecture 1  
**Purpose:** Give students a low-stress entry into the course workflow and connect the first lecture ideas about data and study design to hands-on practice.

**Primary objectives supported**
- Classify and Analyze Variables
- Evaluate Study Design and Its Implications
- Use R for Data Management and Exploration
- Carry out a reproducible statistical workflow in R

**Pre-lab activity**
- Complete the R onboarding site
- Review the course workflow for scripts, data files, and submissions

**In-lab activity**
- Load a data set in R
- Inspect rows, columns, and variable types
- Compute simple summaries and make initial plots
- Save and knit a reproducible document

**Post-lab activity**
- Submit a short R-based reflection or guided worksheet

**Deliverables**
- Lab1 in-lab activity submission 
- Tutorial 1 (pre-lab for lab2)

---

### Lab 2: Data transformation, visualization, and interpretation

**Lecture anchor:** Lecture 3  
**Purpose:** Reinforce the ideas from the data and visualization lectures by asking students to move from raw data to meaningful descriptions.

**Primary objectives supported**
- Classify and Analyze Variables
- Visualize and Describe Data Distributions
- Use R for Data Management and Exploration

**Pre-lab activity**
- Review the lecture notes on categorical and numerical variables
- Complete a short tutorial on `dplyr` and plotting

**In-lab activity**
- Decide whether variables are categorical or numerical
- Use `filter()`, `mutate()`, and `summarize()`
- Create appropriate plots for the variable type
- Write interpretations in context

**Post-lab activity**
- Submit a short analysis summary with plots and written conclusions

**Deliverables**
- Lab2 in-lab activity submission 
- Tutorial 2 (pre-lab for lab3)

---

### Lab 3: Probability rules and simulation

**Lecture anchor:** Lecture 5  
**Purpose:** Connect the lecture discussion of probability to concrete practice with events, tables, and simulation.

**Primary objectives supported**
- Validate and Explain Probability Distributions
- Compute Probabilities Using Various Tools
- Apply the Law of Large Numbers and Its Implications

**Pre-lab activity**
- Review probability notation and event language
- Complete a tutorial on simulation basics in R

**In-lab activity**
- Compute probabilities using diagrams, tables, and formulas
- Compare theoretical and simulated probabilities
- Explore whether outcomes are equally likely or not

**Post-lab activity**
- Submit a worksheet with probabilities and interpretation

**Deliverables**
- Lab3 in-lab activity submission 
- Tutorial 3 (pre-lab for lab4)

---

### Lab 4: Random variables, expectation, variance, and normality

**Lecture anchor:** Lecture 9 or 10  
**Purpose:** Help students see how probability ideas become useful for understanding distributions and unusual data values.

**Primary objectives supported**
- Understand and Compute Expectations and Variances
- Model Data Using Bernoulli, Geometric, and Binomial Distributions
- Assess Data Using the Normal Distribution

**Pre-lab activity**
- Review expected value, variance, and normal approximation ideas

**In-lab activity**
- Compute expected value and variance from a distribution
- Compare different probability models
- Use R to examine normality and unusual observations

**Post-lab activity**
- Submit a short interpretation of the model choices and results

**Deliverables**
- Lab4 in-lab activity submission 
- Tutorial 4 (pre-lab for lab5)

---

### Lab 5: Sampling distributions and CLT

**Lecture anchor:** Lecture 15 or 16  
**Purpose:** Make sampling variability visible so students can understand why inference is possible.

**Primary objectives supported**
- Understand Point Estimates and Sampling Variability
- Visualize and Interpret Sampling Distributions
- Inference for a Single Proportion

**Pre-lab activity**
- Review sampling distributions and the CLT

**In-lab activity**
- Simulate repeated samples from different distributions
- Compare sample means and sampling distributions
- Discuss how sample size affects variability

**Post-lab activity**
- Submit a short summary of what the simulations show

**Deliverables**
- Lab5 in-lab activity submission 
- Tutorial 5 (pre-lab for lab6)

---

### Lab 6: Confidence intervals and hypothesis tests

**Lecture anchor:** Lecture 18 or 20  
**Purpose:** Give students practice choosing and interpreting inference procedures, not just computing formulas.

**Primary objectives supported**
- Inference for a Single Proportion
- Understand Errors and Significance Levels
- Distinguish Statistical vs. Practical Significance
- Design, execute, and interpret confidence intervals and hypothesis tests

**Pre-lab activity**
- Review the difference between confidence intervals and hypothesis tests

**In-lab activity**
- Choose the appropriate inference method for a given setting
- Check conditions in R
- Compute intervals, p-values, and effect sizes
- Interpret results in context

**Post-lab activity**
- Submit a graded inference worksheet

**Deliverables**
- Lab6 in-lab activity submission 
- Tutorial 6 (pre-lab for lab7)

---

### Lab 7: Bayesian 

**Lecture anchor:** Lecture 26 or later  
**Purpose:** End the course with a flexible application that connects modeling ideas to the methods already studied.

**Primary objectives supported**
- Compare Bayesian and Frequentist Approaches

**Pre-lab activity**
- Review the final lecture material on modeling and inference

**In-lab activity**
- Option A: fit a linear model and interpret slope, residuals, and fit
- Option B: use a Bayesian update framework to compare prior and posterior thinking

**Post-lab activity**
- Submit a brief interpretation of the chosen modeling approach

**Deliverables**
- Lab6 in-lab activity submission 

---

## Project support sessions in the original order

### P1-1: Project 1 launch
- Use this session after Lab 3 to introduce the project prompt, rubric, data access, and timeline.
- Students should leave with a clear project question and a first analysis plan.

### P1-2: Project 1 workday
- Use this session after Lab 5 to help students refine plots, summaries, and presentation structure.
- The emphasis should remain on data interpretation and clear communication.

### P2-1: Project 2 launch
- Use this session before or near Lab 6 so students can begin connecting inference methods to their report.
- Students should identify candidate response and explanatory variables and think about possible methods.

### P2-2/P2-3: Project 2 workdays
- Use these sessions around the inference unit to support analysis, revision, peer review, and report polishing.
- Students should be encouraged to justify both the method choice and the interpretation.

