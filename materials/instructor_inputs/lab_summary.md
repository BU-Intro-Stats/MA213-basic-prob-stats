# MA 213 Lab Summary

<!--
Instructor notes:
- Calendar: each Lab/Project heading becomes a compact calendar event. Lecture Anchor appears as metadata; Purpose, Primary Objectives, activities, Deliverables, and prerequisite flags are not shown on the lab meeting event.
- Calendar due events: Deliverables create separate "Deliverables Due" calendar events on the tagged weekday, such as `[Thursday] Tutorial 2`. Untagged deliverables use the day set in Lab_schedules.md.
- Weekly schedule: Lab/Project headings fill the "Labs" column. Deliverables fill the "Lab Deliverables" column.
- Primary Objectives: used for fallback prerequisite matching when learningObjectives.md tags are missing; they are not displayed directly in the weekly schedule or calendar.
-->

This version preserves the original course sequence so that the lab flow stays familiar to students while still being aligned with the lecture schedule and learning objectives.

## Lab details

### Lab 1: Lab orientation, R setup, and Tutorial 0

- **Lecture Anchor:** Lecture 1
- **Purpose:** Onboard students to how lab works for the rest of the semester and get everyone set up to do the hands-on work: a working R and RStudio install, the tutorial-to-Blackboard submission habit, and a first data import.
- **Primary Objectives:**
  - Use R for Data Management and Exploration
  - Carry out a reproducible statistical workflow in R
- **Pre-Lab Activity:**
  - Bring a laptop
  - Review the Lab 1 materials before lab
- **In-Lab Activity:**
  - Lab orientation: lab structure, weekly rhythm, grading, projects, and ground rules
  - Install and verify R and RStudio
  - Complete Tutorial 0 and submit the hash code to Blackboard
  - Import a data set with `read.csv()` and compute a simple column summary

- **Deliverables:**

---

### Lab 2: Data transformation and interpretation

- **Lecture Anchor:** Lecture 3
- **Purpose:** Reinforce the ideas from the data lectures by asking students to move from raw data to meaningful descriptions.
- **Primary Objectives:**
  - Classify and Analyze Variables
  - Describe Data Distributions
  - Use R for Data Management and Exploration
- **Pre-Lab Activity:**
  - Review the lecture notes on categorical and numerical variables
  - Complete a short tutorial on `dplyr` and plotting
- **In-Lab Activity:**
  - Decide whether variables are categorical or numerical
  - Use `filter()`, `mutate()`, and `summarize()`
  - Write interpretations in context

- **Deliverables:**
  - [Friday] Lab2 in-lab activity submission
  - [Thursday] Tutorial 2

---

### Lab 3: Data visualization, and interpretation

- **Lecture Anchor:** Lecture 6
- **Purpose:** Reinforce the ideas from the data to visualization by asking students to move from raw data to meaningful descriptions using visualization.
- **Primary Objectives:**
  - Classify and Analyze Variables
  - Visualize and Describe Data Distributions
  - Use R for Data Management and Exploration
- **Pre-Lab Activity:**
  - Review the lecture notes on categorical and numerical variables
  - Complete Tutorial 3 and submit the hash code to Blackboard
- **In-Lab Activity:**
  - Decide whether variables are categorical or numerical
  - Use `filter()`, `mutate()`, `group_by()`, and `summarize()`
  - Create appropriate plots for the variable type
  - Compare counts against proportions
  - Write interpretations in context

- **Deliverables:**
  - [Friday] Lab3 in-lab activity submission
  - [Thursday] Tutorial 3

---

### Project1-1: Project 1 launch

- **Lecture Anchor:**
- **Purpose:** Use this session after Lab 3 to introduce the project prompt, rubric, data access, and timeline. Students should leave with a clear project question and a first analysis plan.
- **Primary Objectives:**
- **Pre-Lab Activity:**
- **In-Lab Activity:**
- **Post-Lab Activity:**
- **Deliverables:**
  - Project 1 plan

---

### Lab 4: Probability rules and Expectation/Variance Rules

- **Lecture Anchor:** Lecture 11
- **Purpose:** Connect the lecture discussion of probability to concrete practice with events, tables, and simulation. The lab is deliberately simulation-first: students observe each result in simulated data before the corresponding formula is introduced.
- **Primary Objectives:**
  - Validate and Explain Probability Distributions
  - Compute Probabilities Using Various Tools
  - Apply the Law of Large Numbers and Its Implications
  - Understand and Compute Expectations and Variances
- **Pre-Lab Activity:**
  - Review probability notation and event language
  - Complete a tutorial on simulation basics in R
- **In-Lab Activity:**
  - Estimate probabilities from simulated rolls and check that they sum to 1
  - Simulate a dice game first, then derive the expected value formula that explains the observed long-run average
  - Visualize the law of large numbers, and see that a simulated average is itself a random quantity
  - Compute probabilities using diagrams, tables, and formulas, using `mean()` on TRUE/FALSE vectors
  - Test the addition and multiplication rules against simulated estimates
  - Explore whether outcomes are equally likely or not
  - Settle the linear-combination variance rule by simulation, then break independence to see which rule fails
- **Post-Lab Activity:**
  - Submit a worksheet with probabilities and interpretation
- **Deliverables:**
  - Project 1 Outline
  - [Friday] Lab4 in-lab activity submission
  - [Thursday] Tutorial 4

---

### Lab 5: Simulating LLN/CLT with Different Distributions

- **Lecture Anchor:** Lecture 14
- **Purpose:** Help students see how probability ideas become useful for understanding distributions and unusual data values.
- **Primary Objectives:**
  - Understand and Compute Expectations and Variances
  - Model Data Using Bernoulli, Geometric, and Binomial Distributions
  - Assess Data Using the Normal Distribution
- **Pre-Lab Activity:**
  - Review expected value, variance, and normal approximation ideas
- **In-Lab Activity:**
  - Compute expected value and variance from a distribution
  - Compare different probability models
  - Use R to examine normality and unusual observations

- **Deliverables:**
  - [Friday] Lab5 in-lab activity submission
  - [Thursday] Tutorial 5

---

### Project1-2: Project 1 workday

- **Lecture Anchor:**
- **Purpose:** Use this session after Lab 5 to help students refine plots, summaries, and presentation structure. The emphasis should remain on data interpretation and clear communication.
- **Primary Objectives:**
- **Pre-Lab Activity:**
- **In-Lab Activity:**
- **Post-Lab Activity:**
- **Deliverables:**

---

### Project2-1: Project 2 launch

- **Lecture Anchor:**
- **Purpose:** Use this session before or near Lab 6 so students can begin connecting inference methods to their report. Students should identify candidate response and explanatory variables and think about possible methods.
- **Primary Objectives:**
- **Pre-Lab Activity:**
- **In-Lab Activity:**
- **Post-Lab Activity:**
- **Deliverables:**
  - Project1 Video

---

### Lab 6: Confidence intervals and hypothesis tests

- **Lecture Anchor:** Lecture 22
- **Purpose:** Give students practice constructing and interpreting confidence intervals, testing a one-proportion hypothesis, and carrying out a chi-square test of independence.
- **Primary Objectives:**
  - Inference for a Single Proportion
  - Understand Errors and Significance Levels
  - Distinguish Statistical vs. Practical Significance
  - Conduct and Interpret Chi-Square Tests
- **Pre-Lab Activity:**
  - Review the difference between confidence intervals and hypothesis tests
- **In-Lab Activity:**
  - Construct and interpret a confidence interval for one population proportion
  - Compare confidence intervals with hypothesis tests using the same sample
  - Simulate confidence interval coverage and examine the effect of confidence level
  - Build a two-way table, calculate expected counts, and conduct a chi-square test of independence
  - Interpret results in context
- **Deliverables:**
  - [Friday] Lab6 in-lab activity submission
  - [Thursday] Tutorial 6
  - Project2 Outline

---

### Project2-2: Project 2 workdays

- **Lecture Anchor:**
- **Purpose:** Use these sessions around the inference unit to support analysis, revision, peer review, and report polishing. Students should be encouraged to justify both the method choice and the interpretation.
- **Primary Objectives:**
- **Pre-Lab Activity:**
- **In-Lab Activity:**
- **Post-Lab Activity:**
- **Deliverables:**

---

### Project2-3: Project 2 workdays

- **Lecture Anchor:**
- **Purpose:** Use these sessions around the inference unit to support analysis, revision, peer review, and report polishing. Students should be encouraged to justify both the method choice and the interpretation.
- **Primary Objectives:**
- **Pre-Lab Activity:**
- **In-Lab Activity:**
- **Post-Lab Activity:**
- **Deliverables:**
  - Project2 Progress Report

---

### Lab 7: Bayesian updating

- **Lecture Anchor:** Lecture 30
- **Purpose:** End the course with a focused modeling activity that connects conditional probability, evidence, and Bayesian updating to the inference methods already studied.
- **Primary Objectives:**
  - Compare Bayesian and Frequentist Approaches
- **Pre-Lab Activity:**
  - Review the final lecture material on modeling and inference
- **In-Lab Activity:**
  - Use Bayes' rule and a grid approximation to compare prior and posterior thinking
  - Interpret posterior summaries and compare Bayesian and frequentist language
- **Post-Lab Activity:**
  - Submit the completed Lab 7 worksheet and starter code
- **Deliverables:**
  - Project2 writeup
  - [Friday] Lab7 in-lab activity submission
  - [Thursday] Tutorial 7

---

### Lab 8: extra

- **Lecture Anchor:**
- **Purpose:**
- **Primary Objectives:**
- **Pre-Lab Activity:**
- **In-Lab Activity:**
- **Post-Lab Activity:**
- **Deliverables:**
  - Project2 Resubmission

---
