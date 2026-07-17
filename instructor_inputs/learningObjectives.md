# MA 213 Learning Objectives

<!--
Instructor notes:
- Calendar/weekly schedule: objective text is not shown directly, but tags such as Quiz1, Lab6, and Project2 drive prerequisite checks and instructor flags.
- Lecture links: lecture_summary.md should use objective codes in M#.L## format so the generator can match coverage timing.
- The table below is shown on the learning objectives page; bold entries indicate Core objectives.
-->

|          |     |     |     |     |     |     |     |
|----------|-----|-----|-----|-----|-----|-----|-----|
| **Quiz 1** | **M1.L01** | **M1.L02** | **M1.L03** | **M1.L04** | M1.L05 | | |
| **Quiz 2** | **M2.L01** | **M2.L02** | **M2.L03** | **M2.L04** | **M2.L05** | **M2.L06** | |
| **Quiz 3** | **M3.L01** | **M3.L02** | **M3.L03** | **M3.L04** | **M3.L05** | M3.L06 | |
| **Quiz 4** | **M4.L01** | M4.L02 | M4.L03 | **M4.L04** | **M4.L05** | M4.L06 | M4.L07 |
| **Quiz 5** | M5.L01 | M5.L02 | M5.L03 | M5.L04 | M5.L05 | | |

**Bold** = Core &nbsp;&nbsp; plain = Auxiliary

---

## Module 1: Exploratory Data Analysis and Study Design
*(Chapters 1 and 2; 5 objectives)*

1. **Classify and Analyze Variables:** Categorize variables based on their types (e.g., numerical/categorical, continuous/discrete, ordinal), assess their association (positive, negative, or independent), and determine which make sense as explanatory vs. response variables. [Quiz1, Lab1, Lab2, Lab3] **Core**
2. **Evaluate Study Design and Its Implications:** Identify and explain experimental design choices (observational vs. experimental, sampling methods, blinding, potential biases), and judge whether results can be generalized to a population or used to infer causation. [Quiz1, Lab1, Project1] **Core**
3. **Use R for Data Management and Exploration:** Utilize R to load, pre-process, and explore data through visualization and summarization techniques. [Quiz1, Lab1, Lab2, Lab3] **Core**
4. **Visualize and Describe Data Distributions:** Select appropriate visualizations (scatterplots, histograms, box plots, bar plots) to depict data, and describe distributions qualitatively (shape, center, spread, outliers) and quantitatively (mean, median, mode, range, IQR, standard deviation). [Quiz1, Lab2, Lab3, Project1] **Core**
5. **Conduct Hypothesis Testing Using Simulation:** Set up null and alternative hypotheses to test for independence between variables, and use simulation techniques to evaluate data support for these hypotheses. [Quiz1] Auxiliary

---

## Module 2: Probability, Random Variables, and Distributions
*(Chapters 3 and 4; 6 objectives)*

1. **Validate and Explain Probability Distributions:** Assess the validity of a probability distribution using the concepts of outcome, sample space, and probability properties (e.g., disjoint outcomes, probabilities between 0 and 1, and total probabilities summing to 1). [Quiz2, Lab4] **Core**
2. **Apply the Law of Large Numbers and Its Implications:** Explain the Law of Large Numbers, why it holds, and its implications for predicting long-term averages in probability and statistics. [Quiz2, Lab4] **Core**
3. **Compute Probabilities Using Various Tools:** Use logic, Venn diagrams, and probability rules to compute probabilities for events. [Quiz2, Lab4] **Core**
4. **Understand and Compute Expectations and Variances:** Explain the concepts of expectations and variances of random variables, and compute the expectation and variance of a linear combination of random variables. [Quiz2, Lab5] **Core**
5. **Model Data Using Bernoulli, Geometric, and Binomial Distributions:** Recognize when to appropriately model data using the Bernoulli, geometric, and binomial distributions, and compute quantities of interest such as mean, standard deviation, and tail probabilities. [Quiz2, Lab5] **Core**
6. **Assess Data Using the Normal Distribution:** Use the normal distribution to assess the "unusualness" of data points, apply the 68-95-99.7% rule, evaluate normality through histograms, and determine when a normal approximation to the binomial model is valid for calculating binomial probabilities. [Quiz2, Lab5] **Core**

---

## Module 3: Foundations for Inference
*(Chapter 5–Chapter 6.1; 6 objectives)*

1. **Understand Point Estimates and Sampling Variability:** Define a sample statistic (point estimate) for a population parameter, and explain how it varies across different samples. [Quiz3] **Core**
2. **Visualize and Interpret Sampling Distributions:** Draw and interpret sampling distributions for a point estimate (e.g., population proportion) across different sample sizes, explaining how the distribution changes as the sample size increases. [Quiz3] **Core**
3. **Inference for a Single Proportion:** Design, execute, and interpret confidence intervals and hypothesis tests for a single proportion. [Quiz3, Lab6, Project2] **Core**
4. **Understand Errors and Significance Levels:** Identify Type I and Type II errors and explain how they are influenced by changes in the significance level. [Quiz3, Lab6] **Core**
5. **Distinguish Statistical vs. Practical Significance:** Differentiate between statistical significance and practical significance, and explain the implications of each. [Quiz3, Lab6, Project2] **Core**
6. **Calculate Sample Size for Confidence Intervals:** Calculate the required minimum sample size for a given margin of error and confidence level. [Quiz3] Auxiliary

---

## Module 4: Inference
*(Chapters 6 and 7; 7 objectives)*

1. **Inference for a Difference in Proportions:** Design, execute, and interpret confidence intervals and hypothesis tests for a difference in proportions. Choose when to use a paired test and when to use an independent test. [Quiz4, Lab6, Project2] **Core**
2. **Conduct and Interpret Chi-Square Tests:** Assess whether the conditions for a chi-square test (goodness of fit or independence) are met, and if so, design, execute, and interpret the test. [Quiz4, Lab6, Project2] Auxiliary
3. **Explain and Use the t-Distribution:** Explain how the t-distribution differs from the normal distribution and why it is used for population mean inference. [Quiz4] Auxiliary
4. **Conduct and Interpret t-Tests:** Design, execute, and interpret t-tests for a single population mean, a difference of paired means, and a difference of independent means, calculating the standard error appropriately for each. Describe how to obtain a p-value for a t-test and a critical t-score for a confidence interval. [Quiz4, Project2] **Core**
5. **Explain Hypothesis Testing and Its Limitations:** Discuss the use cases and potential issues with hypothesis testing, including the interpretation of results. [Quiz4, Project2] **Core**
6. **Calculate Test Power and Evaluate Factors:** Calculate the power of a test for a given effect size and significance level, and explain how the power would change with variations in effect size, sample size, significance level, or standard error. [Quiz5] Auxiliary
7. **Conduct and Interpret ANOVA:** Assess whether conditions for an ANOVA are met, and if so, design, execute, and interpret the test to compare sample means across several groups. [Quiz5] Auxiliary

---

## Module 5: Beyond Recipes: Frequentist and Bayesian Approaches to Statistical Modeling
*(*Bayes' Rules*, Chapters 1–3; 5 objectives: 5 auxiliary)*

1. **The Frequentist Perspective:** Explain how Frequentist modeling uses sampling distributions to evaluate estimators. Describe the difference between a sampling distribution and a likelihood. Use maximum likelihood to estimate a parameter and identify which of the estimators in the course are maximum likelihood estimators. [Quiz5] Auxiliary
2. **The Bayesian Perspective:** Explain how Bayesian modeling uses likelihoods, priors, and posteriors to perform inference. Explain how Bayesian analyses can be updated sequentially as new data arrives and how the results remain invariant to the order in which the data is observed. [Quiz5] Auxiliary
3. **Comparing Bayesian and Frequentist Approaches:** Describe the key differences between Bayesian and frequentist approaches to statistical inference and reasoning, including providing examples of when each method is appropriate. [Quiz5, Lab7] Auxiliary
4. **Bayesian Modeling 1:** Construct Bayesian models for discrete distributions and identify the likelihood, prior, and posterior, and use the probability rules from Module 2 to compute conditional and marginal probabilities for the models. [Quiz5] Auxiliary
5. **Bayesian Modeling 2:** Apply and interpret the beta-binomial model, including understanding the prior and posterior distributions, and evaluating the influence of the prior relative to the likelihood. [Quiz5] Auxiliary

---

## Module 6: Global Module
*(Shared with MA 214; 4 objectives)*

1. I can carry out a complete, reproducible statistical workflow in R (load data, run exploratory analyses, transform variables, run statistical analyses, display and interpret results) using the inference methods from the course. [Lab1, Project1, Project2] **Core**
2. Given R code for a statistical analysis, I can explain what it does and why (in terms of the methods from the course), and identify both programmatic and statistical errors. [Quizzes] **Core**
3. When solving probability and statistics problems, I can support my answers by writing out the steps using the notation and conventions of statistical exposition. [Quizzes] **Core**
4. I can recognize whether a statistical workflow is appropriate for the given data and data analysis goals, and explain the results of a statistical analysis to stakeholders
   1. as a presentation, and [Project1] **Core**
   2. in a written report. [Project2] **Core**
