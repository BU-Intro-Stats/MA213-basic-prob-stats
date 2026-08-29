### Lab 6: Confidence intervals and hypothesis tests

**Lecture anchor:** Lecture 22

## Lesson Plan (50 minutes)

- 5 min Getting started: review the difference between a parameter and a statistic, then compute a sample proportion
- 7 min Question 1: construct and interpret a 95% confidence interval for one population proportion
- 6 min Question 2: test a hypothesis about the same proportion and compare the question answered by a test with the question answered by an interval
- 9 min Question 3: simulate repeated confidence intervals and estimate coverage
- 5 min Question 4: change the confidence level and discuss the tradeoff between coverage and interval width
- 6 min Question 5: convert the built-in `Titanic` table into a two-way table and explain independence in context
- 7 min Questions 6–7: calculate expected counts, check conditions, and compute the chi-square statistic and p-value
- 5 min Question 8 and closing: repeat the chi-square workflow for sex and survival, then state the conclusion in context

The worksheet is pair work throughout. Collect it at the door.

## Before lab

- Print `Lab6_worksheet.pdf`, one per student
- Post `Lab6.R` and the worksheet PDF to Blackboard
- Check Tutorial 6 hash submissions and remind students that Tutorial 6 is the pre-lab for today
- Explain that no external data file is needed: the first simulation creates a population and the second analysis uses R's built-in `Titanic` table
- Have `install.packages(c("dplyr", "ggplot2"))` ready to project

## Notes

- Keep the interpretation distinction explicit: a confidence interval estimates a population parameter, while a hypothesis test evaluates evidence against a null value.
- In the coverage simulation, students should understand that 95% describes the long-run procedure, not a probability assigned to one completed interval.
- When the confidence level changes from 95% to 99%, ask students to compare both coverage and interval width.
- For the chi-square work, make students state what independence means in context before calculating expected counts.
- Check that students verify expected counts before using the chi-square approximation and report both the p-value and the contextual conclusion.
- After lab: collect the Lab 6 in-lab activity submission and remind students about the Project 2 outline.

---
