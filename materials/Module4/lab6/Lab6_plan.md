### Lab 6: Confidence intervals and hypothesis tests

**Lecture anchor:** Lecture 22

## Lesson Plan (50 minutes)

- 5 min Getting started: review the difference between a parameter and a statistic, then compute a sample proportion
- 7 min Question 1: construct and interpret a 95% confidence interval for one population proportion
- 6 min Question 2: test a hypothesis about the same proportion and compare the question answered by a test with the question answered by an interval
- 14 min Question 3: simulate repeated confidence intervals, estimate coverage, then change the confidence level and discuss the tradeoff between coverage and interval width
- 6 min Question 4: read `Titanic.csv` and build a two-way table and explain independence in context
- 7 min Questions 5–6: calculate expected counts, check conditions, and compute the chi-square statistic and p-value
- 5 min Question 7 and closing: wrap the chi-square workflow in a reusable function, check it on class and survival, then use it on sex and survival and state the conclusion in context

The worksheet is pair work throughout. Instructors rotate through pairs for three check-ins (after Q2, after Q3, and at the end) and initial each box. Pairs turn in as much as they finish; collect it at the door.

- Check-in 1 (after Q2)
  - Check: CI is about 0.568 to 0.702 and interpreted as a statement about the population proportion; z is about 3.82 and p is about 0.0001; conclusion is in context
  - Ask: What question does the CI answer, and what question does the test answer? Is 0.50 inside your CI, and how does that match your test decision? Why does the test SE use p0 while the CI SE uses p_hat? Why is qnorm right for z* but pnorm right for the p-value?
- Check-in 2 (after Q3)
  - Check: coverage is about 0.947 (95%) and 0.988 (99%); typical width is about 0.083 and 0.109
  - Ask: What does one row of `df_table` represent, and what does `included` record? Why isn't coverage exactly 95%? Does "95%" describe your interval from Q1 or the procedure? Why are 99% intervals wider, and why not always use 99%?
- Check-in 3 (end)
  - Check: class and survival gives chi-square about 190.4 with df = 3; sex and survival gives about 456.9 with df = 1; p-values reported as p < 0.001, not 0; conclusions stated in context
  - Ask: Your function doesn't check conditions. Are the conditions met for sex and survival, and how do you know? What do the expected counts represent? Why is df 1 for sex but 3 for class? Does a small p-value mean sex caused differences in survival?

## Before lab

- Print `Lab6_worksheet.pdf`, one per student
- Post `Titanic.csv`, `Lab6.R`, and the worksheet PDF to Blackboard
- Set up the Blackboard item for the Tutorial 6 hash and confirm that it is open
- Set up the Gradescope item for the Worksheet
- Check Tutorial 6 hash submissions and remind students that Tutorial 6 is the pre-lab for today
- Remind students to keep `Titanic.csv` (same file as Lab 3) and `Lab6.R` in the same folder; the simulation creates its own population

## Notes

- Keep the interpretation distinction explicit: a confidence interval estimates a population parameter, while a hypothesis test evaluates evidence against a null value.
- In the coverage simulation, students should understand that 95% describes the long-run procedure, not a probability assigned to one completed interval.
- When the confidence level changes from 95% to 99%, ask students to compare both coverage and interval width.
- For the chi-square work, make students state what independence means in context before calculating expected counts.
- Check that students verify expected counts before using the chi-square approximation and report both the p-value and the contextual conclusion.
- After lab: collect the Lab 6 in-lab activity submission and remind students about the Project 2 outline.

---
