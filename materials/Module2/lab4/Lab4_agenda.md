### Lab 4: Probability rules and expectation/variance rules

**Lecture anchor:** Lecture 11

## Lesson Plan (50 minutes)

- 5 min Getting started: load `dplyr`/`ggplot2`, simulate fair and weighted die rolls, and compare observed proportions with the true probabilities
- 7 min Question 1: play Game A, record outcomes, and compare short-run averages with a partner
- 7 min Question 2: use the outcome and probability table to calculate the expected value of Game A
- 8 min Question 3: visualize the law of large numbers and compare the variability of averages from 100 versus 1,000 games
- 7 min Question 4: calculate and interpret the variance and standard deviation of Game A
- 8 min Question 5: discover the addition and multiplication rules by comparing simulated and theoretical probabilities
- 5 min Question 6: simulate the two-dice game and compare the expected value and variance with Game A
- 3 min Question 7 and closing: break independence with a shared die and explain which variance rule changes

The worksheet is pair work throughout. Collect it at the door.

## Before lab

- Print `Lab4_worksheet.pdf`, one per student
- Post `Lab4.R`, `Distribution_table.tex`, and the worksheet PDF to Blackboard
- Check Tutorial 4 hash submissions and remind students that Tutorial 4 is the pre-lab for today
- Assign pairs with `group_divider.py` if the class list is available
- Have `install.packages(c("dplyr", "ggplot2"))` ready to project

## Notes

- Students should simulate first and introduce formulas only after they have a numerical pattern to explain.
- Do not set a seed for Game A: different partner results are useful evidence that short-run averages vary.
- Use `set.seed()` from Question 3 onward so the submitted graphs and numerical results are reproducible.
- Emphasize the difference between expected value (long-run center) and variance (how much outcomes or averages spread out).
- In Question 5, check that students distinguish the addition rule from multiplication and understand when independence is required.
- Question 7 intentionally breaks independence by using one die for both terms. The expected value remains additive, but the variance calculation that assumes independence does not.
- After lab: collect the Lab 4 in-lab activity submission.

---
