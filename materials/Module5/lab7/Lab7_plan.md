### Lab 7: Bayesian updating and statistical evidence

**Lecture anchor:** Lecture 30

## Lesson Plan (50 minutes)

- 5 min Getting started: review prior, likelihood, evidence, and posterior using Bayes' rule
- 7 min Question 1: calculate the posterior probability for a positive diagnostic test and interpret it in context
- 6 min Question 2: rebuild the diagnostic-test result with a count table and compare the two explanations
- 9 min Question 3: use a grid approximation to update a flat prior after observing 8 heads in 10 coin flips
- 7 min Question 4: summarize the posterior with a posterior mean and 90% credible interval
- 7 min Question 5: change the prior and discuss how prior beliefs affect the posterior
- 6 min Question 6: increase the amount of data and examine why the prior has less influence
- 3 min Question 7 and closing: compare Bayesian and frequentist language; discuss the optional custom-prior challenge

The worksheet is pair work throughout. Collect it at the door.

## Before lab

- Print `Lab7_worksheet.pdf`, one per student
- Post `Lab7.R` and the worksheet PDF to Blackboard
- Set up the Blackboard item for the Tutorial 7 hash and confirm that it is open
- Set up the Gradescope item for the Worksheet
- Check Tutorial 7 hash submissions and remind students that Tutorial 7 is the pre-lab for today
- No external data file is needed; all examples are generated in the worksheet and starter script

## Notes

- Keep the four Bayesian terms distinct: the prior describes beliefs before the new data, the likelihood describes how compatible the data are with each parameter value, and the posterior combines both.
- In Question 1, emphasize base-rate information. Sensitivity is not the same as the probability that a positive test is correct.
- The count table in Question 2 is an audience-friendly version of the same Bayes calculation; ask students to explain the denominator as all positive tests.
- In Questions 3–4, the grid approximation treats the posterior values as a discrete distribution and normalizes them to sum to 1.
- In Question 5, students should describe how a skeptical prior changes the posterior without claiming that the prior is automatically wrong.
- Question 6 demonstrates that more data can reduce—but does not conceptually erase—the influence of a prior.
- In Question 7, distinguish Bayesian probability statements about parameters from frequentist long-run coverage or testing statements.
- After lab: collect the Lab 7 in-lab activity submission and remind students about the Project 2 writeup.

---
