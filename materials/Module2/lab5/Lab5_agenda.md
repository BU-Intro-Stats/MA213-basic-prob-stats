### Lab 5: Sampling distributions and the central limit theorem

**Lecture anchor:** Lecture 14

## Lesson Plan (50 minutes)

- 5 min Getting started: distinguish population, sample, parameter, and statistic; load `dplyr`/`ggplot2`
- 6 min Question 1: build the finite population and inspect its shape and summary statistics
- 6 min Question 2: draw one sample and compare the sample mean with the population mean
- 10 min Question 3: repeat the sampling process and visualize the sampling distribution of the sample mean
- 6 min Question 4: write and test a reusable simulation function
- 7 min Question 5: vary the sample size and observe how the sampling distribution changes
- 5 min Question 6: vary the number of repetitions and distinguish simulation resolution from sampling variability
- 5 min Question 7 and closing: compare theoretical standard error with simulated spread and complete the final reflection

The worksheet is pair work throughout. Collect it at the door.

## Before lab

- Print `Lab5_worksheet.pdf`, one per student
- Post `Lab5.R` and the worksheet PDF to Blackboard
- Check Tutorial 5 hash submissions and remind students that Tutorial 5 is the pre-lab for today
- Have `install.packages(c("dplyr", "ggplot2"))` ready to project

## Notes

- Establish the distinction between the population distribution and the sampling distribution before students begin repeated simulation.
- The population mean is a parameter; the mean from one sample is a statistic. Ask students to use those terms in their written interpretations.
- Question 3 is the central activity: the sampling distribution is built by repeatedly taking samples and recording one statistic from each sample.
- Question 5 demonstrates that increasing the sample size reduces the spread of sample means and supports the standard-error relationship.
- Question 6 changes the number of repetitions, not the underlying sampling variability. Students should distinguish a smoother histogram from a narrower distribution.
- Use the reproducible seed in `Lab5.R` so students can compare results while still understanding that the simulation represents random sampling.
- After lab: collect the Lab 5 in-lab activity submission.

---
