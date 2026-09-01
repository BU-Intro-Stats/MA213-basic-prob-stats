### Lab 3: Data visualization and interpretation

**Lecture anchor:** Lecture 6

## Lesson Plan (50 minutes)

- 5 min Getting started: load `dplyr`/`ggplot2`/`tidyr`, read both CSVs, `head()` and `str()`
  - triage failed `library()` calls immediately
- 5 min Icebreaker and Question 1: classify variables, name the observational unit for each data set
- 10 min Question 2: average solar radiation by month, then a bar plot from the summary table
- 8 min Question 3: scatter plot of temperature against solar radiation, and what the shape shows
- 8 min Question 4: build the air-quality categories with `mutate()` and `case_when()`, then `count()`
- 10 min Questions 5-6: Titanic survival by class, first as counts, then as proportions
- 4 min Closing: which claim the evidence actually supports

The worksheet is pair work throughout. Collect it at the door.

## Before lab

- Print `Lab3_worksheet.pdf`, one per student
- Post `airquality.csv`, `Titanic.csv`, and `Lab3.R` to Blackboard
- Set up the Blackboard item for the Tutorial 3 hash and confirm that it is open
- Set up the Gradescope item for the Worksheet
- Check Tutorial 3 hash submissions and chase anyone missing; Tutorial 3 is the pre-lab for today
- Assign pairs with `group_divider.py` (reads `Class_list.csv`, writes `group_pairs.pdf`)
- Have `install.packages(c("ggplot2", "tidyr"))` ready to project

## Notes

- Three packages are needed today, not just `dplyr`. Students who have never installed `ggplot2` will lose the first ten minutes to it, so get the install command up on screen before anyone asks.
- Both files are read with `row.names = 1`. Leaving that out adds a stray `X` column of row numbers and breaks nothing visibly, so students may not notice; it is worth calling out once.
- `NA` handling is the real lesson in Question 2. `Ozone` has 37 missing values and `Solar.R` has 7, so `mean(Solar.R)` returns `NA` without `na.rm = TRUE`. Let them hit it before explaining.
- Expected answers:
  - `air` is 153 x 6; `titanic` is 2201 x 4.
  - Q2, mean solar radiation by month: 181.30, 190.17, 216.48, 171.86, 167.43 for months 5-9. July (month 7) is the highest.
  - Q3, 146 complete cases with a weak positive association, r = 0.276. Expect students to overstate this; the plot is a cloud, not a line.
  - Q4, the median ozone is 31.5, so "High" means above that. The largest cells are Gentle breeze (27 High, 34 Low) and Moderate breeze (7 High, 18 Low).
  - Q5, survival counts by class: 1st 203, 2nd 118, 3rd 178, Crew 212. **Crew has the highest count.**
  - Q6, survival proportions within class: 1st 62.5%, 2nd 41.4%, 3rd 25.2%, Crew 24.0%. **1st class has the highest rate.**
- Questions 5 and 6 are the point of the lab. The class with the most survivors (Crew) is not the class most likely to survive (1st), because the groups are different sizes. If a pair gets only one of the two, they have missed the lesson; send them back to compare the two plots.
- After lab: Lab 3 in-lab activity submission. Project 1 launches next session.

---
