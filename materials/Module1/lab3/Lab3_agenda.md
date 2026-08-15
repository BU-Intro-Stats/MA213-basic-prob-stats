### Lab 3: Data visualization and interpretation

**Lecture anchor:** Lecture 6  
**Purpose:** Move from raw data to visual evidence, and practice saying what a plot does and does not support.

**Primary objectives supported**
- Classify and Analyze Variables
- Visualize and Describe Data Distributions
- Use R for Data Management and Exploration

**Pre-lab activity**
- Review the lecture notes on categorical and numerical variables
- Complete Tutorial 3 and submit your hash code to Blackboard

**In-lab activity**
- Decide whether variables are categorical or numerical
- Use `filter()`, `mutate()`, `group_by()`, and `summarize()`
- Create appropriate plots for the variable type
- Compare counts against proportions
- Write interpretations in context

**Post-lab activity**
- Lab3 in-lab activity submission

**Deliverables**
- Lab2 in-lab activity submission
- Tutorial 3 (pre-lab for lab3)

**Notes**
- You need `dplyr`, `ggplot2`, and `tidyr`. If a `library()` call fails, run `install.packages(c("ggplot2", "tidyr"))` once, then try again.
- Keep `airquality.csv`, `Titanic.csv`, and `Lab3.R` in the same folder.
- The air quality data has missing values. `mean()` returns `NA` unless you use `na.rm = TRUE`.
- `Lab3.R` is a starter, not a solution. Fill in the `________` blanks and uncomment each block as you go.

---
