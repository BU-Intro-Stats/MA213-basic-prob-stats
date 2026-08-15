### Lab 1: Lab orientation, R setup, and Tutorial 0

**Lecture anchor:** Lecture 1

## Lesson Plan (50 minutes)

- 20 min Slides (`Lab1.pdf`): what lab is, weekly rhythm, grading, projects and ESRN, peer evaluation, ground rules, R vs RStudio
- 10 min Setup: R and RStudio install check
  - students run `R.version.string`; show of hands, then triage the failures
- 15 min Tutorial 0: complete in lab, submit hash code to Blackboard
- 5 min Worksheet (`Lab1_worksheet.pdf`): pair work throughout, hand in at the door
  - partner introductions, install check, `read.csv()` + column sum, hash confirmation

The last three run concurrently, not in sequence. Students who already have R start Tutorial 0 right away, which frees you for the install problems.

## Before lab

- Print `Lab1_worksheet.pdf`, one per student; build `Lab1.pdf`
- Post `sleep.csv` and `Lab1.R` to Blackboard
- Confirm the Blackboard item for the Tutorial 0 hash is open
- Links ready to project: <https://cloud.r-project.org>, <https://posit.co/download/rstudio-desktop/>

## Notes

- Install R first, then RStudio. RStudio is only a front end and will not run without R.
- Common fixes: macOS "app is damaged" -> right-click Open; Apple Silicon -> arm64 installer; Windows admin block -> install to user directory. If it cannot be fixed in the time available, move the student to a lab machine and follow up by email.
- The hash goes to **Blackboard**, not on the worksheet. This is the most common Week 1 mistake, and the habit set today carries through Labs 2 and 3.
- Worksheet answers: `sum(sleep_data$extra)` is 30.8; `dim()` is 20x4 because `read.csv()` reads the unnamed index column as `X`.
- `read.csv()` "cannot open file" means the working directory is wrong -> Session > Set Working Directory > To Source File Location.
- After lab: check Blackboard hash submissions against the roster, email anyone missing. Tutorial 1 is due **before** Lab 2.

---
