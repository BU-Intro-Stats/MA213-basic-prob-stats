## MA 213 course content source

The MA 213 course website is built with MkDocs and hosted on GitHub Pages:

https://bu-intro-stats.github.io/MA213-basic-prob-stats/

This repository is the source for the MA 213 Basic Probability and Statistics
course materials. It contains lecture slides and agendas, lab and project
materials, learning objectives, schedule metadata, and the generated website
pages used by students.

The main instructor workflow is input-first: edit the markdown files in
`instructor_inputs/`, then run one Python script to regenerate the schedule,
calendar, downloadable calendar file, Excel schedule, and MkDocs pages. The
files in `generated_outputs/` and generated copies in `docs/` should not be
edited by hand.

### Document organization

Course planning documents are separated by role:

- `instructor_inputs/` contains instructor-edited markdown files and schedule
  source tables.
- `generated_outputs/` contains files produced by the generators, including the
  weekly schedule, calendar view, calendar download, and generated Excel
  schedule.
- `docs/` contains the MkDocs website source copied from the instructor inputs
  and generated outputs. Do not edit generated copies in `docs/` by hand.

### Content generation flow

```mermaid
flowchart TD
    subgraph Inputs["Instructor inputs"]
        lectureSummary["instructor_inputs/lecture_summary.md<br/>Lecture sequence and objectives"]
        labSummary["instructor_inputs/lab_summary.md<br/>Lab and project sequence"]
        objectives["instructor_inputs/learningObjectives.md<br/>Quiz, lab, and project objective tags"]
        quizSchedule["instructor_inputs/quiz_schedule.md<br/>Quiz placement after lectures"]
        lecturePattern["instructor_inputs/Lecture_schedules.md<br/>Lecture and meeting pattern"]
        labPattern["instructor_inputs/Lab_schedules.md<br/>Lab and deliverable pattern"]
        dates["instructor_inputs/important_dates.md<br/>Holidays, breaks, term dates"]
    end

    scheduleGen["generate_schedule_table.py<br/>Assigns dates and instructor flags"]
    docsSync["Docs sync<br/>inside generate_schedule_table.py"]

    subgraph Outputs["Generated outputs"]
        weekly["generated_outputs/weekly_schedule.md"]
        calendar["generated_outputs/calendar_schedule.md"]
        ics["generated_outputs/course_calendar.ics"]
        excel["generated_outputs/Weekly Schedules.xlsx"]
    end

    docs["docs/<br/>MkDocs source"]
    site["GitHub Pages course site"]

    lectureSummary --> scheduleGen
    labSummary --> scheduleGen
    objectives --> scheduleGen
    quizSchedule --> scheduleGen
    lecturePattern --> scheduleGen
    labPattern --> scheduleGen
    dates --> scheduleGen

    scheduleGen --> weekly
    scheduleGen --> calendar
    scheduleGen --> ics
    scheduleGen --> excel

    lectureSummary --> docsSync
    labSummary --> docsSync
    objectives --> docsSync
    weekly --> docsSync
    calendar --> docsSync
    ics --> docsSync
    docsSync --> docs --> site

    classDef input fill:#fff2cc,stroke:#d6b656,stroke-width:2px,color:#000;
    classDef source fill:#f8cecc,stroke:#b85450,stroke-width:2px,color:#000;
    classDef script fill:#dae8fc,stroke:#6c8ebf,stroke-width:2px,color:#000;
    classDef output fill:#d5e8d4,stroke:#82b366,stroke-width:2px,color:#000;
    classDef website fill:#e1d5e7,stroke:#9673a6,stroke-width:3px,color:#000;

    class lectureSummary,labSummary,objectives,quizSchedule,lecturePattern,labPattern,dates input;
    class scheduleGen,docsSync script;
    class weekly,calendar,ics,excel,docs output;
    class site website;
```

### Instructor Workflow

For routine schedule and website updates, instructors should edit only files in
`instructor_inputs/`.

Common semester setup files:

1. `instructor_inputs/important_dates.md` - semester year, first/last class day, holidays,
   recesses, final exam dates, and other academic calendar dates.
2. `instructor_inputs/Lecture_schedules.md` - lecture, discussion, office-hour, and homework
   meeting days/times.
3. `instructor_inputs/Lab_schedules.md` - lab/project meeting day and lab
   deliverable due day/time.
4. `instructor_inputs/quiz_schedule.md` - quiz placement after a lecture, once
   the instructor decides where each quiz belongs.

Content files:

1. `instructor_inputs/lecture_summary.md` - lecture order, topics, readings,
   and lecture learning objectives.
2. `instructor_inputs/lab_summary.md` - lab/project order, purposes,
   activities, deliverables, and lab/project learning objectives.
3. `instructor_inputs/learningObjectives.md` - course learning objectives and
   quiz/lab/project prerequisite tags.

After editing instructor inputs, run this from the repository root:

```bash
python generate_schedule_table.py
```

This regenerates the weekly schedule, calendar view, `.ics` calendar file, Excel
schedule, and the MkDocs copies in `docs/`. It also keeps copied meeting-pattern
blocks in `lecture_summary.md` and `lab_summary.md` synchronized with
`Lecture_schedules.md` and `Lab_schedules.md`.

By default this is an instructor build: the local weekly schedule includes the
`Instructor Flags` column, and calendar metadata may include instructor-facing
flags. This is useful for checking prerequisite and holiday warnings before
publishing.

To preview the website locally, run:

```bash
python -m mkdocs serve
```

Then open:

```text
http://127.0.0.1:8000/
```


### In short, you can just run the following commands in the shell
```bash
python generate_schedule_table.py
python -m mkdocs serve
```

To preview the public/student version locally, hide instructor flags when
generating:

```bash
MA213_PUBLIC_SITE=1 python generate_schedule_table.py
python -m mkdocs serve
```

Run `python generate_schedule_table.py` again afterward to restore the local
instructor-facing schedule.

**Warning**

You do not need to run `python build_docs.py` for routine work.
`generate_schedule_table.py` already refreshes `docs/`.

Do not edit
`generated_outputs/weekly_schedule.md`,
`generated_outputs/calendar_schedule.md`,
`generated_outputs/course_calendar.ics`, or the generated copies in `docs/` by
hand; they will be overwritten the next time the generator runs.

### Markdown Consistency

The instructor markdown files use a consistent, predictable pattern so they are
easy to edit and easy for the generators to parse.

General rules:

- Keep schedule metadata in markdown tables.
- Use no-space AM/PM times in instructor inputs, such as `11:15AM` and
  `10:00PM`. Generated website pages may display times with a space.
- Keep lecture, lab, and project entries as `###` sections.
- Use bold field labels in list items, such as `- **Topic:**` or
  `- **Purpose:**`.
- Use nested bullets for multi-item fields such as learning objectives,
  activities, and deliverables.

Lecture sections in `instructor_inputs/lecture_summary.md` use this pattern:

```md
### Lecture 12
- **Topic:** Geometric distribution (Chapter 4.2)
- **Reading:** Chapter 4.3
- **Learning Objectives:**
  - M2, LO1: Validate and Explain Probability Distributions
  - M2, LO4: Understand and Compute Expectations and Variances
```

Lab/project sections in `instructor_inputs/lab_summary.md` use this pattern:

```md
### Lab 5: Simulating LLN/CLT with Different Distributions

- **Lecture Anchor:** Lecture 14
- **Purpose:** Help students connect probability ideas to distributions.
- **Primary Objectives:**
  - Understand and Compute Expectations and Variances
- **Pre-Lab Activity:**
  - Review expected value and variance.
- **In-Lab Activity:**
  - Compare probability models in R.
- **Post-Lab Activity:**
  - Submit a short interpretation.
- **Deliverables:**
  - Tutorial 5
```

Schedule tables in `Lecture_schedules.md`, `Lab_schedules.md`,
`quiz_schedule.md`, and `important_dates.md` should keep their column names and
table structure. Edit cell values, but avoid renaming columns.

### Instructor Input Files

- `instructor_inputs/lecture_summary.md` is the master lecture sequence by
  module.
- `instructor_inputs/learningObjectives.md` is the master list of course
  learning objectives.
- `instructor_inputs/lab_summary.md` is the master lab/project sequence by
  module.
- `instructor_inputs/quiz_schedule.md` is the instructor-editable quiz placement
  table. Set
  `After Lecture` to the lecture that should immediately precede each quiz.
- `instructor_inputs/Lecture_schedules.md`,
  `instructor_inputs/Lab_schedules.md`, and
  `instructor_inputs/important_dates.md` provide the schedule metadata used to
  build weekly and calendar views.

The website copies in `docs/` are derived from these files. Edit
`instructor_inputs/` first, not the generated copies in `docs/`.

### Website generation files

The website is generated from these planning and build files:

- `instructor_inputs/Lecture_schedules.md` contains the editable
  lecture/discussion/office-hour/homework meeting pattern table.
- `instructor_inputs/Lab_schedules.md` contains the editable lab/project meeting
  pattern table.
- `instructor_inputs/important_dates.md` contains the editable semester date
  table used for holidays, recesses, final exams, and other academic calendar
  dates.
- `instructor_inputs/lecture_summary.md`,
  `instructor_inputs/learningObjectives.md`, and
  `instructor_inputs/lab_summary.md` are the ground-truth planning files used by
  the website.
- `instructor_inputs/quiz_schedule.md` controls where quiz events are inserted
  into the lecture sequence. The schedule generator assigns each quiz to the
  next regular class meeting after its `After Lecture` value.
- `generate_schedule_table.py` reads the course schedule information and creates
  `generated_outputs/weekly_schedule.md`,
  `generated_outputs/calendar_schedule.md`,
  `generated_outputs/course_calendar.ics`, and
  `generated_outputs/Weekly Schedules.xlsx`. It also syncs the MkDocs source
  pages in `docs/`, updates copied meeting-pattern blocks in
  `lecture_summary.md` and `lab_summary.md`, and adds instructor flags when
  scheduled lectures, quizzes, labs, or projects fall on or near holidays,
  recesses, or other important dates, or when prerequisites have not yet been
  covered.
- `build_docs.py` is a small compatibility helper that only copies source pages
  into `docs/`; routine schedule updates do not need it because
  `generate_schedule_table.py` syncs `docs/` automatically.
- `mkdocs.yml` defines the site navigation, theme, and build settings.

Edit the root source files first, not the generated copies in `docs/`.
`generate_schedule_table.py` refreshes the MkDocs pages from the root
ground-truth files.

### Reusable Course Package

The schedule generator now lives in the local Python package
`course_site_builder`. The root `generate_schedule_table.py` file is a thin
MA 213 wrapper:

```py
from pathlib import Path

from course_site_builder.schedule import CourseSiteConfig, main


if __name__ == "__main__":
    main(CourseSiteConfig.for_repo(Path(__file__).resolve().parent, "MA213", course_title="MA 213"))
```

This keeps the instructor workflow unchanged:

```bash
python generate_schedule_table.py
```

For another course repository, such as MA 214, copy the package directory or
install this package, keep the same `instructor_inputs/`, `generated_outputs/`,
and `docs/` folder pattern, and create a course-specific wrapper. An example is
provided in `examples/MA214_generate_schedule_table.py`:

```py
from pathlib import Path

from course_site_builder.schedule import CourseSiteConfig, main


if __name__ == "__main__":
    main(CourseSiteConfig.for_repo(Path(__file__).resolve().parent, "MA214", course_title="MA 214"))
```

The public-site environment variable is based on the course code. For MA 214,
the public build command would be:

```bash
MA214_PUBLIC_SITE=1 python generate_schedule_table.py
```

The reusable package currently assumes the same markdown filenames and syntax
used by MA 213: `lecture_summary.md`, `lab_summary.md`,
`learningObjectives.md`, `quiz_schedule.md`, `Lecture_schedules.md`,
`Lab_schedules.md`, and `important_dates.md`.

When setting up another course, put the wrapper at the root of that course
repository as `generate_schedule_table.py`. The example file lives in
`examples/` only as a template; it is not part of the routine MA 213 build.

### Editable schedule inputs

Lecture, discussion, office-hour, and homework meeting days are controlled by
the table in `instructor_inputs/Lecture_schedules.md`.
`generate_schedule_table.py` syncs this block to the top of
`instructor_inputs/lecture_summary.md`:

```md
| Event Type    | Weekdays                 | Start Time | End Time |
| ---           | ---                      | ---        | ---      |
| Lecture       | Monday, Wednesday, Friday | 11:15AM  | 12:05PM |
| Discussion    | Thursday                 | 12:20PM   | 1:10PM  |
| Office Hour 1 | Friday                   | 3:00PM    | 4:00PM  |
| Office Hour 2 | Monday                   | 4:00PM    | 5:00PM  |
| Homework      | Sunday                   | 2:55PM    | 3:00PM  |
```

Lecture topics, readings, and lecture learning objectives are maintained in
`instructor_inputs/lecture_summary.md`. Keep objective codes in the form
`M1, LO1` so prerequisite checks can match them against
`instructor_inputs/learningObjectives.md`.

Lab and project meeting days are controlled by the table in
`instructor_inputs/Lab_schedules.md`. `generate_schedule_table.py` syncs this
block to the top of `instructor_inputs/lab_summary.md`:

```md
| Event Type      | Weekday   | Start Time | End Time |
| ---             | ---       | ---        | ---      |
| Lab / Project   | Wednesday |            |          |
| Lab Deliverable | Tuesday   |            | 10:00PM |
```

Lab and project details are maintained directly in
`instructor_inputs/lab_summary.md`. Use the normalized lab/project section
syntax shown in `Markdown Consistency`.

Quiz placement is controlled by `instructor_inputs/quiz_schedule.md`:

```md
| Quiz | After Lecture | Status | Notes |
| --- | --- | --- | --- |
| Quiz 1 | Lecture 8 | tentative |  |
```

This means Quiz 1 is scheduled at the next regular class meeting after Lecture
8. Update only the `After Lecture`, `Status`, and `Notes` cells when quiz timing
changes.

Prerequisite flags are inferred from `instructor_inputs/learningObjectives.md`.
Quiz flags use the `Q#` tags, lab flags use the `Lab#` tags, and project flags use
the `P1` or `P2` tags. If an event has no matching learning-objective tag, the
weekly schedule and calendar show a missing-prerequisite-metadata flag so the
instructor knows where an explicit prerequisite entry is needed.

### Instructor Flags

`Instructor Flags` are generated by `generate_schedule_table.py` as an
instructor-only audit column. They are meant to help instructors spot schedule
problems before publishing the student-facing site.

The script creates flags from three sources:

1. Academic calendar timing:
   - Dates come from `instructor_inputs/important_dates.md`.
   - Events are flagged if they fall on a holiday, class suspension, recess,
     study period, last day of classes, or substitute schedule day.
   - Events are also flagged when they are within two days of one of those
     important dates, using wording such as `Near Thanksgiving Recess`.
2. Learning-objective prerequisites:
   - Lecture objective codes come from `instructor_inputs/lecture_summary.md`,
     using codes such as `M2, LO4`.
   - Quiz/lab/project prerequisite tags come from
     `instructor_inputs/learningObjectives.md`, using tags such as `Q2`, `Lab5`,
     `P1`, or `P2`.
   - The script checks whether each prerequisite objective has already appeared
     in a prior lecture.
3. Missing or mismatched metadata:
   - If an event has no matching prerequisite tag, the script writes a missing
     prerequisite metadata flag.
   - If a prerequisite tag points to an objective that never appears in the
     lecture summary, the script writes a prerequisite-not-found flag.

Common prerequisite flag meanings:

- `Prerequisite not yet covered`: the objective first appears after the quiz,
  lab, or project event.
- `Prerequisite may be same-day`: the objective first appears on the same date
  as the quiz, lab, or project event.
- `Missing prerequisite metadata`: the quiz, lab, or project does not have a
  matching `Q#`, `Lab#`, `P1`, or `P2` tag in `learningObjectives.md`.

The local instructor build shows these flags:

```bash
python generate_schedule_table.py
python -m mkdocs serve
```

The public/student build hides these flags:

```bash
MA213_PUBLIC_SITE=1 python generate_schedule_table.py
python -m mkdocs serve
```

GitHub Actions uses the public build setting automatically, so the deployed
course website does not show the `Instructor Flags` column.

Academic dates are controlled by the table in
`instructor_inputs/important_dates.md`:

```md
| Start Date   | End Date    | Event                                             |
| ---          | ---         | ---                                               |
| September 2  |             | Classes Begin; First Seven-Week Session Begins    |
| November 25  | November 29 | Thanksgiving Recess                               |
```

Blank `End Date` values are treated as one-day events. Dates in January are
treated as belonging to the following calendar year for a fall semester.

### Local workflow

After editing files in `instructor_inputs/`, run:

```bash
python generate_schedule_table.py
```

This is the only required generator for routine instructor work. It updates
`generated_outputs/` and `docs/`. This local instructor build includes the
`Instructor Flags` column.

To generate the public/student version locally without instructor flags:

```bash
MA213_PUBLIC_SITE=1 python generate_schedule_table.py
```

To preview the website locally:

```bash
python -m mkdocs serve
```

Then open:

```text
http://127.0.0.1:8000/
```

To create a static local site build without serving it:

```bash
python -m mkdocs build --strict
```

That writes the built website to `site/`. For the public course website, commit
the updated source/generated files and push to `master`; GitHub Actions builds
and deploys the site.

The monthly calendar page is available at:

```text
http://127.0.0.1:8000/calendar_schedule/
```

The calendar page includes a download menu for Google Calendar, Apple Calendar,
and Outlook. These choices all use the generated
`generated_outputs/course_calendar.ics` file.

If port `8000` is already in use, run the server on another port:

```bash
python -m mkdocs serve -a 127.0.0.1:8001
```

Stop the local server with `Ctrl+C` in the terminal where it is running.

The generated MkDocs pages live in `docs/`:

- `docs/weekly_schedule.md`
- `docs/calendar_schedule.md`
- `docs/course_calendar.ics`
- `docs/lecture_summary.md`
- `docs/lab_summary.md`
- `docs/learning_objectives.md`
- `docs/index.md`

The GitHub Actions workflow in `.github/workflows/deploy-site.yml` installs the
Python dependencies, regenerates schedule/site output with
`MA213_PUBLIC_SITE=1`, runs `mkdocs build --strict`, uploads the built `site/`
artifact, and deploys it with `actions/deploy-pages`. The deployed public site
does not show the `Instructor Flags` column. Run the full local workflow above
before committing when you have changed `instructor_inputs/lecture_summary.md`,
`instructor_inputs/learningObjectives.md`,
`instructor_inputs/lab_summary.md`, `instructor_inputs/quiz_schedule.md`, or
schedule source files in `instructor_inputs/`, especially `important_dates.md`,
`Lecture_schedules.md`, or `Lab_schedules.md`.

This site is deployed with GitHub Actions, not GitHub Pages Jekyll. In the
repository settings, GitHub Pages should be configured with:

- Source: GitHub Actions

Deployment runs automatically when changes are pushed to `master`. It can also
be run manually from the Actions tab using the `Deploy course site` workflow.
If deployment fails with `Creating Pages deployment failed` or `HttpError: Not
Found`, check that GitHub Pages is enabled and that the source is set to
`GitHub Actions` under Settings -> Pages.

## Slide license notes

These slides are available at http://www.openintro.org under a Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported license (CC BY-NC-SA):

http://creativecommons.org/licenses/by-nc-sa/3.0/

This file describes guidelines for when the slides' source files are modified and/or shared. The CC BY-SA license guidelines supercede any guidelines put forth in this file; follow the CC BY-SA license if there is any discrepancy between that license and these guidelines.

1. Communication obligation. Any derivative work must communicate that it is licensed under a CC BY-SA license, and it also must in some way include the attribution content contained in the footnote on the first page of the original document.

2. Derivative title. No derivative may include "OpenIntro" in the title, unless it is included in text of the form "Derivative of OpenIntro", e.g. one might add a subtitle such as "Derivative of OpenIntro Slides".

3. For derivative works, we suggest but do not require that contributing authors' names be listed in chronological order of their contribution.
