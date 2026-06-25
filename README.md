## Course website

The MA 213 course website is built with MkDocs and hosted on GitHub Pages:

https://bu-intro-stats.github.io/MA213-basic-prob-stats/

The website is generated from source planning files in this repository. Edit the
source files first, then regenerate the derived summaries and site pages.

- `Lecture_schedules.md` contains the editable
  lecture/discussion/office-hour/homework meeting pattern table that is copied
  to the top of `lecture_summary.md`.
- `Week*/Lecture*/Lecture*_agenda.tex` files contain the source lecture topics,
  readings, and lecture learning-objective codes.
- `Lab_schedules.md` contains the editable lab/project meeting pattern table
  that is copied to the top of `lab_summary.md`.
- Non-lecture `Week*` directories contain lab/project `*_plan.md` files. These
  files are the source for lab/project topics, objectives, activities, and
  deliverables.
- `important_dates.md` contains the editable semester date table used for
  holidays, recesses, final exams, and other academic calendar dates.
- `lecture_summary.md` and `lab_summary.md` are generated summaries. Do not edit
  them directly unless you intentionally want to overwrite generator output.
- `generate_lecture_summary.py` reads `Week*/Lecture*/Lecture*_agenda.tex`,
  cross-references `learningObjectives.md`, and rewrites `lecture_summary.md`.
- `generate_lab_summary.py` reads `*_plan.md` files from non-lecture
  directories inside each `Week*` folder and rewrites `lab_summary.md`.
- `generate_schedule_table.py` reads the course schedule information and creates
  `weekly_schedule.md`, `calendar_schedule.md`, `course_calendar.ics`, and the
  generated Excel schedule.
- `build_docs.py` copies/regenerates the source pages into the `docs/` directory
  used by MkDocs.
- `mkdocs.yml` defines the site navigation, theme, and build settings.

Edit the root source files first, not the generated copies in `docs/`. The
generated summaries are refreshed by `generate_lecture_summary.py` and
`generate_lab_summary.py`; generated MkDocs pages are refreshed by
`build_docs.py`.

### Editable schedule inputs

Lecture, discussion, office-hour, and homework meeting days are controlled by
the table in `Lecture_schedules.md`. `generate_lecture_summary.py` copies this
block to the top of `lecture_summary.md`:

```md
| Event Type | Weekdays | Start Time | End Time |
| --- | --- | --- | --- |
| Lecture | Monday, Wednesday, Friday | 11:15 AM | 12:05 PM |
| Discussion | Thursday | 12:20 PM | 1:10 PM |
| Office Hour 1 | Friday | 3:00 PM | 4:00 PM |
| Office Hour 2 | Monday | 4:00 PM | 5:00 PM |
| Homework | Sunday | 2:55 PM | 3:00 PM |
```

Lecture topics, readings, and lecture learning objectives are generated from
the corresponding `Lecture*_agenda.tex` files. Learning objective labels are
matched against `learningObjectives.md`, so entries such as `M1, LO1` are
expanded with their objective title, assessment tag, and core/auxiliary status.

Lab and project meeting days are controlled by the table in `Lab_schedules.md`.
`generate_lab_summary.py` copies this block to the top of `lab_summary.md`:

```md
| Event Type | Weekday | Start Time | End Time |
| --- | --- | --- | --- |
| Lab / Project | Wednesday |  |  |
| Lab Deliverable | Tuesday |  | 10:00 PM |
```

Lab and project details are generated from `*_plan.md` files in each non-lecture
directory under `Week*`. A directory is treated as a lab/project directory when
its name does not start with `Lecture`. For example, `Week6/Lab5/Lab5_plan.md`
supplies the Lab 5 section, while `Week8/P2W1/P2_1_plan.md` supplies the P2-1
section.

Academic dates are controlled by the table in `important_dates.md`:

```md
| Start Date | End Date | Event |
| --- | --- | --- |
| September 2 |  | Classes Begin; First Seven-Week Session Begins |
| November 25 | November 29 | Thanksgiving Recess |
```

Blank `End Date` values are treated as one-day events. Dates in January are
treated as belonging to the following calendar year for a fall semester.

### Local workflow

To regenerate the schedule and sync the MkDocs source pages locally, run:

```bash
python generate_lecture_summary.py
python generate_lab_summary.py
python generate_schedule_table.py
python build_docs.py
```

To preview the website locally, run:

```bash
python -m mkdocs serve
```

Then open:

```text
http://127.0.0.1:8000/
```

The monthly calendar page is available at:

```text
http://127.0.0.1:8000/calendar_schedule/
```

The calendar page includes a download menu for Google Calendar, Apple Calendar,
and Outlook. These choices all use the generated `course_calendar.ics` file.

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
Python dependencies, regenerates schedule/site output, runs `mkdocs build
--strict`, uploads the built `site/` artifact, and deploys it with
`actions/deploy-pages`. Run the full local workflow above before committing when
you have changed lecture agenda files, lab/project plan files, or schedule
source files.

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
