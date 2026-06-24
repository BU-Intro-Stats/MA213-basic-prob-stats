## Course website

The MA 213 course website is built with MkDocs and hosted on GitHub Pages:

https://bu-intro-stats.github.io/MA213-basic-prob-stats/

The website is generated from the course planning files in this repository:

- `lecture_summary.md` contains the lecture schedule, lecture-level notes, and
  the editable lecture/discussion/office-hour meeting pattern table.
- `lab_summary.md` contains the lab schedule, lab topics, deliverables, and the
  editable lab/project meeting pattern table.
- `important_dates.md` contains the editable semester date table used for
  holidays, recesses, final exams, and other academic calendar dates.
- `generate_schedule_table.py` reads the course schedule information and creates
  `weekly_schedule.md`, `calendar_schedule.md`, `course_calendar.ics`, and the
  generated Excel schedule.
- `build_docs.py` copies/regenerates the source pages into the `docs/` directory
  used by MkDocs.
- `mkdocs.yml` defines the site navigation, theme, and build settings.

Edit the root source files first, not the generated copies in `docs/`. The
generated MkDocs pages are refreshed by `build_docs.py`.

### Editable schedule inputs

Lecture, discussion, and office-hour meeting days are controlled by this table
near the top of `lecture_summary.md`:

```md
| Event Type | Weekdays | Start Time | End Time |
| --- | --- | --- | --- |
| Lecture | Monday, Wednesday, Friday | 11:15 AM | 12:05 PM |
| Discussion | Thursday | 12:20 PM | 1:10 PM |
| Office Hour 1 | Friday | 3:00 PM | 4:00 PM |
| Office Hour 2 | Friday | 4:00 PM | 5:00 PM |
| Homework | Sunday | 2:55 PM | 3:00 PM |
```

Lab and project meeting days are controlled by this table near the top of
`lab_summary.md`:

```md
| Event Type | Weekday | Start Time | End Time |
| --- | --- | --- | --- |
| Lab / Project | Wednesday | 9:05 AM | 1:10 PM |
| Lab Deliverable | Tuesday |  | 10:00 PM |
```

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

The calendar page includes download buttons for Google Calendar, Apple Calendar,
and Outlook. These buttons all use the generated `course_calendar.ics` file.

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
Python dependencies, regenerates the documentation pages, runs
`mkdocs build --strict`, uploads the built `site/` artifact, and deploys it with
`actions/deploy-pages`.

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
