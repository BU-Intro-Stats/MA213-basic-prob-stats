"""Generate the course syllabus from the instructor's Markdown files.

The generator reads the syllabus sections from ``materials/instructor_inputs/``,
converts the supported Markdown into LaTeX, and writes one complete document to
``materials/generated_outputs/syllabus.tex``.  Edit the Markdown inputs rather
than the generated TeX file.
"""

from __future__ import annotations

import re
from datetime import date, datetime, timedelta
from pathlib import Path


ROOT = Path(__file__).resolve().parent
INPUTS = ROOT / "instructor_inputs"
OUTPUT = ROOT / "generated_outputs" / "syllabus.tex"
WEEKLY_SCHEDULE = ROOT / "generated_outputs" / "weekly_schedule.md"
LECTURE_SUMMARY = INPUTS / "lecture_summary.md"
HOMEWORK_SCHEDULE = INPUTS / "Homework_schedule.md"

SYLLABUS_FILES = (
    INPUTS / "syllabus_course.md",
    INPUTS / "syllabus_staff.md",
    INPUTS / "syllabus_grading.md",
    INPUTS / "syllabus_assessments.md",
    INPUTS / "syllabus_lab.md",
    INPUTS / "syllabus_policies.md",
)

LATEX_SPECIALS = {
    "\\": r"\textbackslash{}",
    "&": r"\&",
    "%": r"\%",
    "$": r"\$",
    "#": r"\#",
    "_": r"\_",
    "{": r"\{",
    "}": r"\}",
    "~": r"\textasciitilde{}",
    "^": r"\textasciicircum{}",
}

SOURCE_LABELS = {
    "Homework_schedule.md": "the course schedule",
    "learningObjectives.md": "the course learning objectives",
    "quiz_schedule.md": "the course schedule",
    "lab_summary.md": "the lab summaries",
    "Lab_schedules.md": "meeting schedule",
    "syllabus_grading.md": "the grading rules above",
}


def latex_escape(text: str) -> str:
    return "".join(LATEX_SPECIALS.get(char, char) for char in text)


def inline_latex(text: str) -> str:
    """Convert the small set of inline Markdown used by the syllabus inputs."""
    text = text.replace("&nbsp;", " ")
    pattern = re.compile(r"(\*\*.+?\*\*|`[^`]+`|https?://[^\s|]+)")
    parts = []
    for token in pattern.split(text):
        if not token:
            continue
        if token.startswith("**") and token.endswith("**"):
            parts.append(r"\textbf{" + latex_escape(token[2:-2]) + "}")
        elif token.startswith("`") and token.endswith("`"):
            source = token[1:-1]
            label = SOURCE_LABELS.get(source)
            parts.append(latex_escape(label) if label else r"\texttt{" + latex_escape(source) + "}")
        elif token.startswith(("http://", "https://")):
            url = token.rstrip(".,;:")
            punctuation = token[len(url) :]
            parts.append(r"\url{" + url + "}" + latex_escape(punctuation))
        else:
            parts.append(latex_escape(token))
    return "".join(parts)


def strip_comments(text: str) -> str:
    return re.sub(r"<!--.*?-->", "", text, flags=re.DOTALL)


def split_table_row(line: str) -> list[str]:
    cells = re.split(r"(?<!\\)\|", line.strip().strip("|"))
    return [cell.replace(r"\|", "|").strip() for cell in cells]


def is_separator_row(cells: list[str]) -> bool:
    return bool(cells) and all(re.fullmatch(r":?-{3,}:?", cell) for cell in cells)


def table_latex(rows: list[list[str]]) -> list[str]:
    if len(rows) < 2 or not is_separator_row(rows[1]):
        return [inline_latex(" | ".join(row)) + r"\\" for row in rows]

    column_count = len(rows[0])
    alignment = "l" + "Y" * (column_count - 1)
    size = r"\scriptsize" if column_count >= 5 else r"\small"
    output = [
        r"\begin{center}",
        size,
        r"\setlength{\tabcolsep}{4pt}",
        rf"\begin{{tabularx}}{{\textwidth}}{{{alignment}}}",
        r"\toprule",
        " & ".join(r"\textbf{" + inline_latex(cell) + "}" for cell in rows[0]) + r" \\",
        r"\midrule",
    ]
    for row in rows[2:]:
        padded = row + [""] * (column_count - len(row))
        output.append(" & ".join(inline_latex(cell) for cell in padded[:column_count]) + r" \\")
    output.extend((r"\bottomrule", r"\end{tabularx}", r"\end{center}"))
    return output


def markdown_to_latex(
    path: Path,
    exclude_sections: set[str] | None = None,
    section_replacements: dict[str, str] | None = None,
) -> str:
    """Convert the small Markdown subset used by the syllabus into LaTeX.

    Headings become unnumbered sections, lists become LaTeX lists, and tables
    become ``tabularx`` environments.  ``exclude_sections`` omits a heading and
    everything beneath it; ``section_replacements`` inserts generated LaTeX in
    place of a Markdown section.
    """
    lines = strip_comments(path.read_text(encoding="utf-8")).splitlines()
    excluded = {heading.lower() for heading in (exclude_sections or set())}
    replacements = {
        heading.lower(): replacement
        for heading, replacement in (section_replacements or {}).items()
    }
    output: list[str] = []
    paragraph: list[str] = []
    list_kind: str | None = None
    skip_level: int | None = None

    def flush_paragraph() -> None:
        if paragraph:
            output.append(inline_latex(" ".join(part.strip() for part in paragraph)))
            output.append("")
            paragraph.clear()

    def close_list() -> None:
        nonlocal list_kind
        if list_kind:
            output.append(rf"\end{{{list_kind}}}")
            output.append(r"\end{samepage}")
            output.append("")
            list_kind = None

    index = 0
    while index < len(lines):
        raw = lines[index].rstrip()
        stripped = raw.strip()
        heading = re.match(r"^(#{1,4})\s+(.+)$", stripped)

        if skip_level is not None:
            if heading and len(heading.group(1)) <= skip_level:
                skip_level = None
            else:
                index += 1
                continue

        if heading and heading.group(2).strip().lower() in excluded:
            flush_paragraph()
            close_list()
            skip_level = len(heading.group(1))
            index += 1
            continue
        if heading and heading.group(2).strip().lower() in replacements:
            flush_paragraph()
            close_list()
            output.append(replacements[heading.group(2).strip().lower()])
            output.append("")
            skip_level = len(heading.group(1))
            index += 1
            continue

        if stripped.startswith("|"):
            flush_paragraph()
            close_list()
            rows = []
            while index < len(lines) and lines[index].strip().startswith("|"):
                rows.append(split_table_row(lines[index]))
                index += 1
            output.extend(table_latex(rows))
            output.append("")
            continue

        if heading:
            flush_paragraph()
            close_list()
            level = len(heading.group(1))
            if level > 1:
                if level == 2:
                    output.append(rf"\section*{{{inline_latex(heading.group(2))}}}")
                else:
                    output.append(rf"\textbf{{{inline_latex(heading.group(2))}:}}")
                output.append("")
            index += 1
            continue

        item = re.match(r"^\s*(?:[-*]|\d+\.)\s+(.+)$", raw)
        if item:
            flush_paragraph()
            wanted_kind = "enumerate" if re.match(r"^\s*\d+\.", raw) else "itemize"
            if list_kind != wanted_kind:
                close_list()
                list_kind = wanted_kind
                output.append(r"\begin{samepage}")
                output.append(rf"\begin{{{list_kind}}}")
            label = ""
            if wanted_kind == "enumerate":
                number = re.match(r"^\s*(\d+)\.", raw).group(1)
                label = rf"[{number}.]"
            output.append(r"\item" + label + " " + inline_latex(item.group(1)))
            index += 1
            continue

        if not stripped or stripped == "---":
            flush_paragraph()
            close_list()
            index += 1
            continue

        paragraph.append(stripped)
        index += 1

    flush_paragraph()
    close_list()
    return "\n".join(output).strip()


def semester_label(path: Path) -> str:
    match = re.search(r"^Semester:\s*(.+)$", path.read_text(encoding="utf-8"), re.MULTILINE)
    if not match:
        raise ValueError(f"Add a 'Semester:' line to {path}.")
    return match.group(1).strip()


def course_name(path: Path) -> str:
    text = path.read_text(encoding="utf-8")
    match = re.search(r"^\|\s*Course Name\s*\|\s*(.+?)\s*\|$", text, re.MULTILINE)
    return match.group(1).strip() if match else "Basic Statistics and Probability"


def objective_counts(path: Path) -> tuple[int, int]:
    lines = strip_comments(path.read_text(encoding="utf-8")).splitlines()
    objective_lines = [line for line in lines if re.match(r"^\s*\d+\.\s+", line)]
    core = sum(1 for line in objective_lines if re.search(r"\*\*Core\*\*\s*$", line))
    auxiliary = sum(1 for line in objective_lines if re.search(r"\bAuxiliary\s*$", line))
    return core, auxiliary


def read_markdown_table(path: Path, heading: str) -> list[dict[str, str]]:
    lines = path.read_text(encoding="utf-8").splitlines()
    heading_index = next(
        (
            index
            for index, line in enumerate(lines)
            if re.fullmatch(rf"#{{1,4}}\s+{re.escape(heading)}", line.strip(), re.IGNORECASE)
        ),
        None,
    )
    if heading_index is None:
        raise ValueError(f"Could not find heading '{heading}' in {path}.")

    table_lines = []
    for line in lines[heading_index + 1 :]:
        if line.strip().startswith("|"):
            table_lines.append(line)
        elif table_lines:
            break
    rows = [split_table_row(line) for line in table_lines]
    if len(rows) < 3:
        raise ValueError(f"Could not find a Markdown table under '{heading}' in {path}.")

    headers = rows[0]
    return [
        dict(zip(headers, row + [""] * (len(headers) - len(row))))
        for row in rows[2:]
    ]


def course_details() -> dict[str, str]:
    rows = read_markdown_table(INPUTS / "syllabus_course.md", "Course Details")
    return {row["Field"]: row["Value"] for row in rows}


def meeting_summary() -> str:
    rows = read_markdown_table(INPUTS / "Lecture_schedules.md", "Course Meeting Pattern")
    lecture = next((row for row in rows if row.get("Event Type") == "Lecture"), None)
    if not lecture:
        return "See the current course schedule."
    weekdays = lecture.get("Weekdays", "")
    start = lecture.get("Start Time", "")
    end = lecture.get("End Time", "")
    time = f"{start}--{end}" if start and end else start or end
    return ", ".join(part for part in (weekdays, time) if part)


def staff_latex(path: Path) -> str:
    """Build the Course Staff section from the staff Markdown table."""
    rows = read_markdown_table(path, "Staff")
    grouped: dict[str, list[dict[str, str]]] = {}
    for row in rows:
        grouped.setdefault(row["Role"], []).append(row)

    output = [r"\section*{Course Staff}"]
    for role, people in grouped.items():
        label = role.upper() + ("S" if len(people) > 1 and not role.endswith("s") else "")
        output.append(rf"\textbf{{{inline_latex(label)}:}}")
        output.append(r"\begin{itemize}")
        for person in people:
            name = inline_latex(person.get("Name", ""))
            email = person.get("Email", "")
            contact = rf"\href{{mailto:{email}}}{{{latex_escape(email)}}}" if email else ""
            details = ", ".join(
                part
                for part in (
                    f"Office: {person.get('Office', '')}" if person.get("Office") else "",
                    f"Office hours: {person.get('Office Hours', '')}"
                    if person.get("Office Hours")
                    else "",
                    person.get("Link or Notes", ""),
                )
                if part
            )
            line = name + (f" ({contact})" if contact else "")
            if details:
                line += r" --- " + inline_latex(details)
            output.append(r"\item " + line)
        output.append(r"\end{itemize}")
    return "\n".join(output)


def course_materials_latex(path: Path) -> str:
    """Build the Course Materials section, including optional resource links."""
    rows = read_markdown_table(path, "Course Materials")
    output = [r"\section*{Course Materials}"]
    for row in rows:
        item = inline_latex(row.get("Item", ""))
        required = row.get("Required", "").strip().lower() == "yes"
        details = inline_latex(row.get("Details", ""))
        url = row.get("URL", "").strip()
        label = rf"\textbf{{{item}}}"
        if required:
            label += " (required)"
        output.append(label + (f": {details}" if details else ""))
        if url:
            output.append(r"\begin{center}" + rf"\url{{{url}}}" + r"\end{center}")
    return "\n\n".join(output)


def semester_start_date(path: Path) -> date:
    """Return the first class date listed in the important-dates table."""
    semester = semester_label(path)
    year = int(re.search(r"(\d{4})$", semester).group(1))
    rows = read_markdown_table(path, "Important Dates")
    for row in rows:
        if "classes begin" in row.get("Event", "").lower():
            return datetime.strptime(f"{row['Start Date']} {year}", "%B %d %Y").date()
    raise ValueError(f"Could not find a 'Classes Begin' date in {path}.")


def lecture_metadata(path: Path) -> dict[int, dict[str, str]]:
    """Collect each lecture's module and assigned reading."""
    metadata: dict[int, dict[str, str]] = {}
    current_module = ""
    current_lecture: int | None = None
    for line in strip_comments(path.read_text(encoding="utf-8")).splitlines():
        module_match = re.match(r"^##\s+Module\s+(\d+)", line.strip())
        if module_match:
            current_module = f"Module {module_match.group(1)}"
            continue
        lecture_match = re.match(r"^###\s+Lecture\s+(\d+)", line.strip())
        if lecture_match:
            current_lecture = int(lecture_match.group(1))
            metadata[current_lecture] = {"module": current_module, "reading": ""}
            continue
        reading_match = re.match(r"^[-*]\s+\*\*Reading:\*\*\s*(.+)$", line.strip())
        if reading_match and current_lecture is not None:
            reading = reading_match.group(1).strip()
            metadata[current_lecture]["reading"] = "" if reading.lower() == "(none)" else reading
    return metadata


def homework_by_week(path: Path) -> dict[str, str]:
    """Return homework numbers keyed by schedule week."""
    return {
        row["Week"]: re.search(r"\d+", row["Homework"]).group(0)
        for row in read_markdown_table(path, "Homework Schedule")
    }


def quizzes_by_week(rows: list[dict[str, str]]) -> dict[str, str]:
    """Map weekly schedule rows to the quizzes listed in each week."""
    return {
        row["Week"]: ", ".join(re.findall(r"Quiz\s+\d+", row.get("Lecture #", "")))
        for row in rows
    }


def chapter_numbers(reading: str) -> list[str]:
    """Extract compact chapter references from a reading description."""
    matches = re.findall(
        r"Chapters?\s+([0-9]+(?:\.[0-9]+)?(?:\s*[–-]\s*[0-9]+(?:\.[0-9]+)?)?)",
        reading,
        flags=re.IGNORECASE,
    )
    return [re.sub(r"\s+", "", match) for match in matches]


def schedule_latex(path: Path, first_week_date: date) -> str:
    """Convert the generated weekly schedule into dated, multi-page tables."""
    rows = read_markdown_table(path, "Weekly Schedule")
    lectures = lecture_metadata(LECTURE_SUMMARY)
    homework = homework_by_week(HOMEWORK_SCHEDULE)
    quizzes = quizzes_by_week(rows)
    chunks = [rows[index : index + 8] for index in range(0, len(rows), 8)]
    output = []
    for chunk_index, chunk in enumerate(chunks):
        if chunk_index:
            output.append(r"\newpage")
            output.append(r"\section*{Weekly Plan (continued)}")
        output.extend(
            (
                r"\begin{center}",
                r"\scriptsize",
                r"\setlength{\tabcolsep}{2pt}",
                r"\renewcommand{\arraystretch}{1.1}",
                r"\begin{tabularx}{\textwidth}{|c|p{0.11\textwidth}|p{0.07\textwidth}|p{0.27\textwidth}|p{0.08\textwidth}|p{0.16\textwidth}|p{0.09\textwidth}|p{0.06\textwidth}|}",
                r"\hline",
                r"\textbf{Week} & \textbf{Reading (Chapter)} & \textbf{Module} & \textbf{Lecture Topics} & \textbf{Labs} & \begin{tabular}[t]{@{}c@{}}\textbf{Lab}\\\textbf{Deliverable}\end{tabular} & \textbf{Homework due Mon} & \textbf{Quiz} \\",
                r"\hline",
            )
        )
        for row in chunk:
            week_number = int(row.get("Week", "0"))
            week_date = first_week_date + timedelta(days=7 * (week_number - 1))
            lecture_numbers = [
                int(number) for number in re.findall(r"Lecture\s+(\d+)", row.get("Lecture #", ""))
            ]
            reading_values = list(dict.fromkeys(
                chapter
                for number in lecture_numbers
                for chapter in chapter_numbers(lectures.get(number, {}).get("reading", ""))
            ))
            module_values = list(dict.fromkeys(
                lectures[number]["module"].replace("Module ", "", 1)
                for number in lecture_numbers
                if lectures.get(number, {}).get("module")
            ))
            labs = row.get("Labs", "")
            # The weekly schedule contains the complete deliverable list. Keep
            # every item in the syllabus, but hide weekday tags that are only
            # used by the calendar generator (for example, [Thursday]).
            deliverables = re.sub(r"\[[^\]]+\]\s*", "", row.get("Lab Deliverables", ""))
            deliverables = re.sub(r"\bLab\s+\d+:\s*", "", deliverables)
            deliverables = deliverables.replace(" | ", "; ")
            lecture_topics = re.sub(
                r"\bQuiz\s+\d+:\s*[^;]*(?:;\s*|$)",
                "",
                row.get("Lecture Topics", ""),
                flags=re.IGNORECASE,
            )
            cells = (
                f"{week_number} ({week_date.month}/{week_date.day})",
                "; ".join(reading_values),
                "; ".join(module_values),
                re.sub(r"\s*\(Chapters?\s+[^)]*\)", "", lecture_topics, flags=re.IGNORECASE),
                labs,
                deliverables,
                homework.get(row["Week"], ""),
                quizzes.get(row["Week"], ""),
            )
            output.append(" & ".join(inline_latex(cell) for cell in cells) + r" \\")
            output.append(r"\hline")
        output.extend((r"\end{tabularx}", r"\end{center}"))
    return "\n".join(output)


def build_document() -> str:
    """Assemble the complete syllabus in the order shown in the PDF.

    The opening meeting information is followed immediately by Course Staff;
    the remaining sections are then added from their modular input files.
    """
    required = list(SYLLABUS_FILES)
    required.extend(
        (
            INPUTS / "important_dates.md",
            INPUTS / "Lecture_schedules.md",
            INPUTS / "learningObjectives.md",
            WEEKLY_SCHEDULE,
        )
    )
    missing = [path for path in required if not path.exists()]
    if missing:
        missing_list = "\n".join(f"- {path}" for path in missing)
        raise FileNotFoundError(
            "Required syllabus input(s) are missing:\n"
            f"{missing_list}\n"
            "Run generate_schedule_table.py first if weekly_schedule.md is missing."
        )

    semester = semester_label(INPUTS / "important_dates.md")
    title = course_name(INPUTS / "syllabus_course.md")
    core_count, auxiliary_count = objective_counts(INPUTS / "learningObjectives.md")
    details = course_details()

    header = (
        rf"\textbf{{LECTURES:}} {inline_latex(meeting_summary())}, "
        f"{inline_latex(details.get('Lecture Location', ''))}"
        "\n\n"
        rf"\textbf{{DISCUSSION AND LAB SECTIONS:}} "
        f"{inline_latex(details.get('Discussion and Lab Locations', 'Check your schedule.'))}"
    )
    # Keep staff information near the meeting information so students can
    # quickly identify who teaches lectures, labs, and discussion sections.
    body = (
        staff_latex(INPUTS / "syllabus_staff.md"),
        markdown_to_latex(
            INPUTS / "syllabus_course.md",
            {"Course Details"},
            {"Course Materials": course_materials_latex(INPUTS / "syllabus_course.md")},
        ),
        markdown_to_latex(INPUTS / "syllabus_grading.md"),
        r"\section*{Learning Objectives}",
        (
            f"The current learning-objective file contains "
            f"{core_count} Core-tagged entries and "
            f"{auxiliary_count} Auxiliary-tagged entries. Detailed objectives are "
            "published on the course website and assessed through the quizzes, labs, and projects."
        ),
        markdown_to_latex(INPUTS / "syllabus_assessments.md"),
        markdown_to_latex(INPUTS / "syllabus_lab.md"),
        markdown_to_latex(INPUTS / "syllabus_policies.md"),
        r"\section*{Weekly Plan (Subject to Change)}",
        schedule_latex(WEEKLY_SCHEDULE, semester_start_date(INPUTS / "important_dates.md")),
    )

    return (
        r"""\documentclass[11pt]{article}
\usepackage[margin=1in]{geometry}
\usepackage[T1]{fontenc}
\usepackage{lmodern,color}
\usepackage{parskip}
\usepackage{enumitem}
\usepackage{booktabs}
\usepackage{tabularx}
\usepackage{array}
\usepackage[hidelinks]{hyperref}
\usepackage{xurl}
\newcolumntype{Y}{>{\raggedright\arraybackslash}X}
\setlist{leftmargin=*,topsep=2pt,itemsep=1pt}
\setlength{\parskip}{5pt}
\setlength{\parindent}{0pt}
\newcommand{\syllabussectionspace}{\vspace{-3mm}}
\makeatletter
\renewcommand\section{\@startsection{section}{1}{0pt}%
  {-1.5ex plus -.4ex minus -.2ex}%
  {.5ex plus .2ex}%
  {\normalfont\Large\bfseries}}
\makeatother
\begin{document}
"""
        + "\n"
        + rf"\begin{{center}}{{\large\textbf{{MA213 -- {inline_latex(title.upper())} -- {inline_latex(semester.upper())}}}}}\\[2mm]"
        + "\n"
        + r"SYLLABUS\end{center}"
        + "\n\n"
        + header
        + "\n\n"
        + "\n\n".join(body)
        + "\n\n"
        + r"\end{document}"
        + "\n"
    )


def main() -> None:
    OUTPUT.parent.mkdir(exist_ok=True)
    OUTPUT.write_text(build_document(), encoding="utf-8")
    print(f"Wrote syllabus TeX to {OUTPUT}")


if __name__ == "__main__":
    main()
