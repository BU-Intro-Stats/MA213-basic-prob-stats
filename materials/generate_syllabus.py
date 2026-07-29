"""Generate a LaTeX syllabus from the modular instructor Markdown files."""

from __future__ import annotations

import re
from pathlib import Path


ROOT = Path(__file__).resolve().parent
INPUTS = ROOT / "instructor_inputs"
OUTPUT = ROOT / "generated_outputs" / "syllabus.tex"
WEEKLY_SCHEDULE = ROOT / "generated_outputs" / "weekly_schedule.md"

SYLLABUS_FILES = (
    INPUTS / "syllabus_course.md",
    INPUTS / "syllabus_staff.md",
    INPUTS / "syllabus_grading.md",
    INPUTS / "syllabus_assessments.md",
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
            output.append(r"\item " + inline_latex(item.group(1)))
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


def schedule_latex(path: Path) -> str:
    rows = read_markdown_table(path, "Weekly Schedule")
    chunks = [rows[index : index + 8] for index in range(0, len(rows), 8)]
    output = []
    for chunk_index, chunk in enumerate(chunks):
        if chunk_index:
            output.append(r"\newpage")
            output.append(r"\section*{Weekly Plan (continued)}")
        output.extend(
            (
                r"\begin{center}",
                r"\footnotesize",
                r"\setlength{\tabcolsep}{3pt}",
                r"\renewcommand{\arraystretch}{1.15}",
                r"\begin{tabularx}{\textwidth}{|c|Y|Y|p{0.12\textwidth}|}",
                r"\hline",
                r"\textbf{Week} & \textbf{Lecture Topics} & \textbf{Labs and Deliverables} & \textbf{Other} \\",
                r"\hline",
            )
        )
        for row in chunk:
            labs = row.get("Labs", "")
            deliverables = row.get("Lab Deliverables", "")
            lab_text = " -- ".join(part for part in (labs, deliverables) if part)
            cells = (
                row.get("Week", ""),
                row.get("Lecture Topics", ""),
                lab_text,
                row.get("Additional Events", ""),
            )
            output.append(" & ".join(inline_latex(cell) for cell in cells) + r" \\")
            output.append(r"\hline")
        output.extend((r"\end{tabularx}", r"\end{center}"))
    return "\n".join(output)


def build_document() -> str:
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
    body = (
        markdown_to_latex(
            INPUTS / "syllabus_course.md",
            {"Course Details"},
            {"Course Materials": course_materials_latex(INPUTS / "syllabus_course.md")},
        ),
        staff_latex(INPUTS / "syllabus_staff.md"),
        markdown_to_latex(INPUTS / "syllabus_grading.md"),
        markdown_to_latex(INPUTS / "syllabus_assessments.md"),
        markdown_to_latex(INPUTS / "syllabus_policies.md"),
        r"\section*{Learning Objectives}",
        (
            f"The current learning-objective file contains "
            f"{core_count} Core-tagged entries and "
            f"{auxiliary_count} Auxiliary-tagged entries. Detailed objectives are "
            "published on the course website and assessed through the quizzes, labs, and projects."
        ),
        r"\section*{Weekly Plan (Subject to Change)}",
        schedule_latex(WEEKLY_SCHEDULE),
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
  {\normalfont\large\bfseries}}
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
