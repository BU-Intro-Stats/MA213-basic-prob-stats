"""Generate a LaTeX syllabus from the modular instructor Markdown files."""

from __future__ import annotations

import re
from pathlib import Path


ROOT = Path(__file__).resolve().parent
INPUTS = ROOT / "instructor_inputs"
OUTPUT = ROOT / "generated_outputs" / "syllabus.tex"
WEEKLY_SCHEDULE = ROOT / "generated_outputs" / "weekly_schedule.md"

SYLLABUS_SECTIONS = (
    ("Course Information", INPUTS / "syllabus_course.md"),
    ("Course Staff", INPUTS / "syllabus_staff.md"),
    ("Grading", INPUTS / "syllabus_grading.md"),
    ("Assessments", INPUTS / "syllabus_assessments.md"),
    ("Course Policies and Student Resources", INPUTS / "syllabus_policies.md"),
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
            parts.append(r"\texttt{" + latex_escape(token[1:-1]) + "}")
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
    alignment = "|" + "|".join("X" for _ in range(column_count)) + "|"
    output = [
        r"\begin{center}",
        r"\small",
        rf"\begin{{tabularx}}{{\textwidth}}{{{alignment}}}",
        r"\hline",
        " & ".join(r"\textbf{" + inline_latex(cell) + "}" for cell in rows[0]) + r" \\",
        r"\hline",
    ]
    for row in rows[2:]:
        padded = row + [""] * (column_count - len(row))
        output.append(" & ".join(inline_latex(cell) for cell in padded[:column_count]) + r" \\")
        output.append(r"\hline")
    output.extend((r"\end{tabularx}", r"\end{center}"))
    return output


def markdown_to_latex(path: Path) -> str:
    lines = strip_comments(path.read_text(encoding="utf-8")).splitlines()
    output: list[str] = []
    paragraph: list[str] = []
    list_kind: str | None = None

    def flush_paragraph() -> None:
        if paragraph:
            output.append(inline_latex(" ".join(part.strip() for part in paragraph)))
            output.append("")
            paragraph.clear()

    def close_list() -> None:
        nonlocal list_kind
        if list_kind:
            output.append(rf"\end{{{list_kind}}}")
            output.append("")
            list_kind = None

    index = 0
    while index < len(lines):
        raw = lines[index].rstrip()
        stripped = raw.strip()

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

        heading = re.match(r"^(#{1,4})\s+(.+)$", stripped)
        if heading:
            flush_paragraph()
            close_list()
            level = len(heading.group(1))
            if level > 1:
                command = {2: "subsection", 3: "subsubsection", 4: "paragraph"}[level]
                output.append(rf"\{command}{{{inline_latex(heading.group(2))}}}")
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
        (index for index, line in enumerate(lines) if line.strip() == f"# {heading}"),
        None,
    )
    if heading_index is None:
        raise ValueError(f"Could not find '# {heading}' in {path}.")

    table_lines = []
    for line in lines[heading_index + 1 :]:
        if line.strip().startswith("|"):
            table_lines.append(line)
        elif table_lines:
            break
    rows = [split_table_row(line) for line in table_lines]
    if len(rows) < 3:
        raise ValueError(f"Could not find a Markdown table under '# {heading}' in {path}.")

    headers = rows[0]
    return [
        dict(zip(headers, row + [""] * (len(headers) - len(row))))
        for row in rows[2:]
    ]


def schedule_latex(path: Path) -> str:
    rows = read_markdown_table(path, "Weekly Schedule")
    output = [
        r"\begin{landscape}",
        r"\footnotesize",
        r"\setlength{\LTleft}{0pt}",
        r"\setlength{\LTright}{0pt}",
        r"\begin{longtable}{@{}p{0.04\linewidth}p{0.14\linewidth}p{0.35\linewidth}p{0.22\linewidth}p{0.19\linewidth}@{}}",
        r"\toprule",
        r"\textbf{Week} & \textbf{Meetings} & \textbf{Lecture Topics} & \textbf{Labs and Deliverables} & \textbf{Additional Events} \\",
        r"\midrule",
        r"\endfirsthead",
        r"\toprule",
        r"\textbf{Week} & \textbf{Meetings} & \textbf{Lecture Topics} & \textbf{Labs and Deliverables} & \textbf{Additional Events} \\",
        r"\midrule",
        r"\endhead",
    ]
    for row in rows:
        labs = row.get("Labs", "")
        deliverables = row.get("Lab Deliverables", "")
        lab_text = " -- ".join(part for part in (labs, deliverables) if part)
        cells = (
            row.get("Week", ""),
            row.get("Lecture #", ""),
            row.get("Lecture Topics", ""),
            lab_text,
            row.get("Additional Events", ""),
        )
        output.append(" & ".join(inline_latex(cell) for cell in cells) + r" \\")
        output.append(r"\addlinespace")
    output.extend((r"\bottomrule", r"\end{longtable}", r"\end{landscape}"))
    return "\n".join(output)


def build_document() -> str:
    required = [path for _, path in SYLLABUS_SECTIONS]
    required.extend(
        (
            INPUTS / "important_dates.md",
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

    body = []
    for section_title, path in SYLLABUS_SECTIONS:
        body.append(rf"\section{{{inline_latex(section_title)}}}")
        body.append(markdown_to_latex(path))

    body.extend(
        (
            r"\section{Learning Objectives}",
            (
                f"The current learning-objective file contains "
                f"{core_count} Core-tagged entries and "
                f"{auxiliary_count} Auxiliary-tagged entries."
            ),
            markdown_to_latex(INPUTS / "learningObjectives.md"),
            r"\section{Weekly Schedule}",
            (
                "This schedule is generated from the course schedule inputs. "
                "Dates and activities may change; updates will be announced through the course learning management system."
            ),
            schedule_latex(WEEKLY_SCHEDULE),
        )
    )

    return (
        r"""\documentclass[11pt]{article}
\usepackage[margin=0.75in]{geometry}
\usepackage[T1]{fontenc}
\usepackage[utf8]{inputenc}
\usepackage{lmodern}
\usepackage{microtype}
\usepackage{enumitem}
\usepackage{booktabs}
\usepackage{longtable}
\usepackage{tabularx}
\usepackage{pdflscape}
\usepackage[hidelinks]{hyperref}
\usepackage{xurl}
\setlist{nosep,leftmargin=*}
\setlength{\parindent}{0pt}
\setlength{\parskip}{0.55em}
\renewcommand{\arraystretch}{1.15}
\begin{document}
"""
        + "\n"
        + rf"\begin{{center}}\LARGE\textbf{{MA 213: {inline_latex(title)}}}\\[0.4em]"
        + "\n"
        + rf"\large {inline_latex(semester)}\end{{center}}"
        + "\n\n"
        + r"\tableofcontents"
        + "\n"
        + r"\newpage"
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
