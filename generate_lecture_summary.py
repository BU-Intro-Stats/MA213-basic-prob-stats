from pathlib import Path
import re


ROOT = Path(__file__).resolve().parent
LECTURE_SUMMARY = ROOT / "lecture_summary.md"
LEARNING_OBJECTIVES = ROOT / "learningObjectives.md"
SCHEDULES = ROOT / "Schedules.md"


QUIZ_AFTER_LECTURE = {
    8: "Quiz 1",
    16: "Quiz 2",
    24: "Quiz 3",
    32: "Quiz 4",
    35: "Quiz 5",
}


def clean_text(text: str) -> str:
    return re.sub(r"\s+", " ", text).strip()


def latex_to_markdown(text: str) -> str:
    text = text.replace("\\%", "%")
    text = text.replace("--", "–")
    text = re.sub(r"\\hl\{([^{}]*)\}", r"\1", text)
    text = re.sub(r"\\textbf\{([^{}]*)\}", r"\1", text)
    text = re.sub(r"\\emph\{([^{}]*)\}", r"\1", text)
    text = re.sub(r"\\textit\{([^{}]*)\}", r"\1", text)
    text = re.sub(r"\\[A-Za-z]+\*?(?:\[[^\]]*\])?(?:\{([^{}]*)\})?", r"\1", text)
    text = text.replace("{", "").replace("}", "")
    return clean_text(text)


def build_intro(schedule_path: Path) -> str:
    if schedule_path.exists():
        schedule_text = schedule_path.read_text(encoding="utf-8").strip()
    else:
        schedule_text = "\n".join(
            [
                "## Course Meeting Pattern",
                "",
                "Edit this table when the recurring course meeting days change.",
                "",
                "| Event Type | Weekdays | Start Time | End Time |",
                "| --- | --- | --- | --- |",
                "| Lecture | Monday, Wednesday, Friday | 11:15 AM | 12:05 PM |",
                "| Discussion | Thursday | 12:20 PM | 1:10 PM |",
                "| Office Hour 1 | Friday | 3:00 PM | 4:00 PM |",
                "| Office Hour 2 | Monday | 4:00 PM | 5:00 PM |",
                "| Homework | Sunday | 2:55 PM | 3:00 PM |",
                "",
                "---",
            ]
        )

    return f"# MA 213 Lecture Summary\n\n{schedule_text}".rstrip()


def parse_learning_objectives(path: Path):
    objectives = {}
    module_num = None
    module_heading = None

    for raw in path.read_text(encoding="utf-8").splitlines():
        line = raw.strip()
        module_match = re.match(r"^## Module\s+(\d+):\s+(.+)$", line)
        if module_match:
            module_num = int(module_match.group(1))
            module_heading = module_match.group(2)
            continue

        objective_match = re.match(r"^(\d+)\.\s+\*\*(.+?):\*\*\s+(.+)$", line)
        if not objective_match or module_num is None:
            continue

        lo_num = int(objective_match.group(1))
        title = clean_text(objective_match.group(2))
        rest = clean_text(objective_match.group(3))
        assessment_match = re.search(r"\[([^\]]+)\]", rest)
        assessment = assessment_match.group(1) if assessment_match else ""
        emphasis = "Core" if "**Core**" in rest else "Auxiliary" if "Auxiliary" in rest else ""
        description = re.sub(r"\s*\[[^\]]+\]\s*", " ", rest)
        description = description.replace("**Core**", "").replace("Auxiliary", "")

        objectives[(module_num, lo_num)] = {
            "module_heading": module_heading,
            "title": title,
            "description": clean_text(description),
            "assessment": assessment,
            "emphasis": emphasis,
        }

    return objectives


def lecture_number_from_path(path: Path) -> int:
    match = re.search(r"Lecture(\d+)_agenda\.tex$", path.name)
    if not match:
        raise ValueError(f"Could not find lecture number in {path}")
    return int(match.group(1))


def extract_hl_item(text: str, label: str) -> str:
    pattern = rf"\\item[ \t]+\\hl\{{{re.escape(label)}:[ \t]*\}}[ \t]*([^\n]*)"
    match = re.search(pattern, text)
    return latex_to_markdown(match.group(1)) if match else ""


def extract_reading(text: str) -> str:
    reading = extract_hl_item(text, "Reading")
    if reading:
        return reading

    reading_match = re.search(
        r"\\item[ \t]+\\hl\{Reading:[ \t]*\}[ \t]*\n(?P<body>.*?)(?=\n[ \t]*\\end\{itemize\})",
        text,
        flags=re.DOTALL,
    )
    if not reading_match:
        return ""

    readings = []
    for item in re.finditer(r"\\item[ \t]+([^\n]+)", reading_match.group("body")):
        readings.append(latex_to_markdown(item.group(1)))
    return "; ".join(reading for reading in readings if reading)


def parse_agenda(path: Path, objectives):
    text = path.read_text(encoding="utf-8")
    lecture_num = lecture_number_from_path(path)

    module_title = ""
    module_match = re.search(r"\\frametitle\{(Module\s+\d+:[^{}]+)\}", text)
    if module_match:
        module_title = latex_to_markdown(module_match.group(1))

    topic = extract_hl_item(text, "This time")
    reading = extract_reading(text)
    if not reading:
        reading = "(none)"

    learning_objectives = []
    for match in re.finditer(
        r"\\item\s+\\textbf\{\s*M\s*(\d+)\s*,\s*L(?:O|0)?\s*(\d+)\s*:\s*([^:}]+):?\s*\}",
        text,
    ):
        module_num = int(match.group(1))
        lo_num = int(match.group(2))
        fallback_title = latex_to_markdown(match.group(3))
        objective = objectives.get((module_num, lo_num))
        if objective:
            title = objective["title"]
            suffix = ""
            if objective["assessment"] and objective["emphasis"]:
                suffix = f" [{objective['assessment']}] {objective['emphasis']}"
            elif objective["emphasis"]:
                suffix = f" {objective['emphasis']}"
            learning_objectives.append(f"M{module_num}, LO{lo_num}: {title}{suffix}")
        else:
            learning_objectives.append(f"M{module_num}, LO{lo_num}: {fallback_title}")

    return {
        "lecture_num": lecture_num,
        "module_title": module_title,
        "topic": topic,
        "reading": reading,
        "learning_objectives": learning_objectives,
    }


def agenda_paths():
    return sorted(
        ROOT.glob("Week*/Lecture*/Lecture*_agenda.tex"),
        key=lecture_number_from_path,
    )


def build_summary(lectures, intro: str) -> str:
    lines = [intro.rstrip(), ""]
    current_module = None

    for lecture in lectures:
        module_title = lecture["module_title"]
        if module_title and module_title != current_module:
            if current_module is not None:
                lines.append("---")
                lines.append("")
            lines.append(f"## {module_title}")
            lines.append("")
            current_module = module_title

        lines.append(f"### Lecture {lecture['lecture_num']}")
        lines.append(f"- **Topic:** {lecture['topic'] or '(none)'}")
        lines.append(f"- **Reading:** {lecture['reading'] or '(none)'}")
        objectives = lecture["learning_objectives"]
        if objectives:
            lines.append("- **Learning Objectives:**")
            for objective in objectives:
                lines.append(f"  - {objective}")
        else:
            lines.append("- **Learning Objectives:** (none listed)")
        lines.append("")

        quiz = QUIZ_AFTER_LECTURE.get(lecture["lecture_num"])
        if quiz:
            lines.append(f"### {quiz}")
            lines.append("")

    return "\n".join(lines).rstrip() + "\n"


def main():
    objectives = parse_learning_objectives(LEARNING_OBJECTIVES)
    lectures = [parse_agenda(path, objectives) for path in agenda_paths()]
    summary = build_summary(lectures, build_intro(SCHEDULES))
    LECTURE_SUMMARY.write_text(summary, encoding="utf-8")
    print(f"Wrote {LECTURE_SUMMARY.name} from {len(lectures)} lecture agenda files.")


if __name__ == "__main__":
    main()
