from pathlib import Path


ROOT = Path(__file__).resolve().parent
LAB_SUMMARY = ROOT / "lab_summary.md"
LAB_SCHEDULES = ROOT / "Lab_schedules.md"

PLAN_DIRECTORY_ORDER = (
    "lab1",
    "lab2",
    "lab3",
    "project1-part1",
    "lab4",
    "lab5",
    "project1-part2",
    "project2-part1",
    "lab6",
    "project2-part2",
    "project2-part3",
    "lab7",
    "lab8-bonus",
)


def discover_lab_plan_files() -> list[Path]:
    plan_files_by_directory = {
        path.parent.name: path
        for path in ROOT.glob("Moduel*/*/*_plan.md")
        if path.parent.name.startswith(("lab", "project"))
    }
    return [
        plan_files_by_directory[directory]
        for directory in PLAN_DIRECTORY_ORDER
        if directory in plan_files_by_directory
    ]


def default_lab_schedule() -> str:
    return "\n".join(
        [
            "## Lab Meeting Pattern",
            "",
            "Edit this table when the recurring lab or project meeting day changes.",
            "",
            "| Event Type | Weekday | Start Time | End Time |",
            "| --- | --- | --- | --- |",
            "| Lab / Project | Wednesday |        |        |",
            "| Lab Deliverable | Tuesday |         | 10:00 PM |",
        ]
    )


def read_lab_schedule(path: Path) -> str:
    if path.exists():
        return path.read_text(encoding="utf-8").strip()
    return default_lab_schedule()


def build_intro(summary_path: Path, schedule_path: Path) -> str:
    schedule_text = read_lab_schedule(schedule_path)
    if not summary_path.exists():
        return "\n".join(
            [
                "# MA 213 Lab Summary",
                "",
                "This version preserves the original course sequence so that the lab flow stays familiar to students while still being aligned with the lecture schedule and learning objectives.",
                "",
                schedule_text,
            ]
        ).rstrip()

    text = summary_path.read_text(encoding="utf-8")
    details_marker = "## Lab details"
    before_details = text.split(details_marker, 1)[0]
    schedule_marker = "## Lab Meeting Pattern"
    if schedule_marker not in before_details:
        return f"{before_details.rstrip()}\n\n{schedule_text}".rstrip()

    prefix, after_schedule_start = before_details.split(schedule_marker, 1)
    after_schedule = f"{schedule_marker}{after_schedule_start}"
    next_section = after_schedule.find("\n## ", len(schedule_marker))
    suffix = after_schedule[next_section:].strip() if next_section != -1 else ""
    pieces = [prefix.rstrip(), schedule_text]
    if suffix:
        pieces.append(suffix)
    return "\n\n".join(piece for piece in pieces if piece).rstrip()


def read_plan(path: Path) -> str:
    if not path.exists():
        raise FileNotFoundError(f"Missing lab plan file: {path.relative_to(ROOT)}")
    text = path.read_text(encoding="utf-8").strip()
    if not text.endswith("---"):
        text = f"{text}\n\n---"
    return text


def build_summary(intro: str, plan_files: list[Path]) -> str:
    lines = [intro.rstrip(), "", "## Lab details", ""]
    for path in plan_files:
        lines.append(read_plan(path))
        lines.append("")
    return "\n".join(lines).rstrip() + "\n"


def main():
    plan_files = discover_lab_plan_files()
    summary = build_summary(build_intro(LAB_SUMMARY, LAB_SCHEDULES), plan_files)
    LAB_SUMMARY.write_text(summary, encoding="utf-8")
    print(f"Wrote {LAB_SUMMARY.name} from {len(plan_files)} lab/project plan files.")


if __name__ == "__main__":
    main()
