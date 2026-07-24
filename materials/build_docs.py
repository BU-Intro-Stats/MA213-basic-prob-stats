from pathlib import Path
import shutil


ROOT = Path(__file__).resolve().parent
DOCS = ROOT / "docs"
INSTRUCTOR_INPUTS = ROOT / "instructor_inputs"
GENERATED_OUTPUTS = ROOT / "generated_outputs"

SOURCE_PAGES = {
    GENERATED_OUTPUTS / "weekly_schedule.md": "weekly_schedule.md",
    GENERATED_OUTPUTS / "calendar_schedule.md": "calendar_schedule.md",
    GENERATED_OUTPUTS / "course_calendar.ics": "course_calendar.ics",
    INSTRUCTOR_INPUTS / "lecture_summary.md": "lecture_summary.md",
    INSTRUCTOR_INPUTS / "lab_summary.md": "lab_summary.md",
    INSTRUCTOR_INPUTS / "learningObjectives.md": "learning_objectives.md",
}


def main():
    DOCS.mkdir(exist_ok=True)

    for source, output_name in SOURCE_PAGES.items():
        destination = DOCS / output_name
        shutil.copyfile(source, destination)

    print(f"Synced {len(SOURCE_PAGES)} site source files to {DOCS.name}/")


if __name__ == "__main__":
    main()
