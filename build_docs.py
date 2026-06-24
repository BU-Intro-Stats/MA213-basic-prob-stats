from pathlib import Path
import shutil


ROOT = Path(__file__).resolve().parent
DOCS = ROOT / "docs"

SOURCE_PAGES = {
    "weekly_schedule.md": "weekly_schedule.md",
    "calendar_schedule.md": "calendar_schedule.md",
    "lecture_summary.md": "lecture_summary.md",
    "lab_summary.md": "lab_summary.md",
    "learningObjectives.md": "learning_objectives.md",
}


def main():
    DOCS.mkdir(exist_ok=True)

    for source_name, output_name in SOURCE_PAGES.items():
        source = ROOT / source_name
        destination = DOCS / output_name
        shutil.copyfile(source, destination)

    print(f"Synced {len(SOURCE_PAGES)} markdown files to {DOCS.name}/")


if __name__ == "__main__":
    main()
