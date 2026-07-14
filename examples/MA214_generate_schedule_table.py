from pathlib import Path

from course_site_builder.schedule import CourseSiteConfig, main


if __name__ == "__main__":
    course_root = Path(__file__).resolve().parent
    if course_root.name == "examples":
        course_root = course_root.parent
    main(CourseSiteConfig.for_repo(course_root, "MA214", course_title="MA 214"))
