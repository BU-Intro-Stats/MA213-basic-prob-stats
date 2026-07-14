from pathlib import Path

from course_site_builder.schedule import CourseSiteConfig, main


if __name__ == "__main__":
    main(CourseSiteConfig.for_repo(Path(__file__).resolve().parent, "MA213", course_title="MA 213"))
