from __future__ import annotations

import argparse
import os
from pathlib import Path


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="course-site-builder",
        description="Generate course schedule files and MkDocs source pages.",
    )
    parser.add_argument(
        "--root",
        type=Path,
        default=Path.cwd(),
        help="Course repository root. Defaults to the current directory.",
    )
    parser.add_argument(
        "--course-code",
        default="COURSE",
        help="Course code used for calendar titles and the default public build env var.",
    )
    parser.add_argument(
        "--course-title",
        help="Human-readable course title. Defaults to the course code.",
    )
    parser.add_argument(
        "--public",
        action="store_true",
        help="Generate the public/student version by hiding instructor flags.",
    )
    parser.add_argument(
        "--public-env-var",
        help="Environment variable used to detect public builds. Defaults to <COURSE_CODE>_PUBLIC_SITE.",
    )
    parser.add_argument(
        "--term-year",
        type=int,
        help="Fallback calendar year when important_dates.md does not specify one.",
    )
    parser.add_argument(
        "--instructor-inputs-dir",
        help="Directory containing instructor-edited schedule inputs.",
    )
    parser.add_argument(
        "--generated-outputs-dir",
        help="Directory where generated files are written.",
    )
    parser.add_argument(
        "--docs-dir",
        help="MkDocs source directory to sync generated pages into.",
    )
    parser.add_argument(
        "--learning-objectives-filename",
        help="Instructor input filename for course learning objectives.",
    )
    parser.add_argument(
        "--xlsx-filename",
        help="Generated Excel schedule filename.",
    )
    parser.add_argument(
        "--output-sheet",
        help="Excel worksheet name for the generated schedule.",
    )
    parser.add_argument(
        "--timezone",
        dest="local_timezone",
        help="Timezone used in the generated ICS calendar.",
    )
    return parser


def config_from_args(args: argparse.Namespace):
    from .schedule import CourseSiteConfig

    overrides = {}
    for name in (
        "term_year",
        "instructor_inputs_dir",
        "generated_outputs_dir",
        "docs_dir",
        "learning_objectives_filename",
        "xlsx_filename",
        "output_sheet",
        "local_timezone",
    ):
        value = getattr(args, name)
        if value is not None:
            overrides[name] = value

    config = CourseSiteConfig.for_repo(
        args.root,
        args.course_code,
        public_env_var=args.public_env_var,
        course_title=args.course_title,
        **overrides,
    )
    if args.public:
        os.environ[config.public_env_var] = "1"
    return config


def cli_main(argv: list[str] | None = None) -> None:
    parser = build_parser()
    args = parser.parse_args(argv)

    from .schedule import main

    main(config_from_args(args))


if __name__ == "__main__":
    cli_main()
