import csv
import os
import pandas as pd
import random
from reportlab.lib.pagesizes import letter, landscape
from reportlab.pdfgen import canvas

from local_config import LAB_BASE_DIR as BASE_DIR

########################
# PARAMETERS
########################


# GROUP_SET = "Lab3_gc_Groups"
# GROUP_SIZE_MIN = 2
# GROUP_SIZE_MAX = 3
# GROUP_SIZE_PREFERRED = 2
# SECTIONS = ["C1", "C2", "C3", "C4"]
# INPUT_FILE_TEMPLATE = os.path.join(BASE_DIR, "{section}_mybu.csv")

# OUTPUT_PDF_TEMPLATE = os.path.join(BASE_DIR, "Lab3", "groups_{section}.pdf")
# OUTPUT_BB_CSV = os.path.join(BASE_DIR, "Lab3", "groups_bb.csv")
# OUTPUT_GROUPS_CSV = os.path.join(BASE_DIR, "Lab3", "group_definitions_bb.csv")

GROUP_SET = "Project1_gc_Groups"
GROUP_SIZE_MIN = 3
GROUP_SIZE_MAX = 4
GROUP_SIZE_PREFERRED = 4
SECTIONS = ["C1", "C2", "C3", "C4"]
INPUT_FILE_TEMPLATE = os.path.join(BASE_DIR, "{section}_mybu.csv")

OUTPUT_PDF_TEMPLATE = os.path.join(BASE_DIR, "Project 1", "groups_{section}.pdf")
OUTPUT_BB_CSV = os.path.join(BASE_DIR, "Project 1", "groups_bb.csv")
OUTPUT_GROUPS_CSV = os.path.join(BASE_DIR, "Project 1", "group_definitions_bb.csv")

PAGE_WIDTH, PAGE_HEIGHT = landscape(letter)

X_MARGIN = 40
Y_MARGIN = 40
X_GAP = 20
Y_GAP = 20

with open(OUTPUT_BB_CSV, "w", newline="", encoding="utf-8-sig") as bb_csv_file, \
     open(OUTPUT_GROUPS_CSV, "w", newline="", encoding="utf-8-sig") as groups_csv_file:

    bb_csv_writer = csv.writer(bb_csv_file)
    bb_csv_writer.writerow(["Group Code*", "User Name*", "Student Id", "First Name", "Last Name", "Group Set"])

    groups_csv_writer = csv.writer(groups_csv_file)
    groups_csv_writer.writerow(["Group Code*", "Title*", "Description", "Group Set*", "Self Enroll*"])

    for section in SECTIONS:

        INPUT_FILE = INPUT_FILE_TEMPLATE.format(section=section)
        OUTPUT_PDF = OUTPUT_PDF_TEMPLATE.format(section=section)

        ########################
        # LOAD + CLEAN DATA
        ########################

        df = pd.read_csv(INPUT_FILE)

        df["Name"] = df["Name"].str.replace(r"\s*\([^)]*\)", "", regex=True)

        names = df["Name"].str.split(",", regex=True, expand=True)
        df["Last"] = names[0].str.strip()
        df["First"] = names[1].str.strip()

        df["Username"] = df["Email Address"].str.split("@").str[0]

        df = df.sort_values("Last").reset_index(drop=True)

        ########################
        # SHUFFLE + GROUP
        ########################

        students = list(zip(df["First"], df["Last"], df["Student ID"], df["Username"]))
        random.shuffle(students)

        n = len(students)

        # Start from the group count that best matches GROUP_SIZE_PREFERRED, then
        # nudge it until every group's size falls within [GROUP_SIZE_MIN, GROUP_SIZE_MAX].
        num_groups = max(1, round(n / GROUP_SIZE_PREFERRED))
        while num_groups > 1 and n / num_groups > GROUP_SIZE_MAX:
            num_groups += 1
        while num_groups > 1 and n / num_groups < GROUP_SIZE_MIN:
            num_groups -= 1

        base, remainder = divmod(n, num_groups)
        sizes = [base + 1 if i < remainder else base for i in range(num_groups)]

        groups = []
        i = 0
        for size in sizes:
            groups.append(students[i:i + size])
            i += size

        ########################
        # LAYOUT — fit every group's box on a single page
        ########################

        available_width = PAGE_WIDTH - 2 * X_MARGIN
        available_height = PAGE_HEIGHT - 2 * Y_MARGIN

        # Try every column count and keep the grid that gives the largest box
        # (i.e. the roomiest layout that still fits len(groups) boxes on one page).
        best = None
        for columns in range(1, len(groups) + 1):
            rows = -(-len(groups) // columns)
            box_width = (available_width - (columns - 1) * X_GAP) / columns
            box_height = (available_height - (rows - 1) * Y_GAP) / rows
            if best is None or box_width * box_height > best[0]:
                best = (box_width * box_height, columns, rows, box_width, box_height)

        _, COLUMNS, ROWS, BOX_WIDTH, BOX_HEIGHT = best

        # Scale text to the box so it stays readable whether the grid is roomy
        # (few groups) or tight (many groups on one page).
        NAME_FONT_SIZE = max(6, min(12, BOX_HEIGHT / (GROUP_SIZE_MAX + 2)))
        TITLE_FONT_SIZE = max(6, min(10, NAME_FONT_SIZE))
        TEXT_INDENT = max(6, min(25, BOX_WIDTH * 0.12))
        TITLE_Y_OFFSET = TITLE_FONT_SIZE + 8
        NAMES_TOP_OFFSET = TITLE_Y_OFFSET + NAME_FONT_SIZE + 6
        NAME_LINE_GAP = (BOX_HEIGHT - NAMES_TOP_OFFSET - 6) / max(GROUP_SIZE_MAX - 1, 1)

        if NAME_FONT_SIZE <= 6:
            print(
                f"Warning: [{section}] fitting {len(groups)} groups on one page gives "
                f"boxes {BOX_WIDTH:.0f}x{BOX_HEIGHT:.0f}pt — text is at the minimum "
                "readable size and may be cramped."
            )

        ########################
        # PDF DRAWING
        ########################

        c = canvas.Canvas(OUTPUT_PDF, pagesize=(PAGE_WIDTH, PAGE_HEIGHT))

        for idx, group in enumerate(groups):

            row = idx // COLUMNS
            col = idx % COLUMNS

            x = X_MARGIN + col * (BOX_WIDTH + X_GAP)
            y = PAGE_HEIGHT - Y_MARGIN - (row + 1) * (BOX_HEIGHT + Y_GAP)

            # Box
            c.rect(
                x,
                y,
                BOX_WIDTH,
                BOX_HEIGHT
            )

            # Group title
            c.setFont("Helvetica-Bold", TITLE_FONT_SIZE)
            c.drawString(x + TEXT_INDENT / 2, y + BOX_HEIGHT - TITLE_Y_OFFSET, f"Group {idx + 1}")

            # Student names
            c.setFont("Helvetica", NAME_FONT_SIZE)

            for j, (first, last, student_id, username) in enumerate(group):
                c.drawString(
                    x + TEXT_INDENT,
                    y + BOX_HEIGHT - NAMES_TOP_OFFSET - j * NAME_LINE_GAP,
                    f"{first} {last}"
                )

        c.showPage()
        c.save()

        print("PDF created:", OUTPUT_PDF)

        ########################
        # BLACKBOARD GROUP CSV ROWS
        ########################

        for idx, group in enumerate(groups):
            group_code = f"{section}_gc_Lab_gc_Group_gc_{idx + 1}"
            title = f"{section} Lab Group {idx + 1}"

            groups_csv_writer.writerow([group_code, title, "", GROUP_SET, "N"])

            for first, last, student_id, username in group:
                bb_csv_writer.writerow([group_code, username, student_id, first, last, GROUP_SET])

print("CSV created:", OUTPUT_BB_CSV)
print("CSV created:", OUTPUT_GROUPS_CSV)
