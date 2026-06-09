import pandas as pd
import random
from reportlab.lib.pagesizes import letter
from reportlab.pdfgen import canvas

########################
# PARAMETERS
########################

NUM_PER_GROUP = 2
INPUT_FILE = "Class_list.csv"
OUTPUT_PDF = "group_pairs.pdf"

PAGE_WIDTH, PAGE_HEIGHT = letter

OVALS_PER_PAGE = 8      # 4 rows × 2 columns

OVAL_WIDTH = 220
OVAL_HEIGHT = 90

X_MARGIN = 60
Y_MARGIN = 80
X_GAP = 60
Y_GAP = 40

########################
# LOAD + CLEAN DATA
########################

df = pd.read_csv(INPUT_FILE)

names = df["Name"].str.split(",", expand=True)
df["Last"] = names[0].str.strip()
df["First"] = names[1].str.strip()

df = df.sort_values("Last").reset_index(drop=True)

########################
# SHUFFLE + GROUP
########################

students = list(zip(df["First"], df["Last"]))
random.shuffle(students)

groups = [
    students[i:i + NUM_PER_GROUP]
    for i in range(0, len(students), NUM_PER_GROUP)
]

########################
# PDF DRAWING
########################

c = canvas.Canvas(OUTPUT_PDF, pagesize=letter)

group_counter = 1

for page_start in range(0, len(groups), OVALS_PER_PAGE):

    page_groups = groups[page_start:page_start + OVALS_PER_PAGE]

    for idx, group in enumerate(page_groups):

        row = idx // 2
        col = idx % 2

        x = X_MARGIN + col * (OVAL_WIDTH + X_GAP)
        y = PAGE_HEIGHT - Y_MARGIN - (row + 1) * (OVAL_HEIGHT + Y_GAP)

        # Oval
        c.ellipse(
            x,
            y,
            x + OVAL_WIDTH,
            y + OVAL_HEIGHT
        )

        # Group title
        c.setFont("Helvetica-Bold", 10)
        c.drawString(x + 10, y + OVAL_HEIGHT - 18, f"Group {group_counter}")

        # Student names
        c.setFont("Helvetica", 12)

        for j, (first, last) in enumerate(group):
            c.drawString(
                x + 25,
                y + OVAL_HEIGHT - 40 - j * 18,
                f"{first} {last}"
            )

        group_counter += 1

    c.showPage()   # NEW PAGE

c.save()

print("PDF created:", OUTPUT_PDF)
