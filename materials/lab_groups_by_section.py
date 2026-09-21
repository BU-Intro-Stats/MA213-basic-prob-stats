import csv
import os
import pandas as pd

from local_config import LAB_BASE_DIR as BASE_DIR

########################
# PARAMETERS
########################

GROUP_SET = "Lab_gc_Group"
SECTIONS = ["C1", "C2", "C3", "C4"]
INPUT_FILE_TEMPLATE = os.path.join(BASE_DIR, "{section}_mybu.csv")

OUTPUT_BB_CSV = os.path.join(BASE_DIR, "Section Groups", "sections_bb.csv")
OUTPUT_GROUPS_CSV = os.path.join(BASE_DIR, "Section Groups", "section_group_definitions_bb.csv")

with open(OUTPUT_BB_CSV, "w", newline="", encoding="utf-8-sig") as bb_csv_file, \
     open(OUTPUT_GROUPS_CSV, "w", newline="", encoding="utf-8-sig") as groups_csv_file:

    bb_csv_writer = csv.writer(bb_csv_file)
    bb_csv_writer.writerow(["Group Code*", "User Name*", "Student Id", "First Name", "Last Name", "Group Set"])

    groups_csv_writer = csv.writer(groups_csv_file)
    groups_csv_writer.writerow(["Group Code*", "Title*", "Description", "Group Set*", "Self Enroll*"])

    for section in SECTIONS:

        INPUT_FILE = INPUT_FILE_TEMPLATE.format(section=section)

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

        students = list(zip(df["First"], df["Last"], df["Student ID"], df["Username"]))

        ########################
        # BLACKBOARD GROUP CSV ROWS — one group per section
        ########################

        group_code = f"{section}_gc_Section"
        title = f"{section} Section"

        groups_csv_writer.writerow([group_code, title, "", GROUP_SET, "N"])

        for first, last, student_id, username in students:
            bb_csv_writer.writerow([group_code, username, student_id, first, last, GROUP_SET])

print("CSV created:", OUTPUT_BB_CSV)
print("CSV created:", OUTPUT_GROUPS_CSV)
