#!/bin/bash

# Check if the user provided an Rmd file name as an argument
if [ "$#" -ne 1 ]; then
  echo "Usage: $0 <Rmd file name>"
  exit 1
fi

RMD_FILE="$1"

# Check if the input file exists
if [ ! -f "$RMD_FILE" ]; then
  echo "Error: File '$RMD_FILE' not found!"
  exit 1
fi

# Automatically generate the Qmd file name and title
QMD_FILE="${RMD_FILE%.Rmd}.qmd"      # Replace .Rmd with .qmd
TITLE="${RMD_FILE%.Rmd}"              # Remove .Rmd to derive the title

# Define qmd YAML front matter with dynamic title
cat << EOF > "$QMD_FILE"
---
title: "$TITLE"
format: revealjs
editor: visual
---

EOF

# Extract content strictly from the first "## " up to "## Lab Activities" (case-sensitive), without copying anything after "## Lab Activities"
awk '
BEGIN { in_section = 0 }
/^## / { in_section = 1 }
/^## Lab Activities$/ { print; exit }
in_section { print }
' "$RMD_FILE" >> "$QMD_FILE"

# Notify user of success
echo "Conversion completed: $RMD_FILE -> $QMD_FILE"
echo "Title set to: '$TITLE'"