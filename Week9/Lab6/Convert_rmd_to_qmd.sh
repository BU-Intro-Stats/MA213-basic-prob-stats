#!/bin/bash

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 <Rmd file name>"
  exit 1
fi

RMD_FILE="$1"

if [ ! -f "$RMD_FILE" ]; then
  echo "Error: File '$RMD_FILE' not found!"
  exit 1
fi

QMD_FILE="${RMD_FILE%.Rmd}.qmd"
TITLE=$(basename "${RMD_FILE%.Rmd}" | sed -E 's/[_-]/ /g' | awk '{for(i=1;i<=NF;i++) $i=toupper(substr($i,1,1)) substr($i,2)}1')

cat << EOF > "$QMD_FILE"
---
title: "$TITLE"
format: revealjs
editor: visual
---

EOF

awk '
  /^----$/ { 
    if (found_start) exit;   
    else { found_start=1; next }
  }
  found_start { print }
' "$RMD_FILE" >> "$QMD_FILE"

echo "Conversion completed: $RMD_FILE -> $QMD_FILE"
echo "Title set to: '$TITLE'"