#!/bin/bash
# Script to convert all .ppm files in out/ to .png in docs/media/renders

input_dir="out"
output_dir="docs/media/renders"

# Create output directory if it doesn't exist
mkdir -p "$output_dir"

# Loop through all .ppm files in the out/ directory
for file in "$input_dir"/*.ppm; do
  if [[ -f "$file" ]]; then
    # Get the base filename without the directory
    filename=$(basename "$file")
    # Replace .ppm with .png for the output filename
    output_file="$output_dir/${filename%.ppm}.png"

    # Convert the file to .png
    convert "$file" "$output_file"
    echo "Converted: $file to $output_file"
  fi
done
