#!/bin/bash

output_file="all_rvtest_results.txt"

# Print the header to the combined output file
echo -e "N\tn_variants\tn_polyvar\tN_nonref\tPvalue_cmc\tTrait" > "$output_file"

# Loop through all txt files
for file in *.assoc; do
  # Extract trait name: part after the second underscore
  # For example: prefix_123_traitname_more.txt => traitname
  trait=$(basename "$file" | cut -d'_' -f3-)
  
  # Skip header line and process data lines
  # Extract last 5 columns and add trait as last column
  tail -n +2 "$file" | awk -v trait="$trait" '{print $(NF-4), $(NF-3), $(NF-2), $(NF-1), $NF, trait}' OFS='\t' >> "$output_file"
done

echo "Combined file created at $output_file"