#!/bin/bash

# Flag to track indentation status
error_code=0
# Getting list of added *.F and *.F90 files in the HEAD commit
added_files=$(git show --pretty="" --name-only --diff-filter=A HEAD | grep -E '\.F$|\.F90$')
# Staging current changes to avoid interference with the script

# Loop through each file and check indentation
for file in $added_files; do
    # Apply wfindent in-place
    wfindent -i2 $file

    # Check if the file is now modified according to git
    if git status --porcelain | grep -q "^ M $file"; then
        echo "Incorrectly indented file: $file"
        error_code=1
        # Revert changes made by wfindent
        git checkout -- "$file"
    fi
done

# Unstage the previously staged changes
git reset HEAD

# Return 1 if any file is incorrectly indented
exit $error_code

