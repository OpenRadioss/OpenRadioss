#!/bin/bash
# Usage: bash ./integrate.sh <target_branch> <commit id>
# Check if both arguments are provided
if [ $# -ne 2 ]; then
    echo "Error: You must provide a release branch name and a commit SHA1"
    exit 1
fi

# Get the branch name and commit SHA1 from the arguments
release_name=$1
commit_sha1=$2

# Get the short version (8 characters) of the commit SHA1
short_sha1=$(echo $commit_sha1 | cut -c -8)

# Fetch the upstream repository
echo "Fetching the upstream repository..."
if ! git fetch upstream; then
    echo "Error: Failed to fetch the upstream repository"
    exit 1
fi

# Create and checkout to new branch
echo "Creating and checking out to branch integ_${short_sha1}..."
if ! git checkout -b integ_${short_sha1} upstream/$release_name; then
    echo "Error: Failed to checkout to branch integ_${short_sha1}"
    exit 1
fi

# Cherry-pick the commit
echo "Cherry-picking commit ${commit_sha1}..."
if ! git cherry-pick ${commit_sha1}; then
    echo "Error: Failed to cherry-pick commit ${commit_sha1}"
    exit 1
fi

# Pull and rebase from the upstream branch
echo "Pulling and rebasing from upstream/$release_name..."
if ! git pull --rebase upstream $release_name; then
    echo "Error: Failed to pull and rebase from upstream/$release_name"
    exit 1
fi

echo "build, test and:"
echo "git push origin integ_${short_sha1}"

