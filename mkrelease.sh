#!/bin/sh

# Configuration
CABAL_FILE="gopherdashboard.cabal"  # Replace with your actual .cabal file name
CHANGELOG_FILE="CHANGELOG.md"

# Function to get the latest version number from the changelog
get_latest_version_from_changelog() {
    grep -oP "\[\K\d+\.\d+\.\d+\.\d+(?=\])" $CHANGELOG_FILE | head -1
}

# Get the latest version from the changelog
new_version=$(get_latest_version_from_changelog)

# Get the last tagged version from Git
last_tagged_version=$(git describe --tags --abbrev=0)

echo "New version from changelog: $new_version"
echo "Last tagged version: $last_tagged_version"

# Ask for confirmation
printf "Do you want to make a release for version $new_version? [y/N] "
read REPLY

if [ "$REPLY" = "y" ] || [ "$REPLY" = "Y" ]; then
    # Update version in the cabal file
    sed -i "s/^version: .*/version: $new_version/" $CABAL_FILE

    # Update version in the Debian control file
    sed -i "s/Version: .*/Version: $new_version/" debian-pkg/control

    # Build the deb package
    ./debian-pkg/mkdeb.sh "$new_version"

    # Build and copy the binary and then zip it up
    cabal build
    BINARY_PATH=$(cabal exec -- which gopherdashboard-exe)
    zip "gopherdashboard-$new_version-linux-amd64.zip" "$BINARY_PATH"

    # Build the package
    git add .
    git diff --staged

    printf "Do you want to commit staged changes for $new_version? [y/N] "
    read REPLY

    # Commit the changes
    if [ "$REPLY" = "y" ] || [ "$REPLY" = "Y" ]; then
        git commit -S -m "Release version $new_version"

        # Tag the release
        git tag -s "v$new_version" -m "Version $new_version"
        echo "Release $new_version created and tagged. Please push."
    else
	echo "Release aborted."
    fi

else
    echo "Release creation aborted."
fi

