#!/bin/sh

##### STEP 1: prune arguments with no value

#printf "raw arguments: %s\n" "$@"

# We always need to produce the summary to parse the command outputs
CLEAN_ARGS="--summary"
for arg in $@
do
    if echo "$arg" | grep -Eq '\-\-.+'; then
        # The argument starts with '--', so check if it also has a value
        if echo "$arg" | grep -Eq '.+=\S+'; then
            # The argument also has a value, add it to the command line
            CLEAN_ARGS="$CLEAN_ARGS $arg"
        fi
        # If the argument has no value we discard it
    else
        # The argument doesn't start with -- (it's probably the Nyx command argument)
        # so add it to the command line as is
        CLEAN_ARGS="$CLEAN_ARGS $arg"
    fi
done

#printf "clean arguments: %s\n" "$CLEAN_ARGS"

##### STEP 2: run Nyx
OUTPUT=$(/usr/bin/nyx $CLEAN_ARGS 2>&1)
res=$?

# Print the output we captured
printf "%s\n" "$OUTPUT"

if [ $res -ne 0 ]; then
    echo "Nyx returned an error $res"
    exit $res
fi

##### STEP 3: for each value from the output, discard everything before the '=' sign and set its corresponding output value
# Remove all the log outputs from the output before we parse it
OUTPUT=$(echo "$OUTPUT" | grep -v 'msg=' )

# Parse values and remove any newline character
echo "branch=$(echo "$OUTPUT" | grep 'branch' | sed 's/^.*\s*=\s*//' | tr -d \\n)" >> $GITHUB_OUTPUT
echo "bump=$(echo "$OUTPUT" | grep 'bump' | sed 's/^.*\s*=\s*//' | tr -d \\n)" >> $GITHUB_OUTPUT
echo "coreVersion=$(echo "$OUTPUT" | grep 'core version' | sed 's/^.*\s*=\s*//' | tr -d \\n)" >> $GITHUB_OUTPUT
echo "latestVersion=$(echo "$OUTPUT" | grep 'latest version' | sed 's/^.*\s*=\s*//' | tr -d \\n)" >> $GITHUB_OUTPUT
echo "newRelease=$(echo "$OUTPUT" | grep 'new release' | sed 's/^.*\s*=\s*//' | tr -d \\n)" >> $GITHUB_OUTPUT
echo "newVersion=$(echo "$OUTPUT" | grep 'new version' | sed 's/^.*\s*=\s*//' | tr -d \\n)" >> $GITHUB_OUTPUT
echo "scheme=$(echo "$OUTPUT" | grep 'scheme' | sed 's/^.*\s*=\s*//' | tr -d \\n)" >> $GITHUB_OUTPUT
echo "timestamp=$(echo "$OUTPUT" | grep 'timestamp' | sed 's/^.*\s*=\s*//' | tr -d \\n)" >> $GITHUB_OUTPUT
echo "previousVersion=$(echo "$OUTPUT" | grep 'previous version' | sed 's/^.*\s*=\s*//' | tr -d \\n)" >> $GITHUB_OUTPUT
echo "primeVersion=$(echo "$OUTPUT" | grep 'prime version' | sed 's/^.*\s*=\s*//' | tr -d \\n)" >> $GITHUB_OUTPUT
echo "version=$(echo "$OUTPUT" | grep 'current version' | sed 's/^.*\s*=\s*//' | tr -d \\n)" >> $GITHUB_OUTPUT
