# Runbook

[![Open with Runme](https://badgen.net/badge/Open%20with/Runme/5B3ADF?icon=https://docs.runme.dev/img/logo.svg)](https://www.runme.dev/api/runme?repository=https%3A%2F%2Fgithub.com%2Fmooltiverse%2Fnyx.git)

This is the general purpose runbook for the project. Here you can find the scripts that you can use for common repetitive operations and also run them interactively.

> [!TIP]
> You can run these commands interactively with [Runme](https://www.runme.dev/). Consider [installing](https://docs.runme.dev/installation/) Runme locally, as a VS Code add on or as a browser extension to streamline the process.

## General commands

```bash
# Build the whole project
./gradlew build
```

```bash
# Clean the artifacts from previous builds
./gradlew clean
# Clean the artifacts from previous builds, including external cached contents
# and test projects created on target platforms (GitHub, GitLab)
./gradlew deepClean
```

```bash
# Run all tests
./gradlew tests
# Run all unit tests
./gradlew unitTest
# Run all integration tests
./gradlew integrationTest
# Run all functional tests
./gradlew functionalTest
```

## Documentation

```bash
export DOCUSAURUS_PORT=3000

# Start the Docusaurus development server
cd docs/main
npm run start -- --host 0.0.0.0 --port $DOCUSAURUS_PORT
```
