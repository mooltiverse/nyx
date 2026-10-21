# Running Nyx in builds and CI/CD

## Principles

- Infer early, release late: run `infer` at the start of the pipeline so the version is available to every build step, then `publish` (or `mark`) at the end, after tests pass.
- Connect separate runs with `stateFile` plus `resume: true`. The state file and configuration are flavor agnostic, so the CLI, Docker, Gradle and the GitHub Action can share them.
- Always check out the full history.
- Pass secrets as environment variables and reference them with `{{#environmentVariable}}NAME{{/environmentVariable}}` templates.
- Gate official releases on the CI environment with `matchEnvironmentVariables: { CI: "^true$" }` so local runs never tag or publish.

## GitHub Actions with the Nyx action

The action is a Docker action, so it runs on Linux runners only.

```yaml
permissions:
  contents: write            # needed to push tags and create releases
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v6
        with:
          fetch-depth: 0     # REQUIRED: full history and tags
      - name: Infer the version
        id: nyx
        uses: mooltiverse/nyx@main
        with:
          command: infer
          resume: true
          stateFile: build/.nyx-state.json
      - name: Build
        run: ./build.sh "${{ steps.nyx.outputs.version }}"
      - name: Release
        if: steps.nyx.outputs.newRelease == 'true'
        uses: mooltiverse/nyx@main
        env:
          GH_TOKEN: ${{ secrets.GITHUB_TOKEN }}
        with:
          command: publish
          resume: true
          stateFile: build/.nyx-state.json
```

- Inputs: `command`, `bump`, `changelogPath`, `changelogTemplate`, `commitMessageConventionsEnabled`, `configurationFile`, `directory`, `dryRun`, `initialVersion`, `preset`, `releaseLenient`, `releasePrefix`, `releaseTypesEnabled`, `releaseTypesPublicationServices`, `releaseTypesRemoteRepositories`, `resume`, `scheme`, `sharedConfigurationFile`, `stateFile`, `summaryFile`, `verbosity`, `version`. Options with dynamic names (services, release type items) are only available through configuration files or `NYX_*` environment variables.
- Outputs: `branch`, `bump`, `coreVersion`, `latestVersion`, `newRelease`, `newVersion`, `scheme`, `timestamp`, `previousVersion`, `primeVersion`, `version`.
- Across jobs, carry the state file with `actions/cache` or artifacts, keyed by `${{ github.run_id }}`.

## GitLab CI

```yaml
variables:
  GIT_STRATEGY: clone
  GIT_DEPTH: "0"           # REQUIRED: full history and tags
release:
  image:
    name: mooltiverse/nyx:latest
    entrypoint: [""]
  script:
    - nyx --state-file=build/.nyx-state.json --resume publish
```

Store the token as a masked CI/CD variable (i.e. `GITLAB_TOKEN`, with the `api`, `read_user`, `read_api`, `write_repository` and `write_registry` scopes) and reference it from the `gitlab` service and the Git remote (user `PRIVATE-TOKEN`, token as the password).

## Command line and Docker

```sh
nyx --state-file=build/.nyx-state.json --resume infer
# ... build and test using the state or summary file ...
nyx --state-file=build/.nyx-state.json --resume publish

docker run --rm -v "$PWD":/project -u "$(id -u):$(id -g)" -e GH_TOKEN mooltiverse/nyx:latest publish
```

Nyx exits with `0` on success and non-zero on errors. `-e GH_TOKEN` without a value forwards the variable from the host without exposing it on the command line.

## Gradle plugin

Apply it in `settings.gradle` so the version is inferred during initialization:

```groovy
plugins {
  id "com.mooltiverse.oss.nyx" version "<NYX_VERSION>"
}
```

- `version` (Gradle's own property) is set from the inferred version. In multi-project builds use `rootProject.version`.
- The whole state is available as `project.nyxState`, i.e. `rootProject.nyxState.newRelease`.
- Tasks: `nyxClean`, `nyxInfer`, `nyxMake`, `nyxMark`, `nyxPublish`, and the `release` lifecycle task. `nyxMake` hooks into `assemble` and `nyxClean` into `clean`.
- Gate publishing tasks with `onlyIf { rootProject.nyxState.newRelease }`.
- The `nyx { }` block can hold configuration, but a `.nyx.yaml` keeps it shareable with the CLI and the GitHub Action. In the Gradle DSL, list items must be written as named blocks (`"0" { ... }`, `"1" { ... }`) because of a Gradle limitation.
