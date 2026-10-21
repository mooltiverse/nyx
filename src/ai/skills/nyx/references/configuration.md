# Configuring Nyx

## Where configuration comes from

Sources in order of priority, highest first. The first source defining an option wins:

1. command line options (`--release-prefix=v`)
2. environment variables (`NYX_RELEASE_PREFIX=v`)
3. Gradle plugin settings (the `nyx { }` block)
4. the custom configuration file (`configurationFile`, which may be a URL)
5. the standard configuration file in the repository root. Only one is loaded: `.nyx.yml`, else `.nyx.yaml`, else `.nyx.json`. The others are ignored, not merged
6. the custom shared configuration file (`sharedConfigurationFile`)
7. the standard shared configuration file, with the same rule: `.nyx-shared.yml`, else `.nyx-shared.yaml`, else `.nyx-shared.json`
8. the `preset`
9. built-in defaults

Prefer one YAML file (`.nyx.yaml`) plus environment variables for secrets and per-environment values. The shared files are meant for organization-wide baselines.

**Items are inherited or overridden as a whole, never merged attribute by attribute.** Collections (`releaseTypes.items`, `commitMessageConventions.items`, `substitutions.items`, `services`) are resolved one item name at a time, so a file can add a release type and still inherit the preset's other ones. But when you redefine one item, such as `releaseTypes.items.mainline`, you must write every attribute it needs, including the ones the preset used to provide. The `enabled`, `publicationServices` and `remoteRepositories` lists are also replaced as a whole.

## File layout

Some sections hold an `enabled` list plus an `items` map. Others are plain maps keyed by name. Using the wrong shape is **silently ignored**. The layouts below were verified against the Nyx binary.

```yaml
preset: "simple"                      # or "extended"
releasePrefix: "v"                    # empty by default
initialVersion: "0.1.0"               # used when no previous release tag is found
scheme: "SEMVER"                      # the only scheme available
stateFile: "build/.nyx-state.json"    # .json or .yaml; keep it Git-ignored
summaryFile: "build/.nyx-summary.txt"
resume: true                          # reuse the state file across separate runs

commitMessageConventions:             # enabled + items
  enabled: [ "conventionalCommits" ]
  items:
    conventionalCommits:
      expression: "<regex with named groups type, scope, title>"
      bumpExpressions:
        major: "<regex>"
        minor: "<regex>"
        patch: "<regex>"

releaseTypes:                         # enabled + items, plus two lists
  enabled: [ "mainline", "internal" ] # ORDER MATTERS: the first matching type wins
  publicationServices: [ "github" ]   # names of entries in 'services'
  remoteRepositories: [ "origin" ]    # Git remotes to push to; default: origin
  items:
    mainline: { ... }                 # see "Release types" below

substitutions:                        # enabled + items
  enabled: [ "node_version" ]
  items:
    node_version:
      files: "**/package.json"
      match: "<regex>"
      replace: "{{version}}"

changelog:                            # plain options
  path: "build/CHANGELOG.md"          # setting a path enables the changelog
  template: "config/CHANGELOG.tpl"    # optional Handlebars template
  sections:                           # section title -> regex on the commit 'type'
    "Added": "^feat$"
    "Fixed": "^fix$"
  substitutions:                      # regex -> format string; every %s gets capture group 1
    "(?m)#([0-9]+)(?s)": "[#%s](https://github.com/OWNER/REPO/issues/%s)"

services:                             # plain map keyed by service name (NO 'items')
  github:
    type: "GITHUB"                    # GITHUB or GITLAB
    options:
      AUTHENTICATION_TOKEN: "{{#environmentVariable}}GH_TOKEN{{/environmentVariable}}"
      REPOSITORY_OWNER: "OWNER"
      REPOSITORY_NAME: "REPO"
      # BASE_URI: only for self-hosted instances

git:
  remotes:                            # plain map keyed by remote name (NO 'items')
    origin:                           # GitHub: the token as both user and password
      authenticationMethod: "USER_PASSWORD"   # or PUBLIC_KEY (SSH remote URLs, keys from ~/.ssh)
      user: "{{#environmentVariable}}GH_TOKEN{{/environmentVariable}}"
      password: "{{#environmentVariable}}GH_TOKEN{{/environmentVariable}}"
      # GitLab: user "PRIVATE-TOKEN" and the token as the password

releaseAssets:                        # plain map keyed by asset name (NO 'items')
  binary:
    fileName: "app-{{version}}.tar.gz"
    description: "App {{version}}"
    path: "build/dist/app.tar.gz"     # local file (uploaded) or URL (linked)
    type: "application/gzip"
```

Some pages of the configuration reference show `services`, `git/remotes` and `releaseAssets` paths with an `items` level. The binary ignores that level for these three sections, so use the plain maps shown above.

### Command line and environment variable names

These names are derived from the configuration path:

- Command line: mostly kebab case with `-` between levels: `--release-types-mainline-git-tag=true`, `--services-github-options-REPOSITORY_NAME=repo`. A few options keep camel case: `--git-remotes-<NAME>-authenticationMethod`, `--git-remotes-<NAME>-privateKey`, `--release-assets-<NAME>-fileName`, `--commit-message-conventions-<NAME>-bumpExpressions-<ID>`. Check `nyx --help` when unsure.
- Item names used in command line options must be alphanumeric (`[a-zA-Z0-9]+`), so items like `node_version` can only be configured in files or environment variables.
- Environment variables: `NYX_` plus upper snake case: `NYX_RELEASE_TYPES_MAINLINE_GIT_TAG=true`. A user-chosen name keeps its case: `NYX_SERVICES_github_TYPE=GITHUB`.
- List items use numeric ordinals: `--release-types-internal-identifiers-0-qualifier=build`.
- Boolean flags may omit the value: `--dry-run` means `--dry-run=true`.
- Unknown names are ignored without errors.

## Presets

| Preset | Conventions | Release types (in order) | Other |
| ------ | ----------- | ------------------------ | ----- |
| `simple` | `conventionalCommits` | `mainline` (`main`/`master`, clean workspace, `gitTag`, `gitPush` and `publish` enabled, but see the tagging note below) and `internal` (any branch, collapsed `internal` pre-release with a timestamp build identifier, no tag, no push) | Nothing else |
| `extended` | `conventionalCommits` and `gitmoji` enabled; `conventionalCommitsForMerge` defined but disabled | `mainline`, `integration` (`develop`, `development`, `integration`, `latest`), `maturity` (`alpha`, `beta`, ... `omega`), `feature` (`feat[-/]x`, `feature[-/]x`), `fix` (`fix[-/]x`), `hotfix` (`hotfix[-/]x`), `release` (`rel[-/]1.2`, `release[-/]1.x`), `maintenance` (`1.x`, `v2.3.x`), `internal` | Changelog at `CHANGELOG.md`, `github` and `gitlab` services reading `GITHUB_TOKEN` and `GITLAB_TOKEN`, version substitutions for common manifests (all disabled) |

Neither preset lists any `publicationServices`, so nothing is published until you set that list.

In Nyx 3.1.x the preset release types have an empty `gitTagNames` list, so `mark` creates **no tag** for them. Redefine every release type that must tag in your configuration file. A user-defined item gets the default `gitTagNames: [ "{{version}}" ]`, as in the [verified example](#verified-example).

## Release types

Nyx evaluates the types in `releaseTypes.enabled` in order and uses the **first** one whose `matchBranches`, `matchEnvironmentVariables` and `matchWorkspaceStatus` all match. When none matches, Nyx fails with "no suitable release types have been configured or none of the configured release types matches the current environment". Keep a catch-all type (no match conditions, like `internal`) last.

Attributes, with defaults for a type that does not set them:

| Attribute | Default | Meaning |
| --------- | ------- | ------- |
| `matchBranches` | empty (any) | Regex on the current branch name |
| `matchEnvironmentVariables` | empty | Map of variable name to regex. All must match, i.e. `CI: "^true$"` to release only from CI |
| `matchWorkspaceStatus` | empty (any) | `CLEAN` or `DIRTY` |
| `filterTags` | empty (all) | Template rendering to a regex. Only matching tags count as previous releases. It must also accept the tags this type produces, including pre-release and extra identifiers |
| `collapseVersions` | `false` | Pre-release versioning: all commits on the branch converge to one next version, i.e. `1.2.0-alpha.1`, `1.2.0-alpha.2` |
| `collapsedVersionQualifier` | empty | Template for the pre-release qualifier, i.e. `{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}` |
| `identifiers` | empty | List of `{qualifier, value, position: PRE_RELEASE or BUILD}` extra identifiers |
| `versionRange` | empty | Regex the version must match or the release fails. Protects release and maintenance branches |
| `versionRangeFromBranchName` | `false` | Derives the range from branch names like `release-1.2` or `1.x` |
| `gitCommit` | `false` | Commit changes made by `make` (changelog, substitutions) |
| `gitCommitMessage` | `Release version {{version}}` | |
| `gitTag` | `false` | Create the release tag |
| `gitTagNames` | `[ "{{version}}" ]` | Tag names; add e.g. `latest` along with `gitTagForce: "true"` to move it |
| `gitTagMessage` | empty | Non-empty creates an annotated tag |
| `gitPush` | `false` | Push the commit and tags to `remoteRepositories` |
| `publish` | `false` | Publish to `publicationServices` |
| `publishPreRelease` | `false` | Mark the published release as a pre-release |
| `publishDraft` | `false` | Publish as a draft |
| `description` | `Release {{version}}` | Release notes. Use `{{#fileContent}}build/CHANGELOG.md{{/fileContent}}` to publish the changelog |
| `assets` | all assets | Names from `releaseAssets` to attach. Omit it to attach every asset |

Boolean-like attributes that accept templates (`gitTag`, `gitPush`, `gitCommit`, `publish`, `publishPreRelease`, `publishDraft`) are strings. Unquoted YAML booleans also work, but quoting them is the convention used by the presets and the documentation.

## Mapping a branching model

Pick only the release types the user's branches need. Each type must have a `filterTags` regex that accepts the tags it creates. Shapes that work:

- **Mainline only** (trunk based): the `simple` preset with the `mainline` item redefined in full (needed for tagging in 3.1.x, see above). Add `matchEnvironmentVariables: { CI: "^true$" }` to it if only CI may release.
- **Mainline plus pre-release branches**: add types with `collapseVersions: true` and a branch-derived `collapsedVersionQualifier`. Tag and push them when users consume the pre-releases, and set `publishPreRelease: "true"` if they are published.
- **Release and maintenance branches**: use `versionRangeFromBranchName: true` so a `release-1.2` or `1.x` branch can never issue versions outside its line.
- **Feature branches**: usually no tag or push, just a collapsed version for build artifacts.
- **Gitflow-like**: start from the `extended` preset and trim `releaseTypes.enabled` to the types the user actually has.

Read <https://mooltiverse.github.io/nyx/docs/user/best-practice/branching-models> for complete models.

## Verified example

Mainline releases only from a clean CI workspace with a `v` prefix, a changelog used as the GitHub release notes, collapsed pre-release versions on feature branches, and a catch-all for everything else:

```yaml
preset: "simple"
releasePrefix: "v"
stateFile: "build/.nyx-state.json"   # build/ is Git-ignored
resume: true
changelog:
  path: "build/CHANGELOG.md"
  sections:
    "Added": "^feat$"
    "Fixed": "^fix$"
releaseTypes:
  enabled: [ "mainline", "feature", "internal" ]
  publicationServices: [ "github" ]
  items:
    mainline:
      description: "{{#fileContent}}build/CHANGELOG.md{{/fileContent}}"
      filterTags: "^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$"
      gitPush: "true"
      gitTag: "true"
      matchBranches: "^(master|main)$"
      matchEnvironmentVariables:
        CI: "^true$"
      matchWorkspaceStatus: "CLEAN"
      publish: "true"
    feature:
      collapseVersions: true
      collapsedVersionQualifier: "{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}"
      filterTags: "^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-([0-9a-z]+)(\\.([0-9]\\d*))?)?$"
      gitPush: "false"
      gitTag: "false"
      matchBranches: "^(feat|feature)((-|\\/)[0-9a-zA-Z-_]+)?$"
      publish: "false"
    internal:
      collapseVersions: true
      collapsedVersionQualifier: "internal"
      identifiers:
        - qualifier: "{{#short7}}{{releaseScope.finalCommit}}{{/short7}}"
          position: "BUILD"
      gitPush: "false"
      gitTag: "false"
      publish: "false"
services:
  github:
    type: "GITHUB"
    options:
      AUTHENTICATION_TOKEN: "{{#environmentVariable}}GH_TOKEN{{/environmentVariable}}"
      REPOSITORY_OWNER: "OWNER"
      REPOSITORY_NAME: "REPO"
```

The outcomes of this configuration after a `feat` commit on top of `v1.0.0`:

- On `main` in CI: `v1.1.0`, tagged, pushed and published.
- On `main` locally: `v1.1.0-internal.1+<sha>`, nothing tagged.
- On `feature/login`: `v1.1.0-featurelogin.1`.

## Templates

Many options accept [Handlebars](https://handlebarsjs.com/) templates. State values are available by path (`{{version}}`, `{{branch}}`, `{{releaseScope.previousVersion}}`, `{{configuration.releasePrefix}}`). Useful functions, used as `{{#fn}}arg{{/fn}}`:

- Strings: `lower`, `upper`, `trim`, `first`, `last`, `sanitize`, `sanitizeLower`, `replace`, `capture`, `cutLeft`, `cutRight`
- Hashes: `short5`, `short6`, `short7`
- Time: `timestampISO8601`, `timestampYYYYMMDDHHMMSS`, `timeFormat`
- Environment: `environmentVariable`, `environmentUser`
- Files: `fileContent`, `fileExists`

Full list: <https://mooltiverse.github.io/nyx/docs/user/configuration-reference/templates>.

## Checking a configuration

```sh
nyx --resume=false --dry-run --summary --state-file=build/.nyx-state.json infer
```

Then confirm that:

- `configuration.releaseTypes`, `configuration.services` and the other sections contain what you wrote. A missing value usually means a wrong layout or a misspelled key. Items not listed in their section's `enabled` list are left out of `configuration` on purpose.
- `releaseType` matches the attributes of the type you expected for the current branch and environment. The state does not record the type's name, so compare `matchBranches` and the other attributes.
- `version`, `newVersion` and `newRelease` are what the user expects.

To check other branches or CI conditions without switching branches, simulate them with environment variables (`CI=true nyx ...`). For the branch itself, run on a throwaway branch or clone.
