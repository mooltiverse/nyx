---
name: nyx
description: >
  Configure, troubleshoot and safely run Nyx, the semantic release tool that infers versions from the Git commit history, generates changelogs, tags, pushes and publishes releases to GitHub or GitLab.
  Use when a repository has a .nyx.yaml, .nyx.json, .nyx-shared.* file, a com.mooltiverse.oss.nyx Gradle plugin or a mooltiverse/nyx GitHub Action, or when the user asks to set up automatic semantic versioning or releases, map a branching model to release types, explain why Nyx inferred a certain version, write commit messages that bump the version as intended, wire Nyx into CI/CD, or migrate from semantic-release, release-please or standard-version.
license: Apache-2.0
compatibility: Requires Git and the Nyx command line tool (or Docker, the Gradle plugin or the GitHub Action) to run Nyx; reasoning about configurations works without it.
metadata:
  author: Mooltiverse
  homepage: https://mooltiverse.github.io/nyx/
---

# Nyx

Nyx reads the Git commit history, infers the next version from commit messages, then optionally generates a changelog, commits, tags, pushes and publishes a release. Users drive it through the CLI (`nyx`), the Docker image (`mooltiverse/nyx`), the Gradle plugin (`com.mooltiverse.oss.nyx`) or the GitHub Action (`mooltiverse/nyx@main`). All flavors share the same configuration and state model.

## Commands

| Command | Effect | Changes anything? |
| ------- | ------ | ----------------- |
| `infer` (default) | Computes the version and the state | No, read only |
| `make` | Runs `infer`, then applies substitutions and renders the changelog | Local files only |
| `mark` | Runs `make`, then commits, tags and pushes as the matched release type says | **Yes**: local and remote Git |
| `publish` | Runs `mark`, then publishes the release to the configured services | **Yes**: remote services |
| `clean` | Deletes files Nyx created (state, summary, changelog) | Local files only |

Commands chain: running `publish` runs everything before it. Gradle tasks are `nyxInfer`, `nyxMake`, `nyxMark`, `nyxPublish`, `nyxClean`, plus the `release` lifecycle task.

## Safety rules

- **Never run `mark` or `publish` (or `nyxMark`, `nyxPublish`, `release`) without the user's explicit confirmation.** They create tags, push to remotes and publish releases, which are hard to undo.
- Before any of them, run the same command with `--dry-run` and show the user what would happen.
- Prefer `infer` with a state file for everything else: it never changes the repository.
- **Never put tokens or passwords on the command line or in committed configuration files.** Command lines leak through process lists, shell history, CI logs and agent transcripts. Read secrets from environment variables with a template, i.e. `AUTHENTICATION_TOKEN: "{{#environmentVariable}}GH_TOKEN{{/environmentVariable}}"`.
- Write state and summary files under a Git-ignored directory such as `build/`. An untracked state file makes the workspace dirty, which changes the matched release type on the next run (see the gotchas).

## Inspecting what Nyx does

Always look at real output before reasoning about versions:

```sh
nyx --dry-run --summary --state-file=build/.nyx-state.json infer
```

- `--summary` prints the version, previous and prime versions, bump, and the new version/new release flags.
- The state file (`.json` or `.yaml` by extension) has everything else. Key attributes: `version`, `bump`, `newVersion`, `newRelease` (`newVersion` and the matched type has `publish` enabled), `coreVersion`, `branch`, `releaseType` (the matched type's attributes, **without its name**), `releaseScope` (`previousVersion`, `primeVersion`, `commits`, `significantCommits`, `initialCommit`, `finalCommit`), and `configuration` (the **fully resolved** configuration after merging all sources and the preset).
- Use `configuration` in the state file to check what Nyx actually loaded. Do not trust the files you wrote until you have checked it. Items that are not listed in their section's `enabled` list (release types, conventions, substitutions) are left out of `configuration`, so a missing item may just be disabled.

If `nyx` is not installed, use `docker run --rm -v "$PWD":/project mooltiverse/nyx:latest <args>`, or `./gradlew nyxInfer` with `stateFile` set when the Gradle plugin is applied.

## Workflows

Pick the workflow that matches the request and read the referenced file before acting.

1. **Set up or change the configuration**: read [references/configuration.md](references/configuration.md).
   1. Ask for, or infer from the repository, the branching model, which branches produce official releases and pre-releases, the tag prefix, whether to generate a changelog, and where to publish.
   2. Start from the `simple` preset (mainline only) or the `extended` preset (many branch types) and override as little as possible.
   3. Write `.nyx.yaml` (or the Gradle `nyx { }` block when the user prefers it).
   4. Check it with `infer` as shown above: confirm `configuration` contains what you wrote, and that `version` and `releaseType` are what the user expects on the current branch.
2. **Explain or fix an unexpected version**: read [references/troubleshooting.md](references/troubleshooting.md). Run `infer` with a state file, then explain the result from `releaseScope`, `bump` and `releaseType` instead of guessing.
3. **Write commit messages**: read the configured `commitMessageConventions` from the state file's `configuration`. Do not assume Conventional Commits. Pick a message whose bump expression yields the bump the change deserves, and tell the user which identifier it will bump. With Conventional Commits: `feat` bumps minor, `fix` bumps patch, `!` after the type or a `BREAKING CHANGE:` footer bumps major. Other types are not significant, so they produce no new version.
4. **Set up CI/CD, Gradle or migrate from another tool**: read [references/ci-cd.md](references/ci-cd.md).
5. **Release**: run the dry run first, show the planned version, tag and publication, ask for confirmation, then run the real command.

## Gotchas

- **Shallow clones** (CI default) hide tags and history, so the version falls back to the initial version (`0.1.0`) or one bump above it. Fetch the full history (`fetch-depth: 0` on GitHub Actions, `GIT_DEPTH: "0"` and `GIT_STRATEGY: clone` on GitLab CI).
- **Unknown arguments and options are silently ignored**, because many names are dynamic (`--services-<NAME>-type`). A typo produces no error, just default behavior. Always check the resolved `configuration` in the state file.
- **Items are overridden as a whole.** Collections such as `releaseTypes.items`, `commitMessageConventions.items` and `services` are merged by item name across sources. But a single item you redefine, such as `releaseTypes.items.mainline`, replaces the inherited one completely, and so does an `enabled`, `publicationServices` or `remoteRepositories` list. An overridden `mainline` that only sets `matchBranches` loses `gitTag`, `gitPush` and `publish` from the preset, and those default to `false`. Copy every attribute you want to keep.
- **Preset release types don't tag with the command line tool, the Docker image or the GitHub Action (Nyx 2.5.0 to 3.1.x; the Gradle plugin is not affected).** They come with an empty `gitTagNames` list, so `mark` creates no tag even with `gitTag: "true"`. Redefine the release types that must tag (i.e. `mainline`) in the configuration file with all their attributes. A user-defined item gets the default `gitTagNames: [ "{{version}}" ]`. Check after the first real release that the tag exists locally and on the remote.
- **The workspace status matters.** The presets' `mainline` type requires `matchWorkspaceStatus: CLEAN`. Any uncommitted or untracked file, including a state file outside a Git-ignored directory, makes Nyx fall back to `internal`, which produces a pre-release such as `1.1.0-internal.1+timestamp.20260101120000` and no release.
- **The release prefix is part of the version and of tag matching**: with `releasePrefix: v` the version is `v1.2.0`. The presets' release types set `filterTags` to `^({{configuration.releasePrefix}})?<major>.<minor>.<patch>...`, so with the `simple` or `extended` preset and no `releasePrefix`, existing `v1.2.0` tags are **ignored** and the version restarts from the initial version. Set `releasePrefix` to match the existing tags. `releaseLenient` only tolerates prefixes when no `filterTags` applies.
- **Merge commits**: Nyx follows the first parent only, so commits on a merged branch are not scanned. After a `--no-ff` merge only the merge commit's own message counts: a default `Merge branch 'x'` message bumps nothing even if the branch had `feat` commits. Fast-forward and rebase merges keep the individual commits. Squash merges count only the squash commit's message. Ask for conventional merge or squash messages, or use the `extended` preset's `conventionalCommitsForMerge` convention.
- **No new version** means no significant commits since the last release tag on the current branch. `mark` and `publish` then do nothing. That is correct behavior, not a failure. When there is a new version, `mark` commits, tags and pushes according to the matched type even if `newRelease` is `false`, because `newRelease` only reflects `publish`.
- **Publishing needs both a service and its name in `releaseTypes.publicationServices`**, plus `publish: "true"` on the matched release type. The presets enable `publish` but list no publication service.
- **Gradle**: apply the plugin in `settings.gradle` (a settings plugin) so `version` is set during initialization. Otherwise `version` is `unspecified` until `nyxInfer` runs, and subprojects must use `rootProject.version`.

## Documentation

The reference files cover what is needed most often. For anything else, use the official documentation at <https://mooltiverse.github.io/nyx/>, mainly:

- Configuration reference: <https://mooltiverse.github.io/nyx/docs/user/configuration-reference/>
- State reference: <https://mooltiverse.github.io/nyx/docs/user/state-reference/>
- Presets: <https://mooltiverse.github.io/nyx/docs/user/configuration-presets/>
- Branching models: <https://mooltiverse.github.io/nyx/docs/user/best-practice/branching-models>
- `nyx --help` lists every command line option of the installed version.
