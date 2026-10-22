# Explaining and fixing versions

## Gather the facts first

```sh
git status --porcelain                        # anything listed makes the workspace DIRTY
git branch --show-current
git rev-parse --is-shallow-repository         # 'true' means history is missing
git log --first-parent --oneline -20          # the commits Nyx actually scans
git tag --merged HEAD --sort=-creatordate | head
nyx --resume=false --dry-run --summary --state-file=build/.nyx-state.json infer
```

Use `--resume=false` while diagnosing. With `resume: true`, Nyx reuses a previous state file when the repository hasn't changed, even if environment variables changed, so you would be looking at a stale result.

Then read from the state file:

| Attribute | What it tells you |
| --------- | ----------------- |
| `releaseScope.previousVersion` and `previousVersionCommit.tags` | The last release tag Nyx recognized. When no tag was found, `previousVersion` equals `initialVersion` (i.e. `0.1.0`) and `previousVersionCommit` is absent |
| `releaseScope.primeVersion` | The last *core* (non pre-release) version. It is the baseline for collapsed pre-releases |
| `releaseScope.commits` | Every commit in the scope, from the newest to the one after the previous release |
| `releaseScope.significantCommits` | The commits that matched a bump expression. Empty means no new version |
| `bump` | The identifier that was bumped (`major`, `minor`, `patch`, or a pre-release qualifier) |
| `releaseType` | Attributes of the matched release type. Compare them with `configuration.releaseTypes.items` to identify it |
| `newVersion` / `newRelease` | Whether the version changed (if so, `mark` commits, tags and pushes as the matched type says), and whether it would also be published (`newVersion` and `publish` enabled) |
| `configuration` | The resolved configuration Nyx actually used |

Explain the outcome to the user in these terms: "the previous release is X (tag T on commit C), the scope has N commits of which M are significant (list them with the bump each one triggers), and the matched release type is R because of branch, environment or workspace status, so the version is V".

## Symptoms

| Symptom | Likely cause | Check | Fix |
| ------- | ------------ | ----- | --- |
| Version is `0.1.0`, or one bump above it, on CI but correct locally | Shallow clone without tags | `git rev-parse --is-shallow-repository` | Full history: `fetch-depth: 0` (GitHub Actions), `GIT_DEPTH: "0"` and `GIT_STRATEGY: clone` (GitLab CI), `git fetch --unshallow --tags` elsewhere |
| Existing `vX.Y.Z` tags are ignored and the version restarts | A preset's `filterTags` includes `{{configuration.releasePrefix}}`, which is empty | `previousVersion` is the initial version and `previousVersionCommit` is absent | Set `releasePrefix: "v"` |
| `mark` or `publish` ran but no tag was created | The matched type comes from a preset, whose `gitTagNames` list is empty in the command line tool, Docker image and GitHub Action from 2.5.0 to 3.1.x | `releaseType.gitTagNames` is `[]` | Redefine the release type in the configuration file with all its attributes, so it gets the default `gitTagNames: [ "{{version}}" ]` |
| Version like `1.3.0-internal.1+...` on `main` | The workspace is dirty, `CI` is not set, or the branch doesn't match `mainline` | `git status --porcelain`; compare `releaseType` with the `mainline` item | Commit or ignore the files (state and summary files under a Git-ignored `build/`), run in CI, or fix `matchBranches` |
| No new version although commits were added | No significant commits: wrong types (`chore`, `docs`), non-conventional messages, or merged commits hidden behind a merge commit | `significantCommits` is empty; `git log --first-parent` | Use conventional messages (`feat`, `fix`, `!`, `BREAKING CHANGE:`), conventional merge or squash messages, or enable `conventionalCommitsForMerge` (`extended` preset) |
| A `feat` from a merged branch didn't bump minor | Nyx follows only the first parent of merge commits | The merge commit's message in `git log --first-parent` | Same as above |
| Unexpected `major` bump | `!` after the type, or a `BREAKING CHANGE:`/`BREAKING-CHANGE:` footer anywhere in the body | `significantCommits[*].message.fullMessage` | Amend if not yet pushed; otherwise accept it or tag manually |
| "no suitable release types have been configured or none of the configured release types matches the current environment" | No enabled type matches the branch, environment or workspace | `configuration.releaseTypes` | Add a catch-all type (no match conditions) at the end of `enabled` |
| Option has no effect | Misspelled name or wrong file layout (unknown names are ignored), or overridden by a higher-priority source | Look for it in `configuration` | Fix the key or layout; check the sources in priority order (command line, environment, Gradle, files, preset) |
| An overridden preset release type stopped tagging or publishing | The item was replaced as a whole and lost `gitTag`, `gitPush` and `publish` | `releaseType` in the state | Repeat every attribute in the override |
| Nothing is published although `publish` is `"true"` | `releaseTypes.publicationServices` is empty, the service isn't defined, or there is no new release | `configuration.releaseTypes.publicationServices`, `newRelease` | Define the service and list it |
| Authentication errors on push or publish | Token missing from the environment, or wrong user/password mapping | Whether the variable is set (never print its value) | GitHub: token as both user and password; GitLab: user `PRIVATE-TOKEN`, token as the password |
| Gradle `version` is `unspecified` | The plugin is applied as a project plugin and the task runs before `nyxInfer`, or a subproject reads its own `version` | Where the plugin is applied | Apply it in `settings.gradle`, or make tasks depend on `:nyxInfer` and read `rootProject.version` |
| `versionRange` error on a release branch | The inferred version is outside the branch's line (i.e. a `feat` on `release-1.2` would produce `1.3.0`) | `versionRange` in `releaseType` | Use only `fix` commits there, or cut a new release branch |

## Concepts to explain correctly

- **Version vs release**: Nyx always computes a version. Git actions (commit, tag, push) happen when `newVersion` is true and the matched type enables them. Publication happens when `newRelease` is true, which means `newVersion` plus `publish` enabled.
- **Previous vs prime version**: the previous version is the latest release tag found, which may be a pre-release. The prime version is the latest core release. Collapsed pre-releases bump from the prime version and only increment the pre-release counter, so many commits on `alpha` produce `2.0.0-alpha.1`, `2.0.0-alpha.2`... instead of jumping several major versions.
- **Significance**: a commit is significant only if its message matches a bump expression of an enabled convention. Unmatched commits are in the scope but don't change the version.
