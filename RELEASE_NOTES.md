# Nyx Release Notes

## 3.1.3

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/3.1.3).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

This release:

* adds the GitHub and GitLab services to the extended preset ([352](https://github.com/mooltiverse/nyx/issues/352))
* upgrades all the Go libraries to fix the latest security alerts

## 3.1.2

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/3.1.2).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

This release:

* adds support for Gradle `8.12`

## 3.1.1

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/3.1.1).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

This release:

* fixes a [vulnerability issue](https://github.com/advisories/GHSA-r9px-m959-cxf4) in the go-git library
* upgrades Go to version 1.23.4

## 3.1.0

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/3.1.0).

### Upgrade instructions

To replace the former [GitHub Action](https://github.com/mooltiverse/nyx-github-action) with the new one replace all occurrences of `mooltiverse/nyx-github-action` in your GitHub workflows with `mooltiverse/nyx`.

The new action is a drop-in replacement and does not require any other change on existing workflows.

### New features and improvements

This release:

* brings the former [GitHub Action](https://github.com/mooltiverse/nyx-github-action) into the main Nyx project [324](https://github.com/mooltiverse/nyx/issues/324)
* adds support for Gradle `8.10.2`, `8.11`, `8.11.1`
* upgraded all libraries
* replaced the former and deprecated `github.com/xanzy/go-gitlab` Go library with `gitlab.com/gitlab-org/api/client-go`
* fixes the [338](https://github.com/mooltiverse/nyx/issues/338) bug and allows paths other than globs to specify files to apply substitutions to
* fixes the [339](https://github.com/mooltiverse/nyx/issues/339) bug which was too restrictive on the commit scope field in Conventional Commits messages
* fixes the [311](https://github.com/mooltiverse/nyx/issues/311) documentation bug

## 3.0.12

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/3.0.12).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

This release:

* applied conventions and styling rules
  * the `modules` directory has been renamed to to `src`
  * GitHub Actions workflows have been refactored for reusability
* the Go packages layout has changed: the `github.com/mooltiverse/nyx/src/go/main` package has been merged into `github.com/mooltiverse/nyx/src/go`, while the `Nyx` main structure has been moved into `github.com/mooltiverse/nyx/src/go/nyx`
* dismissed SonarCloud and Codecov
* converted the documentation microsite from Jekyll to Docusaurus

## 3.0.11

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/3.0.11).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

This release:

* fixes the mismatch between Go and Java on the Git entity serialization ([#332](https://github.com/mooltiverse/nyx/issues/332))

## 3.0.10

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/3.0.10).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

This release:

* fixes the mismatch between Go and Java on the Git entity serialization ([#332](https://github.com/mooltiverse/nyx/issues/332))

## 3.0.9

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/3.0.9).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

This release:

* fixes the mismatch between Go and Java on the Git entity serialization ([#332](https://github.com/mooltiverse/nyx/issues/332))

## 3.0.8

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/3.0.8).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

This release:

* fixes the representation of commit dates in Java ([#330](https://github.com/mooltiverse/nyx/issues/330))

## 3.0.7

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/3.0.7).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

This release:

* adds support for Gradle `8.10.1`.

## 3.0.6

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/3.0.6).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

This release:

* upgrades all Go libraries to the latest version

## 3.0.5

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/3.0.5).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

This release:

* adds support for Gradle `8.9`.

## 3.0.4

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/3.0.4).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

This release:

* upgrades Go to version 1.22.4
* upgrades all Go libraries to the latest version

## 3.0.3

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/3.0.3).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

This release:

* adds support for Gradle `7.6.4`, `8.7` and `8.8`.

## 3.0.2

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/3.0.2).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

This release:

* applies security fixed in Go binaries and upgrades dependencies

## 3.0.1

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/3.0.1).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

This release:

* fixes a bug that allowed to publish a new release without tagging and pushing, resulting in a remote error ([#300](https://github.com/mooltiverse/nyx/issues/300))
* fixes the Java source and compatibility version to Java 17 ([#302](https://github.com/mooltiverse/nyx/issues/302))
* upgrades the [Jackson](https://github.com/FasterXML/jackson) Java library to version 2.16.0
* upgrades Go packages and fixes the following dependabot alerts: [4](https://github.com/mooltiverse/nyx/security/dependabot/4), [5](https://github.com/mooltiverse/nyx/security/dependabot/5), [6](https://github.com/mooltiverse/nyx/security/dependabot/6), [7](https://github.com/mooltiverse/nyx/security/dependabot/7), [8](https://github.com/mooltiverse/nyx/security/dependabot/8), [9](https://github.com/mooltiverse/nyx/security/dependabot/9), [10](https://github.com/mooltiverse/nyx/security/dependabot/10), [11](https://github.com/mooltiverse/nyx/security/dependabot/11), [12](https://github.com/mooltiverse/nyx/security/dependabot/12), [13](https://github.com/mooltiverse/nyx/security/dependabot/13), [14](https://github.com/mooltiverse/nyx/security/dependabot/14), [15](https://github.com/mooltiverse/nyx/security/dependabot/15)
* adds the description of the `--summary` and `--summary-file` arguments in the `--help` output ([#292](https://github.com/mooltiverse/nyx/issues/292))
* fixes a misleading attribute in the configuration examples ([#301](https://github.com/mooltiverse/nyx/issues/301))

### Fixed issues

There are no fixes in this release.

## 3.0.0

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/3.0.0).

### Upgrade instructions

If using Gradle you need to upgrade to Java version 17 or later. Java versions older than 17 are no longer supported.

### New features and improvements

This release:

* drops support for Java versions older than `17`.
* adds support for Gradle `7.6.3`, `8.4`, `8.5` and `8.6`.

### Fixed issues

There are no fixes in this release.

## 2.5.1

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/2.5.1).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

This release:

* adds support for commit message conventions matching multiple portions of commit messages (yielding to multiple bump identifiers per commit), useful for merge commits ([#125](https://github.com/mooltiverse/nyx/issues/125))
* improves regular expressions used for the Conventional Commits convention
* adds the unofficial `conventionalCommitsForMerge` convention (disabled by default) to the [extended preset](https://mooltiverse.github.io/nyx/docs/user/configuration-presets/extended), useful for merge commits ([#125](https://github.com/mooltiverse/nyx/issues/125))

### Fixed issues

This release:

* fixes a bug in changelog templates where commit SHA was not rendered properly ([#243](https://github.com/mooltiverse/nyx/issues/259))

## 2.5.0

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/2.5.0).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

This release:

* adds support for arbitrary [substitutions](https://mooltiverse.github.io/nyx/docs/user/configuration-reference/substitutions/) in text files within the project using dynamic values from the [State](https://mooltiverse.github.io/nyx/docs/user/state-reference/) ([#182](https://github.com/mooltiverse/nyx/issues/182)) ([#223](https://github.com/mooltiverse/nyx/issues/223))
* adds support for multiple Git tag [names](https://mooltiverse.github.io/nyx/docs/user/configuration-reference/release-types/#git-tag-names) that can be used to change the default git tag or add some tag aliases ([#243](https://github.com/mooltiverse/nyx/issues/243))
* adds support for the new [`gitPushForce`](https://mooltiverse.github.io/nyx/docs/user/configuration-reference/release-types/#git-push-force), [`gitTagForce`](https://mooltiverse.github.io/nyx/docs/user/configuration-reference/release-types/#git-tag-force) flags to control whether Git push and tag operations need to use the `--force` flag, useful when using tag aliases in the Git tag [names](https://mooltiverse.github.io/nyx/docs/user/configuration-reference/release-types/#git-tag-names) ([#243](https://github.com/mooltiverse/nyx/issues/243))
* adds support for the new `versionMajorNumber`, `versionMinorNumber`, `versionPatchNumber`, `versionPreReleaseIdentifier`, `versionBuildMetadata` [state attributes](https://mooltiverse.github.io/nyx/docs/user/state-reference/global-attributes)
* adds support for the new [`releaseName`](https://mooltiverse.github.io/nyx/docs/user/configuration-reference/release-types/#release-name) configuration option to control the name of releases published to remote services ([#238](https://github.com/mooltiverse/nyx/issues/238))
* adds support for the new [`publishDraft`](https://mooltiverse.github.io/nyx/docs/user/configuration-reference/release-types/#publish-draft) and [`publishPreRelease`](https://mooltiverse.github.io/nyx/docs/user/configuration-reference/release-types/#publish-pre-release) configuration options to control extra flags of releases published to remote services (as of now only [supported by GitHub](https://docs.github.com/en/repositories/releasing-projects-on-github/managing-releases-in-a-repository)) ([#238](https://github.com/mooltiverse/nyx/issues/238))
* adds support for the new [`append`](https://mooltiverse.github.io/nyx/docs/user/configuration-reference/changelog/#append) configuration option to control whether the [changelog](https://mooltiverse.github.io/nyx/docs/user/configuration-reference/changelog/) file is overwritten or appended and, if appended, in which order ([#183](https://github.com/mooltiverse/nyx/issues/183))
* adds support for Git tag updates so existing tags can be replaced and overwritten ([#243](https://github.com/mooltiverse/nyx/issues/243))
* adds support for Gradle `8.2.1` and `8.3`.

### Fixed issues

There are no fixes in this release.

## 2.4.7

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/2.4.7).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

There are no new features or improvements in this release.

### Fixed issues

This release:

* fixes a typo Java Gradle interfaces ([#240](https://github.com/mooltiverse/nyx/issues/240))

## 2.4.6

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/2.4.6).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

There are no new features or improvements in this release.

### Fixed issues

This release:

* fixes a bug in the Java version ([#236](https://github.com/mooltiverse/nyx/issues/236)) due to a misalignment of Gradle logger API

## 2.4.5

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/2.4.5).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

There are no new features or improvements in this release.

### Fixed issues

This release:

* fixes a bug in the Java version ([#236](https://github.com/mooltiverse/nyx/issues/236)) due to a misalignment of Gradle logger API
* updates Java dependencies and adds support for Gradle `8.2`.

## 2.4.4

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/2.4.4).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

There are no new features or improvements in this release.

### Fixed issues

This release:

* fixes a bug in the command line version that does not comply with `.gitignore` ([#219](https://github.com/mooltiverse/nyx/issues/219)). The issue is inherited by [go-git](https://github.com/go-git/go-git) (see [here](https://github.com/go-git/go-git/issues/597) for an example). The issue is worked around by programmatically adding each pattern from the `.gitignore` to the Git worktree excludes. Only the `.gitignore` file in the repo root directory is read
* fixes a bug in the command line version that does not allow to disable the generation of the state file ([#230](https://github.com/mooltiverse/nyx/issues/230))

## 2.4.3

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/2.4.3).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

There are no new features or improvements in this release.

### Fixed issues

This release:

* fixes a bug in the command line version that caused a segment violation error when running the `infer` matching a release type that doesn't define the `collapsedVersioning` attribute ([#225](https://github.com/mooltiverse/nyx/issues/225))

## 2.4.1

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/2.4.1).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

There are no new features or improvements in this release.

### Fixed issues

This release:

* fixes a bug in the command line version that caused a segment violation error when running the `infer` command when the `collapsedVersioning` attribute is not defined in the configuration ([#221](https://github.com/mooltiverse/nyx/issues/221))

## 2.4.0

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/2.4.0).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

The Docker image is now published (again) to [Docker Hub](https://hub.docker.com/repository/docker/mooltiverse/nyx) after Docker Hub decided [not to sunset](https://www.docker.com/blog/no-longer-sunsetting-the-free-team-plan/) support for free organizations ([#184](https://github.com/mooltiverse/nyx/issues/184)).

### Fixed issues

There are no fixes in this release.

## 2.3.5

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/2.3.5).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

There are no new features or improvements in this release.

### Fixed issues

This release:

* fixes a bug in the command line version that caused a segment violation error when running the `clean` command when no previous state is found and so the current version hasn't been initialized yet ([#217](https://github.com/mooltiverse/nyx/issues/217))

## 2.3.4

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/2.3.4).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

There are no new features or improvements in this release.

### Fixed issues

This release:

* fixes a bug in the Java version prevents SSH authentication to complete with private keys passed as strings due to the SSH-agent misuse ([#201](https://github.com/mooltiverse/nyx/issues/201))

## 2.3.3

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/2.3.3).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

There are no new features or improvements in this release.

### Fixed issues

This release:

* fixes a bug in the command line version that caused a an exit code 1 to be returned when pushing changes to a Git remote that is already up to date ([#194](https://github.com/mooltiverse/nyx/issues/194))

## 2.3.2

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/2.3.2).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

There are no new features or improvements in this release.

### Fixed issues

This release:

* fixes a bug in the command line version that caused a segment violation error when the `collapsedVersioning` configuration attribute is not set ([#196](https://github.com/mooltiverse/nyx/issues/196))

## 2.3.1

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/2.3.1).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

There are no new features or improvements in this release.

### Fixed issues

This release:

* fixes a bug in the command line version that prevented Git remote services to use template variables in their configuration ([#186](https://github.com/mooltiverse/nyx/issues/186))

## 2.3.0

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/2.3.0).

### Upgrade instructions

* existing State files must be generated again due to the change of the format of timestamp offsets for Git commits

### New features and improvements

This release:

* adds support for remote [changelog templates](https://mooltiverse.github.io/nyx/docs/user/configuration-reference/changelog/#template) ([#158](https://github.com/mooltiverse/nyx/issues/158))
* adds support for Gradle 8.0 ([#162](https://github.com/mooltiverse/nyx/issues/162)), while backward compatibility doesn't change

### Fixed issues

This release:

* fixes a bug that prevented State files serialized with one version of the tool (Go or Java) to be deserialized with another version, due to the format used to serialize the Git author action or commit action timestamp offets ([#170](https://github.com/mooltiverse/nyx/issues/170)). Now offsets are stored as simple signed integers. With more details, Git author actions and commot actions had their timestamps with a string representing the offset to UTC. That format was not portable so now they are stored as positive or negative integers, representing the offset in minutes from UTC. The field name within the `timeStamp` structure has changed from `timeZone` to `offset`.

## 2.2.0

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/2.2.0).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

This release:

* this release adds support for the `summary` and `summaryFile` configuration options, for an easily parseable output from Nyx ([#156](https://github.com/mooltiverse/nyx/issues/156))

### Fixed issues

There are no fixes in this release.

## 2.1.0

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/2.1.0).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

This release:

* this release extends backward compatibility for the Java version (the command line version is not affected) ([#153](https://github.com/mooltiverse/nyx/issues/153)), in details:
  * the recommended JVM version is `15` or newer and the recommended Gradle version is `7.0` or newer
  * older Java versions starting from `11` and older Gradle versions starting from `6.0` are supported with the exception of Gradle versions between `6.5` and `6.9.3` (see [#153](https://github.com/mooltiverse/nyx/issues/153#issuecomment-1416732299))
  * JVM versions older than `11` and Gradle versions older than `6.0` are not supported

### Fixed issues

There are no fixes in this release.

## 2.0.0

* when using the Java (Gradle) version, the minimum Java version is now `15` and the minimum Gradle version is `7.0` (see [here](https://mooltiverse.github.io/nyx/docs/user/introduction/requirements) and [here](https://docs.gradle.org/current/userguide/compatibility.html)); Gradle versions from `6.7` on should still work but they are no longer tested
* the public key (SSH) authentication method is now supported for Git ([#94](https://github.com/mooltiverse/nyx/issues/94))
* the Git internal library (`github.com/mooltiverse/nyx/modules/go/nyx/git` package for Go, `com.mooltiverse.oss.nyx.git` package for Java) has been refactored in order to support multiple authentication mechanisms (with the new SSH method) and some methods have been replaced while others have been added
* support for deprecated [template functions](https://mooltiverse.github.io/nyx/docs/user/configuration-reference/templates/#functions) `environment.user`, `environment.variable`, `file.content`, `file.exists` has been removed, replaced by `environmentUser`, `environmentVariable`, `fileContent`, `fileExists`
* new parametric [template functions](https://mooltiverse.github.io/nyx/docs/user/configuration-reference/templates/) are now available: `capture`, `cutLeft`, `cutRight`, `replace`, `timeFormat` ([#104](https://github.com/mooltiverse/nyx/issues/104), ([#119](https://github.com/mooltiverse/nyx/issues/119)))
* Java template functions in `com.mooltiverse.oss.nyx.template.Functions.java` no longer implement the `java.util.function.Function` interface which was provided for backward compatibility; from now on they only implement the `com.github.jknack.handlebars.Helper` interface provided by [Handlebars](https://github.com/jknack/handlebars.java)
* release assets published to GitLab now show their Description instead of the File Name ([#128](https://github.com/mooltiverse/nyx/issues/128))
* the [state](https://mooltiverse.github.io/nyx/docs/user/state-reference/global-attributes) now has two additional boolean attributes: `coreVersion` and `latestVersion`, telling if the current version is a *core* version (using only core identifiers) and if it is the latest version (compared to all tags in the repository) ([#105](https://github.com/mooltiverse/nyx/issues/105))

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/2.0.0).

### Upgrade instructions

* if you are using the Java (Gradle) version make sure you run Java `15` and Gradle `7.0` or newer
* if you embedded Nyx in your code and used the `github.com/mooltiverse/nyx/modules/go/nyx/git` Go package or the `com.mooltiverse.oss.nyx.git` package, fix your code reflecting the changes made to the API
* if you plan to use Git SSH authentication please refer to [the configuration reference](https://mooltiverse.github.io/nyx/docs/user/configuration-reference/git/) for the required configuration options
* replace usage of deprecated [template functions](https://mooltiverse.github.io/nyx/docs/user/configuration-reference/templates/#functions) `environment.user`, `environment.variable`, `file.content`, `file.exists` (with dots) with `environmentUser`, `environmentVariable`, `fileContent`, `fileExists` (camel case names) in the configuration

### New features and improvements

* the public key (SSH) authentication method is now supported for Git ([#94](https://github.com/mooltiverse/nyx/issues/94))
* new parametric [template functions](https://mooltiverse.github.io/nyx/docs/user/configuration-reference/templates/) are now available: `capture`, `cutLeft`, `cutRight`, `replace`, `timeFormat`

### Fixed issues

There are no fixes in this release.

### Known issues

There are no known issues in this release.

## 1.3.4

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/1.3.4).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

There are no new features or improvements in this release.

### Fixed issues

This release:

* fixes bug [#141](https://github.com/mooltiverse/nyx/issues/141) about confured templates not being rendered correctly when they use transient objects from the State like, for example, the `configuration` tree (Go)

## 1.3.3

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/1.3.3).

### Upgrade instructions

Make sure the `git` executable must be available in the local `PATH` for the workaround to woork. If `git` is not available Nyx doesn't break but the workaround is not effective.

### New features and improvements

There are no new features or improvements in this release.

### Fixed issues

This release:

* fixes bug [#130](https://github.com/mooltiverse/nyx/issues/130) about Git repository status being wrongly detected in some circumstances; a workaround has been applied using the local `git` executable (if available in the local `PATH`) (Go)

### Known issues

There are no known issues in this release.

## 1.3.2

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/1.3.2).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

There are no new features or improvements in this release.

### Fixed issues

This release:

* fixes bug [#132](https://github.com/mooltiverse/nyx/issues/132) about the preset configuration layer not being loaded unless configuration files are provided (Go)

### Known issues

There are no known issues in this release.

## 1.3.1

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/1.3.1).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

There are no new features or improvements in this release.

### Fixed issues

This release:

* fixes bug [#131](https://github.com/mooltiverse/nyx/issues/131) about wrong dates and timestamps in generated changelogs and versions (Go)

### Known issues

There are no known issues in this release.

## 1.3.0

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/1.3.0).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

The command line version is now available [#120](https://github.com/mooltiverse/nyx/issues/120). Binaries can be downloaded from [the release page](https://github.com/mooltiverse/nyx/releases/tag/1.3.0).

### Fixed issues

There are no fixes in this release.

### Known issues

* GitHub release assets are only displayed by their file names, while the description is ignored (due to a limitation in the underlying library)

## 1.2.3

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/1.2.3).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

There are no new features or improvements in this release.

### Fixed issues

This release:

* fixes bug [#122](https://github.com/mooltiverse/nyx/issues/122) about extracting the commit type using Gitmoji and rendering the changelog

### Known issues

There are no known issues in this release.

## 1.2.2

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/1.2.2).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

There are no new features or improvements in this release.

### Fixed issues

This release:

* fixes the issue with up-to-date checks causing duplicate tags to be applied when runnin in *dirty* repositories [#116](https://github.com/mooltiverse/nyx/issues/116)
* bug [#116](https://github.com/mooltiverse/nyx/issues/116) also fixes bug [#115](https://github.com/mooltiverse/nyx/issues/115) although it was originally about a different issue

### Known issues

There are no known issues in this release.

## 1.2.1

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/1.2.1).

### Upgrade instructions

If you use Gradle and [configure Nyx through the Gradle Extension](https://mooltiverse.github.io/nyx/docs/user/introduction/usage#using-the-extension) with some [release assets](https://mooltiverse.github.io/nyx/docs/user/configuration-reference/release-assets/) and also filter release assets within release types using the [assets filter](https://mooltiverse.github.io/nyx/docs/user/configuration-reference/release-types/#assets) you need to update the assets filter configuration to use a simple string (defining a comma separated list of asset names to enable for the release type) instead of a native list. For example you need to replace `assets = [ "asset1", "asset2" ]` with `assets = [ "asset1,asset2" ]` within each [release type](https://mooltiverse.github.io/nyx/docs/user/configuration-reference/release-types/).

### New features and improvements

There are no new features or improvements in this release.

### Fixed issues

This release:

* fixes the issue with release type [assets](https://mooltiverse.github.io/nyx/docs/user/configuration-reference/release-types/#assets) filter configured through the Gradle plugin [#110](https://github.com/mooltiverse/nyx/issues/110)
* fixes the issue with the [resume](https://mooltiverse.github.io/nyx/docs/user/configuration-reference/global-options/#resume) flag causing NullPointerException [#81](https://github.com/mooltiverse/nyx/issues/81)

### Known issues

There are no known issues in this release.

## 1.2.0

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/1.2.0).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

This release:

* adds support for release assets so that published releases can also contain artifacts. You can configure release assets following [these instructions](https://mooltiverse.github.io/nyx/docs/user/configuration-reference/release-assets/)

### Fixed issues

There are no fixes in this release.

### Known issues

There are no known issues in this release.

## 1.1.0

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/1.1.0).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

This release:

* replaces the internal [Mustache](https://mustache.github.io/) template engine with [Handlebars](https://handlebarsjs.com/) for extended feature support

### Fixed issues

There are no fixes in this release.

### Known issues

There are no known issues in this release.

## 1.0.1

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/1.0.1).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

There are no new features or improvements in this release.

### Fixed issues

This release:

* fixes the [Extended](https://mooltiverse.github.io/nyx/docs/user/configuration-presets/extended) preset to also support the *Fix* release type [#89](https://github.com/mooltiverse/nyx/issues/89)
* fixes the [Make](https://mooltiverse.github.io/nyx/docs/user/introduction/how-nyx-works#make) command to comply with the [`dryRun`](https://mooltiverse.github.io/nyx/docs/user/configuration-reference/global-options/#dry-run) flag [#81](https://github.com/mooltiverse/nyx/issues/81)

### Known issues

There are no known issues in this release.
