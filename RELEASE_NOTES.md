# Nyx Release Notes

## 2.0.0

* support for deprecated [template functions](https://mooltiverse.github.io/nyx/guide/user/configuration-reference/templates/#functions) `environment.user`, `environment.variable`, `file.content`, `file.exists` has been removed, replaced by `environmentUser`, `environmentVariable`, `fileContent`, `fileExists`
* the public key (SSH) authentication method is now supported for Git ([#94](https://github.com/mooltiverse/nyx/issues/94))
* the Git internal library (`github.com/mooltiverse/nyx/modules/go/nyx/git` package for Go, `com.mooltiverse.oss.nyx.git` package for Java) has been refactored in order to support multiple authentication mechanisms (with the new SSH method) and some methods have been replaced while others have been added

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/2.0.0).

### Upgrade instructions

* replace usage of deprecated [template functions](https://mooltiverse.github.io/nyx/guide/user/configuration-reference/templates/#functions) `environment.user`, `environment.variable`, `file.content`, `file.exists` (with dots) with `environmentUser`, `environmentVariable`, `fileContent`, `fileExists` (camel case names) in the configuration
* if you embedded Nyx in your code and used the `github.com/mooltiverse/nyx/modules/go/nyx/git` Go package or the `com.mooltiverse.oss.nyx.git` package, fix your code reflecting the changes made to the API

### New features and improvements

There are no new features or improvements in this release.

### Fixed issues

There are no fixes in this release.

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

If you use Gradle and [configure Nyx through the Gradle Extension](https://mooltiverse.github.io/nyx/guide/user/introduction/usage/#using-the-extension) with some [release assets](https://mooltiverse.github.io/nyx/guide/user/configuration-reference/release-assets/) and also filter release assets within release types using the [assets filter](https://mooltiverse.github.io/nyx/guide/user/configuration-reference/release-types/#assets) you need to update the assets filter configuration to use a simple string (defining a comma separated list of asset names to enable for the release type) instead of a native list. For example you need to replace `assets = [ "asset1", "asset2" ]` with `assets = [ "asset1,asset2" ]` within each [release type](https://mooltiverse.github.io/nyx/guide/user/configuration-reference/release-types/).

### New features and improvements

There are no new features or improvements in this release.

### Fixed issues

This release:

* fixes the issue with release type [assets](https://mooltiverse.github.io/nyx/guide/user/configuration-reference/release-types/#assets) filter configured through the Gradle plugin [#110](https://github.com/mooltiverse/nyx/issues/110)
* fixes the issue with the [resume](https://mooltiverse.github.io/nyx/guide/user/configuration-reference/global-options/#resume) flag causing NullPointerException [#81](https://github.com/mooltiverse/nyx/issues/81)

### Known issues

There are no known issues in this release.

## 1.2.0

This release is available at [this link](https://github.com/mooltiverse/nyx/releases/tag/1.2.0).

### Upgrade instructions

There are no actions to take for backward compatibility.

### New features and improvements

This release:

* adds support for release assets so that published releases can also contain artifacts. You can configure release assets following [these instructions](https://mooltiverse.github.io/nyx/guide/user/configuration-reference/release-assets/)

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

* fixes the [Extended](https://mooltiverse.github.io/nyx/guide/user/configuration-presets/extended/) preset to also support the *Fix* release type [#89](https://github.com/mooltiverse/nyx/issues/89)
* fixes the [Make](https://mooltiverse.github.io/nyx/guide/user/introduction/how-nyx-works/#make) command to comply with the [`dryRun`](https://mooltiverse.github.io/nyx/guide/user/configuration-reference/global-options/#dry-run) flag [#81](https://github.com/mooltiverse/nyx/issues/81)

### Known issues

There are no known issues in this release.
