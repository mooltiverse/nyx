[![](https://github.com/mooltiverse/nyx/workflows/CI/badge.svg)](https://github.com/mooltiverse/nyx/actions?query=workflow%3ACI)

[![Javadoc](https://javadoc.io/badge2/com.mooltiverse.oss.nyx/java/Javadoc.svg)](https://javadoc.io/doc/com.mooltiverse.oss.nyx/java)

[![](https://img.shields.io/badge/powered%20by-Nyx-blue)](https://github.com/mooltiverse/nyx)

[![License](https://img.shields.io/badge/License-Apache%202.0-grey.svg)](LICENSE.md) [![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-grey.svg)](CODE_OF_CONDUCT.md)

# Nyx
Nyx is a portable and extensible automatic semantic release tool with several availability options. With Nyx you can put release management on auto pilot regardless of the kind of project, languages, tools and technologies. You can use the tools out of the box or you can extend them with your own customizations or even build your own using the Nyx libraries.

Check out the [**documentation**](https://mooltiverse.github.io/nyx/) now for any further information.

## Project Status
**THE PROJECT IS IN ITS EARLY STAGES SO ONLY A FEW COMPONENTS HAVE BEEN RELEASED. PLEASE SEE THE [DOCUMENTATION](https://mooltiverse.github.io/nyx/#project-status) TO KNOW THE CURRENT STATUS**

Nyx is a best effort project and we can't commit to a deadline. Stay tuned for updates and [releases](https://github.com/mooltiverse/nyx/releases).

## Features

* full [Semantic Release (SemVer)](https://semver.org/) compliance, with additional support for Maven version scheme
* version number consistency checks to avoid version conflicts and wrong ordering
* previous version history automatic detection
* automatic inference of next version by configurable rules using commit history messages and changelogs according to specific conventions (i.e. [Conventional Commits](https://www.conventionalcommits.org/))
* changelog and release notes generation
* early version number availability for complex workflows and pipelines
* support for official releases, pre-releases, post-releases (a.k.a. maintenance releases), *non*-releases (i.e. local builds, *dirty* workspaces etc)
* configurable branching model to support any kind of strategy, like [GitFlow](https://nvie.com/posts/a-successful-git-branching-model/), [GitHub Flow](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/github-flow), [GitLab Flow](https://docs.gitlab.com/ee/topics/gitlab_flow.html), [OneFlow](https://www.endoflineblog.com/oneflow-a-git-branching-model-and-workflow) and custom ones
* support for CI/CD environments (i.e. [GitHub Actions](https://help.github.com/en/actions/getting-started-with-github-actions/about-github-actions), [GitLab CI/CD](https://docs.gitlab.com/ee/ci/)) and local environments
* release tagging and announcements, with native extensions for common platforms like [GitHub Releases](https://help.github.com/en/github/administering-a-repository/releasing-projects-on-github), [GitLab Releases](https://docs.gitlab.com/ee/user/project/releases/)
* notifications and announcements
* plugin support

For more see the [features documentation](https://mooltiverse.github.io/nyx/#features).

## Availability

Nyx is available as:

* a command line executable tool:
    * Java version (**not yet available**)
    * Go version (**not yet available**)
* a [Docker](https://www.docker.com/) image ([**not yet released**](https://hub.docker.com/r/mooltiverse/nyx))
* a build tool plugin:
    * for [Gradle](https://gradle.org/) ([**not yet released**](https://plugins.gradle.org/plugin/com.mooltiverse.oss.nyx))
    * for [Bazel](https://bazel.build/) (**not yet available**)
* a native CI/CD extension:
    * for [GitHub Actions](https://help.github.com/en/actions/building-actions) (**not yet available**)

You can also embed parts of the Nyx implementation like:
* the [Semantic Version](https://semver.org/) library:
    * [Java version](https://mooltiverse.github.io/nyx/java-library/using-the-version-library/)
    * Go version (**not yet available**)
* the *Core* library:
    * Java version (**not yet available**)
    * Go version (**not yet available**)

You can extend Nyx by using its API:
* Java version (**not yet available**)
* Go version (**not yet available**)

For more see the [availability documentation](https://mooltiverse.github.io/nyx/#availability).

## Quick Links

* Project:
    * [repository](https://github.com/mooltiverse/nyx)
    * [issues](https://github.com/mooltiverse/nyx/issues)
    * [releases](https://github.com/mooltiverse/nyx/releases)
* Documentation:
    * [main documentation site](https://mooltiverse.github.io/nyx/)
    * [Javadoc API](https://javadoc.io/doc/com.mooltiverse.oss.nyx/java)
* Deliverables:
    * [GitHub packages](https://github.com/mooltiverse/nyx/packages)
    * [Maven repository](https://search.maven.org/search?q=g:com.mooltiverse.oss.nyx) ([raw version](https://repo.maven.apache.org/maven2/com/mooltiverse/oss/nyx/))
    * [Gradle plugin](https://plugins.gradle.org/plugin/com.mooltiverse.oss.nyx)
    * [Docker image](https://hub.docker.com/r/mooltiverse/nyx)

## Badge

If you like Nyx please consider showing the badge [![](https://img.shields.io/badge/powered%20by-Nyx-blue)](https://github.com/mooltiverse/nyx) on your project page by inserting this snippet:

```md
[![](https://img.shields.io/badge/powered%20by-Nyx-blue)](https://github.com/mooltiverse/nyx)
```
