[![](https://github.com/mooltiverse/nyx/workflows/CI/badge.svg)](https://github.com/mooltiverse/nyx/actions?query=workflow%3ACI)

[![javadoc](https://javadoc.io/badge2/com.mooltiverse.oss.nyx/java/javadoc.svg)](https://javadoc.io/doc/com.mooltiverse.oss.nyx/java)

[![](https://img.shields.io/badge/powered%20by-Nyx-blue)](https://github.com/mooltiverse/nyx)

[![License](https://img.shields.io/badge/License-Apache%202.0-grey.svg)](LICENSE.md) [![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-grey.svg)](CODE_OF_CONDUCT.md)

# Nyx
Nyx is a portable and extensible automatic semantic release tool with several availability options. With Nyx you can put release management on auto pilot regardless of the kind of project, its languages, tools and technologies. You can use the tools out of the box or you can extend them with your own customizations or even build your own using the Nyx libraries.

For any further information please go to the [**documentation site**](https://mooltiverse.github.io/nyx/).

## Project Status
**THE PROJECT IS IN ITS EARLY STAGES SO ONLY A FEW COMPONENTS HAVE BEEN RELEASED. PLEASE SEE THE [DOCUMENTATION](https://mooltiverse.github.io/nyx/#project-status) TO KNOW THE CURRENT STATUS**

Nyx is a best effort project and we can't commit to a deadline. Stay tuned for updates and [releases](https://github.com/mooltiverse/nyx/releases).

## Features

* full [Semantic Release (SemVer)](https://semver.org/) compliance, with additional support for Maven version scheme
* automatic detection of the previous version from Git tags
* automatic generation of the next version, where numbers are bumped using different criteria (or combinations of) like:
    * inspection of commit messages in the [Git](https://git-scm.com/) commit history following specific conventions (i.e. [Conventional Commits](https://www.conventionalcommits.org/))
    * parsing of previously generated change logs
* automatic changelog and release notes generation
* version number consistency checks to avoid version conflicts and wrong ordering
* early version number availability to allow build processes to use version numbers since the early stages
* support for unreleased versions, local builds, *dirty* workspaces
* configurable branching model (workflow) to support for any kind of strategy, like [GitFlow](https://nvie.com/posts/a-successful-git-branching-model/), [GitHub Flow](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/github-flow), [GitLab Flow](https://docs.gitlab.com/ee/topics/gitlab_flow.html), [OneFlow](https://www.endoflineblog.com/oneflow-a-git-branching-model-and-workflow) and custom
* support for CI/CD environments (i.e. [GitHub Actions](https://help.github.com/en/actions/getting-started-with-github-actions/about-github-actions), [GitLab CI/CD](https://docs.gitlab.com/ee/ci/)) and local environments
* release tagging and announcements, with native extensions for common platforms like [GitHub Releases](https://help.github.com/en/github/administering-a-repository/releasing-projects-on-github), [GitLab Releases](https://docs.gitlab.com/ee/user/project/releases/)
* notifications and announcements upon new releases

Additional features are considered for future releases like:

* microservices versioning, allowing to manage upstream and downstream versions of components belonging to the same *tree*

For more see the [features documentation](https://mooltiverse.github.io/nyx/#features).

## Availability

Nyx is available as:

* a set of [java libraries](https://search.maven.org/search?q=g:com.mooltiverse.oss.nyx), each allowing you to have the minimal set of dependencies, depending on your use case
* a command line tool, as an executable jar file
* a [Docker image](https://hub.docker.com/r/mooltiverse/nyx) to encapsulate Nyx and use it in your projects without injecting any component
* plugins and extensions for multiple build tools, starting with [Gradle](https://plugins.gradle.org/plugin/com.mooltiverse.oss.nyx) and [Bazel](https://bazel.build/)
* [GitHub Actions](https://help.github.com/en/actions/building-actions)

Future releases will also be available as a Go executable for multiple platforms and a set of Go packages.

For more see the [availability documentation](https://mooltiverse.github.io/nyx/#features).

## Links

* Project:
    * [repository](https://github.com/mooltiverse/nyx)
    * [issues](https://github.com/mooltiverse/nyx/issues)
    * [releases](https://github.com/mooltiverse/nyx/releases)
* Documentation:
    * [documentation site](https://mooltiverse.github.io/nyx/)
    * [Javadoc API](https://javadoc.io/doc/com.mooltiverse.oss.nyx/java)
* Deliverables:
    * [packages](https://github.com/mooltiverse/nyx/packages)
    * [Gradle plugin](https://plugins.gradle.org/plugin/com.mooltiverse.oss.nyx)
    * [Docker image](https://hub.docker.com/r/mooltiverse/nyx)
    * [Maven repository](https://search.maven.org/search?q=g:com.mooltiverse.oss.nyx) ([raw version](https://repo.maven.apache.org/maven2/com/mooltiverse/oss/nyx/))

## Badge

If you like Nyx please consider showing the badge [![](https://img.shields.io/badge/powered%20by-Nyx-blue)](https://github.com/mooltiverse/nyx) on your project page by inserting this snippet:

```md
[![](https://img.shields.io/badge/powered%20by-Nyx-blue)](https://github.com/mooltiverse/nyx)
```
