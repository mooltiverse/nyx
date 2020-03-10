---
title: Nyx documentation
layout: single
toc: true
permalink: /
---

Nyx is a portable and extensible automatic semantic release tool with several availability options. With Nyx you can put release management on auto pilot regardless of the kind of project, its languages, tools and technologies. You can use the tools out of the box or you can extend them with your own customizations or even build your own using the Nyx libraries.

Follow the links on the left side to read through the [**guide**]({{ site.baseurl }}/getting-started/introduction/) or browse the [examples]({{ site.baseurl }}/examples/) from the top bar to access the library of specific use cases.

The project at the [repository home](https://github.com/mooltiverse/nyx).

## Project Status
**THE PROJECT IS IN ITS EARLY STAGES SO ONLY A FEW COMPONENTS HAVE BEEN RELEASED**.
{: .notice--warning}

This guide highlights the current availability for each feature and component.

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

## Availability

Nyx is available as:

* a set of [java libraries](https://search.maven.org/search?q=g:com.mooltiverse.oss.nyx), each allowing you to have the minimal set of dependencies, depending on your use case
* a command line tool, as an executable jar file
* a [Docker image](https://hub.docker.com/r/mooltiverse/nyx) to encapsulate Nyx and use it in your projects without injecting any component
* plugins and extensions for multiple build tools, starting with [Gradle](https://plugins.gradle.org/plugin/com.mooltiverse.oss.nyx) and [Bazel](https://bazel.build/)
* [GitHub Actions](https://help.github.com/en/actions/building-actions)

Future releases will also be available as a Go executable for multiple platforms and a set of Go packages.

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










# Objectives

## Branching strategy support
*Nyx* support for Git branching models comes with built-in configurations for the most popular workflows but is also fully customizable, making no assumptions on which branches you use and at which stage. You can manage *release* branches as well as *pre-release* or *maintenance* (*post-release*) branches.

Other branches (like *feature branches*) can still have their version numbers assigned, properly identified to avoid clashes with official releases. So called *dirty* workspaces (with uncommitted changes) can also build with specific version numbers, to let developers iterate locally before committing. These branches are not meant to publish official releases but, instead, give developers *continuity*.

Local environments are important to *Nyx* to give developers a consistent build process that seamlessly adapts when on local or CI environments. This is meant to grant consistency of the build process and avoid workarounds driving to *works on my machine* situations.

## Automatic bumping of version numbers
If you need to leverage automation and want versions to be managed on autopilot you probably don't want to inspect the entire commit history for every *merge* or release you need to issue. You can automate the decision on which component of the version number to increase based on combinations of:
* tags already available in the repository
* the current [branch](https://git-scm.com/book/it/v2/Git-Branching-Basic-Branching-and-Merging)
* the commit history messages supporting different standard conventions (i.e. [Conventional Commits](https://www.conventionalcommits.org/) or [Angular conventions](https://github.com/angular/angular/blob/22b96b9/CONTRIBUTING.md#-commit-message-guidelines)) or your own custom convention
* already available changelogs

Needless to say you can always override automatic version at any time.

Tags can be automatically pushed to remote repositories.

## Build tools integrations
*Nyx* will be available for use by build tools so that its features can be easily integrated within build scripts. Version numbers will be available in the early stages of the process (i.e. the [version property](https://docs.gradle.org/current/userguide/writing_build_scripts.html#sec:standard_project_properties) of Gradle projects will be available since the [Initialization phase](https://docs.gradle.org/current/userguide/build_lifecycle.html)) so that they can be used when building the project artifacts, not just at the end. Also, checks can be performed early.

The overhead introduced by *Nyx* is insignificant.

## CI integrations
*Nyx* provides extensions to support integrations with the most popular CI environments to:
* leverage automation
* support headless and unattended environments
* integrate with custom workflows

We are also contemplating *ChatOps* features and integrations.

## Publications and notifications
*Nyx* lets you publish additional artifacts (i.e. release notes or pages) as well as send notifications (i.e. to Slack channels) upon configurable events.

The templates are configurable and allow to add specific contents like links to Docker images or software repositories.

## Microservices versioning
We are contemplating the implementation of additional features that allow to version a group of projects and their dependencies automatically. This is the case of microservices, when each service has its own lifecycle and version numbers but downstream services need to test, build and deliver also taking into account upstream ones.

## No technology constraints
*Nyx* is written in Java and that's the only technological constraint to run. You can use the `java` executable to run *Nyx* from the command line or use its Docker container.

Java apart, you can run *Nyx* on any environment and without any assumption on the kind of project you're working on.

## API availability
In case build tools extensions or command line tools are not enough, *Zyx* will be available on public repositories to let you use it as a library.
