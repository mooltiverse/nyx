---
title: Nyx documentation
layout: single
toc: true
permalink: /
---

[Nyx]({{ site.data.project.home }}) is a powerful, flexible and extremely configurable **semantic release** tool. You can put release management on *auto pilot* regardless of the kind of project, languages, tools and technologies or you can control any aspect of release management manually. Preset configuration values let you get started in minutes without digging into detailed settings.

Nyx is the *all in one* tool that gives you integrated support for:

* the [Semantic Versioning (SemVer)](https://semver.org/) scheme
* *conventional* specifications and best practices like [Conventional Commits](https://www.conventionalcommits.org/), [Keep a Changelog](https://keepachangelog.com/)
* build tools like [Gradle](https://gradle.org/)
* Git hosting services like [GitHub](https://github.com/) and [GitLab](https://gitlab.com/) and their extra features like [GitHub Releases](https://docs.github.com/en/github/administering-a-repository/about-releases) and [GitLab Releases](https://docs.gitlab.com/ee/user/project/releases/)
* CI/CD platforms like [GitHub Actions]({{ site.data.project.home }}-github-action) and [GitLab CI](https://docs.gitlab.com/ee/ci/)
* custom or standard workflows (a.k.a. *branching models*) like [GitFlow](https://nvie.com/posts/a-successful-git-branching-model/), [GitHub Flow](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/github-flow), [GitLab Flow](https://docs.gitlab.com/ee/topics/gitlab_flow.html), [OneFlow](https://www.endoflineblog.com/oneflow-a-git-branching-model-and-workflow)
* changelog generation
* any (combination of) configuration means like command line arguments, local or remote files, environment variables and build tools extensions

Want to embed the libraries into your own project instead? You're covered as they are published just for that. See the [available libraries](#libraries) for more.

Browse the [documentation]({{ site.baseurl }}{% link _pages/index.md %}) for more. Check out the [**quick start**]({{ site.baseurl }}{% link _pages/guide/user/01.quick-start/index.md %}) guide to get Nyx running in minutes or see the [developer guide]({{ site.baseurl }}{% link _pages/guide/developer/index.md %}) if you're interested in using the libraries.

The project is open source and hosted on [GitHub]({{ site.data.project.home }}).

## Getting started in 30 seconds

1. download the binary package for your distribution from the [latest release]({{ site.data.project.home }}/releases/latest) assets
2. put the binary somewhere visible within the `PATH` and (optionally) rename it to `nyx` (or `nyx.exe` on Windows)
3. open a shell, *cd* into your project folder and run:

```bash
$ nyx --preset=simple infer
Version: 1.2.3
```

This will not apply any change to your repository (as the *infer* command doesn't) but will give you a taste of what Nyx finds out. Here `1.2.3` is the version that Nyx has inferred from the commit history and will be the one it will tag and release with if you use other commands, like `nyx --preset=simple publish`.

The *simple* preset is a streamlined configuration suitable for *mainline only* projects but you can replace it with your own configuration or override its values selectively.

From here:

* run `nyx --help` for the synopsis or jump to the [documentation]({{ site.baseurl }}{% link _pages/index.md %})
* tune your configuration or tweak command line arguments according to your needs
* try your configuration using the `--dry-run` mode
* embed Nyx into your build scripts and automation environments

## Downloads and availability

### Binaries

Available for all platforms in the [latest release]({{ site.data.project.home }}/releases/latest) assets.

### Docker images

* on [Docker Hub](https://hub.docker.com/repository/docker/mooltiverse/nyx)
* on [GitHub container registry]({{ site.data.project.home }}/pkgs/container/nyx)

### Build tools plugin

* [Gradle plug-in](https://plugins.gradle.org/plugin/com.mooltiverse.oss.nyx)
* [GitHub Action]({{ site.data.project.home }}-github-action)

### Libraries

* Main library:
  * [for Go]({{ site.data.project.docs_home }}/guide/developer/go/nyx-main/)
  * [for Java]({{ site.data.project.docs_home }}/guide/developer/java/nyx-main/)
* [Semantic Version](https://semver.org/) library:
  * [for Go]({{ site.data.project.docs_home }}/guide/developer/java/semantic-version/)
  * [for Java]({{ site.data.project.docs_home }}/guide/developer/java/semantic-version/)

## Documentation

* [Main Nyx documentation]({{ site.data.project.docs_home }}/)
  * [user guide]({{ site.data.project.docs_home }}/guide/user/)
  * [developer guide]({{ site.data.project.docs_home }}/guide/developer/)
* [Godoc API](https://godocs.io/github.com/mooltiverse/nyx/modules/go/nyx)
* [Javadoc API](https://javadoc.io/doc/com.mooltiverse.oss.nyx/java)
  
### Articles

* [Nyx, the Semantic Release Automation Tool](https://levelup.gitconnected.com/nyx-the-semantic-release-automation-tool-4e2dfa949f38)
* [Semantic Release Automation with Gradle using Nyx](https://levelup.gitconnected.com/semantic-release-automation-with-gradle-using-nyx-ba345235a365)

### Reports and metrics

* security: [![Security Rating](https://sonarcloud.io/api/project_badges/measure?project=mooltiverse_nyx&metric=security_rating)](https://sonarcloud.io/summary/new_code?id=mooltiverse_nyx), [![Vulnerabilities](https://sonarcloud.io/api/project_badges/measure?project=mooltiverse_nyx&metric=vulnerabilities)](https://sonarcloud.io/summary/new_code?id=mooltiverse_nyx)
* code quality: [![Code Smells](https://sonarcloud.io/api/project_badges/measure?project=mooltiverse_nyx&metric=code_smells)](https://sonarcloud.io/summary/new_code?id=mooltiverse_nyx), [![Bugs](https://sonarcloud.io/api/project_badges/measure?project=mooltiverse_nyx&metric=bugs)](https://sonarcloud.io/summary/new_code?id=mooltiverse_nyx), [![Technical Debt](https://sonarcloud.io/api/project_badges/measure?project=mooltiverse_nyx&metric=sqale_index)](https://sonarcloud.io/summary/new_code?id=mooltiverse_nyx), [![Reliability Rating](https://sonarcloud.io/api/project_badges/measure?project=mooltiverse_nyx&metric=reliability_rating)](https://sonarcloud.io/summary/new_code?id=mooltiverse_nyx)
* code coverage: [![Coverage](https://sonarcloud.io/api/project_badges/measure?project=mooltiverse_nyx&metric=coverage)](https://sonarcloud.io/summary/new_code?id=mooltiverse_nyx), [![Codecov](https://codecov.io/gh/mooltiverse/nyx/branch/main/graph/badge.svg)](https://codecov.io/gh/mooltiverse/nyx) [flaky]({{ site.data.project.home }}/issues/151)

## Badge

If you like Nyx please consider showing the badge [![](https://img.shields.io/badge/powered%20by-Nyx-blue)]({{ site.data.project.home }}) on your project page by inserting this snippet:

```md
[![](https://img.shields.io/badge/powered%20by-Nyx-blue)]({{ site.data.project.home }})
```
