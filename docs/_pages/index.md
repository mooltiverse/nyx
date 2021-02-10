---
title: Nyx documentation
layout: single
toc: true
permalink: /
---

Nyx is a powerful, flexible and extremely configurable **semantic release** tool. You can put release management on *auto pilot* regardless of the kind of project, languages, tools and technologies or you can control any aspect of release management manually. Preset configuration values let you get started in minutes without digging into detailed settings.

Nyx is the *all in one* tool that gives you integrated support for:

* the *semantic* and *conventional* specifications and best practices out there like [Semantic Versioning](https://semver.org/), [Conventional Commits](https://www.conventionalcommits.org/), [Keep a Changelog](https://keepachangelog.com/)
* the most pupular Git hosting services like [GitHub](https://github.com/) and [GitLab](https://gitlab.com/) and their advanced features like [GitHub Releases](https://docs.github.com/en/github/administering-a-repository/about-releases) and [GitLab Releases](https://docs.gitlab.com/ee/user/project/releases/)
* CI/CD platforms like [GitHub Actions](https://docs.github.com/en/free-pro-team@latest/actions/guides/about-continuous-integration), [GitLab CI](https://docs.gitlab.com/ee/ci/), [Bamboo](https://www.atlassian.com/software/bamboo), [CircleCI](https://circleci.com/), [Jenkins](https://www.jenkins.io/), [TeamCity](https://www.jetbrains.com/teamcity/)
* custom or standard workflows (a.k.a. *branching models*) like [GitFlow](https://nvie.com/posts/a-successful-git-branching-model/), [GitHub Flow](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/github-flow), [GitLab Flow](https://docs.gitlab.com/ee/topics/gitlab_flow.html), [OneFlow](https://www.endoflineblog.com/oneflow-a-git-branching-model-and-workflow)

Want to embed the libraries into your own project instead? We got you covered as they are published just for that. See the [available libraries](#libraries) for more.

Browse the [documentation]({{ site.baseurl }}{% link _pages/index.md %}) for more. Check out the [**quick start**]({{ site.baseurl }}{% link _pages/guide/user/01.quick-start/index.md %}) guide to get Nyx running in minutes or see the [developer guide]({{ site.baseurl }}{% link _pages/guide/developer/index.md %}) if you're interested in using the libraries.

The project is open source and hosted on [GitHub](https://github.com/mooltiverse/nyx).

## Project Status

**THE PROJECT IS IN ITS EARLY STAGES SO ONLY A FEW COMPONENTS HAVE BEEN RELEASED. FOR MORE SEE THE [AVAILABILITY](#availability) SECTION BELOW, THE OPEN [ISSUES](https://github.com/mooltiverse/nyx/milestones) AND THE SCHEDULED [MILESTONES](https://github.com/mooltiverse/nyx/milestones)**.
{: .notice--warning}

Nyx is a best effort project and we can't commit to a deadline. Stay tuned for updates and [releases](https://github.com/mooltiverse/nyx/releases).

## Highlights

* [configuration presets]({{ site.baseurl }}/reference/standard-configurations/) available covering most if not all configuration needs to get started in minutes using streamlined and maintained configs
* multiple [versioning schemes]({{ site.baseurl }}/reference/version-schemes/) support: [Semantic Versioning (SemVer)](https://semver.org/) and [Maven version scheme](https://cwiki.apache.org/confluence/display/MAVEN/Version+number+policy) ([future](https://github.com/mooltiverse/nyx/issues/4))
* support for extra qualifiers in release names (i.e. timestamps, commit SHA, pipeline ID, user name etc) for better traceability
* version history detection based on Git tags and consistency checks to avoid version conflicts and wrong ordering
* multiple commit message convention support: [Conventional Commits](https://www.conventionalcommits.org/), [Angular](https://github.com/angular/angular/blob/master/CONTRIBUTING.md#commit), [Atom](https://github.com/atom/atom/blob/master/CONTRIBUTING.md#git-commit-messages), [Ember](https://github.com/emberjs/ember.js/blob/master/CONTRIBUTING.md), [ESLint](https://eslint.org/docs/developer-guide/contributing/pull-requests#step2), [JQuery](https://github.com/jquery/contribute.jquery.org/blob/master/pages/commits-and-pull-requests.md#commit-guidelines), [JSHint](https://github.com/jshint/jshint/blob/master/CONTRIBUTING.md#commit-message-guidelines), [Karma](http://karma-runner.github.io/4.0/dev/git-commit-msg.html), [gitmoji](https://gitmoji.dev/)
* next version inference by commit message convention
* [changelog]({{ site.baseurl }}/reference/output/#changelog) ([Keep a Changelog](https://keepachangelog.com/) is provided out of the box) and other artifacts generation by configurable templates
* [non intrusive]({{ site.baseurl }}/in-depth/design-principles/#immutable-workspace) operations to keep your workspace clean and uncluttered: Nyx can work on your project without even storing state or configuration files
* support for [local and shared]({{ site.baseurl }}/reference/configuration-methods/) configuration files to allow organizations to share their configuration among projects
* [incremental]({{ site.baseurl }}/in-depth/design-principles/#incremental-process) and [transactional]({{ site.baseurl }}/in-depth/design-principles/#transactional-process) operations for better integration with other tools and build scripts
* early version number availability for use in any workflows and pipelines
* support for official releases, pre-releases, post-releases (a.k.a. maintenance releases), local releases, *non*-releases (i.e. built from *dirty* workspaces etc)
* support for any [branching model]({{ site.baseurl }}/best-practice/branching-models/) like [GitFlow](https://nvie.com/posts/a-successful-git-branching-model/), [GitHub Flow](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/github-flow), [GitLab Flow](https://docs.gitlab.com/ee/topics/gitlab_flow.html), [OneFlow](https://www.endoflineblog.com/oneflow-a-git-branching-model-and-workflow) and custom ones
* support for configurable [build environments]({{ site.baseurl }}/configuration/environments/), from local (i.e. developer workstations) to fully fledged CI/CD platforms like [GitHub Actions](https://docs.github.com/en/free-pro-team@latest/actions/guides/about-continuous-integration), [GitLab CI](https://docs.gitlab.com/ee/ci/), [Bamboo](https://www.atlassian.com/software/bamboo), [CircleCI](https://circleci.com/), [Jenkins](https://www.jenkins.io/), [TeamCity](https://www.jetbrains.com/teamcity/)
* release publication to popular [services]({{ site.baseurl }}/reference/services/#git-hosting-services) like [GitHub Releases]({{ site.baseurl }}/reference/services/#github-service), [GitLab Releases]({{ site.baseurl }}/reference/services/#gitlab-service) (with more in future releases, including [Bitbucket]({{ site.baseurl }}/reference/services/#bitbucket-service))
* notifications and announcements

## Availability

Nyx is available as:

* a command line executable tool:
  * Go version (**not yet available**)
* a [Docker](https://www.docker.com/) image ([**not yet released**](https://hub.docker.com/r/mooltiverse/nyx))
* a build tool plugin:
  * for [Gradle](https://gradle.org/) ([**not yet released**](https://plugins.gradle.org/plugin/com.mooltiverse.oss.nyx))
  * for [Bazel](https://bazel.build/) (**not yet available**)
* a native CI/CD extension:
  * for [GitHub Actions](https://help.github.com/en/actions/building-actions) (**not yet available**)

You can also embed parts of the Nyx implementation like:

* the [Semantic Version](https://semver.org/) library:
  * [Java version]({{ site.baseurl }}/java-library/using-the-version-library/)
  * Go version (**not yet available**)
* the *Core* library:
  * Java version (**not yet available**)
  * Go version (**not yet available**)

## Quick Links

* Project:
  * [repository](https://github.com/mooltiverse/nyx){:target="_blank"}
  * [issues](https://github.com/mooltiverse/nyx/issues){:target="_blank"}
  * [releases](https://github.com/mooltiverse/nyx/releases){:target="_blank"}
* Documentation:
  * [Nyx documentation](https://mooltiverse.github.io/nyx/)
    * [user guide](https://mooltiverse.github.io/nyx/guide/user/)
    * [developer guide](https://mooltiverse.github.io/nyx/guide/developer/)
  * [Javadoc API](https://javadoc.io/doc/com.mooltiverse.oss.nyx/java)
* Deliverables:
  * [GitHub packages](https://github.com/mooltiverse/nyx/packages){:target="_blank"}
  * [Maven repository](https://search.maven.org/search?q=g:com.mooltiverse.oss.nyx){:target="_blank"} ([raw version](https://repo.maven.apache.org/maven2/com/mooltiverse/oss/nyx/){:target="_blank"})
  * [Gradle plugin](https://plugins.gradle.org/plugin/com.mooltiverse.oss.nyx){:target="_blank"}
  * [Docker image](https://hub.docker.com/r/mooltiverse/nyx){:target="_blank"}

## Badge

If you like Nyx please consider showing the badge [![](https://img.shields.io/badge/powered%20by-Nyx-blue)](https://github.com/mooltiverse/nyx) on your project page by inserting this snippet:

```md
[![](https://img.shields.io/badge/powered%20by-Nyx-blue)](https://github.com/mooltiverse/nyx)
```
