![](https://github.com/mooltiverse/nyx/workflows/build/badge.svg?branch=master)

# Nyx
Nyx is a semantic release tool built around [Git](https://git-scm.com/) with some unique features. The term *semantic release* means [semantic versioning](https://semver.org/) compliance in the first place, with the addition of several features pertaining the release *process* like:

* support for branching models and workflows like [GitFlow](https://nvie.com/posts/a-successful-git-branching-model/), [GitHub Flow](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/github-flow), [GitLab Flow](https://docs.gitlab.com/ee/topics/gitlab_flow.html), [OneFlow](https://www.endoflineblog.com/oneflow-a-git-branching-model-and-workflow), to mention a few:
* automatic detection of the next version number
* version number consistency checks
* changelog and release notes generation
* version publishing and announcements
* notifications

If you're searching for such a tool you probably have already checked out [semantic-release](https://github.com/semantic-release/semantic-release). If not, you should. If you're working on a Node project, *semantic-release* is probably the best option for you.

**THE PROJECT IS IN ITS EARLY STAGES SO IT'S NOT AVAILABLE YET**

Meanwhile you can take a look at the list of other tools below.

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

## Extensibility
*Nyx* is extensible and beside the built-in features it allows custom configurations and plugins to extend its workflow and behavior.

# Availability
Nyx will be available as:
* java library
* command line tool, as an executable jar
* a Docker image
* plugins and and extensions for multiple build tools, starting with [Gradle](https://gradle.org/) and [Bazel](https://bazel.build/)
* [GitHub Actions](https://help.github.com/en/actions/building-actions)

We don't know **when** *Nyx* will be available yet.

# Background

# Acknowledgments

## Links and references
* [Semantic Versioning](https://semver.org/)

# FAQ

Q: **When will *Nyx* be available?**

A: We don't know yet and we can't commit to a deadline. Drop us a line on open an issue if you need, or stay tuned for updates.

Q: **How does *Nyx* compare to [*semantic-release*](https://github.com/semantic-release/semantic-release)? Isn't *semantic-release* enough?**

A: *semantic-release* is an excellent tool and, again, if you're working on Node projects you should definitely go that way. However, when not on Node projects, it introduces some constraints that are not always so simple to overcome. *Nyx* is meant to be an alternative to *semantic-release* for those cases. For the differences between *Nyx* and *semantic-release* please see the sections above.

Q: **Where does the Nyx name come from?**

A: Nyx is named after *3908 Nyx*, a notable [Mars crossing asteroid](https://en.wikipedia.org/wiki/3908_Nyx), which in turn is named after the [the Greek goddess of the night](https://en.wikipedia.org/wiki/Nyx). We liked the name as it's three letters like the number of a semantic version number.
