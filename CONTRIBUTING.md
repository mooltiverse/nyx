# Contributing

We're happy you're reading this and willing to contribute! We also appreciate if you read through this document to help us accepting your contributions.

This project and everyone participating in it is governed by the [Contributor Covenant Code of Conduct](CODE_OF_CONDUCT.md). By participating, you are expected to uphold this code.

## Issues

Feel free to open new [issues](https://github.com/mooltiverse/nyx/issues) for bugs and support requests. Before doing so we just ask you to:

1. search for similar requests that might have been opened in the past
2. read through the [docs](https://mooltiverse.github.io/nyx/) and see if a similar was already addressed. Also read through the [examples](https://mooltiverse.github.io/nyx/examples/)
3. try with the latest version in case you're using an old one

If you can't find any answer, when submitting a new issue please provide as much information as you can to help us understand. Don't forget to:

* include a **clear title and description**
* give as much relevant information as possible, which may include a code snippet or an executable test case
* give instructions on how to reproduce the problem
* describe the expected behavior and the current behavior
* tell us about the kind of deliverable you're using and their version and describe the environment so we can reproduce it

We will do our best to take relevant requests and update the documentation or publish a new [example post](https://mooltiverse.github.io/nyx/examples/) about the specific use case in order to make it reusable by others.

## Code and Documentation

If you're going to contribute with code or documentation please read through the following sections. When contributing to this repository, please first discuss the change you wish to make via issue, email, or any other method with the owners of this repository before making a change.

### Branching strategy and commits
This project uses the [GitHub flow](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/github-flow) branching strategy. Before contributing make sure you have a clear understanding of it.

Always write a clear log message for your commits. One-line messages are fine for small changes, but bigger changes should look like this:

```text
$ git commit -m "A brief summary of the commit
> 
> A paragraph describing what changed and its impact."
```

### Prerequisites

You can work on the project on any platform (Linux, Windows, Mac). You need to have installed:

* [Git](https://git-scm.com/)
* Java JDK or JRE version 8 or above, either from [Oracle](https://www.oracle.com/java/technologies/javase-downloads.html) or [OpenJDK](https://openjdk.java.net/install/)
* [Gradle](https://gradle.org/install/) 6.x or above
* [Docker CE](https://docs.docker.com/install/)
* [Jekyll](https://jekyllrb.com/docs/installation/) 4.0.0 or above (also requires [Ruby](https://www.ruby-lang.org/en/downloads/) 2.4.0 or above)

You also need a local copy of the repository that you can get by running:

```shell script
$ git clone https://github.com/mooltiverse/nyx.git
```

You can use any IDE, although we commonly use [IntelliJ Idea](https://www.jetbrains.com/idea/).

### Repository organization and layout

The repository uses [Gradle](https://gradle.org/) as the primary build tool and [GitHub Actions](https://help.github.com/en/actions) for CI/CD. GitHub Actions, in turn, run Gradle tasks. This gives contributor a perfectly consistent environment to work locally before they commit or submit pull requests. Some tasks, like `release` and `publish` can only be executed on the CI/CD platform as they require access to some secrets variables not available locally.

The repository is divided into several [sub projects](https://docs.gradle.org/current/userguide/multi_project_builds.html), all under the `modules` directory plus the documentation, in the `docs` directory.

The resulting layout is:

```text
nyx
+-- .github                         
|   \--- workflows                  # GitHub Actions worflows
+-- docs                            # Documentation micro site
|   +--- _data                      # Data files used by the documentation site
|   +--- _pages                     # Documentation content pages
|   +--- _posts                     # Documentation articles (i.e. examples)
|   \--- _config.yml                # Jekyll configuration for the documentation site
+-- gradle                          # Gradle wrapper files
+-- modules                         # Project sub-modules
|   +--- docker                     # Docker image sub module
|   \--- java                       # Java sub modules
|        +--- gradle                # Gradle plugin sub module
|        \--- lib                   # Java libraries sub modules
|             +--- api              # Java API library sub module
|             +--- cli              # Java Command Line Interface sub module
|             +--- core             # Java Core library sub module
|             \--- version          # Java Version library sub module
+-- build.gradle                    # Gradle build script (main)
+-- gradle.properties               # Project and system properties
+-- gradlew                         # Gradle wrapper
+-- gradlew.bat                     # Gradle wrapper (for Windows)
\-- settings.gradle                 # Gradle project and sub-modules definition
```

Git [ignored](.gitignore) and non relevant items are omitted from the list. All sub-modules have a `build.gradle` script file and a `src` directory containing the [source sets](https://docs.gradle.org/current/dsl/org.gradle.api.tasks.SourceSet.html), not shown above.

Use the [Gradle wrapper](https://docs.gradle.org/current/userguide/gradle_wrapper.html) to run Gradle, like:

```shell script
$ ./gradlew [TASK]    # on Linux and Mac
> gradlew [TASK]      # on Windows
```

where the main tasks are:

* `clean` to restore the entire project directory to its initial state
* `build` to build the entire project
* `test` to test the entire project

The `publish` and `release` tasks can only be executed on the CI/CD platform.

### Contributing Documentation

The documentation is under the `docs` directory and uses [GitHub Pages](https://help.github.com/en/github/working-with-github-pages/about-github-pages) for rendering. The documentation is a micro site written in [Markdown](https://en.wikipedia.org/wiki/Markdown) files and rendered using [Jekyll](https://jekyllrb.com/), the static web site generator supported by GitHub pages out of the box, along with the [Minimal Mistakes](https://mmistakes.github.io/minimal-mistakes/) theme. The site is available at [https://mooltiverse.github.io/nyx/](https://mooltiverse.github.io/nyx/).

The regular [Guide](https://mooltiverse.github.io/nyx/guide/) source files are in the `docs/_pages` directory while the [Examples](https://mooltiverse.github.io/nyx/examples/) sources are in `docs/_posts`. The [navigation](https://mmistakes.github.io/minimal-mistakes/docs/navigation/) is modelled in the `docs/_data/navigation.yml` file and it defines the navigation items you see on top of the site and the [sidebar](https://mmistakes.github.io/minimal-mistakes/docs/layouts/#custom-sidebar-navigation-menu).

When authoring content make sure that:

* the [front matter](https://jekyllrb.com/docs/front-matter/) of your pages is properly defined, with special regards to the *title*, [*layout*](https://mmistakes.github.io/minimal-mistakes/docs/layouts/), [*permalink*](https://mmistakes.github.io/minimal-mistakes/docs/pages/) (for pages), [*tags* and *categories*](https://mmistakes.github.io/minimal-mistakes/docs/layouts/#taxonomy-archives) (for posts and examples)
* content is properly formatted using the right [helpers](https://mmistakes.github.io/minimal-mistakes/docs/helpers/) and [utility classes](https://mmistakes.github.io/minimal-mistakes/docs/utility-classes/)

To test the site, get into the `docs` directory and run:

```shell script
$ bundle exec jekyll serve --watch
```

then open a browser window to [http://localhost:4000/](http://localhost:4000/) to see the changes (other options are [available](https://jekyllrb.com/docs/configuration/options/)). Also make sure that running `./gradlew build test` doesn't break.

To troubleshoot the documentation site or to set up your local environment it's worth knowing that the documentation site has been bootstrapped by installing Ruby and Jekyll by following the [Jekyll installation instructions](https://jekyllrb.com/docs/installation/) and [initialized using Bundler](https://jekyllrb.com/tutorials/using-jekyll-with-bundler/) then following the [Minimal Mistakes theme Quick Start Guide](https://mmistakes.github.io/minimal-mistakes/docs/quick-start-guide/).

### Contributing Code

When contributing code make sure that you also provide extensive tests for the changes you make and that those tests achieve a sufficient coverage.

### Coding conventions

This is open source software. Consider the people who will read your code, and make it look nice for them.

We also:

* use spaces instead of tabs
* document code pervasively so that it can serve as an example for future readers

### Technical debt

We manage [technical debt](https://en.wikipedia.org/wiki/Technical_debt) carefully either by using correct issue labels and commenting code with proper `TODO` comments, linking issues and other sources of informations. Every time we introduce or detect some technical debt we make sure it's properly managed.
