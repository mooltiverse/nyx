# Contributing

We're happy you're reading this and willing to contribute! We also appreciate if you read through this document to help us accepting your contributions.

This project and everyone participating in it is governed by the [Contributor Covenant Code of Conduct](CODE_OF_CONDUCT.md). By participating, you are expected to uphold this code.

## Issues

Feel free to open new [issues](https://github.com/mooltiverse/nyx/issues) for bugs and support requests. Before doing so we ask you to:

1. search for similar requests that might have been opened in the past to avoid repetitiveness
2. read through the [docs](https://mooltiverse.github.io/nyx/) and see if a similar was already addressed. Also read through the [examples](https://mooltiverse.github.io/nyx/examples/)
3. make sure you're running the [latest release](https://github.com/mooltiverse/nyx/releases)

If you can't find any answer, when submitting a new issue please provide as much information as you can to help us understand. Don't forget to:

* include a **clear title and description**
* give as much relevant information as possible, which may include a code snippet or an executable test case
* give instructions on how to reproduce the problem
* describe the expected behavior and the current behavior
* tell us about the kind of deliverable you're using and their version and describe the environment so we can reproduce it

We will do our best to take relevant requests and update the documentation or publish a new [example post](https://mooltiverse.github.io/nyx/examples/) about the specific use case in order to make it reusable by others.

### Version scheme

This project is versioned according to [Semantic Versioning](https://semver.org/).

## Code and Documentation

If you're going to contribute with code or documentation please read through the following sections. When contributing to this repository, please first discuss the change you wish to make via issue, email, or any other method with the repository owners.

### Branching strategy and commits

This project uses the [GitHub flow](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/github-flow) branching strategy. Before contributing make sure you have a clear understanding of it.

### Commit messages

We use [Conventional Commits](https://www.conventionalcommits.org/) so please give commit messages the same format, like:

```text
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
```

One-line messages are fine for small changes, for example:

```bash
$ git commit -m "feat: brand new feature added"
```

while for bigger changes additional description in the body is welcome, like:

```bash
$ git commit -m "feat: brand new feature added
>
> Here is the detailed description of what has changed."
```

Allowed *types* are:

* `feat`: new features
* `fix`: a bug fix
* `build`: changes that affect the build system or external dependencies
* `chore`: various changes not falling into other categories
* `ci`: changes to CI configuration files and scripts
* `docs`: documentation only changes
* `style`: changes that do not affect the meaning of the code but just its coding stye
* `refactor`: code changes that neither fix bugs nor add features
* `perf`: changes affecting performances
* `test`: changes on the test code and suites

Comments using the `feat` type will bump the [minor](https://semver.org/) number while those using the `fix` type bump the [patch](https://semver.org/) number. Other types do not bump any version number.

Breaking changes must have an exclamation mark at the end of the *type* (i.e. `feat!: brand new feature added`) or a body line starting with `BREAKING CHANGE:` (i.e. `BREAKING CHANGE: this feature breaks backward compatibility`). Breaking changes will bump the [major](https://semver.org/) number.

Allowed *scopes* are:

* `config`: the change affects the configuration
* `core`: the change affects the core logic

Other scopes will be added in the future.

### Prerequisites

You can work on the project on any platform (Linux, Windows, Mac). You need to have installed:

* [Git](https://git-scm.com/)
* [Go](https://go.dev/) 1.17 or above
* Java JDK or JRE version 8 or above, either from [Oracle](https://www.oracle.com/java/technologies/javase-downloads.html) or [OpenJDK](https://openjdk.java.net/install/)
* [Gradle](https://gradle.org/install/) 7.x or above
* [Docker CE](https://docs.docker.com/install/)
* [Jekyll](https://jekyllrb.com/docs/installation/) 4.2.0 or above (also requires [Ruby](https://www.ruby-lang.org/en/downloads/) 2.4.0 or above)

You also need a local copy of the repository that you can get by running:

```shell script
$ git clone https://github.com/mooltiverse/nyx.git
```

You can use any IDE, just make sure you don't clutter the repo with IDE files.

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
|   \--- go                         # Go sub modules
|        +--- main                  # Go Main library sub module
|        \--- version               # Go Version library sub module
|   \--- java                       # Java sub modules
|        +--- gradle                # Gradle plugin sub module
|        +--- main                  # Java Main library sub module
|        \--- version               # Java Version library sub module
+-- build.gradle                    # Gradle build script (main)
+-- gradle.properties               # Project and system properties
+-- gradlew                         # Gradle wrapper
+-- gradlew.bat                     # Gradle wrapper (for Windows)
\-- settings.gradle                 # Gradle project and sub-modules definition
```

Git [ignored](.gitignore) and non relevant items are omitted from the list. All sub-modules have a `build.gradle` script file and a `src` directory containing the [source sets](https://docs.gradle.org/current/dsl/org.gradle.api.tasks.SourceSet.html), not shown above.

### Build

Use the [Gradle wrapper](https://docs.gradle.org/current/userguide/gradle_wrapper.html) to run Gradle, like:

```shell script
$ ./gradlew [TASK]    # on Linux and Mac
> gradlew [TASK]      # on Windows
```

where the main tasks are:

* `clean` to restore the entire project directory to its initial state
* `assemble` to build the project artifacts
* `test` to run unit tests
* `check` to run all tests (unit, integration and functional)
* `build` to build the entire project and run all tests
* `publish` to publish project outcomes to distribution services
* `release` to tag the current version and publish it as a release

The `publish` and `release` tasks can only be executed on the CI/CD platform.

### Contributing Documentation

The documentation is under the `docs` directory and uses [GitHub Pages](https://help.github.com/en/github/working-with-github-pages/about-github-pages) for rendering. The documentation is a micro site written in [Markdown](https://en.wikipedia.org/wiki/Markdown) files and rendered using [Jekyll](https://jekyllrb.com/), the static web site generator supported by GitHub pages out of the box, along with the [Minimal Mistakes](https://mmistakes.github.io/minimal-mistakes/) theme. The site is available at [https://mooltiverse.github.io/nyx/](https://mooltiverse.github.io/nyx/).

The regular content files are in the `docs/_pages` directory while the examples, F.A.Qq. and other post sources are in `docs/_posts`. The [navigation](https://mmistakes.github.io/minimal-mistakes/docs/navigation/) is modelled in the `docs/_data/navigation.yml` file and it defines the navigation items you see on top of the site and the [sidebar](https://mmistakes.github.io/minimal-mistakes/docs/layouts/#custom-sidebar-navigation-menu).

Images are in `docs/assets` and vector graphics (`.svg`) are preferred over other formats. Diagrams and charts have been edited with [Lucid tools](https://lucid.app/) (*Lucidspark* and *Lucidchart*).

When authoring content make sure that:

* the [front matter](https://jekyllrb.com/docs/front-matter/) of your pages is properly defined, with special regards to the *title*, [*layout*](https://mmistakes.github.io/minimal-mistakes/docs/layouts/), [*permalink*](https://mmistakes.github.io/minimal-mistakes/docs/pages/) (for pages), [*tags* and *categories*](https://mmistakes.github.io/minimal-mistakes/docs/layouts/#taxonomy-archives) (for posts and examples)
* content is properly formatted using the right [helpers](https://mmistakes.github.io/minimal-mistakes/docs/helpers/) and [utility classes](https://mmistakes.github.io/minimal-mistakes/docs/utility-classes/)
* you have a basic knowledge of [Kramdown](https://kramdown.gettalong.org/), the Markdown superset used by Jekyll

To test the site, get into the `docs` directory and run:

```shell script
$ bundle update
$ bundle exec jekyll serve --watch
```

then open a browser window to [http://localhost:4000/](http://localhost:4000/) to see the changes (other options are [available](https://jekyllrb.com/docs/configuration/options/)). Also make sure that running `./gradlew build test` doesn't break.

To troubleshoot the documentation site or to set up your local environment it's worth knowing that the documentation site has been bootstrapped by installing Ruby and Jekyll by following the [Jekyll installation instructions](https://jekyllrb.com/docs/installation/) and [initialized using Bundler](https://jekyllrb.com/tutorials/using-jekyll-with-bundler/) then following the [Minimal Mistakes theme Quick Start Guide](https://mmistakes.github.io/minimal-mistakes/docs/quick-start-guide/).

### Contributing Code

When contributing code make sure that you also provide extensive tests for the changes you make and that those tests achieve a sufficient coverage.

##### Narrowing the test sets for daily activities

Some tests, specifically among functional tests, are very extensive and may take hours to complete. While running the entire suite of tests is critical and must always be performed for any release, during daily work we can run a reduced, yet significant, set of tests. For example we can run tests against the latest version of Gradle only rather than any supported version.

To limit the number of tests to a smaller set you can run setting the `quickTests` property to `true` when launching the Gradle script, like:

```shell script
./gradlew -PquickTests=true functionaltest
```

##### Testing against remote repositories

Since Nyx supports remote repositories and their extra features, tests must be provided against those features as well. To do so we need a test user on each platform along with its credentials:

* [GitHub Nyx Test User](https://github.com/nyxtest20200701) for [GitHub](https://github.com/)
* [GitLab Nyx Test User](https://gitlab.com/nyxtest20200701) for [GitLab](https://gitlab.com/)

**Whenever possible use OAuth tokens or Personal Access Tokens instead of plain user name and passwords**.

These accounts are used to to dynamically create repositories, fiddle with them and clean up at the end of a test run. Complete cleanup is performed at the end of tests so when tests complete successfully there should be no contents left on the above accounts. In case some tests fail some stale repository might remain and can be deleted manually from the UI once logged in as the test user.

These users are not connected to any team or organization so they do not expose any sensitive information. In case they get compromised we can replace them with some new accounts at any time.

Main credentials for these users are managed by Nyx project owners but they are also safely passed to [GitHub Actions jobs](.github/workflows/github-ci.yml) so that they can be used by CI/CD.

When testing locally you can still run tests by passing credentials for your own users (never use your personal accounts, create additional test users instead) and pass their credentials to Gradle scripts as environment variables or system properties. See the [Gradle Build Environment](https://docs.gradle.org/current/userguide/build_environment.html) for more.

Example using a local `gradle.properties` file in the `GRADLE_USER_HOME` (which has to be defined) directory with content:

```properties
# GitHub test user
gitHubTestUserName=nyxtest20200701
gitHubTestUserToken=<token goes here>

# GitLab test user
gitLabTestUserName=nyxtest20200701
gitLabTestUserToken=<token goes here>
```

Examples using the command line or environment variables:

```shell script
# Example 1: pass the credentials as system properties on the Gradle command line
$ ./gradlew -PgitHubTestUserToken=<GITHUB_TOKEN> -PgitLabTestUserToken=<GITLAB_TOKEN> test
```

```shell script
# Example 2: pass the credentials as environment variables
export ORG_GRADLE_PROJECT_gitHubTestUserToken=<GITHUB_TOKEN>
export ORG_GRADLE_PROJECT_gitLabTestUserToken=<GITLAB_TOKEN>
$ ./gradlew test
```

```shell script
# Example 3: use an additional gradle.properties in your $GRADLE_USER_HOME where you define the above properties

# This is not requested if you already have the GRADLE_USER_HOME variable already defined. It can point wherever you want
export GRADLE_USER_HOME=~

# Create or edit the $GRADLE_USER_HOME/gradle.properties with the properties like these
gitHubTestUserToken=<GITHUB_TOKEN>
gitLabTestUserToken=<GITLAB_TOKEN>

# Run Gradle as normal
$ ./gradlew test
```

For more on the above options see the [Gradle Build Environment](https://docs.gradle.org/current/userguide/build_environment.html). In any case, **never store your credentials along with the project files**.

### Coding conventions

This is open source software. Consider the people who will read your code, and make it look nice for them.

We also:

* use spaces instead of tabs
* document code pervasively so that it can serve as an example for future readers

### Technical debt

We manage [technical debt](https://en.wikipedia.org/wiki/Technical_debt) carefully either by using correct issue labels and commenting code with proper `TODO` comments, linking issues and other sources of informations. Every time we introduce or detect some technical debt we make sure it's properly managed.

### IDE support

#### Eclipse and Visual Studio Code

Eclipse and VSCode use the `.project` and `.project` files to define how the project is built. In order to keep these files in sync with Gradle (which is still the primary source of information for build scripts and classpaths) you can run the `eclipse` [task](https://docs.gradle.org/current/userguide/eclipse_plugin.html) in advance. You can also run the `cleanEclipse` to clean previous files.

Example:

```shell script
# Clean previous files
$ ./gradlew cleanEclipse

# Generate the files from Gradle definitions
$ ./gradlew eclipse
```

Because these files define local specific paths they are ignored by Git and they need to be generated on the local environment.

Also note that since the project is split into different modules, different copies of these files are available in the different sub-project directories.
