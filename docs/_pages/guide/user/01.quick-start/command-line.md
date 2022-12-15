---
title: Command Line
layout: single
toc: true
permalink: /guide/user/quick-start/command-line/
---

In this section you can find instructions to get started using the command line in minutes. Consider these your first steps based or a standard scenario but there is much more you can do and control with Nyx. For more see [Using the command line]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#using-the-command-line).

## Download Nyx

You can find the latest binaries for any platform in the [latest release](https://github.com/mooltiverse/nyx/releases/latest) assets.

Make sure you store the binary so that it's available from the `PATH` and it has execution permissions.

This guide assumes you rename the executable as `nyx`, regardless of the platform (or `nyx.exe` on Windows).
{: .notice--info}

## Synopsis

Nyx comes with a whole lot of arguments that you can pass on the command line to let you control every single aspect of its behavior. You can see them all by running `nyx --help`, from which we have an abbreviated output here:

```bash
nyx --help

Nyx version: {{ site.data.nyx.version }}

Usage:
    nyx [arguments] [command]

Commands are:
    clean               reverts the repository to its initial state and removes files created by other commands, if any
    infer               inspects the commit history and repository status and computes the project version
    make                produces artifacts (i.e. changelog) as per the configuration
    mark                commits, tags and pushes, according to the configuration and the repository status
    publish             publish the new release, if any, to the configured services

Global arguments are:
    [...]

Changelog arguments are:
    [...]

Commit Message Conventions arguments are:
    [...]

Git arguments are:
    [...]

Git arguments are:
    [...]

Release Type arguments are:
    [...]

Services arguments are:
    [...]
```

As you can see you can give as much arguments as you like and one command at the end. Arguments start with a single (`-`) or double dash (`--`) and are detailed in the full output from `nyx --help` and in the [configuration reference]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/index.md %}#configuration-file).

The command needs to appear at the end of the command line with no dash sign. If you don't specify any command Nyx will run the default one `infer`. For an introduction on what every command does see [below](#commands) or [How Nyx Works]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}).

### Dynamic argument names

One thing that is important to note is that many arguments names are dynamic and this may be confusing at first as it's different than common arguments on other tools. For example, the [`--services-<NAME>-type=<TYPE>`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/services.md %}#type) argument has a dynamic `<NAME>` in the attribute name.

This is to allow users to define the `<TYPE>` value for arbitrary services whose name is dynamically configured. When passing this as an actual argument on the command line also the `<NAME>` part needs to be replaced so for example `--services-gh-type=GITHUB` sets the type `GITHUB` for a service instance configured under the name `gh`.

## Configuration

To start easy, you can get into your project directory and run:

```bash
nyx --preset=simple
```

In this example we are skipping the configuration complexities and use the [*simple* preset]({{ site.baseurl }}{% link _pages/guide/user/04.configuration-presets/simple.md %}), which:

* configures the [Conventional Commits](https://www.conventionalcommits.org/) convention so that Nyx knows how to infer release information from the Git commit history
* configures two [release types]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}): the *mainline* for official releases managed through the main branch (`main` or `master`) and another one for any other branch for *internal* use

Other features like the changelog generation, custom version identifiers, environment variables constraints, complex branching strategies etc are not enabled.

With this configuration, every time you run Nyx and there are new unreleased commits whose messages describe a significant change (according to [Conventional Commits](https://www.conventionalcommits.org/)), Nyx will generate a new version, tag the latest commit in the current branch and push it to the remote repository. Publishing to remote services like [GitHub](https://github.com/) and [GitLab](https://gitlab.com/) may happen if you just extend the configuration by adding a section to the [`services`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/services.md %}).

As you can see, presets are quite powerful when it comes to avoid configuration complexity. Other presets with extended configurations are available but for now let's stick with this simple one.

Assuming you now want to add the configuration to publish your artifacts to [GitHub](https://github.com/), you can extend the configuration by passing additional arguments like:

```bash
nyx --preset=simple --services-github-type=GITHUB --services-github-options-AUTHENTICATION_TOKEN="<TOKEN>" --services-github-options-REPOSITORY_NAME="<REPO_NAME>" --services-github-options-REPOSITORY_OWNER="<REPO_OWNER>" 
```

In this section we only show configuration on the command line. However you can set the configuration in JSON or YAML files and even combine multiple files with command line arguments, environment variables etc as you can see by the [`--configuration-file`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#configuration-file).
{: .notice--info}

Here we are still using the *simple* preset as a baseline, just extending it with new options. You can see the [configuration reference]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/index.md %}) to see how powerful the configuration mechanism is but for now let's stay simple.

## Commands

In the above example we didn't specify which command to run so Nyx runs the default [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) command. To run a specific command just pass it on the command line after other arguments and flags.

The *clean* command deletes the local artifacts (like a changelog file) that might have been created by Nyx in previous runs, otherwise it does nothing.

The other *core commands* (*infer*, *make*, *mark*, *publish*), instead, perform some actions incrementally, which means their dependencies are chained to make sure that *infer* is always executed first when the others run, *make* runs before *mark* and *publish* only runs after all the others have completed. On the other hand, they are all available as single commands so you can customize your build workflow to meet any needs (i.e. by performing other actions in between two Nyx commands).

*infer* scans the commit history according to the configured commit message convention ([Conventional Commits](https://www.conventionalcommits.org/) as it's defined in the [*simple* preset]({{ site.baseurl }}{% link _pages/guide/user/04.configuration-presets/simple.md %})) and determines the project [version](https://docs.gradle.org/current/userguide/writing_build_scripts.html#sec:standard_project_properties), which may be a new one if there are significant and unreleased commits, or the latest (already tagged) if there are no new significant commits. *infer* also instantiates and makes available the [state]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/index.md %}) object, that you can inspect to get lots of useful information for any custom purpose.

The way the new version is generated depends on what has been found in the release scope and the configuration. You can find a detailed description on how this process works in the following sections but generally speaking, the version is determined by *bumping* an indentifier against the *previous version* and (optionally) adding some extra qualifiers. Determining the identifiers to bump depends on the configured commit message convention and the release type. Nyx covers all use cases about version bumping, from manual override to linear increments up to *pre-releases* using the *collapsed* versioning. You may wish to start with some [examples]({{ site.baseurl }}{% link _posts/2020-01-01-git-history-examples.md %}) on the various cases.

At this stage, no tags are applied nor any other change is made as Nyx has only performed inspections.

Running *make* has no effect with the [*simple* preset]({{ site.baseurl }}{% link _pages/guide/user/04.configuration-presets/simple.md %}) configuration but if you add a changelog generation to you configuration, here is where the local changelog file would be created. For example, adding these arguments to you command line:

```bash
nyx --preset=simple --changelog-path=build/CHANGELOG.md make
```

creates the `CHANGELOG.md` file in your `build` folder if, and only if, a new version has been generated by *infer* (which is implicitly executed as *make* depends on it).

Assuming *infer* has determined that a new version is going to be issued, *mark* tags the local repository with the new version and pushes changes to the remote repositories. Uncommitted changes (including a changelog file) are not committed because the [*simple* preset]({{ site.baseurl }}{% link _pages/guide/user/04.configuration-presets/simple.md %}) has the `gitCommit` flag disabled, but that would be easily enabled by overriding the flag like:

```bash
nyx --preset=simple --changelog-path=build/CHANGELOG.md --release-types-mainline-git-commit=true mark
```

Here with the `--release-types-mainline-git-commit=true` we override a single option for the *mainline* release type that is defined by the [*simple* preset]({{ site.baseurl }}{% link _pages/guide/user/04.configuration-presets/simple.md %}) so that the *mark* command commits changes.

Finally, *publish* is when the new release is published. For this to actually happen you need to tell Nyx so it knows which services to use for your own project. Let's recall the example of using [GitHub](https://github.com/):

```bash
nyx --preset=simple --changelog-path=build/CHANGELOG.md --release-types-mainline-git-commit=true --release-types-mainline-description="{% raw %}{{#fileContent}}build/CHANGELOG.md{{/fileContent}}{% endraw %}" --release-types-publication-services=github --services-github-type=GITHUB --services-github-options-AUTHENTICATION_TOKEN="<TOKEN>" --services-github-options-REPOSITORY_NAME="<REPO_NAME>" --services-github-options-REPOSITORY_OWNER="<REPO_OWNER>" publish
```

In this case the new release would be published as a [GitHub Release](https://docs.github.com/en/github/administering-a-repository/about-releases), and if you generated a changelog (see example above) you also use the changelog as the release description by setting the `description` option for the release type.

By this example you can also see how to use [templates]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}#filecontent) to dynamically embed the content of an external file into a configuration option. In this case the `fileContent` is used to read the value of the `build/CHANGELOG.md` file at runtime but many others are available. By using templates your configuration can be dynamic and adaptive.

[Here]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}) you have the detailed description of each task.

## Nyx State

Now that you've seen the main features provided by Nyx is time to take a step further and leverage the information that Nyx infers and produces so you can make use of them elsewhere. Let's assume you want to know if a new version has to be published.

You can enable writing the Nyx [state]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/index.md %}) file by using the [`--state-file`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#state-file) argument. You can have it in JSON or YAML, depending on the extension you set. Example:

```bash
nyx --state-file=nyx-state.json infer
```

After this command runs you can read the `nyx-state.json` file to inspect its contents and use them for other purposes. Check out the [State Reference]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/index.md %}) to know more about the contents and how powerful this can be.

## Recap

We have seen:

* how to download the binaries
* the configuration means and the power of [presets]({{ site.baseurl }}{% link _pages/guide/user/04.configuration-presets/index.md %})
* how [templates]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) can be used to make configurations even more flexible and dynamic
* the commands provided by Nyx and their features, like inspecting the commit history to detect new versions, generate changelogs, commit, tag and push Git changes, publish releases
* how to fetch Nyx internal information from the [state]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/index.md %}) object

This is just a first introduction on the features provided by the tool! Jump to the [introduction]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/index.md %}) if you want to know more!

Enjoy!
