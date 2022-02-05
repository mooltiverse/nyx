---
title: Gradle Plugin
layout: single
toc: true
permalink: /guide/user/quick-start/gradle-plugin/
---

In this section you can find instructions to get started using the Gradle plugin in minutes. Consider these your first steps based or a standard scenario but there is much more you can do and control with Nyx. For more on using the Gradle plugin see [Using the Gradle plugin]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#using-the-gradle-plugin).

We assume you already have a Gradle project in place and have base familiarity with it, otherwise take a look at the [Gradle User Manual](https://docs.gradle.org/current/userguide/userguide.html).

## Apply the plugin

First off, you need to apply the [plugin](https://plugins.gradle.org/plugin/com.mooltiverse.oss.nyx) (you're suggested to use the [plugin DSL](https://docs.gradle.org/current/userguide/plugins.html#sec:plugins_block)) and you do so by editing your `settings.gradle` file to add the new plugin:

```groovy
plugins {
  id "com.mooltiverse.oss.nyx" version "{{ site.data.nyx.version }}"
}
```

In this example we use the *settings plugin* as it's the suggested and safer way to use it. [Here]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#apply-the-plugin) you also have instructions to apply it as a regular *project plugin*.
{: .notice--success}

There is a very good reason to apply the plugin in the settings and is outlined [here]({{ site.baseurl }}{% link _posts/2020-01-01-the-gradle-version-project-property-is-unspecified.md %}). To make it short, you likely need the [`version`](https://docs.gradle.org/current/userguide/writing_build_scripts.html#sec:standard_project_properties) standard Gradle property to be available throughout your project and so you need it available before the build script runs. Configuring Nyx as a *settings plugin* does extactly that. If you use it as a regular plugin the `version` attribute may be set too late or be inconsistent.
{: .notice--info}

Then you add the `nyx` configuration block to the same `settings.gradle`, like:

```groovy
plugins {
  id "com.mooltiverse.oss.nyx" version "{{ site.data.nyx.version }}"
}

// other definitions here like project name etc...

nyx {
  preset = 'simple'
}
```

In this example the only configuration option is `preset`, which uses the [*simple* preset]({{ site.baseurl }}{% link _pages/guide/user/04.configuration-presets/simple.md %}).

[Presets]({{ site.baseurl }}{% link _pages/guide/user/04.configuration-presets/index.md %}) are a very powerful way of configuring Nyx with streamlined settings without dealing with configuration details. While Nyx gives you control on every detail with lots of configuration options, presets are the most effective way to avoid that complexity.
{: .notice--success}

See [here]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#configure-the-plugin) or the [configuration reference]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/index.md %}) for more informations on how to configure the Gradle plugin. There are also several [examples]({{ site.baseurl }}/examples/) available.
{: .notice--info}

Now run `gradle tasks` to make sure the above works. You should see the *Release tasks* section showing the new tasks along with a summary description of their intents:

All the examples in this page assume you're using the plain `gradle` command. If you're using the (recommended) [wrapper](https://docs.gradle.org/current/userguide/gradle_wrapper.html) just change instances of the `gradle` command with `./gradlew`.
{: .notice--info}

```bash
$ gradle tasks
> Task :tasks

[...]

Release tasks
-------------
nyxClean - Deletes local release artifacts and reverts the release process to its initial state
nyxInfer - Collects informations from the local Git repository to generate the new version and plan the release actions
nyxMake - Builds the configured local release artifacts
nyxMark - Marks the release by tagging and committing the repository
nyxPublish - Publishes the new release to remote services and emits notifications
release - Runs all the release tasks

[...]

BUILD SUCCESSFUL in 280ms
1 actionable task: 1 executed
```

As you can see you now have six tasks available. This means the plugin is properly set up.

## Configuration

As you've seen above, in this example we are using a very simple configuration to only use the [*simple* preset]({{ site.baseurl }}{% link _pages/guide/user/04.configuration-presets/simple.md %}):

```groovy
nyx {
  preset = 'simple'
}
```

This preset:

* configures the [Conventional Commits](https://www.conventionalcommits.org/) convention so that Nyx knows how to infer release informations from the Git commit history
* configures two [release types]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}): the *mainline* for official releases managed through the main branch (`main` or `master`) and another one for any other branch for *internal* use

Other features like the changelog generation, custom version identifiers, environment variables constraints, complex branching strategies etc are not enabled.

With this configuration, every time you run Nyx and there are new unreleased commits whose messages describe a significant change (according to [Conventional Commits](https://www.conventionalcommits.org/)), Nyx will generate a new version, tag the latest commit in the current branch and push it to the remote repository. Publishing to remote services like [GitHub](https://github.com/) and [GitLab](https://gitlab.com/) may happen if you just extend the configuration by adding a section to the [`services`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/services.md %}).

As you can see, presets are quite powerful when it comes to avoid configuration complexity. Other presets with extended configurations are available but for now let's stick with this simple one.

Assuming you now want to add the configuration to publish your artifacts to [GitHub](https://github.com/), you can extend the configuration like this:

```groovy
nyx {
  preset = 'simple'

  releaseTypes {
    publicationServices = [ 'github' ]
  }
  services {
    github {
      type = 'GITHUB'
      options {
        // The authentication token is read from the GH_TOKEN environment variable.
        AUTHENTICATION_TOKEN = '{% raw %}{{#environment.variable}}GH_TOKEN{{/environment.variable}}{% endraw %}'
        REPOSITORY_NAME = ... // your project name here
        REPOSITORY_OWNER = ... // your user or organization name here
      }
    }
  }
}
```

As you can see, we are still using the *simple* preset as a baseline, just extending it with new options. You can see the [configuration reference]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/index.md %}) to see how powerful the configuration mechanism is but for now let's stay simple.

You've just been introduced to an extremely powerful feature in Nyx: [templates]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}). In this case the `environment.variable` is used to read the value of the `GH_TOKEN` variable at runtime but many others are available. By using templates your configuration can be dynamic and adaptive.
{: .notice--success}

## Running the tasks

Let's resume from the tasks available from the Nyx plugin. The *release* task is a [*lifecycle task*](https://docs.gradle.org/current/userguide/more_about_tasks.html#sec:lifecycle_tasks) that is only configured if no other task with the same name already exists and is a shorthand for running all the other *core* tasks (except *nyxClean*).

The *nyxClean* task deletes the local artifacts (like a changelog file) that might have been created by Nyx in previous runs, otherwise it does nothing. If you have a global *clean* task, it's a good idea to make it dependent on *nyxClean*, like:

```groovy
tasks.clean.dependsOn tasks.nyxClean
```

or

```groovy
tasks.register('clean') {
    dependsOn 'nyxClean'
    ...
}
```

The other *core tasks* (*nyxInfer*, *nyxMake*, *nyxMark*, *nyxPublish*), instead, perform some actions incrementally, which means their dependencies are chained to make sure that *nyxInfer* is always executed first when the others run, *nyxMake* runs before *nyxMark* and *nyxPublish* only runs after all the others have completed. On the other hand, they are all available as single tasks so you can customize your build workflow to meet any needs (i.e. by setting task dependencies as above).

Please note that when using the *settings plugin* (applied in the `settings.gradle` file), using the `nyxInfer` task is not needed (unless your Git repository changes throughout the build process) as it is executed during the project initialization phase.
{: .notice--info}

*nyxInfer* scans the commit history according to the configured commit message convention ([Conventional Commits](https://www.conventionalcommits.org/) as it's defined in the [*simple* preset]({{ site.baseurl }}{% link _pages/guide/user/04.configuration-presets/simple.md %})) and determines the project [version](https://docs.gradle.org/current/userguide/writing_build_scripts.html#sec:standard_project_properties), which may be a new one if there are significant and unreleased commits, or the latest (already tagged) if there are no new significant commits. *nyxInfer* also instantiates and makes available the [state]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/index.md %}) object, that you can inspect to get lots of useful informations for any custom purpose.

The way the new version is generated depends on what has been found in the release scope and the configuration. You can find a detailed description on how this process works in the following sections but generally speaking, the version is determined by *bumping* an indentifier against the *previous version* and (optionally) adding some extra qualifiers. Determining the identifiers to bump depends on the configured commit message convention and the release type. Nyx covers all use cases about version bumping, from manual override to linear increments up to *pre-releases* using the *collapsed* versioning. You may wish to start with some [examples]({{ site.baseurl }}{% link _posts/2020-01-01-git-history-examples.md %}) on the various cases.

At this stage, no tags are applied nor any other change is made as Nyx has only performed inspections. Nonetheless, the `version` (or `project.version`, or `rootProject.version`) property is set for use in Gradle scripts.

Running *nyxMake* has no effect with the [*simple* preset]({{ site.baseurl }}{% link _pages/guide/user/04.configuration-presets/simple.md %}) configuration but if you add a changelog generation to you configuration, here is where the local changelog file would be created. For example, adding this snippet to your configuration:

```groovy
nyx {
  preset = 'simple'

  changelog {
    path = 'build/CHANGELOG.md'
  }
}
```

creates the `CHANGELOG.md` file in your `build` folder if, and only if, a new version has been generated by *nyxInfer*.

Assuming *nyxInfer* has determined that a new version is going to be issued, *nyxMark* tags the local repository with the new version and pushes changes to the remote repositories. Uncommitted changes (including a changelog file) are not committed because the [*simple* preset]({{ site.baseurl }}{% link _pages/guide/user/04.configuration-presets/simple.md %}) has the `gitCommit` flag disabled, but that would be easily enabled by overriding the flag like:

```groovy
nyx {
  preset = 'simple'

  releaseTypes {
    items {
      mainline {
        ...
        gitCommit = 'true'
      }
    }
  }
}
```

Finally, *nyxPublish* is when the new release is published. As we've already seen, for this to actually happen you need to configure Nyx so it knows which services to use for your own project. Let's recall the example of using [GitHub](https://github.com/)

```groovy
nyx {
  preset = 'simple'

  releaseTypes {
    publicationServices = [ 'github' ]
    items {
      mainline {
        ...
        gitCommit = 'true'
      }
    }
  }
  services {
    github {
      type = 'GITHUB'
      options {
        // The authentication token is read from the GH_TOKEN environment variable.
        AUTHENTICATION_TOKEN = '{% raw %}{{#environment.variable}}GH_TOKEN{{/environment.variable}}{% endraw %}'
        REPOSITORY_NAME = ... // your project name here
        REPOSITORY_OWNER = ... // your user or organization name here
      }
    }
  }
}
```

In this case the new release would be published as a [GitHub Release](https://docs.github.com/en/github/administering-a-repository/about-releases), and if you generated a changelog (see example above) you could also use the changelog as the release description by setting the `description` option for the release type, like:

```groovy
nyx {
  preset = 'simple'

  changelog {
    path = 'build/CHANGELOG.md'
  }
  releaseTypes {
    publicationServices = [ 'github' ]
    items {
      mainline {
        ...
        description = '{% raw %}{{#file.content}}build/CHANGELOG.md{{/file.content}}{% endraw %}'
        gitCommit = 'true'
      }
    }
  }
  services {
    github {
      type = 'GITHUB'
      options {
        // The authentication token is read from the GH_TOKEN environment variable.
        AUTHENTICATION_TOKEN = '{% raw %}{{#environment.variable}}GH_TOKEN{{/environment.variable}}{% endraw %}'
        REPOSITORY_NAME = ... // your project name here
        REPOSITORY_OWNER = ... // your user or organization name here
      }
    }
  }
}
```

By this example you can also see how to use [templates]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}#filecontent) to dynamically embed the content of an external file into a configuration option.

[Here]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#core-tasks) you have the detailed description of each task.

## Nyx State

Now that you've seen the main features provided by Nyx is time to embed it into your build scripts. Let's assume your build script has other two tasks: `myBuildTask` and `myReleaseTask`. You want to make `myBuildTask` dependent on the `nyxPublish` task and `myReleaseTask` executed only when Nyx determines a new version has to be published.

For this last part you can access the internal Nyx [state]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/index.md %}), which is available as `nyxState` at the root project level. So your build script may look like this:

```groovy
// make myBuildTask depend on nyxPublish
tasks.myBuildTask.dependsOn tasks.nyxPublish

// nyxState.newRelease is true only when a new version has been generated and it must be published
tasks.myReleaseTask.onlyIf { rootProject.nyxState.newRelease }
```

The State object gives you plenty of information you can use in your build scripts to exploit the full potential of Nyx.

## Recap

We have seen:

* how to set up the plug-in (as a *settings* or a *regular* plug-in) and why the *settings* plugin should be preferred
* how the [`version`](https://docs.gradle.org/current/userguide/writing_build_scripts.html#sec:standard_project_properties) standard Gradle property is managed by Nyx for seamless integration with your build scripts
* the configuration means and the power of [presets]({{ site.baseurl }}{% link _pages/guide/user/04.configuration-presets/index.md %})
* how [templates]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) can be used to make configurations even more flexible and dynamic
* the tasks provided by Nyx and their features, like inspecting the commit history to detect new versions, generate changelogs, commit, tag and push Git changes, publish releases
* how to fetch Nyx internal informations from the [state]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/index.md %}) object

This is just a first introduction on the features provided by the tool! Jump to the [introduction]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/index.md %}) if you want to know more!

Enjoy!
