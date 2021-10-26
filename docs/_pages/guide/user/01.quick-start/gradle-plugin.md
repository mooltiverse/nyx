---
title: Gradle Plugin
layout: single
toc: true
permalink: /guide/user/quick-start/gradle-plugin/
---

In this section you can find instructions to get started using the Gradle plugin in minutes. Consider these your first steps based or a standard scenario but there is much more you can do and control with Nyx. For more on using the Gradle plugin see [Using the Gradle plugin]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#using-the-gradle-plugin).

We assume you already have a Gradle project in place and have base familiarity with it, otherwise take a look at the [Gradle User Manual](https://docs.gradle.org/current/userguide/userguide.html).

## Apply and configure the plugin

First off, you need to apply the [plugin](https://plugins.gradle.org/plugin/com.mooltiverse.oss.nyx) (you're suggested to use the [plugin DSL](https://docs.gradle.org/current/userguide/plugins.html#sec:plugins_block)) and you do so by editing your `settings.gradle` file to add the new plugin:

```groovy
plugins {
  id "com.mooltiverse.oss.nyx" version "{{ site.data.nyx.version }}"
}
```

In this example we use the *settings plugin* as it's the suggested and safer way to use it. [Here]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#apply-the-plugin) you also have instructions to apply it as a regular *project plugin*.
{: .notice--success}

Then you add the `nyx` configuration block to the same `settings.gradle`, to make it look like:

```groovy
plugins {
  id "com.mooltiverse.oss.nyx" version "{{ site.data.nyx.version }}"
}

// other definitions here like project name etc...

nyx {
  preset = 'simple'
}
```

In this example the only configuration option is `preset`, which uses the [*simple* preset]({{ site.baseurl }}{% link _pages/guide/user/04.configuration-presets/simple.md %}). For more informations on how to configure the Gradle plugin see [here]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#configure-the-plugin) or the [configuration reference]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/index.md %}). There are also several [examples]({{ site.baseurl }}/examples/) available.

Now run `gradle tasks` to make sure the above works. You should see the *Release tasks* section showing the new tasks along with a summary description of their intents:

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

See [here]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#core-tasks) for the detailed description of each task.

Please note that when using the *settings plugin* (applied in the `settings.gradle` file), using the `nyxInfer` task is not needed (unless your Git repository changes throughout the build process) as it is executed during the project initialization phase.

## Releasing

All is left to do is to run the tasks provided by the plugin from your build scripts. You can run [core tasks]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#core-tasks) like `nyxClean`, `nyxInfer`, `nyxMake`, `nyxMark`, `nyxPublish` or, for a cleaner script, the `release` [lifecycle task]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#lifecycle-tasks):

```bash
$ gradle release
Project version: 1.2.3-alpha.1

[...]

BUILD SUCCESSFUL in 280ms
1 actionable task: 1 executed
```

But actual Gradle scripts are more complex than this. Let's assume your build script has other two tasks: `myBuildTask` and `myReleaseTask`. You want to make `myBuildTask` dependent on the `nyxPublish` task and `myReleaseTask` executed only when Nyx determines a new version has to be published.

For this last part you can access the internal Nyx [state]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/index.md %}), which is available as `nyxState` at the root project level. So your build script may look like this:

```groovy
// make myBuildTask depend on nyxPublish
tasks.myBuildTask.dependsOn tasks.nyxPublish

// nyxState.newRelease is true only when a new version has been generated and it must be published
tasks.myReleaseTask.onlyIf { rootProject.nyxState.newRelease }
```
