---
title: Gradle Plugin
layout: single
toc: true
permalink: /guide/user/quick-start/gradle-plugin/
---

In this section you can find instructions to get started using the Gradle plugin in minutes. Consider these your first steps based or a standard scenario but there is much more you can do and control with Nyx. For more on using the Gradle plugin see [Using the Gradle plugin]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#using-the-gradle-plugin).

We assume you already have a Gradle project in place and have base familiarity with it, otherwise take a look at the [Gradle User Manual](https://docs.gradle.org/current/userguide/userguide.html).

## Apply and configure the plugin

First off, you need to apply the [plugin](https://plugins.gradle.org/plugin/com.mooltiverse.oss.nyx) (you're suggested to use the [plugin DSL](https://docs.gradle.org/current/userguide/plugins.html#sec:plugins_block)) and you do so by editing your `build.gradle` file to add the new plugin:

```groovy
plugins {
  id "com.mooltiverse.oss.nyx" version "{{ site.data.nyx.version }}"
}
```

Then you add the `nyx` configuration block to the same `build.gradle`, to make it look like:

```groovy
plugins {
  id "com.mooltiverse.oss.nyx" version "{{ site.data.nyx.version }}"
}

nyx {
  // configuration options here...
  // TODO: add the bare minimum options here
}
```

Now run `gradle tasks` to make sure the above works. You should see the *Release tasks* section showing the new tasks along with a summary description of their intents:

```bash
$ gradle tasks
> Task :tasks

[...]

Release tasks
-------------
nyxArrange - Integrates the local repository history with extra information from remotes
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

TODO: complete this section with standard configuration options
{: .notice--warning}

TODO: complete this section with tasks and their dependencies and how to set custom task dependencies un user scripts
{: .notice--warning}

## Releasing

TODO: write this section
{: .notice--warning}
