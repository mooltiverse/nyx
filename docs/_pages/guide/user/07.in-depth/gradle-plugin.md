---
title: Gradle Plugin
layout: single
toc: true
permalink: /guide/user/in-depth/gradle-plugin/
---

In this page you can find extra informations about the Nyx Gradle Plugin internals that may be useful for [troubleshooting]({{ site.baseurl }}/troubleshooting/).

See the [Quick Start]({{ site.baseurl }}{% link _pages/guide/user/01.quick-start/gradle-plugin.md %}) or [Using the Gradle plugin]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#using-the-gradle-plugin) for regular usage instructions.

## Performance improvements

### Deferred task creation

According to [configuration avoidance](https://docs.gradle.org/current/userguide/task_configuration_avoidance.html) the plugin *registers* the tasks to the gradle project instead of *creating* them. This means that Gradle will only instantiate them when needed to improve the overall performances and avoid unnecessary tasks to run when not needed.

For more see the [`register`](https://docs.gradle.org/current/javadoc/org/gradle/api/tasks/TaskContainer.html#register-java.lang.String-java.lang.Class-) and [`create`](https://docs.gradle.org/current/javadoc/org/gradle/api/tasks/TaskContainer.html#create-java.lang.String-java.lang.Class-) methods on the [`TaskContainer`](https://docs.gradle.org/current/javadoc/org/gradle/api/tasks/TaskContainer.html) class.
