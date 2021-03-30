---
layout: single
title:  "The Gradle *version* project property is *unspecified*"
date:   2020-01-01 00:00:00 +0000
categories: troubleshooting user
tags: support configuration gradle version
---

When you use the [`version`](https://docs.gradle.org/current/userguide/writing_build_scripts.html#sec:standard_project_properties) project property and it yields to the `unspecified` value although you expected it to be something else the reason may be:

* you are in a multi project scenario and you're querying a sub-project `version` property instead of the root projects
* you have [applied]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#apply-the-plugin) as a *project plugin* instead of a *settings plugin*

`unspecified` is the default value for the [`version`](https://docs.gradle.org/current/userguide/writing_build_scripts.html#sec:standard_project_properties) standard Gradle property so it's what you get unless it is set to any other value either statically or dinamically.
{: .notice--info}

Here are solutions to these issues.

### Use the right `version` property or propagate

Let's assume you have a multi-project hierarchy like:

```text
rootProject
|- subProjectA
|  \- subProjectA1
\- subProjectB
```

When you get the right `version` in the `rootProject` but not in the sub projects that simply means that you need to replace the `project.version` expressions in your build scripts to `rootProject.version`. Do this for all the projects in the hierarchy you want the same project version for.

If you want all the projects in the hierarchy to have the same version you may go the other way so, in order to propagate the version from the root project to children, add the following to your root project build script:

```groovy
subprojects {
    version = rootProject.version
}
```

If you have [applied]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#apply-the-plugin) as a *project plugin* instead of a *settings plugin* you may prefer the `afterEvaluate` version, like:

```groovy
subprojects {
    afterEvaluate {
        version = rootProject.version
    }
}
```

Remember that even with this, when using the *project plugin* version, some properties may still be *unspecified*, as illustrated below.

### Apply the plugin at the settings level

You can [apply]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#apply-the-plugin) the plugin as a *project plugin* or a *settings plugin*, and here are the differences:

1. the *project plugin* is applied in the `build.gradle` file while the *settings plugin* is applied in the `settings.gradle` file. You can use the same [plugin DSL](https://docs.gradle.org/current/userguide/plugins.html#sec:plugins_block) to apply and the same grammar to [configure]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#configure-the-plugin)
2. the *project plugin* **should** be applied to the root project only, the *settings plugin* **must** be applied to the root project only as Gradle restricts the `settings.gradle` file to appear at the root level only
3. the *project plugin* defines [tasks]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#core-tasks) for you to use at any time in your build script, the *settings plugin* does that too, but also triggers [inference]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) during the [initialization](https://docs.gradle.org/current/userguide/build_lifecycle.html) of the Gradle project, before the *configuration* and *execution* take place (which is where you need properties like the project `version`)

This last point is what makes the real difference as inferred properties, including the project [`version`](https://docs.gradle.org/current/userguide/writing_build_scripts.html#sec:standard_project_properties), are evaluated in advance and made available to the build scripts as if they were statically defined.

What Nyx does behind the scenes is running the [`nyxInfer`]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#nyxinfer) task in the early initialization phase, before build scripts are evaluated and project plugins applied. While you still have these tasks (along with others) available for you to invoke at any time during the build process, invoking them again won't change the value of the inferred properties (as long as the Git repository contents don't change).

Let's take a common example of using the [Maven Publish Plugin](https://docs.gradle.org/current/userguide/publishing_maven.html). The plugin uses the project `version` to define the version of the artifacts that will be published and it does so in the [evaluation](https://docs.gradle.org/current/userguide/build_lifecycle.html) phase. For example:

```groovy
publishing {
    publications {
        maven(MavenPublication) {
            groupId = 'com.example.library'
            artifactId = 'library'
            version = project.version

            ...
        }
    }
}
```

In this example the `version = project.version` assignment is superfluous as it's the default, but helps making the example clearer. What's important is that if you apply the Nyx plugin as a *project plugin* the Maven artifacts will have their version set as *unspecified*, while if you apply as a settings plugin the version will be what is [inferred]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) by [`nyxInfer`]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#nyxinfer). That's because the *settings plugin* kicks in before the Maven publications are evaluated.