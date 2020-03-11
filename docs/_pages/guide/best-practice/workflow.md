---
title: Workflow
layout: single
toc: true
permalink: /best-practice/workflow/
---

## Build workflow

Whether you're building on a CI/CD environment or locally you should try to keep your build process **consistent**, **portable** and **efficient**. When using a CI/CD platform (and you should) chances are that you also need to build locally before you commit. Even better if you have a *branching strategy* in place.

Having a **portable** build process means that your build scripts run idempotently locally and on CI/CD and this ensures that the build process itself is defined and tested just like any other artifact in your project. This is the foundation of the *continuous* process after all, right?

An **efficient** build process avoids useless repetitions and usually means supporting **incremental** builds.

A **consistent** process doesn't need to repeat the same tasks in order to grant that the output of one task never changes.

To achieve all this you should author your primary scripts using a build tool of your choice (i.e. Gradle, Maven etc) and have the CI/CD platform run those scripts, instead of using custom ones.

At this point you often encounter a dilemma about the relationship of build tasks in the primary script and those used in the CI/CD platform. Ideally you have a 1 to 1 correspondence between the two, so for example the `build` task in the primary script is triggered by a `build` job from the CI/CD and the same goes for other tasks like `test`, `publish`, `release` etc. However, depending on the toolchain, you may find that this is not a viable model or that enforcing this relationship introduces additional complexities. For example, GitHub CI assumes that all CI jobs, even in the same *pipeline*, may run on different *runners* (instances) and the so called *sticky runners*, allowing jobs to run on the same instance, are not available (as of this writing). This implies that using two jobs *A* and *B*, where *B* depends on *A* in the *primary* script (i.e. `build.gradle`), cannot be simply mapped to jobs *A1* and *B1* on the CI/CD platform (invoking *A* and *B* respectively) because when *B1* runs *B* it may be running on a different runner than *A1*, running *A*. In these case it's up to you to define the model that better suits your need.
{: .notice--info}

How does all this relate to Nyx and the release process?

In the first place the release process must not break any of the above principles. Second, the release process can be [transactional]({{ site.baseurl }}/getting-started/introduction/#transactional-release-process) as it should. Third, it must allow the right degree of segregation [by branch]({{ site.baseurl }}/best-practice/workflow/#segregation-by-branch) and/or [by environment]({{ site.baseurl }}/best-practice/workflow/#segregation-by-environment).

Nyx complies with all of the above. Let's discuss it with the typical build process again:

```
1. build      # preprocess, compile, assemble, package etc
2. test       # deploy to test environment, run tests
3. publish    # publish artifacts to repositories and registries
4. release    # tag, generate change log, notify
```

Nyx makes the version number available since the *build* step so you can use it to generate artifacts documentation, references, URLs etc. It also [grants]({{ site.baseurl }}/getting-started/introduction/#transactional-release-process) the number to be consistent throughout the entire process, until the final steps.

Whether to run the *publish* before or after the *release* step is often controversial. We believe that *release* should be executed as the very final step because while you can publish additional releases (in case you can't *roll back* a publication by deleting the artifacts), releasing multiple times might be very misleading for the project consumers. From a semantic standpoint the *release* is also a sort of *stamp* that, once applied, grants that the version being delivered has passed all of the required steps, including publications.

## Segregation by branch

The release process should use the [branching model]({{ site.baseurl }}/getting-started/introduction/#branching-model) that best suits your needs and organization and also produce different releases based on the branch it's been invoked from.

## Segregation by environment

The *release* task should be considered critical as it affects the audience and consumers of your project. Issuing wrong releases or not granting a consistent release process has serious impact on the reputation of your project and that's why you should release carefully.

Release should only be issued from a centralized CI/CD environment with limited access for configuration changes and only after all the tests have been performed. Releases should never be issued from local developer environments.

In order to enforce this principle just make sure that the credentials needed by the release process are kept secret and only available on the CI/CD environment, usually as environment variables. Never share those credentials with anyone nor store them in any file within the repository.
