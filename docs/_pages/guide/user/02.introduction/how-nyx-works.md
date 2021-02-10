---
title: How Nyx Works
layout: single
toc: true
permalink: /guide/user/introduction/how-nyx-works/
---

Although you can configure and control Nyx behavior on every detail, executing or skipping each individual step, a streamlined sequence of operations looks like this:

TODO: review the entire page, verify existing links, provide links to other pages where available
{: .notice--warning}

1. **[amend]({{ site.baseurl }}/reference/usage/#amend)**: in this (optional) phase Nyx brings the local repository in the right state before proceeding. An example is fixing the *[detached HEAD state](https://www.git-tower.com/learn/git/faq/detached-head-when-checkout-commit/)*
2. **infer**: in this collects and processes informations (without applying any change) in this sequence:
   1. **release type resolution**: Nyx matches the **current branch** and **[configuration]({{ site.baseurl }}/reference/release-types/)** to establish which actions and constraints to apply to the release to be issued
   2. **latest release detection**: Nyx [finds out the previous release]({{ site.baseurl }}/in-depth/internals/#latest-release-detection) starting from the **current commit** and following *parent commits* backward up to the first one it encounters with a valid release tag (a tag that is compliant with the **[version scheme]({{ site.baseurl }}/reference/version-schemes/)** in use). The **latest release** is then assumed to be such tag value
   3. **release scope definition**: Nyx determines the set of commits belonging to the release. The set **ends** with the **current commit** and, inspecting its parents recursively going backward, **begins** with the commit that comes right after the **latest released commit** (the one detected at the previous step). If the previous step didn't find any previous released commit then the first commit in the tree is assumed to be the first one (this happens when releasing for the first time)
   4. **scope analysis**: analyze the commits within the scope to figure out which rules to apply in order to increment the **latest release** to generate the new version number, depending on the rules configured. This step analyzes commit messages to detect hints formatted according to a [specific convention]({{ site.baseurl }}/reference/commit-message-conventions/) telling Nyx which numbers to bump
   5. **generate version**: creates the **current version** number. If the above steps didn't find any **latest release** then the new version number with be the **default initial release** (which depends on the configured version scheme but can be overridden), otherwise the **most significant** version number identifier among all commits analyzed will be bumped to generate the new version number starting from the previous one. The newly generated version number is then made available for use in other build tasks
   6. **validate version**: validates the new version number against **consistency checks** in order to grant the right order between releases and that, depending on the **[release type]({{ site.baseurl }}/reference/release-types/)**, basic constraints are not violated
3. **make**: in this (optional) phase Nyx produces artifacts like the changelog
   1. **generate artifacts**: generates artifacts such as the **[changelog]({{ site.baseurl }}/reference/output/#changelog)** based upon the informations collected in the previous steps. These artifacts may be stored on disk and/or just used later to be **published**
   2. **commit**: if Nyx is configured to store artifacts in the current repository and commit them it will add them to the repository and **commit**. Please note that this step is against the [immutable workspace]({{ site.baseurl }}/in-depth/design-principles/#immutable-workspace) principle as it may create new files or new version of existing ones and also commit them so **by the end of this step the local repository may no longer be in the original state**. However, if a new commit is created, the **release scope** that was defined in the previous steps will be adjusted to include this **last commit**
4. **publish**: in this phase Nyx actually publishes the release to the audience, which implies, depending on the configuration, tagging, pushing to remote repositories, invoking external API to make the release public and notifying
   1. **tag**: tags the **current commit** with the newly generated version number
   2. **push**: pushes the latest changes to the remote repository
   3. **publish**: performs additional publication tasks like publishing the release on GitHub or GitLab by using provider specific API, optionally including the generated **[changelog]({{ site.baseurl }}/reference/output/#changelog)**
   4. **notify**: upon success on all the previous tasks Nyx can then send **notifications** using different channels (i.e. email, Slack etc)

## Command dependencies

Top level steps in the above list (`amend`, `infer`, `make`, `publish`) are **[commands]({{ site.baseurl }}/reference/usage/)** that you can run individually. Nested items are tasks that are executed within each command, if needed.

What is important to understand here is that each command and step needs the previous one to complete and to pass its outputs over to the next one in order to complete. With more detail:

* `amend`'s output is reflected into the local Git repository state and it's not relevant here. Once executed, even repeating it has no effects as the reposititory already has the previously missing informations. If the local repository was in the *[detached HEAD state](https://www.git-tower.com/learn/git/faq/detached-head-when-checkout-commit/)* it no longer is after `amend` runs once
* `infer` doesn't need any input from `amend` (other than a usable local Git repository). On the other hand it outputs some essential informations for next steps:
  * the generated **current version** number
  * the release **scope** (**initial commit** > **final commit**)
  * the release **type**
* `make` needs input from `infer` to create artifacts, while its output may also be reflected to the local Git repository in terms of new file versions. These files may also be needed to publish in the next step
* `publish` needs input from `make` when the artifacts need to be published and from `infer` to tag and publish the release

### Avoiding repetitiveness

When a task needs output from a previous one Nyx just takes care of running upstream tasks in the right order so you shouldn't be concerned about the above. However this implies that in some circumstances the complete isolation and [non repetitiveness]({{ site.baseurl }}/in-depth/design-principles/) among tasks cannot be granted unless the outputs from one command are handed over to the next one. To make this clearer, let's give two examples: running Nyx as a plugin of a build tool like Gradle and on the command line.

When using Gradle the state between tasks is preserved as the process running the various tasks is the same. [Non repetitiveness]({{ site.baseurl }}/in-depth/design-principles/) is safe here.

When using the command line and you don't need to run the commands at different times (i.e. you don't need to know the version number in adcance), you don't need to take care of any status and you may just run the `publish` command. The `publish` command will trigger the previous ones and the entire execution will be *transactionally atomic* and *non repetitive*.

Otherwise, if you need to run commands separately (i.e. because you need to run some custom tasks between `infer` and `publish`), you can:

* enable the [state file]({{ site.baseurl }}/reference/output/#state-file) so that each command can read the state so far from that, avoiding to run any action that was already executed
* scrape the `infer` and `make` command line output and use it as input options for the next commands
* just let Nyx have some repetition and run the commands when you need them

Most of the times you just don't care of these strategies to avoid repetition as it's a matter of a few milliseconds. But if you work with a huge repository or need to dig with some Nyx internals, the above lets you know how to deal with it.
{: .notice--info}