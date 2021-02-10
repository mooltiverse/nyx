---
title: Usage
layout: single
toc: true
permalink: /guide/user/introduction/usage/
---

TODO: review the entire page, verify existing links, provide links to other pages where available
{: .notice--warning}
## Using the command line

TODO: write this section
{: .notice--warning}

### Synopsis

TODO: write this section
{: .notice--warning}

### Exit codes

TODO: write this section
{: .notice--warning}

## Using the Gradle plugin

TODO: write this section
{: .notice--warning}

## Available commands

| Name                                        | Command Line Command                   | Gradle Task Name                       |
| ------------------------------------------- | -------------------------------------- | -------------------------------------- |
| [Amend](#amend)                             | `amend`                                | `nyxAmend`                             |
| [Clean](#clean)                             | `clean`                                | `nyxClean`                             |
| [Help](#help)                               | `help`                                 | N/A                                    |
| [Infer](#infer)                             | `infer`                                | `nyxInfer`                             |
| [Make](#make)                               | `make`                                 | `nyxMake`                              |
| [Publish](#publish)                         | `publish`                              | `nyxPublish`                           |
| [Recall](#recall)                           | `recall`                               | `nyxRecall`                            |

### Amend

This command makes sure that the local Git repository is in the proper state before proceeding.

This command may change the state (current branch) of the local repository and if you need to bring it back to the original state you need to run some Git commands manually. In order to avoid working on branches other than expected by mistake be careful before *amending*. Make sure you understand the changes that *amend* makes to the local repository state. If you plan to *amend*, consider to run in only on *ephemeral* environments, like CI/CD pipelines.
{: .notice--warning}

Consider this an *helper* command that you should not run unless you're in particular circumstances, like when the local repository is in the *[detached HEAD state](https://www.git-tower.com/learn/git/faq/detached-head-when-checkout-commit/)*. In this case the local repository is not checked out to any branch but, instead, to a specific commit (technically speaking, the `HEAD` symbolic reference does not link to a branch, hence is *detached*). This is often the case of CI/CD environments, where the latest commit is specifically checked out, instead of a branch.

Missing the current branch name prevents Nyx to perform most of its work so you need to fix it in advance. You may also fix this outside of Nyx by running:

```bash
git checkout <BRANCH_NAME>
git pull
```

To make sure you are aware of what you're doing this command will take no action by default even if you invoke it explicitly. In order to enable this phase you need to enable the [amend branch]({{ site.baseurl }}/reference/configuration/#amend-branch) option and override the [branch]({{ site.baseurl }}/reference/configuration/#branch).
{: .notice--info}

If the current branch of the local repository is already as set by the [branch]({{ site.baseurl }}/reference/configuration/#branch) option no action is taken. Running this command in a *dirty* workspace (with uncommitted changes) will produce an error.
{: .notice--info}

### Clean

Deletes all the [artifacts]({{ site.baseurl }}/reference/output/#artifacts) created by [make](#make) or reverts the changes, if any. Running this command has no effect when there is nothing to clean.

This command/task has no dependencies.

### Help

Prints the synopis and usage instructions and exits.

This command has no dependencies.

### Infer

TODO: write this section
{: .notice--warning}

### Make

TODO: write this section
{: .notice--warning}

### Publish

TODO: write this section
{: .notice--warning}

### Recall

TODO: write this section
{: .notice--warning}
