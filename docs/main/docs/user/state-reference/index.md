---
sidebar_position: 5
tags:
  - user
  - guide
  - reference
  - state
---

# State Reference

You can get access to the internal state of Nyx if you need to use one of the values it computes internally or just to inspect its internals, like for debugging purpose.

You can save the State by setting the [`stateFile`](../configuration-reference/global-options.mdx#state-file) configuration option and even use it to break the execution in phases, in which case you can load a previously stored State file by enabling the [`resume`](../configuration-reference/global-options.mdx#resume) option. This way, when working on large repositories, where the release process may take long to complete, you can split the process in phases (and run other tasks in between), making sure that you always start from where you left.

:::tip
Since state files are platform and flavor agnostic they can be used in [combined release processes](../introduction/combined-release-process.mdx).
:::

:::info
You can access the State attributes directly from **Gradle** build scripts as explained [here](../introduction/usage.mdx#accessing-the-nyx-state-extra-project-property-from-build-scripts).
:::

:::info
State attributes can be used to [replace](../configuration-reference/substitutions.mdx) text token into arbitrary files within the project (i.e. to update the `version` attribute in platform-specific descriptors).
:::

If you need less informations but in an easily parseable for also consider using the [`summary`](../configuration-reference/global-options.mdx#summary) or the [`summaryFile`](../configuration-reference/global-options.mdx#summary-file).

In this section you can find details about every value contained in the State file.
