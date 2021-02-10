---
title: Release Strategy
layout: single
toc: true
permalink: /guide/user/introduction/release-strategy/
---

## Background

A release strategy is the definition of the types of releases you are going to issue with your project and actually starts with the workflow.

The workflow, in turn, is driven by several factors like the size and complexity of your team, commitments to the audience and stakeholders, the automation infrastructure you use, regulatory requirements, quality assurance, testing and much more. These all culminate in the definition of a [branching strategy]({{ site.baseurl }}/best-practice/branching-models/), which is one of the pillars of a release strategy. Wheter you have a formal branching model or not doesn't matter as long as you have a clear understanding of which branches to use and how. If you don't, you should definitely pause here and have a deep dive into [branching models]({{ site.baseurl }}/best-practice/branching-models/) or assume you're using the simplest *[mainline only]({{ site.baseurl }}/best-practice/branching-models/#mainline-only)* strategy to get started.

The second pillar is how to translate the contributions from the branches you use into releases, which may be issued to the audience or be internal. Once you're there you have a release strategy and you just need to instruct Nyx about it by configuring your release types.

## Release types

Chances are that your strategy needs several release types to be defined, like, for instance, *official* stable releases to be issued to the audience but also intermediate releases to be used for testing or preview. You will likely need to send out hotfixes to past releases or have your project go through several maturity stages before it's released, each stage represented by a release type.

Nyx comes with several [preset release types](#presets) that likely fit most of the workflows, including yours. You have complete freedom to define your [custom ones](#custom-release-types) from scratch or just override a few options from the presets but before going down that route consider starting with presets before you start defining your own.
{: .notice--info}

From a conceptual standpoint a release type in Nyx:

* needs to match a few factors ([branch name]({{ site.baseurl }}/configuration/release-types/#on-branches), [environment]({{ site.baseurl }}/configuration/release-types/#on-environments) and [workspace status]({{ site.baseurl }}/configuration/release-types/#on-workspace-status)) before it's selected for a certain commit. These factors may be statically defined or dynamically inferred from the Git repository and its current status or the runtime environment so they can be very simple or be configured to build a configuration matrix. Thanks to conditionals one release type may be selected on one runtime environment and not others (i.e. on CI/CD environments only), or only if the Git workspace happens to be in a certain state and not others (i.e. when it's in a *clean* state only, without uncommitted changes)
* gives instructions on the actions to take in order to release (or not), like [creating tags]({{ site.baseurl }}/configuration/release-types/#git-tag) and [commits]({{ site.baseurl }}/configuration/release-types/#git-commit), [publishing artifacts]({{ site.baseurl }}/configuration/release-types/#publish) etc
* defines if some extra constraints need to be verified, like making sure that hotfixes to a previous version (i.e. `1.2.x`) are going to be issued within the allowed [version range]({{ site.baseurl }}/configuration/release-types/#version-range) only (i.e. between `1.2.1` and `1.2.N`), otherwise they need to be issued as major or minor releases instead of patches (according to [SemVer](https://semver.org/))
* selects the [version numbering]({{ site.baseurl }}/configuration/release-types/#collapse-versions) (i.e. *linear* or *collapsed*), according to the [version scheme]({{ site.baseurl }}/reference/version-schemes/) in use

A few remarks here:

* when multiple release types are configured, Nyx tries to match them in the order they have been [enabled]({{ site.baseurl }}/configuration/release-types/#enabled-release-types) and stops when the first one matches. This is why you should always have a default, *last resort* release type defined at the end of the list
* the [current branch name]({{ site.baseurl }}/configuration/release-types/#on-branches) is the most relevant information when selecting the release type but you can use regular expressions to match them. This means that even when using multiple branch types you can group them in the same release type as long as they have the same expected behavior when it comes to releasing
* by using conditionals, you can have a certain release type be *eligible* on some environments and not others (i.e. when you want to issue some releases only when running on your master, highly controlled and secured CI/CD environment while local developer workstations can only produce *internal* releases). Conditionals are available for the [runtime environment]({{ site.baseurl }}/configuration/release-types/#on-environments) and the [workspace status]({{ site.baseurl }}/configuration/release-types/#on-workspace-status)
* while the release type defines the [version numbering strategy]({{ site.baseurl }}/configuration/release-types/#collapse-versions) it does not define the version identifier to bump nor the rules to infer it. That is done by [commit message conventions]({{ site.baseurl }}/reference/commit-message-conventions/)

| ----------------------------- | ------------------------------------------------------------------------------------------------------------ |
| Related configuration options | [releaseTypes]({{ site.baseurl }}/configuration/release-types/){: .btn .btn--success .btn--small} [releaseTypes/enabled]({{ site.baseurl }}/configuration/release-types/#enabled-release-types){: .btn .btn--success .btn--small} [releaseTypes/\<ID\>/collapseVersions]({{ site.baseurl }}/configuration/release-types/#collapse-versions){: .btn .btn--success .btn--small} [releaseTypes/\<ID\>/enabledIdentifiers]({{ site.baseurl }}/configuration/release-types/#enabled-identifiers){: .btn .btn--success .btn--small} [releaseTypes/\<ID\>/gitCommit]({{ site.baseurl }}/configuration/release-types/#git-commit){: .btn .btn--success .btn--small} [releaseTypes/\<ID\>/gitCommitMessage]({{ site.baseurl }}/configuration/release-types/#git-commit-message){: .btn .btn--success .btn--small} [releaseTypes/\<ID\>/gitPush]({{ site.baseurl }}/configuration/release-types/#git-push){: .btn .btn--success .btn--small} [releaseTypes/\<ID\>/gitTag]({{ site.baseurl }}/configuration/release-types/#git-tag){: .btn .btn--success .btn--small} [releaseTypes/\<ID\>/gitTagMessage]({{ site.baseurl }}/configuration/release-types/#git-tag-message){: .btn .btn--success .btn--small} [releaseTypes/\<ID\>/name]({{ site.baseurl }}/configuration/release-types/#name-release-type){: .btn .btn--success .btn--small} [releaseTypes/\<ID\>/onBranches]({{ site.baseurl }}/configuration/release-types/#on-branches){: .btn .btn--success .btn--small} [releaseTypes/\<ID\>/onEnvironments]({{ site.baseurl }}/configuration/release-types/#on-environments){: .btn .btn--success .btn--small} [releaseTypes/\<ID\>/onWorkspaceStatus]({{ site.baseurl }}/configuration/release-types/#on-workspace-status){: .btn .btn--success .btn--small} [releaseTypes/\<ID\>/publish]({{ site.baseurl }}/configuration/release-types/#publish){: .btn .btn--success .btn--small} [releaseTypes/\<ID\>/type]({{ site.baseurl }}/configuration/release-types/#type-release){: .btn .btn--success .btn--small} [releaseTypes/\<ID\>/versionRange]({{ site.baseurl }}/configuration/release-types/#version-range){: .btn .btn--success .btn--small} [releaseTypes/\<ID\>/versionRangeFromBranchName]({{ site.baseurl }}/configuration/release-types/#version-range-from-branch-name){: .btn .btn--success .btn--small} |
| Related state attributes      | [collapseVersions]({{ site.baseurl }}/state/release-types/#collapse-versions){: .btn .btn--info .btn--small} [releaseType]({{ site.baseurl }}/state/release-types/#release-type){: .btn .btn--info .btn--small} [versionRange]({{ site.baseurl }}/state/release-types/#version-range){: .btn .btn--info .btn--small} |
{: .notice--info}

### Presets

The following preset release types that come preconfigured in Nyx are intended to cover the needs of most teams, at least those following common best practice.

#### Main releases

Main (a.k.a. *Official*) releases are the regular releases which, according to [Semantic Versioning](https://semver.org/), are identified by *core* identifiers only (`major.minor.patch`).

Most teams issue these releases from the [mainline]({{ site.baseurl }}/best-practice/branching-models/#mainline-only) branch (`master` or `main`) but others have dedicated [release branches]({{ site.baseurl }}/best-practice/branching-models/#release-branches) (i.e. `rel-v1.1`, `release/v1.2.3` etc.). Nyx has [enabled]({{ site.baseurl }}/configuration/release-types/#enabled-release-types) presets for both release types and since they don't clash they can coexist in the same configuration.

The two presets are named [`main`]({{ site.baseurl }}/configuration/release-types/#main-release-type) and [`release`]({{ site.baseurl }}/configuration/release-types/#release), respectively, and aside the name they only differ about the [branch name]({{ site.baseurl }}/configuration/release-types/#on-branches) they match, which in the first case matches `master` or `main` (`^(master|main)$`) while in the second any branch name starting with `rel` or `release`, followed by the `-` or `/` separator, the optional `v` version prefix and three integers separated by dots (`^(rel(ease)?[-\/](v)?(0|[1-9]\d*)(\.(0|[1-9]\d*)(\.(0|[1-9]\d*))?)?)$`) (i.e. `release-1.2`, `release/1.2.3`, `rel-v1.2` etc).

Since official releases should be published by controlled environments only, they do not match when running on [local]({{ site.baseurl }}/configuration/environments/#local) environments and also the [push]({{ site.baseurl }}/configuration/release-types/#git-push), [tag]({{ site.baseurl }}/configuration/release-types/#git-tag) and [publish]({{ site.baseurl }}/configuration/release-types/#publish) are only allowed on [server]({{ site.baseurl }}/configuration/environments/#server) environments.

Only *core* identifiers (no [extra identifiers]({{ site.baseurl }}/configuration/release-types/#enabled-identifiers)) are used. Also, no [version constraint]({{ site.baseurl }}/configuration/release-types/#version-range) is applied and [linear]({{ site.baseurl }}/configuration/release-types/#collapse-versions) version numbering is used.

| ----------------------------- | ------------------------------------------------------------------------------------------------------------ |
| Related configuration options | [releaseTypes/enabled]({{ site.baseurl }}/configuration/release-types/#enabled-release-types){: .btn .btn--success .btn--small} |
| Available in presets          | [generic]({{ site.baseurl }}/configuration/presets/#generic){: .btn .btn--small}               |
| Related preset configurations | [releaseTypes/main/]({{ site.baseurl }}/configuration/presets/#main-release-type){: .btn .btn--warning .btn--small} [releaseTypes/release/]({{ site.baseurl }}/configuration/presets/#release){: .btn .btn--warning .btn--small} |

#### Pre-release

Pre-releases are previews of future releases or work in progress toward them. Teams may even have multiple pre-release branches toward a single release and each represents a different [maturity]({{ site.baseurl }}/best-practice/branching-models/#maturity-branches) of code. More broadly, these release types may also represent different [integration stages]({{ site.baseurl }}/best-practice/branching-models/#integration-branches) or [environments]({{ site.baseurl }}/best-practice/branching-models/#environment-branches).

Regardless of the semantics used for the branches used, the types of release have the same technical treats like:

* [collapsed version numbers]({{ site.baseurl }}/configuration/release-types/#collapse-versions) in order to shrink version bumps in a way that is coherent with the future official release
* use the [preset pre-release identifier]({{ site.baseurl }}/configuration/presets/#pre-release-identifier) to disambiguate *collapsed versions*; this identifier uses the [`ticker`]({{ site.baseurl }}/reference/state/#global-attributes) as the numeric value and the sanitized branch name for the qualifier, according to [Semantic Versioning](https://semver.org/)

See [this example]({{ site.baseurl }}/example/version-history-for-different-release-types/) and [this FAQ]({{ site.baseurl }}/faq/#whats-the-difference-between-the-prime-version-and-the-previous-version) for more.
{: .notice--info}

Presets for these release types are [`maturity`]({{ site.baseurl }}/configuration/presets/#maturity) and [`stage`]({{ site.baseurl }}/configuration/presets/#stage) and they have the same configuration, just matching two different branch nomenclatures. The former matches branches using the [greek alphabet](https://en.wikipedia.org/wiki/Greek_alphabet) for subsequent stages (*alpha*, *beta*, *gamma* etc), the latter using common stage names like *develop* (or *development*), *integration*, *production*, *rc*, *stable*, *stage*, *test* and others.

These release types have [publish]({{ site.baseurl }}/configuration/release-types/#publish) enabled only when running on [server]({{ site.baseurl }}/configuration/environments/#server) environments and are configured to be matched only on *clean* workspaces.

| ----------------------------- | ------------------------------------------------------------------------------------------------------------ |
| Related configuration options | [releaseTypes/enabled]({{ site.baseurl }}/configuration/release-types/#enabled-release-types){: .btn .btn--success .btn--small} |
| Available in presets          | [generic]({{ site.baseurl }}/configuration/presets/#generic){: .btn .btn--small}               |
| Related preset configurations | [releaseTypes/maturity/]({{ site.baseurl }}/configuration/presets/#maturity){: .btn .btn--warning .btn--small} [releaseTypes/stage/]({{ site.baseurl }}/configuration/presets/#stage){: .btn .btn--warning .btn--small} |

#### Post-release

Post-releases (a.k.a. *Maintenance* releases) are meant to issue fixes to past releases so that they can be deployed into existing environments without major upgrades. On the development side they allow the development team to just incorporate small changes (bug fixes) into these branches while still working on other future releases. To this extent these releases are peculiar as they are constrained to only increment the *patch* number (rarely the *minor*, never the *major* number) starting from the past release they are meant to fix. Apart from this, these releases are just like official releases.

The downside of this strategy is that you need one branch for each release you need to maintain and unless you can use patterns, you need to configure one release type for each. Suppose you need to support release `v1.2.3` and over time you need to issue two hotfixes, then you have two additional branches (`v1.2.4` and `v1.2.5`) and you are also supposed to create two additional release types, one for each. Nyx can match them by using regular expressions so you don't need to change the configuration over time. What's most interesting is that Nyx also infers the version constraints from the branch name so you can have one branch only do match all the fixes to `v1.2.3` and compute the constraints. Concretely, you can name the branch `v1.2.x` and Nyx knows, by the [`versionRangeFromBranchName`]({{ site.baseurl }}/configuration/release-types/#version-range-from-branch-name) flag, that only releases in the `v1.2.0` - `v1.2.N` can be issued from there.

Only *core* identifiers (no [extra identifiers]({{ site.baseurl }}/configuration/release-types/#enabled-identifiers)) are used and [linear]({{ site.baseurl }}/configuration/release-types/#collapse-versions) version numbering is used.

There is one preset release type for post releases and it's [`maintenance`]({{ site.baseurl }}/configuration/presets/#maintenance).

Also this release type has [publish]({{ site.baseurl }}/configuration/release-types/#publish) enabled only when running on [server]({{ site.baseurl }}/configuration/environments/#server) environments and is configured to be matched only on *clean* workspaces.

| ----------------------------- | ------------------------------------------------------------------------------------------------------------ |
| Related configuration options | [releaseTypes/enabled]({{ site.baseurl }}/configuration/release-types/#enabled-release-types){: .btn .btn--success .btn--small} |
| Available in presets          | [generic]({{ site.baseurl }}/configuration/presets/#generic){: .btn .btn--small}               |
| Related preset configurations | [releaseTypes/maintenance/]({{ site.baseurl }}/configuration/presets/#maintenance){: .btn .btn--warning .btn--small} |

#### Internal release

Before official releases teams and single developers make many other releases for internal use only, be it for testing a single code change, try out a new library etc. They may also need to try what official releases will be by building and testing them on non [server]({{ site.baseurl }}/configuration/environments/#server) environments. Las but not least it's recommended to have a default release type that Nyx can match when no other is suitable, also considering conditionals.

This release type is meant to issue releases for internal use from **any** branch that isn't matched by previous definitions (even those whose name is configured in other release types but with non matching conditionals, [environment]({{ site.baseurl }}/configuration/release-types/#on-environments) and [workspace status]({{ site.baseurl }}/configuration/release-types/#on-workspace-status)).

In short this is the **default**, *catch-all* release type that kicks in when other more specific release types couldn't be matched and basically:

* never [publishes]({{ site.baseurl }}/configuration/release-types/#publish) but does [tag]({{ site.baseurl }}/configuration/release-types/#git-tag) and [push]({{ site.baseurl }}/configuration/release-types/#git-push) when on [server]({{ site.baseurl }}/configuration/environments/#server) environments
* uses several extra identifiers ([`branchName`]({{ site.baseurl }}/configuration/presets/#branch-name-identifier), [`commitShortSHA`]({{ site.baseurl }}/configuration/presets/#commit-short-sha-identifier), [`environmentName`]({{ site.baseurl }}/configuration/presets/#environment-name-identifier) and [`timestampYYYYMMDDHHMMSS`]({{ site.baseurl }}/configuration/presets/#timestamp-yyyymmddhhmmss-identifier)) to generate the version in order to make it clearly identifiable. Examples: `v1.2.3-stage-e81bd-local-20200101123000`, `v2.0.1-rc-378e4-server-20200101123000`
* also works on *dirty* [workspaces]({{ site.baseurl }}/configuration/release-types/#on-workspace-status) and without any distinction on the [environment]({{ site.baseurl }}/configuration/release-types/#on-environments)
* has no [version constraints]({{ site.baseurl }}/configuration/release-types/#version-range)

| ----------------------------- | ------------------------------------------------------------------------------------------------------------ |
| Related configuration options | [releaseTypes/enabled]({{ site.baseurl }}/configuration/release-types/#enabled-release-types){: .btn .btn--success .btn--small} |
| Available in presets          | [generic]({{ site.baseurl }}/configuration/presets/#generic){: .btn .btn--small}               |
| Related preset configurations | [releaseTypes/internal/]({{ site.baseurl }}/configuration/presets/#internal){: .btn .btn--warning .btn--small} |

### Custom release types

When defining your own release type you need to pass the new configuration within the [`releaseTypes`]({{ site.baseurl }}/configuration/release-types/#release-type-definition) and [enable]({{ site.baseurl }}/configuration/release-types/#enabled-release-types) it.

Often times you can simply override a few fields of an existing preset to get the configuration you need. For example if you need to match extra branches you can just override the [`onBranches`]({{ site.baseurl }}/configuration/release-types/#on-branches) option of an existing preset release type.
{: .notice--info}

The most important option is [`onBranches`]({{ site.baseurl }}/configuration/release-types/#on-branches), which lets you decide which branch names should be matched by the release type. This can be a simple hardcoded branch name or a regular expression to match a range of branches.

Then, by setting the [`onEnvironments`]({{ site.baseurl }}/configuration/release-types/#on-environments) and [`onWorkspaceStatus`]({{ site.baseurl }}/configuration/release-types/#on-workspace-status) you instruct Nyx whether the release type must be matched on any [environment]({{ site.baseurl }}/configuration/environments/) and workspace status or you just want those to be conditional preconditions for the relase type to be selected.

As a best practice, *official* releases should be issued only from centralized and controlled environments so, for them, consider limiting the release type to [server]({{ site.baseurl }}/configuration/environments/#server) environments and also *clean* workspace status.
{: .notice--info}

You have several options to let you control how to behave in terms of the Git repository so you can decide if Nyx has to [tag]({{ site.baseurl }}/configuration/release-types/#git-tag), [commit]({{ site.baseurl }}/configuration/release-types/#git-commit), [push]({{ site.baseurl }}/configuration/release-types/#git-push) to the remote repository and, if so, the template for the [commit message]({{ site.baseurl }}/configuration/release-types/#git-commit-message) and the [tag message]({{ site.baseurl }}/configuration/release-types/#git-tag-message) (which also controls if annotated or lightweight tags are used).

If the release type is meant to be made available to the audience consider [publishing]({{ site.baseurl }}/configuration/release-types/#publish) the release, otherwise, like for internal releases, do not enable that.

Last but not least you can apply a version range constraint to the release type. This is optional and only gives you more control and confidence on the releases being issued and should be done on [maintenance branches]({{ site.baseurl }}/best-practice/branching-models/#maintenance-branches) only. If so you can [configure the constraint as a simple regular expression]({{ site.baseurl }}/configuration/release-types/#version-range) or, if you name your branches in a way that models the version constraint, [let Nyx infer]({{ site.baseurl }}/configuration/release-types/#version-range-from-branch-name) the constraint from the branch name.