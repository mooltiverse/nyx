---
title: Branching Models
layout: single
toc: true
permalink: /guide/user/best-practice/branching-models/
---

## Branching Models

Whether the project is developed by a team or you are the sole contributor you soon need to define your workflow. In version control, this means you have rules and semantics in place to track your code changes consistently. Even when you have never thought about a branching model you're actually using [one](#mainline-only). This set of rules is often called in many different ways, like *branching strategy*, *branching pattern*, *git worlflow*. A few well known ones are:

* [GitFlow](https://nvie.com/posts/a-successful-git-branching-model/)
* [GitHub Flow](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/github-flow)
* [GitLab Flow](https://docs.gitlab.com/ee/topics/gitlab_flow.html)
* [OneFlow](https://www.endoflineblog.com/oneflow-a-git-branching-model-and-workflow)

Moreover you probably may find interesting the [Patterns for Managing Source Code Branches by Martin Fowler](https://martinfowler.com/articles/branching-patterns.html), which is not a workflow model but probably the most extensive collection of branch patterns available. There are some original thoughts in there that may help you deciding which strategy to adopt, like [*integration* oriented patterns](https://martinfowler.com/articles/branching-patterns.html#integration-patterns) and [*path to production* oriented patterns](https://martinfowler.com/articles/branching-patterns.html#path-to-production), the former being generally suitable for code development and the latter for operations. No need to fall into one of these two categories but this helps clarifying what a typical workflow may be depending on the project type and the organization processes.

Nyx can support any strategy so you're not constrained at all into one strategy or another. Instead you have complete freedom in defining your own and below you can find some useful considerations to do so. The only assumption here is that Git is the version control system in use and so terms like *branch*, *commit*, *tag*, *merge* etc are to be interpreted in the Git jargon.

Most of the times you are using a centralized repository hosted on some Git server so, from a branching perspective, this means that even when working on the same branch, contributors are still using different local copies of such branch that will eventually need to be merged.

A simple yet critical decision you need to make is about which branches are to be kept [*healthy*](https://martinfowler.com/articles/branching-patterns.html#healthy-branch) or *production ready* any time. By *healthy* we mean that there are errors preventing the code to run or pipelines to complete succesfully. By *production ready*, instead, we also mean that code could be released to production at any time as it's considered to be fully tested. You should have **at least** your [*mainline*](https://martinfowler.com/articles/branching-patterns.html#mainline) always *healthy* or even *production ready* but as the workflow complexity grows you need more.

It's also useful to adopt a coherent and shared **naming convention** for branches in order to leverage automation and also clearly communicate to others what is the purpose and scope of a branch. Using hierarchical names is usually a good practice so that all branches of the same kind are collected under the same prefix. For example, branches in the `release/` path are all [release branches](#release-branches) and [feature branches](#feature-branches) are all under the `feature/` path. This pattern is also very helpful in automation as it makes the branch purpose easy to detect even when branch names are dynamic. You can also use a variant of this nomenclature that uses flat prefixes instead of hierarchy separator, so that, for instance, you use `feature-X` instead of `feature/X` etc.

The **frequency** of merges is also a topic you should tackle soon as you need to find the right balance between the burden of merging a large number of changes (with low-frequency merges) or reviewing so many merges (when code reviews on merges belong to your practice).

The branching model also affects (and leverages) [**automation**]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/build-and-automation.md %}). For example you may need to decide how branches are related to **stages** in your **pipelines**, which tests to run for each, which **environments** are to be used to deploy your artifacts depending on the branch etc.

### [Mainline](https://martinfowler.com/articles/branching-patterns.html#mainline)

Git repositories start with one *default* branch usually called `master` or `main` and unless you take other decisions (i.e. when using [multiple mainlines](#multiple-mainlines)) it will be the repository *mainline*.

In this guide we use `master` and `main` interchangeably.

This is the bare minimum that you can use in a [mainline only](#mainline-only) or [multi branches](#multiple-branches) strategy.

### Mainline only

The simplest strategy is [using a single branch](https://martinfowler.com/articles/branching-patterns.html#mainline-integration) and is also how any project starts. You basically don't use branches as every commit goes into the [mainline](#mainline).

Single contributor projects often use the *mainline only* strategy but as soon as more contributors join the project or the development workflow involves maintenance, bug fixing etc it falls short.

In other words, you need a better strategy and [more branches](#multiple-branches).

### Multiple branches

Once you decided to use multiple branches you need to know which ones. Here's where the branching strategy really comes in.

[Tailoring the strategy](#your-custom-branching-model) is completely up to you but one thing needs to drive your decision: **your branching model needs to reflect your workflow**.

The following are common branch types that you may consider.

#### [Feature branches](https://martinfowler.com/articles/branching-patterns.html#feature-branching)

They are scoped to the development of a specific feature and deleted when the feature development is merged into other branches. They are also known as *topic branches* and they are supposed to live only on local developer environments unless they are also used for collaboration, in which case they are pushed.

Common names for these branches are `feature-X`, `feature/X` (note the use of the forward slash) or abbreviations. Here `X` is the short name or ID or the feature being developed. The feature ID is often the *issue number* of an issue you manage on your issue management platform (i.e. Jira, GitHub, GitLab etc). Examples: `feature-SSO`, `feature/1234`.

Don't think to *features* too strictly here as these branches may be used for tasks (i.e. *refactoring*, *porting* etc) or other activities. 

*Feature* branches are also useful for [cherry picking](https://git-scm.com/docs/git-cherry-pick) a commit into other branches.

*Feature branches* are part of [GitFlow](https://nvie.com/posts/a-successful-git-branching-model/#feature-branches), [GitHub Flow](https://guides.github.com/introduction/flow/), [GitLab Flow](https://docs.gitlab.com/ee/topics/gitlab_flow.html) and [OneFlow](https://www.endoflineblog.com/oneflow-a-git-branching-model-and-workflow#feature-branches).
{: .notice--info}

#### [Hotfix branches](https://martinfowler.com/articles/branching-patterns.html#hotfix-branch)

Technically speaking these branches are just like [feature branches](#feature-branches) but from a workflow perspective they are about changes made to the codebase under pressure and with the need to deliver quickly.

Also because the defect to be fixed is usually tracked on the issue tracking platform of choice, the naming convention is very similar to [*feature branches*](#feature-branches), like for example `fix-5678`, `hotfix-5678`, `fix/5678`, `hotfix/5678`.

One peculiar aspect of hotfixes is that they usually need to be merged into multiple branches ([integration branches](#integration-branches), [release branches](#release-branches), [maintenance branches](#maintenance-branches) etc) so again [cherry picking](https://git-scm.com/docs/git-cherry-pick) might be your friend here.

*Hotfix branches* are part of [GitFlow](https://nvie.com/posts/a-successful-git-branching-model/#hotfix-branches) and [OneFlow](https://www.endoflineblog.com/oneflow-a-git-branching-model-and-workflow#hotfix-branches) while [GitLab Flow](https://docs.gitlab.com/ee/topics/gitlab_flow.html) and [GitHub Flow](https://guides.github.com/introduction/flow/) use [feature branches](#feature-branches) for hotfixes.
{: .notice--info}

#### Integration branches

These are long-lived branches meant to merge code from different contributors in order to spot integration issues.

While the [*mainline*](#mainline) is actually an integration branch itself, many teams use other integration branches named `develop` (or `development`), `integration`, `latest` etc with lower expectations than `master` in terms of confidence but higher merge frequency.

Integration branches are commonly based on the level of **confidence** of the code they merge and are periodically merged into the [mainline](#mainline) or into other integration branches.

While in most cases one integration branch is enough you may need more than one as your workflow grows in complexity. Many think that teams should use [*feature*](#feature-branches) branches **OR** *integration* branches, not both. You can actually use them in conjuction so that *feature* branches are merged into *integration* branches rather than the mainline.

[GitFlow](https://nvie.com/posts/a-successful-git-branching-model/#hotfix-branches) heavily relies on the [`develop` main branch](https://nvie.com/posts/a-successful-git-branching-model/#the-main-branches) to live in parallel with mainline and so does the [OneFlow *develop + master* variation](https://www.endoflineblog.com/oneflow-a-git-branching-model-and-workflow#variation-develop-master).
{: .notice--info}

#### [Release branches](https://martinfowler.com/articles/branching-patterns.html#release-branch)

They are used to prepare a release **to be issued**, do the packaging, final testing etc. These branches are usually created from the [*mainline*](#mainline) once it's ready to release the product. These branches can be closed once they are merged back to the *mainline* and the release is issued unless you want to keep [using them also as maintenance branches](#release-and-maintenance-branches), in which case they and are long lived as such.

These branches are usually named after the major (and minor) number of the release they are meant to issue, like `release-v1.2` `release/v1.2`, `rel/v1.2`. The `v` prefix may be omitted or replaced.

While these branches are very meaningful they often lead to regressions due to committing bug fixes to the *release branch* only and not into the [*mainline*](#mainline). So **be careful** about this and also merge fixes to the *mainline* (i.e. by [cherry picking](https://git-scm.com/docs/git-cherry-pick)). You may also invert the flow and pick fixes from the *mainline* into the *release branch*. Any way you deal with it, remember to merge fixes in both branches (or more, as the same fix may need to apply to several releases). You don't need *release branches* if you have a single version in production.

Release branches are addressed by [GitFlow](https://nvie.com/posts/a-successful-git-branching-model/#release-branches), [GitLab Flow](https://docs.gitlab.com/ee/topics/gitlab_flow.html#release-branches-with-gitlab-flow) and [OneFlow](https://www.endoflineblog.com/oneflow-a-git-branching-model-and-workflow#release-branches).
{: .notice--info}

#### Maintenance branches

They are also known as *support branches* and used to maintain a specific release that was **already issued** to only fix defects, without adding any new feature. These branches are long lived (at least until the release they refer to is supported).

If releases are also tagged there is no need to create a *maintenance branch* for every release as they can be created at any time starting from the specific tagged commit that generated the release.

These branches are usually named after the major (and minor) number of the release they are meant to maintain, like `v1`, `v1.x`, `v1.2`, `v1.2.x`. The `v` prefix may be omitted.

When defining the naming convention do not use names that may conflict with [*release branches*](#release-branches) unless you [also use them as *release branches*](#release-and-maintenance-branches).
{: .notice--warning}

While these branches are very meaningful they often lead to regressions due to committing bug fixes to the *maintenance branch* only and not into the [*mainline*](#mainline). So **be careful** about this and also merge fixes to the *mainline* (i.e. by [cherry picking](https://git-scm.com/docs/git-cherry-pick)). You may also invert the flow and pick fixes from the *mainline* into the *release branch*. Any way you deal with it, remember to merge fixes in both branches (or more, as the same fix may need to apply to several releases). You don't need *maintenance branches* if you have a single version in production.

Maintenance branches should be used for maintenance only **after** a release has been issued. If you need branches to track progress while **approaching** a release you may want to consider [*release branches*](#release-branches). See also [this section](#branches-to-and-from-the-mainline) for more.
{: .notice--info}

#### Release and maintenance branches

When you use branches for preparing releases to be issued and also maintain past releases, you should consider the same branch for both phases of a release to merge the semantics of [*release branches*](#release-branches) and [*maintenance branches*](#maintenance-branches).

This way your branches are long lived: they start when the release starts to be prepared (usually branching off the [*mainline*](#mainline)) and finish when the release is no longer supported and they are merged back to the *mainline* (or be perpetual).

These long lived branches require many merges from and to the *mainline* or other branches and often [cherry picking](https://git-scm.com/docs/git-cherry-pick) and just like *release* and *maintenance* branches they may lead to missed merges.

These branches may be named like *maintenance branches* (i.e. `v1`, `v1.x`, `v1.2`, `v1.2.x`) or *release branches* (i.e. `release-v1.2` `release/v1.2`, `rel/v1.2`) so which one to use is up to your organization.

#### [Maturity branches](https://martinfowler.com/articles/branching-patterns.html#maturity-branch)

These branches, often used by *release teams*, are used to track the maturity level of the codebase.

Examples of these branches are `production`, `rc` (or `release-candidate`), `stage`, `test`, `development`, `alpha`, `beta`, `gamma` etc.

When adopting these branches along with [*integration branches*](#integration-branches), the latter may also represent *maturity branches*. For example, the `development` branch can be considered to host code at an early maturity level that will be merged into more mature branches at a certain point.

These branches are well suited for both [development and operations driven](#development-driven-and-operations-driven-branching-models) workflows (merging [to and from the *mainline*](#branches-to-and-from-the-mainline)). When used both for pre and post release maturity stages it's a good practice to organize and disambiaguate them by a prefix. Examples are: `dev/integration`, `dev/test`, `ops/test` and `ops/production` or `pre/integration`, `pre/test`, `post/test` and `post/production`.

It's safe to manage these branches in order to merge them according to a [directed acyclic graph](https://en.wikipedia.org/wiki/Directed_acyclic_graph), or *waterfall* if you prefer the process perspective, from the less to the most mature. In these case, when one branch is merged into the next one it means the code is *promoted* to the next maturity level. For example: `development > test > stage > production`, or `gamma > beta > alpha`. What is important here is to avoid cycles or maintenance soon becomes overly complicated.

These branches are addressed by [GitLab Flow as *environment* branches](https://docs.gitlab.com/ee/topics/gitlab_flow.html#environment-branches-with-gitlab-flow).
{: .notice--info}

#### [Environment branches](https://martinfowler.com/articles/branching-patterns.html#environment-branch)

As [Fowler says](https://martinfowler.com/articles/branching-patterns.html#environment-branch), these are an anti-pattern and are discouraged, at least in the way he describes them: branches that basically *plug* environment-specific configuration files and other artifacts into the code.

Remember that nothing keeps you from storing environment specific artifacts into sub folders of your project.

A better usage of *environment branches* is to use them to manage *promotions* of one version from one environment to another. Strictly speaking, this makes them exactly like [*maturity branches*](#maturity-branches).

These branches are addressed by [GitLab Flow](https://docs.gitlab.com/ee/topics/gitlab_flow.html#environment-branches-with-gitlab-flow).
{: .notice--info}

#### Flavor branches

These branches have separate codebases depending on the flavor of the product, say a commercial version (i.e. *standard* or *enterprise*), a platform (i.e. *amd64*, *i386*) etc.

A naming convention for these branches might be like `platform/i386`, `platform/amd64` or `flavor/standard`, `flavor/enterprise` etc.

Using this branching model is discouraged because it usually drives to the [multiple mainlines](#multiple-mainlines) scenario, with all of its complexities.

#### Schedule branches

Many organizations stick to a release schedule and they need to have evidence of which features will make into one release or another, regardless of where the features scope is defined upfront or is dynamic.

The schedule model may change slightly from one organization to another. Here are a few examples:

* **periodic** schedule: the project has regular releases, say every month, three months, six months etc. This is also called [release train](https://martinfowler.com/articles/branching-patterns.html#release-train) by Martin Fowler. In this case the release date is fixed while the features scope may change as not all the features may make it on time. Branch names may look like `2020-01-01`, `schedule/2020/may`, `roadmap/2020/02`, `future/2020/may`, `future-release/2020-04`, `next/2020/may`
* **roadmap** schedule: in this case the features scope is often defined upfront by the roadmap while the release date may change. In this case you should just refer to [*release branches*](#release-branches)
* **agile** schedule: this is the case of Agile organizations that like to reflect their organization and schedule into the code management platform. For example they may use one branch for each [*sprint*](https://en.wikipedia.org/wiki/Scrum_Sprint) and optionally have broader scoped branches collecting changes related to [user stories, epics, themes, initialives](https://en.wikipedia.org/wiki/User_story) etc. Examples are `sprint/423`, `epic/21`, `story/6354`

When using schedule branches it's usually a good practice to have a *code freeze* period before the actual release in which additional tests are performed to grant the release stability. This period is proportional to the scope of the changes within the release.
{: .notice}

Schedule branches should be **short lived** and deleted when the schedule they refer to has been reached.

Again, [*feature branches*](#feature-branches) and [cherry picking](https://git-scm.com/docs/git-cherry-pick) are helpful in managing this kind of branches, especially when the features scope is flexible.

#### Organization branches

There are two examples of these:

* **individual** branches: these are used by single developers and, when they are pushed to the central repository, they are also used for [collaboration](https://martinfowler.com/articles/branching-patterns.html#collaboration-branch). Examples: `johndoe`, `member/jsmith`.
* **team** branches: are used to segregate contibutions based on the [team](https://martinfowler.com/articles/branching-patterns.html#team-integration-branch). Examples: `team/green`, `team/ny`.

#### [Experimental branches](https://martinfowler.com/articles/branching-patterns.html#experimental-branch)

Often used to encapsulate changes that may eventually be merged or not, or to showcase some changes, evaluate impact of some critical changes etc. These branches may be short or long lived and if they're frequent it's important to give them a clear name and optionally a prefix, like `exp/X`, `experiment/X`, `off/X`, `poc/X`.

## Development driven and Operations driven branching models

It's common to think of Git as a tool for development but practices like [GitOps](https://www.weave.works/technologies/gitops/) prove how operations can benefit from Git too.

As an example, let's imagine an organization that develops a product and then rolls it out into production after a few maturity levels have been tested. At first we can outline the overall process into:

1. **path to release** which culminates when the product has been developed, tested and ready to be deployed
2. **path to production** which takes the above product, configures and deploys it to production for actual users

This is a peculiar organization as it needs to cover the software lifecycle at 30 degrees while actual organizations in the commercial software industry are usually on one side only. This is what makes this example effective but you can take the half of your interest.

To the sake of simplicity let's assume the workflow is *continuous* in the sense that is not split into milestones or releases. On the other hand there is so much focus on testing so the process goes through different *maturity stages* in both phases. So let's expand the above process into stages, where each stage has a dedicated branch:

```
1. development    # developers integrate their code into this branch
2. test           # developers perform extensive tests here
3. master         # code in the mainline is considered released
4. test           # operations perform live tests here
5. stage          # operations showcase the new version to a restricted number of users
6. production     # operations rolls out the product to end users
```

We can notice a few things here:

* there are two `test` branches, so we need to disambiguate them as they host different versions
* the `master` branch is pivot between development and operations
* stages 1-2 merge **to** `master` (directly or indirectly) while stages 4-6 merge **from** `master`
* all of these branches are permanent as they serve a *continuous* workflow but other branches might be used although they're not shown in this example

So let's disambiguate and make our naming convention clearer, like:

```
1. dev/development
2. dev/test
3. master
4. ops/test
5. ops/stage
6. ops/production
```

This not only disambiguates the two `test` branches but also makes clearer who owns the branches and for what purpose.

## Branches TO and FROM the mainline

As the [above](#development-driven-and-operations-driven-branching-models) section has shown us, it's important to have a clear understanding the **merge flow** in terms of branches merging **to** or **from** master, either directly or indirectly. We often call them *afferent* or *efferent* branches (with regards to the *mainline*).

These branches are to be treated differently as we already introduced with [*release branches*](#release-branches) and [*schedule branches*](#schedule-branches) because they have different constraints workflows attached.

## Multiple mainlines

While the common practice is to have just one [*mainline*](#mainline) to store the primary, [*healthy*](https://martinfowler.com/articles/branching-patterns.html#healthy-branch) version of the project code, some others need multiple ones, like when using [*flavor branches*](#flavor-branches).

This is an *edge* case and requires several customizations in the project organization and process also because it implies that official releases are issued from multiple branches, with implications in the coherence of version numbers and tags etc. See [this example]({{site.baseurl}}{% link _posts/2020-01-01-git-history-examples.md %}#regular-branches) for a taste of how easy is to incur into version conflicts with this strategy.

For the sake of simplicity you should avoid having multiple *mainlines* and this can usually be achieved by using configuration flags, [feature toggles](https://en.wikipedia.org/wiki/Feature_toggle), build process ramifications, pre-processor conditionals etc. However, when you have no other chance, you probably should consider:

* avoiding the use of the standard `master` or `main` to avoid being mislead and, when possible, even delete it
* use coherent names for your multiple mainline branches, like `master/X`, `master/Y` or `main-X`, `main-Y`
* have a precise merge strategy that lets you merge the right code changes into the proper *mainlines*
* avoid drifts among the mainlines as much as possible, for example by having one common *mainline* from which others merge down periodically and limit specific merges to single changes into the multiple *mainlines*
* configure additional mainlines not to be released

## Your custom branching model

After reading all the above you might be tempted to create your super detailed branching model but refrain until you have read the following considerations:

* keep the branching model as simple as possible, according to your organization. Some even say that *branches are evil* and that's why simplified workflows like the ones we mentioned became popular
* remember that **branching is easy**, while **merging can be hard**. With too many branches the burden of maintaining them and merging can easily overcome the benefits of using them
* have a clear understanding of your **merge flow** between branches and avoid cyclic flows between them. Design your branches using a [directed acyclic graph](https://en.wikipedia.org/wiki/Directed_acyclic_graph), where the [*mainline*](#mainline) is (usually) at the root and is used as a pivot between branches [**to**](#branches-to-and-from-the-mainline) the *mainline* and those [**from**](#branches-to-and-from-the-mainline) the *mainline*
* when defining your model, take into account:
  * your **merge frequency** (try to increase it)
  * whether you do **code reviews** upon merge
  * your **automation** platform (CI/CD) and how the branching model and your pipelines affect each other
  * whether you have a single version to maintain or multiple ones
  * the size and complexity of your organization
* make the naming convention as clear as possible
* organize branches into hierarchies when this helps clarifying the process and the relationships among branches

Start simple and add branches when needed, when you're sure you can handle them consistently, provided all the above.