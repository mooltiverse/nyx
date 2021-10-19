---
layout: single
toc: true
title:  "Git history examples"
date:   2020-01-01 00:00:00 +0000
categories: example user
tags: support configuration git version
---

In this page you can find some examples of Git histories. To the sake of simplicity, these examples do not use a specific [commit message convention]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/commit-message-conventions.md %}) but, instead, they just show the kind of commit that would be inferred by a convention. [Semantic Versioning]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/version-schemes.md %}#semantic-versioning-semver) is the version scheme used.

## *Mainline* only

[![]({{site.baseurl}}/assets/git-history-mainline-only-using-defaults.svg){: .align-left}]({{site.baseurl}}/assets/git-history-mainline-only-using-defaults.svg)

Here we have a brand new repository with an *Initial commit* and several others:

The first *Initial commit* doesn't yield to any version number because it's not considered [*significant*]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#significant-commits) as its message (*Initial commit*) doesn't match any message convention.

Commit `c1` instead produces a new version `0.1.1` and a new release because its commit message is matched as a *patch* commit. The version number `0.1.1` is produced by [bumping]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/global-attributes.md %}#bump) the `patch` identifier on the [previous version]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#previous-version), which in this case is the default [`initialVersion`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-version) `0.1.0` since the repository history has no previous commits with valid version tags.

Next commit `c2` bumps the `minor` identifier (by the commit message) on the previous version `0.1.1` and produces a new release `0.2.0`.

Commit `c3` doesn't produce any new version or release because the commit message does not represent any significant change.

Subsequent commits produce new versions and releases as they are all significant, even though they bump different identifiers.

### Custom initial version

[![]({{site.baseurl}}/assets/git-history-mainline-only-with-custom-initial-version.svg){: .align-left}]({{site.baseurl}}/assets/git-history-mainline-only-with-custom-initial-version.svg)

In case you want to start from a different initial version (i.e. `1.5.3`) you have two options:

* manually tag the first commit `c0` as `1.5.3`
* set the [`initialVersion`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-version) configuration option to `1.5.3`

This will change the previous example so that the first bumped version is `1.5.4`, obtained by bumping the *patch* identifier on the initial version `1.5.3`.

## Branches

Let's see how things work with branches. In these examples the first commit is manually tagged as `0.1.0` (or the `initialVersion` is set to `0.1.0`). The mainline is always on the left side column while branches are on the right.

In order to better compare the branch types the following examples follow the same commit history.

### Regular branches

[![]({{site.baseurl}}/assets/git-history-regular-branch.svg){: .align-left}]({{site.baseurl}}/assets/git-history-regular-branch.svg)

Here we go through the Git commit history of a regular branch (a branch that has a standard versioning and no version constraints or extra identifiers). This scenario is basically the [multiple mainlines]({{site.baseurl}}{% link _pages/guide/user/06.best-practice/branching-models.md %}#multiple-mainlines) strategy and is deprecated for the reasons you will see right below.

Versions are incremented intuitively as once the version identifier to bump is known the bump operation does exactly that, with no further considerations.

Commits `c1` through `c8` could as well be done in the mainline (as you can see from the dashed line on the left) and commit `c9` is a regular commit, not a merge commit (unless the merge is done using the `--no-ff` option), because the commit history up to `c8` is linear.

The user branch is deleted after commit `c9` (as you can see by `c8` having just one child commit `c9`).

Another user branch is created with `c10` being the first commit in the branch. Commits `c10`-`c12` introduce a divergence in versions as `c1` in the mainline only bumps the *patch* identifier (version `3.0.1`) while `c10` and `c12` in the user branch produce version `3.2.0`. The interesting part here is merge commit `m13` as:

* the commit message yields to *minor* as the identifier to bump (regardless if it's mentioned once or twice, since we have two commits `c10` and `c12` in the user branch bumping *minor*)
* the commit [first parent](https://git-scm.com/docs/git-log#Documentation/git-log.txt---first-parent) `c11` is tagged as `3.0.1`
* the new version number is then obtained by bumping the *minor* identifier on version `3.0.1`, indeed producing version `3.1.0`
* a tagged version `3.1.0` already exists at commit `c10` so the release process fails and no release is issued at commit `m13`

Commit `c14` starts a new branch off of merge commit `m13` and is released as `3.0.2` as it bumps the *patch* identifier on the previous version `3.0.1` (from its closest tagged parent `c11`). But commits `c15` and `c16`, in the mainline and in the custom branch, respectively, both fail to release because they both generate version `3.1.0` but, again, such version is already tagged at `c10`, so both commits fail to release with an error.

Commit `c16` is then merged into the mainline with merge commit `m17`, failing for the same reasons of previous commits. What's interesting here is that the branch is not deleted. Instead, the mainline is merged back to the custom branch at commit `m18` which, as you can see, is a merge commit because it has two parents. Please note here that `m18`'s [first parent](https://git-scm.com/docs/git-log#Documentation/git-log.txt---first-parent) is `c16`, not `m17` as one may think, and that's why the closest parent version to bump is `3.0.2` (from commit `c14`), which, bumping the *patch*, yields to the new version `3.0.3`. This time the release succeeds as there was no other tag `3.0.3` in the repository.

Unfortunately, subsequent commits `c19` and `c20` fail because of the same version conflict as above.

### Pre-release branch

Pre-release branches are peculiar in a couple of ways:

* they have additional identifiers, usually (as in this example) there's one that represents the branch name (`alpha` in this case) that is also bumped
* the versioning is *collapsed* so version numbers are not incremented linearly as in other branches but, instead, new version numbers are [computed]({{ site.baseurl }}{% link _posts/2020-01-01-how-does-collapsed-versioning-work.md %}) bumping from the [prime version]({{ site.baseurl }}{% link _posts/2020-01-01-whats-the-difference-between-the-prime-version-and-the-previous-version.md %})

This is better explained through the example where *prime* versions are highlighted by commits in orange circles and the path from any *pre-release* commit to its *prime* version is highlighted with orange arrows.

[![]({{site.baseurl}}/assets/git-history-pre-release-branch.svg){: .align-left}]({{site.baseurl}}/assets/git-history-pre-release-branch.svg)

The *Initial commit* `c0` is tagged manually as `0.1.0` and this will also be the *prime number* for all commits `c1`-`c8`. Then the `alpha` branch is created with the first commit `c1`, bumping the *patch* identifier. As you can see, the generated version `0.1.1-alpha.1` is obtained by actually bumping the *patch* against the *previous version* `0.1.0` (which happens to also be the *prime version* here), but an additional identifier `alpha` is added in the *pre-release* block. The `alpha` identifier also has an integer value, initialized to `1` as the initial value.

The next commit `c2` also bumps the *patch* but something weird happens here as the new version `0.1.1-alpha.2` just bumps the `alpha` pre-release identifier, while the *patch* doesn't change. Why so? Here is wher the difference between the *previous* and *prime* versions comes in.

At commit `c2` the *previous* version is `0.1.1-alpha.1` (from `c1`) but the *prime* version is `0.1.0`, from `c0`. The *prime* version is the first *regular* version (with only *core* identifiers) that is reachable by following the [first parent](https://git-scm.com/docs/git-log#Documentation/git-log.txt---first-parent) and while for commit `c1` this was the same as the *previous* version, `c2` is the first commit where the two differ. Ok but what about the bumping for commit `c2`? Starting from the *prime* version `0.1.0` the *patch* identifier was already bumped at `c1` so it's not bumped anymore. Not bumping the *patch* would lead to a version clash with `0.1.1-alpha.1` (from `c1`) so the `alpha` identifier is incremented to disambiguate.

You should start seeing why the version numbers are *collapsed* here as multiple bumps of the same core identifier are avoided as if such identifier was bumped only once.

At commit `c3` we have a *previous* version `0.1.1-alpha.2` from `c2` and a *prime* version `0.1.0` from `c0`. What differs from `c2` is that this time the *minor* identifier has to be bumped, so we do. The new version is `0.2.0-alpha.1` as the *minor* is bumped for the first time on the *prime* version `0.1.0` and since the *minor* identifier is more significant than the *patch*, the *patch* is reset to `0` and the `alpha` identifier is reset to the initial value `1` as there's no need to disambiguate by the pre-release.

After a non significant commit `c4`, commit `c5` bumps the `major` identifier so what happens is very similar to the previous commit with the only difference that since the *major* number is more significant than the *minor* and the *patch*, this also resets them, yielding to `1.0.0-alpha.1`.

Pay attention to commit `c6` now. It bumps the *patch* but since more significant identifiers (*major* and *minor*) were already bumped since the *prime* version, no *core* identifier is bumped. Just the `alpha` pre-release identifier is bumped to disambiguate from the previous version `1.0.0-alpha.1` at `c5`, yielding to `1.0.0-alpha.2`. The same goes for commits `c7` and `c8` but commit `c9` is different.

As we can see, merging `c8` to the mainline does not produce a merge commit (unless the merge is done using the `--no-ff` option) as the history so far was linear but what changes here is that the versioning is no longer evaluated as a pre-release, so it's no longer *collapsed*, because we are now in a regular branch. The identifier bumped here is the *major* because it's the most significant one bumped so far by commits `c1`-`c8` and the new version `1.0.0` is like bumping the *prime* version `0.1.0` like all the commits in between were *collapsed* into one (`c9`).

In other words, **when a *pre-release* branch is merged into a regular branch, all of the version that were generated in the *pre-release* branch are *collapsed* into one**. This is the beauty and usefulness of *collapsed* versioning as a pre-release branch reduces the number of version being issued and its final outcome is to *group* them all together. You can see using the *collapsed* versioning is conceptually close to [*squashing*](https://git-scm.com/docs/git-merge#Documentation/git-merge.txt---squash) the version history but without losing the commits you've done in the middle and, instead, being able to issue intermediate, non official releases, while approaching a new official one.

Let's get back to our example for a few more caveats.

Version `1.0.0` at commit `c9` is now the *prime* version for commits `c10` and `c12`. Note that this is true because the pre-release branch has been deleted after commit `c9`, as you can see by the fact that `c8` has no child commit other than `c9`.

Commits `c10` and `c12` both bump the *minor* and end with version `1.1.0-alpha.2` but this time, when they are merged into the mainline, an actual merge commit is generated because, in the meanwhile, also commit `c11` was added. Merge commit `m13` bumps the *minor* identifier (inferred by the commits in the pre-release branch) against its *previous* version `1.0.1` (at `c11`) and produces version `1.1.0`. Version `1.1.0` now becomes the *prime version* for `c14` and all the subsequent commits in the branch. You should be already familiar with what happens up to `m17` (`1.3.0`) but here we have a new caveat.

The branch is not deleted after merging `c16` into the mainline at `m17`. In fact, `m17` is then merged back to the branch so it has an update version of the mainline but `m18` is another merge commit.

Can you guess what's the *prime* version at commit `m18`? If you think it's `1.3.0` (from commit `m17`), think again.

As we said, the *prime* version is the first regular version reachable going backward in the commit history by following the [first parent](https://git-scm.com/docs/git-log#Documentation/git-log.txt---first-parent) of every commit (merge commits, in particular). The *first parent* of commit `m18` is `c16`, not `m17`, as the merge was done pulling the contents **from** the mainline **into** the pre-release branch. So going back in the commit history, the *prime* version is `1.1.0` at commit `m13`.

Knowing that you can tell why bumping the *patch* at commit `m18` yields to version `1.2.0-alpha.2` instead of `1.3.1-alpha.1` (as it would've been if `1.3.0` was the *prime*).

Keep this in mind if you use long running pre-release branches that you merge to and from. If this is not what you want, just delete the old branch when you merge into the mainline and create a new one with the same name to start with a new *prime* number.

### Version constrained branch

[![]({{site.baseurl}}/assets/git-history-maintenance-branch.svg){: .align-left}]({{site.baseurl}}/assets/git-history-maintenance-branch.svg)

These branches are often defined in those branching strategies using [release branches]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#release-branches), [maintenance branches]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#maintenance-branches) or [release and maintenance]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#release-and-maintenance-branches).

What they basically do is make sure that versions issued by these branches comply with a certain range, preventing releases outside the range to be issued. This is done by configuring the branch with static rules (like [`versionRange`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range)) or dynamic rules (like [`versionRangeFromBranchName`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range-from-branch-name)).

In this example we use a branch named `0.x` (might as well be `v0.x`) that only allows version with the *major* identifier to be `0`, so the range is between `0.0.0` and `0.N.M`, with `N` and `M` set to be any positive integer.

As you can see by the graph, any attempt to create a version outside the range produces an error. This happens at commits `c5` and `c8` trying to bump the *major* number (to `1`) but also at commits `c10` and `c12` where the *minor* number is supposed to be bumped on a previous version (`1.0.0`) that is already beyond the allowed range.

Applying a version constraint to a branch doesn't add any other feature or control so you may not use them and have everything working as expected. The only value you get from version constrained branches is an additional check to version consistency so that you don't issue out-of-range releases by mistake.
