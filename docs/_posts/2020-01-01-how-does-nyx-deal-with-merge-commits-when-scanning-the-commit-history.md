---
layout: single
title:  "How does Nyx deal with *merge* commits when scanning the commit history?"
date:   2020-01-01 00:00:00 +0000
categories: faq user
tags: git
---

*Merge* commits a just like regular commits with just more than one [parent](https://git-scm.com/docs/git-commit-tree).

When browsing the history of *regular* commits things are very intuitive since each commit has just one *parent*. This way they are chained linearly and you can just hop from one to its parent until you reach your target commit (or hit the *root* commit which is the one with no parents).

*Merge* commits pose an issue as you need to decide which parent to follow in the *chain*, provided that you can't [traverse](https://en.wikipedia.org/wiki/Tree_traversal) all the possible paths.

The answer is: follow the **first parent**. This is documented in the [`git log` page](https://git-scm.com/docs/git-log#Documentation/git-log.txt---first-parent) although, as usual, it's not easy to grasp.

Long story short, when [merging](https://git-scm.com/docs/git-merge) commits, git does not store parents in a random order but the *first commit* is always the one in the *current* branch while the *second* (and more, in case of *octopus* merges) follow in the order.

Nyx always follows the *first parent* to make sure it follows the right *chain*.

You can find more on this [`git log --first-parent` documentation](https://git-scm.com/docs/git-log#Documentation/git-log.txt---first-parent) or at [this post](https://redfin.engineering/visualize-merge-history-with-git-log-graph-first-parent-and-no-merges-c6a9b5ff109c).
