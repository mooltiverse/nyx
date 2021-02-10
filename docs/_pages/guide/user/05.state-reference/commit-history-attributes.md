---
title: Commit History Attributes
layout: single
toc: true
permalink: /guide/user/state-reference/commit-history-attributes/
---

## Top level attributes

The following attributes are at the top of the hierarchy:

| Name                                      | Type    | Values                               |
| ----------------------------------------- | ------- | ------------------------------------ |
| [`commitHistory`](#commit-history)        | object  | The list of commits in the history   |

### Commit history

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `commitHistory`                                                                          |
| Type                          | object                                                                                   |
| Related configuration options |                                                                                          |

This object has a list of [`commit`](#commit) objects as children where each item represents one commit in the history, starting from the most recent.

See also [`git log`](https://git-scm.com/docs/git-log).

#### Commit

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `commitHistory/<ID>`                                                                     |
| Type                          | object                                                                                   |
| Related configuration options |                                                                                          |

This object holds relevant informations about a single Git commit.

Each `commit` object has the following children:

| Name                                      | Type    | Values                               |
| ----------------------------------------- | ------- | ------------------------------------ |
| [`sha`](#sha)                             | string  | The commit SHA                       |
| [`author`](#author)                       | string  | The commit author                    |
| [`authorDate`](#author-date)              | string  | The commit author date               |
| [`committer`](#committer)                 | string  | The committer                        |
| [`committerDate`](#committer-date)        | string  | The committer date                   |
| [`message`](#message)                     | string  | The commit message                   |
| [`parents`](#parents)                     | list    | The commit parents SHAs              |
| [`tags`](#tags)                           | list    | The commit tags                      |

##### SHA

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `commitHistory/<ID>/sha`                                                                 |
| Type                          | string                                                                                   |
| Related configuration options |                                                                                          |

The commit SHA.

##### Author

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `commitHistory/<ID>/author`                                                              |
| Type                          | string                                                                                   |
| Related configuration options |                                                                                          |

The commit author.

##### Author date

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `commitHistory/<ID>/authorDate`                                                          |
| Type                          | string                                                                                   |
| Related configuration options |                                                                                          |

The commit author date.

##### Committer

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `commitHistory/<ID>/committer`                                                           |
| Type                          | string                                                                                   |
| Related configuration options |                                                                                          |

The commit committer.

##### Committer date

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `commitHistory/<ID>/committerDate`                                                       |
| Type                          | string                                                                                   |
| Related configuration options |                                                                                          |

The commit committer date.

##### Message

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `commitHistory/<ID>/message`                                                             |
| Type                          | string                                                                                   |
| Related configuration options |                                                                                          |

The commit message.

##### Parents

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `commitHistory/<ID>/parents`                                                             |
| Type                          | list                                                                                     |
| Related configuration options |                                                                                          |

The commit parents, which is a list of other commit SHAs.

##### Tags

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `commitHistory/<ID>/tags`                                                                |
| Type                          | list                                                                                     |
| Related configuration options |                                                                                          |

The commit tags, as a list of strings.
